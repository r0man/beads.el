;;; beads-agent-ralph-persist-test.el --- Tests for ralph persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the Ralph persistence module: JSONL summary writer,
;; per-iter NDJSON event capture, retention pruning, and the resume-
;; detection helpers.  Tests stub the controller / stream objects so
;; no real `claude' subprocess or bd workspace is required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-ralph-stream)
(require 'beads-agent-ralph)
(require 'beads-agent-ralph-persist)

;;; Fixtures

(defmacro beads-agent-ralph-persist-test--in-tempdir (var &rest body)
  "Bind VAR to a fresh temp directory, run BODY, then delete."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (file-name-as-directory (make-temp-file "ralph-persist-test" t))))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun beads-agent-ralph-persist-test--make-iter (&rest plist)
  "Construct an iteration record for tests with sensible defaults.
PLIST overrides individual slots."
  (apply #'beads-agent-ralph--iteration
         (append plist
                 (list :issue-id (or (plist-get plist :issue-id) "bde-a")
                       :status (or (plist-get plist :status) 'finished)
                       :duration-ms (or (plist-get plist :duration-ms) 1234)
                       :cost-usd (or (plist-get plist :cost-usd) 0.123)
                       :tool-call-count
                       (or (plist-get plist :tool-call-count) 2)
                       :bd-updates-count
                       (or (plist-get plist :bd-updates-count) 1)
                       :sentinel-hit (or (plist-get plist :sentinel-hit) nil)
                       :summary (or (plist-get plist :summary) "did stuff")))))

;;; Path helpers

(ert-deftest beads-agent-ralph-persist-test-jsonl-path-shape ()
  "Summary log lives under .beads/scratch/ralph/<root>.jsonl."
  (let ((path (beads-agent-ralph-persist-jsonl-path "/tmp/x/" "bde-1")))
    (should (string-match-p "/x/.beads/scratch/ralph/bde-1\\.jsonl\\'" path))))

(ert-deftest beads-agent-ralph-persist-test-iter-event-path-shape ()
  "Per-iter NDJSON path follows .iter-N pattern."
  (let ((raw (beads-agent-ralph-persist-iter-event-path
              "/tmp/x/" "bde-2" 7))
        (gz (beads-agent-ralph-persist-iter-event-path
             "/tmp/x/" "bde-2" 7 t)))
    (should (string-match-p "bde-2\\.iter-7\\.ndjson\\'" raw))
    (should (string-match-p "bde-2\\.iter-7\\.ndjson\\.gz\\'" gz))))

;;; JSONL writer

(ert-deftest beads-agent-ralph-persist-test-record-iteration-creates-file ()
  "Recording an iteration creates the JSONL with one record."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let ((iter (beads-agent-ralph-persist-test--make-iter
                 :summary "did A"))
          (path (beads-agent-ralph-persist-jsonl-path dir "bde-r")))
      (beads-agent-ralph-persist-record-iteration dir "bde-r" iter)
      (should (file-readable-p path))
      (let ((records (beads-agent-ralph-persist-read-jsonl dir "bde-r")))
        (should (= 1 (length records)))
        (should (equal "iteration" (cdr (assoc "kind" (car records)))))
        (should (equal "did A" (cdr (assoc "summary" (car records)))))))))

(ert-deftest beads-agent-ralph-persist-test-record-iteration-appends ()
  "Multiple iteration records append in order."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let ((iter-1 (beads-agent-ralph-persist-test--make-iter :summary "one"))
          (iter-2 (beads-agent-ralph-persist-test--make-iter :summary "two")))
      (beads-agent-ralph-persist-record-iteration dir "bde-r" iter-1)
      (beads-agent-ralph-persist-record-iteration dir "bde-r" iter-2)
      (let ((records (beads-agent-ralph-persist-read-jsonl dir "bde-r")))
        (should (= 2 (length records)))
        (should (equal "one" (cdr (assoc "summary" (nth 0 records)))))
        (should (equal "two" (cdr (assoc "summary" (nth 1 records)))))))))

(ert-deftest beads-agent-ralph-persist-test-record-status ()
  "Status transition records survive the round-trip."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let ((controller
           (beads-agent-ralph--controller
            :root-id "bde-r"
            :project-dir dir
            :iteration 4
            :cumulative-cost-usd 1.5)))
      (beads-agent-ralph-persist-record-status dir controller 'done)
      (let* ((records (beads-agent-ralph-persist-read-jsonl dir "bde-r"))
             (status-record (car records)))
        (should (= 1 (length records)))
        (should (equal "status" (cdr (assoc "kind" status-record))))
        (should (equal "done" (cdr (assoc "status" status-record))))
        (should (= 4 (cdr (assoc "iteration" status-record))))))))

(ert-deftest beads-agent-ralph-persist-test-read-jsonl-handles-malformed ()
  "An unparsable JSONL line becomes a `malformed' placeholder record."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let ((path (beads-agent-ralph-persist-jsonl-path dir "bde-m")))
      (beads-agent-ralph-persist--ensure-dir dir)
      (with-temp-file path
        (insert "{\"kind\":\"iteration\",\"summary\":\"good\"}\n")
        (insert "not-json\n")
        (insert "{\"kind\":\"iteration\",\"summary\":\"alsoGood\"}\n"))
      (let ((records (beads-agent-ralph-persist-read-jsonl dir "bde-m")))
        (should (= 3 (length records)))
        (should (equal "malformed" (cdr (assoc "kind" (nth 1 records)))))))))

;;; Per-iter event capture

(ert-deftest beads-agent-ralph-persist-test-event-subscriber-writes-events ()
  "The event subscriber writes new events to the per-iter NDJSON."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let* ((stream (beads-agent-ralph--stream
                    :partial-messages (make-hash-table :test #'equal)
                    :status 'starting))
           (subscriber (beads-agent-ralph-persist-make-event-subscriber
                        dir "bde-c" 1)))
      ;; Push a couple of events into the stream then drive the subscriber.
      (oset stream events
            (list (list :type "system" :subtype "init")))
      (funcall subscriber stream)
      ;; Add more events then run the subscriber again — only new events
      ;; should be written.
      (oset stream events
            (append (list (list :type "assistant" :message
                                (list :id "msg_1")))
                    (oref stream events)))
      (funcall subscriber stream)
      (let* ((events (beads-agent-ralph-persist-read-iter-events
                      dir "bde-c" 1)))
        (should (= 2 (length events)))
        (should (equal "system" (cdr (assoc "type" (nth 0 events)))))
        (should (equal "assistant" (cdr (assoc "type" (nth 1 events)))))))))

(ert-deftest beads-agent-ralph-persist-test-event-subscriber-compresses-on-finish ()
  "Terminal status triggers compression of the per-iter NDJSON."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (skip-unless (executable-find "gzip"))
    (let* ((stream (beads-agent-ralph--stream
                    :partial-messages (make-hash-table :test #'equal)
                    :status 'starting))
           (subscriber (beads-agent-ralph-persist-make-event-subscriber
                        dir "bde-z" 1))
           (raw (beads-agent-ralph-persist-iter-event-path
                 dir "bde-z" 1 nil))
           (gz (beads-agent-ralph-persist-iter-event-path
                dir "bde-z" 1 t)))
      (oset stream events
            (list (list :type "result" :subtype "success"
                        :total_cost_usd 0.001)))
      (funcall subscriber stream)
      (should (file-readable-p raw))
      ;; Mark finished and run subscriber again — should compress.
      (oset stream status 'finished)
      (funcall subscriber stream)
      (should (file-readable-p gz))
      (should-not (file-exists-p raw)))))

;;; Retention pruning

(ert-deftest beads-agent-ralph-persist-test-prune-keeps-newest ()
  "Pruning leaves only the N newest per-iter files."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (beads-agent-ralph-persist--ensure-dir dir)
    ;; Make five fake event files for iters 1..5.
    (dotimes (n 5)
      (let ((p (beads-agent-ralph-persist-iter-event-path
                dir "bde-p" (1+ n) nil)))
        (with-temp-file p (insert "{}\n"))))
    (let ((beads-agent-ralph-event-retention 3))
      (beads-agent-ralph-persist--prune-event-files dir "bde-p"))
    ;; Iters 1 and 2 should be gone; 3, 4, 5 remain.
    (should-not
     (file-exists-p (beads-agent-ralph-persist-iter-event-path
                     dir "bde-p" 1 nil)))
    (should-not
     (file-exists-p (beads-agent-ralph-persist-iter-event-path
                     dir "bde-p" 2 nil)))
    (should
     (file-exists-p (beads-agent-ralph-persist-iter-event-path
                     dir "bde-p" 5 nil)))))

(ert-deftest beads-agent-ralph-persist-test-prune-disabled-by-nil ()
  "Setting retention to nil disables pruning entirely."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (beads-agent-ralph-persist--ensure-dir dir)
    (dotimes (n 4)
      (let ((p (beads-agent-ralph-persist-iter-event-path
                dir "bde-x" (1+ n) nil)))
        (with-temp-file p (insert "{}\n"))))
    (let ((beads-agent-ralph-event-retention nil))
      (beads-agent-ralph-persist--prune-event-files dir "bde-x"))
    (should
     (file-exists-p (beads-agent-ralph-persist-iter-event-path
                     dir "bde-x" 1 nil)))))

;;; Resume detection

(ert-deftest beads-agent-ralph-persist-test-resume-summary-returns-nil-without-log ()
  "No JSONL means no resume summary."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (should (null (beads-agent-ralph-persist-resume-summary
                   dir "bde-none")))))

(ert-deftest beads-agent-ralph-persist-test-resume-summary-totals ()
  "Resume summary aggregates iterations and cost."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let ((iter-1 (beads-agent-ralph-persist-test--make-iter
                   :cost-usd 0.5 :summary "one"))
          (iter-2 (beads-agent-ralph-persist-test--make-iter
                   :cost-usd 1.0 :summary "two")))
      (beads-agent-ralph-persist-record-iteration dir "bde-s" iter-1)
      (beads-agent-ralph-persist-record-iteration dir "bde-s" iter-2)
      (let ((summary (beads-agent-ralph-persist-resume-summary dir "bde-s")))
        (should summary)
        (should (= 2 (plist-get summary :iterations)))
        (should (= 1.5 (plist-get summary :cumulative-cost)))))))

(ert-deftest beads-agent-ralph-persist-test-archive-jsonl ()
  "Archiving renames the JSONL out of the way with a timestamp."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let ((iter (beads-agent-ralph-persist-test--make-iter)))
      (beads-agent-ralph-persist-record-iteration dir "bde-a" iter)
      (let* ((src (beads-agent-ralph-persist-jsonl-path dir "bde-a"))
             (dst (beads-agent-ralph-persist-archive-jsonl dir "bde-a")))
        (should dst)
        (should (file-exists-p dst))
        (should-not (file-exists-p src))))))

(ert-deftest beads-agent-ralph-persist-test-resume-prompt-text-clean ()
  "Clean-tree legend contains only 4 options."
  (let ((text (beads-agent-ralph-persist-resume-prompt-text
               '(:iterations 3 :cumulative-cost 0.25 :last-timestamp "2026-05-12T10:00:00")
               nil)))
    (should (string-match-p "\\[r\\]esume" text))
    (should (string-match-p "\\[f\\]resh" text))
    (should (string-match-p "\\[F\\]ull-reset" text))
    (should (string-match-p "\\[c\\]ancel" text))
    (should-not (string-match-p "\\[s\\]tash" text))))

(ert-deftest beads-agent-ralph-persist-test-resume-prompt-text-dirty ()
  "Dirty-tree legend adds the [s]tash option."
  (let ((text (beads-agent-ralph-persist-resume-prompt-text
               '(:iterations 1 :cumulative-cost 0.1 :last-timestamp "n/a")
               t)))
    (should (string-match-p "Worktree DIRTY" text))
    (should (string-match-p "\\[s\\]tash" text))))

;;; Path enumeration

(ert-deftest beads-agent-ralph-persist-test-record-paths ()
  "Record-paths returns every on-disk artefact for a root."
  (beads-agent-ralph-persist-test--in-tempdir dir
    (let ((iter (beads-agent-ralph-persist-test--make-iter)))
      (beads-agent-ralph-persist-record-iteration dir "bde-q" iter)
      (with-temp-file (beads-agent-ralph-persist-iter-event-path
                       dir "bde-q" 1 nil)
        (insert "{}\n"))
      (let ((paths (beads-agent-ralph-persist-record-paths dir "bde-q")))
        (should (cl-some (lambda (p) (string-suffix-p ".jsonl" p)) paths))
        (should (cl-some (lambda (p) (string-suffix-p ".ndjson" p)) paths))))))

(provide 'beads-agent-ralph-persist-test)

;;; beads-agent-ralph-persist-test.el ends here
