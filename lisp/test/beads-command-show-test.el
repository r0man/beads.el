;;; beads-command-show-test.el --- Live/ACTIVITY tests for show -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Unit tests for the live-journal integration in `beads-command-show.el'
;; (WI-LIVE-14): the ACTIVITY section rendered through
;; `beads-show--insert-section-with', the per-issue subscriber, the
;; bounded buffer-local log, and the debounced live re-render that
;; preserves point.
;;
;; The live stream supervisor and the pure event model are separate work
;; items; these tests stay self-contained by binding the live entry
;; points with `cl-letf' and by using `beads-event-record' objects built
;; through `beads-event-record-from-json' (both already in
;; `beads-types.el').  No subprocess is started.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-command-show)
(require 'beads-test)

;;; Helpers

(defun beads-command-show-test--text ()
  "Return the current buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun beads-command-show-test--issue (alist)
  "Parse JSON-shape ALIST into a `beads-issue'."
  (beads--parse-issue alist))

(defun beads-command-show-test--record (seq op id &optional actor issue)
  "Build a `beads-event-record' with SEQ, OP, ID, ACTOR and ISSUE."
  (beads-event-record-from-json
   (append (list (cons 'seq seq)
                 (cons 'ts "2026-10-10T12:00:00Z")
                 (cons 'op op)
                 (cons 'issue_id id))
           (when actor (list (cons 'actor actor)))
           (when issue (list (cons 'issue issue))))))

;;; ACTIVITY section rendering

(ert-deftest beads-command-show-test-activity-section-renders ()
  "The ACTIVITY section renders records through the shared helper."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42")
    (setq beads-show--activity
          (list (beads-command-show-test--record 2 "update" "bd-42" "alice")
                (beads-command-show-test--record 1 "create" "bd-42" "bob")))
    (let ((inhibit-read-only t))
      (beads-show--insert-activity-section))
    (let ((text (beads-command-show-test--text)))
      (should (string-match-p "^ACTIVITY$" text))
      (should (string-match-p "#2" text))
      (should (string-match-p "@alice" text))
      (should (string-match-p "#1" text))
      (should (string-match-p "@bob" text))
      ;; Newest first: the seq-2 row precedes the seq-1 row.
      (should (< (string-match-p "#2" text)
                 (string-match-p "#1" text))))))

(ert-deftest beads-command-show-test-activity-hidden-when-empty ()
  "No ACTIVITY header is inserted when the log is empty."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42"
          beads-show--activity nil)
    (let ((inhibit-read-only t))
      (beads-show--insert-activity-section))
    (should-not (string-match-p "ACTIVITY" (beads-command-show-test--text)))))

(ert-deftest beads-command-show-test-render-issue-includes-activity ()
  "`beads-show--render-issue' inserts ACTIVITY between body and labels."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42"
          beads-show--activity
          (list (beads-command-show-test--record 5 "update" "bd-42" "alice")))
    (beads-show--render-issue
     (beads-command-show-test--issue
      '((id . "bd-42")
        (title . "Widget detail")
        (description . "Body")
        (status . "open")
        (priority . 2)
        (issue_type . "task")
        (created_at . "2026-10-09T10:00:00Z")
        (updated_at . "2026-10-09T10:00:00Z"))))
    (let ((text (beads-command-show-test--text)))
      (should (string-match-p "ACTIVITY" text))
      (should (string-match-p "#5" text))
      (should (< (string-match-p "DESCRIPTION" text)
                 (string-match-p "ACTIVITY" text))))))

(ert-deftest beads-command-show-test-render-issue-omits-empty-activity ()
  "An empty ACTIVITY log adds no section to the rendered issue."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42"
          beads-show--activity nil)
    (beads-show--render-issue
     (beads-command-show-test--issue
      '((id . "bd-42")
        (title . "No events")
        (status . "open")
        (priority . 2)
        (issue_type . "task")
        (created_at . "2026-10-09T10:00:00Z")
        (updated_at . "2026-10-09T10:00:00Z"))))
    (should-not (string-match-p "ACTIVITY" (beads-command-show-test--text)))))

;;; Live subscriber and bounded log

(ert-deftest beads-command-show-test-live-record-filters-by-issue ()
  "Only records for the shown issue enter the ACTIVITY log."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42")
    (beads-show--live-record (beads-command-show-test--record 1 "update" "bd-42" "a"))
    (beads-show--live-record (beads-command-show-test--record 2 "update" "bd-99" "b"))
    (should (= (length beads-show--activity) 1))
    (should (= (oref (car beads-show--activity) seq) 1))
    (should beads-show--activity-dirty)))

(ert-deftest beads-command-show-test-activity-bounded ()
  "The buffer-local ACTIVITY log is bounded by `beads-show-activity-limit'."
  :tags '(:unit)
  (let ((beads-show-activity-limit 3))
    (with-temp-buffer
      (beads-show-mode)
      (setq beads-show--issue-id "bd-42")
      (dotimes (i 5)
        (beads-show--live-record
         (beads-command-show-test--record (1+ i) "update" "bd-42" "a")))
      (should (= (length beads-show--activity) 3))
      ;; Newest first: seq 5 is at the head and seq 3 at the tail.
      (should (= (oref (car beads-show--activity) seq) 5))
      (should (= (oref (car (last beads-show--activity)) seq) 3)))))

;;; Debounced live re-render

(ert-deftest beads-command-show-test-live-refresh-preserves-point ()
  "A dirty batch re-fetches this issue and restores point."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42"
          beads-show--activity-dirty t)
    (let ((inhibit-read-only t))
      (insert (mapconcat (lambda (n) (format "line-%d\n" n))
                         (number-sequence 1 40) "")))
    (goto-char 100)
    (let (called)
      (cl-letf (((symbol-function 'beads-show--load)
                 (lambda (_buffer _id &rest args)
                   (setq called t)
                   ;; Simulate a re-render that moves point to bob.
                   (goto-char (point-min))
                   (when-let* ((after (plist-get args :after)))
                     (funcall after)))))
        (beads-show--live-refresh))
      (should called)
      (should (= (point) 100))
      (should-not beads-show--activity-dirty))))

(ert-deftest beads-command-show-test-live-refresh-clean-is-noop ()
  "A batch without a record for this issue does not re-fetch."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42"
          beads-show--activity-dirty nil)
    (let (called)
      (cl-letf (((symbol-function 'beads-show--load)
                 (lambda (&rest _) (setq called t))))
        (beads-show--live-refresh))
      (should-not called))))

(ert-deftest beads-command-show-test-live-refresh-clamps-point ()
  "Point is clamped into the buffer when a re-render shortens the text."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42"
          beads-show--activity-dirty t)
    (let ((inhibit-read-only t))
      (insert "short\n"))
    (goto-char (point-max))
    (cl-letf (((symbol-function 'beads-show--load)
               (lambda (_buffer _id &rest args)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert "x"))
                 (when-let* ((after (plist-get args :after)))
                   (funcall after)))))
      (beads-show--live-refresh))
    (should (<= (point) (point-max)))
    (should (= (point) (point-max)))))

;;; Attach wiring

(ert-deftest beads-command-show-test-live-attach-wires-subscription ()
  "Attach wires the buffer's refresh and per-record subscriber."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42")
    (let ((original-require (symbol-function 'require))
          attached-args subscribed)
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional file noerror)
                   (if (eq feature 'beads-live)
                       t
                     (funcall original-require feature file noerror))))
                ((symbol-function 'beads-live-attach)
                 (lambda (&rest args) (setq attached-args args) 'stream))
                ((symbol-function 'beads-live-subscribe)
                 (lambda (fn &optional buffer)
                   (setq subscribed (cons fn buffer))
                   (cons 'stream subscribed))))
        (let ((beads-show--live-inhibit nil)
              (beads-show-live t)
              (beads-show--live-handle nil))
          (beads-show--live-attach)))
      (should attached-args)
      (should (eq (car subscribed) #'beads-show--live-record))
      (should (eq (cdr subscribed) (current-buffer)))
      (should (eq beads-show--live-stream 'stream)))))

(ert-deftest beads-command-show-test-live-attach-inhibited ()
  "Attach is a no-op while `beads-show--live-inhibit' is non-nil."
  :tags '(:unit)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-id "bd-42")
    (let (called)
      (cl-letf (((symbol-function 'beads-live-attach)
                 (lambda (&rest _) (setq called t) 'stream)))
        (let ((beads-show--live-inhibit t))
          (should-not (beads-show--live-attach))))
      (should-not called))))

;;; Diff rendering

(ert-deftest beads-command-show-test-activity-diff-uses-event-model ()
  "The diff lines come from the pure model when it is available."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-event-record-diff)
             (lambda (_record _previous) '((fake))))
            ((symbol-function 'beads-event-diff-string)
             (lambda (_change) "status open → closed")))
    (should (equal (beads-show--activity-diff
                    (beads-command-show-test--record 1 "update" "bd-42" "a")
                    nil)
                   '("status open → closed")))))

(provide 'beads-command-show-test)
;;; beads-command-show-test.el ends here
