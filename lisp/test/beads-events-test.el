;;; beads-events-test.el --- Tests for beads-events -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT `:unit' tests for beads-events.el (WI-LIVE-15): the timeline and
;; city-timeline views over the `bd events' journal.  The pure model
;; (`beads-event.el') and the stream supervisor (`beads-live.el') are
;; absent in isolation; every test injects records directly and stubs
;; the live chip with `cl-letf', so no subprocess is spawned.

;;; Code:

(require 'ert)
(require 'beads-events)
(require 'beads-types)

(defun beads-events-test--issue (&rest args)
  "Build a `beads-issue' for tests from ARGS."
  (apply #'beads-issue
         :id "be-1" :title "A title" :status "open" args))

(defun beads-events-test--record (seq time op issue-id actor &rest args)
  "Build a `beads-event-record' at SEQ/TIME/OP/ISSUE-ID/ACTOR.
ARGS may add an `:issue' snapshot."
  (apply #'beads-event-record
         :seq seq
         :ts (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t)
         :op op
         :issue-id issue-id
         :actor actor
         args))

(defmacro beads-events-test--with-view (records &rest body)
  "Render RECORDS in a fresh `beads-events-mode' buffer, then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (beads-events-mode)
     (setq beads-events--records ,records
           beads-events--filter nil)
     (beads-events--render)
     ,@body))

;;; Construction and mode

(ert-deftest beads-events-test-mode-activation ()
  "`beads-events-mode' derives from `tabulated-list-mode'."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (should (eq major-mode 'beads-events-mode))
    (should (derived-mode-p 'tabulated-list-mode))))

(ert-deftest beads-events-test-render-entry-count-and-columns ()
  "Rendering three records yields three entries with the right columns."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 3 100 "close" "be-1" "alice")
            (beads-events-test--record 2 90 "comment" "be-1" "bob"))
    (should (= (length tabulated-list-entries) 2))
    (let ((entry (car tabulated-list-entries)))
      (should (beads-event-record-p (car entry)))
      (should (= (length (cadr entry)) 5)))
    (should (= beads-events--shown 2))))

(ert-deftest beads-events-test-render-newest-first ()
  "Rendering orders records newest-first by timestamp."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 1 100 "close" "be-old" "alice")
            (beads-events-test--record 3 300 "update" "be-new" "bob")
            (beads-events-test--record 2 200 "close" "be-mid" "carol"))
    (should (equal (mapcar (lambda (e) (oref (car e) issue-id))
                           tabulated-list-entries)
                   '("be-new" "be-mid" "be-old")))))

(ert-deftest beads-events-test-record-cells ()
  "A row shows glyph, op, issue id and actor."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record
             1 100 "close" "be-1" "alice"
             :issue (beads-events-test--issue :id "be-1" :title "Fix it")))
    (let ((cols (cadr (car tabulated-list-entries))))
      (should (equal (aref cols 1) "■"))       ; attention glyph
      (should (equal (aref cols 2) "close"))
      (should (string-match-p "Fix it" (aref cols 3)))
      (should (equal (aref cols 4) "@alice")))))

;;; Filters

(ert-deftest beads-events-test-filter-op ()
  "The op filter keeps only matching ops."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 3 300 "close" "be-1" "a")
            (beads-events-test--record 2 200 "comment" "be-2" "b")
            (beads-events-test--record 1 100 "create" "be-3" "c"))
    (setq beads-events--filter '(:op "close"))
    (beads-events--render)
    (should (= beads-events--shown 1))
    (should (= (length tabulated-list-entries) 1))))

(ert-deftest beads-events-test-filter-actor ()
  "The actor filter is a case-insensitive substring match."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 2 200 "close" "be-1" "beads/worker")
            (beads-events-test--record 1 100 "close" "be-2" "mayor"))
    (setq beads-events--filter '(:actor "WORKER"))
    (beads-events--render)
    (should (= beads-events--shown 1))
    (should (equal (oref (car (car tabulated-list-entries)) issue-id) "be-1"))))

(ert-deftest beads-events-test-filter-issue ()
  "The issue filter matches the exact issue id."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 2 200 "close" "be-1" "a")
            (beads-events-test--record 1 100 "close" "be-2" "b"))
    (setq beads-events--filter '(:issue "be-2"))
    (beads-events--render)
    (should (= beads-events--shown 1))
    (should (equal (oref (car (car tabulated-list-entries)) issue-id) "be-2"))))

(ert-deftest beads-events-test-filter-level ()
  "Signal level filters at-or-above the requested severity."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 3 300 "close" "be-1" "a")     ; attention
            (beads-events-test--record 2 200 "update" "be-2" "b")    ; watch
            (beads-events-test--record 1 100 "comment" "be-3" "c"))  ; plain
    (setq beads-events--filter '(:level watch))
    (beads-events--render)
    (should (= beads-events--shown 2))))

(ert-deftest beads-events-test-filter-window ()
  "A window drops records older than the duration."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record
             2 (- (float-time) 60) "close" "be-new" "a")
            (beads-events-test--record
             1 (- (float-time) 7200) "close" "be-old" "b"))
    (setq beads-events--filter '(:window "1h"))
    (beads-events--render)
    (should (= beads-events--shown 1))
    (should (equal (oref (car (car tabulated-list-entries)) issue-id) "be-new"))))

;;; Churn folding

(ert-deftest beads-events-test-churn-folds-plain-records ()
  "Same issue and op inside one bucket fold into a single ×N row."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 3 300 "comment" "be-1" "a")
            (beads-events-test--record 2 250 "comment" "be-1" "b"))
    ;; Both are plain events in the same 900s bucket.
    (should (= beads-events--folded 2))
    (should (= (length tabulated-list-entries) 1))
    (let ((row (car (car tabulated-list-entries))))
      (should (eq (car row) 'churn))
      (should (= (length (nth 4 row)) 2)))))

(ert-deftest beads-events-test-churn-never-folds-signals ()
  "Attention and watch records are never folded."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 3 300 "close" "be-1" "a")
            (beads-events-test--record 2 250 "close" "be-1" "b"))
    (should (= beads-events--folded 0))
    (should (= (length tabulated-list-entries) 2))))

(ert-deftest beads-events-test-toggle-churn-unfolds ()
  "Toggling a churn row unfolds its member records inline."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 3 300 "comment" "be-1" "a")
            (beads-events-test--record 2 250 "comment" "be-1" "b"))
    (let ((key (nth 1 (car (car tabulated-list-entries)))))
      (push key beads-events--expanded)
      (beads-events--render)
      (should (= (length tabulated-list-entries) 3)))))

;;; Header

(ert-deftest beads-events-test-header-line ()
  "The header line reports the window, count, filter and live chip."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 1 100 "create" "be-1" "a"))
    (setq beads-events--filter '(:op "create"))
    (cl-letf (((symbol-function 'beads-live-header-string)
               (lambda (&optional _dir) "● live ∿3/s")))
      (let ((header (beads-events--header-line)))
        (should (string-match-p "events" header))
        (should (string-match-p "last 1h" header))
        (should (string-match-p "op=create" header))
        (should (string-match-p "● live" header))))))

(ert-deftest beads-events-test-header-line-without-live ()
  "With no live layer the header line still renders (no chip)."
  :tags '(:unit)
  (beads-events-test--with-view
      (list (beads-events-test--record 1 100 "create" "be-1" "a"))
    (cl-letf (((symbol-function 'beads-live-header-string) nil))
      (should (stringp (beads-events--header-line))))))

;;; Org export

(ert-deftest beads-events-test-org-string ()
  "The org export renders a table row per record."
  :tags '(:unit)
  (let ((text (beads-events--org-string
               (list (beads-events-test--record 7 100 "close" "be-1" "alice")))))
    (should (string-match-p (regexp-quote "#+TITLE: beads events") text))
    (should (string-match-p (regexp-quote "| Seq | Time | Op | Issue | Actor |")
                            text))
    (should (string-match-p (regexp-quote "| 7 |") text))
    (should (string-match-p (regexp-quote "| close | be-1 | alice")
                            text))))

;;; City timeline

(ert-deftest beads-events-test-city-merge-by-wall-clock ()
  "City merge orders by wall clock, not by per-store sequence numbers."
  :tags '(:unit)
  (let* ((store-a (list (beads-events-test--record 100 100 "create" "a-1" "x")))
         (store-b (list (beads-events-test--record 1 300 "close" "b-1" "y")))
         (merged (beads-events--merge-stores
                  (list (cons "/store/a" store-a) (cons "/store/b" store-b))))
         (records (car merged))
         (store-of (cdr merged)))
    (should (equal (mapcar (lambda (r) (oref r issue-id)) records) '("b-1" "a-1")))
    (should (equal (gethash (car records) store-of) "/store/b"))
    (should (equal (gethash (cadr records) store-of) "/store/a"))))

(ert-deftest beads-events-test-city-renders-store-column ()
  "A city view adds the Store column before Time."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (setq beads-events--city "bright-lights"
          beads-events--store-name "bright-lights")
    (let* ((pair (beads-events--merge-stores
                  (list (cons "/store/a"
                              (list (beads-events-test--record
                                     1 100 "create" "a-1" "x"))))))
           (beads-events--records (car pair))
           (beads-events--store-of (cdr pair)))
      (beads-events--render)
      (should (= (length (cadr (car tabulated-list-entries))) 6))
      (should (equal (aref (cadr (car tabulated-list-entries)) 0) "/store/a")))))

(ert-deftest beads-events-test-session-seam-absent-by-default ()
  "No session tag is invented without the optional resolver seam."
  :tags '(:unit)
  (should (null (beads-events-session-tag "/store/a" "alice")))
  (let ((beads-events-session-table '(("alice" . "sess-1"))))
    (should (equal (beads-events-session-tag "/store/a" "alice") "sess-1"))))

(ert-deftest beads-events-test-session-resolver-hook ()
  "The session resolver hook wins over the static table."
  :tags '(:unit)
  (let ((beads-event-session-resolver
         (list (lambda (_root actor)
                 (when (equal actor "alice") "from-hook"))))
        (beads-events-session-table '(("alice" . "from-table"))))
    (should (equal (beads-events-session-tag "/store/a" "alice") "from-hook"))
    (should (null (beads-events-session-tag "/store/a" "bob")))))

;;; Live append

(ert-deftest beads-events-test-live-append-prepends ()
  "A live record is queued and folded into the buffer on flush."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (let ((old (beads-events-test--record 1 100 "create" "be-old" "a"))
          (new (beads-events-test--record 2 200 "close" "be-new" "b")))
      (setq beads-events--records (list old))
      (beads-events--live-record new)
      (should (= (length beads-events--queue) 1))
      (beads-events--flush (current-buffer))
      (should (null beads-events--queue))
      (should (= (length beads-events--records) 2))
      (should (eq (car beads-events--records) new)))))

(ert-deftest beads-events-test-live-flush-dedupes-by-seq ()
  "A replayed record at/below the applied seq is not duplicated."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-mode)
    (let ((record (beads-events-test--record 5 500 "close" "be-1" "a")))
      (setq beads-events--records (list record))
      (beads-events--live-record record)
      (beads-events--flush (current-buffer))
      (should (= (length beads-events--records) 1)))))

;;; Fallbacks (pure model absent)

(ert-deftest beads-events-test-actor-fallback-is-system ()
  "A record with no actor renders as system."
  :tags '(:unit)
  (let ((record (beads-events-test--record 1 100 "create" "be-1" nil)))
    (should (equal (beads-events--actor record) "system"))))

(ert-deftest beads-events-test-level-and-glyph-fallback ()
  "Local op mapping classifies the seven journal ops."
  :tags '(:unit)
  (should (eq (beads-events--level
               (beads-events-test--record 1 100 "close" "be-1" "a"))
              'attention))
  (should (eq (beads-events--level
               (beads-events-test--record 1 100 "update" "be-1" "a"))
              'watch))
  (should (null (beads-events--level
                 (beads-events-test--record 1 100 "comment" "be-1" "a"))))
  (should (equal (beads-events--glyph
                  (beads-events-test--record 1 100 "close" "be-1" "a"))
                 "■")))

(provide 'beads-events-test)
;;; beads-events-test.el ends here
