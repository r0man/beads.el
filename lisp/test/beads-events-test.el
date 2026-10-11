;;; beads-events-test.el --- Tests for the beads events views -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT `:unit' tests for the beads events views over the `bd events'
;; journal: the timeline and city-timeline views (WI-LIVE-15) plus the
;; per-issue history and read-only rewind views (WI-LIVE-16,
;; `lisp/beads-events-history.el', `lisp/beads-events-rewind.el').
;; Records are injected directly and the live chip is stubbed with
;; `cl-letf', so no subprocess is spawned.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'beads-event)
(require 'beads-types)
(require 'beads-events)
(require 'beads-events-history)
(require 'beads-events-rewind)

;;; Helpers
(require 'beads-pulse)

(defun beads-events-test--issue (&rest args)
  "Build a `beads-issue' for tests from ARGS."
  (apply #'beads-issue
         :id "be-1" :title "A title" :status "open" args))

(defun beads-events-test--record (seq &rest args)
  "Build a `beads-event-record' at SEQ.
If the first of ARGS is a number it is the explicit TIME in seconds;
otherwise the timestamp is derived from SEQ.  The remaining ARGS are
OP ISSUE-ID ACTOR and an optional `:issue' snapshot."
  (let* ((time (if (numberp (car args)) (pop args) (+ seq 1600000000)))
         (op (pop args))
         (issue-id (pop args))
         (actor (pop args)))
    (apply #'beads-event-record
           :seq seq
           :ts (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t)
           :op op
           :issue-id issue-id
           :actor actor
           args)))

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
(defmacro beads-events-test--with-history (records &rest body)
  "Render RECORDS in a fresh history buffer, then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (beads-events-history-mode)
     (setq beads-events-history--store nil
           beads-events-history--issue-id "be-1"
           beads-events-history--records ,records)
     (beads-events-history--render)
     ,@body))

(defun beads-events-test--state->alist (state)
  "Return STATE hash as an id-sorted alist for comparison."
  (let ((pairs (sort (mapcar (lambda (id) (cons id (gethash id state)))
                             (hash-table-keys state))
                     (lambda (a b) (string< (car a) (car b))))))
    pairs))

(defun beads-events-test--live-at (records k baseline)
  "Independent reference reducer for AC-6.
Apply RECORDS with seq <= K over BASELINE (id . wire alist) using the
journal's sparse-merge contract, and return the id-sorted state alist."
  (let ((state (make-hash-table :test 'equal))
        (subset '(id title status priority issue_type owner created_by
                  created_at updated_at is_blocked assignee labels
                  started_at lease_expires_at heartbeat_at closed_at
                  close_reason)))
    (dolist (pair baseline)
      (puthash (car pair) (cdr pair) state))
    (dolist (record (sort (copy-sequence records)
                          (lambda (a b) (< (oref a seq) (oref b seq)))))
      (when (<= (oref record seq) k)
        (let* ((id (oref record issue-id))
               (snap (beads-events-history--issue->snapshot (oref record issue))))
          (if (null snap)
              (puthash id :deleted state)
            (let ((merged (cl-remove-if
                           (lambda (pair) (memq (car pair) subset))
                           (copy-sequence (gethash id state)))))
              (dolist (key subset)
                (when-let* ((pair (assq key snap)))
                  (push (cons key (cdr pair)) merged)))
              (puthash id (nreverse merged) state))))))
    (beads-events-test--state->alist state)))

;;; History

(ert-deftest beads-events-test-history-mode-activation ()
  "`beads-events-history-mode' derives from `special-mode' and is read-only."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-history-mode)
    (should (eq major-mode 'beads-events-history-mode))
    (should (derived-mode-p 'special-mode))
    (should buffer-read-only)))

(ert-deftest beads-events-test-history-records-for-issue ()
  "History keeps only the issue's records, oldest-first."
  :tags '(:unit)
  (let ((records (list (beads-events-test--record 30 "close" "be-2" "b")
                       (beads-events-test--record 10 "create" "be-1" "a")
                       (beads-events-test--record 20 "update" "be-1" "a")
                       (beads-events-test--record 5 "create" "be-1" "a"))))
    (should (equal (mapcar (lambda (r) (oref r seq))
                           (beads-events-history--records-for-issue records "be-1"))
                   '(5 10 20)))))

(ert-deftest beads-events-test-history-update-diff ()
  "An update renders its field-level diff."
  :tags '(:unit)
  (let ((previous (beads-events-test--issue :status "open")))
    (let ((lines (beads-events-history--diff-lines
                  (beads-events-test--record
                   20 "update" "be-1" "alice"
                   :issue (beads-events-test--issue
                           :status "in_progress" :assignee "alice"))
                  previous)))
      (should (seq-some (lambda (l)
                          (string-match-p "status open → in_progress" l))
                        lines))
      (should (seq-some (lambda (l) (string-match-p "assignee .* → alice" l))
                        lines)))))

(ert-deftest beads-events-test-history-delete-tombstone ()
  "A delete renders a tombstone record, not a field diff."
  :tags '(:unit)
  (let ((lines (beads-events-history--diff-lines
                (beads-events-test--record 40 "delete" "be-1" "alice")
                (beads-events-test--issue :status "closed"))))
    (should (equal lines '("deleted be-1")))))

(ert-deftest beads-events-test-history-render ()
  "Rendering the history shows each record and its diff, newest-first."
  :tags '(:unit)
  (beads-events-test--with-history
      (list (beads-events-test--record
             10 "create" "be-1" "alice"
             :issue (beads-events-test--issue :status "open"))
            (beads-events-test--record
             20 "update" "be-1" "alice"
             :issue (beads-events-test--issue :status "in_progress"))
            (beads-events-test--record
             30 "close" "be-1" "alice"
             :issue (beads-events-test--issue :status "closed")))
    (let ((text (buffer-string)))
      (should (string-match-p "3 events" text))
      (should (string-match-p "status open → in_progress" text))
      (should (string-match-p "status in_progress → closed" text))
      ;; newest-first: the close block precedes the create block.
      (should (< (string-match-p "close" text)
                 (string-match-p "create" text))))))

(ert-deftest beads-events-test-history-inject-via-entry-point ()
  "The entry point accepts injected records and renders them."
  :tags '(:unit)
  (let ((records (list (beads-events-test--record
                        10 "create" "be-1" "alice"
                        :issue (beads-events-test--issue :status "open")))))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (beads-events-history "be-1" nil records))
    (with-current-buffer "*beads-events-history[beads][be-1]*"
      (should (= 1 (length beads-events-history--records)))
      (should (string-match-p "create" (buffer-string))))))

;;; Rewind target parsing

(ert-deftest beads-events-test-rewind-parse-target ()
  "Absolute and relative rewind targets resolve against the head."
  :tags '(:unit)
  (should (eq 'live (beads-events-rewind--parse-target "" 100)))
  (should (eq 'live (beads-events-rewind--parse-target "   " 100)))
  (should (= 40 (beads-events-rewind--parse-target "40" 100)))
  (should (= 95 (beads-events-rewind--parse-target "-5" 100)))
  (should (= 100 (beads-events-rewind--parse-target "+5" 100)))
  (should (= 0 (beads-events-rewind--parse-target "-500" 100)))
  (should (eq 'live (beads-events-rewind--parse-target nil 100))))

(ert-deftest beads-events-test-rewind-parse-target-invalid ()
  "A malformed target signals a user error."
  :tags '(:unit)
  (should-error (beads-events-rewind--parse-target "junk" 100)
                :type 'user-error))

;;; Rewind replay and AC-6

(defun beads-events-test--baseline ()
  "Return the independent AC-6 baseline state."
  '(("be-1" . ((id . "be-1") (title . "One") (status . "open")
               (description . "keep me") (priority . 2)))))

(defun beads-events-test--script ()
  "Return the AC-6 scripted record sequence."
  (list
   (beads-events-test--record
    10 "update" "be-1" "alice"
    :issue (beads-events-test--issue :id "be-1" :title "One"
                                     :status "in_progress" :priority 2))
   (beads-events-test--record
    20 "close" "be-1" "alice"
    :issue (beads-events-test--issue :id "be-1" :title "One"
                                     :status "closed" :priority 2))
   (beads-events-test--record
    30 "update" "be-1" "bob"
    :issue (beads-events-test--issue :id "be-1" :title "One"
                                     :status "closed" :priority 2
                                     :assignee "alice"))
   (beads-events-test--record
    40 "create" "be-2" "bob"
    :issue (beads-events-test--issue :id "be-2" :title "Two" :status "open"))
   (beads-events-test--record 50 "delete" "be-2" "bob")))

(ert-deftest beads-events-test-rewind-state-at-fields ()
  "Rewind at K shows the state as of K, preserving non-wire fields."
  :tags '(:unit)
  (let* ((records (beads-events-test--script))
         (baseline (beads-events-test--baseline))
         (at-20 (beads-events-rewind--state-at records baseline 20))
         (at-30 (beads-events-rewind--state-at records baseline 30))
         (at-50 (beads-events-rewind--state-at records baseline 50)))
    ;; At K=20: closed, no assignee yet.
    (should (equal (alist-get 'status (gethash "be-1" at-20)) "closed"))
    (should (null (alist-get 'assignee (gethash "be-1" at-20))))
    ;; Non-wire fields survive the sparse merge.
    (should (equal (alist-get 'description (gethash "be-1" at-20)) "keep me"))
    ;; At K=30: the assignee update landed.
    (should (equal (alist-get 'assignee (gethash "be-1" at-30)) "alice"))
    ;; At K=50: be-2 is a tombstone, be-1 still present.
    (should (eq (gethash "be-2" at-50) :deleted))
    (should (consp (gethash "be-1" at-50)))))

(ert-deftest beads-events-test-rewind-ac6-property ()
  "AC-6: rewind at K equals the independently reduced live state at K.
The baseline is seeded independently and K=30 is well above the start
of the scripted window, so the assertion is not vacuous."
  :tags '(:unit)
  (let* ((records (beads-events-test--script))
         (baseline (beads-events-test--baseline)))
    (dolist (k '(20 30 40 50))
      (should (equal (beads-events-test--state->alist
                      (beads-events-rewind--state-at records baseline k))
                     (beads-events-test--live-at records k baseline))))
    ;; The head state must still be live: be-1 assigned, be-2 deleted.
    (let ((head (beads-events-rewind--state-at records baseline 50)))
      (should (equal (alist-get 'assignee (gethash "be-1" head)) "alice"))
      (should (eq (gethash "be-2" head) :deleted)))))

(ert-deftest beads-events-test-rewind-below-baseline-not-vacuous ()
  "Rewind below the first record keeps the seeded baseline."
  :tags '(:unit)
  (let ((state (beads-events-rewind--state-at
                (beads-events-test--script)
                (beads-events-test--baseline)
                0)))
    (should (= 1 (hash-table-count state)))
    (should (equal (alist-get 'status (gethash "be-1" state)) "open"))))

;;; Rewind view

(ert-deftest beads-events-test-rewind-render-and-step ()
  "The rewind buffer renders read-only, and g/G step by record."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-rewind-mode)
    (beads-events-rewind--configure
     nil (beads-events-test--script) (beads-events-test--baseline))
    (beads-events-rewind--set-seq 30)
    (should buffer-read-only)
    (should (= beads-events-rewind--seq 30))
    (let ((text (buffer-string)))
      (should (string-match-p "State at seq 30" text))
      (should (string-match-p "be-1" text)))
    ;; g steps to the next record seq, G back.
    (beads-events-rewind-forward)
    (should (= beads-events-rewind--seq 40))
    (beads-events-rewind-backward)
    (should (= beads-events-rewind--seq 30))
    (beads-events-rewind-backward)
    (should (= beads-events-rewind--seq 20))))

(ert-deftest beads-events-test-rewind-header-face ()
  "The rewind header carries the distinct rewind face."
  :tags '(:unit)
  (with-temp-buffer
    (beads-events-rewind-mode)
    (let ((header (beads-events-rewind--header-line)))
      (should (string-match-p "REWIND" header))
      (should (eq (get-text-property 1 'face header) 'beads-events-rewind)))))

;;; Never writes

(defun beads-events-test--source-file (feature)
  "Return the readable .el source file of FEATURE, or nil."
  (let ((file (locate-library (symbol-name feature))))
    (when file
      (if (string-suffix-p ".elc" file)
          (let ((el (concat (file-name-sans-extension file) ".el")))
            (and (file-readable-p el) el))
        file))))

(ert-deftest beads-events-test-history-rewind-never-write ()
  "Neither module executes a `bd' command or spawns a process."
  :tags '(:unit)
  (dolist (feature '(beads-events-history beads-events-rewind))
    (let* ((file (beads-events-test--source-file feature))
           (source (and file
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string)))))
      (should source)
      (should-not (string-match-p "beads-command-execute" source))
      (should-not (string-match-p
                   "make-process\\|start-process\\|call-process" source)))))

(defmacro beads-events-test--with-clean-notify (&rest body)
  "Run BODY with the notification rate table and mode reset."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (clrhash beads-events-notify--last)
         ,@body)
     (beads-events-notify-mode -1)
     (clrhash beads-events-notify--last)))

(defun beads-events-notify-test--record (seq op issue-id)
  "Build a `beads-event-record' at SEQ with OP and ISSUE-ID."
  (beads-event-record :seq seq :op op :issue-id issue-id
                      :ts "2026-10-10T12:00:00Z" :actor "alice"))

;;; Notifications

(ert-deftest beads-events-test-notify-inert-when-disabled ()
  "The notification handler raises nothing while the mode is off."
  :tags '(:unit)
  (beads-events-test--with-clean-notify
    (let ((calls 0))
      (cl-letf (((symbol-function 'beads-events-notify--display)
                 (lambda (_title _body) (setq calls (1+ calls)))))
        (should-not (bound-and-true-p beads-events-notify-mode))
        (beads-events-notify-handler (beads-events-notify-test--record 1 "close" "be-1")
                                     "/tmp/store/")
        (should (= calls 0))))))

(ert-deftest beads-events-test-notify-fires-on-configured-op ()
  "An enabled mode notifies once for a configured op."
  :tags '(:unit)
  (beads-events-test--with-clean-notify
    (beads-events-notify-mode 1)
    (let ((calls 0) (seen nil))
      (cl-letf (((symbol-function 'beads-events-notify--display)
                 (lambda (title body) (setq calls (1+ calls)
                                            seen (list title body)))))
        (beads-events-notify-handler (beads-events-notify-test--record 1 "close" "be-42")
                                     "/tmp/store/")
        (should (= calls 1))
        (should (string-match-p "beads-live" (car seen)))
        (should (string-match-p "be-42" (cadr seen)))
        (should (string-match-p "close" (cadr seen)))))))

(ert-deftest beads-events-test-notify-ignores-unconfigured-op ()
  "An op outside `beads-events-notify-ops' raises nothing."
  :tags '(:unit)
  (beads-events-test--with-clean-notify
    (beads-events-notify-mode 1)
    (let ((calls 0))
      (cl-letf (((symbol-function 'beads-events-notify--display)
                 (lambda (_title _body) (setq calls (1+ calls)))))
        (beads-events-notify-handler (beads-events-notify-test--record 1 "comment" "be-1")
                                     "/tmp/store/")
        (should (= calls 0))))))

(ert-deftest beads-events-test-notify-dep-add-alias ()
  "The default op set selects a `dep_add' record via its alias."
  :tags '(:unit)
  (beads-events-test--with-clean-notify
    (beads-events-notify-mode 1)
    (let ((calls 0))
      (cl-letf (((symbol-function 'beads-events-notify--display)
                 (lambda (_title _body) (setq calls (1+ calls)))))
        (beads-events-notify-handler (beads-events-notify-test--record 1 "dep_add" "be-1")
                                     "/tmp/store/")
        (should (= calls 1))))))

(ert-deftest beads-events-test-notify-rate-limited-per-issue ()
  "A second notification for the same issue is rate-limited."
  :tags '(:unit)
  (beads-events-test--with-clean-notify
    (beads-events-notify-mode 1)
    (let ((calls 0))
      (cl-letf (((symbol-function 'beads-events-notify--rate-limit)
                 (lambda () 60))
                ((symbol-function 'beads-events-notify--display)
                 (lambda (_title _body) (setq calls (1+ calls)))))
        (beads-events-notify-handler
         (beads-events-notify-test--record 1 "close" "be-same") "/tmp/store/")
        (beads-events-notify-handler
         (beads-events-notify-test--record 2 "close" "be-same") "/tmp/store/")
        (should (= calls 1))
        (beads-events-notify-handler
         (beads-events-notify-test--record 3 "close" "be-other") "/tmp/store/")
        (should (= calls 2))))))

(ert-deftest beads-events-test-notify-mode-toggles-hook ()
  "Enabling adds the handler to `beads-event-hooks'; disabling removes it."
  :tags '(:unit)
  (beads-events-test--with-clean-notify
    (beads-events-notify-mode 1)
    (should (memq 'beads-events-notify-handler beads-event-hooks))
    (beads-events-notify-mode -1)
    (should-not (memq 'beads-events-notify-handler beads-event-hooks))))

;;; Pulse

(ert-deftest beads-events-test-pulse-line-empty ()
  "The pulse string is empty with no published or live stores."
  :tags '(:unit)
  (clrhash beads-pulse--cities)
  (should (string-empty-p (beads-pulse-mode-line-string))))

(ert-deftest beads-events-test-pulse-segment-published ()
  "A published store contributes an abbreviated, counted segment."
  :tags '(:unit)
  (clrhash beads-pulse--cities)
  (let ((buf (generate-new-buffer "beads-pulse-test")))
    (unwind-protect
        (progn
          (beads-pulse-publish "/tmp/beads-store" buf "beads.el"
                               :open 3 :inflight 5 :blocked 12 :seq 1047)
          (let ((line (beads-pulse-mode-line-string)))
            (should (string-match-p "beads\\[" line))
            (should (string-match-p "be " line))
            (should (string-match-p "3·5·12" line))))
      (kill-buffer buf)
      (clrhash beads-pulse--cities))))

(ert-deftest beads-events-test-pulse-redisplay-safe ()
  "The pulse string never starts a process or runs a command.
Any `bd' execution at redisplay would signal here."
  :tags '(:unit)
  (clrhash beads-pulse--cities)
  (let ((buf (generate-new-buffer "beads-pulse-test")))
    (unwind-protect
        (progn
          (beads-pulse-publish "/tmp/beads-store" buf "beads.el"
                               :open 1 :inflight 0 :blocked 0 :seq 9)
          (cl-letf (((symbol-function 'beads-command-execute)
                     (lambda (&rest _) (error "pulse ran beads-command-execute")))
                    ((symbol-function 'call-process)
                     (lambda (&rest _) (error "pulse ran call-process")))
                    ((symbol-function 'make-process)
                     (lambda (&rest _) (error "pulse ran make-process")))
                    ((symbol-function 'start-process)
                     (lambda (&rest _) (error "pulse ran start-process"))))
            (should (stringp (beads-pulse-mode-line-string)))
            (should (beads-pulse-mode-line-string))))
      (kill-buffer buf)
      (clrhash beads-pulse--cities))))

(ert-deftest beads-events-test-pulse-record-samples ()
  "`beads-pulse-record' keeps a bounded oldest-first sample ring."
  :tags '(:unit)
  (clrhash beads-pulse--samples)
  (beads-pulse-record "/tmp/beads-store" 1)
  (beads-pulse-record "/tmp/beads-store" 5)
  (beads-pulse-record "/tmp/beads-store" 3)
  (should (equal '(1 5 3) (beads-pulse-store-samples "/tmp/beads-store")))
  (clrhash beads-pulse--samples))

(ert-deftest beads-events-test-pulse-sparkline ()
  "The sparkline maps a range onto the eight levels."
  :tags '(:unit)
  (should (string-empty-p (beads-pulse-sparkline nil)))
  (should (string= "▁▁" (beads-pulse-sparkline '(4 4))))
  (should (string= "▁█" (beads-pulse-sparkline '(0 1))))
  (should (= 5 (length (beads-pulse-sparkline '(1 2 3 4 5))))))

(provide 'beads-events-test)
;;; beads-events-test.el ends here
