;;; beads-event-test.el --- Tests for beads-event -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT unit tests for the pure journal event model (`beads-event.el',
;; plan `plans/beads-events-live/', WI-LIVE-01).  Everything here is
;; `:unit': no `bd', no subprocess, no buffer.  Covers level/glyph,
;; subject, actor description, time and `--since' resolution, the
;; sparse-partial field diff (including an absent `is_blocked' clearing
;; a block and actor-less rows), and churn folding.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-event)

;;; Helpers

(defun beads-event-test--record (&rest args)
  "Make a `beads-event-record' with ARGS."
  (apply #'make-instance 'beads-event-record args))

(defun beads-event-test--issue (&rest args)
  "Make a `beads-issue' with ARGS."
  (apply #'make-instance 'beads-issue args))

(defconst beads-event-test--source
  (or (locate-library "beads-event.el")
      (let ((lib (locate-library "beads-event")))
        (and lib (concat (file-name-sans-extension lib) ".el"))))
  "Absolute path of the beads-event.el under test.")

;;; Ops and levels

(ert-deftest beads-event-test-ops-constant ()
  "The seven journal ops are recorded in bd's documented order."
  :tags '(:unit)
  (should (equal beads-event-ops
                 '("create" "update" "close" "delete"
                   "dep_add" "dep_remove" "comment")))
  (should (= 7 (length beads-event-ops))))

(ert-deftest beads-event-test-level-all-ops ()
  "Every journal op maps to its configured signal level."
  :tags '(:unit)
  (let ((cases '(("create" . nil)
                 ("update" . watch)
                 ("close" . attention)
                 ("delete" . attention)
                 ("dep_add" . attention)
                 ("dep_remove" . nil)
                 ("comment" . nil))))
    (dolist (case cases)
      (should (eq (beads-event-level
                   (beads-event-test--record :op (car case)))
                  (cdr case))))))

(ert-deftest beads-event-test-level-finer-keys-off-diff ()
  "An `update' whose diff is all bookkeeping drops to a plain event."
  :tags '(:unit)
  (let ((record (beads-event-test--record :op "update")))
    (should (eq (beads-event-level record) 'watch))
    (should (null (beads-event-level
                   record
                   '((:field updated-at :kind set :old "a" :new "b")
                     (:field heartbeat-at :kind set :old nil :new "h")))))
    (should (eq (beads-event-level
                 record
                 '((:field status :kind set :old "open" :new "in_progress")))
                'watch))))

(ert-deftest beads-event-test-level-extensible-and-memoized ()
  "`beads-event-levels' is user-extensible and the memo is invalidated."
  :tags '(:unit)
  (should (null (beads-event-level (beads-event-test--record :op "comment"))))
  (let ((beads-event-levels '(("\\`comment\\'" . watch))))
    (should (eq (beads-event-level (beads-event-test--record :op "comment"))
                'watch)))
  (should (null (beads-event-level (beads-event-test--record :op "comment")))))

(ert-deftest beads-event-test-glyph ()
  "Glyphs follow the signal level, and accept a level or a record."
  :tags '(:unit)
  (should (equal (beads-event-glyph 'attention) "■"))
  (should (equal (beads-event-glyph 'watch) "▲"))
  (should (equal (beads-event-glyph nil) " "))
  (should (equal (beads-event-glyph
                  (beads-event-test--record :op "close"))
                 "■"))
  (should (equal (beads-event-glyph
                  (beads-event-test--record :op "comment"))
                 " ")))

;;; Subject and actor

(ert-deftest beads-event-test-subject ()
  "Subject renders \"issue-id  title — op\", and degrades without a title."
  :tags '(:unit)
  (should (equal (beads-event-subject
                  (beads-event-test--record
                   :op "update" :issue-id "be-1"
                   :issue (beads-event-test--issue :id "be-1" :title "Fix it")))
                 "be-1  Fix it — update"))
  (should (equal (beads-event-subject
                  (beads-event-test--record :op "delete" :issue-id "be-1"))
                 "be-1 — delete"))
  (should (equal (beads-event-subject
                  (beads-event-test--record :op "comment" :issue-id "be-1"
                                            :issue (beads-event-test--issue
                                                    :id "be-1" :title "")))
                 "be-1 — comment")))

(ert-deftest beads-event-test-actor-description ()
  "Actor is verbatim, or `system' when absent or empty."
  :tags '(:unit)
  (should (equal (beads-event-actor-description
                  (beads-event-test--record :actor "ec-wisp-21mcmn"))
                 "ec-wisp-21mcmn"))
  (should (equal (beads-event-actor-description
                  (beads-event-test--record :actor ""))
                 "system"))
  (should (equal (beads-event-actor-description
                  (beads-event-test--record))
                 "system")))

;;; Time and --since

(ert-deftest beads-event-test-time ()
  "RFC3339 timestamps parse to the same float as `date-to-time'."
  :tags '(:unit)
  (should (= (beads-event-time
              (beads-event-test--record :ts "2026-10-09T10:21:41Z"))
             (float-time (date-to-time "2026-10-09T10:21:41Z"))))
  (should (= (beads-event-time (beads-event-test--record)) 0))
  (should (= (beads-event-time (beads-event-test--record :ts "not-a-time")) 0)))

(ert-deftest beads-event-test-window-seconds ()
  "Duration windows convert to seconds."
  :tags '(:unit)
  (should (= (beads-event-window-seconds "30s") 30))
  (should (= (beads-event-window-seconds "15m") 900))
  (should (= (beads-event-window-seconds "2h") 7200))
  (should (= (beads-event-window-seconds "7d") 604800))
  (should (= (beads-event-window-seconds "2w") 1209600))
  (should (= (beads-event-window-seconds 42) 42))
  (should (= (beads-event-window-seconds "300") 300))
  (should-error (beads-event-window-seconds "soon")))

(ert-deftest beads-event-test-since-arg ()
  "An explicit seq passes through; a duration resolves to a seq."
  :tags '(:unit)
  (should (= (beads-event-since-arg 7) 7))
  (should (= (beads-event-since-arg "7") 7))
  ;; A duration without records reads from the start.
  (should (= (beads-event-since-arg "2h") 0))
  (let* ((now (float-time (date-to-time "2026-10-09T12:30:00Z")))
         (records (list (beads-event-test--record
                         :seq 1 :ts "2026-10-09T10:00:00Z")
                        (beads-event-test--record
                         :seq 2 :ts "2026-10-09T11:59:00Z")
                        (beads-event-test--record
                         :seq 3 :ts "2026-10-09T12:00:00Z"))))
    (should (= (beads-event-since-arg "2h" records now) 1))
    (should (= (beads-event-since-arg "24h" records now) 0))))

;;; Field diff

(ert-deftest beads-event-test-diff-scalars ()
  "Scalar changes render old and new, and unchanged fields are dropped."
  :tags '(:unit)
  (let ((diff (beads-event-diff
               (beads-event-test--issue :status "open" :assignee nil)
               (beads-event-test--issue :status "in_progress" :assignee "alice"))))
    (should (equal (plist-get (cl-find 'status diff :key (lambda (c) (plist-get c :field)))
                              :old)
                   "open"))
    (should (equal (plist-get (cl-find 'assignee diff :key (lambda (c) (plist-get c :field)))
                              :new)
                   "alice"))
    (should (null (cl-find 'title diff :key (lambda (c) (plist-get c :field)))))
    (should (null (cl-find 'priority diff :key (lambda (c) (plist-get c :field)))))))

(ert-deftest beads-event-test-diff-sparse-partial-clears-block ()
  "An absent `is_blocked' clears a block: diff reads absent as nil."
  :tags '(:unit)
  (let ((diff (beads-event-diff '((id . "be-1") (is_blocked . t))
                                '((id . "be-1")))))
    (let ((change (cl-find 'is-blocked diff :key (lambda (c) (plist-get c :field)))))
      (should change)
      (should (eq (plist-get change :old) t))
      (should (null (plist-get change :new))))))

(ert-deftest beads-event-test-diff-labels ()
  "Label lists diff per label, not as one opaque list."
  :tags '(:unit)
  (let ((diff (beads-event-diff '((id . "be-1") (labels "foo" "bar"))
                                '((id . "be-1") (labels "bar" "baz")))))
    (should (equal (sort (mapcar (lambda (c) (list (plist-get c :kind)
                                                   (or (plist-get c :new)
                                                       (plist-get c :old))))
                                 diff)
                         (lambda (a b) (string< (symbol-name (car a))
                                                (symbol-name (car b)))))
                 '((label-added "baz") (label-removed "foo"))))
    (should (null (beads-event-diff '((id . "be-1") (labels "a"))
                                    '((id . "be-1") (labels "a")))))))

(ert-deftest beads-event-test-diff-delete-tombstone ()
  "A null new snapshot renders a deleted tombstone."
  :tags '(:unit)
  (let ((diff (beads-event-diff (beads-event-test--issue :id "be-9") nil)))
    (should (= (length diff) 1))
    (should (eq (plist-get (car diff) :kind) 'deleted))
    (should (equal (plist-get (car diff) :old) "be-9")))
  (should (null (beads-event-diff nil nil))))

(ert-deftest beads-event-test-record-diff-dep-payload ()
  "Dependency ops render the `dep' payload, not the snapshot."
  :tags '(:unit)
  (let* ((record (beads-event-test--record
                  :op "dep_add" :issue-id "be-d"
                  :dep '((kind . "blocks") (target . "be-b") (metadata . "{}"))))
         (change (car (beads-event-record-diff record))))
    (should (eq (plist-get change :kind) 'dependency-added))
    (should (equal (plist-get change :new) "be-b")))
  (let* ((record (beads-event-test--record
                  :op "dep_remove" :issue-id "be-d"
                  :dep '((kind . "blocks") (target . "be-b"))))
         (change (car (beads-event-record-diff record))))
    (should (eq (plist-get change :kind) 'dependency-removed))
    (should (equal (plist-get change :old) "be-b"))))

(ert-deftest beads-event-test-record-diff-delete-create-comment ()
  "Delete is a tombstone, comment has no diff, create names the issue."
  :tags '(:unit)
  (should (eq (plist-get (car (beads-event-record-diff
                               (beads-event-test--record
                                :op "delete" :issue-id "be-1")))
                         :kind)
              'deleted))
  (should (null (beads-event-record-diff
                 (beads-event-test--record :op "comment" :issue-id "be-1"))))
  (should (equal (plist-get (car (beads-event-record-diff
                                  (beads-event-test--record
                                   :op "create" :issue-id "be-1")))
                            :new)
                 "be-1")))

(ert-deftest beads-event-test-diff-string ()
  "Diff rows render the mockup forms."
  :tags '(:unit)
  (should (equal (beads-event-diff-string
                  '(:field status :kind set :old "open" :new "in_progress"))
                 "status open → in_progress"))
  (should (equal (beads-event-diff-string
                  '(:field assignee :kind set :old nil :new "alice"))
                 "assignee — → alice"))
  (should (equal (beads-event-diff-string
                  '(:field dependencies :kind dependency-added :old nil :new "be-2"))
                 "dependency_added be-2"))
  (should (equal (beads-event-diff-string
                  '(:field labels :kind label-added :old nil :new "foo"))
                 "label \"foo\"")))

;;; Churn folding

(ert-deftest beads-event-test-fold-signal-never-folds ()
  "Signal records are never folded, even when identical."
  :tags '(:unit)
  (let* ((records (list (beads-event-test--record
                         :seq 1 :op "close" :issue-id "be-1"
                         :ts "2026-10-09T10:00:00Z")
                        (beads-event-test--record
                         :seq 2 :op "close" :issue-id "be-1"
                         :ts "2026-10-09T10:00:01Z")))
         (result (beads-event-fold records)))
    (should (= 0 (cdr result)))
    (should (= 2 (length (car result))))
    (should (eq (car (car (car result))) 'event))))

(ert-deftest beads-event-test-fold-coalesces-per-issue-op-bucket ()
  "Same issue and op in one bucket become one churn row; others stay rows."
  :tags '(:unit)
  (let* ((records (list (beads-event-test--record
                         :seq 1 :op "comment" :issue-id "be-1"
                         :ts "2026-10-09T10:00:00Z")
                        (beads-event-test--record
                         :seq 2 :op "comment" :issue-id "be-1"
                         :ts "2026-10-09T10:00:01Z")
                        (beads-event-test--record
                         :seq 3 :op "comment" :issue-id "be-2"
                         :ts "2026-10-09T10:00:02Z")))
         (result (beads-event-fold records 1000000000))
         (rows (car result)))
    ;; One churn row for be-1 and one churn row for be-2.
    (should (= 2 (length rows)))
    (should (= 3 (cdr result)))
    (let ((churn (cl-find "be-1 comment" rows :key (lambda (r) (nth 2 r))
                          :test #'equal)))
      (should churn)
      (should (equal (nth 2 churn) "be-1 comment"))
      (should (= 2 (length (nth 4 churn)))))))

(ert-deftest beads-event-test-fold-respects-bucket ()
  "Different time buckets do not coalesce."
  :tags '(:unit)
  (let* ((records (list (beads-event-test--record
                         :seq 1 :op "comment" :issue-id "be-1"
                         :ts "2026-10-09T10:00:00Z")
                        (beads-event-test--record
                         :seq 2 :op "comment" :issue-id "be-1"
                         :ts "2026-10-09T10:00:30Z")))
         (result (beads-event-fold records 1)))
    (should (= 2 (length (car result))))))

(ert-deftest beads-event-test-fold-newest-first ()
  "Rows are ordered newest first."
  :tags '(:unit)
  (let* ((older (beads-event-test--record
                 :seq 1 :op "create" :issue-id "be-1"
                 :ts "2026-10-09T10:00:00Z"))
         (newer (beads-event-test--record
                 :seq 2 :op "close" :issue-id "be-2"
                 :ts "2026-10-09T12:00:00Z"))
         (result (beads-event-fold (list older newer) 1))
         (rows (car result)))
    (should (eq (car (car rows)) 'event))
    (should (= (oref (nth 1 (car rows)) seq) 2))
    (should (= (oref (car (nth 4 (nth 1 rows))) seq) 1))))

;;; Purity guard

(ert-deftest beads-event-test-pure-source ()
  "beads-event.el contains no process or buffer code."
  :tags '(:unit)
  (should beads-event-test--source)
  (let ((source (with-temp-buffer
                  (insert-file-contents beads-event-test--source)
                  (buffer-string))))
    (dolist (forbidden '("make-process" "start-process"
                         "make-network-process" "call-process"
                         "shell-command" "get-buffer-create"
                         "generate-new-buffer" "with-current-buffer"
                         "set-buffer" "make-temp-file"))
      (should-not (string-match-p (concat "\\b" (regexp-quote forbidden) "\\b")
                                  source)))))

(provide 'beads-event-test)
;;; beads-event-test.el ends here
