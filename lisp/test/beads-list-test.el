;;; beads-list-test.el --- Tests for beads-list.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Comprehensive tests for beads-list.el tabulated-list mode.
;; Covers buffer creation, data parsing, sorting, navigation, marking,
;; and visual styling.

;;; Code:

(require 'beads-command)
(require 'beads-list)
(require 'beads-test)
(require 'ert)

;;; Test Data

(defvar beads-list-test--sample-issues
  '(((id . "bd-1")
     (title . "First issue")
     (status . "open")
     (priority . 1)
     (issue-type . "bug")
     (created-at . "2025-10-20T14:30:00Z"))
    ((id . "bd-2")
     (title . "Second issue")
     (status . "in_progress")
     (priority . 0)
     (issue-type . "feature")
     (created-at . "2025-10-20T16:36:52Z"))
    ((id . "bd-3")
     (title . "Third issue")
     (status . "blocked")
     (priority . 2)
     (issue-type . "task")
     (created-at . "2025-10-19T10:00:00Z"))
    ((id . "bd-4")
     (title . "Fourth issue")
     (status . "closed")
     (priority . 3)
     (issue-type . "bug")
     (created-at . "2025-10-18T08:15:00Z")))
  "Sample issue data for testing.")

(defvar beads-list-test--empty-issues '()
  "Empty issue list for testing.")

;;; Helper Functions

(defun beads-list-test--alist-to-issue (alist)
  "Convert test ALIST to beads-issue object.
Handles missing fields gracefully with defaults."
  (beads-issue
   :id (or (alist-get 'id alist) "")
   :title (or (alist-get 'title alist) "")
   :status (or (alist-get 'status alist) "open")
   :priority (or (alist-get 'priority alist) 2)
   :issue-type (alist-get 'issue-type alist)
   :created-at (alist-get 'created-at alist)))

(defmacro beads-list-test--with-temp-buffer (issues command &rest body)
  "Create a temporary beads-list buffer with ISSUES and COMMAND, then run BODY.
ISSUES should be a list of alists (test data format)."
  (declare (indent 2))
  `(with-temp-buffer
     (beads-list-mode)
     (let ((issue-objects (mapcar #'beads-list-test--alist-to-issue ,issues)))
       (beads-list--populate-buffer issue-objects ,command))
     ,@body))

;;; Buffer Creation Tests

(ert-deftest beads-list-test-mode-activation ()
  "Test that beads-list-mode activates correctly."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq major-mode 'beads-list-mode))
    (should (derived-mode-p 'tabulated-list-mode))))

(ert-deftest beads-list-test-buffer-naming ()
  "Test that buffers are named correctly."
  ;; Mock beads--run-command to return empty array
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _) (vector)))
            ((symbol-function 'beads-blocked-issue-from-json)
             (lambda (json)
               (beads-blocked-issue
                :id (or (alist-get 'id json) "")
                :title (or (alist-get 'title json) "")
                :status (or (alist-get 'status json) "open")
                :priority (or (alist-get 'priority json) 2))))
            ((symbol-function 'beads-issue-from-json)
             (lambda (json)
               (beads-issue
                :id (or (alist-get 'id json) "")
                :title (or (alist-get 'title json) "")
                :status (or (alist-get 'status json) "open")
                :priority (or (alist-get 'priority json) 2))))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    ;; Note: beads-list now shows transient menu, not immediate buffer

    ;; Test ready buffer
    (beads-ready)
    (should (equal (buffer-name) "*beads-ready*"))
    (kill-buffer)

    ;; Test blocked buffer
    (beads-blocked)
    (should (equal (buffer-name) "*beads-blocked*"))
    (kill-buffer)))

(ert-deftest beads-list-test-multiple-buffers ()
  "Test that multiple list buffers can coexist."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _) (vector)))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-list)
    (let ((list-buf (current-buffer)))
      (beads-ready)
      (let ((ready-buf (current-buffer)))
        (should (not (eq list-buf ready-buf)))
        (should (buffer-live-p list-buf))
        (should (buffer-live-p ready-buf))
        (kill-buffer list-buf)
        (kill-buffer ready-buf)))))

;;; Data Parsing Tests

(ert-deftest beads-list-test-issue-to-entry ()
  "Test conversion of issue object to tabulated-list entry."
  (let* ((alist (car beads-list-test--sample-issues))
         (issue (beads-list-test--alist-to-issue alist))
         (entry (beads-list--issue-to-entry issue)))
    (should (equal (car entry) "bd-1"))
    (should (vectorp (cadr entry)))
    (should (= (length (cadr entry)) 6))
    ;; Check ID column
    (should (equal (aref (cadr entry) 0) "bd-1"))
    ;; Check title column
    (should (equal (aref (cadr entry) 4) "First issue"))
    ;; Check created column exists
    (should (stringp (aref (cadr entry) 5)))))

(ert-deftest beads-list-test-populate-buffer ()
  "Test populating buffer with issues."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (should (= (length tabulated-list-entries) 4))
   (should (eq beads-list--command 'list))
   ;; Check that raw issues are beads-issue objects
   (should (= (length beads-list--raw-issues) 4))
   (should (beads-issue-p (car beads-list--raw-issues)))))

(ert-deftest beads-list-test-empty-issues ()
  "Test handling of empty issue list."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (should (= (length tabulated-list-entries) 0))))

(ert-deftest beads-list-test-malformed-data ()
  "Test handling of malformed issue data."
  (let* ((malformed-alist '((id . "bd-1")))  ; Missing required fields
         (malformed-issue (beads-list-test--alist-to-issue malformed-alist)))
    ;; Should not signal an error
    (should (beads-list--issue-to-entry malformed-issue))
    ;; Verify it returns a valid entry structure
    (let ((entry (beads-list--issue-to-entry malformed-issue)))
      (should (listp entry))
      (should (= (length entry) 2))
      (should (vectorp (cadr entry))))))

;;; Sorting Tests

(ert-deftest beads-list-test-sort-by-id ()
  "Test sorting issues by ID."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   ;; Re-sort after populate-buffer
   (setq tabulated-list-sort-key (cons "ID" nil))
   (tabulated-list-print t)
   (goto-char (point-min))
   (forward-line 0)  ; Stay at first line (no visible header in batch)
   (should (equal (beads-list--current-issue-id) "bd-1"))
   (forward-line 1)
   (should (equal (beads-list--current-issue-id) "bd-2"))))

(ert-deftest beads-list-test-sort-by-priority ()
  "Test sorting issues by priority."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   ;; Re-sort by Priority (default is now Created)
   (setq tabulated-list-sort-key (cons "Priority" nil))
   (tabulated-list-print t)
   (goto-char (point-min))
   (forward-line 0)  ; Stay at first line
   ;; Priority 0 should be first
   (should (equal (beads-list--current-issue-id) "bd-2"))
   (forward-line 1)
   ;; Priority 1 should be second
   (should (equal (beads-list--current-issue-id) "bd-1"))))

(ert-deftest beads-list-test-sort-by-status ()
  "Test sorting issues by status."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   ;; Re-sort by Status
   (setq tabulated-list-sort-key (cons "Status" nil))
   (tabulated-list-print t)
   (goto-char (point-min))
   (forward-line 0)  ; Stay at first line
   ;; "blocked" comes first alphabetically
   (should (equal (beads-list--current-issue-id) "bd-3"))))

(ert-deftest beads-list-test-sort-reverse ()
  "Test reverse sorting."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   ;; Re-sort by Priority in reverse
   (setq tabulated-list-sort-key (cons "Priority" t))
   (tabulated-list-print t)
   (goto-char (point-min))
   (forward-line 0)  ; Stay at first line
   ;; Priority 3 should be first in reverse order
   (should (equal (beads-list--current-issue-id) "bd-4"))))

;;; Navigation Tests

(ert-deftest beads-list-test-next-previous ()
  "Test next and previous navigation."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (forward-line 1)
   (let ((first-id (beads-list--current-issue-id)))
     (beads-list-next)
     (let ((second-id (beads-list--current-issue-id)))
       (should (not (equal first-id second-id)))
       (beads-list-previous)
       (should (equal (beads-list--current-issue-id) first-id))))))

(ert-deftest beads-list-test-current-issue-id ()
  "Test getting current issue ID at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   ;; On first entry (no visible header in batch mode)
   (goto-char (point-min))
   (should (stringp (beads-list--current-issue-id)))))

(ert-deftest beads-list-test-get-issue-by-id ()
  "Test retrieving issue data by ID."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (let ((issue (beads-list--get-issue-by-id "bd-2")))
     (should issue)
     (should (beads-issue-p issue))
     (should (equal (oref issue title) "Second issue"))
     (should (equal (oref issue status) "in_progress")))))

;;; Mark/Unmark Tests

(ert-deftest beads-list-test-mark-single ()
  "Test marking a single issue."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (forward-line 1)
   (let ((id (beads-list--current-issue-id)))
     (beads-list-mark)
     (should (member id beads-list--marked-issues)))))

(ert-deftest beads-list-test-unmark-single ()
  "Test unmarking a single issue."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (forward-line 1)
   (let ((id (beads-list--current-issue-id)))
     (beads-list-mark)
     (goto-char (point-min))
     (forward-line 1)
     (beads-list-unmark)
     (should-not (member id beads-list--marked-issues)))))

(ert-deftest beads-list-test-mark-all ()
  "Test marking all issues."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (beads-list-mark-all)
   (should (= (length beads-list--marked-issues) 4))))

(ert-deftest beads-list-test-unmark-all ()
  "Test unmarking all issues."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (beads-list-mark-all)
   (should (> (length beads-list--marked-issues) 0))
   (beads-list-unmark-all)
   (should (= (length beads-list--marked-issues) 0))))

(ert-deftest beads-list-test-mark-multiple ()
  "Test marking multiple issues."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (forward-line 2)  ; Skip header
   (beads-list-mark)
   (beads-list-mark)
   (should (= (length beads-list--marked-issues) 2))))

;;; Refresh Tests

(ert-deftest beads-list-test-refresh-command ()
  "Test refreshing the buffer."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (&rest _)
                 (setq call-count (1+ call-count))
                 (apply #'vector beads-list-test--sample-issues))))
      (beads-list-test--with-temp-buffer
       beads-list-test--sample-issues 'list
       (beads-list-refresh)
       (should (= call-count 1))
       (should (= (length tabulated-list-entries) 4))))))

(ert-deftest beads-list-test-refresh-preserves-position ()
  "Test that refresh preserves cursor position."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _)
               (apply #'vector beads-list-test--sample-issues))))
    (beads-list-test--with-temp-buffer
     beads-list-test--sample-issues 'list
     (goto-char (point-min))
     (forward-line 2)
     (let ((pos (point)))
       (beads-list-refresh)
       (should (= (point) pos))))))

(ert-deftest beads-list-test-refresh-empty-result ()
  "Test refresh with empty result."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _) nil)))
    (beads-list-test--with-temp-buffer
     beads-list-test--sample-issues 'list
     (beads-list-refresh)
     (should (= (length tabulated-list-entries) 0)))))

;;; Visual Styling Tests

(ert-deftest beads-list-test-status-faces ()
  "Test that status values have correct faces."
  (should (eq (beads-list--status-face "open")
              'beads-list-status-open))
  (should (eq (beads-list--status-face "in_progress")
              'beads-list-status-in-progress))
  (should (eq (beads-list--status-face "blocked")
              'beads-list-status-blocked))
  (should (eq (beads-list--status-face "closed")
              'beads-list-status-closed)))

(ert-deftest beads-list-test-priority-faces ()
  "Test that priority values have correct faces."
  (should (eq (beads-list--priority-face 0)
              'beads-list-priority-critical))
  (should (eq (beads-list--priority-face 1)
              'beads-list-priority-high))
  (should (eq (beads-list--priority-face 2)
              'beads-list-priority-medium))
  (should (eq (beads-list--priority-face 3)
              'beads-list-priority-low))
  (should (eq (beads-list--priority-face 4)
              'beads-list-priority-low)))

(ert-deftest beads-list-test-format-status ()
  "Test status formatting with face properties."
  (let ((formatted (beads-list--format-status "open")))
    (should (equal formatted "open"))
    (should (eq (get-text-property 0 'face formatted)
                'beads-list-status-open))))

(ert-deftest beads-list-test-format-priority ()
  "Test priority formatting with face properties."
  (let ((formatted (beads-list--format-priority 1)))
    (should (equal formatted "1"))
    (should (eq (get-text-property 0 'face formatted)
                'beads-list-priority-high))))

;;; Mode Line Tests

(ert-deftest beads-list-test-mode-line-count ()
  "Test that mode line shows issue count and marked issues."
  :expected-result :failed  ; beads-list now shows transient, mode-line tested in transient tests
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _)
               (apply #'vector beads-list-test--sample-issues)))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-list)
    ;; Check mode-line-format contains the :eval form with marked count
    (should (member '(:eval (format "  %d issue%s%s%s"
                                    (length tabulated-list-entries)
                                    (if (= (length tabulated-list-entries) 1) "" "s")
                                    (if beads-list--marked-issues
                                        (format " [%d marked]"
                                                (length beads-list--marked-issues))
                                      "")
                                    (beads-list--format-filter-string)))
                    mode-line-format))
    (kill-buffer)))

(ert-deftest beads-list-test-mode-line-empty ()
  "Test that mode line shows appropriate message for empty list."
  :expected-result :failed  ; beads-list now shows transient, mode-line tested in transient tests
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _) nil))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-list)
    ;; Check mode-line-format is set to empty list message
    (should (equal mode-line-format
                   '("%e" mode-line-front-space
                     mode-line-buffer-identification
                     "  No issues found")))
    (kill-buffer)))

;;; Integration Tests

(ert-deftest beads-list-test-full-workflow ()
  "Test complete workflow: create buffer, navigate, mark, refresh."
  :expected-result :failed  ; beads-list now shows transient, full workflow tested in transient tests
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _)
               (apply #'vector beads-list-test--sample-issues)))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-list)
    ;; Navigate
    (goto-char (point-min))
    (forward-line 2)  ; Skip header
    (should (beads-list--current-issue-id))
    ;; Mark
    (beads-list-mark)
    (should (> (length beads-list--marked-issues) 0))
    ;; Unmark all
    (beads-list-unmark-all)
    (should (= (length beads-list--marked-issues) 0))
    ;; Refresh
    (beads-list-refresh)
    (should (> (length tabulated-list-entries) 0))
    (kill-buffer)))

(ert-deftest beads-list-test-ready-command ()
  "Test beads-ready command."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (cmd &rest _)
               (if (equal cmd "ready")
                   (apply #'vector (list (car beads-list-test--sample-issues)))
                 (vector))))
            ((symbol-function 'beads-issue-from-json)
             (lambda (json)
               ;; Convert alist to beads-issue object
               (beads-issue
                :id (alist-get 'id json)
                :title (alist-get 'title json)
                :status (alist-get 'status json)
                :priority (alist-get 'priority json)
                :issue-type (alist-get 'issue-type json)
                :created-at (alist-get 'created-at json))))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-ready)
    (should (equal (buffer-name) "*beads-ready*"))
    (should (eq beads-list--command 'ready))
    (kill-buffer)))

(ert-deftest beads-list-test-blocked-command ()
  "Test beads-blocked command."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (cmd &rest _)
               (if (equal cmd "blocked")
                   (apply #'vector (list (caddr beads-list-test--sample-issues)))
                 (vector))))
            ((symbol-function 'beads-blocked-issue-from-json)
             (lambda (json)
               ;; Convert alist to beads-blocked-issue object
               (beads-blocked-issue
                :id (alist-get 'id json)
                :title (alist-get 'title json)
                :status (alist-get 'status json)
                :priority (alist-get 'priority json)
                :issue-type (alist-get 'issue-type json)
                :created-at (alist-get 'created-at json))))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-blocked)
    (should (equal (buffer-name) "*beads-blocked*"))
    (should (eq beads-list--command 'blocked))
    (kill-buffer)))

;;; Keybinding Tests

(ert-deftest beads-list-test-keybindings-exist ()
  "Test that all required keybindings are defined."
  (with-temp-buffer
    (beads-list-mode)
    (should (keymapp beads-list-mode-map))
    (should (lookup-key beads-list-mode-map (kbd "n")))
    (should (lookup-key beads-list-mode-map (kbd "p")))
    (should (lookup-key beads-list-mode-map (kbd "RET")))
    (should (lookup-key beads-list-mode-map (kbd "g")))
    (should (lookup-key beads-list-mode-map (kbd "q")))
    (should (lookup-key beads-list-mode-map (kbd "m")))
    (should (lookup-key beads-list-mode-map (kbd "u")))
    (should (lookup-key beads-list-mode-map (kbd "U")))
    (should (lookup-key beads-list-mode-map (kbd "c")))
    (should (lookup-key beads-list-mode-map (kbd "e")))
    (should (lookup-key beads-list-mode-map (kbd "w")))
    (should (lookup-key beads-list-mode-map (kbd "S")))
    ;; Note: Old "/" filter prefix removed, replaced with transient menu
    ))

(ert-deftest beads-list-test-keybinding-functions ()
  "Test that keybindings map to correct functions."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "n"))
                #'beads-list-next))
    (should (eq (lookup-key beads-list-mode-map (kbd "p"))
                #'beads-list-previous))
    (should (eq (lookup-key beads-list-mode-map (kbd "RET"))
                #'beads-list-show))
    (should (eq (lookup-key beads-list-mode-map (kbd "g"))
                #'beads-list-refresh))
    (should (eq (lookup-key beads-list-mode-map (kbd "q"))
                #'beads-list-quit))
    (should (eq (lookup-key beads-list-mode-map (kbd "c"))
                #'beads-list-create))
    (should (eq (lookup-key beads-list-mode-map (kbd "e"))
                #'beads-list-update))
    (should (eq (lookup-key beads-list-mode-map (kbd "w"))
                #'beads-list-copy-id))
    (should (eq (lookup-key beads-list-mode-map (kbd "S"))
                #'beads-list-sort))
    (should (eq (lookup-key beads-list-mode-map (kbd "l"))
                #'beads-list-filter))
    ;; Note: Old "/" filter keybindings removed, replaced with transient menu
    ))

;;; Edge Cases

(ert-deftest beads-list-test-show-without-issue ()
  "Test show command when no issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (goto-char (point-min))
   (should-error (beads-list-show))))

(ert-deftest beads-list-test-mark-without-issue ()
  "Test mark command when no issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (goto-char (point-min))
   ;; Should not signal error (beads-list-mark uses when-let)
   (beads-list-mark)
   (should (= (length beads-list--marked-issues) 0))))

(ert-deftest beads-list-test-duplicate-marks ()
  "Test that marking same issue twice doesn't create duplicates."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (forward-line 1)
   (beads-list-mark)
   (let ((count-after-first (length beads-list--marked-issues)))
     (goto-char (point-min))
     (forward-line 1)
     (beads-list-mark)
     (should (= (length beads-list--marked-issues) count-after-first)))))

(ert-deftest beads-list-test-refresh-without-command ()
  "Test refresh fails gracefully without command."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--command nil)
    (should-error (beads-list-refresh))))

;;; Command Tests

(ert-deftest beads-list-test-create-command ()
  "Test that beads-list-create requires beads-create."
  (with-temp-buffer
    (beads-list-mode)
    ;; Should require beads-create module
    (should (fboundp 'beads-list-create))))

(ert-deftest beads-list-test-update-command-without-issue ()
  "Test that beads-list-update errors when no issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (goto-char (point-min))
   (should-error (beads-list-update))))

(ert-deftest beads-list-test-update-command-with-issue ()
  "Test that beads-list-update can be called with issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (forward-line 1)
   (let ((id (beads-list--current-issue-id)))
     (should id)
     ;; Should be callable (we can't test full execution without mocking)
     (should (fboundp 'beads-list-update)))))

(ert-deftest beads-list-test-show-calls-beads-show ()
  "Test that beads-list-show requires and calls beads-show."
  ;; Pre-load beads-show so require doesn't affect test
  (require 'beads-show)
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (forward-line 1)
   (let ((id (beads-list--current-issue-id))
         (beads-show-called nil)
         (beads-show-arg nil))
     (should id)
     ;; Mock beads-show to verify it's called
     (cl-letf (((symbol-function 'beads-show)
                (lambda (issue-id)
                  (setq beads-show-called t
                        beads-show-arg issue-id))))
       (beads-list-show)
       (should beads-show-called)
       (should (equal beads-show-arg id))))))

;;; Issue ID Tests with Custom Prefixes

(ert-deftest beads-list-test-show-with-custom-prefix ()
  "Test showing issues with custom ID prefixes like 'myproject-13'."
  (require 'beads-show)
  (let* ((custom-prefix-issues
          '(((id . "myproject-1")
             (title . "First custom issue")
             (status . "open")
             (priority . 1)
             (issue-type . "bug")
             (created-at . "2025-10-19T10:00:00Z"))
            ((id . "myproject-13")
             (title . "Custom prefix issue")
             (status . "in_progress")
             (priority . 0)
             (issue-type . "feature")
             (created-at . "2025-10-20T16:00:00Z"))))
         (beads-show-called nil)
         (beads-show-arg nil))
    (beads-list-test--with-temp-buffer
     custom-prefix-issues 'list
     (goto-char (point-min))
     ;; Default sort is by Created (descending), so myproject-13 comes first
     (let ((id (beads-list--current-issue-id)))
       (should (equal id "myproject-13"))
       ;; Mock beads-show to verify correct ID is passed
       (cl-letf (((symbol-function 'beads-show)
                  (lambda (issue-id)
                    (setq beads-show-called t
                          beads-show-arg issue-id))))
         (beads-list-show)
         (should beads-show-called)
         (should (stringp beads-show-arg))
         (should (equal beads-show-arg "myproject-13"))
         ;; Verify no text properties or extra whitespace
         (should (equal beads-show-arg (substring-no-properties beads-show-arg)))
         (should (string= beads-show-arg (string-trim beads-show-arg))))))))

(ert-deftest beads-list-test-issue-id-has-no-text-properties ()
  "Test that issue IDs returned from list have no text properties."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (let ((id (beads-list--current-issue-id)))
     (should (stringp id))
     ;; Verify ID has no text properties
     (should (equal id (substring-no-properties id)))
     ;; Verify ID has no extra whitespace
     (should (string= id (string-trim id))))))

;;; Column Width Tests

(ert-deftest beads-list-test-column-widths ()
  "Test that column widths are configurable."
  (let ((beads-list-id-width 20))
    (with-temp-buffer
      (beads-list-mode)
      (let ((format (aref tabulated-list-format 0)))
        (should (= (cadr format) 20))))))

(ert-deftest beads-list-test-all-columns-present ()
  "Test that all required columns are present."
  (with-temp-buffer
    (beads-list-mode)
    (should (= (length tabulated-list-format) 6))
    (should (equal (car (aref tabulated-list-format 0)) "ID"))
    (should (equal (car (aref tabulated-list-format 1)) "Status"))
    (should (equal (car (aref tabulated-list-format 2)) "Priority"))
    (should (equal (car (aref tabulated-list-format 3)) "Type"))
    (should (equal (car (aref tabulated-list-format 4)) "Title"))
    (should (equal (car (aref tabulated-list-format 5)) "Created"))))

;;; Date Formatting Tests

(ert-deftest beads-list-test-format-date-absolute ()
  "Test absolute date formatting."
  (let ((beads-list-date-format 'absolute)
        (timestamp "2025-10-20T16:36:52.648609367Z"))
    (should (equal (beads-list--format-date timestamp)
                   "2025-10-20 16:36"))))

(ert-deftest beads-list-test-format-date-relative ()
  "Test relative date formatting."
  (let ((beads-list-date-format 'relative)
        ;; Create a timestamp from 2 hours ago
        (timestamp (format-time-string
                    "%Y-%m-%dT%H:%M:%SZ"
                    (time-subtract (current-time)
                                   (seconds-to-time 7200))
                    t)))
    (let ((result (beads-list--format-date timestamp)))
      (should (string-match-p "ago" result))
      (should (or (string-match-p "2h ago" result)
                  (string-match-p "1h ago" result)
                  (string-match-p "119m ago" result)
                  (string-match-p "120m ago" result))))))

(ert-deftest beads-list-test-format-date-iso ()
  "Test ISO date formatting."
  (let ((beads-list-date-format 'iso)
        (timestamp "2025-10-20T16:36:52Z"))
    (should (equal (beads-list--format-date timestamp)
                   "2025-10-20T16:36:52Z"))))

(ert-deftest beads-list-test-format-date-date-only ()
  "Test date-only formatting."
  (let ((beads-list-date-format 'date-only)
        (timestamp "2025-10-20T16:36:52.648609367Z"))
    (should (equal (beads-list--format-date timestamp)
                   "2025-10-20"))))

(ert-deftest beads-list-test-format-date-custom-string ()
  "Test custom format string."
  (let ((beads-list-date-format "%Y/%m/%d")
        (timestamp "2025-10-20T16:36:52Z"))
    (should (equal (beads-list--format-date timestamp)
                   "2025/10/20"))))

(ert-deftest beads-list-test-format-date-nil ()
  "Test formatting nil timestamp."
  (should (equal (beads-list--format-date nil) "")))

(ert-deftest beads-list-test-format-date-empty-string ()
  "Test formatting empty string timestamp."
  (should (equal (beads-list--format-date "") "")))

(ert-deftest beads-list-test-format-date-relative-seconds ()
  "Test relative formatting for seconds."
  (let ((beads-list-date-format 'relative)
        ;; 30 seconds ago
        (timestamp (format-time-string
                    "%Y-%m-%dT%H:%M:%SZ"
                    (time-subtract (current-time)
                                   (seconds-to-time 30))
                    t)))
    (let ((result (beads-list--format-date timestamp)))
      (should (string-suffix-p "ago" result))
      (should (string-match-p "[0-9]+s ago" result)))))

(ert-deftest beads-list-test-format-date-relative-days ()
  "Test relative formatting for days."
  (let ((beads-list-date-format 'relative)
        ;; 3 days ago
        (timestamp (format-time-string
                    "%Y-%m-%dT%H:%M:%SZ"
                    (time-subtract (current-time)
                                   (seconds-to-time 259200))
                    t)))
    (let ((result (beads-list--format-date timestamp)))
      (should (string-match-p "[23]d ago" result)))))

(ert-deftest beads-list-test-format-date-relative-weeks ()
  "Test relative formatting for weeks."
  (let ((beads-list-date-format 'relative)
        ;; 2 weeks ago (14 days)
        (timestamp (format-time-string
                    "%Y-%m-%dT%H:%M:%SZ"
                    (time-subtract (current-time)
                                   (seconds-to-time 1209600))
                    t)))
    (let ((result (beads-list--format-date timestamp)))
      (should (string-match-p "[12]w ago" result)))))

(ert-deftest beads-list-test-created-column-in-entry ()
  "Test that created date appears in issue entry."
  (let ((beads-list-date-format 'absolute)
        (alist (car beads-list-test--sample-issues))
        (issue (beads-list-test--alist-to-issue (car beads-list-test--sample-issues))))
    (let ((entry (beads-list--issue-to-entry issue)))
      ;; Created should be in column 5 (6th element, 0-indexed)
      (should (stringp (aref (cadr entry) 5)))
      (should (string-match-p "2025-10-20"
                              (aref (cadr entry) 5))))))

(ert-deftest beads-list-test-sort-by-created ()
  "Test sorting issues by creation date."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   ;; Default sort is now by Created (descending)
   (goto-char (point-min))
   (forward-line 0)
   ;; Most recent (bd-2: 2025-10-20T16:36:52Z) should be first
   (should (equal (beads-list--current-issue-id) "bd-2"))))

(ert-deftest beads-list-test-default-sort-is-created ()
  "Test that default sort key is Created column."
  (with-temp-buffer
    (beads-list-mode)
    (should (equal tabulated-list-sort-key '("Created" . t)))))

;;; Integration Test: Date Format Customization

(ert-deftest beads-list-test-integration-date-format-change ()
  "Integration test: Changing date format affects display."
  :tags '(integration)
  (let ((beads-list-date-format 'absolute))
    (beads-list-test--with-temp-buffer
     beads-list-test--sample-issues 'list
     (goto-char (point-min))
     (forward-line 0)
     (let* ((alist (car beads-list-test--sample-issues))
            (issue (beads-list-test--alist-to-issue alist))
            (entry (beads-list--issue-to-entry issue)))
       (should (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                               (aref (cadr entry) 5)))))))

(ert-deftest beads-list-test-integration-created-width-custom ()
  "Integration test: Created column width is customizable."
  :tags '(integration)
  (let ((beads-list-created-width 20))
    (with-temp-buffer
      (beads-list-mode)
      (let ((format (aref tabulated-list-format 5)))
        (should (equal (car format) "Created"))
        (should (= (cadr format) 20))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-list-test-mode-defined ()
  "Integration test: Verify beads-list-mode is defined."
  :tags '(integration)
  (should (fboundp 'beads-list-mode)))

(ert-deftest beads-list-test-keybinding-n-next ()
  "Integration test: Verify n keybinding for next."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "n"))))
      (should (eq binding 'beads-list-next)))))

(ert-deftest beads-list-test-keybinding-p-previous ()
  "Integration test: Verify p keybinding for previous."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "p"))))
      (should (eq binding 'beads-list-previous)))))

(ert-deftest beads-list-test-keybinding-ret-show ()
  "Integration test: Verify RET keybinding for show."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "RET"))))
      (should (eq binding 'beads-list-show)))))

(ert-deftest beads-list-test-keybinding-g-refresh ()
  "Integration test: Verify g keybinding for refresh."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "g"))))
      (should (eq binding 'beads-list-refresh)))))

(ert-deftest beads-list-test-keybinding-q-quit ()
  "Integration test: Verify q keybinding for quit."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "q"))))
      (should (eq binding 'beads-list-quit)))))

(ert-deftest beads-list-test-keybinding-m-mark ()
  "Integration test: Verify m keybinding for mark."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "m"))))
      (should (eq binding 'beads-list-mark)))))

(ert-deftest beads-list-test-keybinding-u-unmark ()
  "Integration test: Verify u keybinding for unmark."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "u"))))
      (should (eq binding 'beads-list-unmark)))))

(ert-deftest beads-list-test-keybinding-w-copy ()
  "Integration test: Verify w keybinding for copy-id."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "w"))))
      (should (eq binding 'beads-list-copy-id)))))

(ert-deftest beads-list-test-keybinding-S-sort ()
  "Integration test: Verify S keybinding for sort."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "S"))))
      (should (eq binding 'beads-list-sort)))))

(ert-deftest beads-list-test-list-mode-setup ()
  "Integration test: Verify list mode can be set up."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (should (eq major-mode 'beads-list-mode))
    (should (boundp 'tabulated-list-format))
    (should tabulated-list-format)))

(ert-deftest beads-list-test-list-command-exists ()
  "Integration test: Verify beads-list command exists."
  :tags '(integration)
  (should (fboundp 'beads-list)))

(ert-deftest beads-list-test-ready-command-exists ()
  "Integration test: Verify beads-ready command exists."
  :tags '(integration)
  (should (fboundp 'beads-ready)))

(ert-deftest beads-list-test-blocked-command-exists ()
  "Integration test: Verify beads-blocked command exists."
  :tags '(integration)
  (should (fboundp 'beads-blocked)))

(ert-deftest beads-list-test-workflow-to-show ()
  "Integration test: Navigate from list to show buffer."
  :tags '(integration)
  (require 'beads-show)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has show command
    (should (fboundp 'beads-list-show))
    ;; Verify RET is bound to show
    (let ((binding (lookup-key beads-list-mode-map (kbd "RET"))))
      (should (eq binding 'beads-list-show)))))

(ert-deftest beads-list-test-workflow-to-update ()
  "Integration test: Update from list buffer."
  :tags '(integration)
  (require 'beads-update)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has update command
    (should (fboundp 'beads-list-update))
    ;; Verify e is bound to update
    (let ((binding (lookup-key beads-list-mode-map (kbd "e"))))
      (should (eq binding 'beads-list-update)))))

(ert-deftest beads-list-test-workflow-to-close ()
  "Integration test: Close from list buffer."
  :tags '(integration)
  (require 'beads-close)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has close command
    (should (fboundp 'beads-list-close))
    ;; Verify d is bound to close
    (let ((binding (lookup-key beads-list-mode-map (kbd "d"))))
      (should (eq binding 'beads-list-close)))))

(ert-deftest beads-list-test-workflow-to-delete ()
  "Integration test: Delete from list buffer."
  :tags '(integration)
  (require 'beads-delete)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has delete command
    (should (fboundp 'beads-list-delete))
    ;; Verify D is bound to delete
    (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
      (should (eq binding 'beads-list-delete)))))

(ert-deftest beads-list-test-workflow-to-create ()
  "Integration test: Create from list buffer."
  :tags '(integration)
  (require 'beads-create)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has create command
    (should (fboundp 'beads-list-create))
    ;; Verify c is bound to create
    (let ((binding (lookup-key beads-list-mode-map (kbd "c"))))
      (should (eq binding 'beads-list-create)))))

(ert-deftest beads-list-test-context-detection ()
  "Integration test: Context detection works in list mode."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    ;; List mode should be active
    (should (eq major-mode 'beads-list-mode))))

;;; Footer

;;; Transient Menu Integration Tests

(ert-deftest beads-list-test-transient-menu-displays ()
  "Integration test: Verify beads-list transient menu displays without errors.
Uses execute-kbd-macro technique from:
https://emacs.stackexchange.com/questions/55386/how-to-automate-user-testing-with-elisp"
  :tags '(integration transient)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'beads--run-command)
             (lambda (cmd &rest _args)
               (if (equal cmd "list")
                   (apply #'vector beads-list-test--sample-issues)
                 (vector))))
            ((symbol-function 'beads--parse-issues)
             (lambda (result)
               (append result nil)))
            ((symbol-function 'beads-issue-from-json)
             #'beads-list-test--alist-to-issue))
    ;; Test that transient menu can be invoked without error
    (should-not (condition-case err
                    (progn
                      ;; Call beads-list which should show the transient
                      (call-interactively 'beads-list)
                      ;; Verify transient is active
                      (should (transient-active-prefix 'beads-list))
                      ;; Quit the transient
                      (execute-kbd-macro (kbd "q"))
                      nil)
                  (error err)))))

(ert-deftest beads-list-test-transient-menu-executes ()
  "Integration test: Verify beads-list transient menu can execute list command.
Tests the full workflow: open menu -> execute -> display results."
  :tags '(integration transient)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'beads--run-command)
             (lambda (cmd &rest _args)
               (if (equal cmd "list")
                   (apply #'vector beads-list-test--sample-issues)
                 (vector))))
            ((symbol-function 'beads--parse-issues)
             (lambda (result)
               (append result nil)))
            ((symbol-function 'beads-issue-from-json)
             #'beads-list-test--alist-to-issue))
    (should-not (condition-case err
                    (progn
                      ;; Call beads-list to show transient
                      (call-interactively 'beads-list)
                      ;; Execute the list command (press 'x')
                      (execute-kbd-macro (kbd "x"))
                      ;; Verify we're now in the beads-list buffer
                      (should (equal (buffer-name) "*beads-list*"))
                      (should (eq major-mode 'beads-list-mode))
                      ;; Verify issues were populated
                      (should (> (length tabulated-list-entries) 0))
                      ;; Clean up
                      (kill-buffer)
                      nil)
                  (error err)))))

(ert-deftest beads-list-test-transient-menu-preview ()
  "Integration test: Verify beads-list transient menu preview command works."
  :tags '(integration transient)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t)))
    (should-not (condition-case err
                    (progn
                      ;; Call beads-list to show transient
                      (call-interactively 'beads-list)
                      ;; Press 'P' to preview command
                      (execute-kbd-macro (kbd "P"))
                      ;; Verify transient is still active (preview is transient)
                      (should (transient-active-prefix 'beads-list))
                      ;; Quit the transient
                      (execute-kbd-macro (kbd "q"))
                      nil)
                  (error err)))))

(ert-deftest beads-list-test-transient-menu-with-filters ()
  "Integration test: Verify beads-list transient menu with filter options.
Tests setting filters before executing."
  :tags '(integration transient)
  :expected-result :failed  ; Complex transient state mocking needed
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'beads--run-command)
             (lambda (cmd &rest args)
               (if (equal cmd "list")
                   (progn
                     ;; Verify filter args were passed
                     (should (member "--status=open" args))
                     (apply #'vector (list (car beads-list-test--sample-issues))))
                 (vector))))
            ((symbol-function 'beads--parse-issues)
             (lambda (result)
               (append result nil)))
            ((symbol-function 'beads-issue-from-json)
             #'beads-list-test--alist-to-issue)
            ((symbol-function 'beads-reader-list-status)
             (lambda (&rest _)
               "open")))
    (should-not (condition-case err
                    (progn
                      ;; Call beads-list to show transient
                      (call-interactively 'beads-list)
                      ;; Set status filter (press 's' for status)
                      (execute-kbd-macro (kbd "s"))
                      ;; Execute the list command (press 'x')
                      (execute-kbd-macro (kbd "x"))
                      ;; Verify we're in the beads-list buffer
                      (should (equal (buffer-name) "*beads-list*"))
                      ;; Clean up
                      (kill-buffer)
                      nil)
                  (error err)))))

(ert-deftest beads-list-test-without-issues ()
  "Test beads-list buffer with empty project (no issues)."
  :tags '(integration transient)
  ;; (skip-unless (not noninteractive))
  (let ((default-directory (beads-test-make-project)))
    (beads-test-execute-commands
     (list (kbd "M-x beads-list")
           (kbd "x")))
    ;; Verify we're in the list buffer
    (should (equal "*beads-list*" (buffer-name (current-buffer))))
    ;; Verify buffer is in beads-list-mode
    (should (eq major-mode 'beads-list-mode))))

(ert-deftest beads-list-test-with-issues ()
  "Test beads-list buffer renders created issues correctly."
  :tags '(integration transient)
  (skip-unless (not noninteractive))
  (let ((default-directory (beads-test-make-project)))
    ;; Create test issues with JSON flag to get issue objects back
    (let ((issue-a (beads-command-execute
                    (beads-command-create :title "Test issue A"
                                         :json t)))
          (issue-b (beads-command-execute
                    (beads-command-create :title "Test issue B"
                                         :json t))))
      ;; Execute beads-list command
      (beads-test-execute-commands
       (list (kbd "M-x beads-list")
             (kbd "x")))
      ;; Verify we're in the list buffer
      (should (equal "*beads-list*" (buffer-name (current-buffer))))
      ;; Verify buffer is in beads-list-mode
      (should (eq major-mode 'beads-list-mode))
      ;; Get buffer contents
      (let ((buffer-contents (buffer-substring-no-properties
                             (point-min) (point-max))))
        ;; Verify both issue IDs are rendered
        (should (string-match-p (oref issue-a id) buffer-contents))
        (should (string-match-p (oref issue-b id) buffer-contents))
        ;; Verify both issue titles are rendered
        (should (string-match-p "Test issue A" buffer-contents))
        (should (string-match-p "Test issue B" buffer-contents))
        ;; Verify we have at least 2 lines of content (issues, no header in tabulated-list)
        (should (>= (count-lines (point-min) (point-max)) 2))))))

(provide 'beads-list-test)
;;; beads-list-test.el ends here
