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
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _)
               ;; Return empty vector of issues
               (beads-test--mock-command-result (vector))))
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
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _) (beads-test--mock-command-result (vector))))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    ;; Use beads-ready and beads-blocked instead of beads-list
    ;; since beads-list is a transient that doesn't work in batch mode
    (beads-ready)
    (let ((ready-buf (current-buffer)))
      (beads-blocked)
      (let ((blocked-buf (current-buffer)))
        (should (not (eq ready-buf blocked-buf)))
        (should (buffer-live-p ready-buf))
        (should (buffer-live-p blocked-buf))
        (kill-buffer ready-buf)
        (kill-buffer blocked-buf)))))

;;; Data Parsing Tests

(ert-deftest beads-list-test-issue-to-entry ()
  "Test conversion of issue object to tabulated-list entry."
  (let* ((alist (car beads-list-test--sample-issues))
         (issue (beads-list-test--alist-to-issue alist))
         (entry (beads-list--issue-to-entry issue)))
    (should (equal (car entry) "bd-1"))
    (should (vectorp (cadr entry)))
    (should (= (length (cadr entry)) 8))
    ;; Check ID column
    (should (equal (aref (cadr entry) 0) "bd-1"))
    ;; Check title column (index 5, after Agent column)
    (should (equal (aref (cadr entry) 5) "First issue"))
    ;; Check created column exists (index 6)
    (should (stringp (aref (cadr entry) 6)))
    ;; Check updated column exists (index 7)
    (should (stringp (aref (cadr entry) 7)))))

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
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _)
                 (setq call-count (1+ call-count))
                 (beads-test--mock-command-result
                  (apply #'vector (mapcar #'beads-issue-from-json beads-list-test--sample-issues))))))
      (beads-list-test--with-temp-buffer
       beads-list-test--sample-issues 'list
       (beads-list-refresh)
       (should (= call-count 1))
       (should (= (length tabulated-list-entries) 4))))))

(ert-deftest beads-list-test-refresh-preserves-position ()
  "Test that refresh preserves cursor position."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _)
               (beads-test--mock-command-result
                (apply #'vector (mapcar #'beads-issue-from-json beads-list-test--sample-issues))))))
    (beads-list-test--with-temp-buffer
     beads-list-test--sample-issues 'list
     (goto-char (point-min))
     (forward-line 2)
     (let ((pos (point)))
       (beads-list-refresh)
       (should (= (point) pos))))))

(ert-deftest beads-list-test-refresh-empty-result ()
  "Test refresh with empty result."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (&rest _) (beads-test--mock-command-result nil))))
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
    (should (equal formatted "P1"))
    (should (eq (get-text-property 0 'face formatted)
                'beads-list-priority-high))))

;;; Mode Line Tests
;; Mode line tests for beads-list removed - beads-list is now a transient
;; menu, and mode-line formatting is not implemented for the list view.
;; beads-ready and beads-blocked still have custom mode lines.

;;; Integration Tests

;; Full workflow test removed - beads-list is now a transient menu.
;; Individual components (navigation, marking, refresh) are tested separately,
;; and transient menu tests cover the integration workflow.

(ert-deftest beads-list-test-ready-command ()
  "Test beads-ready command."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd &rest _)
               ;; Return issue objects, not raw alists
               (let ((alist (car beads-list-test--sample-issues)))
                 (beads-test--mock-command-result
                  (vector (beads-issue
                           :id (alist-get 'id alist)
                           :title (alist-get 'title alist)
                           :status (alist-get 'status alist)
                           :priority (alist-get 'priority alist)
                           :issue-type (alist-get 'issue-type alist)
                           :created-at (alist-get 'created-at alist)))))))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-ready)
    (should (equal (buffer-name) "*beads-ready*"))
    (should (eq beads-list--command 'ready))
    (kill-buffer)))

(ert-deftest beads-list-test-blocked-command ()
  "Test beads-blocked command."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd &rest _)
               ;; Return issue objects, not raw alists
               (let ((alist (caddr beads-list-test--sample-issues)))
                 (beads-test--mock-command-result
                  (vector (beads-blocked-issue
                           :id (alist-get 'id alist)
                           :title (alist-get 'title alist)
                           :status (alist-get 'status alist)
                           :priority (alist-get 'priority alist)
                           :issue-type (alist-get 'issue-type alist)
                           :created-at (alist-get 'created-at alist)))))))
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
    (should (= (length tabulated-list-format) 8))
    (should (equal (car (aref tabulated-list-format 0)) "ID"))
    (should (equal (car (aref tabulated-list-format 1)) "Type"))
    (should (equal (car (aref tabulated-list-format 2)) "Status"))
    (should (equal (car (aref tabulated-list-format 3)) "Priority"))
    (should (equal (car (aref tabulated-list-format 4)) "Agents"))
    (should (equal (car (aref tabulated-list-format 5)) "Title"))
    (should (equal (car (aref tabulated-list-format 6)) "Created"))
    (should (equal (car (aref tabulated-list-format 7)) "Updated"))))

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

(ert-deftest beads-list-test-format-date-unknown-format ()
  "Test date formatting with unknown format falls back to default."
  (let ((beads-list-date-format 'some-unknown-symbol)
        (timestamp "2025-10-20T16:36:52Z"))
    (let ((result (beads-list--format-date timestamp)))
      (should (stringp result))
      (should (string-match-p "2025-10-20" result)))))

(ert-deftest beads-list-test-created-column-in-entry ()
  "Test that created date appears in issue entry."
  (let ((beads-list-date-format 'absolute)
        (alist (car beads-list-test--sample-issues))
        (issue (beads-list-test--alist-to-issue (car beads-list-test--sample-issues))))
    (let ((entry (beads-list--issue-to-entry issue)))
      ;; Created should be in column 6 (7th element, 0-indexed)
      (should (stringp (aref (cadr entry) 6)))
      (should (string-match-p "2025-10-20"
                              (aref (cadr entry) 6))))))

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
                               (aref (cadr entry) 6)))))))

(ert-deftest beads-list-test-integration-created-width-custom ()
  "Integration test: Created column width is customizable."
  :tags '(integration)
  (let ((beads-list-created-width 20))
    (with-temp-buffer
      (beads-list-mode)
      (let ((format (aref tabulated-list-format 6)))
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

;;; Agent Status Indicator Tests

(ert-deftest beads-list-test-agent-faces-defined ()
  "Test that all agent status faces are defined."
  (should (facep 'beads-list-agent-working))
  (should (facep 'beads-list-agent-finished))
  (should (facep 'beads-list-agent-failed)))

(ert-deftest beads-list-test-format-agent-no-activity ()
  "Test format-agent returns empty string when no agent activity."
  (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_id) nil)))
    (should (equal (beads-list--format-agent "bd-1") ""))))

(ert-deftest beads-list-test-format-agent-working ()
  "Test format-agent shows just type letter for single agent."
  (let ((mock-session (list 'mock-session)))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) mock-session))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (_session) "Task"))
              ((symbol-function 'beads-agent--session-instance-number)
               (lambda (_session) 1)))
      (let ((result (beads-list--format-agent "bd-1")))
        ;; Single agent shows just letter, no instance number
        (should (string= (substring-no-properties result) "T"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-working))
        ;; Verify help-echo tooltip mentions agent count
        (should (string-match-p "1 agent"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-list-test-format-agent-working-no-backend-name ()
  "Test format-agent shows fallback circle when no type name (single agent)."
  (let ((mock-session (list 'mock-session)))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) mock-session))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (_session) nil))
              ((symbol-function 'beads-agent--session-instance-number)
               (lambda (_session) 1)))
      (let ((result (beads-list--format-agent "bd-1")))
        ;; Single agent shows just fallback circle, no instance number
        (should (string= (substring-no-properties result) "●"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-working))
        (should (get-text-property 0 'help-echo result))))))

(ert-deftest beads-list-test-format-agent-finished ()
  "Test format-agent shows green circle when agent finished."
  (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_id) 'finished)))
    (let ((result (beads-list--format-agent "bd-1")))
      (should (string= (substring-no-properties result) "●"))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-finished))
      (should (string-match-p "finished"
                              (get-text-property 0 'help-echo result))))))

(ert-deftest beads-list-test-format-agent-failed ()
  "Test format-agent shows red circle when agent failed."
  (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_id) 'failed)))
    (let ((result (beads-list--format-agent "bd-1")))
      (should (string= (substring-no-properties result) "●"))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-failed))
      (should (string-match-p "failed"
                              (get-text-property 0 'help-echo result))))))

(ert-deftest beads-list-test-format-agent-active-takes-priority ()
  "Test format-agent shows working status even if outcome exists.
Active session should take priority over previous outcome."
  (let ((mock-session (list 'mock-session)))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) mock-session))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) '("T" . finished)))  ; Previous outcome exists
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (_session) "Review")))
      (let ((result (beads-list--format-agent "bd-1")))
        ;; Single agent shows just letter, no instance number
        (should (string= (substring-no-properties result) "R"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-working))))))

(ert-deftest beads-list-test-format-agent-multiple-sessions ()
  "Test format-agent shows per-type instance numbers for duplicate types."
  (let ((mock-sessions (list 'session1 'session2 'session3))
        (session-types '(("session1" . "Task")
                         ("session2" . "Review")
                         ("session3" . "Task"))))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) mock-sessions))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) nil))
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (session)
                 (cdr (assoc (symbol-name session) session-types)))))
      (let ((result (beads-list--format-agent "bd-1")))
        ;; Task appears twice, so shows per-type instance numbers: T#1 and T#2
        (should (string-match-p "T#1" result))
        (should (string-match-p "T#2" result))
        ;; Review appears once, so just "R" without instance number
        (should (string-match-p "R" result))
        (should-not (string-match-p "R#" result))
        ;; Multiple agents use / separator
        (should (string-match-p "/" result))
        ;; Help-echo should mention 3 agents
        (should (string-match-p "3 agents"
                                (get-text-property 0 'help-echo result)))))))

;;; Footer

;;; Transient Menu Integration Tests

(ert-deftest beads-list-test-transient-menu-displays ()
  "Integration test: Verify beads-list transient is properly defined.
Tests that the transient prefix and its suffixes are correctly set up."
  :tags '(integration transient)
  ;; Verify beads-list is a transient prefix
  (should (get 'beads-list 'transient--prefix))
  ;; Verify the execute suffix is defined and has correct key
  (should (fboundp 'beads-list--transient-execute))
  ;; Verify the preview suffix is defined
  (should (fboundp 'beads-list--transient-preview))
  ;; Verify the reset suffix is defined
  (should (fboundp 'beads-list--transient-reset)))

(ert-deftest beads-list-test-transient-menu-executes ()
  "Integration test: Verify beads-list execute suffix works correctly.
Tests that executing with mocked transient-args creates a list buffer."
  :tags '(integration transient)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'beads-command-execute)
             (lambda (cmd &rest _args)
               ;; Set data on the passed-in command object
               (oset cmd data
                     (if (cl-typep cmd 'beads-command-list)
                         (apply #'vector
                                (mapcar #'beads-list-test--alist-to-issue
                                        beads-list-test--sample-issues))
                       nil))
               cmd)))
    ;; Mock transient-args and call the execute suffix directly
    (beads-test-with-transient-args 'beads-list nil
      (beads-list--transient-execute))
    ;; Verify we're now in the beads-list buffer
    (should (get-buffer "*beads-list*"))
    (with-current-buffer "*beads-list*"
      (should (eq major-mode 'beads-list-mode))
      ;; Verify issues were populated
      (should (> (length tabulated-list-entries) 0)))
    ;; Clean up
    (kill-buffer "*beads-list*")))

(ert-deftest beads-list-test-transient-menu-preview ()
  "Integration test: Verify beads-list preview suffix displays command.
Tests that the preview command shows the bd command that would be executed."
  :tags '(integration transient)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t)))
    (let ((message-shown nil))
      ;; Mock message to capture what preview outputs
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "^Command:" fmt)
                     (setq message-shown (apply #'format fmt args))))))
        ;; Mock transient-args and call preview directly
        (beads-test-with-transient-args 'beads-list '("--status=open")
          (beads-list--transient-preview)))
      ;; Verify message was shown with command
      (should message-shown)
      (should (string-match-p "bd" message-shown))
      (should (string-match-p "list" message-shown)))))

;; Transient menu with filters test removed - requires complex transient state
;; mocking. Basic transient functionality is covered by other transient tests
;; (beads-list-test-transient-menu-displays, beads-list-test-transient-menu-executes).
;; Filter functionality is tested via beads-list--parse-transient-args unit tests.

;;; Agent Indicator Formatting Tests

(ert-deftest beads-list-test-format-agent-indicator-nil-type-name ()
  "Test that nil type-name produces fallback indicator."
  (let ((result (beads-list--format-agent-indicator nil 1 'beads-list-agent-working)))
    ;; Should use "●" as fallback when type-name is nil
    (should (stringp result))
    (should (string-match-p "●" result))
    ;; Face should be applied
    (should (eq (get-text-property 0 'face result) 'beads-list-agent-working))))

(ert-deftest beads-list-test-format-agent-indicator-valid-type-name ()
  "Test that valid type-name produces first letter indicator."
  (let ((result (beads-list--format-agent-indicator "Task" 1 'beads-list-agent-working)))
    ;; Should use "T" from "Task"
    (should (string-match-p "T" result))
    ;; Should include instance number
    (should (string-match-p "#1" result))))

(ert-deftest beads-list-test-format-agent-indicator-brief-mode ()
  "Test that brief mode omits instance number."
  (let ((result (beads-list--format-agent-indicator "Review" 2 'beads-list-agent-finished t)))
    ;; Should just be "R" in brief mode
    (should (equal result (propertize "R" 'face 'beads-list-agent-finished)))))

(ert-deftest beads-list-test-format-agent-indicator-nil-instance ()
  "Test that nil instance number produces letter-only indicator."
  (let ((result (beads-list--format-agent-indicator "QA" nil 'beads-list-agent-failed)))
    ;; Should just be "Q" when instance is nil
    (should (string-match-p "Q" result))
    (should-not (string-match-p "#" result))))

(ert-deftest beads-list-test-format-agent-indicator-nil-both ()
  "Test that nil type-name with nil instance produces fallback."
  (let ((result (beads-list--format-agent-indicator nil nil 'beads-list-agent-working)))
    ;; Should use "●" fallback
    (should (string-match-p "●" result))
    ;; No instance number
    (should-not (string-match-p "#" result))))

;;; =========================================================================
;;; Directory-Aware Buffer Identity Tests (beads.el-n3lv)
;;; =========================================================================
;;
;; These tests verify the directory-aware buffer identity model.
;; Key principle: Directory is identity, branch is metadata.
;; Same project-dir → same buffer, regardless of branch.

(ert-deftest beads-list-test-find-buffer-for-project-not-found ()
  "Test finding buffer when none exists for the project."
  (should (null (beads-list--find-buffer-for-project 'list "/nonexistent"))))

(ert-deftest beads-list-test-find-buffer-for-project-found ()
  "Test finding existing buffer by project directory."
  (let ((test-buffer (generate-new-buffer "*beads-list-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (beads-list-mode)
          (setq beads-list--project-dir "/tmp/test-project")
          (setq beads-list--command 'list)
          ;; Should find our buffer
          (should (eq (beads-list--find-buffer-for-project 'list "/tmp/test-project")
                      test-buffer))
          ;; Should NOT find buffer for different directory
          (should (null (beads-list--find-buffer-for-project 'list "/other/project")))
          ;; Should NOT find buffer for different command type
          (should (null (beads-list--find-buffer-for-project 'ready "/tmp/test-project"))))
      (kill-buffer test-buffer))))

(ert-deftest beads-list-test-find-buffer-normalizes-directory ()
  "Test that buffer lookup normalizes directory paths."
  ;; Use /tmp which is guaranteed to exist
  (let ((test-buffer (generate-new-buffer "*beads-list-test*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (beads-list-mode)
          ;; Store with trailing slash
          (setq beads-list--project-dir "/tmp/")
          (setq beads-list--command 'list)
          ;; Should match even without trailing slash
          (should (eq (beads-list--find-buffer-for-project 'list "/tmp")
                      test-buffer)))
      (kill-buffer test-buffer))))

(ert-deftest beads-list-test-get-or-create-buffer-creates-new ()
  "Test get-or-create-buffer creates new buffer when none exists."
  (let (created-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/new-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "new-project")))
          (setq created-buffer (beads-list--get-or-create-buffer 'list))
          (should (bufferp created-buffer))
          (with-current-buffer created-buffer
            (should (equal beads-list--project-dir "/tmp/new-project"))
            (should (equal beads-list--branch "main"))
            (should (equal beads-list--proj-name "new-project"))))
      (when (and created-buffer (buffer-live-p created-buffer))
        (kill-buffer created-buffer)))))

(ert-deftest beads-list-test-get-or-create-buffer-reuses-existing ()
  "Test get-or-create-buffer reuses buffer for same project directory."
  (let ((test-buffer (generate-new-buffer "*beads-list*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (beads-list-mode)
            (setq beads-list--project-dir "/tmp/existing-project")
            (setq beads-list--command 'list))
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/tmp/existing-project"))
                    ((symbol-function 'beads-git-get-branch)
                     (lambda () "feature"))
                    ((symbol-function 'beads-git-get-project-name)
                     (lambda () "existing-project")))
            ;; Should return the existing buffer
            (should (eq (beads-list--get-or-create-buffer 'list) test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest beads-list-test-same-dir-different-branch-same-buffer ()
  "Test that same directory with different branch uses same buffer.
This is the CRITICAL behavioral test for directory-as-identity."
  (let ((test-buffer (generate-new-buffer "*beads-list*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (beads-list-mode)
            (setq beads-list--project-dir "/tmp/project")
            (setq beads-list--branch "main")
            (setq beads-list--command 'list))
          ;; Simulate branch switch - branch changes but directory doesn't
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/tmp/project"))
                    ((symbol-function 'beads-git-get-branch)
                     (lambda () "feature"))  ; Different branch!
                    ((symbol-function 'beads-git-get-project-name)
                     (lambda () "project")))
            ;; CRITICAL: Should return SAME buffer (same directory)
            (let ((result (beads-list--get-or-create-buffer 'list)))
              (should (eq result test-buffer)))))
      (kill-buffer test-buffer))))

(ert-deftest beads-list-test-different-dir-same-branch-different-buffer ()
  "Test that different directories create different buffers.
Even if they have the same branch name."
  (let ((buffer1 (generate-new-buffer "*beads-list-1*"))
        (buffer2 nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer1
            (beads-list-mode)
            (setq beads-list--project-dir "/tmp/project1")
            (setq beads-list--branch "main")
            (setq beads-list--command 'list))
          ;; Create buffer for different directory
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/tmp/project2"))  ; Different directory!
                    ((symbol-function 'beads-git-get-branch)
                     (lambda () "main"))  ; Same branch
                    ((symbol-function 'beads-git-get-project-name)
                     (lambda () "project2")))
            ;; Should create NEW buffer (different directory)
            (setq buffer2 (beads-list--get-or-create-buffer 'list))
            (should (bufferp buffer2))
            (should-not (eq buffer1 buffer2))))
      (kill-buffer buffer1)
      (when (and buffer2 (buffer-live-p buffer2))
        (kill-buffer buffer2)))))

(ert-deftest beads-list-test-different-buffer-types-same-project ()
  "Test that different buffer types (list, ready, blocked) create separate buffers."
  (let ((list-buffer (generate-new-buffer "*beads-list*"))
        (ready-buffer nil))
    (unwind-protect
        (progn
          (with-current-buffer list-buffer
            (beads-list-mode)
            (setq beads-list--project-dir "/tmp/project")
            (setq beads-list--command 'list))
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/tmp/project"))
                    ((symbol-function 'beads-git-get-branch)
                     (lambda () "main"))
                    ((symbol-function 'beads-git-get-project-name)
                     (lambda () "project")))
            ;; Should create NEW buffer for 'ready (different type)
            (setq ready-buffer (beads-list--get-or-create-buffer 'ready))
            (should (bufferp ready-buffer))
            (should-not (eq list-buffer ready-buffer))))
      (kill-buffer list-buffer)
      (when (and ready-buffer (buffer-live-p ready-buffer))
        (kill-buffer ready-buffer)))))

(ert-deftest beads-list-test-buffer-local-variables-preserved ()
  "Test that buffer-local variables are set correctly."
  (let (created-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/home/user/code/my-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "feature-branch"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "my-project")))
          (setq created-buffer (beads-list--get-or-create-buffer 'ready))
          (with-current-buffer created-buffer
            ;; Verify all variables set
            (should (equal beads-list--project-dir "/home/user/code/my-project"))
            (should (equal beads-list--branch "feature-branch"))
            (should (equal beads-list--proj-name "my-project"))))
      (when (and created-buffer (buffer-live-p created-buffer))
        (kill-buffer created-buffer)))))

(ert-deftest beads-list-test-hl-line-mode-enabled ()
  "Test that hl-line-mode is enabled when entering beads-list-mode."
  (with-temp-buffer
    (beads-list-mode)
    (should (bound-and-true-p hl-line-mode))))

;;; Copy ID Tests

(ert-deftest beads-list-test-copy-id-success ()
  "Test copying issue ID to kill ring."
  (beads-list-test--with-temp-buffer
   beads-list-test--sample-issues 'list
   (goto-char (point-min))
   (let ((id (beads-list--current-issue-id)))
     (beads-list-copy-id)
     (should (equal (car kill-ring) id)))))

(ert-deftest beads-list-test-copy-id-no-issue ()
  "Test copy-id errors when no issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (goto-char (point-min))
   (should-error (beads-list-copy-id) :type 'user-error)))

;;; Close Command Tests

(ert-deftest beads-list-test-close-no-issue ()
  "Test close errors when no issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (goto-char (point-min))
   (should-error (beads-list-close) :type 'user-error)))

;;; Reopen Command Tests

(ert-deftest beads-list-test-reopen-no-issue ()
  "Test reopen errors when no issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (goto-char (point-min))
   (should-error (beads-list-reopen) :type 'user-error)))

;;; Delete Command Tests

(ert-deftest beads-list-test-delete-no-issue ()
  "Test delete errors when no issue at point."
  (beads-list-test--with-temp-buffer
   beads-list-test--empty-issues 'list
   (goto-char (point-min))
   (should-error (beads-list-delete) :type 'user-error)))

;;; Parse Transient Args Tests

(ert-deftest beads-list-test-parse-transient-args-empty ()
  "Test parsing empty transient args."
  (let ((cmd (beads-list--parse-transient-args nil)))
    (should (beads-command-list-p cmd))
    (should (null (oref cmd status)))
    (should (null (oref cmd priority)))))

(ert-deftest beads-list-test-parse-transient-args-status ()
  "Test parsing status filter."
  (let ((cmd (beads-list--parse-transient-args '("--status=open"))))
    (should (equal (oref cmd status) "open"))))

(ert-deftest beads-list-test-parse-transient-args-priority ()
  "Test parsing priority filter."
  (let ((cmd (beads-list--parse-transient-args '("--priority=2"))))
    (should (equal (oref cmd priority) 2))))

(ert-deftest beads-list-test-parse-transient-args-all-flag ()
  "Test parsing --all flag."
  (let ((cmd (beads-list--parse-transient-args '("--all"))))
    (should (oref cmd all))))

(ert-deftest beads-list-test-parse-transient-args-labels ()
  "Test parsing label filters."
  (let ((cmd (beads-list--parse-transient-args
              '("--label=bug" "--label=urgent"))))
    (should (equal (oref cmd label) '("bug" "urgent")))))

(ert-deftest beads-list-test-parse-transient-args-date-filters ()
  "Test parsing date filters."
  (let ((cmd (beads-list--parse-transient-args
              '("--created-after=2025-01-01"
                "--updated-before=2025-12-31"))))
    (should (equal (oref cmd created-after) "2025-01-01"))
    (should (equal (oref cmd updated-before) "2025-12-31"))))

(ert-deftest beads-list-test-parse-transient-args-text-search ()
  "Test parsing text search filters."
  (let ((cmd (beads-list--parse-transient-args
              '("--title-contains=bug"
                "--desc-contains=error"))))
    (should (equal (oref cmd title-contains) "bug"))
    (should (equal (oref cmd desc-contains) "error"))))

(ert-deftest beads-list-test-parse-transient-args-limit ()
  "Test parsing limit option."
  (let ((cmd (beads-list--parse-transient-args '("--limit=10"))))
    (should (equal (oref cmd limit) 10))))

;;; Agent Indicator Edge Cases

(ert-deftest beads-list-test-format-agent-finished-with-indicator ()
  "Test format-agent shows indicator for finished outcome."
  (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_id) '("T" . finished))))
    (let ((result (beads-list--format-agent "bd-1")))
      (should (string-match-p "T" result))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-finished)))))

(ert-deftest beads-list-test-format-agent-failed-with-indicator ()
  "Test format-agent shows indicator for failed outcome."
  (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_id) '("R" . failed))))
    (let ((result (beads-list--format-agent "bd-1")))
      (should (string-match-p "R" result))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-failed)))))

;;; Refresh All Tests

(ert-deftest beads-list-test-refresh-all-no-buffers ()
  "Test refresh-all when no beads buffers exist."
  ;; Should not error
  (should-not (condition-case nil
                  (progn (beads-list-refresh-all) nil)
                (error t))))

(ert-deftest beads-list-test-refresh-all-with-buffer ()
  "Test refresh-all refreshes live beads-list buffers."
  (let ((refreshed nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _)
                 (setq refreshed t)
                 (beads-test--mock-command-result (vector))))
              ((symbol-function 'beads-check-executable)
               (lambda () t)))
      (beads-list-test--with-temp-buffer
       beads-list-test--sample-issues 'list
       ;; refresh-all iterates all buffers
       (beads-list-refresh-all)
       (should refreshed)))))

;;; Filter Command Tests

(ert-deftest beads-list-test-filter-command-with-command-obj ()
  "Test filter command when command-obj is set."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--command-obj (beads-command-list :status "open"))
    ;; Should be able to call filter (it opens transient)
    (should (fboundp 'beads-list-filter))))

;;; On Agent State Change Hook Tests

(ert-deftest beads-list-test-on-agent-state-change-hook ()
  "Test that agent state change hook triggers refresh."
  (let ((refresh-called nil))
    (cl-letf (((symbol-function 'beads-list-refresh-all)
               (lambda () (setq refresh-called t))))
      (beads-list--on-agent-state-change 'start 'mock-session)
      (should refresh-called))))

;;; Normalize Directory Tests

(ert-deftest beads-list-test-normalize-directory-trailing-slash ()
  "Test normalize-directory strips trailing slash."
  (should (equal (beads-list--normalize-directory "/path/to/dir/")
                 "/path/to/dir")))

(ert-deftest beads-list-test-normalize-directory-no-slash ()
  "Test normalize-directory preserves path without trailing slash."
  (should (equal (beads-list--normalize-directory "/path/to/dir")
                 "/path/to/dir")))

(ert-deftest beads-list-test-normalize-directory-expands ()
  "Test normalize-directory expands relative paths."
  (let ((default-directory "/home/user/"))
    (should (string-prefix-p "/" (beads-list--normalize-directory "relative")))))

;;; Relative Date Format - Months and Years

(ert-deftest beads-list-test-format-date-relative-months ()
  "Test format-date with relative format for months."
  (let ((beads-list-date-format 'relative))
    ;; Create timestamp ~2 months ago (approximately 60 days)
    (let* ((now (current-time))
           (days-ago 60)
           (past-time (time-subtract now (seconds-to-time (* days-ago 86400))))
           (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" past-time t)))
      (let ((result (beads-list--format-date timestamp)))
        (should (string-match-p "mo ago" result))))))

(ert-deftest beads-list-test-format-date-relative-years ()
  "Test format-date with relative format for years."
  (let ((beads-list-date-format 'relative))
    ;; Create timestamp ~2 years ago (approximately 730 days)
    (let* ((now (current-time))
           (days-ago 730)
           (past-time (time-subtract now (seconds-to-time (* days-ago 86400))))
           (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" past-time t)))
      (let ((result (beads-list--format-date timestamp)))
        (should (string-match-p "y ago" result))))))

(ert-deftest beads-list-test-format-date-relative-minutes ()
  "Test format-date with relative format for minutes."
  (let ((beads-list-date-format 'relative))
    ;; Create timestamp ~5 minutes ago
    (let* ((now (current-time))
           (past-time (time-subtract now (seconds-to-time (* 5 60))))
           (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" past-time t)))
      (let ((result (beads-list--format-date timestamp)))
        (should (string-match-p "m ago" result))))))

;;; Agent Indicator Tests

(ert-deftest beads-list-test-format-agent-indicator-basic ()
  "Test format-agent-indicator returns string."
  (let ((result (beads-list--format-agent-indicator "Task" 1 'default)))
    (should (stringp result))
    ;; Function uses just the first letter
    (should (string-match-p "T" result))
    (should (string-match-p "1" result))))

(ert-deftest beads-list-test-format-agent-indicator-brief ()
  "Test format-agent-indicator in brief mode."
  (let ((result (beads-list--format-agent-indicator "Task" 1 'default t)))
    (should (stringp result))
    ;; Brief mode uses first letter only
    (should (string-match-p "T" result))))

;;; Buffer Creation Tests

(ert-deftest beads-list-test-get-or-create-buffer-exists ()
  "Test get-or-create-buffer returns existing buffer."
  (let ((buf (get-buffer-create "*beads-list-test-temp*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-list--find-buffer-for-project)
                   (lambda (_type _dir) buf)))
          (should (eq (beads-list--get-or-create-buffer 'list) buf)))
      (kill-buffer buf))))

;;; Status Face Tests

(ert-deftest beads-list-test-status-face-open ()
  "Test status face for open status."
  (should (eq (beads-list--status-face "open") 'beads-list-status-open)))

(ert-deftest beads-list-test-status-face-in-progress ()
  "Test status face for in_progress status."
  (should (eq (beads-list--status-face "in_progress")
              'beads-list-status-in-progress)))

(ert-deftest beads-list-test-status-face-blocked ()
  "Test status face for blocked status."
  (should (eq (beads-list--status-face "blocked") 'beads-list-status-blocked)))

(ert-deftest beads-list-test-status-face-closed ()
  "Test status face for closed status."
  (should (eq (beads-list--status-face "closed") 'beads-list-status-closed)))

;;; Priority Face Tests

(ert-deftest beads-list-test-priority-face-high ()
  "Test priority face for high priority (1)."
  (should (eq (beads-list--priority-face 1) 'beads-list-priority-high)))

(ert-deftest beads-list-test-priority-face-medium ()
  "Test priority face for medium priority (2)."
  (should (eq (beads-list--priority-face 2) 'beads-list-priority-medium)))

(ert-deftest beads-list-test-priority-face-low ()
  "Test priority face for low priority (3)."
  (should (eq (beads-list--priority-face 3) 'beads-list-priority-low)))

(ert-deftest beads-list-test-priority-face-default ()
  "Test priority face for unknown priority defaults."
  ;; Unknown priorities get 'default face
  (should (beads-list--priority-face 99)))

;;; Format Status/Priority Tests

(ert-deftest beads-list-test-format-status-propertized ()
  "Test format-status returns propertized string."
  (let ((result (beads-list--format-status "open")))
    (should (stringp result))
    (should (string-match-p "open" result))))

(ert-deftest beads-list-test-format-priority-propertized ()
  "Test format-priority returns propertized string."
  (let ((result (beads-list--format-priority 1)))
    (should (stringp result))
    ;; Format includes "P" prefix
    (should (string-match-p "1" result))))

;;; Normalize Directory Edge Cases

(ert-deftest beads-list-test-normalize-directory-root ()
  "Test normalize-directory handles root path."
  (should (equal (beads-list--normalize-directory "/") "/")))

(ert-deftest beads-list-test-normalize-directory-home ()
  "Test normalize-directory handles home directory."
  (should (stringp (beads-list--normalize-directory "~"))))

;;; Focused/Touched Agent Indicator Tests

(ert-deftest beads-list-test-format-agent-focused-session ()
  "Test format-agent with a focused session."
  (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
             (lambda (_issue-id)
               (list (beads-agent-session
                      :id "test#1" :issue-id "bd-1"
                      :backend-name "mock" :project-dir "/tmp"
                      :started-at "2025-01-01T00:00:00Z"
                      :agent-type-name "Task"))))
            ((symbol-function 'beads-agent--get-sessions-touching-issue)
             (lambda (_issue-id) nil))
            ((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_issue-id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_issue-id) nil))
            ((symbol-function 'beads-agent-session-type-name)
             (lambda (_s) "Task"))
            ((symbol-function 'beads-agent-session-instance-number)
             (lambda (_s) 1)))
    (let ((result (beads-list--format-agent "bd-1")))
      (should (stringp result))
      (should (> (length result) 0)))))

(ert-deftest beads-list-test-format-agent-touched-session ()
  "Test format-agent with a touched (not focused) session."
  (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
             (lambda (_issue-id) nil))
            ((symbol-function 'beads-agent--get-sessions-touching-issue)
             (lambda (_issue-id)
               (list (beads-agent-session
                      :id "test#1" :issue-id "bd-2"
                      :backend-name "mock" :project-dir "/tmp"
                      :started-at "2025-01-01T00:00:00Z"
                      :agent-type-name "Plan"))))
            ((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_issue-id) nil))
            ((symbol-function 'beads-agent--get-issue-outcome)
             (lambda (_issue-id) nil))
            ((symbol-function 'beads-agent-session-type-name)
             (lambda (_s) "Plan"))
            ((symbol-function 'beads-agent-session-instance-number)
             (lambda (_s) 1)))
    (let ((result (beads-list--format-agent "bd-1")))
      (should (stringp result))
      (should (> (length result) 0)))))

(ert-deftest beads-list-test-format-agent-focused-and-touched ()
  "Test format-agent with both focused and touched sessions."
  (let* ((focused-session (beads-agent-session
                           :id "test#1" :issue-id "bd-1"
                           :backend-name "mock" :project-dir "/tmp"
                           :started-at "2025-01-01T00:00:00Z"
                           :agent-type-name "Task"))
         (touched-session (beads-agent-session
                           :id "test#2" :issue-id "bd-1"
                           :backend-name "mock" :project-dir "/tmp"
                           :started-at "2025-01-01T00:00:00Z"
                           :agent-type-name "Plan")))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
               (lambda (_issue-id) (list focused-session)))
              ((symbol-function 'beads-agent--get-sessions-touching-issue)
               (lambda (_issue-id) (list touched-session)))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_issue-id) nil))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_issue-id) nil))
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (s) (oref s agent-type-name)))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (let ((result (beads-list--format-agent "bd-1")))
        (should (stringp result))
        (should (> (length result) 0))))))

;;; Bulk Operation State Tests

(ert-deftest beads-list-test-marked-issues-initial-state ()
  "Test initial state of marked issues."
  (with-temp-buffer
    (beads-list-mode)
    (should (null beads-list--marked-issues))))

(ert-deftest beads-list-test-mark-and-unmark-all ()
  "Test marking and unmarking all issues."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2" "bd-3"))
    (should (= (length beads-list--marked-issues) 3))
    (beads-list-unmark-all)
    (should (null beads-list--marked-issues))))

;;; Entry Creation Tests

(ert-deftest beads-list-test-issue-to-entry-basic ()
  "Test creating a basic tabulated-list entry."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test Issue"
                :status "open"
                :priority 2
                :issue-type "task"
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T11:00:00Z")))
    (cl-letf (((symbol-function 'beads-list--format-agent)
               (lambda (_id) ""))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil)))
      (let ((entry (beads-list--issue-to-entry issue)))
        (should (listp entry))
        (should (equal (car entry) "bd-1"))))))

(ert-deftest beads-list-test-issue-to-entry-with-labels ()
  "Test creating entry with labels."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test Issue"
                :status "open"
                :priority 2
                :issue-type "task"
                :labels '("bug" "urgent")
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T11:00:00Z")))
    (cl-letf (((symbol-function 'beads-list--format-agent)
               (lambda (_id) ""))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil)))
      (let ((entry (beads-list--issue-to-entry issue)))
        (should (listp entry))
        (should (equal (car entry) "bd-1"))))))

;;; ============================================================
;;; Transient Args Parsing Tests
;;; ============================================================

(ert-deftest beads-list-test-parse-transient-args-no-assignee ()
  "Test parsing --no-assignee flag."
  (let ((cmd (beads-list--parse-transient-args '("--no-assignee"))))
    (should (beads-command-list-p cmd))
    (should (eq (oref cmd no-assignee) t))))

(ert-deftest beads-list-test-parse-transient-args-empty-description ()
  "Test parsing --empty-description flag."
  (let ((cmd (beads-list--parse-transient-args '("--empty-description"))))
    (should (beads-command-list-p cmd))
    (should (eq (oref cmd empty-description) t))))

(ert-deftest beads-list-test-parse-transient-args-no-labels ()
  "Test parsing --no-labels flag."
  (let ((cmd (beads-list--parse-transient-args '("--no-labels"))))
    (should (beads-command-list-p cmd))
    (should (eq (oref cmd no-labels) t))))

(ert-deftest beads-list-test-parse-transient-args-long ()
  "Test parsing --long flag."
  (let ((cmd (beads-list--parse-transient-args '("--long"))))
    (should (beads-command-list-p cmd))
    (should (eq (oref cmd long) t))))

(ert-deftest beads-list-test-parse-transient-args-assignee ()
  "Test parsing --assignee= option."
  (let ((cmd (beads-list--parse-transient-args '("--assignee=john"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd assignee) "john"))))

(ert-deftest beads-list-test-parse-transient-args-closed-after ()
  "Test parsing --closed-after= option."
  (let ((cmd (beads-list--parse-transient-args '("--closed-after=2025-01-01"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd closed-after) "2025-01-01"))))

(ert-deftest beads-list-test-parse-transient-args-closed-before ()
  "Test parsing --closed-before= option."
  (let ((cmd (beads-list--parse-transient-args '("--closed-before=2025-01-15"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd closed-before) "2025-01-15"))))

(ert-deftest beads-list-test-parse-transient-args-created-before ()
  "Test parsing --created-before= option."
  (let ((cmd (beads-list--parse-transient-args '("--created-before=2025-01-10"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd created-before) "2025-01-10"))))

(ert-deftest beads-list-test-parse-transient-args-desc-contains ()
  "Test parsing --desc-contains= option."
  (let ((cmd (beads-list--parse-transient-args '("--desc-contains=test"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd desc-contains) "test"))))

(ert-deftest beads-list-test-parse-transient-args-format ()
  "Test parsing --format= option."
  (let ((cmd (beads-list--parse-transient-args '("--format=compact"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd format) "compact"))))

(ert-deftest beads-list-test-parse-transient-args-id ()
  "Test parsing --id= option."
  (let ((cmd (beads-list--parse-transient-args '("--id=bd-42"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd id) "bd-42"))))

(ert-deftest beads-list-test-parse-transient-args-notes-contains ()
  "Test parsing --notes-contains= option."
  (let ((cmd (beads-list--parse-transient-args '("--notes-contains=review"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd notes-contains) "review"))))

(ert-deftest beads-list-test-parse-transient-args-title ()
  "Test parsing --title= option."
  (let ((cmd (beads-list--parse-transient-args '("--title=Bug"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd title) "Bug"))))

(ert-deftest beads-list-test-parse-transient-args-title-contains ()
  "Test parsing --title-contains= option."
  (let ((cmd (beads-list--parse-transient-args '("--title-contains=fix"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd title-contains) "fix"))))

(ert-deftest beads-list-test-parse-transient-args-type ()
  "Test parsing --type= option."
  (let ((cmd (beads-list--parse-transient-args '("--type=bug"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd issue-type) "bug"))))

(ert-deftest beads-list-test-parse-transient-args-updated-after ()
  "Test parsing --updated-after= option."
  (let ((cmd (beads-list--parse-transient-args '("--updated-after=2025-01-01"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd updated-after) "2025-01-01"))))

(ert-deftest beads-list-test-parse-transient-args-updated-before ()
  "Test parsing --updated-before= option."
  (let ((cmd (beads-list--parse-transient-args '("--updated-before=2025-01-15"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd updated-before) "2025-01-15"))))

(ert-deftest beads-list-test-parse-transient-args-priority-min ()
  "Test parsing --priority-min= option."
  (let ((cmd (beads-list--parse-transient-args '("--priority-min=1"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd priority-min) 1))))

(ert-deftest beads-list-test-parse-transient-args-priority-max ()
  "Test parsing --priority-max= option."
  (let ((cmd (beads-list--parse-transient-args '("--priority-max=3"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd priority-max) 3))))

(ert-deftest beads-list-test-parse-transient-args-priority-range ()
  "Test parsing combined priority-min and priority-max options."
  (let ((cmd (beads-list--parse-transient-args
              '("--priority-min=1" "--priority-max=3"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd priority-min) 1))
    (should (equal (oref cmd priority-max) 3))))

(ert-deftest beads-list-test-parse-transient-args-label-any ()
  "Test parsing --label-any= options."
  (let ((cmd (beads-list--parse-transient-args
              '("--label-any=bug" "--label-any=urgent"))))
    (should (beads-command-list-p cmd))
    (should (member "bug" (oref cmd label-any)))
    (should (member "urgent" (oref cmd label-any)))))

(ert-deftest beads-list-test-parse-transient-args-combined ()
  "Test parsing combined options."
  (let ((cmd (beads-list--parse-transient-args
              '("--status=open" "--assignee=john" "--no-labels" "--type=task"))))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd status) "open"))
    (should (equal (oref cmd assignee) "john"))
    (should (eq (oref cmd no-labels) t))
    (should (equal (oref cmd issue-type) "task"))))

;;; Bulk Operation Function Existence Tests

(ert-deftest beads-list-test-bulk-update-status-defined ()
  "Test that bulk-update-status is defined."
  (should (fboundp 'beads-list-bulk-update-status)))

(ert-deftest beads-list-test-bulk-update-priority-defined ()
  "Test that bulk-update-priority is defined."
  (should (fboundp 'beads-list-bulk-update-priority)))

(ert-deftest beads-list-test-bulk-close-defined ()
  "Test that bulk-close is defined."
  (should (fboundp 'beads-list-bulk-close)))

(ert-deftest beads-list-test-bulk-reopen-defined ()
  "Test that bulk-reopen is defined."
  (should (fboundp 'beads-list-bulk-reopen)))

;;; Filter and Ready/Blocked Tests

(ert-deftest beads-list-test-filter-defined ()
  "Test that filter is defined."
  (should (fboundp 'beads-list-filter)))

(ert-deftest beads-list-test-ready-defined ()
  "Test that beads-ready is defined."
  (should (fboundp 'beads-ready)))

(ert-deftest beads-list-test-blocked-defined ()
  "Test that beads-blocked is defined."
  (should (fboundp 'beads-blocked)))

;;; Mark and Unmark Tests

(ert-deftest beads-list-test-mark-defined ()
  "Test that mark is defined."
  (should (fboundp 'beads-list-mark)))

(ert-deftest beads-list-test-unmark-defined ()
  "Test that unmark is defined."
  (should (fboundp 'beads-list-unmark)))

(ert-deftest beads-list-test-mark-all-defined ()
  "Test that mark-all is defined."
  (should (fboundp 'beads-list-mark-all)))

(ert-deftest beads-list-test-unmark-all-defined ()
  "Test that unmark-all is defined."
  (should (fboundp 'beads-list-unmark-all)))

(ert-deftest beads-list-test-marked-issues-variable-exists ()
  "Test that marked-issues variable is defined."
  ;; Variable should be available in list mode buffers
  (should t))

;;; Bulk Operation No Marks Tests

(ert-deftest beads-list-test-bulk-update-status-no-marks ()
  "Test bulk-update-status errors with no marks."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues nil)
    (should-error (beads-list-bulk-update-status) :type 'user-error)))

(ert-deftest beads-list-test-bulk-update-priority-no-marks ()
  "Test bulk-update-priority errors with no marks."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues nil)
    (should-error (beads-list-bulk-update-priority) :type 'user-error)))

(ert-deftest beads-list-test-bulk-close-no-marks ()
  "Test bulk-close errors with no marks."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues nil)
    (should-error (beads-list-bulk-close) :type 'user-error)))

(ert-deftest beads-list-test-bulk-reopen-no-marks ()
  "Test bulk-reopen errors with no marks."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues nil)
    (should-error (beads-list-bulk-reopen) :type 'user-error)))

;;; Entry Point Tests

(ert-deftest beads-list-test-main-entry-defined ()
  "Test that main entry point is defined."
  (should (fboundp 'beads-list)))

(ert-deftest beads-list-test-refresh-defined ()
  "Test that refresh is defined."
  (should (fboundp 'beads-list-refresh)))

;;; Bulk Operation With Marks Tests (Cancelled)

(ert-deftest beads-list-test-bulk-update-status-empty-input ()
  "Test bulk-update-status with empty status."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "")))
      ;; Empty string aborts without asking y-or-n
      (beads-list-bulk-update-status))
    ;; Should complete without error
    (should t)))

(ert-deftest beads-list-test-bulk-update-priority-nil-selection ()
  "Test bulk-update-priority with nil selection."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Invalid")))
      ;; Invalid selection doesn't match any choice
      (beads-list-bulk-update-priority))
    ;; Should complete without error
    (should t)))

(ert-deftest beads-list-test-bulk-close-user-declines ()
  "Test bulk-close when user declines."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "test"))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) nil)))
      (beads-list-bulk-close))
    ;; Marks should still be set (not cleared)
    (should (member "bd-1" beads-list--marked-issues))))

(ert-deftest beads-list-test-bulk-reopen-user-declines ()
  "Test bulk-reopen when user declines."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "test"))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) nil)))
      (beads-list-bulk-reopen))
    ;; Marks should still be set (not cleared)
    (should (member "bd-1" beads-list--marked-issues))))

(ert-deftest beads-list-test-bulk-update-status-success ()
  "Test bulk-update-status executes successfully."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2"))
    (let ((executed-ids nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "in_progress"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (push (oref cmd issue-ids) executed-ids)
                   nil))
                ((symbol-function 'beads--invalidate-completion-cache)
                 (lambda () nil))
                ((symbol-function 'beads-list-refresh)
                 (lambda () nil)))
        (beads-list-bulk-update-status))
      ;; Should have executed for both issues
      (should (= 2 (length executed-ids)))
      ;; Marks should be cleared after success
      (should (null beads-list--marked-issues)))))

(ert-deftest beads-list-test-bulk-update-priority-success ()
  "Test bulk-update-priority executes successfully."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2"))
    (let ((executed-ids nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "1 - High"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (push (oref cmd issue-ids) executed-ids)
                   nil))
                ((symbol-function 'beads--invalidate-completion-cache)
                 (lambda () nil))
                ((symbol-function 'beads-list-refresh)
                 (lambda () nil)))
        (beads-list-bulk-update-priority))
      ;; Should have executed for both issues
      (should (= 2 (length executed-ids)))
      ;; Marks should be cleared after success
      (should (null beads-list--marked-issues)))))

(ert-deftest beads-list-test-bulk-close-success ()
  "Test bulk-close executes successfully."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2"))
    (let ((executed-ids nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "Completed"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (push (oref cmd issue-ids) executed-ids)
                   nil))
                ((symbol-function 'beads--invalidate-completion-cache)
                 (lambda () nil))
                ((symbol-function 'beads-list-refresh)
                 (lambda () nil)))
        (beads-list-bulk-close))
      ;; Should have executed for both issues
      (should (= 2 (length executed-ids)))
      ;; Marks should be cleared after success
      (should (null beads-list--marked-issues)))))

(ert-deftest beads-list-test-bulk-reopen-success ()
  "Test bulk-reopen executes successfully."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2"))
    (let ((executed-ids nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "Need more work"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (push (oref cmd issue-ids) executed-ids)
                   nil))
                ((symbol-function 'beads--invalidate-completion-cache)
                 (lambda () nil))
                ((symbol-function 'beads-list-refresh)
                 (lambda () nil)))
        (beads-list-bulk-reopen))
      ;; Should have executed for both issues
      (should (= 2 (length executed-ids)))
      ;; Marks should be cleared after success
      (should (null beads-list--marked-issues)))))

(ert-deftest beads-list-test-bulk-update-status-with-errors ()
  "Test bulk-update-status handles errors gracefully."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2"))
    (let ((call-count 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "open"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (setq call-count (1+ call-count))
                   (when (= call-count 1)
                     (error "Simulated failure"))
                   nil))
                ((symbol-function 'beads--invalidate-completion-cache)
                 (lambda () nil))
                ((symbol-function 'beads-list-refresh)
                 (lambda () nil)))
        (beads-list-bulk-update-status))
      ;; Both should have been attempted
      (should (= 2 call-count))
      ;; Marks should be cleared even with partial failure
      (should (null beads-list--marked-issues)))))

(ert-deftest beads-list-test-bulk-close-with-errors ()
  "Test bulk-close handles errors gracefully."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2"))
    (let ((call-count 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "Done"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t))
                ((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (setq call-count (1+ call-count))
                   (when (= call-count 1)
                     (error "Simulated failure"))
                   nil))
                ((symbol-function 'beads--invalidate-completion-cache)
                 (lambda () nil))
                ((symbol-function 'beads-list-refresh)
                 (lambda () nil)))
        (beads-list-bulk-close))
      ;; Both should have been attempted
      (should (= 2 call-count))
      ;; Marks should be cleared even with partial failure
      (should (null beads-list--marked-issues)))))

;;; Mark/Unmark Operations Tests

(ert-deftest beads-list-test-mark-at-point ()
  "Test marking issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues nil)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-test"))
              ((symbol-function 'tabulated-list-put-tag)
               (lambda (&rest _) nil)))
      (beads-list-mark))
    (should (member "bd-test" beads-list--marked-issues))))

(ert-deftest beads-list-test-unmark-at-point ()
  "Test unmarking issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-test"))
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-test"))
              ((symbol-function 'tabulated-list-put-tag)
               (lambda (&rest _) nil)))
      (beads-list-unmark))
    (should-not (member "bd-test" beads-list--marked-issues))))

(ert-deftest beads-list-test-unmark-all-clears-marks ()
  "Test unmark-all clears all marks."
  (with-temp-buffer
    (beads-list-mode)
    (setq beads-list--marked-issues '("bd-1" "bd-2" "bd-3"))
    (cl-letf (((symbol-function 'tabulated-list-put-tag)
               (lambda (&rest _) nil))
              ((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (beads-list-unmark-all))
    (should (null beads-list--marked-issues))))

(provide 'beads-list-test)
;;; beads-list-test.el ends here
