;;; beads-list-test.el --- Tests for beads-list.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Comprehensive tests for beads-list.el tabulated-list mode.
;; Covers buffer creation, data parsing, sorting, navigation, marking,
;; and visual styling.

;;; Code:

(require 'ert)
(require 'beads-list)

;;; Test Data

(defvar beads-list-test--sample-issues
  '(((id . "bd-1")
     (title . "First issue")
     (status . "open")
     (priority . 1)
     (issue-type . "bug"))
    ((id . "bd-2")
     (title . "Second issue")
     (status . "in_progress")
     (priority . 0)
     (issue-type . "feature"))
    ((id . "bd-3")
     (title . "Third issue")
     (status . "blocked")
     (priority . 2)
     (issue-type . "task"))
    ((id . "bd-4")
     (title . "Fourth issue")
     (status . "closed")
     (priority . 3)
     (issue-type . "bug")))
  "Sample issue data for testing.")

(defvar beads-list-test--empty-issues '()
  "Empty issue list for testing.")

;;; Helper Functions

(defmacro beads-list-test--with-temp-buffer (issues command &rest body)
  "Create a temporary beads-list buffer with ISSUES and COMMAND, then run BODY."
  (declare (indent 2))
  `(with-temp-buffer
     (beads-list-mode)
     (beads-list--populate-buffer ,issues ,command)
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
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    ;; Test list buffer
    (beads-list)
    (should (equal (buffer-name) "*beads-list*"))
    (kill-buffer)

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
  "Test conversion of issue alist to tabulated-list entry."
  (let* ((issue (car beads-list-test--sample-issues))
         (entry (beads-list--issue-to-entry issue)))
    (should (equal (car entry) "bd-1"))
    (should (vectorp (cadr entry)))
    (should (= (length (cadr entry)) 5))
    ;; Check ID column
    (should (equal (aref (cadr entry) 0) "bd-1"))
    ;; Check title column
    (should (equal (aref (cadr entry) 4) "First issue"))))

(ert-deftest beads-list-test-populate-buffer ()
  "Test populating buffer with issues."
  (beads-list-test--with-temp-buffer
      beads-list-test--sample-issues 'list
    (should (= (length tabulated-list-entries) 4))
    (should (eq beads-list--command 'list))
    (should (equal beads-list--raw-issues beads-list-test--sample-issues))))

(ert-deftest beads-list-test-empty-issues ()
  "Test handling of empty issue list."
  (beads-list-test--with-temp-buffer
      beads-list-test--empty-issues 'list
    (should (= (length tabulated-list-entries) 0))))

(ert-deftest beads-list-test-malformed-data ()
  "Test handling of malformed issue data."
  (let ((malformed-issue '((id . "bd-1"))))  ; Missing required fields
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
    ;; Already sorted by Priority (default)
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
      (should (equal (alist-get 'title issue) "Second issue"))
      (should (equal (alist-get 'status issue) "in_progress")))))

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
    ;; Test filter prefix key exists and is a keymap
    (should (keymapp (lookup-key beads-list-mode-map (kbd "/"))))))

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
    ;; Test filter keybindings
    (let ((filter-map (lookup-key beads-list-mode-map (kbd "/"))))
      (should (keymapp filter-map))
      (should (eq (lookup-key filter-map (kbd "s"))
                  #'beads-list-filter-by-status))
      (should (eq (lookup-key filter-map (kbd "p"))
                  #'beads-list-filter-by-priority))
      (should (eq (lookup-key filter-map (kbd "t"))
                  #'beads-list-filter-by-type))
      (should (eq (lookup-key filter-map (kbd "/"))
                  #'beads-list-filter-by-text))
      (should (eq (lookup-key filter-map (kbd "c"))
                  #'beads-list-clear-filters)))))

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
             (issue-type . "bug"))
            ((id . "myproject-13")
             (title . "Custom prefix issue")
             (status . "in_progress")
             (priority . 0)
             (issue-type . "feature"))))
         (beads-show-called nil)
         (beads-show-arg nil))
    (beads-list-test--with-temp-buffer
        custom-prefix-issues 'list
      (goto-char (point-min))
      ;; Default sort is by Priority, so myproject-13 (pri 0) comes first
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
    (should (= (length tabulated-list-format) 5))
    (should (equal (car (aref tabulated-list-format 0)) "ID"))
    (should (equal (car (aref tabulated-list-format 1)) "Status"))
    (should (equal (car (aref tabulated-list-format 2)) "Priority"))
    (should (equal (car (aref tabulated-list-format 3)) "Type"))
    (should (equal (car (aref tabulated-list-format 4)) "Title"))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-list-test-integration-mode-defined ()
  "Integration test: Verify beads-list-mode is defined."
  :tags '(integration)
  (should (fboundp 'beads-list-mode)))

(ert-deftest beads-list-test-integration-keybinding-n-next ()
  "Integration test: Verify n keybinding for next."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "n"))))
      (should (eq binding 'beads-list-next)))))

(ert-deftest beads-list-test-integration-keybinding-p-previous ()
  "Integration test: Verify p keybinding for previous."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "p"))))
      (should (eq binding 'beads-list-previous)))))

(ert-deftest beads-list-test-integration-keybinding-ret-show ()
  "Integration test: Verify RET keybinding for show."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "RET"))))
      (should (eq binding 'beads-list-show)))))

(ert-deftest beads-list-test-integration-keybinding-g-refresh ()
  "Integration test: Verify g keybinding for refresh."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "g"))))
      (should (eq binding 'beads-list-refresh)))))

(ert-deftest beads-list-test-integration-keybinding-q-quit ()
  "Integration test: Verify q keybinding for quit."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "q"))))
      (should (eq binding 'beads-list-quit)))))

(ert-deftest beads-list-test-integration-keybinding-m-mark ()
  "Integration test: Verify m keybinding for mark."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "m"))))
      (should (eq binding 'beads-list-mark)))))

(ert-deftest beads-list-test-integration-keybinding-u-unmark ()
  "Integration test: Verify u keybinding for unmark."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "u"))))
      (should (eq binding 'beads-list-unmark)))))

(ert-deftest beads-list-test-integration-keybinding-w-copy ()
  "Integration test: Verify w keybinding for copy-id."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "w"))))
      (should (eq binding 'beads-list-copy-id)))))

(ert-deftest beads-list-test-integration-keybinding-S-sort ()
  "Integration test: Verify S keybinding for sort."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "S"))))
      (should (eq binding 'beads-list-sort)))))

(ert-deftest beads-list-test-integration-list-mode-setup ()
  "Integration test: Verify list mode can be set up."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (should (eq major-mode 'beads-list-mode))
    (should (boundp 'tabulated-list-format))
    (should tabulated-list-format)))

(ert-deftest beads-list-test-integration-list-command-exists ()
  "Integration test: Verify beads-list command exists."
  :tags '(integration)
  (should (fboundp 'beads-list)))

(ert-deftest beads-list-test-integration-ready-command-exists ()
  "Integration test: Verify beads-ready command exists."
  :tags '(integration)
  (should (fboundp 'beads-ready)))

(ert-deftest beads-list-test-integration-blocked-command-exists ()
  "Integration test: Verify beads-blocked command exists."
  :tags '(integration)
  (should (fboundp 'beads-blocked)))

(ert-deftest beads-list-test-integration-workflow-to-show ()
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

(ert-deftest beads-list-test-integration-workflow-to-update ()
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

(ert-deftest beads-list-test-integration-workflow-to-close ()
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

(ert-deftest beads-list-test-integration-workflow-to-delete ()
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

(ert-deftest beads-list-test-integration-workflow-to-create ()
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

(ert-deftest beads-list-test-integration-context-detection ()
  "Integration test: Context detection works in list mode."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    ;; List mode should be active
    (should (eq major-mode 'beads-list-mode))))

;;; Footer

(provide 'beads-list-test)
;;; beads-list-test.el ends here
