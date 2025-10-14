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
  "Test that mode line shows issue count."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest _)
               (apply #'vector beads-list-test--sample-issues)))
            ((symbol-function 'beads-check-executable)
             (lambda () t)))
    (beads-list)
    ;; Check mode-line-format contains the count
    (should (member "4 issues" mode-line-format))
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
    (should (lookup-key beads-list-mode-map (kbd "M")))
    (should (lookup-key beads-list-mode-map (kbd "U")))))

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
                #'beads-list-quit))))

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

;;; Footer

(provide 'beads-list-test)
;;; beads-list-test.el ends here
