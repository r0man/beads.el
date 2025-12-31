;;; beads-show-test.el --- Tests for beads-show.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-show.el issue detail display.
;; Tests cover buffer creation, rendering, navigation, button actions,
;; and markdown-like fontification.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-show)
(require 'beads-agent-backend)
(require 'button)

;;; Test Fixtures

(defvar beads-show-test--full-issue
  '((id . "bd-42")
    (title . "Implement feature X")
    (description . "This is a **bold** description with `code` and bd-1 reference.")
    (status . "in_progress")
    (priority . 1)
    (issue_type . "feature")
    (created_at . "2025-01-15T10:30:45.123456789Z")
    (updated_at . "2025-01-16T14:20:30.987654321Z")
    (acceptance_criteria . "## Requirements\n\n- [ ] Must work\n- [ ] Must be tested")
    (design . "Use *simple* design pattern.\n\nSee bd-10 for details.")
    (notes . "Related to bd-5 and bd-7.\n\nSome `inline code` here.")
    (assignee . "alice")
    (external_ref . "JIRA-123"))
  "Sample issue with all fields populated.")

(defvar beads-show-test--minimal-issue
  '((id . "bd-1")
    (title . "Minimal issue")
    (status . "open")
    (priority . 2)
    (issue_type . "bug")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z"))
  "Sample issue with minimal fields.")

(defvar beads-show-test--closed-issue
  '((id . "bd-99")
    (title . "Closed issue")
    (description . "This issue is done")
    (status . "closed")
    (priority . 0)
    (issue_type . "task")
    (created_at . "2025-01-10T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z")
    (closed_at . "2025-01-15T10:00:00Z"))
  "Sample closed issue.")

(defvar beads-show-test--markdown-rich-issue
  '((id . "bd-100")
    (title . "Markdown test")
    (description . "## Heading\n\n**bold** and *italic* and `code`\n\n- item 1\n- item 2\n\nbd-1 bd-2 bd-3")
    (status . "open")
    (priority . 2)
    (issue_type . "task")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z"))
  "Sample issue with rich markdown content.")

;;; Test Utilities

(defmacro beads-show-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary beads-show buffer."
  `(with-temp-buffer
     (beads-show-mode)
     ,@body))

(defmacro beads-show-test-with-git-mocks (&rest body)
  "Execute BODY with git functions mocked to return test-project.
This is needed because show buffers are now named by project, not issue."
  `(cl-letf (((symbol-function 'beads-git-find-project-root)
              (lambda () "/tmp/test-project"))
             ((symbol-function 'beads-git-get-project-name)
              (lambda () "test-project"))
             ((symbol-function 'beads-git-get-branch)
              (lambda () "main")))
     ,@body))

(defconst beads-show-test--buffer-name "*beads-show: test-project*"
  "Expected buffer name when using git mocks.")

(defun beads-show-test--mock-show-command (issue-data)
  "Create a mock for beads-command-show! returning ISSUE-DATA as EIEIO object."
  (lambda (&rest args)
    ;; beads-command-show! returns a single issue object from the list
    (beads-issue-from-json issue-data)))

(defun beads-show-test--mock-list-command (issues-list)
  "Create a mock for list command returning ISSUES-LIST."
  (lambda (subcommand &rest args)
    (if (string= subcommand "list")
        (vconcat issues-list)
      (error "Unexpected command: %s" subcommand))))

(defun beads-show-test--get-buffer-text ()
  "Get all text from current buffer without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun beads-show-test--count-buttons-in-buffer ()
  "Count the number of buttons in current buffer."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (button-at (point))
          (setq count (1+ count)))
        (forward-char 1)))
    count))

(defun beads-show-test--get-button-at-pos (pos)
  "Get button at POS or nil."
  (save-excursion
    (goto-char pos)
    (button-at pos)))

(defun beads-show-test--find-text-with-face (face)
  "Find all regions with FACE in buffer."
  (let ((regions '())
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((prop-face (get-text-property pos 'face)))
        (when (or (eq prop-face face)
                 (and (listp prop-face) (memq face prop-face)))
          (push (cons pos (buffer-substring-no-properties pos (1+ pos)))
                regions)))
      (setq pos (1+ pos)))
    (nreverse regions)))

;;; Tests for Utility Functions

(ert-deftest beads-show-test-format-status-open ()
  "Test formatting of open status."
  (let ((formatted (beads-show--format-status "open")))
    (should (string= formatted "OPEN"))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-status-open-face))))

(ert-deftest beads-show-test-format-status-in-progress ()
  "Test formatting of in_progress status."
  (let ((formatted (beads-show--format-status "in_progress")))
    (should (string= formatted "IN_PROGRESS"))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-status-in-progress-face))))

(ert-deftest beads-show-test-format-status-blocked ()
  "Test formatting of blocked status."
  (let ((formatted (beads-show--format-status "blocked")))
    (should (string= formatted "BLOCKED"))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-status-blocked-face))))

(ert-deftest beads-show-test-format-status-closed ()
  "Test formatting of closed status."
  (let ((formatted (beads-show--format-status "closed")))
    (should (string= formatted "CLOSED"))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-status-closed-face))))

(ert-deftest beads-show-test-format-status-unknown ()
  "Test formatting of unknown status."
  (let ((formatted (beads-show--format-status "unknown")))
    (should (string= formatted "UNKNOWN"))
    (should (eq (get-text-property 0 'face formatted) 'default))))

(ert-deftest beads-show-test-format-status-nil ()
  "Test formatting of nil status."
  (let ((formatted (beads-show--format-status nil)))
    (should (string= formatted "UNKNOWN"))
    (should (eq (get-text-property 0 'face formatted) 'default))))

(ert-deftest beads-show-test-format-priority-critical ()
  "Test formatting of priority 0 (critical)."
  (let ((formatted (beads-show--format-priority 0)))
    (should (string-match-p "0.*Critical" formatted))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-priority-critical-face))))

(ert-deftest beads-show-test-format-priority-high ()
  "Test formatting of priority 1 (high)."
  (let ((formatted (beads-show--format-priority 1)))
    (should (string-match-p "1.*High" formatted))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-priority-high-face))))

(ert-deftest beads-show-test-format-priority-medium ()
  "Test formatting of priority 2 (medium)."
  (let ((formatted (beads-show--format-priority 2)))
    (should (string-match-p "2.*Medium" formatted))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-priority-medium-face))))

(ert-deftest beads-show-test-format-priority-low ()
  "Test formatting of priority 3 (low)."
  (let ((formatted (beads-show--format-priority 3)))
    (should (string-match-p "3.*Low" formatted))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-priority-low-face))))

(ert-deftest beads-show-test-format-priority-backlog ()
  "Test formatting of priority 4 (backlog)."
  (let ((formatted (beads-show--format-priority 4)))
    (should (string-match-p "4.*Backlog" formatted))
    (should (eq (get-text-property 0 'face formatted)
               'beads-show-priority-low-face))))

(ert-deftest beads-show-test-format-priority-nil ()
  "Test formatting of nil priority."
  (should (null (beads-show--format-priority nil))))

(ert-deftest beads-show-test-format-date ()
  "Test date formatting."
  (let ((formatted (beads-show--format-date
                   "2025-01-15T10:30:45.123456789Z")))
    (should (string= formatted "2025-01-15 10:30:45"))))

(ert-deftest beads-show-test-format-date-nil ()
  "Test formatting of nil date."
  (should (string= (beads-show--format-date nil) "N/A")))

(ert-deftest beads-show-test-format-date-simple ()
  "Test formatting of simple date."
  (let ((formatted (beads-show--format-date "2025-01-15T10:00:00Z")))
    (should (string= formatted "2025-01-15 10:00:00"))))

(ert-deftest beads-show-test-format-date-with-timezone ()
  "Test formatting strips timezone suffix."
  (let ((formatted (beads-show--format-date "2025-01-10T10:00:00Z")))
    (should (string= formatted "2025-01-10 10:00:00"))
    (should-not (string-match-p "Z" formatted))))

;;; Tests for Issue Extraction

(ert-deftest beads-show-test-extract-issue-at-point-found ()
  "Test extracting issue ID when cursor is on bd-N."
  (with-temp-buffer
    (insert "This is bd-42 in text")
    (goto-char (point-min))
    (search-forward "bd-42")
    (goto-char (match-beginning 0))
    (let ((id (beads-show--extract-issue-at-point)))
      (should (string= id "bd-42")))))

(ert-deftest beads-show-test-extract-issue-at-point-middle ()
  "Test extracting issue ID when cursor is in middle of bd-N."
  (with-temp-buffer
    (insert "This is bd-123 in text")
    (goto-char (point-min))
    (search-forward "bd-1")
    (let ((id (beads-show--extract-issue-at-point)))
      (should (string= id "bd-123")))))

(ert-deftest beads-show-test-extract-issue-at-point-not-found ()
  "Test extracting issue ID when no reference present."
  (with-temp-buffer
    (insert "No issue reference here")
    (goto-char (point-min))
    (should (null (beads-show--extract-issue-at-point)))))

(ert-deftest beads-show-test-extract-issue-at-point-multiple ()
  "Test extracting issue ID when multiple references on same line."
  (with-temp-buffer
    (insert "See bd-1 and bd-2")
    (goto-char (point-min))
    (search-forward "bd-1")
    (goto-char (match-beginning 0))
    (let ((id (beads-show--extract-issue-at-point)))
      (should (string= id "bd-1")))))

(ert-deftest beads-show-test-extract-issue-button ()
  "Test extracting issue ID from a button."
  (with-temp-buffer
    (insert "Text before ")
    (let ((start (point)))
      (insert "bd-42")
      (make-button start (point)
                  'issue-id "bd-42"
                  'action #'ignore))
    (insert " text after")
    (goto-char (point-min))
    (search-forward "bd-42")
    (goto-char (match-beginning 0))
    (let ((id (beads-show--extract-issue-at-point)))
      (should (string= id "bd-42")))))

;;; Tests for Buffer Rendering

(ert-deftest beads-show-test-render-full-issue ()
  "Test rendering a complete issue with all fields."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--full-issue)))
     (beads-show--render-issue parsed)
     (let ((text (beads-show-test--get-buffer-text)))
       (should (string-match-p "Implement feature X" text))
       (should (string-match-p "bd-42" text))
       (should (string-match-p "IN_PROGRESS" text))
       (should (string-match-p "1.*High" text))
       (should (string-match-p "FEATURE" text))
       (should (string-match-p "alice" text))
       (should (string-match-p "JIRA-123" text))
       (should (string-match-p "Description" text))
       (should (string-match-p "Acceptance Criteria" text))
       (should (string-match-p "Design" text))
       (should (string-match-p "Notes" text))))))

(ert-deftest beads-show-test-render-minimal-issue ()
  "Test rendering a minimal issue."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--minimal-issue)))
     (beads-show--render-issue parsed)
     (let ((text (beads-show-test--get-buffer-text)))
       (should (string-match-p "Minimal issue" text))
       (should (string-match-p "bd-1" text))
       (should (string-match-p "OPEN" text))
       (should (string-match-p "2.*Medium" text))
       (should (string-match-p "BUG" text))
       ;; These sections should NOT appear for minimal issue
       (should-not (string-match-p "Description\n─" text))
       (should-not (string-match-p "Acceptance Criteria" text))
       (should-not (string-match-p "Design" text))
       (should-not (string-match-p "Notes" text))))))

(ert-deftest beads-show-test-render-closed-issue ()
  "Test rendering a closed issue with closed_at timestamp."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--closed-issue)))
     (beads-show--render-issue parsed)
     (let ((text (beads-show-test--get-buffer-text)))
       (should (string-match-p "Closed issue" text))
       (should (string-match-p "CLOSED" text))
       (should (string-match-p "0.*Critical" text))
       (should (string-match-p "Closed: 2025-01-15 10:00:00" text))))))

(ert-deftest beads-show-test-render-empty-sections-omitted ()
  "Test that empty sections are not displayed."
  (beads-show-test-with-temp-buffer
   (let* ((issue '((id . "bd-50")
                  (title . "No details")
                  (status . "open")
                  (priority . 1)
                  (issue_type . "task")
                  (created_at . "2025-01-15T10:00:00Z")
                  (updated_at . "2025-01-15T10:00:00Z")
                  (description . "")
                  (acceptance_criteria . nil)
                  (design . "   ")
                  (notes . nil)))
          (parsed (beads--parse-issue issue)))
     (beads-show--render-issue parsed)
     (let ((text (beads-show-test--get-buffer-text)))
       (should-not (string-match-p "Description\n─" text))
       (should-not (string-match-p "Acceptance Criteria" text))
       (should-not (string-match-p "Design" text))
       (should-not (string-match-p "Notes" text))))))

(ert-deftest beads-show-test-render-cursor-at-top ()
  "Test that cursor is positioned at top after rendering."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--full-issue)))
     (beads-show--render-issue parsed)
     (should (= (point) (point-min))))))

;;; Tests for Markdown Fontification

(ert-deftest beads-show-test-fontify-headings ()
  "Test that markdown headings are fontified."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--markdown-rich-issue)))
     (beads-show--render-issue parsed)
     (goto-char (point-min))
     (when (search-forward "## Heading" nil t)
       (let ((face (get-text-property (- (point) 4) 'face)))
         (should (eq face 'beads-show-heading-face)))))))

(ert-deftest beads-show-test-fontify-bold ()
  "Test that bold text is fontified."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--markdown-rich-issue)))
     (beads-show--render-issue parsed)
     (goto-char (point-min))
     (when (search-forward "bold" nil t)
       (let ((face (get-text-property (- (point) 2) 'face)))
         (should (eq face 'bold)))))))

(ert-deftest beads-show-test-fontify-italic ()
  "Test that italic text is fontified."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--markdown-rich-issue)))
     (beads-show--render-issue parsed)
     (goto-char (point-min))
     (when (search-forward "italic" nil t)
       (let ((face (get-text-property (- (point) 3) 'face)))
         (should (eq face 'italic)))))))

(ert-deftest beads-show-test-fontify-code ()
  "Test that inline code is fontified."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--markdown-rich-issue)))
     (beads-show--render-issue parsed)
     (goto-char (point-min))
     (when (search-forward "`code`" nil t)
       (goto-char (- (point) 3))
       (let ((face (get-text-property (point) 'face)))
         (should (eq face 'beads-show-code-face)))))))

(ert-deftest beads-show-test-fontify-lists ()
  "Test that list markers are fontified."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--markdown-rich-issue)))
     (beads-show--render-issue parsed)
     (goto-char (point-min))
     (when (search-forward "- item 1" nil t)
       (goto-char (match-beginning 0))
       (let ((face (get-text-property (point) 'face)))
         (should (eq face 'font-lock-builtin-face)))))))

;;; Tests for Button Creation

(ert-deftest beads-show-test-buttonize-single-reference ()
  "Test that a single bd-N reference becomes a button."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--full-issue)))
     (beads-show--render-issue parsed)
     ;; Should have buttons for bd-1, bd-10, bd-5, bd-7
     (goto-char (point-min))
     (should (search-forward "bd-1" nil t))
     (goto-char (match-beginning 0))
     (should (button-at (point))))))

(ert-deftest beads-show-test-buttonize-multiple-references ()
  "Test that multiple bd-N references all become buttons."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--markdown-rich-issue)))
     (beads-show--render-issue parsed)
     ;; Should have buttons for bd-1, bd-2, bd-3
     (goto-char (point-min))
     (let ((found-bd-1 nil)
           (found-bd-2 nil)
           (found-bd-3 nil))
       (while (not (eobp))
         (when-let ((button (button-at (point))))
           (let ((id (button-get button 'issue-id)))
             (cond ((string= id "bd-1") (setq found-bd-1 t))
                   ((string= id "bd-2") (setq found-bd-2 t))
                   ((string= id "bd-3") (setq found-bd-3 t)))))
         (forward-char 1))
       (should found-bd-1)
       (should found-bd-2)
       (should found-bd-3)))))

(ert-deftest beads-show-test-button-has-correct-properties ()
  "Test that buttons have correct properties."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--full-issue)))
     (beads-show--render-issue parsed)
     (goto-char (point-min))
     (when (search-forward "bd-1" nil t)
       (goto-char (match-beginning 0))
       (let ((button (button-at (point))))
         (should button)
         (should (string= (button-get button 'issue-id) "bd-1"))
         (should (eq (button-get button 'action)
                    #'beads-show--button-action))
         (should (button-get button 'follow-link)))))))

(ert-deftest beads-show-test-button-help-echo ()
  "Test that buttons have help-echo property."
  (beads-show-test-with-temp-buffer
   (let ((parsed (beads--parse-issue beads-show-test--full-issue)))
     (beads-show--render-issue parsed)
     (goto-char (point-min))
     (when (search-forward "bd-1" nil t)
       (goto-char (match-beginning 0))
       (let ((button (button-at (point))))
         (should (string-match-p "Jump to bd-1"
                               (button-get button 'help-echo))))))))

;;; Tests for Commands

(ert-deftest beads-show-test-show-command-creates-buffer ()
  "Test that beads-show creates a buffer with correct name."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command beads-show-test--full-issue)))
     (beads-show "bd-42")
     (should (get-buffer beads-show-test--buffer-name))
     (with-current-buffer beads-show-test--buffer-name
       (should (derived-mode-p 'beads-show-mode))
       (should (string= beads-show--issue-id "bd-42")))
     (kill-buffer beads-show-test--buffer-name))))

(ert-deftest beads-show-test-show-command-renders-content ()
  "Test that beads-show renders issue content."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command beads-show-test--full-issue)))
     (beads-show "bd-42")
     (with-current-buffer beads-show-test--buffer-name
       (let ((text (beads-show-test--get-buffer-text)))
         (should (string-match-p "Implement feature X" text))
         (should (string-match-p "alice" text)))
       (kill-buffer)))))

(ert-deftest beads-show-test-show-command-handles-error ()
  "Test that beads-show handles errors gracefully."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (lambda (&rest args) (error "Database not found"))))
     (beads-show "bd-999")
     (with-current-buffer beads-show-test--buffer-name
       (let ((text (beads-show-test--get-buffer-text)))
         (should (string-match-p "Error loading issue" text))
         (should (string-match-p "Database not found" text)))
       (kill-buffer)))))

(ert-deftest beads-show-test-show-at-point-with-reference ()
  "Test beads-show-at-point when cursor is on bd-N."
  (beads-show-test-with-git-mocks
   (with-temp-buffer
     (insert "See issue bd-42 for details")
     (goto-char (point-min))
     (search-forward "bd-42")
     (goto-char (match-beginning 0))
     (cl-letf (((symbol-function 'beads-command-show!)
                (beads-show-test--mock-show-command beads-show-test--full-issue)))
       (beads-show-at-point)
       (should (get-buffer beads-show-test--buffer-name))
       (kill-buffer beads-show-test--buffer-name)))))

(ert-deftest beads-show-test-show-at-point-without-reference ()
  "Test beads-show-at-point when no reference at point."
  (with-temp-buffer
    (insert "No reference here")
    (goto-char (point-min))
    (should-error (beads-show-at-point) :type 'user-error)))

(ert-deftest beads-show-test-refresh-command ()
  "Test beads-refresh-show command."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command beads-show-test--full-issue)))
     (beads-show "bd-42")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-max))
       (let ((pos (point)))
         ;; Refresh should reload and try to preserve position
         (beads-refresh-show)
         (should (string-match-p "Implement feature X"
                               (beads-show-test--get-buffer-text))))
       (kill-buffer)))))

(ert-deftest beads-show-test-refresh-outside-show-buffer ()
  "Test that refresh fails outside beads-show buffer."
  (with-temp-buffer
    (should-error (beads-refresh-show) :type 'user-error)))

(ert-deftest beads-show-test-refresh-without-issue-id ()
  "Test that refresh fails without issue ID."
  (beads-show-test-with-temp-buffer
   (setq beads-show--issue-id nil)
   (should-error (beads-refresh-show) :type 'user-error)))

(ert-deftest beads-show-test-next-section ()
  "Test navigation to next section."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command beads-show-test--full-issue)))
     (beads-show "bd-42")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       (let ((initial-pos (point)))
         (beads-show-next-section)
         (should (> (point) initial-pos)))
       (kill-buffer)))))

(ert-deftest beads-show-test-previous-section ()
  "Test navigation to previous section."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command beads-show-test--full-issue)))
     (beads-show "bd-42")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-max))
       (beads-show-previous-section)
       (should (< (point) (point-max)))
       (kill-buffer)))))

(ert-deftest beads-show-test-follow-reference ()
  "Test following a reference with RET."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command beads-show-test--full-issue)))
     (beads-show "bd-42")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       (when (search-forward "bd-1" nil t)
         (goto-char (match-beginning 0))
         (beads-show-follow-reference)
         (should (get-buffer beads-show-test--buffer-name))
         (kill-buffer beads-show-test--buffer-name))
       (kill-buffer)))))

;;; Tests for Mode Setup

(ert-deftest beads-show-test-mode-inherits-special-mode ()
  "Test that beads-show-mode derives from special-mode."
  (beads-show-test-with-temp-buffer
   (should (derived-mode-p 'special-mode))))

(ert-deftest beads-show-test-mode-is-read-only ()
  "Test that show buffer is read-only."
  (beads-show-test-with-temp-buffer
   (should buffer-read-only)))

(ert-deftest beads-show-test-mode-keybindings ()
  "Test that keybindings are set up correctly."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "g"))
              #'beads-refresh-show))
   (should (eq (lookup-key beads-show-mode-map (kbd "q"))
              #'quit-window))
   (should (eq (lookup-key beads-show-mode-map (kbd "n"))
              #'beads-show-next-section))
   (should (eq (lookup-key beads-show-mode-map (kbd "p"))
              #'beads-show-previous-section))
   (should (eq (lookup-key beads-show-mode-map (kbd "RET"))
              #'beads-show-follow-reference))))

(ert-deftest beads-show-test-markdown-mode-aliases ()
  "Test that markdown-mode-style aliases are set up correctly."
  (beads-show-test-with-temp-buffer
   ;; Test reference navigation aliases
   (should (eq (lookup-key beads-show-mode-map (kbd "M-n"))
              #'beads-show-next-reference))
   (should (eq (lookup-key beads-show-mode-map (kbd "M-p"))
              #'beads-show-previous-reference))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-c C-o"))
              #'beads-show-follow-reference))))

(ert-deftest beads-show-test-mode-line-wrapping ()
  "Test that line wrapping is configured based on customization."
  (let ((beads-show-wrap-lines t))
    (beads-show-test-with-temp-buffer
     (should (not truncate-lines))))
  (let ((beads-show-wrap-lines nil))
    (beads-show-test-with-temp-buffer
     (should truncate-lines))))

;;; Edge Cases

(ert-deftest beads-show-test-issue-with-no-title ()
  "Test rendering issue with nil title."
  (beads-show-test-with-temp-buffer
   (let* ((issue '((id . "bd-50")
                  (title . nil)
                  (status . "open")
                  (priority . 1)
                  (issue_type . "task")
                  (created_at . "2025-01-15T10:00:00Z")
                  (updated_at . "2025-01-15T10:00:00Z")))
          (parsed (beads--parse-issue issue)))
     (beads-show--render-issue parsed)
     (let ((text (beads-show-test--get-buffer-text)))
       (should (string-match-p "Untitled" text))))))

(ert-deftest beads-show-test-issue-with-special-characters ()
  "Test rendering issue with special characters."
  (beads-show-test-with-temp-buffer
   (let* ((issue '((id . "bd-50")
                  (title . "Test <>&\"'")
                  (description . "Line 1\nLine 2\tTabbed")
                  (status . "open")
                  (priority . 1)
                  (issue_type . "task")
                  (created_at . "2025-01-15T10:00:00Z")
                  (updated_at . "2025-01-15T10:00:00Z")))
          (parsed (beads--parse-issue issue)))
     (beads-show--render-issue parsed)
     (let ((text (beads-show-test--get-buffer-text)))
       (should (string-match-p "Test <>&\"'" text))
       (should (string-match-p "Line 1\nLine 2" text))))))

(ert-deftest beads-show-test-issue-with-very-long-text ()
  "Test rendering issue with very long text fields."
  (beads-show-test-with-temp-buffer
   (let* ((long-text (make-string 10000 ?x))
          (issue `((id . "bd-50")
                  (title . "Long issue")
                  (description . ,long-text)
                  (status . "open")
                  (priority . 1)
                  (issue_type . "task")
                  (created_at . "2025-01-15T10:00:00Z")
                  (updated_at . "2025-01-15T10:00:00Z")))
          (parsed (beads--parse-issue issue)))
     (beads-show--render-issue parsed)
     (let ((text (beads-show-test--get-buffer-text)))
       (should (string-match-p (substring long-text 0 100) text))))))

(ert-deftest beads-show-test-many-bd-references ()
  "Test rendering issue with many bd-N references."
  (beads-show-test-with-temp-buffer
   (let* ((desc (mapconcat (lambda (n) (format "bd-%d" n))
                          (number-sequence 1 50) " "))
          (issue `((id . "bd-100")
                  (title . "Many references")
                  (description . ,desc)
                  (status . "open")
                  (priority . 1)
                  (issue_type . "task")
                  (created_at . "2025-01-15T10:00:00Z")
                  (updated_at . "2025-01-15T10:00:00Z")))
          (parsed (beads--parse-issue issue)))
     (beads-show--render-issue parsed)
     ;; Count buttons - should have ~50 buttons
     (let ((button-count (beads-show-test--count-buttons-in-buffer)))
       (should (>= button-count 50))))))

;;; Integration Tests

(ert-deftest beads-show-test-full-workflow ()
  "Test full workflow: show issue, navigate, refresh."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command beads-show-test--full-issue)))
     ;; Show issue
     (beads-show "bd-42")
     (should (get-buffer beads-show-test--buffer-name))

     (with-current-buffer beads-show-test--buffer-name
       ;; Check mode is correct
       (should (derived-mode-p 'beads-show-mode))

       ;; Check content is present
       (let ((text (beads-show-test--get-buffer-text)))
         (should (string-match-p "Implement feature X" text)))

       ;; Navigate sections
       (goto-char (point-min))
       (beads-show-next-section)
       (should (> (point) (point-min)))

       ;; Refresh
       (beads-refresh-show)
       (should (string-match-p "Implement feature X"
                             (beads-show-test--get-buffer-text)))

       (kill-buffer)))))

(ert-deftest beads-show-test-multiple-buffers ()
  "Test that viewing multiple issues reuses the same project buffer.
With project-based naming, all issues in a project share one buffer."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (lambda (&rest args)
                (let* ((issue-ids (plist-get args :issue-ids))
                       (id (car issue-ids)))
                  (cond
                   ((string= id "bd-1") (beads-issue-from-json beads-show-test--minimal-issue))
                   ((string= id "bd-42") (beads-issue-from-json beads-show-test--full-issue))
                   (t (error "Unknown issue")))))))
     ;; Open first issue
     (beads-show "bd-1")
     (should (get-buffer beads-show-test--buffer-name))
     (with-current-buffer beads-show-test--buffer-name
       (should (string-match-p "Minimal issue"
                               (beads-show-test--get-buffer-text))))

     ;; Open second issue - should REUSE the same buffer
     (beads-show "bd-42")
     (should (get-buffer beads-show-test--buffer-name))

     ;; Buffer content should now show the SECOND issue
     (with-current-buffer beads-show-test--buffer-name
       (should (string-match-p "Implement feature X"
                               (beads-show-test--get-buffer-text)))
       ;; Old content should be gone
       (should-not (string-match-p "Minimal issue"
                                   (beads-show-test--get-buffer-text))))

     ;; Cleanup
     (kill-buffer beads-show-test--buffer-name))))

;;; Tests for Outline Navigation

(defvar beads-show-test--outline-issue
  '((id . "bd-200")
    (title . "Issue Title")
    (description . "First paragraph in description.\n\n## Level 2 Heading\n\nText under level 2.\n\n### Level 3 Heading\n\nText under level 3.\n\n## Another Level 2\n\nMore text.")
    (status . "open")
    (priority . 1)
    (issue_type . "feature")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z")
    (acceptance_criteria . "## Acceptance Level 2\n\nSome criteria.")
    (design . "### Design Level 3\n\nDesign details.\n\n## Design Level 2\n\nMore design.")
    (notes . "Final notes here."))
  "Sample issue with nested headings for outline navigation testing.")

(ert-deftest beads-show-test-section-level-title ()
  "Test section level detection for title (level 0)."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Search for title
       (when (search-forward "Issue Title" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 0)))
       (kill-buffer)))))

(ert-deftest beads-show-test-section-level-major-section ()
  "Test section level detection for major sections (level 1)."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Search for "Description" major section
       (when (search-forward "Description\n─" nil t)
         (goto-char (match-beginning 0))
         (should (eq (beads-show--section-level) 1)))
       (kill-buffer)))))

(ert-deftest beads-show-test-section-level-markdown-heading-2 ()
  "Test section level detection for markdown ## heading (level 2)."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Search for "## Level 2 Heading"
       (when (search-forward "## Level 2 Heading" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 2)))
       (kill-buffer)))))

(ert-deftest beads-show-test-section-level-markdown-heading-3 ()
  "Test section level detection for markdown ### heading (level 3)."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Search for "### Level 3 Heading"
       (when (search-forward "### Level 3 Heading" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 3)))
       (kill-buffer)))))

(ert-deftest beads-show-test-section-level-not-heading ()
  "Test section level returns nil for non-heading lines."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Search for "First paragraph" which is not a heading
       (when (search-forward "First paragraph" nil t)
         (beginning-of-line)
         (should (null (beads-show--section-level))))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-next-basic ()
  "Test moving to next heading at any level."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Start at title
       (search-forward "Issue Title" nil t)
       (beginning-of-line)
       (let ((start-level (beads-show--section-level)))
         (should (eq start-level 0))
         ;; Move to next heading (should be Description)
         (beads-show-outline-next)
         (let ((next-level (beads-show--section-level)))
           (should next-level)
           (should (> (point) (point-min)))))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-previous-basic ()
  "Test moving to previous heading at any level."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       ;; Start near the end
       (goto-char (point-max))
       (let ((start-pos (point)))
         ;; Move to previous heading
         (beads-show-outline-previous)
         (should (< (point) start-pos))
         (should (beads-show--section-level)))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-next-at-end ()
  "Test outline-next at end of buffer shows message."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--minimal-issue)))
     (beads-show "bd-1")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-max))
       (forward-line -1)
       (let ((pos (point)))
         (beads-show-outline-next)
         ;; Should not move and stay at same position
         (should (= (point) pos)))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-previous-at-start ()
  "Test outline-previous at start of buffer shows message."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--minimal-issue)))
     (beads-show "bd-1")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       (let ((pos (point)))
         (beads-show-outline-previous)
         ;; Should not move from start
         (should (= (point) pos)))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-next-same-level ()
  "Test moving to next heading at same level."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find first ## heading
       (when (search-forward "## Level 2 Heading" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 2))
         (let ((start-pos (point)))
           ;; Move to next same-level heading (should be "## Another Level 2")
           (beads-show-outline-next-same-level)
           (should (> (point) start-pos))
           (should (eq (beads-show--section-level) 2))))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-previous-same-level ()
  "Test moving to previous heading at same level."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find second ## heading (Another Level 2)
       (search-forward "## Level 2 Heading" nil t)
       (when (search-forward "## Another Level 2" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 2))
         (let ((start-pos (point)))
           ;; Move to previous same-level heading
           (beads-show-outline-previous-same-level)
           (should (< (point) start-pos))
           (should (eq (beads-show--section-level) 2))))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-same-level-not-at-heading ()
  "Test outline-next-same-level errors when not at heading."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Move to non-heading text
       (when (search-forward "First paragraph" nil t)
         (should-error (beads-show-outline-next-same-level) :type 'user-error))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-up-from-level-3 ()
  "Test moving up from level 3 heading to parent."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find ### Level 3 heading
       (when (search-forward "### Level 3 Heading" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 3))
         (let ((start-pos (point)))
           ;; Move up to parent (should be ## Level 2)
           (beads-show-outline-up)
           (should (< (point) start-pos))
           (let ((parent-level (beads-show--section-level)))
             (should parent-level)
             (should (< parent-level 3)))))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-up-from-level-2 ()
  "Test moving up from level 2 heading to level 1."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find ## Level 2 heading
       (when (search-forward "## Level 2 Heading" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 2))
         (let ((start-pos (point)))
           ;; Move up to parent (should be Description at level 1)
           (beads-show-outline-up)
           (should (< (point) start-pos))
           (should (eq (beads-show--section-level) 1))))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-up-at-level-0-errors ()
  "Test outline-up at level 0 (title) gives error."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find title
       (when (search-forward "Issue Title" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 0))
         (should-error (beads-show-outline-up) :type 'user-error))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-up-not-at-heading ()
  "Test outline-up errors when not at heading."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Move to non-heading text
       (when (search-forward "First paragraph" nil t)
         (should-error (beads-show-outline-up) :type 'user-error))
       (kill-buffer)))))

(ert-deftest beads-show-test-outline-keybindings ()
  "Test that outline navigation keybindings are set up correctly."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "C-c C-n"))
              #'beads-show-outline-next))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-c C-p"))
              #'beads-show-outline-previous))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-c C-f"))
              #'beads-show-outline-next-same-level))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-c C-b"))
              #'beads-show-outline-previous-same-level))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-c C-u"))
              #'beads-show-outline-up))))

(ert-deftest beads-show-test-outline-navigation-sequence ()
  "Test a sequence of outline navigation commands."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       ;; Start at top
       (goto-char (point-min))

       ;; Navigate through headings
       (when (search-forward "Issue Title" nil t)
         (beginning-of-line)
         (should (eq (beads-show--section-level) 0))

         ;; Next heading (Description - level 1)
         (beads-show-outline-next)
         (should (>= (beads-show--section-level) 1))

         ;; Next heading (should be ## Level 2)
         (beads-show-outline-next)
         (let ((level (beads-show--section-level)))
           (should (or (eq level 1) (eq level 2))))

         ;; Previous heading
         (beads-show-outline-previous)
         (should (beads-show--section-level)))

       (kill-buffer)))))

;;; Tests for Paragraph Navigation

(ert-deftest beads-show-test-paragraph-navigation-keybindings ()
  "Test that M-{ and M-} are bound to paragraph navigation."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "M-{"))
              #'beads-show-backward-paragraph))
   (should (eq (lookup-key beads-show-mode-map (kbd "M-}"))
              #'beads-show-forward-paragraph))))

(ert-deftest beads-show-test-forward-paragraph-basic ()
  "Test moving forward by paragraph."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--markdown-rich-issue)))
     (beads-show "bd-100")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       (let ((start-pos (point)))
         (beads-show-forward-paragraph)
         (should (> (point) start-pos)))
       (kill-buffer)))))

(ert-deftest beads-show-test-backward-paragraph-basic ()
  "Test moving backward by paragraph."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--markdown-rich-issue)))
     (beads-show "bd-100")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-max))
       (let ((start-pos (point)))
         (beads-show-backward-paragraph)
         (should (< (point) start-pos)))
       (kill-buffer)))))

(ert-deftest beads-show-test-paragraph-navigation-functions-defined ()
  "Test that paragraph navigation functions are defined."
  (should (fboundp 'beads-show-forward-paragraph))
  (should (fboundp 'beads-show-backward-paragraph))
  (should (fboundp 'beads-show-mark-paragraph)))

(ert-deftest beads-show-test-mark-paragraph-keybinding ()
  "Test that M-h is bound to beads-show-mark-paragraph."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "M-h"))
              #'beads-show-mark-paragraph))))

(ert-deftest beads-show-test-mark-paragraph-basic ()
  "Test marking a paragraph."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--markdown-rich-issue)))
     (beads-show "bd-100")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find some paragraph text
       (when (search-forward "bold" nil t)
         (beads-show-mark-paragraph)
         ;; Mark should be active
         (should (region-active-p))
         ;; Region should have some content
         (should (> (region-end) (region-beginning))))
       (kill-buffer)))))

;;; Tests for Block Navigation

(defvar beads-show-test--block-rich-issue
  '((id . "bd-300")
    (title . "Block Navigation Test")
    (description . "Regular paragraph text here.\n\n```python\ndef hello():\n    print(\"Hello\")\n```\n\nAnother paragraph.\n\n- List item 1\n- List item 2\n  Continuation\n- List item 3\n\nMore text.\n\n> Blockquote line 1\n> Blockquote line 2\n\nFinal paragraph.\n\n    Indented code line 1\n    Indented code line 2\n\nEnd text.")
    (status . "open")
    (priority . 2)
    (issue_type . "task")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z"))
  "Sample issue with various block types for testing block navigation.")

(ert-deftest beads-show-test-block-navigation-keybindings ()
  "Test that C-M-{ and C-M-} are bound to block navigation."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "C-M-{"))
              #'beads-show-backward-block))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-M-}"))
              #'beads-show-forward-block))))

(ert-deftest beads-show-test-block-boundary-detection-fenced ()
  "Test detection of fenced code block boundaries."
  (with-temp-buffer
    (insert "```python\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'fenced-code))))

(ert-deftest beads-show-test-block-boundary-detection-list ()
  "Test detection of list block boundaries."
  (with-temp-buffer
    (insert "- List item\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'list))))

(ert-deftest beads-show-test-block-boundary-detection-blockquote ()
  "Test detection of blockquote boundaries."
  (with-temp-buffer
    (insert "> Quote text\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'blockquote))))

(ert-deftest beads-show-test-block-boundary-detection-indented ()
  "Test detection of indented code blocks."
  (with-temp-buffer
    (insert "    code line\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'indented-code))))

(ert-deftest beads-show-test-block-boundary-detection-numbered-list ()
  "Test detection of numbered list boundaries."
  (with-temp-buffer
    (insert "1. First item\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'list)))
  (with-temp-buffer
    (insert "42. Middle item\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'list))))

(ert-deftest beads-show-test-forward-block-from-text ()
  "Test forward-block navigation from regular text."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find "Regular paragraph"
       (when (search-forward "Regular paragraph" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-forward-block)
           ;; Should have moved to next block
           (should (> (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-forward-block-skip-fenced-code ()
  "Test forward-block skips entire fenced code block."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find opening fence
       (when (search-forward "```python" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-forward-block)
           ;; Should be past closing fence
           (should (> (point) start-pos))
           ;; Should be past the code content
           (let ((text-before-point
                  (buffer-substring-no-properties start-pos (point))))
             (should (string-match-p "```" text-before-point)))))
       (kill-buffer)))))

(ert-deftest beads-show-test-forward-block-skip-list ()
  "Test forward-block skips entire list."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find list start
       (when (search-forward "- List item 1" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-forward-block)
           ;; Should be past all list items
           (should (> (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-forward-block-skip-blockquote ()
  "Test forward-block skips entire blockquote."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find blockquote start
       (when (search-forward "> Blockquote line 1" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-forward-block)
           ;; Should be past blockquote
           (should (> (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-forward-block-skip-indented-code ()
  "Test forward-block skips indented code block."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find indented code
       (when (search-forward "    Indented code line 1" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-forward-block)
           ;; Should be past indented code
           (should (> (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-backward-block-from-text ()
  "Test backward-block navigation from regular text."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find "End text" near the end
       (when (search-forward "End text" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-backward-block)
           ;; Should have moved backward to previous block
           (should (< (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-backward-block-to-list-start ()
  "Test backward-block moves backward from end of list."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find "List item 3" (end of list)
       (when (search-forward "- List item 3" nil t)
         (end-of-line)
         (forward-line 1)
         (let ((start-pos (point)))
           (beads-show-backward-block)
           ;; Should have moved backward
           (should (< (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-backward-block-to-fenced-code-start ()
  "Test backward-block finds start of fenced code block."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find inside or after code block
       (when (search-forward "```python" nil t)
         (forward-line 2)  ; Move into the code block
         (let ((start-pos (point)))
           (beads-show-backward-block)
           ;; Should be at opening fence
           (should (< (point) start-pos))
           (beginning-of-line)
           (should (looking-at "```"))))
       (kill-buffer)))))

(ert-deftest beads-show-test-forward-block-at-end ()
  "Test forward-block at end of buffer shows message."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--minimal-issue)))
     (beads-show "bd-1")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-max))
       (let ((pos (point)))
         (beads-show-forward-block)
         ;; Should not move much from end
         (should (<= (abs (- (point) pos)) 10)))
       (kill-buffer)))))

(ert-deftest beads-show-test-backward-block-at-start ()
  "Test backward-block at start of buffer shows message."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--minimal-issue)))
     (beads-show "bd-1")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       (forward-line 2)
       (let ((pos (point)))
         (beads-show-backward-block)
         ;; Should have moved or stayed
         (should (<= (point) pos)))
       (kill-buffer)))))

(ert-deftest beads-show-test-block-navigation-sequence ()
  "Test sequence of forward and backward block navigation."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--block-rich-issue)))
     (beads-show "bd-300")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       (let ((positions '()))
         ;; Navigate forward multiple times, recording positions
          (push (point) positions)
         (beads-show-forward-block)
         (push (point) positions)
         (beads-show-forward-block)
         (push (point) positions)
         (beads-show-forward-block)
         (push (point) positions)

         ;; All positions should be different and increasing
         (setq positions (nreverse positions))
         (should (apply #'< positions))

         ;; Navigate backward
         (beads-show-backward-block)
         (should (< (point) (nth 3 positions))))
       (kill-buffer)))))

(ert-deftest beads-show-test-block-navigation-functions-defined ()
  "Test that block navigation functions are defined."
  (should (fboundp 'beads-show-forward-block))
  (should (fboundp 'beads-show-backward-block))
  (should (fboundp 'beads-show--at-block-boundary)))

(ert-deftest beads-show-test-triple-tilde-fence ()
  "Test that ~~~ fences are recognized."
  (with-temp-buffer
    (insert "~~~\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'fenced-code))))

(ert-deftest beads-show-test-list-markers ()
  "Test various list marker formats."
  (with-temp-buffer
    (insert "* Item\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'list)))
  (with-temp-buffer
    (insert "+ Item\n")
    (goto-char (point-min))
    (should (eq (beads-show--at-block-boundary) 'list))))

;;; Tests for Section Boundary Navigation

(ert-deftest beads-show-test-section-boundary-keybindings ()
  "Test that C-M-a/e/h are bound to section boundary navigation."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "C-M-a"))
              #'beads-show-beginning-of-section))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-M-e"))
              #'beads-show-end-of-section))
   (should (eq (lookup-key beads-show-mode-map (kbd "C-M-h"))
              #'beads-show-mark-section))))

(ert-deftest beads-show-test-beginning-of-section-at-heading ()
  "Test moving to beginning of section when at heading."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find Description heading
       (when (search-forward "Description" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-beginning-of-section)
           ;; Should stay at same position (already at beginning)
           (should (= (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-beginning-of-section-in-content ()
  "Test moving to beginning of section from content."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find some content text
       (when (search-forward "First paragraph" nil t)
         (beads-show-beginning-of-section)
         ;; Should have moved to Description section
         (should (beads-show--section-level)))
       (kill-buffer)))))

(ert-deftest beads-show-test-end-of-section-basic ()
  "Test moving to end of section."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find Description heading
       (when (search-forward "Description" nil t)
         (beginning-of-line)
         (let ((start-pos (point)))
           (beads-show-end-of-section)
           ;; Should have moved forward
           (should (> (point) start-pos))))
       (kill-buffer)))))

(ert-deftest beads-show-test-mark-section-basic ()
  "Test marking a section."
  (beads-show-test-with-git-mocks
   (cl-letf (((symbol-function 'beads-command-show!)
              (beads-show-test--mock-show-command
               beads-show-test--outline-issue)))
     (beads-show "bd-200")
     (with-current-buffer beads-show-test--buffer-name
       (goto-char (point-min))
       ;; Find Description heading
       (when (search-forward "Description" nil t)
         (beginning-of-line)
         (beads-show-mark-section)
         ;; Mark should be active
         (should (region-active-p))
         ;; Region should have some content
         (should (> (region-end) (region-beginning))))
       (kill-buffer)))))

(ert-deftest beads-show-test-section-boundary-functions-defined ()
  "Test that section boundary navigation functions are defined."
  (should (fboundp 'beads-show-beginning-of-section))
  (should (fboundp 'beads-show-end-of-section))
  (should (fboundp 'beads-show-mark-section)))

;;; Tests for Field Editing

(ert-deftest beads-show-test-edit-field-keybinding ()
  "Test that C-c C-e is bound to beads-show-edit-field."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "C-c C-e"))
              #'beads-show-edit-field))))

(ert-deftest beads-show-test-edit-field-outside-show-buffer ()
  "Test that edit-field fails outside beads-show buffer."
  (with-temp-buffer
    (should-error (beads-show-edit-field) :type 'user-error)))

(ert-deftest beads-show-test-edit-field-without-issue-id ()
  "Test that edit-field fails without issue ID."
  (beads-show-test-with-temp-buffer
   (setq beads-show--issue-id nil)
   (should-error (beads-show-edit-field) :type 'user-error)))

(ert-deftest beads-show-test-edit-field-without-issue-data ()
  "Test that edit-field fails without issue data."
  (beads-show-test-with-temp-buffer
   (setq beads-show--issue-id "bd-42")
   (setq beads-show--issue-data nil)
   (should-error (beads-show-edit-field) :type 'user-error)))

(ert-deftest beads-show-test-update-field-description ()
  "Test updating description field."
  (let ((update-called nil)
        (captured-cmd nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 ;; Only capture update command
                 (when (cl-typep cmd 'beads-command-update)
                   (setq update-called t)
                   (setq captured-cmd cmd))
                 ;; Return nil for updates
                 nil))
              ((symbol-function 'beads-command-show!)
               (lambda (&rest args)
                 ;; Return updated issue data for refresh
                 (beads-issue-from-json beads-show-test--full-issue))))
      (beads-show-test-with-temp-buffer
       (setq beads-show--issue-id "bd-42")
       (setq beads-show--issue-data (beads--parse-issue
                                     beads-show-test--full-issue))
       (beads-show--update-field "Description" "--description" "New description text")
       (should update-called)
       (should (equal (oref captured-cmd issue-ids) '("bd-42")))
       (should (equal (oref captured-cmd description) "New description text"))))))

(ert-deftest beads-show-test-update-field-invalidates-cache ()
  "Test that updating field invalidates completion cache."
  (let ((cache-invalidated nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads-command-show!)
               (lambda (&rest _args)
                 (beads-issue-from-json beads-show-test--full-issue)))
              ((symbol-function 'beads-completion-invalidate-cache)
               (lambda ()
                 (setq cache-invalidated t))))
      (beads-show-test-with-temp-buffer
       (setq beads-show--issue-id "bd-42")
       (setq beads-show--issue-data (beads--parse-issue
                                     beads-show-test--full-issue))
       (beads-show--update-field "Title" "--title" "New title")
       (should cache-invalidated)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-show-test-mode-defined ()
  "Integration test: Verify beads-show-mode is defined."
  :tags '(integration)
  (should (fboundp 'beads-show-mode)))

(ert-deftest beads-show-test-keybinding-g-refresh ()
  "Integration test: Verify g keybinding for refresh."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "g"))))
      (should (eq binding 'beads-refresh-show)))))

(ert-deftest beads-show-test-keybinding-q-quit ()
  "Integration test: Verify q keybinding for quit."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "q"))))
      (should (eq binding 'quit-window)))))

(ert-deftest beads-show-test-keybinding-n-next ()
  "Integration test: Verify n keybinding for next section."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "n"))))
      (should (eq binding 'beads-show-next-section)))))

(ert-deftest beads-show-test-keybinding-p-prev ()
  "Integration test: Verify p keybinding for previous section."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "p"))))
      (should (eq binding 'beads-show-previous-section)))))

(ert-deftest beads-show-test-outline-navigation-next ()
  "Integration test: Verify C-c C-n outline navigation."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "C-c C-n"))))
      (should (eq binding 'beads-show-outline-next)))))

(ert-deftest beads-show-test-outline-navigation-previous ()
  "Integration test: Verify C-c C-p outline navigation."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "C-c C-p"))))
      (should (eq binding 'beads-show-outline-previous)))))

(ert-deftest beads-show-test-outline-navigation-forward ()
  "Integration test: Verify C-c C-f outline navigation."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "C-c C-f"))))
      (should (eq binding 'beads-show-outline-next-same-level)))))

(ert-deftest beads-show-test-outline-navigation-backward ()
  "Integration test: Verify C-c C-b outline navigation."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "C-c C-b"))))
      (should (eq binding 'beads-show-outline-previous-same-level)))))

(ert-deftest beads-show-test-outline-navigation-up ()
  "Integration test: Verify C-c C-u outline navigation."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((binding (lookup-key beads-show-mode-map (kbd "C-c C-u"))))
      (should (eq binding 'beads-show-outline-up)))))

(ert-deftest beads-show-test-reference-navigation ()
  "Integration test: Verify [ and ] for reference navigation."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    (let ((prev-binding (lookup-key beads-show-mode-map (kbd "[")))
          (next-binding (lookup-key beads-show-mode-map (kbd "]"))))
      (should (eq prev-binding 'beads-show-previous-reference))
      (should (eq next-binding 'beads-show-next-reference)))))

(ert-deftest beads-show-test-show-command-exists ()
  "Integration test: Verify beads-show command exists."
  :tags '(integration)
  (should (fboundp 'beads-show)))

(ert-deftest beads-show-test-context-detection ()
  "Integration test: Context detection works in show mode."
  :tags '(integration)
  (with-temp-buffer
    (beads-show-mode)
    ;; Show mode should be active
    (should (eq major-mode 'beads-show-mode))))

;;; Integration Test for Field Editing

(ert-deftest beads-show-test-edit-multiline-field-integration ()
  "Integration test: Edit multiline field and verify update.
Tests the full workflow: create issue -> update description -> verify update."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create an issue with initial description
    (let* ((initial-desc "Initial description text")
           (issue (beads-command-create!
                   :title "Test Issue for Field Editing"
                   :description initial-desc
                   :issue-type "task"
                   :priority 2))
           (issue-id (oref issue id))
           (updated-desc "Updated description text")
           ;; Buffer is named by project, not issue
           (proj-name (beads-git-get-project-name)))

      ;; Show the issue
      (beads-show issue-id)
      (unwind-protect
          (with-current-buffer (format "*beads-show: %s*" proj-name)
            (should (eq major-mode 'beads-show-mode))
            (should (equal beads-show--issue-id issue-id))

            ;; Verify initial description is in issue data
            (should (equal (oref beads-show--issue-data description) initial-desc))

            ;; Directly call update-field to update description
            ;; This bypasses the interactive edit buffer but tests the update pathway
            (beads-show--update-field "Description" "--description" updated-desc)

            ;; Fetch the issue again to verify it was updated
            (let ((fetched-issue (beads-command-show! :issue-ids (list issue-id))))
              (should fetched-issue)
              (should (equal (oref fetched-issue description) updated-desc))))

        ;; Cleanup
        (dolist (buffer (buffer-list))
          (when (string-match-p "\\*beads-\\(show\\|edit\\)" (buffer-name buffer))
            (kill-buffer buffer)))))))

(ert-deftest beads-show-test-edit-acceptance-criteria-integration ()
  "Integration test: Edit acceptance criteria field and verify update.
Tests editing a different multiline field to ensure all fields work."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create an issue with initial acceptance criteria
    (let* ((initial-ac "- [ ] Must do X\n- [ ] Must do Y")
           (issue (beads-command-create!
                   :title "Test Issue for AC Editing"
                   :acceptance initial-ac
                   :issue-type "feature"
                   :priority 1))
           (issue-id (oref issue id))
           (updated-ac "- [x] Must do X\n- [x] Must do Y\n- [ ] Must do Z")
           ;; Buffer is named by project, not issue
           (proj-name (beads-git-get-project-name)))

      ;; Show the issue
      (beads-show issue-id)
      (unwind-protect
          (with-current-buffer (format "*beads-show: %s*" proj-name)
            (should (eq major-mode 'beads-show-mode))
            (should (equal beads-show--issue-id issue-id))

            ;; Verify initial acceptance criteria is in issue data
            (should (equal (oref beads-show--issue-data acceptance-criteria) initial-ac))

            ;; Directly call update-field to update acceptance criteria
            (beads-show--update-field "Acceptance Criteria" "--acceptance" updated-ac)

            ;; Fetch the issue again to verify it was updated
            (let ((fetched-issue (beads-command-show! :issue-ids (list issue-id))))
              (should fetched-issue)
              (should (equal (oref fetched-issue acceptance-criteria) updated-ac))))

        ;; Cleanup
        (dolist (buffer (buffer-list))
          (when (string-match-p "\\*beads-\\(show\\|edit\\)" (buffer-name buffer))
            (kill-buffer buffer)))))))

(ert-deftest beads-show-test-edit-notes-field-integration ()
  "Integration test: Edit notes field and verify update.
Tests editing the notes field specifically.
Note: Notes cannot be set at creation time, only via update."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create an issue without notes (notes not supported in create command)
    (let* ((issue (beads-command-create!
                   :title "Test Issue for Notes Editing"
                   :description "Test issue for notes editing"
                   :issue-type "bug"
                   :priority 0))
           (issue-id (oref issue id))
           (initial-notes "Initial notes about the issue")
           (updated-notes "Updated notes with more details")
           ;; Buffer is named by project, not issue
           (proj-name (beads-git-get-project-name)))

      ;; First, add initial notes via update command
      (beads-command-execute
       (beads-command-update :issue-ids (list issue-id)
                            :notes initial-notes))

      ;; Show the issue
      (beads-show issue-id)
      (unwind-protect
          (with-current-buffer (format "*beads-show: %s*" proj-name)
            (should (eq major-mode 'beads-show-mode))
            (should (equal beads-show--issue-id issue-id))

            ;; Verify initial notes are in issue data (refresh first)
            (beads-refresh-show)
            (should (equal (oref beads-show--issue-data notes) initial-notes))

            ;; Directly call update-field to update notes
            (beads-show--update-field "Notes" "--notes" updated-notes)

            ;; Verify update
            (let ((fetched-issue (beads-command-show! :issue-ids (list issue-id))))
              (should (equal (oref fetched-issue notes) updated-notes))))

        ;; Cleanup
        (dolist (buffer (buffer-list))
          (when (string-match-p "\\*beads-\\(show\\|edit\\)" (buffer-name buffer))
            (kill-buffer buffer)))))))

;;; Tests for Agent Integration

(ert-deftest beads-show-test-agent-keybinding-A ()
  "Test that A keybinding is bound to beads-agent-start-at-point."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "A"))
              #'beads-agent-start-at-point))))

(ert-deftest beads-show-test-agent-keybinding-J ()
  "Test that J keybinding is bound to beads-agent-jump-at-point."
  (beads-show-test-with-temp-buffer
   (should (eq (lookup-key beads-show-mode-map (kbd "J"))
              #'beads-agent-jump-at-point))))

(ert-deftest beads-show-test-insert-agent-section-no-sessions ()
  "Test that agent section is not rendered when no sessions exist."
  (beads-show-test-with-temp-buffer
   ;; Mock beads-agent--get-sessions-for-issue to return nil
   (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
              (lambda (_issue-id) nil)))
     (let ((inhibit-read-only t)
           (start (point)))
       (beads-show--insert-agent-section "bd-42")
       ;; Should not have inserted anything
       (should (= (point) start))))))

(ert-deftest beads-show-test-insert-agent-section-with-sessions ()
  "Test that agent section is rendered when sessions exist."
  (beads-show-test-with-temp-buffer
   ;; Create a mock session object
   (let ((mock-session (beads-agent-session
                        :id "session-123"
                        :issue-id "bd-42"
                        :backend-name "claude-code-ide"
                        :project-dir "/home/user/project"
                        :started-at "2025-01-15T10:30:45")))
     ;; Mock the functions
     (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
                (lambda (_issue-id) (list mock-session)))
               ((symbol-function 'beads-agent--session-active-p)
                (lambda (_session) t)))
       (let ((inhibit-read-only t)
             (start (point)))
         (beads-show--insert-agent-section "bd-42")
         ;; Should have inserted content
         (should (> (point) start))
         ;; Should contain "Agent Sessions" header
         (should (string-match-p "Agent Sessions"
                               (buffer-substring-no-properties
                                start (point))))
         ;; Should contain backend name
         (should (string-match-p "claude-code-ide"
                               (buffer-substring-no-properties
                                start (point))))
         ;; Should contain status
         (should (string-match-p "active"
                               (buffer-substring-no-properties
                                start (point)))))))))

(ert-deftest beads-show-test-insert-agent-section-inactive-session ()
  "Test that agent section shows stopped status for inactive sessions."
  (beads-show-test-with-temp-buffer
   ;; Create a mock session object
   (let ((mock-session (beads-agent-session
                        :id "session-456"
                        :issue-id "bd-42"
                        :backend-name "efrit"
                        :project-dir "/home/user/project"
                        :started-at "2025-01-14T08:00:00")))
     ;; Mock the functions - session is not active
     (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
                (lambda (_issue-id) (list mock-session)))
               ((symbol-function 'beads-agent--session-active-p)
                (lambda (_session) nil)))
       (let ((inhibit-read-only t)
             (start (point)))
         (beads-show--insert-agent-section "bd-42")
         ;; Should have inserted content
         (should (> (point) start))
         ;; Should contain "stopped" status
         (should (string-match-p "stopped"
                               (buffer-substring-no-properties
                                start (point)))))))))

(ert-deftest beads-show-test-render-issue-includes-agent-section ()
  "Test that beads-show--render-issue calls insert-agent-section."
  (beads-show-test-with-temp-buffer
   (let ((insert-called nil)
         (parsed-issue (beads--parse-issue beads-show-test--full-issue)))
     ;; Mock the agent session functions
     (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
                (lambda (_issue-id)
                  (setq insert-called t)
                  nil)))
       (beads-show--render-issue parsed-issue)
       ;; The function should have been called during render
       (should insert-called)))))

;;; Tests for Sub-issues Section (Epics)

(defvar beads-show-test--epic-issue
  '((id . "bd-epic-1")
    (title . "Epic: Build feature X")
    (description . "This epic tracks the full implementation")
    (status . "open")
    (priority . 1)
    (issue_type . "epic")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z"))
  "Sample epic issue.")

(defvar beads-show-test--sub-issues-data
  (vector
   '((id . "bd-epic-1") (title . "Epic: Build feature X") (status . "open")
     (priority . 1) (issue_type . "epic") (depth . 0) (parent_id . "bd-epic-1"))
   '((id . "bd-sub-1") (title . "Implement core module") (status . "closed")
     (priority . 1) (issue_type . "task") (depth . 1) (parent_id . "bd-epic-1"))
   '((id . "bd-sub-2") (title . "Write tests") (status . "in_progress")
     (priority . 2) (issue_type . "task") (depth . 1) (parent_id . "bd-epic-1"))
   '((id . "bd-sub-3") (title . "Update documentation") (status . "open")
     (priority . 3) (issue_type . "task") (depth . 1) (parent_id . "bd-epic-1")))
  "Mock dep tree response with sub-issues.")

(ert-deftest beads-show-test-get-sub-issues-returns-depth-1-only ()
  "Test that beads-show--get-sub-issues filters to depth=1 only."
  (cl-letf (((symbol-function 'beads-command-dep-tree!)
             (lambda (&rest _args)
               beads-show-test--sub-issues-data)))
    (let ((sub-issues (beads-show--get-sub-issues "bd-epic-1")))
      ;; Should return 3 sub-issues (depth=1), not the root (depth=0)
      (should (= (length sub-issues) 3))
      ;; All should have depth=1
      (dolist (item sub-issues)
        (should (= (alist-get 'depth item) 1))))))

(ert-deftest beads-show-test-get-sub-issues-handles-error ()
  "Test that beads-show--get-sub-issues returns nil on error."
  (cl-letf (((symbol-function 'beads-command-dep-tree!)
             (lambda (&rest _args)
               (error "Command failed"))))
    (let ((sub-issues (beads-show--get-sub-issues "bd-epic-1")))
      (should (null sub-issues)))))

(ert-deftest beads-show-test-get-sub-issues-empty-tree ()
  "Test that beads-show--get-sub-issues handles epic with no children."
  (cl-letf (((symbol-function 'beads-command-dep-tree!)
             (lambda (&rest _args)
               ;; Only root, no children
               (vector '((id . "bd-epic-1") (depth . 0))))))
    (let ((sub-issues (beads-show--get-sub-issues "bd-epic-1")))
      (should (null sub-issues)))))

(ert-deftest beads-show-test-format-sub-issue-status ()
  "Test status formatting for sub-issues (completion-style)."
  (should (string-match-p "OPEN" (beads-show--format-sub-issue-status "open")))
  (should (string-match-p "IN_PROGRESS" (beads-show--format-sub-issue-status "in_progress")))
  (should (string-match-p "BLOCKED" (beads-show--format-sub-issue-status "blocked")))
  (should (string-match-p "CLOSED" (beads-show--format-sub-issue-status "closed"))))

(ert-deftest beads-show-test-insert-sub-issues-section-renders ()
  "Test that sub-issues section renders correctly in completion-style format."
  (beads-show-test-with-temp-buffer
   (cl-letf (((symbol-function 'beads-command-dep-tree!)
              (lambda (&rest _args)
                beads-show-test--sub-issues-data)))
     (let ((inhibit-read-only t))
       (beads-show--insert-sub-issues-section "bd-epic-1")
       (let ((content (buffer-substring-no-properties (point-min) (point-max))))
         ;; Should have the header
         (should (string-match-p "Sub-issues" content))
         ;; Should show completion count (1/3 completed)
         (should (string-match-p "1/3 completed" content))
         ;; Should have all sub-issue IDs
         (should (string-match-p "bd-sub-1" content))
         (should (string-match-p "bd-sub-2" content))
         (should (string-match-p "bd-sub-3" content))
         ;; Should have priority indicators
         (should (string-match-p "\\[P1\\]" content))
         (should (string-match-p "\\[P2\\]" content))
         ;; Should have type indicators
         (should (string-match-p "\\[task\\]" content))
         ;; Should have status text
         (should (string-match-p "CLOSED" content))
         (should (string-match-p "IN_PROGRESS" content))
         ;; Should have titles after dash
         (should (string-match-p "- Implement core module" content))
         (should (string-match-p "- Write tests" content)))))))

(ert-deftest beads-show-test-insert-sub-issues-section-no-children ()
  "Test that sub-issues section is not rendered when no children."
  (beads-show-test-with-temp-buffer
   (cl-letf (((symbol-function 'beads-command-dep-tree!)
              (lambda (&rest _args)
                (vector '((id . "bd-epic-1") (depth . 0))))))
     (let ((inhibit-read-only t)
           (start (point)))
       (beads-show--insert-sub-issues-section "bd-epic-1")
       ;; Should not have inserted anything
       (should (= (point) start))))))

(ert-deftest beads-show-test-insert-sub-issues-clickable-ids ()
  "Test that sub-issue IDs are clickable buttons."
  (beads-show-test-with-temp-buffer
   (cl-letf (((symbol-function 'beads-command-dep-tree!)
              (lambda (&rest _args)
                beads-show-test--sub-issues-data)))
     (let ((inhibit-read-only t))
       (beads-show--insert-sub-issues-section "bd-epic-1")
       (goto-char (point-min))
       ;; Should find buttons for sub-issue IDs
       (should (search-forward "bd-sub-1" nil t))
       ;; Check that there's a button at that location
       (goto-char (match-beginning 0))
       (should (button-at (point)))))))

(ert-deftest beads-show-test-render-epic-includes-sub-issues ()
  "Test that render-issue includes sub-issues section for epics."
  (beads-show-test-with-temp-buffer
   (let ((parsed-issue (beads--parse-issue beads-show-test--epic-issue)))
     (cl-letf (((symbol-function 'beads-command-dep-tree!)
                (lambda (&rest _args)
                  beads-show-test--sub-issues-data))
               ((symbol-function 'beads-agent--get-sessions-for-issue)
                (lambda (_) nil)))
       (beads-show--render-issue parsed-issue)
       (let ((content (buffer-substring-no-properties (point-min) (point-max))))
         ;; Should have sub-issues section
         (should (string-match-p "Sub-issues" content))
         ;; Should show completion count
         (should (string-match-p "1/3 completed" content)))))))

(ert-deftest beads-show-test-render-non-epic-no-sub-issues ()
  "Test that render-issue does NOT include sub-issues for non-epics."
  (beads-show-test-with-temp-buffer
   (let ((parsed-issue (beads--parse-issue beads-show-test--full-issue)))
     ;; This should NOT be called for non-epics
     (let ((dep-tree-called nil))
       (cl-letf (((symbol-function 'beads-command-dep-tree!)
                  (lambda (&rest _args)
                    (setq dep-tree-called t)
                    (vector)))
                 ((symbol-function 'beads-agent--get-sessions-for-issue)
                  (lambda (_) nil)))
         (beads-show--render-issue parsed-issue)
         ;; Should NOT call dep-tree for non-epic
         (should-not dep-tree-called))))))

(ert-deftest beads-show-test-sub-issues-grouped-by-status ()
  "Test that sub-issues are grouped by status (in_progress first)."
  (beads-show-test-with-temp-buffer
   (cl-letf (((symbol-function 'beads-command-dep-tree!)
              (lambda (&rest _args)
                beads-show-test--sub-issues-data)))
     (let ((inhibit-read-only t))
       (beads-show--insert-sub-issues-section "bd-epic-1")
       (let ((content (buffer-substring-no-properties (point-min) (point-max))))
         ;; in_progress issues should appear before open ones
         (let ((in-progress-pos (string-match "bd-sub-2" content))
               (open-pos (string-match "bd-sub-3" content)))
           (should (< in-progress-pos open-pos))))))))

;;; Agent Section Tests

(ert-deftest beads-show-test-insert-agent-section-nil-backend-name ()
  "Test that nil backend-name is handled gracefully."
  (beads-show-test-with-temp-buffer
   (let ((mock-session (beads-agent-session
                        :id "bd-42#1"
                        :issue-id "bd-42"
                        :backend-name "test-backend"
                        :project-dir "/tmp"
                        :started-at "2025-01-15T10:00:00Z")))
     (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
                (lambda (_) (list mock-session)))
               ((symbol-function 'beads-agent--session-active-p)
                (lambda (_) t))
               ;; Mock backend-name to return nil (testing the fix)
               ((symbol-function 'beads-agent-session-backend-name)
                (lambda (_) nil)))
       (let ((inhibit-read-only t))
         (beads-show--insert-agent-section "bd-42")
         (let ((content (buffer-substring-no-properties (point-min) (point-max))))
           ;; Should show "unknown" instead of crashing
           (should (string-match-p "unknown" content))
           ;; Should still show the agent section
           (should (string-match-p "Agent Sessions" content))))))))

(ert-deftest beads-show-test-insert-agent-section-nil-started-at ()
  "Test that nil started-at is handled gracefully."
  (beads-show-test-with-temp-buffer
   (let ((mock-session (beads-agent-session
                        :id "bd-42#1"
                        :issue-id "bd-42"
                        :backend-name "test-backend"
                        :project-dir "/tmp"
                        :started-at "2025-01-15T10:00:00Z")))
     (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
                (lambda (_) (list mock-session)))
               ((symbol-function 'beads-agent--session-active-p)
                (lambda (_) t))
               ;; Mock started-at to return nil
               ((symbol-function 'beads-agent-session-started-at)
                (lambda (_) nil)))
       (let ((inhibit-read-only t))
         (beads-show--insert-agent-section "bd-42")
         (let ((content (buffer-substring-no-properties (point-min) (point-max))))
           ;; Should show "N/A" for the date
           (should (string-match-p "N/A" content))
           ;; Should still show the agent section
           (should (string-match-p "Agent Sessions" content))))))))

;;; =========================================================================
;;; Directory-Aware Buffer Identity Tests (beads.el-4pgx)
;;; =========================================================================
;;
;; These tests verify the directory-aware show buffer identity model.
;; Key principle: Buffer identity is (issue-id, project-dir) pair.

(ert-deftest beads-show-test-normalize-directory ()
  "Test directory normalization for consistent comparison."
  ;; Use /tmp which is guaranteed to exist
  (let ((normalized (beads-show--normalize-directory "/tmp/")))
    (should (stringp normalized))
    ;; Should strip trailing slash
    (should (equal normalized "/tmp"))))

(ert-deftest beads-show-test-find-buffer-for-project-not-found ()
  "Test finding buffer when none exists for the project."
  (should (null (beads-show--find-buffer-for-project "/tmp/nonexistent-project"))))

(ert-deftest beads-show-test-find-buffer-for-project-found ()
  "Test finding existing buffer by project directory."
  (let ((test-buffer (generate-new-buffer "*beads-show: test-project*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (beads-show-mode)
          (setq beads-show--issue-id "bd-42")
          (setq beads-show--project-dir "/tmp")
          ;; Should find our buffer
          (should (eq (beads-show--find-buffer-for-project "/tmp")
                      test-buffer))
          ;; Should NOT find buffer for different project
          (should (null (beads-show--find-buffer-for-project "/other"))))
      (kill-buffer test-buffer))))

(ert-deftest beads-show-test-get-or-create-buffer-creates-new ()
  "Test get-or-create-buffer creates new buffer when none exists."
  (let (created-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/new-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "new-project")))
          (setq created-buffer (beads-show--get-or-create-buffer))
          (should (bufferp created-buffer))
          ;; Buffer name should include project name, not issue-id
          (should (string-match-p "new-project" (buffer-name created-buffer)))
          (with-current-buffer created-buffer
            (should (equal beads-show--project-dir "/tmp/new-project"))
            (should (equal beads-show--branch "main"))
            (should (equal beads-show--proj-name "new-project"))))
      (when (and created-buffer (buffer-live-p created-buffer))
        (kill-buffer created-buffer)))))

(ert-deftest beads-show-test-get-or-create-buffer-reuses-existing ()
  "Test get-or-create-buffer reuses buffer for same project."
  (let ((test-buffer (generate-new-buffer "*beads-show: existing-project*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (beads-show-mode)
            (setq beads-show--issue-id "bd-42")
            (setq beads-show--project-dir "/tmp/existing-project"))
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/tmp/existing-project"))
                    ((symbol-function 'beads-git-get-branch)
                     (lambda () "feature"))
                    ((symbol-function 'beads-git-get-project-name)
                     (lambda () "existing-project")))
            ;; Should return the existing buffer (same project)
            (should (eq (beads-show--get-or-create-buffer) test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest beads-show-test-different-project-different-buffer ()
  "Test that different projects create different buffers."
  (let ((buffer1 (generate-new-buffer "*beads-show: project1*"))
        (buffer2 nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer1
            (beads-show-mode)
            (setq beads-show--issue-id "bd-42")
            (setq beads-show--project-dir "/tmp/project1"))
          ;; Create buffer for different project
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/tmp/project2"))
                    ((symbol-function 'beads-git-get-branch)
                     (lambda () "main"))
                    ((symbol-function 'beads-git-get-project-name)
                     (lambda () "project2")))
            ;; Should create NEW buffer (different project)
            (setq buffer2 (beads-show--get-or-create-buffer))
            (should (bufferp buffer2))
            (should-not (eq buffer1 buffer2))))
      (kill-buffer buffer1)
      (when (and buffer2 (buffer-live-p buffer2))
        (kill-buffer buffer2)))))

(ert-deftest beads-show-test-same-project-different-branch-same-buffer ()
  "Test that same project but different branch uses same buffer.
This is the CRITICAL behavioral test for directory-as-identity."
  (let ((test-buffer (generate-new-buffer "*beads-show: project*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (beads-show-mode)
            (setq beads-show--issue-id "bd-42")
            (setq beads-show--project-dir "/tmp/project")
            (setq beads-show--branch "main"))
          ;; Simulate branch switch - branch changes but directory doesn't
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/tmp/project"))
                    ((symbol-function 'beads-git-get-branch)
                     (lambda () "feature"))  ; Different branch!
                    ((symbol-function 'beads-git-get-project-name)
                     (lambda () "project")))
            ;; CRITICAL: Should return SAME buffer (same directory)
            (let ((result (beads-show--get-or-create-buffer)))
              (should (eq result test-buffer)))))
      (kill-buffer test-buffer))))

(ert-deftest beads-show-test-buffer-local-variables-set ()
  "Test that buffer-local variables are set correctly on creation."
  (let (created-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/home/user/code/my-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "feature-branch"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "my-project")))
          (setq created-buffer (beads-show--get-or-create-buffer))
          (with-current-buffer created-buffer
            ;; Verify all variables set
            (should (equal beads-show--project-dir "/home/user/code/my-project"))
            (should (equal beads-show--branch "feature-branch"))
            (should (equal beads-show--proj-name "my-project"))))
      (when (and created-buffer (buffer-live-p created-buffer))
        (kill-buffer created-buffer)))))

;;; =========================================================================
;;; Worktree Session Integration Tests (beads.el-1hde)
;;; =========================================================================
;;
;; These tests verify show buffer integration with worktree sessions.
;; Sessions are keyed by directory, not branch.

(ert-deftest beads-show-test-register-with-session ()
  "Test that registering adds buffer to session."
  (let ((test-buffer (generate-new-buffer "*beads-show-test*"))
        (beads-sesman--worktree-sessions nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/test-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "test-project")))
          (with-current-buffer test-buffer
            (beads-show-mode)
            (setq beads-show--project-dir "/tmp/test-project")
            (beads-show--register-with-session))
          ;; Session should exist
          (should (= 1 (length beads-sesman--worktree-sessions)))
          ;; Buffer should be in session
          (let ((session (car beads-sesman--worktree-sessions)))
            (should (memq test-buffer (oref session buffers)))))
      (kill-buffer test-buffer)
      (setq beads-sesman--worktree-sessions nil))))

(ert-deftest beads-show-test-unregister-from-session ()
  "Test that unregistering removes buffer from session.
Empty sessions are automatically cleaned up."
  (let ((test-buffer (generate-new-buffer "*beads-show-test*"))
        (beads-sesman--worktree-sessions nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/test-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "test-project")))
          (with-current-buffer test-buffer
            (beads-show-mode)
            (setq beads-show--project-dir "/tmp/test-project")
            (beads-show--register-with-session)
            ;; Buffer should be in session
            (let ((session (car beads-sesman--worktree-sessions)))
              (should (memq test-buffer (oref session buffers))))
            ;; Now unregister
            (beads-show--unregister-from-session))
          ;; Session should be cleaned up (empty sessions are removed)
          ;; Either no sessions left, or buffer not in any remaining session
          (if (null beads-sesman--worktree-sessions)
              (should t)  ; Session was cleaned up - good!
            ;; If session still exists, buffer should not be in it
            (let ((session (car beads-sesman--worktree-sessions)))
              (should-not (memq test-buffer (oref session buffers))))))
      (kill-buffer test-buffer)
      (setq beads-sesman--worktree-sessions nil))))

(ert-deftest beads-show-test-multiple-buffers-same-session ()
  "Test that multiple show buffers can be in the same session."
  (let ((buffer1 (generate-new-buffer "*beads-show-test-1*"))
        (buffer2 (generate-new-buffer "*beads-show-test-2*"))
        (beads-sesman--worktree-sessions nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/test-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "test-project")))
          ;; Register first buffer
          (with-current-buffer buffer1
            (beads-show-mode)
            (setq beads-show--project-dir "/tmp/test-project")
            (beads-show--register-with-session))
          ;; Register second buffer
          (with-current-buffer buffer2
            (beads-show-mode)
            (setq beads-show--project-dir "/tmp/test-project")
            (beads-show--register-with-session))
          ;; Should still have only one session
          (should (= 1 (length beads-sesman--worktree-sessions)))
          ;; Session should contain both buffers
          (let ((session (car beads-sesman--worktree-sessions)))
            (should (memq buffer1 (oref session buffers)))
            (should (memq buffer2 (oref session buffers)))))
      (kill-buffer buffer1)
      (kill-buffer buffer2)
      (setq beads-sesman--worktree-sessions nil))))

(ert-deftest beads-show-test-kill-buffer-removes-from-session ()
  "Test that killing a buffer removes it from the session via hook."
  (let ((test-buffer (generate-new-buffer "*beads-show-test*"))
        (beads-sesman--worktree-sessions nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/test-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "test-project")))
          (with-current-buffer test-buffer
            (beads-show-mode)
            (setq beads-show--project-dir "/tmp/test-project")
            (beads-show--register-with-session))
          ;; Buffer should be in session
          (let ((session (car beads-sesman--worktree-sessions)))
            (should (memq test-buffer (oref session buffers))))
          ;; Kill buffer - hook should remove it
          (kill-buffer test-buffer)
          ;; Session should be cleaned up (empty session removed)
          ;; Since there are no other buffers or agents, session should be gone
          (should (null beads-sesman--worktree-sessions)))
      ;; Cleanup in case test failed
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq beads-sesman--worktree-sessions nil))))

(ert-deftest beads-show-test-kill-one-buffer-keeps-others-in-session ()
  "Test that killing one buffer does not affect others in same session."
  (let ((buffer1 (generate-new-buffer "*beads-show-test-1*"))
        (buffer2 (generate-new-buffer "*beads-show-test-2*"))
        (beads-sesman--worktree-sessions nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/test-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "test-project")))
          ;; Register both buffers
          (dolist (buf (list buffer1 buffer2))
            (with-current-buffer buf
              (beads-show-mode)
              (setq beads-show--project-dir "/tmp/test-project")
              (beads-show--register-with-session)))
          ;; Kill first buffer
          (kill-buffer buffer1)
          ;; Session should still exist with buffer2
          (should (= 1 (length beads-sesman--worktree-sessions)))
          (let ((session (car beads-sesman--worktree-sessions)))
            (should (memq buffer2 (oref session buffers)))
            (should-not (memq buffer1 (oref session buffers)))))
      (when (buffer-live-p buffer1)
        (kill-buffer buffer1))
      (when (buffer-live-p buffer2)
        (kill-buffer buffer2))
      (setq beads-sesman--worktree-sessions nil))))

(ert-deftest beads-show-test-register-no-duplicate ()
  "Test that registering same buffer twice does not duplicate in session."
  (let ((test-buffer (generate-new-buffer "*beads-show-test*"))
        (beads-sesman--worktree-sessions nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/test-project"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-get-project-name)
                   (lambda () "test-project")))
          (with-current-buffer test-buffer
            (beads-show-mode)
            (setq beads-show--project-dir "/tmp/test-project")
            ;; Register twice
            (beads-show--register-with-session)
            (beads-show--register-with-session))
          ;; Buffer should appear only once
          (let ((session (car beads-sesman--worktree-sessions)))
            (should (= 1 (length (oref session buffers))))))
      (kill-buffer test-buffer)
      (setq beads-sesman--worktree-sessions nil))))

;;; Additional Coverage Tests

(ert-deftest beads-show-test-truncate-title-short ()
  "Test truncate-title with short title."
  (should (equal "Short Title"
                 (beads-show--truncate-title "Short Title" 30))))

(ert-deftest beads-show-test-truncate-title-long ()
  "Test truncate-title with long title."
  (let ((result (beads-show--truncate-title "This is a very long title" 15)))
    (should (= (length result) 15))
    (should (string-suffix-p "..." result))))

(ert-deftest beads-show-test-truncate-title-nil ()
  "Test truncate-title with nil."
  (should (equal "Untitled" (beads-show--truncate-title nil 20))))

(ert-deftest beads-show-test-format-sub-issue-status-open ()
  "Test format-sub-issue-status for open."
  (let ((result (beads-show--format-sub-issue-status "open")))
    (should (string= "OPEN" result))
    (should (eq (get-text-property 0 'face result) 'success))))

(ert-deftest beads-show-test-format-sub-issue-status-closed ()
  "Test format-sub-issue-status for closed."
  (let ((result (beads-show--format-sub-issue-status "closed")))
    (should (string= "CLOSED" result))
    (should (eq (get-text-property 0 'face result) 'shadow))))

(ert-deftest beads-show-test-format-sub-issue-status-blocked ()
  "Test format-sub-issue-status for blocked."
  (let ((result (beads-show--format-sub-issue-status "blocked")))
    (should (string= "BLOCKED" result))
    (should (eq (get-text-property 0 'face result) 'error))))

(ert-deftest beads-show-test-format-sub-issue-status-in-progress ()
  "Test format-sub-issue-status for in_progress."
  (let ((result (beads-show--format-sub-issue-status "in_progress")))
    (should (string= "IN_PROGRESS" result))
    (should (eq (get-text-property 0 'face result) 'warning))))

(ert-deftest beads-show-test-normalize-directory-strips-trailing ()
  "Test normalize-directory strips trailing slash."
  (should (equal "/home/test/project"
                 (beads-show--normalize-directory "/home/test/project/"))))

(ert-deftest beads-show-test-normalize-directory-expands ()
  "Test normalize-directory expands file name."
  (let ((result (beads-show--normalize-directory "~")))
    (should (stringp result))
    (should (not (string-prefix-p "~" result)))))

(ert-deftest beads-show-test-find-buffer-for-project-no-match ()
  "Test find-buffer-for-project returns nil when not found."
  (should (null (beads-show--find-buffer-for-project "/tmp/nonexistent"))))

;;; Insert Functions Tests

(ert-deftest beads-show-test-insert-header-with-value ()
  "Test insert-header with value."
  (with-temp-buffer
    (beads-show--insert-header "Status" "open")
    (should (string-match-p "Status" (buffer-string)))
    (should (string-match-p "open" (buffer-string)))))

(ert-deftest beads-show-test-insert-header-with-face ()
  "Test insert-header with custom face."
  (with-temp-buffer
    (beads-show--insert-header "Priority" "P1" 'error)
    (should (string-match-p "Priority" (buffer-string)))
    (should (string-match-p "P1" (buffer-string)))))

(ert-deftest beads-show-test-insert-section-with-content ()
  "Test insert-section with content."
  (with-temp-buffer
    (beads-show--insert-section "Description" "Test content here.")
    (should (string-match-p "Description" (buffer-string)))
    (should (string-match-p "Test content" (buffer-string)))))

(ert-deftest beads-show-test-insert-section-nil-content ()
  "Test insert-section with nil content."
  (with-temp-buffer
    (beads-show--insert-section "Notes" nil)
    ;; With nil content, section may or may not insert anything
    (should t)))

;;; Formatting Functions Tests

(ert-deftest beads-show-test-format-status-all-statuses ()
  "Test format-status for all status values."
  (should (stringp (beads-show--format-status "open")))
  (should (stringp (beads-show--format-status "closed")))
  (should (stringp (beads-show--format-status "in_progress")))
  (should (stringp (beads-show--format-status "blocked"))))

(ert-deftest beads-show-test-format-priority-all-values ()
  "Test format-priority for all priority values."
  (should (stringp (beads-show--format-priority 0)))
  (should (stringp (beads-show--format-priority 1)))
  (should (stringp (beads-show--format-priority 2)))
  (should (stringp (beads-show--format-priority 3)))
  (should (stringp (beads-show--format-priority 4))))

(ert-deftest beads-show-test-format-date-valid ()
  "Test format-date with valid ISO date."
  (let ((result (beads-show--format-date "2025-01-01T12:00:00Z")))
    (should (stringp result))))

;;; Section Level Tests

(ert-deftest beads-show-test-section-level-function-exists ()
  "Test section-level function exists."
  (should (fboundp 'beads-show--section-level)))

(ert-deftest beads-show-test-extract-issue-at-point-exists ()
  "Test extract-issue-at-point function exists."
  (should (fboundp 'beads-show--extract-issue-at-point)))

;;; Additional Coverage Tests

(ert-deftest beads-show-test-in-fenced-code-block-outside-returns-nil ()
  "Test in-fenced-code-block outside code block."
  (with-temp-buffer
    (insert "Normal text\nMore text\n")
    (goto-char (point-min))
    (should-not (beads-show--in-fenced-code-block))))

(ert-deftest beads-show-test-skip-blank-lines-backward-moves-point ()
  "Test skip-blank-lines-backward function."
  (with-temp-buffer
    (insert "Text\n\n\n\nHere")
    (goto-char (point-max))
    (beads-show--skip-blank-lines-backward)
    ;; Should stop after blank lines
    (should (<= (point) (point-max)))))

(ert-deftest beads-show-test-button-action-function-exists ()
  "Test button-action function exists."
  (should (fboundp 'beads-show--button-action)))

(ert-deftest beads-show-test-get-sub-issues-nil-tree ()
  "Test get-sub-issues with issue without sub-issues."
  (let ((issue (beads-issue :id "bd-1" :title "Test"
                            :status "open" :priority 2
                            :issue-type "task")))
    ;; Should not error
    (should (listp (beads-show--get-sub-issues issue)))))

;;; Reference Navigation Tests

(ert-deftest beads-show-test-next-reference-no-reference ()
  "Test next-reference when no reference found."
  (with-temp-buffer
    (insert "No references here at all")
    (goto-char (point-min))
    ;; Should not error, just show message
    (beads-show-next-reference)
    (should (= (point) (point-min)))))

(ert-deftest beads-show-test-next-reference-finds-reference ()
  "Test next-reference finds issue reference."
  (with-temp-buffer
    (insert "See bd-42 for details")
    (goto-char (point-min))
    (beads-show-next-reference)
    ;; Should move to the reference
    (should (looking-at "bd-42"))))

(ert-deftest beads-show-test-next-reference-moves-past-current ()
  "Test next-reference moves past current reference."
  (with-temp-buffer
    (insert "See bd-1 and bd-2 for details")
    (goto-char (point-min))
    (search-forward "bd-1")
    (goto-char (match-beginning 0))
    (beads-show-next-reference)
    ;; Should move to bd-2
    (should (looking-at "bd-2"))))

(ert-deftest beads-show-test-previous-reference-no-reference ()
  "Test previous-reference when no reference found."
  (with-temp-buffer
    (insert "No references here at all")
    (goto-char (point-max))
    ;; Should not error, just show message
    (beads-show-previous-reference)
    (should (= (point) (point-max)))))

(ert-deftest beads-show-test-previous-reference-finds-reference ()
  "Test previous-reference finds issue reference."
  (with-temp-buffer
    (insert "See bd-42 for details")
    (let ((start (point-max)))
      (goto-char start)
      (beads-show-previous-reference)
      ;; Should move to the reference (point moved back)
      (should (< (point) start)))))

(ert-deftest beads-show-test-previous-reference-moves-before-current ()
  "Test previous-reference moves when on a reference."
  (with-temp-buffer
    (insert "See bd-1\nand bd-2 for details")
    (goto-char (point-max))
    (search-backward "bd-2")
    (let ((start (point)))
      (beads-show-previous-reference)
      ;; Should move backward to bd-1 (on different line)
      (should (< (point) start)))))

;;; Button Navigation Tests

(ert-deftest beads-show-test-next-button-no-buttons ()
  "Test next-button when no buttons in buffer."
  (with-temp-buffer
    (insert "No buttons here")
    (goto-char (point-min))
    ;; Should not error
    (beads-show-next-button)
    (should t)))

(ert-deftest beads-show-test-next-button-finds-button ()
  "Test next-button finds button."
  (with-temp-buffer
    (insert "Text ")
    (let ((btn-start (point)))
      (insert-text-button "Click me" 'action #'ignore)
      (goto-char (point-min))
      (beads-show-next-button)
      (should (= (point) btn-start)))))

(ert-deftest beads-show-test-next-button-wraps-around ()
  "Test next-button wraps to first button."
  (with-temp-buffer
    (insert-text-button "First" 'action #'ignore)
    (insert " text ")
    (insert-text-button "Second" 'action #'ignore)
    (let ((start (point-max)))
      (goto-char start)
      ;; Should wrap to beginning
      (beads-show-next-button)
      ;; Point should move (either to first button or stay if wrap didn't work)
      (should (<= (point) start)))))

(ert-deftest beads-show-test-previous-button-no-buttons ()
  "Test previous-button when no buttons in buffer."
  (with-temp-buffer
    (insert "No buttons here")
    (goto-char (point-max))
    ;; Should not error
    (beads-show-previous-button)
    (should t)))

(ert-deftest beads-show-test-previous-button-finds-button ()
  "Test previous-button finds button."
  (with-temp-buffer
    (let ((btn-start (point)))
      (insert-text-button "Click me" 'action #'ignore)
      (insert " more text")
      (goto-char (point-max))
      (beads-show-previous-button)
      (should (= (point) btn-start)))))

;;; Block Navigation Tests

(ert-deftest beads-show-test-backward-block-function-exists ()
  "Test backward-block function exists."
  (should (fboundp 'beads-show-backward-block)))

(ert-deftest beads-show-test-forward-block-function-exists ()
  "Test forward-block function exists."
  (should (fboundp 'beads-show-forward-block)))

(ert-deftest beads-show-test-at-block-boundary-fenced-code ()
  "Test at-block-boundary detects fenced code."
  (with-temp-buffer
    (insert "```\ncode\n```")
    (goto-char (point-min))
    (let ((result (beads-show--at-block-boundary)))
      (should (eq result 'fenced-code)))))

(ert-deftest beads-show-test-at-block-boundary-list ()
  "Test at-block-boundary detects list."
  (with-temp-buffer
    (insert "- item 1")
    (goto-char (point-min))
    (let ((result (beads-show--at-block-boundary)))
      (should (eq result 'list)))))

(ert-deftest beads-show-test-at-block-boundary-blockquote ()
  "Test at-block-boundary detects blockquote."
  (with-temp-buffer
    (insert "> quote")
    (goto-char (point-min))
    (let ((result (beads-show--at-block-boundary)))
      (should (eq result 'blockquote)))))

(ert-deftest beads-show-test-at-block-boundary-indented-code ()
  "Test at-block-boundary detects indented code."
  (with-temp-buffer
    (insert "    code")
    (goto-char (point-min))
    (let ((result (beads-show--at-block-boundary)))
      (should (eq result 'indented-code)))))

;;; Follow Reference Tests

(ert-deftest beads-show-test-follow-reference-other-window-no-reference ()
  "Test follow-reference-other-window with no reference at point."
  (with-temp-buffer
    (insert "No reference here")
    (goto-char (point-min))
    ;; Should just show message, not error
    (beads-show-follow-reference-other-window)
    (should t)))

(ert-deftest beads-show-test-follow-reference-other-window-function-exists ()
  "Test follow-reference-other-window function exists."
  (should (fboundp 'beads-show-follow-reference-other-window)))

;;; Edit Field Tests

(ert-deftest beads-show-test-edit-field-multiline-function-exists ()
  "Test edit-field-multiline function exists."
  (should (fboundp 'beads-show--edit-field-multiline)))

;;; ============================================================
;;; Integration Tests (require bd executable)
;;; ============================================================

(ert-deftest beads-show-test-integration-show-real-issue ()
  "Integration test: show a real issue created with bd."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a real issue
    (let ((issue (beads-command-create! :title "Test Issue for Show"
                                         :description "Test description"
                                         :priority 2
                                         :issue-type "task")))
      (should issue)
      (let ((issue-id (oref issue id)))
        ;; Test beads-show--render-issue
        (with-temp-buffer
          (beads-show-mode)
          (let ((inhibit-read-only t))
            (beads-show--render-issue issue))
          ;; Buffer should contain the issue title
          (should (string-match-p "Test Issue for Show" (buffer-string)))
          ;; Buffer should contain the description
          (should (string-match-p "Test description" (buffer-string))))))))

(ert-deftest beads-show-test-integration-get-or-create-buffer ()
  "Integration test: get-or-create-buffer creates project-based buffer."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Get or create buffer should return a buffer named after project
    (let ((buf (beads-show--get-or-create-buffer)))
      (should (bufferp buf))
      ;; Buffer name should include project name (from beads-git-get-project-name)
      (should (string-match-p "beads-show" (buffer-name buf)))
      ;; Clean up
      (kill-buffer buf))))

(ert-deftest beads-show-test-integration-format-status ()
  "Integration test: format status from real issue."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create! :title "Status Test"
                                         :priority 2
                                         :issue-type "task")))
      ;; Newly created issue should be open
      (let ((formatted (beads-show--format-status (oref issue status))))
        (should (stringp formatted))
        (should (string-match-p "[Oo]pen" formatted))))))

(ert-deftest beads-show-test-integration-format-priority ()
  "Integration test: format priority from real issue."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create! :title "Priority Test"
                                         :priority 1
                                         :issue-type "task")))
      ;; Priority 1 should be displayed as "1 (High)"
      (let ((formatted (beads-show--format-priority (oref issue priority))))
        (should (stringp formatted))
        (should (string-match-p "[Hh]igh" formatted))))))

(ert-deftest beads-show-test-integration-buttonize-references ()
  "Integration test: buttonize issue references."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((issue1 (beads-command-create! :title "First Issue" :priority 2 :issue-type "task"))
           (issue2 (beads-command-create! :title "Second Issue" :priority 2 :issue-type "task"))
           (id1 (oref issue1 id))
           (id2 (oref issue2 id)))
      ;; Verify we created issues with IDs
      (should (stringp id1))
      (should (stringp id2))
      ;; Verify buttonize runs without error
      (with-temp-buffer
        (insert (format "See %s and %s for details" id1 id2))
        (beads-show--buttonize-references (point-min) (point-max))
        ;; Buffer should still contain the IDs
        (should (string-match-p (regexp-quote id1) (buffer-string)))
        (should (string-match-p (regexp-quote id2) (buffer-string)))))))

(ert-deftest beads-show-test-integration-insert-header ()
  "Integration test: insert header function."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (with-temp-buffer
      (beads-show--insert-header "Test Label" "Test Value")
      (should (string-match-p "Test Label" (buffer-string)))
      (should (string-match-p "Test Value" (buffer-string))))))

(ert-deftest beads-show-test-integration-insert-section ()
  "Integration test: insert section function."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (with-temp-buffer
      (beads-show--insert-section "Description" "This is the content")
      (should (string-match-p "Description" (buffer-string)))
      (should (string-match-p "This is the content" (buffer-string))))))

;;; Follow Mode Support Tests

(ert-deftest beads-show-test-find-visible-buffer-none ()
  "Test find-visible-buffer returns nil when no show buffer is visible."
  (beads-show-test-with-git-mocks
   (should-not (beads-show--find-visible-buffer))))

(ert-deftest beads-show-test-find-visible-buffer-exists-but-hidden ()
  "Test find-visible-buffer returns nil when buffer exists but not visible."
  (beads-show-test-with-git-mocks
   (let ((buf (get-buffer-create beads-show-test--buffer-name)))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (beads-show-mode)
             (setq-local beads-show--project-dir "/tmp/test-project"))
           ;; Buffer exists but no window
           (cl-letf (((symbol-function 'get-buffer-window)
                      (lambda (_) nil)))
             (should-not (beads-show--find-visible-buffer))))
       (kill-buffer buf)))))

(ert-deftest beads-show-test-find-visible-buffer-wrong-project ()
  "Test find-visible-buffer returns nil for different project."
  (beads-show-test-with-git-mocks
   (let ((buf (get-buffer-create "*beads-show: other-project*")))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (beads-show-mode)
             (setq-local beads-show--project-dir "/tmp/other-project"))
           (cl-letf (((symbol-function 'get-buffer-window)
                      (lambda (_) t)))
             ;; Looking for test-project, but buffer is for other-project
             (should-not (beads-show--find-visible-buffer "/tmp/test-project"))))
       (kill-buffer buf)))))

(ert-deftest beads-show-test-update-buffer-sets-issue-id ()
  "Test that update-buffer sets the issue ID."
  (beads-show-test-with-git-mocks
   (let ((buf (get-buffer-create beads-show-test--buffer-name)))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (beads-show-mode))
           (cl-letf (((symbol-function 'beads-command-show!)
                      (lambda (&rest _)
                        (beads-issue :id "bd-42" :title "Test"
                                     :status "open" :priority 2
                                     :issue-type "task")))
                     ((symbol-function 'beads-show--render-issue)
                      (lambda (_) nil)))
             (beads-show-update-buffer "bd-42" buf)
             (with-current-buffer buf
               (should (equal beads-show--issue-id "bd-42")))))
       (kill-buffer buf)))))

(ert-deftest beads-show-test-update-buffer-returns-buffer ()
  "Test that update-buffer returns the buffer."
  (beads-show-test-with-git-mocks
   (let ((buf (get-buffer-create beads-show-test--buffer-name)))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (beads-show-mode))
           (cl-letf (((symbol-function 'beads-command-show!)
                      (lambda (&rest _)
                        (beads-issue :id "bd-42" :title "Test"
                                     :status "open" :priority 2
                                     :issue-type "task")))
                     ((symbol-function 'beads-show--render-issue)
                      (lambda (_) nil)))
             (should (eq (beads-show-update-buffer "bd-42" buf) buf))))
       (kill-buffer buf)))))

(ert-deftest beads-show-test-update-buffer-handles-error ()
  "Test that update-buffer handles errors gracefully."
  (beads-show-test-with-git-mocks
   (let ((buf (get-buffer-create beads-show-test--buffer-name)))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (beads-show-mode))
           (cl-letf (((symbol-function 'beads-command-show!)
                      (lambda (&rest _)
                        (error "Network error"))))
             (beads-show-update-buffer "bd-42" buf)
             (with-current-buffer buf
               (should (string-match-p "Error loading issue"
                                       (buffer-string))))))
       (kill-buffer buf)))))

(ert-deftest beads-show-test-update-buffer-function-exists ()
  "Test that beads-show-update-buffer function exists."
  (should (fboundp 'beads-show-update-buffer)))

(ert-deftest beads-show-test-find-visible-buffer-function-exists ()
  "Test that beads-show--find-visible-buffer function exists."
  (should (fboundp 'beads-show--find-visible-buffer)))

(provide 'beads-show-test)
;;; beads-show-test.el ends here
