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

(defun beads-show-test--mock-show-command (issue-data)
  "Create a mock for show command returning ISSUE-DATA."
  (lambda (subcommand &rest args)
    (if (string= subcommand "show")
        issue-data
      (error "Unexpected command: %s" subcommand))))

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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command beads-show-test--full-issue)))
    (beads-show "bd-42")
    (should (get-buffer "*beads-show: bd-42*"))
    (with-current-buffer "*beads-show: bd-42*"
      (should (derived-mode-p 'beads-show-mode))
      (should (string= beads-show--issue-id "bd-42")))
    (kill-buffer "*beads-show: bd-42*")))

(ert-deftest beads-show-test-show-command-renders-content ()
  "Test that beads-show renders issue content."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command beads-show-test--full-issue)))
    (beads-show "bd-42")
    (with-current-buffer "*beads-show: bd-42*"
      (let ((text (beads-show-test--get-buffer-text)))
        (should (string-match-p "Implement feature X" text))
        (should (string-match-p "alice" text)))
      (kill-buffer))))

(ert-deftest beads-show-test-show-command-handles-error ()
  "Test that beads-show handles errors gracefully."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (&rest args) (error "Database not found"))))
    (beads-show "bd-999")
    (with-current-buffer "*beads-show: bd-999*"
      (let ((text (beads-show-test--get-buffer-text)))
        (should (string-match-p "Error loading issue" text))
        (should (string-match-p "Database not found" text)))
      (kill-buffer))))

(ert-deftest beads-show-test-show-at-point-with-reference ()
  "Test beads-show-at-point when cursor is on bd-N."
  (with-temp-buffer
    (insert "See issue bd-42 for details")
    (goto-char (point-min))
    (search-forward "bd-42")
    (goto-char (match-beginning 0))
    (cl-letf (((symbol-function 'beads--run-command)
               (beads-show-test--mock-show-command beads-show-test--full-issue)))
      (beads-show-at-point)
      (should (get-buffer "*beads-show: bd-42*"))
      (kill-buffer "*beads-show: bd-42*"))))

(ert-deftest beads-show-test-show-at-point-without-reference ()
  "Test beads-show-at-point when no reference at point."
  (with-temp-buffer
    (insert "No reference here")
    (goto-char (point-min))
    (should-error (beads-show-at-point) :type 'user-error)))

(ert-deftest beads-show-test-refresh-command ()
  "Test beads-refresh-show command."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command beads-show-test--full-issue)))
    (beads-show "bd-42")
    (with-current-buffer "*beads-show: bd-42*"
      (goto-char (point-max))
      (let ((pos (point)))
        ;; Refresh should reload and try to preserve position
        (beads-refresh-show)
        (should (string-match-p "Implement feature X"
                              (beads-show-test--get-buffer-text))))
      (kill-buffer))))

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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command beads-show-test--full-issue)))
    (beads-show "bd-42")
    (with-current-buffer "*beads-show: bd-42*"
      (goto-char (point-min))
      (let ((initial-pos (point)))
        (beads-show-next-section)
        (should (> (point) initial-pos)))
      (kill-buffer))))

(ert-deftest beads-show-test-previous-section ()
  "Test navigation to previous section."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command beads-show-test--full-issue)))
    (beads-show "bd-42")
    (with-current-buffer "*beads-show: bd-42*"
      (goto-char (point-max))
      (beads-show-previous-section)
      (should (< (point) (point-max)))
      (kill-buffer))))

(ert-deftest beads-show-test-follow-reference ()
  "Test following a reference with RET."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command beads-show-test--full-issue)))
    (beads-show "bd-42")
    (with-current-buffer "*beads-show: bd-42*"
      (goto-char (point-min))
      (when (search-forward "bd-1" nil t)
        (goto-char (match-beginning 0))
        (beads-show-follow-reference)
        (should (get-buffer "*beads-show: bd-1*"))
        (kill-buffer "*beads-show: bd-1*"))
      (kill-buffer))))

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

(ert-deftest beads-show-test-integration-full-workflow ()
  "Test full workflow: show issue, navigate, refresh."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command beads-show-test--full-issue)))
    ;; Show issue
    (beads-show "bd-42")
    (should (get-buffer "*beads-show: bd-42*"))

    (with-current-buffer "*beads-show: bd-42*"
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

      (kill-buffer))))

(ert-deftest beads-show-test-integration-multiple-buffers ()
  "Test managing multiple show buffers simultaneously."
  (cl-letf (((symbol-function 'beads--run-command)
             (lambda (cmd id)
               (cond
                ((string= id "bd-1") beads-show-test--minimal-issue)
                ((string= id "bd-42") beads-show-test--full-issue)
                (t (error "Unknown issue"))))))
    ;; Open first issue
    (beads-show "bd-1")
    (should (get-buffer "*beads-show: bd-1*"))

    ;; Open second issue
    (beads-show "bd-42")
    (should (get-buffer "*beads-show: bd-42*"))

    ;; Both buffers should exist
    (should (get-buffer "*beads-show: bd-1*"))
    (should (get-buffer "*beads-show: bd-42*"))

    ;; Each should have correct content
    (with-current-buffer "*beads-show: bd-1*"
      (should (string-match-p "Minimal issue"
                            (beads-show-test--get-buffer-text))))

    (with-current-buffer "*beads-show: bd-42*"
      (should (string-match-p "Implement feature X"
                            (beads-show-test--get-buffer-text))))

    ;; Cleanup
    (kill-buffer "*beads-show: bd-1*")
    (kill-buffer "*beads-show: bd-42*")))

(provide 'beads-show-test)
;;; beads-show-test.el ends here
