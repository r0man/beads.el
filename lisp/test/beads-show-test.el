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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Search for title
      (when (search-forward "Issue Title" nil t)
        (beginning-of-line)
        (should (eq (beads-show--section-level) 0)))
      (kill-buffer))))

(ert-deftest beads-show-test-section-level-major-section ()
  "Test section level detection for major sections (level 1)."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Search for "Description" major section
      (when (search-forward "Description\n─" nil t)
        (goto-char (match-beginning 0))
        (should (eq (beads-show--section-level) 1)))
      (kill-buffer))))

(ert-deftest beads-show-test-section-level-markdown-heading-2 ()
  "Test section level detection for markdown ## heading (level 2)."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Search for "## Level 2 Heading"
      (when (search-forward "## Level 2 Heading" nil t)
        (beginning-of-line)
        (should (eq (beads-show--section-level) 2)))
      (kill-buffer))))

(ert-deftest beads-show-test-section-level-markdown-heading-3 ()
  "Test section level detection for markdown ### heading (level 3)."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Search for "### Level 3 Heading"
      (when (search-forward "### Level 3 Heading" nil t)
        (beginning-of-line)
        (should (eq (beads-show--section-level) 3)))
      (kill-buffer))))

(ert-deftest beads-show-test-section-level-not-heading ()
  "Test section level returns nil for non-heading lines."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Search for "First paragraph" which is not a heading
      (when (search-forward "First paragraph" nil t)
        (beginning-of-line)
        (should (null (beads-show--section-level))))
      (kill-buffer))))

(ert-deftest beads-show-test-outline-next-basic ()
  "Test moving to next heading at any level."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
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
      (kill-buffer))))

(ert-deftest beads-show-test-outline-previous-basic ()
  "Test moving to previous heading at any level."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      ;; Start near the end
      (goto-char (point-max))
      (let ((start-pos (point)))
        ;; Move to previous heading
        (beads-show-outline-previous)
        (should (< (point) start-pos))
        (should (beads-show--section-level)))
      (kill-buffer))))

(ert-deftest beads-show-test-outline-next-at-end ()
  "Test outline-next at end of buffer shows message."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--minimal-issue)))
    (beads-show "bd-1")
    (with-current-buffer "*beads-show: bd-1*"
      (goto-char (point-max))
      (forward-line -1)
      (let ((pos (point)))
        (beads-show-outline-next)
        ;; Should not move and stay at same position
        (should (= (point) pos)))
      (kill-buffer))))

(ert-deftest beads-show-test-outline-previous-at-start ()
  "Test outline-previous at start of buffer shows message."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--minimal-issue)))
    (beads-show "bd-1")
    (with-current-buffer "*beads-show: bd-1*"
      (goto-char (point-min))
      (let ((pos (point)))
        (beads-show-outline-previous)
        ;; Should not move from start
        (should (= (point) pos)))
      (kill-buffer))))

(ert-deftest beads-show-test-outline-next-same-level ()
  "Test moving to next heading at same level."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
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
      (kill-buffer))))

(ert-deftest beads-show-test-outline-previous-same-level ()
  "Test moving to previous heading at same level."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
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
      (kill-buffer))))

(ert-deftest beads-show-test-outline-same-level-not-at-heading ()
  "Test outline-next-same-level errors when not at heading."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Move to non-heading text
      (when (search-forward "First paragraph" nil t)
        (should-error (beads-show-outline-next-same-level) :type 'user-error))
      (kill-buffer))))

(ert-deftest beads-show-test-outline-up-from-level-3 ()
  "Test moving up from level 3 heading to parent."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
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
      (kill-buffer))))

(ert-deftest beads-show-test-outline-up-from-level-2 ()
  "Test moving up from level 2 heading to level 1."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
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
      (kill-buffer))))

(ert-deftest beads-show-test-outline-up-at-level-0-errors ()
  "Test outline-up at level 0 (title) gives error."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Find title
      (when (search-forward "Issue Title" nil t)
        (beginning-of-line)
        (should (eq (beads-show--section-level) 0))
        (should-error (beads-show-outline-up) :type 'user-error))
      (kill-buffer))))

(ert-deftest beads-show-test-outline-up-not-at-heading ()
  "Test outline-up errors when not at heading."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Move to non-heading text
      (when (search-forward "First paragraph" nil t)
        (should-error (beads-show-outline-up) :type 'user-error))
      (kill-buffer))))

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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
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

      (kill-buffer))))

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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--markdown-rich-issue)))
    (beads-show "bd-100")
    (with-current-buffer "*beads-show: bd-100*"
      (goto-char (point-min))
      (let ((start-pos (point)))
        (beads-show-forward-paragraph)
        (should (> (point) start-pos)))
      (kill-buffer))))

(ert-deftest beads-show-test-backward-paragraph-basic ()
  "Test moving backward by paragraph."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--markdown-rich-issue)))
    (beads-show "bd-100")
    (with-current-buffer "*beads-show: bd-100*"
      (goto-char (point-max))
      (let ((start-pos (point)))
        (beads-show-backward-paragraph)
        (should (< (point) start-pos)))
      (kill-buffer))))

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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--markdown-rich-issue)))
    (beads-show "bd-100")
    (with-current-buffer "*beads-show: bd-100*"
      (goto-char (point-min))
      ;; Find some paragraph text
      (when (search-forward "bold" nil t)
        (beads-show-mark-paragraph)
        ;; Mark should be active
        (should (region-active-p))
        ;; Region should have some content
        (should (> (region-end) (region-beginning))))
      (kill-buffer))))

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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
      (goto-char (point-min))
      ;; Find "Regular paragraph"
      (when (search-forward "Regular paragraph" nil t)
        (beginning-of-line)
        (let ((start-pos (point)))
          (beads-show-forward-block)
          ;; Should have moved to next block
          (should (> (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-forward-block-skip-fenced-code ()
  "Test forward-block skips entire fenced code block."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
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
      (kill-buffer))))

(ert-deftest beads-show-test-forward-block-skip-list ()
  "Test forward-block skips entire list."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
      (goto-char (point-min))
      ;; Find list start
      (when (search-forward "- List item 1" nil t)
        (beginning-of-line)
        (let ((start-pos (point)))
          (beads-show-forward-block)
          ;; Should be past all list items
          (should (> (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-forward-block-skip-blockquote ()
  "Test forward-block skips entire blockquote."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
      (goto-char (point-min))
      ;; Find blockquote start
      (when (search-forward "> Blockquote line 1" nil t)
        (beginning-of-line)
        (let ((start-pos (point)))
          (beads-show-forward-block)
          ;; Should be past blockquote
          (should (> (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-forward-block-skip-indented-code ()
  "Test forward-block skips indented code block."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
      (goto-char (point-min))
      ;; Find indented code
      (when (search-forward "    Indented code line 1" nil t)
        (beginning-of-line)
        (let ((start-pos (point)))
          (beads-show-forward-block)
          ;; Should be past indented code
          (should (> (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-backward-block-from-text ()
  "Test backward-block navigation from regular text."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
      (goto-char (point-min))
      ;; Find "End text" near the end
      (when (search-forward "End text" nil t)
        (beginning-of-line)
        (let ((start-pos (point)))
          (beads-show-backward-block)
          ;; Should have moved backward to previous block
          (should (< (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-backward-block-to-list-start ()
  "Test backward-block moves backward from end of list."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
      (goto-char (point-min))
      ;; Find "List item 3" (end of list)
      (when (search-forward "- List item 3" nil t)
        (end-of-line)
        (forward-line 1)
        (let ((start-pos (point)))
          (beads-show-backward-block)
          ;; Should have moved backward
          (should (< (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-backward-block-to-fenced-code-start ()
  "Test backward-block finds start of fenced code block."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
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
      (kill-buffer))))

(ert-deftest beads-show-test-forward-block-at-end ()
  "Test forward-block at end of buffer shows message."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--minimal-issue)))
    (beads-show "bd-1")
    (with-current-buffer "*beads-show: bd-1*"
      (goto-char (point-max))
      (let ((pos (point)))
        (beads-show-forward-block)
        ;; Should not move much from end
        (should (<= (abs (- (point) pos)) 10)))
      (kill-buffer))))

(ert-deftest beads-show-test-backward-block-at-start ()
  "Test backward-block at start of buffer shows message."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--minimal-issue)))
    (beads-show "bd-1")
    (with-current-buffer "*beads-show: bd-1*"
      (goto-char (point-min))
      (forward-line 2)
      (let ((pos (point)))
        (beads-show-backward-block)
        ;; Should have moved or stayed
        (should (<= (point) pos)))
      (kill-buffer))))

(ert-deftest beads-show-test-block-navigation-sequence ()
  "Test sequence of forward and backward block navigation."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--block-rich-issue)))
    (beads-show "bd-300")
    (with-current-buffer "*beads-show: bd-300*"
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
      (kill-buffer))))

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
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Find Description heading
      (when (search-forward "Description" nil t)
        (beginning-of-line)
        (let ((start-pos (point)))
          (beads-show-beginning-of-section)
          ;; Should stay at same position (already at beginning)
          (should (= (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-beginning-of-section-in-content ()
  "Test moving to beginning of section from content."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Find some content text
      (when (search-forward "First paragraph" nil t)
        (beads-show-beginning-of-section)
        ;; Should have moved to Description section
        (should (beads-show--section-level)))
      (kill-buffer))))

(ert-deftest beads-show-test-end-of-section-basic ()
  "Test moving to end of section."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Find Description heading
      (when (search-forward "Description" nil t)
        (beginning-of-line)
        (let ((start-pos (point)))
          (beads-show-end-of-section)
          ;; Should have moved forward
          (should (> (point) start-pos))))
      (kill-buffer))))

(ert-deftest beads-show-test-mark-section-basic ()
  "Test marking a section."
  (cl-letf (((symbol-function 'beads--run-command)
             (beads-show-test--mock-show-command
              beads-show-test--outline-issue)))
    (beads-show "bd-200")
    (with-current-buffer "*beads-show: bd-200*"
      (goto-char (point-min))
      ;; Find Description heading
      (when (search-forward "Description" nil t)
        (beginning-of-line)
        (beads-show-mark-section)
        ;; Mark should be active
        (should (region-active-p))
        ;; Region should have some content
        (should (> (region-end) (region-beginning))))
      (kill-buffer))))

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
        (update-args nil))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (subcommand &rest args)
                 ;; Only capture update command, not show
                 (when (string= subcommand "update")
                   (setq update-called t)
                   (setq update-args (cons subcommand args)))
                 ;; Return updated issue data
                 beads-show-test--full-issue)))
      (beads-show-test-with-temp-buffer
       (setq beads-show--issue-id "bd-42")
       (setq beads-show--issue-data (beads--parse-issue
                                     beads-show-test--full-issue))
       (beads-show--update-field "Description" "-d" "New description text")
       (should update-called)
       (should (member "update" update-args))
       (should (member "bd-42" update-args))
       (should (member "-d" update-args))
       (should (member "New description text" update-args))))))

(provide 'beads-show-test)
;;; beads-show-test.el ends here
