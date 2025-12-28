;;; beads-epic-status-test.el --- Tests for beads-epic-status -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-epic-status.el.
;; Tests cover epic status parsing, rendering, expand/collapse,
;; navigation, and buffer management.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-types)
(require 'beads-epic-status)

;;; Test Fixtures

(defvar beads-epic-status-test--sample-epic-1
  '((epic .
     ((id . "beads.el-7bea")
      (title . "Update beads.el for beads 0.21.5 compatibility")
      (status . "in_progress")
      (priority . 0)
      (issue_type . "epic")))
    (total_children . 17)
    (closed_children . 3)
    (eligible_for_close . :json-false))
  "Sample epic status data for testing.")

(defvar beads-epic-status-test--sample-epic-2
  '((epic .
     ((id . "beads.el-1")
      (title . "beads.el: Magit-like Emacs interface for Beads")
      (status . "open")
      (priority . 1)
      (issue_type . "epic")))
    (total_children . 11)
    (closed_children . 3)
    (eligible_for_close . :json-false))
  "Second sample epic status for testing.")

(defvar beads-epic-status-test--eligible-epic
  '((epic .
     ((id . "beads.el-completed")
      (title . "Completed epic")
      (status . "open")
      (priority . 1)
      (issue_type . "epic")))
    (total_children . 5)
    (closed_children . 5)
    (eligible_for_close . t))
  "Epic eligible for closure.")

(defvar beads-epic-status-test--sample-child
  '((id . "beads.el-abc")
    (title . "Sample child issue")
    (status . "closed")
    (priority . 2)
    (issue_type . "task"))
  "Sample child issue for testing.")

(defvar beads-epic-status-test--sample-child-2
  '((id . "beads.el-def")
    (title . "Another child issue")
    (status . "in_progress")
    (priority . 1)
    (issue_type . "feature"))
  "Second sample child issue for testing.")

;;; Test Utilities

(defun beads-epic-status-test--mock-process-file (exit-code output)
  "Create a mock for `process-file' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile buffer display &rest args)
    (when buffer
      (let ((buf (if (listp buffer) (car buffer) buffer)))
        (when buf
          (with-current-buffer (if (bufferp buf) buf (current-buffer))
            (insert output)))))
    exit-code))

;;; Tests for Epic Status Parsing

(ert-deftest beads-epic-status-test-parse-single-epic ()
  "Test parsing single epic status from JSON."
  (let* ((epics (list beads-epic-status-test--sample-epic-1))
         (parsed (beads--parse-epic-statuses epics)))
    (should (= (length parsed) 1))
    (let ((epic-status (car parsed)))
      (should (beads-epic-status-p epic-status))
      (should (equal (oref (oref epic-status epic) id) "beads.el-7bea"))
      (should (= (oref epic-status total-children) 17))
      (should (= (oref epic-status closed-children) 3))
      (should (not (oref epic-status eligible-for-close))))))

(ert-deftest beads-epic-status-test-parse-multiple-epics ()
  "Test parsing multiple epic statuses."
  (let* ((epics (list beads-epic-status-test--sample-epic-1
                      beads-epic-status-test--sample-epic-2))
         (parsed (beads--parse-epic-statuses epics)))
    (should (= (length parsed) 2))
    (should (equal (oref (oref (nth 0 parsed) epic) id) "beads.el-7bea"))
    (should (equal (oref (oref (nth 1 parsed) epic) id) "beads.el-1"))))

(ert-deftest beads-epic-status-test-parse-eligible-epic ()
  "Test parsing epic eligible for closure."
  (let* ((epics (list beads-epic-status-test--eligible-epic))
         (parsed (beads--parse-epic-statuses epics)))
    (should (= (length parsed) 1))
    (let ((epic-status (car parsed)))
      (should (oref epic-status eligible-for-close))
      (should (= (oref epic-status total-children) 5))
      (should (= (oref epic-status closed-children) 5)))))

(ert-deftest beads-epic-status-test-parse-empty-list ()
  "Test parsing empty epic list."
  (let ((parsed (beads--parse-epic-statuses '())))
    (should (listp parsed))
    (should (= (length parsed) 0))))

;;; Tests for Rendering

(ert-deftest beads-epic-status-test-render-basic ()
  "Test basic rendering of epic status."
  (with-temp-buffer
    (let ((beads-epic-status--epics
           (list (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-1))))
      (beads-epic-status--render)
      (let ((content (buffer-string)))
        (should (string-match-p "Beads Epic Status" content))
        (should (string-match-p "beads.el-7bea" content))
        (should (string-match-p "Update beads.el for beads 0.21.5" content))
        (should (string-match-p "Progress: 3/17" content))))))

(ert-deftest beads-epic-status-test-render-multiple-epics ()
  "Test rendering multiple epics."
  (with-temp-buffer
    (let ((beads-epic-status--epics
           (list (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-1)
                 (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-2))))
      (beads-epic-status--render)
      (let ((content (buffer-string)))
        (should (string-match-p "beads.el-7bea" content))
        (should (string-match-p "beads.el-1" content))
        (should (string-match-p "3/17" content))
        (should (string-match-p "3/11" content))))))

(ert-deftest beads-epic-status-test-render-eligible-epic ()
  "Test rendering epic eligible for closure."
  (with-temp-buffer
    (let ((beads-epic-status--epics
           (list (beads-epic-status-from-json
                  beads-epic-status-test--eligible-epic))))
      (beads-epic-status--render)
      (let ((content (buffer-string)))
        (should (string-match-p "✓" content))
        (should (string-match-p "Eligible for closure" content))
        (should (string-match-p "5/5" content))))))

(ert-deftest beads-epic-status-test-render-empty-state ()
  "Test rendering with no epics."
  (with-temp-buffer
    (let ((beads-epic-status--epics nil))
      (beads-epic-status--render)
      (let ((content (buffer-string)))
        (should (string-match-p "No epics found" content))))))

(ert-deftest beads-epic-status-test-render-child-issue ()
  "Test rendering a child issue."
  (with-temp-buffer
    (let ((child (beads-issue-from-json
                  beads-epic-status-test--sample-child)))
      (beads-epic-status--render-child child)
      (let ((content (buffer-string)))
        (should (string-match-p "beads.el-abc" content))
        (should (string-match-p "\\[P2\\]" content))
        (should (string-match-p "\\[task\\]" content))
        (should (string-match-p "closed" content))
        (should (string-match-p "Sample child issue" content))))))

(ert-deftest beads-epic-status-test-render-progress-percentage ()
  "Test progress percentage calculation and rendering."
  (with-temp-buffer
    (let ((beads-epic-status--epics
           (list (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-1))))
      (beads-epic-status--render)
      (let ((content (buffer-string)))
        ;; 3/17 = 17%
        (should (string-match-p "(17%)" content))))))

;;; Tests for Text Properties

(ert-deftest beads-epic-status-test-epic-id-property ()
  "Test that epic-id text property is set correctly."
  (with-temp-buffer
    (let ((beads-epic-status--epics
           (list (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-1))))
      (beads-epic-status--render)
      ;; Find the epic line
      (goto-char (point-min))
      (search-forward "beads.el-7bea")
      (beginning-of-line)
      (should (equal (get-text-property (point) 'epic-id)
                     "beads.el-7bea")))))

;;; Tests for Navigation

(ert-deftest beads-epic-status-test-next-epic ()
  "Test navigation to next epic."
  (with-temp-buffer
    (beads-epic-status-mode)
    (let ((beads-epic-status--epics
           (list (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-1)
                 (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-2))))
      (beads-epic-status--render)
      ;; Go to first epic
      (goto-char (point-min))
      (search-forward "beads.el-7bea")
      (beginning-of-line)
      ;; Move to next epic
      (beads-epic-status-next)
      ;; Should be at second epic
      (should (get-text-property (point) 'epic-id))
      (let ((line-text (buffer-substring (line-beginning-position)
                                         (line-end-position))))
        (should (string-match-p "beads.el-1" line-text))))))

(ert-deftest beads-epic-status-test-previous-epic ()
  "Test navigation to previous epic."
  (with-temp-buffer
    (beads-epic-status-mode)
    (let ((beads-epic-status--epics
           (list (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-1)
                 (beads-epic-status-from-json
                  beads-epic-status-test--sample-epic-2))))
      (beads-epic-status--render)
      ;; Go to second epic
      (goto-char (point-min))
      (search-forward "beads.el-1")
      (beginning-of-line)
      ;; Move to previous epic
      (beads-epic-status-previous)
      ;; Should be at first epic
      (should (get-text-property (point) 'epic-id))
      (let ((line-text (buffer-substring (line-beginning-position)
                                         (line-end-position))))
        (should (string-match-p "beads.el-7bea" line-text))))))

;;; Tests for Expand/Collapse

(ert-deftest beads-epic-status-test-toggle-expand-initialization ()
  "Test that expanded state is initialized for all epics."
  (let* ((json-output (json-encode
                       (list beads-epic-status-test--sample-epic-1)))
         (beads-list-called nil))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        ;; Check that expanded state was initialized
        (should beads-epic-status--expanded)
        ;; Should have entry for the epic
        (should (assoc "beads.el-7bea" beads-epic-status--expanded)))
      (kill-buffer "*beads-epic-status*"))))

(ert-deftest beads-epic-status-test-fetch-children ()
  "Test fetching children for an epic."
  (let* ((epic-node `((id . "beads.el-7bea")
                      (title . "Epic")
                      (status . "open")
                      (priority . 1)
                      (issue_type . "epic")
                      (depth . 0)
                      (parent_id . "beads.el-7bea")))
         (child1 (append beads-epic-status-test--sample-child
                         '((depth . 1) (parent_id . "beads.el-7bea"))))
         (child2 (append beads-epic-status-test--sample-child-2
                         '((depth . 1) (parent_id . "beads.el-7bea"))))
         (tree-nodes (list epic-node child1 child2))
         (json-output (json-encode tree-nodes)))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (let ((result (beads-epic-status--fetch-children "beads.el-7bea")))
        (should (= (length result) 2))
        (should (beads-issue-p (car result)))
        (should (equal (oref (car result) id) "beads.el-abc"))))))

;;; Tests for Mode

(ert-deftest beads-epic-status-test-mode-defined ()
  "Test that beads-epic-status-mode is defined."
  (with-temp-buffer
    (beads-epic-status-mode)
    (should (eq major-mode 'beads-epic-status-mode))))

(ert-deftest beads-epic-status-test-mode-read-only ()
  "Test that beads-epic-status-mode buffer is read-only."
  (with-temp-buffer
    (beads-epic-status-mode)
    (should buffer-read-only)))

(ert-deftest beads-epic-status-test-mode-truncate-lines ()
  "Test that beads-epic-status-mode truncates lines."
  (with-temp-buffer
    (beads-epic-status-mode)
    (should truncate-lines)))

(ert-deftest beads-epic-status-test-mode-keymap ()
  "Test that beads-epic-status-mode keymap is set."
  (with-temp-buffer
    (beads-epic-status-mode)
    (should (keymapp (current-local-map)))))

(ert-deftest beads-epic-status-test-mode-keybindings ()
  "Test that key bindings are set correctly."
  (with-temp-buffer
    (beads-epic-status-mode)
    (should (eq (lookup-key (current-local-map) (kbd "SPC"))
               'beads-epic-status-toggle-expand))
    (should (eq (lookup-key (current-local-map) (kbd "TAB"))
               'beads-epic-status-next-item))
    (should (eq (lookup-key (current-local-map) (kbd "S-TAB"))
               'beads-epic-status-previous-item))
    (should (eq (lookup-key (current-local-map) (kbd "<backtab>"))
               'beads-epic-status-previous-item))
    (should (eq (lookup-key (current-local-map) (kbd "RET"))
               'beads-epic-status-show-at-point))
    (should (eq (lookup-key (current-local-map) (kbd "n"))
               'beads-epic-status-next))
    (should (eq (lookup-key (current-local-map) (kbd "p"))
               'beads-epic-status-previous))
    (should (eq (lookup-key (current-local-map) (kbd "g"))
               'beads-epic-status-refresh))
    (should (eq (lookup-key (current-local-map) (kbd "q"))
               'quit-window))))

;;; Tests for Main Command

(ert-deftest beads-epic-status-test-command-creates-buffer ()
  "Test that beads-epic creates a buffer."
  (let ((json-output (json-encode
                      (list beads-epic-status-test--sample-epic-1))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (should (get-buffer "*beads-epic-status*"))
      (kill-buffer "*beads-epic-status*"))))

(ert-deftest beads-epic-status-test-command-sets-mode ()
  "Test that beads-epic sets the correct mode."
  (let ((json-output (json-encode
                      (list beads-epic-status-test--sample-epic-1))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        (should (eq major-mode 'beads-epic-status-mode)))
      (kill-buffer "*beads-epic-status*"))))

(ert-deftest beads-epic-status-test-command-displays-content ()
  "Test that beads-epic displays epic content."
  (let ((json-output (json-encode
                      (list beads-epic-status-test--sample-epic-1))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        (let ((content (buffer-string)))
          (should (string-match-p "beads.el-7bea" content))
          (should (string-match-p "3/17" content))))
      (kill-buffer "*beads-epic-status*"))))

(ert-deftest beads-epic-status-test-command-empty-result ()
  "Test beads-epic with no epics."
  (let ((json-output (json-encode '())))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        (let ((content (buffer-string)))
          (should (string-match-p "No epics found" content))))
      (kill-buffer "*beads-epic-status*"))))

;;; Tests for Refresh

(ert-deftest beads-epic-status-test-refresh-updates-buffer ()
  "Test that beads-epic-status-refresh updates the buffer."
  (let ((json-output (json-encode
                      (list beads-epic-status-test--sample-epic-1))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        ;; Modify buffer to test refresh
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert "TEST"))
        ;; Refresh
        (beads-epic-status-refresh)
        (let ((content (buffer-string)))
          (should-not (string-match-p "TEST" content))
          (should (string-match-p "beads.el-7bea" content))))
      (kill-buffer "*beads-epic-status*"))))

(ert-deftest beads-epic-status-test-refresh-preserves-expand-state ()
  "Test that refresh preserves expanded state."
  (let ((json-output (json-encode
                      (list beads-epic-status-test--sample-epic-1))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        ;; Set some expanded state
        (setq beads-epic-status--expanded
              '(("beads.el-7bea" t . nil)))
        ;; Refresh
        (beads-epic-status-refresh)
        ;; Expanded state should be preserved
        (should beads-epic-status--expanded)
        (should (assoc "beads.el-7bea" beads-epic-status--expanded)))
      (kill-buffer "*beads-epic-status*"))))

;;; Integration Tests

(ert-deftest beads-epic-status-test-full-workflow ()
  "Test complete workflow from fetching to display."
  (let ((json-output (json-encode
                      (list beads-epic-status-test--sample-epic-1
                            beads-epic-status-test--sample-epic-2))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      ;; Open epic status
      (beads-epic)
      (should (get-buffer "*beads-epic-status*"))

      ;; Verify mode
      (with-current-buffer "*beads-epic-status*"
        (should (eq major-mode 'beads-epic-status-mode))

        ;; Verify content
        (let ((content (buffer-string)))
          (should (string-match-p "beads.el-7bea" content))
          (should (string-match-p "beads.el-1" content))
          (should (string-match-p "3/17" content))
          (should (string-match-p "3/11" content)))

        ;; Test navigation
        (goto-char (point-min))
        (search-forward "beads.el-7bea")
        (beginning-of-line)
        (beads-epic-status-next)
        (should (get-text-property (point) 'epic-id))

        ;; Test refresh
        (beads-epic-status-refresh)
        (should (string-match-p "beads.el-7bea" (buffer-string))))

      ;; Cleanup
      (kill-buffer "*beads-epic-status*"))))

;;; Edge Cases

(ert-deftest beads-epic-status-test-edge-case-zero-children ()
  "Test epic with zero children."
  (let* ((zero-children-epic
          '((epic .
             ((id . "beads.el-empty")
              (title . "Empty epic")
              (status . "open")
              (priority . 2)
              (issue_type . "epic")))
            (total_children . 0)
            (closed_children . 0)
            (eligible_for_close . :json-false)))
         (json-output (json-encode (list zero-children-epic))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        (let ((content (buffer-string)))
          (should (string-match-p "beads.el-empty" content))
          (should (string-match-p "0/0" content))
          (should (string-match-p "(0%)" content))))
      (kill-buffer "*beads-epic-status*"))))

(ert-deftest beads-epic-status-test-edge-case-100-percent ()
  "Test epic with 100% completion."
  (let ((json-output (json-encode
                      (list beads-epic-status-test--eligible-epic))))
    (cl-letf (((symbol-function 'process-file)
               (beads-epic-status-test--mock-process-file 0 json-output)))
      (beads-epic)
      (with-current-buffer "*beads-epic-status*"
        (let ((content (buffer-string)))
          (should (string-match-p "5/5" content))
          (should (string-match-p "(100%)" content))
          (should (string-match-p "✓" content))))
      (kill-buffer "*beads-epic-status*"))))

(ert-deftest beads-epic-status-test-command-exists ()
  "Integration test: Verify beads-epic command exists."
  :tags '(integration)
  (should (fboundp 'beads-epic)))

(ert-deftest beads-epic-status-test-autoload ()
  "Test that beads-epic is autoloaded or already loaded."
  :tags '(integration)
  ;; Function should either be autoloaded or already loaded (compiled)
  (should (or (autoloadp (symbol-function 'beads-epic))
              (functionp 'beads-epic))))

;;; Navigation and Toggle Tests

(ert-deftest beads-epic-status-test-navigation-next-previous ()
  "Test next and previous navigation functions exist."
  (should (fboundp 'beads-epic-status-next))
  (should (fboundp 'beads-epic-status-previous))
  (should (fboundp 'beads-epic-status-next-item))
  (should (fboundp 'beads-epic-status-previous-item)))

(ert-deftest beads-epic-status-test-toggle-expand-exists ()
  "Test toggle-expand function exists."
  (should (fboundp 'beads-epic-status-toggle-expand)))

(ert-deftest beads-epic-status-test-show-at-point-exists ()
  "Test show-at-point function exists."
  (should (fboundp 'beads-epic-status-show-at-point)))

(ert-deftest beads-epic-status-test-refresh-exists ()
  "Test refresh function exists."
  (should (fboundp 'beads-epic-status-refresh)))

(ert-deftest beads-epic-status-test-show-children-exists ()
  "Test show-children function exists."
  (should (fboundp 'beads-epic-status-show-children)))

(ert-deftest beads-epic-status-test-fetch-children-exists ()
  "Test fetch-children function exists."
  (should (fboundp 'beads-epic-status--fetch-children)))

(ert-deftest beads-epic-status-test-mode-keymap-bindings ()
  "Test that mode keymap has expected bindings."
  (should (keymapp beads-epic-status-mode-map))
  (should (eq (lookup-key beads-epic-status-mode-map (kbd "n"))
              'beads-epic-status-next))
  (should (eq (lookup-key beads-epic-status-mode-map (kbd "p"))
              'beads-epic-status-previous))
  (should (eq (lookup-key beads-epic-status-mode-map (kbd "g"))
              'beads-epic-status-refresh))
  (should (eq (lookup-key beads-epic-status-mode-map (kbd "q"))
              'quit-window)))

(ert-deftest beads-epic-status-test-render-child-format ()
  "Test that render-child produces expected format."
  (let ((child (beads-issue
                :id "bd-1"
                :title "Child Issue"
                :status "open"
                :priority 2
                :issue-type "task")))
    ;; The function inserts text, so we test in a buffer
    (with-temp-buffer
      (beads-epic-status--render-child child)
      (let ((content (buffer-string)))
        (should (string-match-p "bd-1" content))
        (should (string-match-p "Child Issue" content))))))

(provide 'beads-epic-status-test)
;;; beads-epic-status-test.el ends here
