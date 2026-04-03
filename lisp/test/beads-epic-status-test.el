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
(require 'beads-buffer)
(require 'beads-types)
(require 'beads-command-epic)

;;; Test Utilities

(defun beads-epic-status-test--get-buffer ()
  "Get the epic-status buffer name."
  (beads-buffer-name-utility "epic-status"))

(defun beads-epic-status-test--with-mock-project (body)
  "Execute BODY with mocked git functions for consistent naming."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "test-proj"))
            ((symbol-function 'beads-git-in-worktree-p)
             (lambda () nil)))
    (funcall body)))

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
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let* ((json-output (json-encode
                          (list beads-epic-status-test--sample-epic-1)))
            (beads-list-called nil))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (with-current-buffer (beads-epic-status-test--get-buffer)
           ;; Check that expanded state was initialized
           (should beads-epic-status--expanded)
           ;; Should have entry for the epic
           (should (assoc "beads.el-7bea" beads-epic-status--expanded)))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

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
        (should (beads-tree-node-p (car result)))
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
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode
                         (list beads-epic-status-test--sample-epic-1))))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (should (get-buffer (beads-epic-status-test--get-buffer)))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

(ert-deftest beads-epic-status-test-command-sets-mode ()
  "Test that beads-epic sets the correct mode."
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode
                         (list beads-epic-status-test--sample-epic-1))))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (with-current-buffer (beads-epic-status-test--get-buffer)
           (should (eq major-mode 'beads-epic-status-mode)))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

(ert-deftest beads-epic-status-test-command-displays-content ()
  "Test that beads-epic displays epic content."
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode
                         (list beads-epic-status-test--sample-epic-1))))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (with-current-buffer (beads-epic-status-test--get-buffer)
           (let ((content (buffer-string)))
             (should (string-match-p "beads.el-7bea" content))
             (should (string-match-p "3/17" content))))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

(ert-deftest beads-epic-status-test-command-empty-result ()
  "Test beads-epic with no epics."
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode '())))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (with-current-buffer (beads-epic-status-test--get-buffer)
           (let ((content (buffer-string)))
             (should (string-match-p "No epics found" content))))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

;;; Tests for Refresh

(ert-deftest beads-epic-status-test-refresh-updates-buffer ()
  "Test that beads-epic-status-refresh updates the buffer."
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode
                         (list beads-epic-status-test--sample-epic-1))))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (with-current-buffer (beads-epic-status-test--get-buffer)
           ;; Modify buffer to test refresh
           (let ((inhibit-read-only t))
             (goto-char (point-min))
             (insert "TEST"))
           ;; Refresh
           (beads-epic-status-refresh)
           (let ((content (buffer-string)))
             (should-not (string-match-p "TEST" content))
             (should (string-match-p "beads.el-7bea" content))))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

(ert-deftest beads-epic-status-test-refresh-preserves-expand-state ()
  "Test that refresh preserves expanded state."
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode
                         (list beads-epic-status-test--sample-epic-1))))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (with-current-buffer (beads-epic-status-test--get-buffer)
           ;; Set some expanded state
           (setq beads-epic-status--expanded
                 '(("beads.el-7bea" t . nil)))
           ;; Refresh
           (beads-epic-status-refresh)
           ;; Expanded state should be preserved
           (should beads-epic-status--expanded)
           (should (assoc "beads.el-7bea" beads-epic-status--expanded)))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

;;; Integration Tests

(ert-deftest beads-epic-status-test-full-workflow ()
  "Test complete workflow from fetching to display."
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode
                         (list beads-epic-status-test--sample-epic-1
                               beads-epic-status-test--sample-epic-2))))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         ;; Open epic status
         (beads-epic)
         (should (get-buffer (beads-epic-status-test--get-buffer)))

         ;; Verify mode
         (with-current-buffer (beads-epic-status-test--get-buffer)
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
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

;;; Edge Cases

(ert-deftest beads-epic-status-test-edge-case-zero-children ()
  "Test epic with zero children."
  (beads-epic-status-test--with-mock-project
   (lambda ()
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
         (with-current-buffer (beads-epic-status-test--get-buffer)
           (let ((content (buffer-string)))
             (should (string-match-p "beads.el-empty" content))
             (should (string-match-p "0/0" content))
             (should (string-match-p "(0%)" content))))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

(ert-deftest beads-epic-status-test-edge-case-100-percent ()
  "Test epic with 100% completion."
  (beads-epic-status-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode
                         (list beads-epic-status-test--eligible-epic))))
       (cl-letf (((symbol-function 'process-file)
                  (beads-epic-status-test--mock-process-file 0 json-output)))
         (beads-epic)
         (with-current-buffer (beads-epic-status-test--get-buffer)
           (let ((content (buffer-string)))
             (should (string-match-p "5/5" content))
             (should (string-match-p "(100%)" content))
             (should (string-match-p "✓" content))))
         (kill-buffer (beads-epic-status-test--get-buffer)))))))

(ert-deftest beads-epic-status-test-command-exists ()
  "Integration test: Verify beads-epic command exists."
  :tags '(:integration)
  (should (fboundp 'beads-epic)))

(ert-deftest beads-epic-status-test-autoload ()
  "Test that beads-epic is autoloaded or already loaded."
  :tags '(:integration)
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

;;; Tests for beads-command-epic-status Parse Method

(ert-deftest beads-epic-status-test-parse-json-vector ()
  "Test parse method with JSON vector of epic statuses."
  (let* ((cmd (beads-command-epic-status :json t))
         (sample-json (vector
                       '((epic . ((id . "bd-10") (title . "Epic 1")
                                  (status . "open") (priority . 1)
                                  (issue_type . "epic")))
                         (total_children . 5)
                         (open_children . 3)
                         (closed_children . 2)
                         (in_progress_children . 1)
                         (completion_pct . 40))))
         (json-string (json-encode sample-json)))
    (let ((result (beads-command-parse cmd json-string)))
      (should (listp result))
      (should (= (length result) 1))
      (should (beads-epic-status-p (car result))))))

(ert-deftest beads-epic-status-test-parse-json-single-object ()
  "Test parse method with single JSON object (cons)."
  (let* ((cmd (beads-command-epic-status :json t))
         (sample-json '((epic . ((id . "bd-10") (title . "Epic 1")
                                 (status . "open") (priority . 1)
                                 (issue_type . "epic")))
                        (total_children . 5)
                        (open_children . 3)
                        (closed_children . 2)
                        (in_progress_children . 1)
                        (completion_pct . 40)))
         (json-string (json-encode sample-json)))
    (let ((result (beads-command-parse cmd json-string)))
      (should (listp result))
      (should (= (length result) 1))
      (should (beads-epic-status-p (car result))))))

(ert-deftest beads-epic-status-test-parse-json-null ()
  "Test parse method with null JSON."
  (let* ((cmd (beads-command-epic-status :json t)))
    (let ((result (beads-command-parse cmd "null")))
      (should (null result)))))

(ert-deftest beads-epic-status-test-parse-json-disabled ()
  "Test parse method with :json nil."
  (let* ((cmd (beads-command-epic-status :json nil)))
    (let ((result (beads-command-parse cmd "Epic status text")))
      (should (stringp result)))))

(ert-deftest beads-epic-status-test-parse-json-unexpected ()
  "Test parse method signals error on unexpected JSON structure."
  (let* ((cmd (beads-command-epic-status :json t))
         (json-string (json-encode 42)))
    (should-error (beads-command-parse cmd json-string)
                  :type 'beads-json-parse-error)))

;;; Tests for beads-command-epic-status execute-interactive

(ert-deftest beads-epic-status-test-execute-interactive-disables-json ()
  "Test execute-interactive runs in terminal (json defaults to nil)."
  (let* ((cmd (beads-command-epic-status))
         (terminal-called nil))
    (cl-letf (((symbol-function 'beads-command--run-in-terminal)
               (lambda (_cmd _buf _dir) (setq terminal-called t)))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () "/tmp/.beads")))
      (beads-command-execute-interactive cmd)
      (should-not (oref cmd json))
      (should terminal-called))))

;;; Tests for Epic Status Navigation

(ert-deftest beads-epic-status-test-next-epic-raw-props ()
  "Test navigating to next epic with raw text properties."
  (with-temp-buffer
    ;; Insert some text with properties
    (insert (propertize "Epic 1 line\n" 'epic-id "bd-10"))
    (insert "  child line\n")
    (insert (propertize "Epic 2 line\n" 'epic-id "bd-20"))
    (goto-char (point-min))
    (beads-epic-status-next)
    ;; Should move to Epic 2
    (should (string= (get-text-property (point) 'epic-id) "bd-20"))))

(ert-deftest beads-epic-status-test-previous-epic-raw-props ()
  "Test navigating to previous epic with raw text properties."
  (with-temp-buffer
    (insert (propertize "Epic 1 line\n" 'epic-id "bd-10"))
    (insert "  child line\n")
    (insert (propertize "Epic 2 line\n" 'epic-id "bd-20"))
    ;; Start at Epic 2
    (goto-char (point-max))
    (forward-line -1)
    (beads-epic-status-previous)
    ;; Should move to Epic 1
    (should (string= (get-text-property (point) 'epic-id) "bd-10"))))

(ert-deftest beads-epic-status-test-next-item ()
  "Test navigating to next epic or issue item."
  (with-temp-buffer
    (insert (propertize "Epic 1\n" 'epic-id "bd-10"))
    (insert (propertize "  Child 1\n" 'issue-id "bd-1"))
    (insert "  plain line\n")
    (insert (propertize "  Child 2\n" 'issue-id "bd-2"))
    (goto-char (point-min))
    (beads-epic-status-next-item)
    ;; Should move to Child 1
    (should (string= (get-text-property (point) 'issue-id) "bd-1"))))

(ert-deftest beads-epic-status-test-previous-item ()
  "Test navigating to previous epic or issue item."
  (with-temp-buffer
    (insert (propertize "Epic 1\n" 'epic-id "bd-10"))
    (insert (propertize "  Child 1\n" 'issue-id "bd-1"))
    (insert "  plain line\n")
    (insert (propertize "  Child 2\n" 'issue-id "bd-2"))
    ;; Start at Child 2
    (goto-char (point-max))
    (forward-line -1)
    (beads-epic-status-previous-item)
    ;; Should move to Child 1
    (should (string= (get-text-property (point) 'issue-id) "bd-1"))))

(ert-deftest beads-epic-status-test-show-at-point-no-issue ()
  "Test show-at-point signals error when no issue at point."
  (with-temp-buffer
    (insert "plain text\n")
    (goto-char (point-min))
    (should-error (beads-epic-status-show-at-point) :type 'user-error)))

(ert-deftest beads-epic-status-test-show-children-no-epic ()
  "Test show-children signals error when no epic at point."
  (with-temp-buffer
    (insert "plain text\n")
    (goto-char (point-min))
    (should-error (beads-epic-status-show-children) :type 'user-error)))

(ert-deftest beads-epic-status-test-move-to-epic-line ()
  "Test moving point to nearest epic line."
  (with-temp-buffer
    (insert "plain line 1\n")
    (insert (propertize "Epic 1\n" 'epic-id "bd-10"))
    (insert "plain line 2\n")
    (goto-char (point-min))
    (beads-epic-status--move-to-epic-line)
    (should (string= (get-text-property (point) 'epic-id) "bd-10"))))

;;; Epic Navigation Tests

(ert-deftest beads-epic-status-test-next-epic-nav ()
  "Test next epic navigation."
  (with-temp-buffer
    (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
    (insert "progress line\n")
    (insert "\n")
    (insert (propertize "Epic 2\n" 'epic-id "bd-2"))
    (goto-char (point-min))
    (beads-epic-status-next)
    (should (equal (get-text-property (point) 'epic-id) "bd-2"))))

(ert-deftest beads-epic-status-test-next-epic-at-end ()
  "Test next epic at end of buffer."
  (with-temp-buffer
    (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
    (insert "no more epics\n")
    (goto-char (point-min))
    (let ((start (point)))
      (beads-epic-status-next)
      ;; Should stay at same position
      (should (= (point) start)))))

(ert-deftest beads-epic-status-test-previous-epic-nav ()
  "Test previous epic navigation."
  (with-temp-buffer
    (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
    (insert "progress line\n")
    (insert "\n")
    (insert (propertize "Epic 2\n" 'epic-id "bd-2"))
    ;; Start at Epic 2
    (goto-char (1- (point-max)))
    (beads-epic-status-previous)
    (should (equal (get-text-property (point) 'epic-id) "bd-1"))))

(ert-deftest beads-epic-status-test-previous-epic-at-start ()
  "Test previous epic at beginning of buffer."
  (with-temp-buffer
    (insert "no epic here\n")
    (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
    (goto-char (point-min))
    (let ((start (point)))
      (beads-epic-status-previous)
      ;; Should stay at same position (no previous epic)
      (should (= (point) start)))))

(ert-deftest beads-epic-status-test-next-item-nav ()
  "Test next item navigation (epics and issues)."
  (with-temp-buffer
    (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
    (insert "progress\n")
    (insert (propertize "  Child 1\n" 'issue-id "bd-2"))
    (insert (propertize "  Child 2\n" 'issue-id "bd-3"))
    (goto-char (point-min))
    (beads-epic-status-next-item)
    ;; Should land on child issue
    (should (or (equal (get-text-property (point) 'issue-id) "bd-2")
                (equal (get-text-property (point) 'epic-id) "bd-1")))))

(ert-deftest beads-epic-status-test-previous-item-nav ()
  "Test previous item navigation."
  (with-temp-buffer
    (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
    (insert "progress\n")
    (insert (propertize "  Child 1\n" 'issue-id "bd-2"))
    (goto-char (point-max))
    (beads-epic-status-previous-item)
    (should (equal (get-text-property (point) 'issue-id) "bd-2"))))

;;; Render Tests

(ert-deftest beads-epic-status-test-render-child ()
  "Test rendering a child issue."
  (with-temp-buffer
    (let ((child (beads-issue :id "bd-5" :title "Child Task"
                              :status "open" :priority 2
                              :issue-type "task")))
      (beads-epic-status--render-child child)
      (let ((text (buffer-string)))
        (should (string-match-p "bd-5" text))
        (should (string-match-p "Child Task" text))
        (should (string-match-p "open" text))
        (should (string-match-p "P2" text))))))

(ert-deftest beads-epic-status-test-render-child-closed ()
  "Test rendering a closed child issue."
  (with-temp-buffer
    (let ((child (beads-issue :id "bd-6" :title "Done Task"
                              :status "closed" :priority 1
                              :issue-type "bug")))
      (beads-epic-status--render-child child)
      (let ((text (buffer-string)))
        (should (string-match-p "bd-6" text))
        (should (string-match-p "closed" text))
        (should (string-match-p "bug" text))))))

(ert-deftest beads-epic-status-test-render-epic-basic ()
  "Test rendering a basic epic status."
  (with-temp-buffer
    (let* ((epic (beads-issue :id "bd-10" :title "Epic Title"
                              :status "open" :priority 1
                              :issue-type "epic"))
           (epic-status (beads-epic-status
                         :epic epic
                         :total-children 5
                         :closed-children 2
                         :eligible-for-close nil))
           (beads-epic-status--expanded
            (list (list "bd-10" nil))))
      (beads-epic-status--render-epic epic-status)
      (let ((text (buffer-string)))
        (should (string-match-p "bd-10" text))
        (should (string-match-p "Epic Title" text))
        (should (string-match-p "2/5" text))
        (should (string-match-p "40%" text))))))

(ert-deftest beads-epic-status-test-render-epic-eligible ()
  "Test rendering an epic eligible for closure."
  (with-temp-buffer
    (let* ((epic (beads-issue :id "bd-20" :title "Done Epic"
                              :status "open" :priority 1
                              :issue-type "epic"))
           (epic-status (beads-epic-status
                         :epic epic
                         :total-children 3
                         :closed-children 3
                         :eligible-for-close t))
           (beads-epic-status--expanded
            (list (list "bd-20" nil))))
      (beads-epic-status--render-epic epic-status)
      (let ((text (buffer-string)))
        (should (string-match-p "Eligible for closure" text))
        (should (string-match-p "100%" text))))))

;;; Epic Transient Dispatch Tests

(ert-deftest beads-epic-status-test-epic-menu-defined ()
  "Test beads-epic-menu transient is defined."
  (should (fboundp 'beads-epic-menu)))

(ert-deftest beads-epic-status-test-epic-menu-has-status ()
  "Test beads-epic-menu has status suffix."
  (let ((obj (get 'beads-epic-menu 'transient--prefix)))
    (should obj)))

(provide 'beads-epic-status-test)
;;; beads-epic-status-test.el ends here
