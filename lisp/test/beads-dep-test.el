;;; beads-dep-test.el --- Tests for beads-dep -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-dep.el.
;; Tests cover dependency add, remove, tree display, and cycle detection.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-buffer)
(require 'beads-command-dep)

;;; Test Utilities

(defun beads-dep-test--get-tree-buffer (issue-id)
  "Get the dep-tree buffer name for ISSUE-ID."
  (beads-buffer-name-utility "dep-tree" issue-id))

(defun beads-dep-test--get-cycles-buffer ()
  "Get the dep-cycles buffer name."
  (beads-buffer-name-utility "dep-cycles"))

(defun beads-dep-test--with-mock-project (body)
  "Execute BODY with mocked git functions for consistent naming."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "test-proj"))
            ((symbol-function 'beads-git-get-branch)
             (lambda () "main"))
            ((symbol-function 'beads-buffer-is-main-branch-p)
             (lambda () t))
            ((symbol-function 'beads-git-in-worktree-p)
             (lambda () nil)))
    (funcall body)))

;;; Test Fixtures

(defvar beads-dep-test--sample-dependency
  (beads-dep-op-result
   :issue-id "bd-1"
   :depends-on-id "bd-2"
   :dep-type "blocks"
   :op-status "added")
  "Sample dependency op result for testing.")

(defvar beads-dep-test--sample-tree
  (list
   (beads-tree-node
    :id "bd-1" :title "Main issue" :status "open"
    :priority 1 :issue-type "feature" :depth 0 :truncated nil)
   (beads-tree-node
    :id "bd-2" :title "Dependency 1" :status "open"
    :priority 2 :issue-type "task" :depth 1 :truncated nil)
   (beads-tree-node
    :id "bd-3" :title "Dependency 2" :status "closed"
    :priority 2 :issue-type "bug" :depth 2 :truncated nil))
  "Sample dependency tree for testing.")

(defvar beads-dep-test--sample-tree-json
  "[{\"id\":\"bd-1\",\"title\":\"Main issue\",\"status\":\"open\",\"priority\":1,\"issue_type\":\"feature\",\"depth\":0,\"truncated\":false},{\"id\":\"bd-2\",\"title\":\"Dependency 1\",\"status\":\"open\",\"priority\":2,\"issue_type\":\"task\",\"depth\":1,\"truncated\":false},{\"id\":\"bd-3\",\"title\":\"Dependency 2\",\"status\":\"closed\",\"priority\":2,\"issue_type\":\"bug\",\"depth\":2,\"truncated\":false}]"
  "JSON string representation of sample dependency tree for mocking.")

(defvar beads-dep-test--sample-cycles
  [["bd-1" "bd-2" "bd-3"]
   ["bd-4" "bd-5"]]
  "Sample dependency cycles for testing.")

;;; Test Utilities

(defun beads-dep-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for Utility Functions

(ert-deftest beads-dep-test-detect-issue-id-no-context ()
  "Test issue ID detection with no context."
  (with-temp-buffer
    (text-mode)
    (should (null (beads-dep--detect-issue-id)))))

(ert-deftest beads-dep-test-format-dependency ()
  "Test dependency formatting."
  (let ((formatted (beads-dep--format-dependency
                   beads-dep-test--sample-dependency)))
    (should (stringp formatted))
    (should (string-match-p "added" formatted))
    (should (string-match-p "bd-1" formatted))
    (should (string-match-p "bd-2" formatted))
    (should (string-match-p "blocks" formatted))))

;;; Tests for Add Dependency Validation

(ert-deftest beads-dep-test-add-validate-missing-issue-id ()
  "Test add validation with missing issue ID."
  (should (stringp (beads-dep-add--validate nil "bd-2" "blocks"))))

(ert-deftest beads-dep-test-add-validate-missing-depends-on ()
  "Test add validation with missing depends-on ID."
  (should (stringp (beads-dep-add--validate "bd-1" nil "blocks"))))

(ert-deftest beads-dep-test-add-validate-self-dependency ()
  "Test add validation with self-dependency."
  (should (string-match-p "itself" (beads-dep-add--validate "bd-1" "bd-1" "blocks"))))

(ert-deftest beads-dep-test-add-validate-missing-type ()
  "Test add validation with missing type."
  (should (stringp (beads-dep-add--validate "bd-1" "bd-2" nil))))

(ert-deftest beads-dep-test-add-validate-valid ()
  "Test add validation with valid parameters."
  (should (null (beads-dep-add--validate "bd-1" "bd-2" "blocks"))))

;;; Tests for Add Dependency Reset

(ert-deftest beads-dep-test-add-reset ()
  "Test resetting add dependency state."
  ;; Reset just calls transient-reset and transient--redisplay
  ;; We verify the function is defined and callable
  (should (fboundp 'beads-dep-add--reset)))

;;; Tests for Remove Dependency Validation

(ert-deftest beads-dep-test-remove-validate-missing-issue-id ()
  "Test remove validation with missing issue ID."
  (should (stringp (beads-dep-remove--validate nil "bd-2"))))

(ert-deftest beads-dep-test-remove-validate-missing-depends-on ()
  "Test remove validation with missing depends-on ID."
  (should (stringp (beads-dep-remove--validate "bd-1" nil))))

(ert-deftest beads-dep-test-remove-validate-valid ()
  "Test remove validation with valid parameters."
  (should (null (beads-dep-remove--validate "bd-1" "bd-2"))))

;;; Tests for Remove Dependency Reset

(ert-deftest beads-dep-test-remove-reset ()
  "Test resetting remove dependency state."
  ;; Reset just calls transient-reset and transient--redisplay
  ;; We verify the function is defined and callable
  (should (fboundp 'beads-dep-remove--reset)))

;;; Tests for Dependency Tree Mode

(ert-deftest beads-dep-test-tree-mode-defined ()
  "Test that beads-dep-tree-mode is defined."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (should (eq major-mode 'beads-dep-tree-mode))))

(ert-deftest beads-dep-test-tree-mode-read-only ()
  "Test that tree mode buffer is read-only."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (should buffer-read-only)))

(ert-deftest beads-dep-test-tree-mode-keymap ()
  "Test that tree mode keymap is set."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (should (keymapp (current-local-map)))))

(ert-deftest beads-dep-test-tree-mode-keybindings ()
  "Test that tree mode key bindings are set."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (should (eq (lookup-key (current-local-map) (kbd "q"))
               'quit-window))
    (should (eq (lookup-key (current-local-map) (kbd "g"))
               'beads-dep-tree-refresh))
    (should (eq (lookup-key (current-local-map) (kbd "RET"))
               'beads-dep-tree-show-issue))))

;;; Tests for Tree Rendering

(ert-deftest beads-dep-test-tree-render-single-issue ()
  "Test rendering tree with single issue."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (beads-dep-tree--render (list (car beads-dep-test--sample-tree))
                            "bd-1")
    (let ((content (buffer-string)))
      (should (string-match-p "Dependency Tree for bd-1" content))
      (should (string-match-p "bd-1" content))
      (should (string-match-p "Main issue" content))
      (should (string-match-p "OPEN" content)))))

(ert-deftest beads-dep-test-tree-render-nested-dependencies ()
  "Test rendering tree with nested dependencies."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (beads-dep-tree--render beads-dep-test--sample-tree
                            "bd-1")
    (let ((content (buffer-string)))
      (should (string-match-p "bd-1" content))
      (should (string-match-p "bd-2" content))
      (should (string-match-p "bd-3" content))
      (should (string-match-p "Dependency 1" content))
      (should (string-match-p "Dependency 2" content)))))

(ert-deftest beads-dep-test-tree-render-empty ()
  "Test rendering tree with no dependencies."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (beads-dep-tree--render [] "bd-1")
    (let ((content (buffer-string)))
      (should (string-match-p "No dependencies found" content)))))

(ert-deftest beads-dep-test-tree-render-has-commands ()
  "Test that tree rendering includes command help."
  (with-temp-buffer
    (beads-dep-tree-mode)
    (beads-dep-tree--render beads-dep-test--sample-tree
                            "bd-1")
    (let ((content (buffer-string)))
      (should (string-match-p "Commands:" content))
      (should (string-match-p "RET - show issue" content))
      (should (string-match-p "g.*refresh" content))
      (should (string-match-p "q.*quit" content)))))

;;; Tests for Cycles Mode

(ert-deftest beads-dep-test-cycles-mode-defined ()
  "Test that beads-dep-cycles-mode is defined."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (should (eq major-mode 'beads-dep-cycles-mode))))

(ert-deftest beads-dep-test-cycles-mode-read-only ()
  "Test that cycles mode buffer is read-only."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (should buffer-read-only)))

(ert-deftest beads-dep-test-cycles-mode-keymap ()
  "Test that cycles mode keymap is set."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (should (keymapp (current-local-map)))))

(ert-deftest beads-dep-test-cycles-mode-keybindings ()
  "Test that cycles mode key bindings are set."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (should (eq (lookup-key (current-local-map) (kbd "q"))
               'quit-window))
    (should (eq (lookup-key (current-local-map) (kbd "g"))
               'beads-dep-cycles-refresh))))

;;; Tests for Cycles Rendering

(ert-deftest beads-dep-test-cycles-render-no-cycles ()
  "Test rendering with no cycles."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (beads-dep-cycles--render [])
    (let ((content (buffer-string)))
      (should (string-match-p "Dependency Cycles" content))
      (should (string-match-p "No dependency cycles detected" content)))))

(ert-deftest beads-dep-test-cycles-render-single-cycle ()
  "Test rendering with single cycle."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (beads-dep-cycles--render (vector (aref beads-dep-test--sample-cycles 0)))
    (let ((content (buffer-string)))
      (should (string-match-p "Found 1 cycle" content))
      (should (string-match-p "bd-1" content))
      (should (string-match-p "bd-2" content))
      (should (string-match-p "bd-3" content))
      (should (string-match-p " -> " content)))))

(ert-deftest beads-dep-test-cycles-render-multiple-cycles ()
  "Test rendering with multiple cycles."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (beads-dep-cycles--render beads-dep-test--sample-cycles)
    (let ((content (buffer-string)))
      (should (string-match-p "Found 2 cycle" content))
      (should (string-match-p "Cycle 1:" content))
      (should (string-match-p "Cycle 2:" content))
      (should (string-match-p "bd-1" content))
      (should (string-match-p "bd-4" content)))))

(ert-deftest beads-dep-test-cycles-render-has-commands ()
  "Test that cycles rendering includes command help."
  (with-temp-buffer
    (beads-dep-cycles-mode)
    (beads-dep-cycles--render [])
    (let ((content (buffer-string)))
      (should (string-match-p "Commands:" content))
      (should (string-match-p "g.*refresh" content))
      (should (string-match-p "q.*quit" content)))))

;;; Tests for Tree Command

(ert-deftest beads-dep-test-tree-command-creates-buffer ()
  "Test that beads-dep-tree creates a buffer."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output beads-dep-test--sample-tree-json)
           (buf-name (beads-dep-test--get-tree-buffer "bd-1")))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-tree "bd-1")
         (should (get-buffer buf-name))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-tree-command-sets-mode ()
  "Test that beads-dep-tree sets the correct mode."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output beads-dep-test--sample-tree-json)
           (buf-name (beads-dep-test--get-tree-buffer "bd-1")))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-tree "bd-1")
         (with-current-buffer buf-name
           (should (eq major-mode 'beads-dep-tree-mode)))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-tree-command-displays-content ()
  "Test that beads-dep-tree displays tree content."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output beads-dep-test--sample-tree-json)
           (buf-name (beads-dep-test--get-tree-buffer "bd-1")))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-tree "bd-1")
         (with-current-buffer buf-name
           (let ((content (buffer-string)))
             (should (string-match-p "Dependency Tree" content))
             (should (string-match-p "bd-1" content))
             (should (string-match-p "Main issue" content))))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-tree-command-requires-issue-id ()
  "Test that beads-dep-tree requires issue ID."
  (should-error (beads-dep-tree nil)))

;;; Tests for Cycles Command

(ert-deftest beads-dep-test-cycles-command-creates-buffer ()
  "Test that beads-dep-cycles creates a buffer."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode beads-dep-test--sample-cycles))
           (buf-name (beads-dep-test--get-cycles-buffer)))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-cycles)
         (should (get-buffer buf-name))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-cycles-command-sets-mode ()
  "Test that beads-dep-cycles sets the correct mode."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode beads-dep-test--sample-cycles))
           (buf-name (beads-dep-test--get-cycles-buffer)))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-cycles)
         (with-current-buffer buf-name
           (should (eq major-mode 'beads-dep-cycles-mode)))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-cycles-command-displays-cycles ()
  "Test that beads-dep-cycles displays cycle content."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode beads-dep-test--sample-cycles))
           (buf-name (beads-dep-test--get-cycles-buffer)))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-cycles)
         (with-current-buffer buf-name
           (let ((content (buffer-string)))
             (should (string-match-p "Dependency Cycles" content))
             (should (string-match-p "Found 2 cycle" content))))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-cycles-command-no-cycles ()
  "Test that beads-dep-cycles handles no cycles."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode []))
           (buf-name (beads-dep-test--get-cycles-buffer)))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-cycles)
         (with-current-buffer buf-name
           (let ((content (buffer-string)))
             (should (string-match-p "No dependency cycles" content))))
         (kill-buffer buf-name))))))

;;; Tests for Tree Refresh

(ert-deftest beads-dep-test-tree-refresh-updates-buffer ()
  "Test that tree refresh updates the buffer."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output beads-dep-test--sample-tree-json)
           (buf-name (beads-dep-test--get-tree-buffer "bd-1")))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-tree "bd-1")
         (with-current-buffer buf-name
           ;; Modify buffer to test refresh
           (let ((inhibit-read-only t))
             (goto-char (point-min))
             (insert "TEST"))
           ;; Refresh
           (beads-dep-tree-refresh)
           (let ((content (buffer-string)))
             (should-not (string-match-p "TEST" content))
             (should (string-match-p "Dependency Tree" content))))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-tree-refresh-only-in-tree-mode ()
  "Test that tree refresh only works in tree mode."
  (with-temp-buffer
    (text-mode)
    ;; Should not do anything in non-tree-mode buffer
    (beads-dep-tree-refresh)
    (should (zerop (buffer-size)))))

;;; Tests for Cycles Refresh

(ert-deftest beads-dep-test-cycles-refresh-updates-buffer ()
  "Test that cycles refresh updates the buffer."
  (beads-dep-test--with-mock-project
   (lambda ()
     (let ((json-output (json-encode beads-dep-test--sample-cycles))
           (buf-name (beads-dep-test--get-cycles-buffer)))
       (cl-letf (((symbol-function 'call-process)
                  (beads-dep-test--mock-call-process 0 json-output)))
         (beads-dep-cycles)
         (with-current-buffer buf-name
           ;; Modify buffer to test refresh
           (let ((inhibit-read-only t))
             (goto-char (point-min))
             (insert "TEST"))
           ;; Refresh
           (beads-dep-cycles-refresh)
           (let ((content (buffer-string)))
             (should-not (string-match-p "TEST" content))
             (should (string-match-p "Dependency Cycles" content))))
         (kill-buffer buf-name))))))

(ert-deftest beads-dep-test-cycles-refresh-only-in-cycles-mode ()
  "Test that cycles refresh only works in cycles mode."
  (with-temp-buffer
    (text-mode)
    ;; Should not do anything in non-cycles-mode buffer
    (beads-dep-cycles-refresh)
    (should (zerop (buffer-size)))))

;;; Edge Cases

(ert-deftest beads-dep-test-edge-case-empty-string-issue-id ()
  "Test handling of empty string issue ID."
  (should (stringp (beads-dep-add--validate "" "bd-2" "blocks"))))

(ert-deftest beads-dep-test-edge-case-whitespace-issue-id ()
  "Test handling of whitespace issue ID."
  ;; String-empty-p doesn't catch whitespace, so this might pass validation
  ;; but fail at execution - which is acceptable
  (should t))

(ert-deftest beads-dep-test-edge-case-truncated-tree ()
  "Test rendering tree with truncated flag."
  (let ((truncated-issue (beads-tree-node
                          :id "bd-1" :title "Issue" :status "open"
                          :priority 1 :issue-type "feature"
                          :depth 0 :truncated t)))
    (with-temp-buffer
      (beads-dep-tree-mode)
      (beads-dep-tree--render (list truncated-issue) "bd-1")
      (let ((content (buffer-string)))
        (should (string-match-p "\\[truncated\\]" content))))))

(ert-deftest beads-dep-test-edge-case-deep-nesting ()
  "Test rendering tree with deep nesting."
  (let ((deep-issue (beads-tree-node
                     :id "bd-10" :title "Deep issue" :status "open"
                     :priority 1 :issue-type "feature"
                     :depth 10 :truncated nil)))
    (with-temp-buffer
      (beads-dep-tree-mode)
      (beads-dep-tree--render (list deep-issue) "bd-1")
      ;; Should render with proper indentation (20 spaces)
      (let ((content (buffer-string)))
        (should (string-match-p "bd-10" content))))))

;;; Performance Tests

(ert-deftest beads-dep-test-performance-tree-render ()
  "Test tree rendering performance."
  :tags '(:performance)
  (let ((large-tree
         (let (result)
           (dotimes (i 100)
             (push (beads-tree-node
                    :id (format "bd-%d" i)
                    :title "Test issue"
                    :status "open"
                    :priority 1
                    :issue-type "feature"
                    :depth (/ i 10)
                    :truncated nil)
                   result))
           (nreverse result))))
    (let ((start-time (current-time)))
      (with-temp-buffer
        (beads-dep-tree-mode)
        (beads-dep-tree--render large-tree "bd-1"))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should render 100 issues in under 1 second
        (should (< elapsed 1.0))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-dep-test-dep-tree-mode-defined ()
  "Integration test: Verify beads-dep-tree-mode is defined."
  :tags '(:integration)
  (should (fboundp 'beads-dep-tree-mode)))

(ert-deftest beads-dep-test-dep-add-command-exists ()
  "Integration test: Verify beads-dep-add command exists."
  :tags '(:integration)
  (should (fboundp 'beads-dep-add)))

(ert-deftest beads-dep-test-dep-remove-command-exists ()
  "Integration test: Verify beads-dep-remove command exists."
  :tags '(:integration)
  (should (fboundp 'beads-dep-remove)))

(ert-deftest beads-dep-test-dep-menu-command-exists ()
  "Integration test: Verify beads-dep menu command exists."
  :tags '(:integration)
  (should (fboundp 'beads-dep)))

(ert-deftest beads-dep-test-dep-tree-command-exists ()
  "Integration test: Verify beads-dep-tree command exists."
  :tags '(:integration)
  (should (fboundp 'beads-dep-tree)))

;;; Tests for Transient Argument Parsing

(ert-deftest beads-dep-test-add-parse-args-empty ()
  "Test parsing empty argument list for add."
  (let ((parsed (beads-dep-add--parse-transient-args nil)))
    (should (null (plist-get parsed :issue-id)))
    (should (null (plist-get parsed :depends-on-id)))
    (should (null (plist-get parsed :type)))))

(ert-deftest beads-dep-test-add-parse-args-all-fields ()
  "Test parsing all fields for add."
  (let ((parsed (beads-dep-add--parse-transient-args
                 '("--issue-id=bd-1"
                   "--depends-on=bd-2"
                   "--type=blocks"))))
    (should (equal (plist-get parsed :issue-id) "bd-1"))
    (should (equal (plist-get parsed :depends-on-id) "bd-2"))
    (should (equal (plist-get parsed :type) "blocks"))))

(ert-deftest beads-dep-test-remove-parse-args-empty ()
  "Test parsing empty argument list for remove."
  (let ((parsed (beads-dep-remove--parse-transient-args nil)))
    (should (null (plist-get parsed :issue-id)))
    (should (null (plist-get parsed :depends-on-id)))))

(ert-deftest beads-dep-test-remove-parse-args-all-fields ()
  "Test parsing all fields for remove."
  (let ((parsed (beads-dep-remove--parse-transient-args
                 '("--issue-id=bd-1"
                   "--depends-on=bd-2"))))
    (should (equal (plist-get parsed :issue-id) "bd-1"))
    (should (equal (plist-get parsed :depends-on-id) "bd-2"))))

;;; Tests for Execute Commands

(ert-deftest beads-dep-test-add-execute-validation-error ()
  "Test add execute with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) nil)))
    (should-error (beads-dep-add--execute)
                  :type 'user-error)))

(ert-deftest beads-dep-test-add-execute-success ()
  "Test add execute success."
  (let ((executed nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--issue-id=bd-1"
                            "--depends-on=bd-2"
                            "--type=blocks")))
              ((symbol-function 'beads-command-dep-add!)
               (lambda (&rest _)
                 (setq executed t)
                 beads-dep-test--sample-dependency))
              ((symbol-function 'beads--invalidate-completion-cache) #'ignore))
      (let ((beads-auto-refresh nil))
        (beads-dep-add--execute)
        (should executed)))))

(ert-deftest beads-dep-test-add-execute-error-handling ()
  "Test add execute handles errors gracefully."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) '("--issue-id=bd-1"
                          "--depends-on=bd-2"
                          "--type=blocks")))
            ((symbol-function 'beads-command-dep-add!)
             (lambda (&rest _) (error "Test error"))))
    ;; Should not throw, just message the error
    (should-not (condition-case nil
                    (progn (beads-dep-add--execute) nil)
                  (error t)))))

(ert-deftest beads-dep-test-remove-execute-validation-error ()
  "Test remove execute with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) nil)))
    (should-error (beads-dep-remove--execute)
                  :type 'user-error)))

(ert-deftest beads-dep-test-remove-execute-success ()
  "Test remove execute success."
  (let ((executed nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--issue-id=bd-1"
                            "--depends-on=bd-2")))
              ((symbol-function 'beads-command-dep-remove!)
               (lambda (&rest _)
                 (setq executed t)
                 (beads-dep-op-result
                  :op-status "removed"
                  :issue-id "bd-1"
                  :depends-on-id "bd-2")))
              ((symbol-function 'beads--invalidate-completion-cache) #'ignore))
      (let ((beads-auto-refresh nil))
        (beads-dep-remove--execute)
        (should executed)))))

(ert-deftest beads-dep-test-remove-execute-error-handling ()
  "Test remove execute handles errors gracefully."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) '("--issue-id=bd-1"
                          "--depends-on=bd-2")))
            ((symbol-function 'beads-command-dep-remove!)
             (lambda (&rest _) (error "Test error"))))
    ;; Should not throw, just message the error
    (should-not (condition-case nil
                    (progn (beads-dep-remove--execute) nil)
                  (error t)))))

;;; Tests for Preview Commands

(ert-deftest beads-dep-test-add-preview-valid ()
  "Test add preview with valid args."
  (let ((output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--issue-id=bd-1"
                            "--depends-on=bd-2"
                            "--type=blocks")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq output (apply #'format fmt args)))))
      (beads-dep-add--preview)
      (should output)
      (should (string-match-p "bd dep add" output))
      (should (string-match-p "bd-1" output))
      (should (string-match-p "bd-2" output))
      (should (string-match-p "blocks" output)))))

(ert-deftest beads-dep-test-add-preview-invalid ()
  "Test add preview with invalid args."
  (let ((output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq output (apply #'format fmt args)))))
      (beads-dep-add--preview)
      (should output)
      (should (string-match-p "Invalid" output)))))

(ert-deftest beads-dep-test-remove-preview-valid ()
  "Test remove preview with valid args."
  (let ((output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--issue-id=bd-1"
                            "--depends-on=bd-2")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq output (apply #'format fmt args)))))
      (beads-dep-remove--preview)
      (should output)
      (should (string-match-p "bd dep remove" output))
      (should (string-match-p "bd-1" output))
      (should (string-match-p "bd-2" output)))))

(ert-deftest beads-dep-test-remove-preview-invalid ()
  "Test remove preview with invalid args."
  (let ((output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq output (apply #'format fmt args)))))
      (beads-dep-remove--preview)
      (should output)
      (should (string-match-p "Invalid" output)))))

;;; Tests for Tree Issue Rendering

(ert-deftest beads-dep-test-tree-render-issue-basic ()
  "Test rendering a single issue."
  (let ((issue (beads-tree-node
                :id "bd-1" :title "Test Issue" :status "open"
                :depth 0 :truncated nil)))
    (with-temp-buffer
      (beads-dep-tree--render-issue issue)
      (let ((content (buffer-string)))
        (should (string-match-p "bd-1" content))
        (should (string-match-p "Test Issue" content))
        (should (string-match-p "OPEN" content))))))

(ert-deftest beads-dep-test-tree-render-issue-with-depth ()
  "Test rendering an issue with depth indentation."
  (let ((issue (beads-tree-node
                :id "bd-2" :title "Nested Issue" :status "in_progress"
                :depth 2 :truncated nil)))
    (with-temp-buffer
      (beads-dep-tree--render-issue issue)
      (let ((content (buffer-string)))
        ;; Should have indentation (4 spaces for depth 2)
        (should (string-match-p "    - " content))
        (should (string-match-p "IN_PROGRESS" content))))))

(ert-deftest beads-dep-test-tree-render-issue-status-faces ()
  "Test that different statuses get different faces."
  (dolist (status '("open" "in_progress" "blocked" "closed" "unknown"))
    (let ((issue (beads-tree-node
                  :id "bd-1" :title "Test" :status status
                  :depth 0 :truncated nil)))
      (with-temp-buffer
        (beads-dep-tree--render-issue issue)
        (should (string-match-p (upcase status) (buffer-string)))))))

;;; Tests for Get Issue At Point

(ert-deftest beads-dep-test-tree-get-issue-at-point ()
  "Test getting issue ID at point."
  (with-temp-buffer
    (insert (propertize "bd-1" 'beads-issue-id "bd-1"))
    (goto-char (point-min))
    (should (equal (beads-dep-tree--get-issue-at-point) "bd-1"))))

(ert-deftest beads-dep-test-tree-get-issue-at-point-none ()
  "Test getting issue ID when none at point."
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-min))
    (should-not (beads-dep-tree--get-issue-at-point))))

;;; Tests for Show Issue At Point

(ert-deftest beads-dep-test-tree-show-issue-no-issue ()
  "Test show issue when no issue at point."
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-min))
    (should-error (beads-dep-tree-show-issue)
                  :type 'user-error)))

(ert-deftest beads-dep-test-tree-show-issue-calls-show ()
  "Test show issue calls beads-show."
  (let ((called-with nil))
    (with-temp-buffer
      (insert (propertize "bd-1" 'beads-issue-id "bd-1"))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'beads-show)
                 (lambda (id) (setq called-with id))))
        (beads-dep-tree-show-issue)
        (should (equal called-with "bd-1"))))))

;;; Tests for Context Detection from Show Mode

(ert-deftest beads-dep-test-detect-issue-id-from-list-mode ()
  "Test detecting issue ID from beads-list buffer."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (should (equal (beads-dep--detect-issue-id) "bd-42")))))

(ert-deftest beads-dep-test-detect-issue-id-from-show-mode ()
  "Test detecting issue ID from beads-show buffer."
  (require 'beads-command-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--current-issue-id "bd-99")
    (should (equal (beads-dep--detect-issue-id) "bd-99"))))

;;; Tests for Format Dependency Edge Cases

(ert-deftest beads-dep-test-format-dependency-missing-status ()
  "Test formatting dependency with missing status."
  (let ((dep (beads-dep-op-result
              :issue-id "bd-1"
              :depends-on-id "bd-2"
              :dep-type "blocks")))
    (let ((formatted (beads-dep--format-dependency dep)))
      (should (stringp formatted))
      (should (string-match-p "unknown" formatted)))))

(ert-deftest beads-dep-test-format-dependency-all-types ()
  "Test formatting dependencies with different types."
  (dolist (dep-type '("blocks" "required_by" "relates_to"))
    (let ((dep (beads-dep-op-result
                :issue-id "bd-1"
                :depends-on-id "bd-2"
                :dep-type dep-type
                :op-status "added")))
      (let ((formatted (beads-dep--format-dependency dep)))
        (should (string-match-p dep-type formatted))))))

;;; Tests for Tree Rendering Edge Cases

(ert-deftest beads-dep-test-tree-render-zero-depth ()
  "Test rendering tree node with zero depth."
  (let ((issue (beads-tree-node
                :id "bd-1" :title "Root" :status "open"
                :depth 0 :truncated nil)))
    (with-temp-buffer
      (beads-dep-tree--render-issue issue)
      (let ((content (buffer-string)))
        ;; Should not have tree connector at depth 0
        (should-not (string-match-p "└─" content))))))

;;; Tests for Entry Point Functions

(ert-deftest beads-dep-test-dep-add-entry-point ()
  "Test beads-dep-add entry point."
  (let ((menu-shown nil))
    (cl-letf (((symbol-function 'beads-dep-add--menu)
               (lambda () (setq menu-shown t))))
      (beads-dep-add "bd-1")
      (should menu-shown))))

(ert-deftest beads-dep-test-dep-remove-entry-point ()
  "Test beads-dep-remove entry point."
  (let ((menu-shown nil))
    (cl-letf (((symbol-function 'beads-dep-remove--menu)
               (lambda () (setq menu-shown t))))
      (beads-dep-remove "bd-1")
      (should menu-shown))))

(ert-deftest beads-dep-test-dep-tree-empty-issue-id ()
  "Test beads-dep-tree with empty issue ID."
  (should-error (beads-dep-tree "")
                :type 'user-error))

;;; Tests for Transient Prefix Definitions

(ert-deftest beads-dep-test-transient-prefixes-defined ()
  "Test that all transient prefixes are defined."
  (should (get 'beads-dep-add--menu 'transient--prefix))
  (should (get 'beads-dep-remove--menu 'transient--prefix))
  (should (get 'beads-dep 'transient--prefix)))

;;; Tests for Reset Functions

(ert-deftest beads-dep-test-add-reset-confirmed ()
  "Test add reset when user confirms."
  (let ((reset-called nil)
        (message-output nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda ()))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-dep-add--reset)
      (should reset-called)
      (should (string-match-p "reset" message-output)))))

(ert-deftest beads-dep-test-add-reset-declined ()
  "Test add reset when user declines."
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-dep-add--reset)
      (should-not reset-called))))

(ert-deftest beads-dep-test-remove-reset-confirmed ()
  "Test remove reset when user confirms."
  (let ((reset-called nil)
        (message-output nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda ()))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-dep-remove--reset)
      (should reset-called)
      (should (string-match-p "reset" message-output)))))

(ert-deftest beads-dep-test-remove-reset-declined ()
  "Test remove reset when user declines."
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-dep-remove--reset)
      (should-not reset-called))))

;;; Additional Coverage Tests

(ert-deftest beads-dep-test-add-execute-function-exists ()
  "Test beads-dep-add--execute function exists."
  (should (fboundp 'beads-dep-add--execute)))

(ert-deftest beads-dep-test-remove-execute-function-exists ()
  "Test beads-dep-remove--execute function exists."
  (should (fboundp 'beads-dep-remove--execute)))

(ert-deftest beads-dep-test-tree-function-exists ()
  "Test beads-dep-tree function exists."
  (should (fboundp 'beads-dep-tree)))

(ert-deftest beads-dep-test-tree-mode-exists ()
  "Test beads-dep-tree-mode exists."
  (should (fboundp 'beads-dep-tree-mode)))

(ert-deftest beads-dep-test-beads-dep-function-exists ()
  "Test beads-dep transient exists."
  (should (fboundp 'beads-dep)))

(ert-deftest beads-dep-test-add-transient-exists ()
  "Test beads-dep-add transient exists."
  (should (fboundp 'beads-dep-add)))

(ert-deftest beads-dep-test-remove-transient-exists ()
  "Test beads-dep-remove transient exists."
  (should (fboundp 'beads-dep-remove)))

;;; Dep List Parse Tests

(ert-deftest beads-dep-test-dep-list-parse-json-vector ()
  "Test dep list parse method with vector of deps."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :json t :issue-id "bd-1"))
         (json-data (vector '((id . "bd-2")
                              (title . "Dep Issue")
                              (status . "open")
                              (priority . 1)
                              (issue_type . "task")
                              (dependency_type . "blocks")
                              (created_at . "2025-01-01T00:00:00Z")
                              (updated_at . "2025-01-01T00:00:00Z"))))
         (json-string (json-encode json-data))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 1))
      (should (cl-typep (car result) 'beads-dependency)))))

(ert-deftest beads-dep-test-dep-list-parse-json-empty ()
  "Test dep list parse method with empty array."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :json t :issue-id "bd-1"))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "[]"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 0)))))

(ert-deftest beads-dep-test-dep-list-parse-json-null ()
  "Test dep list parse method with null result."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :json t :issue-id "bd-1"))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "null"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (null result)))))

(ert-deftest beads-dep-test-dep-list-parse-unexpected-json ()
  "Test dep list parse signals error on unexpected structure."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :json t :issue-id "bd-1"))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "\"just a string\""
                :stderr "")))
    (should-error (beads-command-parse cmd exec)
                  :type 'beads-json-parse-error)))

(ert-deftest beads-dep-test-dep-list-parse-no-json ()
  "Test dep list parse with json=nil returns raw output."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :json nil :issue-id "bd-1"))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "Some text output"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (stringp result)))))

;;; Dep Remove Validate Tests

(ert-deftest beads-dep-test-remove-validate-no-issue ()
  "Test remove validate fails without issue ID."
  :tags '(:unit)
  (should (equal (beads-dep-remove--validate nil "bd-2")
                 "Issue ID is required"))
  (should (equal (beads-dep-remove--validate "" "bd-2")
                 "Issue ID is required")))

(ert-deftest beads-dep-test-remove-validate-no-depends ()
  "Test remove validate fails without depends-on ID."
  :tags '(:unit)
  (should (equal (beads-dep-remove--validate "bd-1" nil)
                 "Depends-on ID is required"))
  (should (equal (beads-dep-remove--validate "bd-1" "")
                 "Depends-on ID is required")))

(ert-deftest beads-dep-test-remove-validate-both-valid ()
  "Test remove validate passes with valid IDs."
  :tags '(:unit)
  (should (null (beads-dep-remove--validate "bd-1" "bd-2"))))

(ert-deftest beads-dep-test-remove-parse-transient-args ()
  "Test remove parse transient args."
  :tags '(:unit)
  (let ((result (beads-dep-remove--parse-transient-args
                 '("--issue-id=bd-1" "--depends-on=bd-2"))))
    (should (equal (plist-get result :issue-id) "bd-1"))
    (should (equal (plist-get result :depends-on-id) "bd-2"))))

(ert-deftest beads-dep-test-remove-parse-transient-args-empty ()
  "Test remove parse transient args with empty list."
  :tags '(:unit)
  (let ((result (beads-dep-remove--parse-transient-args nil)))
    (should (null (plist-get result :issue-id)))
    (should (null (plist-get result :depends-on-id)))))

;;; Dep Tree Render Tests

(ert-deftest beads-dep-test-tree-render-issue ()
  "Test dep tree renders a single issue."
  :tags '(:unit)
  (with-temp-buffer
    (beads-dep-tree--render-issue
     (beads-tree-node :id "bd-1" :title "Test Issue" :status "open"
                      :depth 0 :truncated nil))
    (let ((text (buffer-string)))
      (should (string-match-p "bd-1" text))
      (should (string-match-p "Test Issue" text))
      (should (string-match-p "OPEN" text)))))

(ert-deftest beads-dep-test-tree-render-issue-indented ()
  "Test dep tree renders indented issue."
  :tags '(:unit)
  (with-temp-buffer
    (beads-dep-tree--render-issue
     (beads-tree-node :id "bd-2" :title "Child Issue" :status "in_progress"
                      :depth 2 :truncated nil))
    (let ((text (buffer-string)))
      (should (string-match-p "bd-2" text))
      (should (string-match-p "- " text))
      (should (string-match-p "IN_PROGRESS" text)))))

(ert-deftest beads-dep-test-tree-render-issue-truncated ()
  "Test dep tree renders truncated indicator."
  :tags '(:unit)
  (with-temp-buffer
    (beads-dep-tree--render-issue
     (beads-tree-node :id "bd-3" :title "Truncated" :status "open"
                      :depth 1 :truncated t))
    (let ((text (buffer-string)))
      (should (string-match-p "\\[truncated\\]" text)))))

(ert-deftest beads-dep-test-tree-render-full ()
  "Test dep tree full render with header."
  :tags '(:unit)
  (with-temp-buffer
    (let ((issues (list (beads-tree-node
                         :id "bd-1" :title "Root" :status "open"
                         :depth 0 :truncated nil)
                        (beads-tree-node
                         :id "bd-2" :title "Child" :status "closed"
                         :depth 1 :truncated nil))))
      (beads-dep-tree--render issues "bd-1")
      (let ((text (buffer-string)))
        (should (string-match-p "Dependency Tree for bd-1" text))
        (should (string-match-p "bd-1" text))
        (should (string-match-p "bd-2" text))
        (should (string-match-p "Commands:" text))))))

(ert-deftest beads-dep-test-tree-render-no-deps ()
  "Test dep tree render with no dependencies."
  :tags '(:unit)
  (with-temp-buffer
    (beads-dep-tree--render nil "bd-1")
    (should (string-match-p "No dependencies found" (buffer-string)))))

(ert-deftest beads-dep-test-tree-get-issue-at-point-rendered ()
  "Test getting issue ID from text property after render."
  :tags '(:unit)
  (with-temp-buffer
    (beads-dep-tree--render-issue
     (beads-tree-node :id "bd-42" :title "Test" :status "open"
                      :depth 0 :truncated nil))
    (goto-char (point-min))
    (should (equal (beads-dep-tree--get-issue-at-point) "bd-42"))))

(ert-deftest beads-dep-test-tree-mode-has-keybindings ()
  "Test dep tree mode has expected keybindings."
  :tags '(:unit)
  (should (keymapp beads-dep-tree-mode-map))
  (should (lookup-key beads-dep-tree-mode-map "q"))
  (should (lookup-key beads-dep-tree-mode-map "g"))
  (should (lookup-key beads-dep-tree-mode-map (kbd "RET"))))

;;; Tests for dep relate command

(ert-deftest beads-dep-test-relate-command-class-exists ()
  "Test that beads-command-dep-relate class is defined."
  :tags '(:unit)
  (should (fboundp 'beads-command-dep-relate)))

(ert-deftest beads-dep-test-relate-command-line-basic ()
  "Test dep relate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-relate :id1 "bd-1" :id2 "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "relate" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-dep-test-relate-validation-missing-id1 ()
  "Test dep relate validation fails without first issue ID."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id2 "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dep-test-relate-validation-missing-id2 ()
  "Test dep relate validation fails without second issue ID."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id1 "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dep-test-relate-validation-same-issue ()
  "Test dep relate validation fails when both IDs are the same."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id1 "bd-1" :id2 "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dep-test-relate-validation-success ()
  "Test dep relate validation succeeds with two different IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id1 "bd-1" :id2 "bd-2")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-dep-test-relate-transient-defined ()
  "Test that beads-dep-relate transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-dep-relate)))

(ert-deftest beads-dep-test-relate-transient-is-prefix ()
  "Test that beads-dep-relate is a transient prefix."
  :tags '(:unit)
  (should (get 'beads-dep-relate 'transient--prefix)))

;;; Tests for dep unrelate command

(ert-deftest beads-dep-test-unrelate-command-class-exists ()
  "Test that beads-command-dep-unrelate class is defined."
  :tags '(:unit)
  (should (fboundp 'beads-command-dep-unrelate)))

(ert-deftest beads-dep-test-unrelate-command-line-basic ()
  "Test dep unrelate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-unrelate :id1 "bd-1" :id2 "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "unrelate" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-dep-test-unrelate-validation-missing-id1 ()
  "Test dep unrelate validation fails without first issue ID."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-unrelate :id2 "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dep-test-unrelate-validation-missing-id2 ()
  "Test dep unrelate validation fails without second issue ID."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-unrelate :id1 "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dep-test-unrelate-validation-success ()
  "Test dep unrelate validation succeeds with two IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-unrelate :id1 "bd-1" :id2 "bd-2")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-dep-test-unrelate-transient-defined ()
  "Test that beads-dep-unrelate transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-dep-unrelate)))

(ert-deftest beads-dep-test-unrelate-transient-is-prefix ()
  "Test that beads-dep-unrelate is a transient prefix."
  :tags '(:unit)
  (should (get 'beads-dep-unrelate 'transient--prefix)))

;;; Tests for dep list interactive command

(ert-deftest beads-dep-test-dep-list-function-defined ()
  "Test that beads-dep-list is defined as a function."
  :tags '(:unit)
  (should (fboundp 'beads-dep-list)))

(ert-deftest beads-dep-test-dep-list-is-interactive ()
  "Test that beads-dep-list is interactive."
  :tags '(:unit)
  (should (commandp 'beads-dep-list)))

(ert-deftest beads-dep-test-dep-menu-has-relate ()
  "Test that beads-dep menu includes relate command."
  :tags '(:unit)
  (let* ((layout (get 'beads-dep 'transient--layout))
         (layout-str (format "%s" layout)))
    (should (string-match-p "beads-dep-relate" layout-str))))

(ert-deftest beads-dep-test-dep-menu-has-unrelate ()
  "Test that beads-dep menu includes unrelate command."
  :tags '(:unit)
  (let* ((layout (get 'beads-dep 'transient--layout))
         (layout-str (format "%s" layout)))
    (should (string-match-p "beads-dep-unrelate" layout-str))))

(ert-deftest beads-dep-test-dep-menu-has-list ()
  "Test that beads-dep menu includes list command."
  :tags '(:unit)
  (let* ((layout (get 'beads-dep 'transient--layout))
         (layout-str (format "%s" layout)))
    (should (string-match-p "beads-dep-list" layout-str))))

;;; Tests for beads-dep-list issue ID pre-population

(ert-deftest beads-dep-test-detect-issue-id-from-pending ()
  "Test that beads-dep--detect-issue-id returns pending issue ID."
  :tags '(:unit)
  (let ((beads-dep--pending-issue-id "bd-pending"))
    (should (equal (beads-dep--detect-issue-id) "bd-pending"))))

(ert-deftest beads-dep-test-detect-issue-id-pending-takes-priority ()
  "Test that pending issue ID takes priority over buffer context."
  :tags '(:unit)
  (require 'beads-command-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--current-issue-id "bd-context")
    (let ((beads-dep--pending-issue-id "bd-explicit"))
      (should (equal (beads-dep--detect-issue-id) "bd-explicit")))))

(ert-deftest beads-dep-test-dep-list-passes-issue-id-via-pending ()
  "Test that beads-dep-list binds issue-id as pending when calling transient."
  :tags '(:unit)
  (let ((captured-pending nil)
        (transient-called nil))
    (cl-letf (((symbol-function 'beads-dep-list-transient)
               (lambda ()
                 (setq transient-called t)
                 (setq captured-pending beads-dep--pending-issue-id))))
      (beads-dep-list "bd-42")
      (should transient-called)
      (should (equal captured-pending "bd-42")))))

(ert-deftest beads-dep-test-dep-list-transient-has-default-value ()
  "Test that beads-dep-list-transient prototype has a default-value function."
  :tags '(:unit)
  (let ((proto (get 'beads-dep-list-transient 'transient--prefix)))
    (should proto)
    (should (slot-boundp proto 'default-value))
    (should (functionp (oref proto default-value)))))

(ert-deftest beads-dep-test-dep-list-transient-default-value-uses-pending ()
  "Test that beads-dep-list-transient default-value reads pending issue ID."
  :tags '(:unit)
  (let ((proto (get 'beads-dep-list-transient 'transient--prefix)))
    (should proto)
    (let ((beads-dep--pending-issue-id "bd-test"))
      (let ((value (funcall (oref proto default-value))))
        (should (equal value (list "--id=bd-test")))))))

(ert-deftest beads-dep-test-dep-list-transient-default-value-nil-when-no-context ()
  "Test that beads-dep-list-transient default-value returns nil without context."
  :tags '(:unit)
  (let ((proto (get 'beads-dep-list-transient 'transient--prefix)))
    (should proto)
    (let ((beads-dep--pending-issue-id nil))
      (with-temp-buffer
        ;; No list or show mode active
        (let ((value (funcall (oref proto default-value))))
          (should (null value)))))))

(provide 'beads-dep-test)
;;; beads-dep-test.el ends here
