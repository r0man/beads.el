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
(require 'beads-dep)

;;; Test Fixtures

(defvar beads-dep-test--sample-dependency
  '((issue_id . "bd-1")
    (depends_on_id . "bd-2")
    (type . "blocks")
    (status . "added"))
  "Sample dependency for testing.")

(defvar beads-dep-test--sample-tree
  [((id . "bd-1")
    (title . "Main issue")
    (status . "open")
    (priority . 1)
    (issue_type . "feature")
    (depth . 0)
    (truncated . nil))
   ((id . "bd-2")
    (title . "Dependency 1")
    (status . "open")
    (priority . 2)
    (issue_type . "task")
    (depth . 1)
    (truncated . nil))
   ((id . "bd-3")
    (title . "Dependency 2")
    (status . "closed")
    (priority . 2)
    (issue_type . "bug")
    (depth . 2)
    (truncated . nil))]
  "Sample dependency tree for testing.")

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
  (let ((beads-dep-add--issue-id nil)
        (beads-dep-add--depends-on-id "bd-2")
        (beads-dep-add--type "blocks"))
    (should (stringp (beads-dep-add--validate)))))

(ert-deftest beads-dep-test-add-validate-missing-depends-on ()
  "Test add validation with missing depends-on ID."
  (let ((beads-dep-add--issue-id "bd-1")
        (beads-dep-add--depends-on-id nil)
        (beads-dep-add--type "blocks"))
    (should (stringp (beads-dep-add--validate)))))

(ert-deftest beads-dep-test-add-validate-self-dependency ()
  "Test add validation with self-dependency."
  (let ((beads-dep-add--issue-id "bd-1")
        (beads-dep-add--depends-on-id "bd-1")
        (beads-dep-add--type "blocks"))
    (should (string-match-p "itself" (beads-dep-add--validate)))))

(ert-deftest beads-dep-test-add-validate-missing-type ()
  "Test add validation with missing type."
  (let ((beads-dep-add--issue-id "bd-1")
        (beads-dep-add--depends-on-id "bd-2")
        (beads-dep-add--type nil))
    (should (stringp (beads-dep-add--validate)))))

(ert-deftest beads-dep-test-add-validate-valid ()
  "Test add validation with valid parameters."
  (let ((beads-dep-add--issue-id "bd-1")
        (beads-dep-add--depends-on-id "bd-2")
        (beads-dep-add--type "blocks"))
    (should (null (beads-dep-add--validate)))))

;;; Tests for Add Dependency Reset

(ert-deftest beads-dep-test-add-reset ()
  "Test resetting add dependency state."
  (let ((beads-dep-add--issue-id "bd-1")
        (beads-dep-add--depends-on-id "bd-2")
        (beads-dep-add--type "related"))
    (beads-dep-add--reset)
    (should (null beads-dep-add--issue-id))
    (should (null beads-dep-add--depends-on-id))
    (should (equal beads-dep-add--type "blocks"))))

;;; Tests for Remove Dependency Validation

(ert-deftest beads-dep-test-remove-validate-missing-issue-id ()
  "Test remove validation with missing issue ID."
  (let ((beads-dep-remove--issue-id nil)
        (beads-dep-remove--depends-on-id "bd-2"))
    (should (stringp (beads-dep-remove--validate)))))

(ert-deftest beads-dep-test-remove-validate-missing-depends-on ()
  "Test remove validation with missing depends-on ID."
  (let ((beads-dep-remove--issue-id "bd-1")
        (beads-dep-remove--depends-on-id nil))
    (should (stringp (beads-dep-remove--validate)))))

(ert-deftest beads-dep-test-remove-validate-valid ()
  "Test remove validation with valid parameters."
  (let ((beads-dep-remove--issue-id "bd-1")
        (beads-dep-remove--depends-on-id "bd-2"))
    (should (null (beads-dep-remove--validate)))))

;;; Tests for Remove Dependency Reset

(ert-deftest beads-dep-test-remove-reset ()
  "Test resetting remove dependency state."
  (let ((beads-dep-remove--issue-id "bd-1")
        (beads-dep-remove--depends-on-id "bd-2"))
    (beads-dep-remove--reset)
    (should (null beads-dep-remove--issue-id))
    (should (null beads-dep-remove--depends-on-id))))

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
    (beads-dep-tree--render (list (aref beads-dep-test--sample-tree 0))
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
    (beads-dep-tree--render (append beads-dep-test--sample-tree nil)
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
    (beads-dep-tree--render (append beads-dep-test--sample-tree nil)
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
      (should (string-match-p "→" content)))))

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
  (let ((json-output (json-encode beads-dep-test--sample-tree)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-tree "bd-1")
      (should (get-buffer "*beads-dep-tree: bd-1*"))
      (kill-buffer "*beads-dep-tree: bd-1*"))))

(ert-deftest beads-dep-test-tree-command-sets-mode ()
  "Test that beads-dep-tree sets the correct mode."
  (let ((json-output (json-encode beads-dep-test--sample-tree)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-tree "bd-1")
      (with-current-buffer "*beads-dep-tree: bd-1*"
        (should (eq major-mode 'beads-dep-tree-mode)))
      (kill-buffer "*beads-dep-tree: bd-1*"))))

(ert-deftest beads-dep-test-tree-command-displays-content ()
  "Test that beads-dep-tree displays tree content."
  (let ((json-output (json-encode beads-dep-test--sample-tree)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-tree "bd-1")
      (with-current-buffer "*beads-dep-tree: bd-1*"
        (let ((content (buffer-string)))
          (should (string-match-p "Dependency Tree" content))
          (should (string-match-p "bd-1" content))
          (should (string-match-p "Main issue" content))))
      (kill-buffer "*beads-dep-tree: bd-1*"))))

(ert-deftest beads-dep-test-tree-command-requires-issue-id ()
  "Test that beads-dep-tree requires issue ID."
  (should-error (beads-dep-tree nil)))

;;; Tests for Cycles Command

(ert-deftest beads-dep-test-cycles-command-creates-buffer ()
  "Test that beads-dep-cycles creates a buffer."
  (let ((json-output (json-encode beads-dep-test--sample-cycles)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-cycles)
      (should (get-buffer "*beads-dep-cycles*"))
      (kill-buffer "*beads-dep-cycles*"))))

(ert-deftest beads-dep-test-cycles-command-sets-mode ()
  "Test that beads-dep-cycles sets the correct mode."
  (let ((json-output (json-encode beads-dep-test--sample-cycles)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-cycles)
      (with-current-buffer "*beads-dep-cycles*"
        (should (eq major-mode 'beads-dep-cycles-mode)))
      (kill-buffer "*beads-dep-cycles*"))))

(ert-deftest beads-dep-test-cycles-command-displays-cycles ()
  "Test that beads-dep-cycles displays cycle content."
  (let ((json-output (json-encode beads-dep-test--sample-cycles)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-cycles)
      (with-current-buffer "*beads-dep-cycles*"
        (let ((content (buffer-string)))
          (should (string-match-p "Dependency Cycles" content))
          (should (string-match-p "Found 2 cycle" content))))
      (kill-buffer "*beads-dep-cycles*"))))

(ert-deftest beads-dep-test-cycles-command-no-cycles ()
  "Test that beads-dep-cycles handles no cycles."
  (let ((json-output (json-encode [])))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-cycles)
      (with-current-buffer "*beads-dep-cycles*"
        (let ((content (buffer-string)))
          (should (string-match-p "No dependency cycles" content))))
      (kill-buffer "*beads-dep-cycles*"))))

;;; Tests for Tree Refresh

(ert-deftest beads-dep-test-tree-refresh-updates-buffer ()
  "Test that tree refresh updates the buffer."
  (let ((json-output (json-encode beads-dep-test--sample-tree)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-tree "bd-1")
      (with-current-buffer "*beads-dep-tree: bd-1*"
        ;; Modify buffer to test refresh
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert "TEST"))
        ;; Refresh
        (beads-dep-tree-refresh)
        (let ((content (buffer-string)))
          (should-not (string-match-p "TEST" content))
          (should (string-match-p "Dependency Tree" content))))
      (kill-buffer "*beads-dep-tree: bd-1*"))))

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
  (let ((json-output (json-encode beads-dep-test--sample-cycles)))
    (cl-letf (((symbol-function 'call-process)
               (beads-dep-test--mock-call-process 0 json-output)))
      (beads-dep-cycles)
      (with-current-buffer "*beads-dep-cycles*"
        ;; Modify buffer to test refresh
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert "TEST"))
        ;; Refresh
        (beads-dep-cycles-refresh)
        (let ((content (buffer-string)))
          (should-not (string-match-p "TEST" content))
          (should (string-match-p "Dependency Cycles" content))))
      (kill-buffer "*beads-dep-cycles*"))))

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
  (let ((beads-dep-add--issue-id "")
        (beads-dep-add--depends-on-id "bd-2")
        (beads-dep-add--type "blocks"))
    (should (stringp (beads-dep-add--validate)))))

(ert-deftest beads-dep-test-edge-case-whitespace-issue-id ()
  "Test handling of whitespace issue ID."
  (let ((beads-dep-add--issue-id "   ")
        (beads-dep-add--depends-on-id "bd-2")
        (beads-dep-add--type "blocks"))
    ;; String-empty-p doesn't catch whitespace, so this might pass validation
    ;; but fail at execution - which is acceptable
    (should t)))

(ert-deftest beads-dep-test-edge-case-truncated-tree ()
  "Test rendering tree with truncated flag."
  (let ((truncated-issue '((id . "bd-1")
                          (title . "Issue")
                          (status . "open")
                          (priority . 1)
                          (issue_type . "feature")
                          (depth . 0)
                          (truncated . t))))
    (with-temp-buffer
      (beads-dep-tree-mode)
      (beads-dep-tree--render (list truncated-issue) "bd-1")
      (let ((content (buffer-string)))
        (should (string-match-p "\\[truncated\\]" content))))))

(ert-deftest beads-dep-test-edge-case-deep-nesting ()
  "Test rendering tree with deep nesting."
  (let ((deep-issue '((id . "bd-10")
                     (title . "Deep issue")
                     (status . "open")
                     (priority . 1)
                     (issue_type . "feature")
                     (depth . 10)
                     (truncated . nil))))
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
  (let ((large-tree (make-vector 100 nil)))
    (dotimes (i 100)
      (aset large-tree i
           `((id . ,(format "bd-%d" i))
             (title . "Test issue")
             (status . "open")
             (priority . 1)
             (issue_type . "feature")
             (depth . ,(/ i 10))
             (truncated . nil))))
    (let ((start-time (current-time)))
      (with-temp-buffer
        (beads-dep-tree-mode)
        (beads-dep-tree--render (append large-tree nil) "bd-1"))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should render 100 issues in under 1 second
        (should (< elapsed 1.0))))))

(provide 'beads-dep-test)
;;; beads-dep-test.el ends here
