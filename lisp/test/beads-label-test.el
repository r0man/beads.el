;;; beads-label-test.el --- Tests for beads-label -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-label.el transient menus.
;; Tests cover all four subcommands: add, remove, list, list-all.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-label)

;;; Test Fixtures

(defvar beads-label-test--sample-labels
  ["bug" "feature" "documentation"]
  "Sample labels for testing.")

(defvar beads-label-test--sample-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (status . "open")
    (labels . ["bug" "feature"]))
  "Sample issue with labels for testing.")

;;; Test Utilities

(defun beads-label-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defmacro beads-label-test-with-state (state &rest body)
  "Execute BODY with beads-label state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-label--issue-id nil
           beads-label--label nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

;;; ============================================================
;;; State Management Tests
;;; ============================================================

(ert-deftest beads-label-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-label-test-with-state
   '((beads-label--issue-id . "bd-42")
     (beads-label--label . "bug"))
   (beads-label--reset-state)
   (should (null beads-label--issue-id))
   (should (null beads-label--label))))

(ert-deftest beads-label-test-format-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-label--format-value "test-value")))
    (should (stringp result))
    (should (string-match-p "test-value" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-label-test-format-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-label--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

;;; ============================================================
;;; Validation Tests
;;; ============================================================

(ert-deftest beads-label-test-validate-issue-id-nil ()
  "Test issue ID validation when nil."
  (beads-label-test-with-state nil
   (should (beads-label--validate-issue-id))))

(ert-deftest beads-label-test-validate-issue-id-empty ()
  "Test issue ID validation when empty."
  (beads-label-test-with-state
   '((beads-label--issue-id . ""))
   (should (beads-label--validate-issue-id))))

(ert-deftest beads-label-test-validate-issue-id-whitespace ()
  "Test issue ID validation when whitespace."
  (beads-label-test-with-state
   '((beads-label--issue-id . "   \n\t  "))
   (should (beads-label--validate-issue-id))))

(ert-deftest beads-label-test-validate-issue-id-valid ()
  "Test issue ID validation when valid."
  (beads-label-test-with-state
   '((beads-label--issue-id . "bd-42"))
   (should (null (beads-label--validate-issue-id)))))

(ert-deftest beads-label-test-validate-label-nil ()
  "Test label validation when nil."
  (beads-label-test-with-state nil
   (should (beads-label--validate-label))))

(ert-deftest beads-label-test-validate-label-empty ()
  "Test label validation when empty."
  (beads-label-test-with-state
   '((beads-label--label . ""))
   (should (beads-label--validate-label))))

(ert-deftest beads-label-test-validate-label-whitespace ()
  "Test label validation when whitespace."
  (beads-label-test-with-state
   '((beads-label--label . "   "))
   (should (beads-label--validate-label))))

(ert-deftest beads-label-test-validate-label-valid ()
  "Test label validation when valid."
  (beads-label-test-with-state
   '((beads-label--label . "bug"))
   (should (null (beads-label--validate-label)))))

(ert-deftest beads-label-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (beads-label-test-with-state
   '((beads-label--issue-id . "bd-42")
     (beads-label--label . "bug"))
   (should (null (beads-label--validate-all)))))

(ert-deftest beads-label-test-validate-all-failure ()
  "Test validate-all with missing parameters."
  (beads-label-test-with-state nil
   (let ((errors (beads-label--validate-all)))
     (should errors)
     (should (listp errors))
     (should (>= (length errors) 2)))))

(ert-deftest beads-label-test-validate-all-partial ()
  "Test validate-all with only issue ID set."
  (beads-label-test-with-state
   '((beads-label--issue-id . "bd-42"))
   (let ((errors (beads-label--validate-all)))
     (should errors)
     (should (= (length errors) 1)))))

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(ert-deftest beads-label-test-get-all-labels-success ()
  "Test getting all labels successfully."
  (let ((json-output (json-encode beads-label-test--sample-labels)))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (let ((labels (beads-label--get-all-labels)))
        (should labels)
        (should (listp labels))
        (should (member "bug" labels))
        (should (member "feature" labels))
        (should (member "documentation" labels))))))

(ert-deftest beads-label-test-get-all-labels-empty ()
  "Test getting all labels when none exist."
  (let ((json-output "[]"))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (let ((labels (beads-label--get-all-labels)))
        (should (null labels))))))

(ert-deftest beads-label-test-get-all-labels-failure ()
  "Test getting all labels handles failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-label-test--mock-call-process 1 "Error")))
    (let ((labels (beads-label--get-all-labels)))
      (should (null labels)))))

(ert-deftest beads-label-test-get-issue-labels-success ()
  "Test getting issue labels successfully."
  (let ((json-output (json-encode ["bug" "feature"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (let ((labels (beads-label--get-issue-labels "bd-42")))
        (should labels)
        (should (listp labels))
        (should (member "bug" labels))
        (should (member "feature" labels))))))

(ert-deftest beads-label-test-get-issue-labels-empty ()
  "Test getting issue labels when none exist."
  (let ((json-output "[]"))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (let ((labels (beads-label--get-issue-labels "bd-42")))
        (should (null labels))))))

(ert-deftest beads-label-test-get-issue-labels-failure ()
  "Test getting issue labels handles failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-label-test--mock-call-process 1 "Error")))
    (let ((labels (beads-label--get-issue-labels "bd-42")))
      (should (null labels)))))

(ert-deftest beads-label-test-get-available-issues-success ()
  "Test getting available issues."
  (let ((json-output (json-encode
                      [((id . "bd-1") (title . "First"))
                       ((id . "bd-2") (title . "Second"))])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (let ((issues (beads-label--get-available-issues)))
        (should issues)
        (should (member "bd-1" issues))
        (should (member "bd-2" issues))))))

;;; ============================================================
;;; Add Label Tests
;;; ============================================================

(ert-deftest beads-label-test-execute-add-success ()
  "Test successful label addition."
  (let ((json-output "{}"))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (should-not (beads-label--execute-add "bd-42" "bug")))))

(ert-deftest beads-label-test-execute-add-failure ()
  "Test label addition failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-label-test--mock-call-process 1 "Error")))
    (should-error (beads-label--execute-add "bd-42" "bug"))))

(ert-deftest beads-label-test-execute-add-special-chars ()
  "Test adding label with special characters."
  (let ((json-output "{}"))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (should-not (beads-label--execute-add "bd-42" "bug-fix"))
      (should-not (beads-label--execute-add "bd-42" "v1.0"))
      (should-not (beads-label--execute-add "bd-42" "high-priority")))))

(ert-deftest beads-label-test-add-command-validation ()
  "Test add command validates parameters."
  (beads-label-test-with-state nil
   (should-error (beads-label--add-command))))

;;; ============================================================
;;; Remove Label Tests
;;; ============================================================

(ert-deftest beads-label-test-execute-remove-success ()
  "Test successful label removal."
  (let ((json-output "{}"))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output)))
      (should-not (beads-label--execute-remove "bd-42" "bug")))))

(ert-deftest beads-label-test-execute-remove-failure ()
  "Test label removal failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-label-test--mock-call-process 1 "Error")))
    (should-error (beads-label--execute-remove "bd-42" "bug"))))

(ert-deftest beads-label-test-execute-remove-nonexistent ()
  "Test removing nonexistent label."
  (cl-letf (((symbol-function 'call-process)
             (beads-label-test--mock-call-process 1
                                                  "Label not found")))
    (should-error (beads-label--execute-remove "bd-42" "nonexistent"))))

(ert-deftest beads-label-test-remove-command-validation ()
  "Test remove command validates parameters."
  (beads-label-test-with-state nil
   (should-error (beads-label--remove-command))))

;;; ============================================================
;;; List Labels Tests
;;; ============================================================

(ert-deftest beads-label-test-execute-list-success ()
  "Test successful label listing for issue."
  (let ((json-output (json-encode ["bug" "feature"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-label--execute-list "bd-42"))
      (should (get-buffer "*beads-labels: bd-42*")))))

(ert-deftest beads-label-test-execute-list-empty ()
  "Test label listing with no labels."
  (let ((json-output "[]"))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-label--execute-list "bd-42"))
      (should (get-buffer "*beads-labels: bd-42*"))
      (with-current-buffer "*beads-labels: bd-42*"
        (should (string-match-p "No labels found"
                                (buffer-string)))))))

(ert-deftest beads-label-test-execute-list-failure ()
  "Test label listing handles failure gracefully."
  (cl-letf (((symbol-function 'call-process)
             (beads-label-test--mock-call-process 1 "Error"))
            ((symbol-function 'display-buffer) (lambda (_buf) nil)))
    ;; Should still create buffer even with error
    (beads-label--execute-list "bd-42")
    (should (get-buffer "*beads-labels: bd-42*"))))

(ert-deftest beads-label-test-execute-list-buffer-content ()
  "Test list buffer contains correct content."
  (let ((json-output (json-encode ["bug" "feature" "enhancement"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (should (string-match-p "bug" (buffer-string)))
        (should (string-match-p "feature" (buffer-string)))
        (should (string-match-p "enhancement" (buffer-string)))
        (should (string-match-p "Labels for bd-42" (buffer-string)))))))

(ert-deftest beads-label-test-execute-list-buffer-mode ()
  "Test list buffer uses special-mode."
  (let ((json-output (json-encode ["bug"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (should (eq major-mode 'special-mode))))))

(ert-deftest beads-label-test-execute-list-buffer-keybindings ()
  "Test list buffer has proper keybindings."
  (let ((json-output (json-encode ["bug"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (should (local-key-binding (kbd "q")))
        (should (local-key-binding (kbd "g")))))))

(ert-deftest beads-label-test-execute-list-refresh ()
  "Test list buffer refresh functionality."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 (with-current-buffer (current-buffer)
                   (insert (json-encode ["bug"])))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (should (= call-count 1))
      (with-current-buffer "*beads-labels: bd-42*"
        (funcall (local-key-binding (kbd "g"))))
      (should (= call-count 2)))))

;;; ============================================================
;;; List All Labels Tests
;;; ============================================================

(ert-deftest beads-label-test-execute-list-all-success ()
  "Test successful listing of all labels."
  (let ((json-output (json-encode beads-label-test--sample-labels)))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-label--execute-list-all))
      (should (get-buffer "*beads-labels-all*")))))

(ert-deftest beads-label-test-execute-list-all-empty ()
  "Test listing all labels when none exist."
  (let ((json-output "[]"))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-label--execute-list-all))
      (should (get-buffer "*beads-labels-all*"))
      (with-current-buffer "*beads-labels-all*"
        (should (string-match-p "No labels found"
                                (buffer-string)))))))

(ert-deftest beads-label-test-execute-list-all-failure ()
  "Test listing all labels handles failure gracefully."
  (cl-letf (((symbol-function 'call-process)
             (beads-label-test--mock-call-process 1 "Error"))
            ((symbol-function 'display-buffer) (lambda (_buf) nil)))
    ;; Should still create buffer even with error
    (beads-label--execute-list-all)
    (should (get-buffer "*beads-labels-all*"))))

(ert-deftest beads-label-test-execute-list-all-buffer-content ()
  "Test list-all buffer contains correct content."
  (let ((json-output (json-encode ["bug" "feature" "enhancement"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list-all)
      (with-current-buffer "*beads-labels-all*"
        (should (string-match-p "bug" (buffer-string)))
        (should (string-match-p "feature" (buffer-string)))
        (should (string-match-p "enhancement" (buffer-string)))
        (should (string-match-p "All Labels" (buffer-string)))))))

(ert-deftest beads-label-test-execute-list-all-buffer-mode ()
  "Test list-all buffer uses special-mode."
  (let ((json-output (json-encode ["bug"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list-all)
      (with-current-buffer "*beads-labels-all*"
        (should (eq major-mode 'special-mode))))))

(ert-deftest beads-label-test-execute-list-all-buffer-keybindings ()
  "Test list-all buffer has proper keybindings."
  (let ((json-output (json-encode ["bug"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list-all)
      (with-current-buffer "*beads-labels-all*"
        (should (local-key-binding (kbd "q")))
        (should (local-key-binding (kbd "g")))))))

(ert-deftest beads-label-test-execute-list-all-refresh ()
  "Test list-all buffer refresh functionality."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 (with-current-buffer (current-buffer)
                   (insert (json-encode ["bug"])))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list-all)
      (should (= call-count 1))
      (with-current-buffer "*beads-labels-all*"
        (funcall (local-key-binding (kbd "g"))))
      (should (= call-count 2)))))

;;; ============================================================
;;; Transient Menu Tests
;;; ============================================================

(ert-deftest beads-label-test-transient-defined ()
  "Test that beads-label transient is defined."
  (should (fboundp 'beads-label)))

(ert-deftest beads-label-test-transient-is-prefix ()
  "Test that beads-label is a transient prefix."
  (should (get 'beads-label 'transient--prefix)))

(ert-deftest beads-label-test-infix-commands-defined ()
  "Test that label infix commands are defined."
  (should (fboundp 'beads-label--infix-issue))
  (should (fboundp 'beads-label--infix-label)))

(ert-deftest beads-label-test-suffix-commands-defined ()
  "Test that label suffix commands are defined."
  (should (fboundp 'beads-label--add-command))
  (should (fboundp 'beads-label--remove-command))
  (should (fboundp 'beads-label--list-command))
  (should (fboundp 'beads-label--list-all-command))
  (should (fboundp 'beads-label--reset)))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-label-test-add-then-list ()
  "Test adding a label and then listing it."
  (let ((add-output "{}")
        (list-output (json-encode ["bug"])))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (with-current-buffer (current-buffer)
                   (insert (if (member "add" args)
                               add-output
                             list-output)))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-add "bd-42" "bug")
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (should (string-match-p "bug" (buffer-string)))))))

(ert-deftest beads-label-test-multiple-labels ()
  "Test handling multiple labels."
  (let ((json-output (json-encode
                      ["bug" "feature" "enhancement" "documentation"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (let ((content (buffer-string)))
          (should (string-match-p "bug" content))
          (should (string-match-p "feature" content))
          (should (string-match-p "enhancement" content))
          (should (string-match-p "documentation" content)))))))

(ert-deftest beads-label-test-label-with-hyphen ()
  "Test labels containing hyphens."
  (let ((json-output (json-encode ["high-priority" "needs-review"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (should (string-match-p "high-priority" (buffer-string)))
        (should (string-match-p "needs-review" (buffer-string)))))))

(ert-deftest beads-label-test-label-with-numbers ()
  "Test labels containing numbers."
  (let ((json-output (json-encode ["v1.0" "v2.0" "milestone-1"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (should (string-match-p "v1.0" (buffer-string)))
        (should (string-match-p "v2.0" (buffer-string)))
        (should (string-match-p "milestone-1" (buffer-string)))))))

(ert-deftest beads-label-test-empty-and-populated ()
  "Test transitioning from empty to populated labels."
  (let ((empty-output "[]")
        (populated-output (json-encode ["bug"]))
        (refresh-func nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 empty-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (should (string-match-p "No labels found" (buffer-string)))
        (setq refresh-func (local-key-binding (kbd "g")))))
    ;; Now simulate refresh with labels
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0
                                                    populated-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (with-current-buffer "*beads-labels: bd-42*"
        (funcall refresh-func)
        (should (string-match-p "bug" (buffer-string)))
        (should-not (string-match-p "No labels found"
                                    (buffer-string)))))))

(ert-deftest beads-label-test-case-sensitivity ()
  "Test that labels are case-sensitive."
  (let ((json-output (json-encode ["Bug" "bug" "BUG"])))
    (cl-letf (((symbol-function 'call-process)
               (beads-label-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-label--execute-list "bd-42")
      (with-current-buffer "*beads-labels: bd-42*"
        (let ((content (buffer-string)))
          (should (string-match-p "Bug" content))
          (should (string-match-p "bug" content))
          (should (string-match-p "BUG" content)))))))

(provide 'beads-label-test)
;;; beads-label-test.el ends here
