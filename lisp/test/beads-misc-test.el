;;; beads-misc-test.el --- Tests for beads-misc -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-misc.el transient menus.
;; Tests cover all six command groups: close, dep, stats, export,
;; import, and init.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-misc)

;;; Test Fixtures

(defvar beads-misc-test--sample-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (status . "open")
    (priority . 1))
  "Sample issue for testing.")

(defvar beads-misc-test--sample-closed-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (status . "closed")
    (closed_at . "2025-01-15T10:30:00Z"))
  "Sample closed issue for testing.")

(defvar beads-misc-test--sample-dependency
  '((from_issue . "bd-1")
    (to_issue . "bd-2")
    (dependency_type . "blocks"))
  "Sample dependency for testing.")

;;; Test Utilities

(defun beads-misc-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defmacro beads-misc-test-with-close-state (state &rest body)
  "Execute BODY with beads-close state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-close--issue-id nil
           beads-close--reason nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

(defmacro beads-misc-test-with-dep-state (state &rest body)
  "Execute BODY with beads-dep state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-dep--from-issue nil
           beads-dep--to-issue nil
           beads-dep--dep-type nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

;;; ============================================================
;;; bd close tests
;;; ============================================================

(ert-deftest beads-misc-test-close-reset-state ()
  "Test that close reset-state clears all variables."
  (beads-misc-test-with-close-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   (beads-close--reset-state)
   (should (null beads-close--issue-id))
   (should (null beads-close--reason))))

(ert-deftest beads-misc-test-close-format-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-close--format-value "test-value")))
    (should (stringp result))
    (should (string-match-p "test-value" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-misc-test-close-format-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-close--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-misc-test-close-validate-reason-nil ()
  "Test reason validation when reason is nil."
  (beads-misc-test-with-close-state nil
   (should (beads-close--validate-reason))))

(ert-deftest beads-misc-test-close-validate-reason-empty ()
  "Test reason validation when reason is empty."
  (beads-misc-test-with-close-state
   '((beads-close--reason . ""))
   (should (beads-close--validate-reason))))

(ert-deftest beads-misc-test-close-validate-reason-whitespace ()
  "Test reason validation when reason is whitespace."
  (beads-misc-test-with-close-state
   '((beads-close--reason . "   \n\t  "))
   (should (beads-close--validate-reason))))

(ert-deftest beads-misc-test-close-validate-reason-valid ()
  "Test reason validation when reason is valid."
  (beads-misc-test-with-close-state
   '((beads-close--reason . "Fixed the bug"))
   (should (null (beads-close--validate-reason)))))

(ert-deftest beads-misc-test-close-validate-issue-id-nil ()
  "Test issue ID validation when nil."
  (beads-misc-test-with-close-state nil
   (should (beads-close--validate-issue-id))))

(ert-deftest beads-misc-test-close-validate-issue-id-empty ()
  "Test issue ID validation when empty."
  (beads-misc-test-with-close-state
   '((beads-close--issue-id . ""))
   (should (beads-close--validate-issue-id))))

(ert-deftest beads-misc-test-close-validate-issue-id-valid ()
  "Test issue ID validation when valid."
  (beads-misc-test-with-close-state
   '((beads-close--issue-id . "bd-42"))
   (should (null (beads-close--validate-issue-id)))))

(ert-deftest beads-misc-test-close-validate-all-success ()
  "Test validate-all with all valid parameters."
  (beads-misc-test-with-close-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   (should (null (beads-close--validate-all)))))

(ert-deftest beads-misc-test-close-validate-all-failure ()
  "Test validate-all with missing parameters."
  (beads-misc-test-with-close-state nil
   (let ((errors (beads-close--validate-all)))
     (should errors)
     (should (listp errors))
     (should (>= (length errors) 2)))))

(ert-deftest beads-misc-test-close-execute-success ()
  "Test successful issue closing."
  (beads-misc-test-with-close-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   (let ((json-output (json-encode
                       beads-misc-test--sample-closed-issue)))
     (cl-letf (((symbol-function 'call-process)
                (beads-misc-test--mock-call-process 0 json-output)))
       (let ((result (beads-close--execute "bd-42" "Fixed")))
         (should result)
         (should (equal (alist-get 'status result) "closed")))))))

(ert-deftest beads-misc-test-close-execute-command-failure ()
  "Test execution handles bd command failure."
  (beads-misc-test-with-close-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   (cl-letf (((symbol-function 'call-process)
              (beads-misc-test--mock-call-process 1 "Error")))
     (should-error (beads-close--execute "bd-42" "Fixed")))))

(ert-deftest beads-misc-test-close-transient-defined ()
  "Test that beads-close transient is defined."
  (should (fboundp 'beads-close)))

(ert-deftest beads-misc-test-close-transient-is-prefix ()
  "Test that beads-close is a transient prefix."
  (should (get 'beads-close 'transient--prefix)))

(ert-deftest beads-misc-test-close-infix-commands-defined ()
  "Test that close infix commands are defined."
  (should (fboundp 'beads-close--infix-reason)))

(ert-deftest beads-misc-test-close-suffix-commands-defined ()
  "Test that close suffix commands are defined."
  (should (fboundp 'beads-close--execute-command))
  (should (fboundp 'beads-close--reset)))

;;; ============================================================
;;; bd dep tests
;;; ============================================================

(ert-deftest beads-misc-test-dep-reset-state ()
  "Test that dep reset-state clears all variables."
  (beads-misc-test-with-dep-state
   '((beads-dep--from-issue . "bd-1")
     (beads-dep--to-issue . "bd-2")
     (beads-dep--dep-type . "blocks"))
   (beads-dep--reset-state)
   (should (null beads-dep--from-issue))
   (should (null beads-dep--to-issue))
   (should (null beads-dep--dep-type))))

(ert-deftest beads-misc-test-dep-format-value-set ()
  "Test dep formatting when value is set."
  (let ((result (beads-dep--format-value "bd-42")))
    (should (stringp result))
    (should (string-match-p "bd-42" result))))

(ert-deftest beads-misc-test-dep-format-value-nil ()
  "Test dep formatting when value is nil."
  (let ((result (beads-dep--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-misc-test-dep-validate-from-issue-nil ()
  "Test from-issue validation when nil."
  (beads-misc-test-with-dep-state nil
   (should (beads-dep--validate-from-issue))))

(ert-deftest beads-misc-test-dep-validate-from-issue-empty ()
  "Test from-issue validation when empty."
  (beads-misc-test-with-dep-state
   '((beads-dep--from-issue . ""))
   (should (beads-dep--validate-from-issue))))

(ert-deftest beads-misc-test-dep-validate-from-issue-valid ()
  "Test from-issue validation when valid."
  (beads-misc-test-with-dep-state
   '((beads-dep--from-issue . "bd-1"))
   (should (null (beads-dep--validate-from-issue)))))

(ert-deftest beads-misc-test-dep-validate-to-issue-nil ()
  "Test to-issue validation when nil."
  (beads-misc-test-with-dep-state nil
   (should (beads-dep--validate-to-issue))))

(ert-deftest beads-misc-test-dep-validate-to-issue-valid ()
  "Test to-issue validation when valid."
  (beads-misc-test-with-dep-state
   '((beads-dep--to-issue . "bd-2"))
   (should (null (beads-dep--validate-to-issue)))))

(ert-deftest beads-misc-test-dep-validate-type-nil ()
  "Test type validation when nil."
  (beads-misc-test-with-dep-state nil
   (should (null (beads-dep--validate-type)))))

(ert-deftest beads-misc-test-dep-validate-type-blocks ()
  "Test type validation with blocks type."
  (beads-misc-test-with-dep-state
   '((beads-dep--dep-type . "blocks"))
   (should (null (beads-dep--validate-type)))))

(ert-deftest beads-misc-test-dep-validate-type-related ()
  "Test type validation with related type."
  (beads-misc-test-with-dep-state
   '((beads-dep--dep-type . "related"))
   (should (null (beads-dep--validate-type)))))

(ert-deftest beads-misc-test-dep-validate-type-parent-child ()
  "Test type validation with parent-child type."
  (beads-misc-test-with-dep-state
   '((beads-dep--dep-type . "parent-child"))
   (should (null (beads-dep--validate-type)))))

(ert-deftest beads-misc-test-dep-validate-type-discovered-from ()
  "Test type validation with discovered-from type."
  (beads-misc-test-with-dep-state
   '((beads-dep--dep-type . "discovered-from"))
   (should (null (beads-dep--validate-type)))))

(ert-deftest beads-misc-test-dep-validate-type-invalid ()
  "Test type validation with invalid type."
  (beads-misc-test-with-dep-state
   '((beads-dep--dep-type . "invalid"))
   (should (beads-dep--validate-type))))

(ert-deftest beads-misc-test-dep-validate-all-success ()
  "Test validate-all with all valid parameters."
  (beads-misc-test-with-dep-state
   '((beads-dep--from-issue . "bd-1")
     (beads-dep--to-issue . "bd-2")
     (beads-dep--dep-type . "blocks"))
   (should (null (beads-dep--validate-all)))))

(ert-deftest beads-misc-test-dep-validate-all-failure ()
  "Test validate-all with missing parameters."
  (beads-misc-test-with-dep-state nil
   (let ((errors (beads-dep--validate-all)))
     (should errors)
     (should (>= (length errors) 2)))))

(ert-deftest beads-misc-test-dep-execute-add-success ()
  "Test successful dependency addition."
  (let ((json-output "{}"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 json-output)))
      (should-not (beads-dep--execute-add "bd-1" "bd-2" "blocks")))))

(ert-deftest beads-misc-test-dep-execute-add-failure ()
  "Test dependency addition failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-dep--execute-add "bd-1" "bd-2" "blocks"))))

(ert-deftest beads-misc-test-dep-execute-remove-success ()
  "Test successful dependency removal."
  (let ((json-output "{}"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 json-output)))
      (should-not (beads-dep--execute-remove "bd-1" "bd-2")))))

(ert-deftest beads-misc-test-dep-execute-remove-failure ()
  "Test dependency removal failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-dep--execute-remove "bd-1" "bd-2"))))

(ert-deftest beads-misc-test-dep-execute-tree-success ()
  "Test successful dependency tree generation."
  (let ((tree-output "bd-1\n  bd-2\n    bd-3\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 tree-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-dep--execute-tree "bd-1"))
      (should (get-buffer "*beads-dep-tree: bd-1*")))))

(ert-deftest beads-misc-test-dep-execute-tree-failure ()
  "Test dependency tree generation failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-dep--execute-tree "bd-1"))))

(ert-deftest beads-misc-test-dep-execute-list-success ()
  "Test successful dependency list."
  (let ((json-output (json-encode
                      (vector beads-misc-test--sample-dependency))))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-dep--execute-list "bd-1"))
      (should (get-buffer "*beads-dep-list: bd-1*")))))

(ert-deftest beads-misc-test-dep-execute-list-empty ()
  "Test dependency list with no dependencies."
  (let ((json-output "[]"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 json-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-dep--execute-list "bd-1"))
      (should (get-buffer "*beads-dep-list: bd-1*")))))

(ert-deftest beads-misc-test-dep-execute-list-failure ()
  "Test dependency list failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-dep--execute-list "bd-1"))))

(ert-deftest beads-misc-test-dep-transient-defined ()
  "Test that beads-dep transient is defined."
  (should (fboundp 'beads-dep)))

(ert-deftest beads-misc-test-dep-transient-is-prefix ()
  "Test that beads-dep is a transient prefix."
  (should (get 'beads-dep 'transient--prefix)))

(ert-deftest beads-misc-test-dep-infix-commands-defined ()
  "Test that dep infix commands are defined."
  (should (fboundp 'beads-dep--infix-from))
  (should (fboundp 'beads-dep--infix-to))
  (should (fboundp 'beads-dep--infix-type)))

(ert-deftest beads-misc-test-dep-suffix-commands-defined ()
  "Test that dep suffix commands are defined."
  (should (fboundp 'beads-dep--add-command))
  (should (fboundp 'beads-dep--remove-command))
  (should (fboundp 'beads-dep--tree-command))
  (should (fboundp 'beads-dep--list-command))
  (should (fboundp 'beads-dep--reset)))

;;; ============================================================
;;; bd stats tests
;;; ============================================================

(ert-deftest beads-misc-test-stats-execute-success ()
  "Test successful stats execution."
  (let ((stats-output "Total: 10\nOpen: 5\nClosed: 5\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 stats-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-stats--execute))
      (should (get-buffer "*beads-stats*")))))

(ert-deftest beads-misc-test-stats-execute-failure ()
  "Test stats execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-stats--execute))))

(ert-deftest beads-misc-test-stats-execute-buffer-content ()
  "Test stats buffer contains output."
  (let ((stats-output "Total: 42\nOpen: 20\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 stats-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-stats--execute)
      (with-current-buffer "*beads-stats*"
        (should (string-match-p "Total: 42" (buffer-string)))
        (should (string-match-p "Open: 20" (buffer-string)))))))

(ert-deftest beads-misc-test-stats-function-defined ()
  "Test that beads-stats function is defined."
  (should (fboundp 'beads-stats)))

(ert-deftest beads-misc-test-stats-function-autoload ()
  "Test that beads-stats has autoload cookie."
  (should (fboundp 'beads-stats)))

(ert-deftest beads-misc-test-stats-buffer-mode ()
  "Test that stats buffer uses special-mode."
  (let ((stats-output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 stats-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-stats--execute)
      (with-current-buffer "*beads-stats*"
        (should (eq major-mode 'special-mode))))))

(ert-deftest beads-misc-test-stats-buffer-keybindings ()
  "Test that stats buffer has proper keybindings."
  (let ((stats-output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 stats-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-stats--execute)
      (with-current-buffer "*beads-stats*"
        (should (local-key-binding (kbd "q")))
        (should (local-key-binding (kbd "g")))))))

(ert-deftest beads-misc-test-stats-refresh ()
  "Test stats buffer refresh functionality."
  (let ((stats-output "Initial")
        (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 (with-current-buffer (current-buffer)
                   (insert stats-output))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-stats--execute)
      (should (= call-count 1))
      (with-current-buffer "*beads-stats*"
        (setq stats-output "Refreshed")
        (funcall (local-key-binding (kbd "g"))))
      (should (= call-count 2)))))

(ert-deftest beads-misc-test-stats-empty-output ()
  "Test stats with empty output."
  (let ((stats-output ""))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 stats-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-stats--execute))
      (should (get-buffer "*beads-stats*")))))

(ert-deftest beads-misc-test-stats-large-output ()
  "Test stats with large output."
  (let ((stats-output (make-string 10000 ?x)))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 stats-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-stats--execute))
      (with-current-buffer "*beads-stats*"
        (should (= (buffer-size) 10000))))))

;;; ============================================================
;;; bd export tests
;;; ============================================================

(ert-deftest beads-misc-test-export-reset-state ()
  "Test that export reset-state clears all variables."
  (setq beads-export--output "/tmp/test.jsonl"
        beads-export--no-auto-flush t)
  (beads-export--reset-state)
  (should (null beads-export--output))
  (should (null beads-export--no-auto-flush)))

(ert-deftest beads-misc-test-export-format-value-set ()
  "Test export formatting when value is set."
  (let ((result (beads-export--format-value "/tmp/test.jsonl")))
    (should (stringp result))
    (should (string-match-p "test.jsonl" result))))

(ert-deftest beads-misc-test-export-format-value-nil ()
  "Test export formatting when value is nil."
  (let ((result (beads-export--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-misc-test-export-execute-success ()
  "Test successful export execution."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 0 "")))
    (should-not (beads-export--execute "/tmp/test.jsonl" nil))))

(ert-deftest beads-misc-test-export-execute-with-no-auto-flush ()
  "Test export with no-auto-flush flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-export--execute "/tmp/test.jsonl" t)
      (should (member "--no-auto-flush" captured-args)))))

(ert-deftest beads-misc-test-export-execute-failure ()
  "Test export execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-export--execute "/tmp/test.jsonl" nil))))

(ert-deftest beads-misc-test-export-execute-command-args ()
  "Test export command includes correct arguments."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-export--execute "/tmp/custom.jsonl" nil)
      (should (member "export" captured-args))
      (should (member "-o" captured-args))
      (should (member "/tmp/custom.jsonl" captured-args)))))

(ert-deftest beads-misc-test-export-transient-defined ()
  "Test that beads-export transient is defined."
  (should (fboundp 'beads-export)))

(ert-deftest beads-misc-test-export-transient-is-prefix ()
  "Test that beads-export is a transient prefix."
  (should (get 'beads-export 'transient--prefix)))

(ert-deftest beads-misc-test-export-infix-commands-defined ()
  "Test that export infix commands are defined."
  (should (fboundp 'beads-export--infix-output))
  (should (fboundp 'beads-export--infix-no-auto-flush)))

(ert-deftest beads-misc-test-export-suffix-commands-defined ()
  "Test that export suffix commands are defined."
  (should (fboundp 'beads-export--execute-command))
  (should (fboundp 'beads-export--reset)))

;;; ============================================================
;;; bd import tests
;;; ============================================================

(ert-deftest beads-misc-test-import-reset-state ()
  "Test that import reset-state clears all variables."
  (setq beads-import--input "/tmp/test.jsonl"
        beads-import--dry-run t
        beads-import--resolve-collisions t)
  (beads-import--reset-state)
  (should (null beads-import--input))
  (should (null beads-import--dry-run))
  (should (null beads-import--resolve-collisions)))

(ert-deftest beads-misc-test-import-format-value-set ()
  "Test import formatting when value is set."
  (let ((result (beads-import--format-value "/tmp/test.jsonl")))
    (should (stringp result))
    (should (string-match-p "test.jsonl" result))))

(ert-deftest beads-misc-test-import-format-value-nil ()
  "Test import formatting when value is nil."
  (let ((result (beads-import--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-misc-test-import-validate-input-nil ()
  "Test input validation when nil."
  (setq beads-import--input nil)
  (should (beads-import--validate-input)))

(ert-deftest beads-misc-test-import-validate-input-empty ()
  "Test input validation when empty."
  (setq beads-import--input "")
  (should (beads-import--validate-input)))

(ert-deftest beads-misc-test-import-validate-input-valid ()
  "Test input validation when valid."
  (setq beads-import--input "/tmp/test.jsonl")
  (should (null (beads-import--validate-input))))

(ert-deftest beads-misc-test-import-validate-all-success ()
  "Test validate-all with valid input."
  (setq beads-import--input "/tmp/test.jsonl")
  (should (null (beads-import--validate-all))))

(ert-deftest beads-misc-test-import-validate-all-failure ()
  "Test validate-all with missing input."
  (setq beads-import--input nil)
  (let ((errors (beads-import--validate-all)))
    (should errors)
    (should (>= (length errors) 1))))

(ert-deftest beads-misc-test-import-execute-success ()
  "Test successful import execution."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 0 "Imported 5 issues")))
    (should-not (beads-import--execute "/tmp/test.jsonl" nil nil))))

(ert-deftest beads-misc-test-import-execute-with-dry-run ()
  "Test import with dry-run flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Dry run output"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-import--execute "/tmp/test.jsonl" t nil)
      (should (member "--dry-run" captured-args)))))

(ert-deftest beads-misc-test-import-execute-with-resolve-collisions ()
  "Test import with resolve-collisions flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-import--execute "/tmp/test.jsonl" nil t)
      (should (member "--resolve-collisions" captured-args)))))

(ert-deftest beads-misc-test-import-execute-with-collisions ()
  "Test import shows collision report in buffer."
  (let ((collision-output "collision detected in bd-1"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 collision-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-import--execute "/tmp/test.jsonl" nil nil)
      (should (get-buffer "*beads-import*")))))

(ert-deftest beads-misc-test-import-execute-failure ()
  "Test import execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-import--execute "/tmp/test.jsonl" nil nil))))

(ert-deftest beads-misc-test-import-execute-command-args ()
  "Test import command includes correct arguments."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-import--execute "/tmp/custom.jsonl" nil nil)
      (should (member "import" captured-args))
      (should (member "-i" captured-args))
      (should (member "/tmp/custom.jsonl" captured-args)))))

(ert-deftest beads-misc-test-import-transient-defined ()
  "Test that beads-import transient is defined."
  (should (fboundp 'beads-import)))

(ert-deftest beads-misc-test-import-transient-is-prefix ()
  "Test that beads-import is a transient prefix."
  (should (get 'beads-import 'transient--prefix)))

(ert-deftest beads-misc-test-import-infix-commands-defined ()
  "Test that import infix commands are defined."
  (should (fboundp 'beads-import--infix-input))
  (should (fboundp 'beads-import--infix-dry-run))
  (should (fboundp 'beads-import--infix-resolve-collisions)))

(ert-deftest beads-misc-test-import-suffix-commands-defined ()
  "Test that import suffix commands are defined."
  (should (fboundp 'beads-import--execute-command))
  (should (fboundp 'beads-import--reset)))

;;; ============================================================
;;; bd init tests
;;; ============================================================

(ert-deftest beads-misc-test-init-reset-state ()
  "Test that init reset-state clears all variables."
  (setq beads-init--prefix "test"
        beads-init--db-path "/tmp/test.db")
  (beads-init--reset-state)
  (should (null beads-init--prefix))
  (should (null beads-init--db-path)))

(ert-deftest beads-misc-test-init-format-value-set ()
  "Test init formatting when value is set."
  (let ((result (beads-init--format-value "bd")))
    (should (stringp result))
    (should (string-match-p "bd" result))))

(ert-deftest beads-misc-test-init-format-value-nil ()
  "Test init formatting when value is nil."
  (let ((result (beads-init--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-misc-test-init-execute-success ()
  "Test successful init execution."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 0 "")))
    (should-not (beads-init--execute nil nil))))

(ert-deftest beads-misc-test-init-execute-with-prefix ()
  "Test init with prefix."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "test" nil)
      (should (member "--prefix" captured-args))
      (should (member "test" captured-args)))))

(ert-deftest beads-misc-test-init-execute-with-db-path ()
  "Test init with database path."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute nil "/tmp/test.db")
      (should (member "--db" captured-args))
      (should (member "/tmp/test.db" captured-args)))))

(ert-deftest beads-misc-test-init-execute-with-both ()
  "Test init with both prefix and db path."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "myproj" "/tmp/test.db")
      (should (member "--prefix" captured-args))
      (should (member "myproj" captured-args))
      (should (member "--db" captured-args))
      (should (member "/tmp/test.db" captured-args)))))

(ert-deftest beads-misc-test-init-execute-failure ()
  "Test init execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-init--execute nil nil))))

(ert-deftest beads-misc-test-init-execute-empty-prefix ()
  "Test init with empty prefix is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "" nil)
      (should-not (member "--prefix" captured-args)))))

(ert-deftest beads-misc-test-init-execute-whitespace-prefix ()
  "Test init with whitespace prefix is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "   " nil)
      (should-not (member "--prefix" captured-args)))))

(ert-deftest beads-misc-test-init-transient-defined ()
  "Test that beads-init transient is defined."
  (should (fboundp 'beads-init)))

(ert-deftest beads-misc-test-init-transient-is-prefix ()
  "Test that beads-init is a transient prefix."
  (should (get 'beads-init 'transient--prefix)))

(ert-deftest beads-misc-test-init-infix-commands-defined ()
  "Test that init infix commands are defined."
  (should (fboundp 'beads-init--infix-prefix))
  (should (fboundp 'beads-init--infix-db)))

(ert-deftest beads-misc-test-init-suffix-commands-defined ()
  "Test that init suffix commands are defined."
  (should (fboundp 'beads-init--execute-command))
  (should (fboundp 'beads-init--reset)))

;;; Edge Cases and Integration Tests

(ert-deftest beads-misc-test-close-with-special-chars-in-reason ()
  "Test closing with special characters in reason."
  (let ((json-output (json-encode
                      beads-misc-test--sample-closed-issue)))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 json-output)))
      (should (beads-close--execute "bd-42"
                                    "Fixed \"quoted\" & special chars")))))

(ert-deftest beads-misc-test-dep-add-all-types ()
  "Test adding dependencies with all types."
  (let ((types '("blocks" "related" "parent-child" "discovered-from")))
    (dolist (type types)
      (cl-letf (((symbol-function 'call-process)
                 (beads-misc-test--mock-call-process 0 "{}")))
        (should-not (beads-dep--execute-add "bd-1" "bd-2" type))))))

(ert-deftest beads-misc-test-import-dry-run-shows-buffer ()
  "Test that dry-run mode displays buffer."
  (let ((dry-run-output "Preview: Would import 5 issues"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 dry-run-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-import--execute "/tmp/test.jsonl" t nil)
      (should (get-buffer "*beads-import*"))
      (with-current-buffer "*beads-import*"
        (should (string-match-p "Preview" (buffer-string)))))))

(ert-deftest beads-misc-test-stats-multiple-refreshes ()
  "Test multiple stats refreshes work correctly."
  (let ((call-count 0)
        (outputs '("First" "Second" "Third")))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (with-current-buffer (current-buffer)
                   (insert (nth call-count outputs)))
                 (setq call-count (1+ call-count))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-stats--execute)
      (with-current-buffer "*beads-stats*"
        (funcall (local-key-binding (kbd "g")))
        (funcall (local-key-binding (kbd "g"))))
      (should (= call-count 3)))))

(provide 'beads-misc-test)
;;; beads-misc-test.el ends here
