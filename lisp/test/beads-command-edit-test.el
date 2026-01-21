;;; beads-command-edit-test.el --- Tests for beads-command-edit -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-edit command classes.

;;; Code:

(require 'ert)
(require 'beads-command-edit)

;;; Unit Tests: beads-command-edit command-line

(ert-deftest beads-command-edit-test-command-line-basic ()
  "Unit test: edit builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-edit :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "edit" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-edit-test-command-line-description ()
  "Unit test: edit includes --description flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-edit :issue-id "bd-1" :description t))
         (args (beads-command-line cmd)))
    (should (member "bd-1" args))
    (should (member "--description" args))))

(ert-deftest beads-command-edit-test-command-line-title ()
  "Unit test: edit includes --title flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-edit :issue-id "bd-1" :title t))
         (args (beads-command-line cmd)))
    (should (member "bd-1" args))
    (should (member "--title" args))))

(ert-deftest beads-command-edit-test-validation-missing-issue-id ()
  "Unit test: edit validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-edit)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-edit-test-validation-success ()
  "Unit test: edit validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-edit :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for transient menu and interactive function

(ert-deftest beads-command-edit-test-detect-issue-id-not-in-context ()
  "Unit test: detect-issue-id returns nil when not in beads buffer."
  :tags '(:unit)
  (with-temp-buffer
    (should (null (beads-edit--detect-issue-id)))))

(ert-deftest beads-command-edit-test-detect-issue-id-from-list-mode ()
  "Unit test: detect-issue-id returns issue ID from beads-list buffer."
  :tags '(:unit)
  (with-temp-buffer
    (let ((beads-list-mode-hook nil))
      (beads-list-mode)
      ;; Mock beads-list--current-issue-id
      (cl-letf (((symbol-function 'beads-list--current-issue-id)
                 (lambda () "bd-42")))
        (should (equal "bd-42" (beads-edit--detect-issue-id)))))))

(ert-deftest beads-command-edit-test-detect-issue-id-from-show-mode ()
  "Unit test: detect-issue-id returns issue ID from beads-show buffer."
  :tags '(:unit)
  (with-temp-buffer
    (let ((beads-show-mode-hook nil)
          (beads-show--issue-id "bd-99"))
      (beads-show-mode)
      (should (equal "bd-99" (beads-edit--detect-issue-id))))))

(ert-deftest beads-command-edit-test-transient-menu-defined ()
  "Unit test: transient menu beads-edit--menu is defined."
  :tags '(:unit)
  (should (fboundp 'beads-edit--menu)))

(ert-deftest beads-command-edit-test-interactive-function-defined ()
  "Unit test: interactive function beads-edit is defined."
  :tags '(:unit)
  (should (fboundp 'beads-edit)))

(ert-deftest beads-command-edit-test-interactive-function-is-interactive ()
  "Unit test: beads-edit is an interactive command."
  :tags '(:unit)
  (should (commandp 'beads-edit)))

(ert-deftest beads-command-edit-test-parse-transient-args-basic ()
  "Unit test: parse-transient-args extracts issue ID."
  :tags '(:unit)
  (let ((cmd (beads-edit--parse-transient-args '("--id=bd-1"))))
    (should (equal "bd-1" (oref cmd issue-id)))))

(ert-deftest beads-command-edit-test-parse-transient-args-with-title ()
  "Unit test: parse-transient-args extracts title flag."
  :tags '(:unit)
  (let ((cmd (beads-edit--parse-transient-args '("--id=bd-1" "--title"))))
    (should (equal "bd-1" (oref cmd issue-id)))
    (should (oref cmd title))))

(ert-deftest beads-command-edit-test-parse-transient-args-with-description ()
  "Unit test: parse-transient-args extracts description flag."
  :tags '(:unit)
  (let ((cmd (beads-edit--parse-transient-args
              '("--id=bd-1" "--description"))))
    (should (equal "bd-1" (oref cmd issue-id)))
    (should (oref cmd description))))

(ert-deftest beads-command-edit-test-validate-all-missing-issue-id ()
  "Unit test: validate-all returns error when issue-id is missing."
  :tags '(:unit)
  (let ((cmd (beads-command-edit)))
    (should (member "Issue ID is required" (beads-edit--validate-all cmd)))))

(ert-deftest beads-command-edit-test-validate-all-success ()
  "Unit test: validate-all returns empty list with valid issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-edit :issue-id "bd-1")))
    (should (null (beads-edit--validate-all cmd)))))

(ert-deftest beads-command-edit-test-suffix-execute-defined ()
  "Unit test: execute suffix is defined."
  :tags '(:unit)
  (should (fboundp 'beads-edit--execute)))

(ert-deftest beads-command-edit-test-suffix-preview-defined ()
  "Unit test: preview suffix is defined."
  :tags '(:unit)
  (should (fboundp 'beads-edit--preview)))

(ert-deftest beads-command-edit-test-suffix-reset-defined ()
  "Unit test: reset suffix is defined."
  :tags '(:unit)
  (should (fboundp 'beads-edit--reset)))

(ert-deftest beads-command-edit-test-infix-issue-id-defined ()
  "Unit test: issue-id infix is defined."
  :tags '(:unit)
  (should (fboundp 'beads-edit--infix-issue-id)))

(ert-deftest beads-command-edit-test-auto-generated-transient-defined ()
  "Unit test: auto-generated transient beads-edit-transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-edit-transient)))

(provide 'beads-command-edit-test)
;;; beads-command-edit-test.el ends here
