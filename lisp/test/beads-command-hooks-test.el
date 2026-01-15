;;; beads-command-hooks-test.el --- Tests for beads-command-hooks -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-hooks command classes.

;;; Code:

(require 'ert)
(require 'beads-command-hooks)

;;; Unit Tests: beads-command-hooks-install command-line

(ert-deftest beads-command-hooks-install-test-command-line-basic ()
  "Unit test: hooks install builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-hooks-install))
         (args (beads-command-line cmd)))
    (should (member "hooks" args))
    (should (member "install" args))))

(ert-deftest beads-command-hooks-install-test-command-line-force ()
  "Unit test: hooks install includes --force option."
  :tags '(:unit)
  (let* ((cmd (beads-command-hooks-install :force t))
         (args (beads-command-line cmd)))
    (should (member "--force" args))))

;;; Unit Tests: beads-command-hooks-uninstall command-line

(ert-deftest beads-command-hooks-uninstall-test-command-line-basic ()
  "Unit test: hooks uninstall builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-hooks-uninstall))
         (args (beads-command-line cmd)))
    (should (member "hooks" args))
    (should (member "uninstall" args))))

;;; Unit Tests: beads-command-hooks-list command-line

(ert-deftest beads-command-hooks-list-test-command-line-basic ()
  "Unit test: hooks list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-hooks-list))
         (args (beads-command-line cmd)))
    (should (member "hooks" args))
    (should (member "list" args))))

;;; Unit Tests: beads-command-hooks-run command-line

(ert-deftest beads-command-hooks-run-test-command-line-basic ()
  "Unit test: hooks run builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-hooks-run :hook-name "post-commit"))
         (args (beads-command-line cmd)))
    (should (member "hooks" args))
    (should (member "run" args))
    (should (member "post-commit" args))))

;; Note: beads-command-hooks-run doesn't have custom validation

(provide 'beads-command-hooks-test)
;;; beads-command-hooks-test.el ends here
