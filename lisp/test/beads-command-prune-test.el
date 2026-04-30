;;; beads-command-prune-test.el --- Tests for beads-command-prune -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-prune command class.

;;; Code:

(require 'ert)
(require 'beads-command-prune)

(ert-deftest beads-command-prune-test-class-exists ()
  "Unit test: beads-command-prune class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-prune)))

(ert-deftest beads-command-prune-test-command-line-basic ()
  "Unit test: prune builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-prune :older-than "30d"))
         (args (beads-command-line cmd)))
    (should (member "prune" args))))

(ert-deftest beads-command-prune-test-command-line-dry-run ()
  "Unit test: prune --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-prune :dry-run t :older-than "30d"))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-prune-test-command-line-force ()
  "Unit test: prune --force flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-prune :force t :older-than "30d"))
         (args (beads-command-line cmd)))
    (should (member "--force" args))))

(ert-deftest beads-command-prune-test-command-line-older-than ()
  "Unit test: prune --older-than flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-prune :older-than "30d"))
         (args (beads-command-line cmd)))
    (should (member "--older-than" args))
    (should (member "30d" args))))

(ert-deftest beads-command-prune-test-command-line-pattern ()
  "Unit test: prune --pattern flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-prune :pattern "gm-old-*"))
         (args (beads-command-line cmd)))
    (should (member "--pattern" args))
    (should (member "gm-old-*" args))))

(ert-deftest beads-command-prune-test-validation-missing-filter ()
  "Unit test: prune validation requires --older-than or --pattern."
  :tags '(:unit)
  (let ((cmd (beads-command-prune)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-prune-test-validation-with-older-than ()
  "Unit test: prune validation succeeds with --older-than."
  :tags '(:unit)
  (let ((cmd (beads-command-prune :older-than "30d")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-prune-test-validation-with-pattern ()
  "Unit test: prune validation succeeds with --pattern."
  :tags '(:unit)
  (let ((cmd (beads-command-prune :pattern "gm-*")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-prune-test-transient-defined ()
  "Unit test: beads-prune transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-prune)))

(provide 'beads-command-prune-test)
;;; beads-command-prune-test.el ends here
