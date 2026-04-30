;;; beads-command-batch-test.el --- Tests for beads-command-batch -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-batch command class.

;;; Code:

(require 'ert)
(require 'beads-command-batch)

(ert-deftest beads-command-batch-test-class-exists ()
  "Unit test: beads-command-batch class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-batch)))

(ert-deftest beads-command-batch-test-command-line-basic ()
  "Unit test: batch builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch))
         (args (beads-command-line cmd)))
    (should (member "batch" args))))

(ert-deftest beads-command-batch-test-command-line-dry-run ()
  "Unit test: batch --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-batch-test-command-line-file ()
  "Unit test: batch --file flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch :file "/tmp/cmds.txt"))
         (args (beads-command-line cmd)))
    (should (member "--file" args))
    (should (member "/tmp/cmds.txt" args))))

(ert-deftest beads-command-batch-test-command-line-message ()
  "Unit test: batch --message flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-batch :message "bd: bulk import"))
         (args (beads-command-line cmd)))
    (should (member "--message" args))
    (should (member "bd: bulk import" args))))

(ert-deftest beads-command-batch-test-transient-defined ()
  "Unit test: beads-batch transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-batch)))

(provide 'beads-command-batch-test)
;;; beads-command-batch-test.el ends here
