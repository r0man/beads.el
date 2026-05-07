;;; beads-command-ping-test.el --- Tests for beads-command-ping -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-ping command class.

;;; Code:

(require 'ert)
(require 'beads-command-ping)

(ert-deftest beads-command-ping-test-class-exists ()
  "Unit test: beads-command-ping class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-ping)))

(ert-deftest beads-command-ping-test-command-line-basic ()
  "Unit test: ping builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-ping))
         (args (beads-command-line cmd)))
    (should (member "ping" args))))

(ert-deftest beads-command-ping-test-validation-always-valid ()
  "Unit test: ping has no required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-ping)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-ping-test-transient-defined ()
  "Unit test: beads-ping transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-ping)))

(provide 'beads-command-ping-test)
;;; beads-command-ping-test.el ends here
