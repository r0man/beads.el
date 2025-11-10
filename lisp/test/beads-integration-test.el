;;; beads-integration-test.el --- Integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Integration tests for beads.el that test full workflows using
;; beads-test-helper-run-commands to simulate user interaction.

;;; Code:

(require 'ert)
(require 'beads-test-helper)

;;; ============================================================
;;; Test Helper Verification
;;; ============================================================

(ert-deftest beads-integration-test-run-commands-basic ()
  "Test that beads-test-helper-run-commands executes commands."
  (with-temp-buffer
    (let ((test-var nil))
      ;; Define a simple test command
      (defun beads-test--dummy-command ()
        (interactive)
        (setq test-var 'executed))

      ;; Run it through the helper
      (beads-test-helper-run-commands '(beads-test--dummy-command))

      ;; Verify it was executed
      (should (eq test-var 'executed)))))

(ert-deftest beads-integration-test-run-commands-sets-command-vars ()
  "Test that beads-test-helper-run-commands sets command variables."
  (with-temp-buffer
    (let ((captured-this-command nil)
          (captured-last-command nil))
      ;; Define a test command that captures command variables
      (defun beads-test--capture-command ()
        (interactive)
        (setq captured-this-command this-command)
        (setq captured-last-command last-command))

      ;; Set initial this-command
      (setq this-command 'initial-command)

      ;; Run the command
      (beads-test-helper-run-commands '(beads-test--capture-command))

      ;; Verify this-command was set correctly
      (should (eq captured-this-command 'beads-test--capture-command))
      ;; Verify last-command was set to previous this-command
      (should (eq captured-last-command 'initial-command)))))

(ert-deftest beads-integration-test-run-commands-multiple ()
  "Test that beads-test-helper-run-commands handles sequences."
  (with-temp-buffer
    (let ((execution-order nil))
      ;; Define test commands
      (defun beads-test--cmd-1 ()
        (interactive)
        (push 'cmd-1 execution-order))

      (defun beads-test--cmd-2 ()
        (interactive)
        (push 'cmd-2 execution-order))

      (defun beads-test--cmd-3 ()
        (interactive)
        (push 'cmd-3 execution-order))

      ;; Run sequence
      (beads-test-helper-run-commands
       '(beads-test--cmd-1
         beads-test--cmd-2
         beads-test--cmd-3))

      ;; Verify execution order (reversed because we pushed)
      (should (equal execution-order '(cmd-3 cmd-2 cmd-1))))))

(provide 'beads-integration-test)
;;; beads-integration-test.el ends here
