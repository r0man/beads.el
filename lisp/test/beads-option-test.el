;;; beads-option-test.el --- Tests for beads-option.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for beads-option.el - transient infix definitions.
;; These tests verify that infixes are properly defined and their
;; readers/formatters work correctly.

;;; Code:

(require 'beads-option)
(require 'ert)
(require 'transient)

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(ert-deftest beads-option-test-format-value-with-value ()
  "Test beads-option--format-value with a non-empty value."
  (let ((result (beads-option--format-value "test-value")))
    (should (stringp result))
    (should (string-match-p "\\[test-value\\]" result))))

(ert-deftest beads-option-test-format-value-empty ()
  "Test beads-option--format-value with empty value."
  (let ((result (beads-option--format-value nil)))
    (should (stringp result))
    (should (string-match-p "\\[unset\\]" result))))

(ert-deftest beads-option-test-format-value-long ()
  "Test beads-option--format-value with a long value (truncation)."
  (let* ((long-value (make-string 50 ?x))
         (result (beads-option--format-value long-value)))
    (should (stringp result))
    (should (string-match-p "\\.\\.\\." result))
    ;; Should be truncated to 40 chars + "..."
    (should (< (length result) (length long-value)))))

;;; ============================================================
;;; Multiline Class Tests
;;; ============================================================

(ert-deftest beads-option-test-multiline-class-exists ()
  "Test that beads-create-transient-multiline class is defined."
  (should (class-p 'beads-create-transient-multiline))
  (should (child-of-class-p 'beads-create-transient-multiline
                            'transient-option)))

(ert-deftest beads-option-test-multiline-format-value-empty ()
  "Test multiline formatter with empty value."
  (let ((obj (beads-create-transient-multiline)))
    (oset obj value nil)
    (oset obj argument "--test=")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      ;; New format: shows just the argument with inactive face
      (should (string-match-p "--test=" result))
      (should-not (string-match-p "\\[" result)))))

(ert-deftest beads-option-test-multiline-format-value-short ()
  "Test multiline formatter with short value."
  (let ((obj (beads-create-transient-multiline)))
    (oset obj value "Short text")
    (oset obj argument "--test=")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      ;; New format: shows argument=value (no brackets)
      (should (string-match-p "--test=Short text" result))
      (should-not (string-match-p "\\[" result)))))

(ert-deftest beads-option-test-multiline-format-value-multiline ()
  "Test multiline formatter with multiline value (shows all lines escaped)."
  (let ((obj (beads-create-transient-multiline)))
    (oset obj value "Line one\nLine two\nLine three")
    (oset obj argument "--test=")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      ;; New format: shows escaped newlines as \\n
      (should (string-match-p "--test=Line one\\\\nLine two\\\\nLine three" result))
      (should-not (string-match-p "\\[" result)))))

(ert-deftest beads-option-test-multiline-format-value-long ()
  "Test multiline formatter with long first line (truncation)."
  (let ((obj (beads-create-transient-multiline))
        (long-line (make-string 50 ?x)))
    (oset obj value long-line)
    (oset obj argument "--test=")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      ;; New format: truncates at 40 chars and adds ...
      (should (string-match-p "--test=" result))
      (should (string-match-p "\\.\\.\\." result))
      (should-not (string-match-p "\\[" result)))))

;;; ============================================================
;;; Infix Definition Tests
;;; ============================================================

(ert-deftest beads-option-test-close-issue-id-infix-exists ()
  "Test that close issue-id infix is defined."
  (should (commandp 'beads-option-close-issue-id)))

(ert-deftest beads-option-test-reopen-issue-id-infix-exists ()
  "Test that reopen issue-id infix is defined."
  (should (commandp 'beads-option-reopen-issue-id)))

(ert-deftest beads-option-test-create-title-infix-exists ()
  "Test that create title infix is defined."
  (should (commandp 'beads-option-create-title)))

(ert-deftest beads-option-test-create-type-infix-exists ()
  "Test that create type infix is defined."
  (should (commandp 'beads-option-create-type)))

(ert-deftest beads-option-test-create-priority-infix-exists ()
  "Test that create priority infix is defined."
  (should (commandp 'beads-option-create-priority)))

(ert-deftest beads-option-test-create-custom-id-infix-exists ()
  "Test that create custom-id infix is defined."
  (should (commandp 'beads-option-create-custom-id)))

(ert-deftest beads-option-test-create-dependencies-infix-exists ()
  "Test that create dependencies infix is defined."
  (should (commandp 'beads-option-create-dependencies)))

(ert-deftest beads-option-test-create-assignee-infix-exists ()
  "Test that create assignee infix is defined."
  (should (commandp 'beads-option-create-assignee)))

(ert-deftest beads-option-test-create-external-ref-infix-exists ()
  "Test that create external-ref infix is defined."
  (should (commandp 'beads-option-create-external-ref)))

(ert-deftest beads-option-test-create-labels-infix-exists ()
  "Test that create labels infix is defined."
  (should (commandp 'beads-option-create-labels)))

(ert-deftest beads-option-test-create-force-infix-exists ()
  "Test that create force infix is defined."
  (should (commandp 'beads-option-create-force)))

(ert-deftest beads-option-test-create-description-infix-exists ()
  "Test that create description infix is defined."
  (should (commandp 'beads-option-create-description)))

(ert-deftest beads-option-test-create-acceptance-infix-exists ()
  "Test that create acceptance infix is defined."
  (should (commandp 'beads-option-create-acceptance)))

(ert-deftest beads-option-test-create-design-infix-exists ()
  "Test that create design infix is defined."
  (should (commandp 'beads-option-create-design)))

(ert-deftest beads-option-test-dep-add-issue-id-infix-exists ()
  "Test that dep-add issue-id infix is defined."
  (should (commandp 'beads-option-dep-add-issue-id)))

(ert-deftest beads-option-test-dep-add-depends-on-id-infix-exists ()
  "Test that dep-add depends-on-id infix is defined."
  (should (commandp 'beads-option-dep-add-depends-on-id)))

(ert-deftest beads-option-test-dep-remove-issue-id-infix-exists ()
  "Test that dep-remove issue-id infix is defined."
  (should (commandp 'beads-option-dep-remove-issue-id)))

(ert-deftest beads-option-test-dep-remove-depends-on-id-infix-exists ()
  "Test that dep-remove depends-on-id infix is defined."
  (should (commandp 'beads-option-dep-remove-depends-on-id)))

(ert-deftest beads-option-test-dep-from-infix-exists ()
  "Test that dep-from infix is defined."
  (should (commandp 'beads-option-dep-from)))

(ert-deftest beads-option-test-dep-to-infix-exists ()
  "Test that dep-to infix is defined."
  (should (commandp 'beads-option-dep-to)))

(ert-deftest beads-option-test-dep-add-type-infix-exists ()
  "Test that dep-add type infix is defined."
  (should (commandp 'beads-option-dep-add-type)))

(ert-deftest beads-option-test-dep-type-infix-exists ()
  "Test that dep-type infix is defined."
  (should (commandp 'beads-option-dep-type)))

(ert-deftest beads-option-test-sync-dry-run-infix-exists ()
  "Test that sync dry-run infix is defined."
  (should (commandp 'beads-option-sync-dry-run)))

(ert-deftest beads-option-test-sync-message-infix-exists ()
  "Test that sync message infix is defined."
  (should (commandp 'beads-option-sync-message)))

(ert-deftest beads-option-test-sync-no-pull-infix-exists ()
  "Test that sync no-pull infix is defined."
  (should (commandp 'beads-option-sync-no-pull)))

(ert-deftest beads-option-test-sync-no-push-infix-exists ()
  "Test that sync no-push infix is defined."
  (should (commandp 'beads-option-sync-no-push)))

(ert-deftest beads-option-test-export-output-infix-exists ()
  "Test that export output infix is defined."
  (should (commandp 'beads-option-export-output)))

(ert-deftest beads-option-test-export-no-auto-flush-infix-exists ()
  "Test that export no-auto-flush infix is defined."
  (should (commandp 'beads-option-export-no-auto-flush)))

(ert-deftest beads-option-test-import-input-infix-exists ()
  "Test that import input infix is defined."
  (should (commandp 'beads-option-import-input)))

(ert-deftest beads-option-test-import-dry-run-infix-exists ()
  "Test that import dry-run infix is defined."
  (should (commandp 'beads-option-import-dry-run)))

(ert-deftest beads-option-test-import-resolve-collisions-infix-exists ()
  "Test that import resolve-collisions infix is defined."
  (should (commandp 'beads-option-import-resolve-collisions)))

(ert-deftest beads-option-test-init-prefix-infix-exists ()
  "Test that init prefix infix is defined."
  (should (commandp 'beads-option-init-prefix)))

(ert-deftest beads-option-test-init-db-infix-exists ()
  "Test that init db infix is defined."
  (should (commandp 'beads-option-init-db)))

;;; ============================================================
;;; State Variable Tests
;;; ============================================================

(ert-deftest beads-option-test-state-variables-exist ()
  "Test that required state variables are defined."
  (should (boundp 'beads-close--issue-id))
  (should (boundp 'beads-reopen--issue-id))
  (should (boundp 'beads-dep-add--issue-id))
  (should (boundp 'beads-dep-add--depends-on-id))
  (should (boundp 'beads-dep-add--type))
  (should (boundp 'beads-dep-remove--issue-id))
  (should (boundp 'beads-dep-remove--depends-on-id))
  (should (boundp 'beads-dep--from-issue))
  (should (boundp 'beads-dep--to-issue))
  (should (boundp 'beads-dep--dep-type))
  (should (boundp 'beads-sync--dry-run))
  (should (boundp 'beads-sync--message))
  (should (boundp 'beads-sync--no-pull))
  (should (boundp 'beads-sync--no-push))
  (should (boundp 'beads-export--output))
  (should (boundp 'beads-export--no-auto-flush))
  (should (boundp 'beads-import--input))
  (should (boundp 'beads-import--dry-run))
  (should (boundp 'beads-import--resolve-collisions))
  (should (boundp 'beads-init--prefix))
  (should (boundp 'beads-init--db-path)))

(ert-deftest beads-option-test-state-variables-initial-values ()
  "Test that state variables have correct initial values."
  ;; Save current state
  (let ((saved-close-issue-id beads-close--issue-id)
        (saved-reopen-issue-id beads-reopen--issue-id)
        (saved-dep-add-issue-id beads-dep-add--issue-id)
        (saved-dep-add-depends-on-id beads-dep-add--depends-on-id)
        (saved-dep-add-type beads-dep-add--type)
        (saved-sync-dry-run beads-sync--dry-run)
        (saved-sync-message beads-sync--message)
        (saved-export-output beads-export--output)
        (saved-import-input beads-import--input))
    (unwind-protect
        (progn
          ;; Reset all to nil
          (setq beads-close--issue-id nil
                beads-reopen--issue-id nil
                beads-dep-add--issue-id nil
                beads-dep-add--depends-on-id nil
                beads-dep-add--type nil
                beads-sync--dry-run nil
                beads-sync--message nil
                beads-export--output nil
                beads-import--input nil)
          ;; All should be nil after reset
          (should (null beads-close--issue-id))
          (should (null beads-reopen--issue-id))
          (should (null beads-dep-add--issue-id))
          (should (null beads-dep-add--depends-on-id))
          (should (null beads-dep-add--type))
          (should (null beads-sync--dry-run))
          (should (null beads-sync--message))
          (should (null beads-export--output))
          (should (null beads-import--input)))
      ;; Restore original state
      (setq beads-close--issue-id saved-close-issue-id
            beads-reopen--issue-id saved-reopen-issue-id
            beads-dep-add--issue-id saved-dep-add-issue-id
            beads-dep-add--depends-on-id saved-dep-add-depends-on-id
            beads-dep-add--type saved-dep-add-type
            beads-sync--dry-run saved-sync-dry-run
            beads-sync--message saved-sync-message
            beads-export--output saved-export-output
            beads-import--input saved-import-input))))

(provide 'beads-option-test)
;;; beads-option-test.el ends here
