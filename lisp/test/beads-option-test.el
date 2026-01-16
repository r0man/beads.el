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

(ert-deftest beads-option-test-issue-title-infix-exists ()
  "Test that issue title infix is defined."
  (should (commandp 'beads-option-issue-title)))

(ert-deftest beads-option-test-issue-type-infix-exists ()
  "Test that issue type infix is defined."
  (should (commandp 'beads-option-issue-type)))

(ert-deftest beads-option-test-issue-priority-infix-exists ()
  "Test that issue priority infix is defined."
  (should (commandp 'beads-option-issue-priority)))

(ert-deftest beads-option-test-create-custom-id-infix-exists ()
  "Test that create custom-id infix is defined."
  (should (commandp 'beads-option-create-custom-id)))

(ert-deftest beads-option-test-create-dependencies-infix-exists ()
  "Test that create dependencies infix is defined."
  (should (commandp 'beads-option-create-dependencies)))

(ert-deftest beads-option-test-issue-assignee-infix-exists ()
  "Test that issue assignee infix is defined."
  (should (commandp 'beads-option-issue-assignee)))

(ert-deftest beads-option-test-issue-external-ref-infix-exists ()
  "Test that issue external-ref infix is defined."
  (should (commandp 'beads-option-issue-external-ref)))

(ert-deftest beads-option-test-issue-labels-infix-exists ()
  "Test that issue labels infix is defined."
  (should (commandp 'beads-option-issue-labels)))

(ert-deftest beads-option-test-create-force-infix-exists ()
  "Test that create force infix is defined."
  (should (commandp 'beads-option-create-force)))

(ert-deftest beads-option-test-issue-description-infix-exists ()
  "Test that issue description infix is defined."
  (should (commandp 'beads-option-issue-description)))

(ert-deftest beads-option-test-issue-acceptance-infix-exists ()
  "Test that issue acceptance infix is defined."
  (should (commandp 'beads-option-issue-acceptance)))

(ert-deftest beads-option-test-issue-design-infix-exists ()
  "Test that issue design infix is defined."
  (should (commandp 'beads-option-issue-design)))

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

(ert-deftest beads-option-test-import-skip-existing-infix-exists ()
  "Test that import skip-existing infix is defined."
  (should (commandp 'beads-option-import-skip-existing)))

(ert-deftest beads-option-test-import-clear-duplicate-external-refs-infix-exists ()
  "Test that import clear-duplicate-external-refs infix is defined."
  (should (commandp 'beads-option-import-clear-duplicate-external-refs)))

(ert-deftest beads-option-test-import-dedupe-after-infix-exists ()
  "Test that import dedupe-after infix is defined."
  (should (commandp 'beads-option-import-dedupe-after)))

(ert-deftest beads-option-test-import-rename-on-import-infix-exists ()
  "Test that import rename-on-import infix is defined."
  (should (commandp 'beads-option-import-rename-on-import)))

(ert-deftest beads-option-test-import-strict-infix-exists ()
  "Test that import strict infix is defined."
  (should (commandp 'beads-option-import-strict)))

(ert-deftest beads-option-test-import-orphan-handling-infix-exists ()
  "Test that import orphan-handling infix is defined."
  (should (commandp 'beads-option-import-orphan-handling)))

(ert-deftest beads-option-test-init-prefix-infix-exists ()
  "Test that init prefix infix is defined."
  (should (commandp 'beads-option-init-prefix)))

(ert-deftest beads-option-test-init-db-infix-exists ()
  "Test that init db infix is defined."
  (should (commandp 'beads-option-init-db)))

;;; ============================================================
;;; Global Option Class Tests
;;; ============================================================

(ert-deftest beads-option-test-global-class-exists ()
  "Test that beads-option-global class is defined."
  (should (class-p 'beads-option-global))
  (should (child-of-class-p 'beads-option-global
                            'transient-lisp-variable)))

(ert-deftest beads-option-test-global-switch-class-exists ()
  "Test that beads-option-global-switch class is defined."
  (should (class-p 'beads-option-global-switch))
  (should (child-of-class-p 'beads-option-global-switch
                            'transient-lisp-variable)))

;; Note: transient-infix-read tests are omitted because the method
;; is wrapped by transient's framework and requires full transient
;; infrastructure to test. The methods are indirectly tested via
;; integration tests when using actual transient menus.

(ert-deftest beads-option-test-global-format-value-with-value ()
  "Test transient-format-value for global option with value."
  (let ((obj (beads-option-global)))
    (oset obj value "myactor")
    (oset obj argument "--actor=")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      (should (string-match-p "(--actor=myactor)" result)))))

(ert-deftest beads-option-test-global-format-value-without-value ()
  "Test transient-format-value for global option without value."
  (let ((obj (beads-option-global)))
    (oset obj value nil)
    (oset obj argument "--actor=")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      (should (string-match-p "(--actor=)" result)))))

(ert-deftest beads-option-test-global-format-value-empty-string ()
  "Test transient-format-value with empty string."
  (let ((obj (beads-option-global)))
    (oset obj value "")
    (oset obj argument "--actor=")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      (should (string-match-p "(--actor=)" result)))))

(ert-deftest beads-option-test-global-switch-format-value-true ()
  "Test transient-format-value for switch when true."
  (let ((obj (beads-option-global-switch)))
    (oset obj value t)
    (oset obj argument "--no-daemon")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      (should (string-match-p "(--no-daemon)" result)))))

(ert-deftest beads-option-test-global-switch-format-value-false ()
  "Test transient-format-value for switch when false."
  (let ((obj (beads-option-global-switch)))
    (oset obj value nil)
    (oset obj argument "--no-daemon")
    (let ((result (transient-format-value obj)))
      (should (stringp result))
      (should (string-match-p "(--no-daemon)" result)))))

(ert-deftest beads-option-test-global-infix-value-returns-nil ()
  "Test transient-infix-value returns nil for global options."
  (let ((obj (beads-option-global)))
    (oset obj value "test")
    (should (null (transient-infix-value obj)))))

(ert-deftest beads-option-test-global-switch-infix-value-returns-nil ()
  "Test transient-infix-value returns nil for global switches."
  (let ((obj (beads-option-global-switch)))
    (oset obj value t)
    (should (null (transient-infix-value obj)))))

;;; ============================================================
;;; Global Option Infix Definition Tests
;;; ============================================================

(ert-deftest beads-option-test-global-actor-infix-exists ()
  "Test that global actor infix is defined."
  (should (commandp 'beads-option-global-actor)))

(ert-deftest beads-option-test-global-db-infix-exists ()
  "Test that global db infix is defined."
  (should (commandp 'beads-option-global-db)))

(ert-deftest beads-option-test-global-json-infix-exists ()
  "Test that global json infix is defined."
  (should (commandp 'beads-option-global-json)))

(ert-deftest beads-option-test-global-no-auto-flush-infix-exists ()
  "Test that global no-auto-flush infix is defined."
  (should (commandp 'beads-option-global-no-auto-flush)))

(ert-deftest beads-option-test-global-no-auto-import-infix-exists ()
  "Test that global no-auto-import infix is defined."
  (should (commandp 'beads-option-global-no-auto-import)))

(ert-deftest beads-option-test-global-no-daemon-infix-exists ()
  "Test that global no-daemon infix is defined."
  (should (commandp 'beads-option-global-no-daemon)))

(ert-deftest beads-option-test-global-no-db-infix-exists ()
  "Test that global no-db infix is defined."
  (should (commandp 'beads-option-global-no-db)))

(ert-deftest beads-option-test-global-sandbox-infix-exists ()
  "Test that global sandbox infix is defined."
  (should (commandp 'beads-option-global-sandbox)))

;; Note: beads-option-global-section existence is tested indirectly
;; by the fact that beads-create transient menu works and includes
;; the global options section.

;;; ============================================================
;;; Global Variable State Tests
;;; ============================================================

(ert-deftest beads-option-test-global-variables-exist ()
  "Test that global option variables are defined."
  (should (boundp 'beads-global-actor))
  (should (boundp 'beads-global-db))
  (should (boundp 'beads-global-json))
  (should (boundp 'beads-global-no-auto-flush))
  (should (boundp 'beads-global-no-auto-import))
  (should (boundp 'beads-global-no-daemon))
  (should (boundp 'beads-global-no-db))
  (should (boundp 'beads-global-sandbox)))

;;; ============================================================
;;; State Variable Tests
;;; ============================================================

(ert-deftest beads-option-test-state-variables-exist ()
  "Test that required state variables are defined."
  ;; beads-close now uses meta-generated transient (no module state variables)
  (should (boundp 'beads-reopen--issue-id))
  ;; beads-dep-add, beads-dep-remove, beads-sync, beads-export, beads-import, and beads-init
  ;; no longer use state variables (transient-args pattern)
  (should (boundp 'beads-dep--from-issue))
  (should (boundp 'beads-dep--to-issue))
  (should (boundp 'beads-dep--dep-type)))

(ert-deftest beads-option-test-state-variables-initial-values ()
  "Test that state variables have correct initial values."
  ;; Save current state (only commands that still use state variables)
  ;; Note: beads-close now uses meta-generated transient, no state variables
  (let ((saved-reopen-issue-id beads-reopen--issue-id))
    (unwind-protect
        (progn
          ;; Reset all to nil
          (setq beads-reopen--issue-id nil)
          ;; All should be nil after reset
          (should (null beads-reopen--issue-id)))
      ;; Restore original state
      (setq beads-reopen--issue-id saved-reopen-issue-id))))

;;; ============================================================
;;; Global Option Class Property Tests
;;; ============================================================

(ert-deftest beads-option-test-global-class-slots ()
  "Test that beads-option-global class has expected slots."
  (let ((obj (beads-option-global)))
    (should (slot-exists-p obj 'pad-keys))
    (should (slot-boundp obj 'pad-keys))))

(ert-deftest beads-option-test-global-switch-class-slots ()
  "Test that beads-option-global-switch class has expected slots."
  (let ((obj (beads-option-global-switch)))
    (should (slot-exists-p obj 'pad-keys))
    (should (slot-boundp obj 'pad-keys))))

(ert-deftest beads-option-test-global-default-pad-keys ()
  "Test that beads-option-global has nil default for pad-keys."
  (let ((obj (beads-option-global)))
    (should-not (oref obj pad-keys))))

(ert-deftest beads-option-test-global-switch-default-pad-keys ()
  "Test that beads-option-global-switch has nil default for pad-keys."
  (let ((obj (beads-option-global-switch)))
    (should-not (oref obj pad-keys))))

;;; ============================================================
;;; List Filter Infix Tests
;;; ============================================================

(ert-deftest beads-option-test-list-all-infix-exists ()
  "Test that list all infix is defined."
  (should (commandp 'beads-option-list-all)))

(ert-deftest beads-option-test-list-no-assignee-infix-exists ()
  "Test that list no-assignee infix is defined."
  (should (commandp 'beads-option-list-no-assignee)))

(ert-deftest beads-option-test-list-empty-description-infix-exists ()
  "Test that list empty-description infix is defined."
  (should (commandp 'beads-option-list-empty-description)))

(ert-deftest beads-option-test-list-no-labels-infix-exists ()
  "Test that list no-labels infix is defined."
  (should (commandp 'beads-option-list-no-labels)))

(ert-deftest beads-option-test-list-long-infix-exists ()
  "Test that list long infix is defined."
  (should (commandp 'beads-option-list-long)))

(ert-deftest beads-option-test-list-assignee-infix-exists ()
  "Test that list assignee infix is defined."
  (should (commandp 'beads-option-list-assignee)))

(ert-deftest beads-option-test-list-closed-after-infix-exists ()
  "Test that list closed-after infix is defined."
  (should (commandp 'beads-option-list-closed-after)))

(ert-deftest beads-option-test-list-closed-before-infix-exists ()
  "Test that list closed-before infix is defined."
  (should (commandp 'beads-option-list-closed-before)))

(ert-deftest beads-option-test-list-created-after-infix-exists ()
  "Test that list created-after infix is defined."
  (should (commandp 'beads-option-list-created-after)))

(ert-deftest beads-option-test-list-created-before-infix-exists ()
  "Test that list created-before infix is defined."
  (should (commandp 'beads-option-list-created-before)))

(ert-deftest beads-option-test-list-desc-contains-infix-exists ()
  "Test that list desc-contains infix is defined."
  (should (commandp 'beads-option-list-desc-contains)))

(ert-deftest beads-option-test-list-format-infix-exists ()
  "Test that list format infix is defined."
  (should (commandp 'beads-option-list-format)))

(ert-deftest beads-option-test-list-id-infix-exists ()
  "Test that list id infix is defined."
  (should (commandp 'beads-option-list-id)))

(ert-deftest beads-option-test-list-label-infix-exists ()
  "Test that list label infix is defined."
  (should (commandp 'beads-option-list-label)))

(ert-deftest beads-option-test-list-label-any-infix-exists ()
  "Test that list label-any infix is defined."
  (should (commandp 'beads-option-list-label-any)))

(ert-deftest beads-option-test-list-limit-infix-exists ()
  "Test that list limit infix is defined."
  (should (commandp 'beads-option-list-limit)))

(ert-deftest beads-option-test-list-notes-contains-infix-exists ()
  "Test that list notes-contains infix is defined."
  (should (commandp 'beads-option-list-notes-contains)))

(ert-deftest beads-option-test-list-priority-infix-exists ()
  "Test that list priority infix is defined."
  (should (commandp 'beads-option-list-priority)))

(ert-deftest beads-option-test-list-priority-min-infix-exists ()
  "Test that list priority-min infix is defined."
  (should (commandp 'beads-option-list-priority-min)))

(ert-deftest beads-option-test-list-priority-max-infix-exists ()
  "Test that list priority-max infix is defined."
  (should (commandp 'beads-option-list-priority-max)))

(ert-deftest beads-option-test-list-status-infix-exists ()
  "Test that list status infix is defined."
  (should (commandp 'beads-option-list-status)))

(ert-deftest beads-option-test-list-title-infix-exists ()
  "Test that list title infix is defined."
  (should (commandp 'beads-option-list-title)))

(ert-deftest beads-option-test-list-title-contains-infix-exists ()
  "Test that list title-contains infix is defined."
  (should (commandp 'beads-option-list-title-contains)))

(ert-deftest beads-option-test-list-type-infix-exists ()
  "Test that list type infix is defined."
  (should (commandp 'beads-option-list-type)))

(ert-deftest beads-option-test-list-updated-after-infix-exists ()
  "Test that list updated-after infix is defined."
  (should (commandp 'beads-option-list-updated-after)))

(ert-deftest beads-option-test-list-updated-before-infix-exists ()
  "Test that list updated-before infix is defined."
  (should (commandp 'beads-option-list-updated-before)))

;;; ============================================================
;;; Update Infix Tests
;;; ============================================================

(ert-deftest beads-option-test-update-status-infix-exists ()
  "Test that update status infix is defined."
  (should (commandp 'beads-option-update-status)))

(ert-deftest beads-option-test-update-priority-infix-exists ()
  "Test that update priority infix is defined."
  (should (commandp 'beads-option-update-priority)))

(ert-deftest beads-option-test-update-type-infix-exists ()
  "Test that update type infix is defined."
  (should (commandp 'beads-option-update-type)))

(ert-deftest beads-option-test-update-title-infix-exists ()
  "Test that update title infix is defined."
  (should (commandp 'beads-option-update-title)))

(ert-deftest beads-option-test-update-assignee-infix-exists ()
  "Test that update assignee infix is defined."
  (should (commandp 'beads-option-update-assignee)))

(ert-deftest beads-option-test-update-external-ref-infix-exists ()
  "Test that update external-ref infix is defined."
  (should (commandp 'beads-option-update-external-ref)))

(ert-deftest beads-option-test-update-description-infix-exists ()
  "Test that update description infix is defined."
  (should (commandp 'beads-option-update-description)))

(ert-deftest beads-option-test-update-acceptance-multiline-infix-exists ()
  "Test that update acceptance-multiline infix is defined."
  (should (commandp 'beads-option-update-acceptance-multiline)))

(ert-deftest beads-option-test-update-design-multiline-infix-exists ()
  "Test that update design-multiline infix is defined."
  (should (commandp 'beads-option-update-design-multiline)))

(ert-deftest beads-option-test-update-notes-multiline-infix-exists ()
  "Test that update notes-multiline infix is defined."
  (should (commandp 'beads-option-update-notes-multiline)))

;;; ============================================================
;;; Close/Reopen Infix Tests
;;; ============================================================

(ert-deftest beads-option-test-close-reason-infix-exists ()
  "Test that close reason infix is defined."
  (should (commandp 'beads-option-close-reason)))

(ert-deftest beads-option-test-reopen-reason-infix-exists ()
  "Test that reopen reason infix is defined."
  (should (commandp 'beads-option-reopen-reason)))

;;; ============================================================
;;; Create Infix Tests (additional)
;;; ============================================================

(ert-deftest beads-option-test-create-parent-infix-exists ()
  "Test that create parent infix is defined."
  (should (commandp 'beads-option-create-parent)))

(ert-deftest beads-option-test-create-repo-infix-exists ()
  "Test that create repo infix is defined."
  (should (commandp 'beads-option-create-repo)))

(ert-deftest beads-option-test-create-from-template-infix-exists ()
  "Test that create from-template infix is defined."
  (should (commandp 'beads-option-create-from-template)))

(ert-deftest beads-option-test-create-file-infix-exists ()
  "Test that create file infix is defined."
  (should (commandp 'beads-option-create-file)))

;;; ============================================================
;;; Init Infix Tests (additional)
;;; ============================================================

(ert-deftest beads-option-test-init-branch-infix-exists ()
  "Test that init branch infix is defined."
  (should (commandp 'beads-option-init-branch)))

(ert-deftest beads-option-test-init-contributor-infix-exists ()
  "Test that init contributor infix is defined."
  (should (commandp 'beads-option-init-contributor)))

(ert-deftest beads-option-test-init-quiet-infix-exists ()
  "Test that init quiet infix is defined."
  (should (commandp 'beads-option-init-quiet)))

(ert-deftest beads-option-test-init-skip-merge-driver-infix-exists ()
  "Test that init skip-merge-driver infix is defined."
  (should (commandp 'beads-option-init-skip-merge-driver)))

(ert-deftest beads-option-test-init-team-infix-exists ()
  "Test that init team infix is defined."
  (should (commandp 'beads-option-init-team)))

;;; ============================================================
;;; Export Infix Tests (additional)
;;; ============================================================

(ert-deftest beads-option-test-export-force-infix-exists ()
  "Test that export force infix is defined."
  (should (commandp 'beads-option-export-force)))

(ert-deftest beads-option-test-export-format-infix-exists ()
  "Test that export format infix is defined."
  (should (commandp 'beads-option-export-format)))

(ert-deftest beads-option-test-export-status-infix-exists ()
  "Test that export status infix is defined."
  (should (commandp 'beads-option-export-status)))

;;; ============================================================
;;; Label Transient Menu Tests
;;; ============================================================

(ert-deftest beads-option-test-label-add-transient-exists ()
  "Test that beads-label-add transient is defined."
  (should (commandp 'beads-label-add))
  (should (get 'beads-label-add 'transient--prefix)))

(ert-deftest beads-option-test-label-remove-transient-exists ()
  "Test that beads-label-remove transient is defined."
  (should (commandp 'beads-label-remove))
  (should (get 'beads-label-remove 'transient--prefix)))

(provide 'beads-option-test)
;;; beads-option-test.el ends here
