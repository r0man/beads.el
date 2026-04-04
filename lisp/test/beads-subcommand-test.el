;;; beads-subcommand-test.el --- Tests for hierarchical subcommand derivation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the hierarchical subcommand derivation (D13):
;; - beads--collect-subcommand-segments walks class hierarchy
;; - beads-command-subcommand joins segments with spaces
;; - :cli-command overrides hierarchy-based derivation
;; - Abstract parent classes contribute prefix segments

;;; Code:

(require 'ert)
(require 'beads-command)
(require 'beads-meta)

;;; ============================================================
;;; Test Classes — Hierarchical Command Hierarchy
;;; ============================================================

;; Abstract parent for a subcommand group
(beads-defcommand beads-command-test-admin (beads-command-global-options)
  ()
  :documentation "Admin commands (test)."
  :transient nil)

;; Concrete leaf under abstract parent
(beads-defcommand beads-command-test-admin-compact
    (beads-command-test-admin)
  ((database
    :type (or null string)
    :short-option "d"))
  :documentation "Compact database (test)."
  :transient nil)

;; Another concrete leaf
(beads-defcommand beads-command-test-admin-gc
    (beads-command-test-admin)
  ()
  :documentation "Garbage collect (test)."
  :transient nil)

;; Two-level nesting: admin → maintenance → prune
(beads-defcommand beads-command-test-admin-maintenance
    (beads-command-test-admin)
  ()
  :documentation "Maintenance commands (test)."
  :transient nil)

(beads-defcommand beads-command-test-admin-maintenance-prune
    (beads-command-test-admin-maintenance)
  ()
  :documentation "Prune old data (test)."
  :transient nil)

;; Command with hyphenated leaf name (should preserve hyphens)
(beads-defcommand beads-command-test-federation (beads-command-global-options)
  ()
  :documentation "Federation commands (test)."
  :transient nil)

(beads-defcommand beads-command-test-federation-add-peer
    (beads-command-test-federation)
  ((url
    :type (or null string)
    :short-option "u"))
  :documentation "Add federation peer (test)."
  :transient nil)

;; Command with :cli-command override
(beads-defcommand beads-command-test-legacy (beads-command-global-options)
  ()
  :documentation "Legacy command (test)."
  :cli-command "custom legacy-path"
  :transient nil)

;; Simple direct child (no abstract parent)
(beads-defcommand beads-command-test-simple (beads-command-global-options)
  ()
  :documentation "Simple command (test)."
  :transient nil)

;;; ============================================================
;;; Tests for beads--collect-subcommand-segments
;;; ============================================================

(ert-deftest beads-subcommand-test-segments-simple ()
  "Direct child of global-options produces one segment."
  (let ((segments (beads--collect-subcommand-segments
                   'beads-command-test-simple)))
    (should (equal segments '("test-simple")))))

(ert-deftest beads-subcommand-test-segments-abstract-parent ()
  "Child of abstract parent produces two segments."
  (let ((segments (beads--collect-subcommand-segments
                   'beads-command-test-admin-compact)))
    (should (equal segments '("test-admin" "compact")))))

(ert-deftest beads-subcommand-test-segments-two-level ()
  "Two levels of nesting produce three segments."
  (let ((segments (beads--collect-subcommand-segments
                   'beads-command-test-admin-maintenance-prune)))
    (should (equal segments '("test-admin" "maintenance" "prune")))))

(ert-deftest beads-subcommand-test-segments-hyphenated-leaf ()
  "Hyphenated leaf name is preserved."
  (let ((segments (beads--collect-subcommand-segments
                   'beads-command-test-federation-add-peer)))
    (should (equal segments '("test-federation" "add-peer")))))

(ert-deftest beads-subcommand-test-segments-abstract-parent-itself ()
  "Abstract parent class produces one segment."
  (let ((segments (beads--collect-subcommand-segments
                   'beads-command-test-admin)))
    (should (equal segments '("test-admin")))))

(ert-deftest beads-subcommand-test-segments-base-class ()
  "Base beads-command produces no segments."
  (should-not (beads--collect-subcommand-segments 'beads-command)))

(ert-deftest beads-subcommand-test-segments-global-options ()
  "Global options class produces no segments."
  (should-not (beads--collect-subcommand-segments
               'beads-command-global-options)))

;;; ============================================================
;;; Tests for beads-command-subcommand
;;; ============================================================

(ert-deftest beads-subcommand-test-simple-command ()
  "Direct child of global-options uses legacy fallback."
  (let ((cmd (beads-command-test-simple)))
    ;; Parent is global-options → legacy fallback (hyphens to spaces)
    (should (equal (beads-command-subcommand cmd) "test simple"))))

(ert-deftest beads-subcommand-test-hierarchical ()
  "Hierarchical command produces space-separated subcommand."
  (let ((cmd (beads-command-test-admin-compact)))
    (should (equal (beads-command-subcommand cmd)
                   "test-admin compact"))))

(ert-deftest beads-subcommand-test-deep-hierarchy ()
  "Deeply nested command produces multi-word subcommand."
  (let ((cmd (beads-command-test-admin-maintenance-prune)))
    (should (equal (beads-command-subcommand cmd)
                   "test-admin maintenance prune"))))

(ert-deftest beads-subcommand-test-hyphenated-leaf ()
  "Hyphenated leaf name preserved in subcommand."
  (let ((cmd (beads-command-test-federation-add-peer)))
    (should (equal (beads-command-subcommand cmd)
                   "test-federation add-peer"))))

(ert-deftest beads-subcommand-test-cli-command-override ()
  ":cli-command takes priority over hierarchy derivation."
  (let ((cmd (beads-command-test-legacy)))
    (should (equal (beads-command-subcommand cmd)
                   "custom legacy-path"))))

(ert-deftest beads-subcommand-test-base-returns-nil ()
  "Base beads-command returns nil subcommand."
  ;; Can't instantiate abstract class directly, but the method should handle it
  (should-not (beads--collect-subcommand-segments 'beads-command)))

;;; ============================================================
;;; Tests for symbol property registration
;;; ============================================================

(ert-deftest beads-subcommand-test-parent-property ()
  "beads-defcommand registers beads-parent symbol property."
  (should (eq (get 'beads-command-test-admin-compact 'beads-parent)
              'beads-command-test-admin)))

(ert-deftest beads-subcommand-test-children-property ()
  "beads-defcommand registers beads-children symbol property."
  (let ((children (get 'beads-command-test-admin 'beads-children)))
    (should (memq 'beads-command-test-admin-compact children))
    (should (memq 'beads-command-test-admin-gc children))
    (should (memq 'beads-command-test-admin-maintenance children))))

(ert-deftest beads-subcommand-test-global-options-no-parent ()
  "beads-command-global-options has no beads-parent."
  ;; Global options is the root for command classes
  (should-not (get 'beads-command-global-options 'beads-parent)))

;;; ============================================================
;;; Tests against real commands
;;; ============================================================

(ert-deftest beads-subcommand-test-close-command ()
  "Real close command derives 'close' subcommand."
  (require 'beads-command-close)
  (let ((cmd (beads-command-close)))
    (should (equal (beads-command-subcommand cmd) "close"))))

(provide 'beads-subcommand-test)

;;; beads-subcommand-test.el ends here
