;;; beads-audit-test.el --- CLI command-parity drift gate tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; The command-parity drift gate for beads.el.  These tests keep the
;; EIEIO command classes in lockstep with the live `bd' CLI surface so
;; that drift cannot be merged silently (see
;; .designs/command-parity/analysis.md sec 2.3):
;;
;;   - `:integration' tests walk the real `bd' (skipped when it is
;;     absent) and FAIL on a new unclassed command or a new slot gap.
;;   - `:unit' tests exercise the gate's pure diff logic on synthetic
;;     input, so the "detects a regression" guarantee holds even without
;;     `bd' installed, and assert the concrete acceptance fact that
;;     `bd migrate schema' is covered.
;;
;; The policy that decides what is a *real* gap lives as data in
;; `beads-meta' (`beads-meta-parity-*'); these tests consume the same
;; constants the gate does.

;;; Code:

(require 'ert)
(require 'beads-audit)

;;; ------------------------------------------------------------
;;; Unit tests -- pure diff logic (no `bd' required)
;;; ------------------------------------------------------------

(ert-deftest beads-audit-test-migrate-schema-covered ()
  "`bd migrate schema' has a command class (reaching 235/235 addressable).
This is the concrete acceptance fact for adding
`beads-command-migrate-schema'."
  :tags '(:unit)
  (let ((inv (beads-audit-class-inventory)))
    (should (assoc "migrate.schema" inv))
    ;; And it resolves to the right CLI subcommand.
    (should (equal "migrate schema"
                   (beads-command-subcommand
                    (make-instance 'beads-command-migrate-schema))))))

(ert-deftest beads-audit-test-missing-command-detection ()
  "Missing-command detection flags unclassed leaves and honors policy.
Proves the gate FAILS on a new unclassed command without needing `bd'."
  :tags '(:unit)
  (let* ((cli '("close" "frobnicate" "admin" "comments.list"))
         (inv '(("close" beads-command-close)))
         (missing (beads-audit-missing-commands cli inv)))
    ;; A new, unclassed, non-excluded leaf is flagged (the regression).
    (should (member "frobnicate" missing))
    ;; A covered command is not flagged.
    (should-not (member "close" missing))
    ;; A router group is excluded by policy.
    (should-not (member "admin" missing))
    ;; A declared non-goal command is excluded by policy.
    (should-not (member "comments.list" missing))))

(ert-deftest beads-audit-test-missing-slot-detection ()
  "Missing-slot detection flags new flags and honors the accepted baseline.
Proves the gate FAILS on a new slot gap without needing `bd'."
  :tags '(:unit)
  ;; A CLI flag with no matching slot is a gap (the regression).
  (should (member "wibble"
                  (beads-audit--missing-slots '("wibble") '("other") "frob")))
  ;; A flag that has a matching slot is not a gap.
  (should-not (member "name"
                      (beads-audit--missing-slots '("name") '("name") "frob")))
  ;; A baseline-accepted flag is not a gap.
  (should-not (member "depends-on"
                      (beads-audit--missing-slots '("depends-on") '() "dep.add")))
  ;; A declared non-goal flag is not a gap.
  (should-not (member "proxied-server"
                      (beads-audit--missing-slots '("proxied-server") '() "init"))))

(ert-deftest beads-audit-test-intentional-collision-data-is-real ()
  "The recorded `admin compact' collision members are real classes on that path.
Keeps `beads-meta-parity-intentional-collisions' from drifting away from
the classes it claims to cover."
  :tags '(:unit)
  (beads-audit--load-command-modules)
  (let* ((entry (cdr (assoc "admin.compact"
                            beads-meta-parity-intentional-collisions)))
         (members (plist-get entry :classes)))
    (should members)
    (dolist (cls members)
      (should (cl--find-class cls))
      (should (equal "admin compact"
                     (ignore-errors
                       (beads-command-subcommand (make-instance cls))))))
    ;; The recorded set is exactly what the gate treats as intentional.
    (should (beads-audit--intentional-collision-p "admin.compact" members))))

;;; ------------------------------------------------------------
;;; Integration tests -- live `bd' walk (the drift gate proper)
;;; ------------------------------------------------------------

(ert-deftest beads-audit-gate-command-presence ()
  "Every addressable `bd' leaf command has a `beads-defcommand' class.
Router groups and declared non-goals are excluded by policy."
  :tags '(:integration)
  (skip-unless (beads-audit-bd-available-p))
  (let ((missing (beads-audit-missing-commands)))
    (when missing
      (ert-fail
       (format
        (concat "Unclassed bd commands found.  Add a `beads-defcommand'"
                " class, or record the path in"
                " `beads-meta-parity-non-goal-commands':\n%s")
        (mapconcat (lambda (p) (concat "  - " p))
                   (sort (copy-sequence missing) #'string<) "\n"))))))

(ert-deftest beads-audit-gate-no-new-slot-drift ()
  "No slot drift beyond the accepted baseline.
Fails on a new CLI flag without a slot, or an unexpected multi-class
collision."
  :tags '(:integration)
  (skip-unless (beads-audit-bd-available-p))
  (let ((drift (beads-audit-slot-drift)))
    (when drift
      (ert-fail
       (format
        (concat "New CLI/slot drift found.  Add the slot, or record it"
                " in `beads-meta-parity-accepted-drift' /"
                " `beads-meta-parity-non-goal-flags':\n%s")
        (mapconcat
         (lambda (d)
           (format "  - %s %s [%s]"
                   (plist-get d :path)
                   (if (plist-get d :flag)
                       (concat "--" (plist-get d :flag))
                     "")
                   (plist-get d :kind)))
         drift "\n"))))))

(ert-deftest beads-audit-gate-drift-detector-is-live ()
  "Clearing the accepted-drift baseline surfaces the known real gaps.
This guards against the no-new-drift gate passing vacuously: with the
baseline emptied, the detector must still find the documented deferred
gaps, proving it would catch a genuinely new one."
  :tags '(:integration)
  (skip-unless (beads-audit-bd-available-p))
  (let ((beads-meta-parity-accepted-drift nil))
    (should (beads-audit-slot-drift))))

(provide 'beads-audit-test)
;;; beads-audit-test.el ends here
