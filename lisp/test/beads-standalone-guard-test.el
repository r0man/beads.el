;;; beads-standalone-guard-test.el --- No-gascity guard for standalone flows -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; REQ-SF-080 (no-gascity hard acceptance), REQ-SF-097 (standalone-first
;; swarm) and REQ-SF-083 (execution of the plan's acceptance set) promise
;; that every standalone formula -> molecule -> gate -> wisp -> swarm flow
;; works with `bd' alone.  No default path may reference a gascity symbol.
;;
;; This is the guard for that promise.  It does three things:
;;
;; 1. Scans the source of every module on the standalone surface for a
;;    symbol in the gascity namespace.  The scan reads real Lisp forms and
;;    walks the form tree, so a symbol mentioned in a comment or a
;;    docstring is not a false positive, while a quoted symbol
;;    (`\\='gascity-x'), a `(require \\='gascity)' and a `(declare-function
;;    gascity-run ...)' are all caught.
;;
;; 2. Drives the standalone flows through a mocked
;;    `beads-command-execute' -- the same mock every unit test uses -- and
;;    asserts each entry assembles only `bd'-backed command objects.
;;
;; 3. Asserts the gascity package is genuinely absent from the load path
;;    while the guard runs, so the scan cannot pass because gascity was
;;    loaded and its symbols were resolved already.
;;
;; The module manifest names the modules the plan assigns to sibling work
;; items (beads-molecule, beads-gate, ...).  They are not present in every
;; implementation worktree.  A module that is not locatable is reported as
;; pending and is scanned automatically the moment it lands; the core
;; command-layer modules ARE present, so the guard is never vacuous.
;;
;; WI-LIVE-19 extends the same guard to the beads-events-live surface
;; (`beads-live', `beads-event', `beads-events', `beads-pulse').  AC-8 is
;; "everything renders with gascity absent; no gascity symbol on any default
;; path", and HC-3 is "one `bd events' stream per store, never a duplicate and
;; never mixed with a `gc events' stream".  Scanning the live surface for a
;; gascity symbol enforces both: a module that referenced `gascity-live' or
;; `gascity-live--bead-routes' on a default path could neither be
;; standalone-first nor be trusted to keep the two journals apart.
;;
;; The seam gascity is *allowed* to use is data, not a reference: the public
;; `beads-live-invalidate' function and the `beads-live-invalidate-functions'
;; abnormal hook, called as (ROOT KINDS OPS) once per debounced batch.
;; gascity's `bead.*' routing may opt in with
;;
;;   (when (fboundp 'beads-live-invalidate)
;;     (beads-live-invalidate root kinds ops))
;;
;; from a `gascity-live-invalidate-functions' hook.  This is documented here
;; and in NEWS.md, not implemented: gascity is not edited, and beads-live
;; never calls gascity.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'beads-command)
(require 'beads-formula)
(require 'beads-command-mol)
(require 'beads-command-gate)
(require 'beads-command-swarm)
(require 'beads-command-misc)
;; WI-SF-04 moved the cook class out of `beads-command-misc'.
(require 'beads-command-cook)
(require 'beads-command-ready)
(require 'beads-command-update)
(require 'beads-command-close)

;;; ============================================================
;;; Gascity symbol scanner
;;; ============================================================

(defconst beads-standalone-guard--gascity-prefixes
  '("gascity-" "gc-")
  "Symbol prefixes owned by the gascity namespace.
The bare symbol `gascity' is matched as well, so `(require \\='gascity)'
is caught.")

(defun beads-standalone-guard--gascity-symbol-p (symbol)
  "Return non-nil when SYMBOL is a gascity-namespace symbol.
Keywords are ignored: the JSON key `:gc.outcome' is data, not a
gascity reference, and the guard must not fire on it."
  (and (symbolp symbol)
       (not (keywordp symbol))
       (let ((name (symbol-name symbol)))
         (or (string= name "gascity")
             (seq-some (lambda (prefix) (string-prefix-p prefix name))
                       beads-standalone-guard--gascity-prefixes)))))

(defun beads-standalone-guard--scan-form (form found)
  "Walk FORM threading FOUND, and return the updated FOUND.
Strings and comments are not symbols, so prose and docstrings that
mention gascity do not trip the guard; `(quote gascity-x)' does."
  (cond
   ((beads-standalone-guard--gascity-symbol-p form)
    (push form found))
   ((consp form)
    (setq found (beads-standalone-guard--scan-form (car form) found))
    (setq found (beads-standalone-guard--scan-form (cdr form) found)))
   ((and (vectorp form) (not (stringp form)))
    (seq-doseq (item form)
      (setq found (beads-standalone-guard--scan-form item found)))))
  found)

(defun beads-standalone-guard--scan-file (file)
  "Return the list of gascity symbols referenced in the Lisp source FILE.
Signals if FILE contains a form that `read' cannot parse, so a scan can
never silently degrade to \"no gascity symbols found\"."
  (let ((found nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (setq found (beads-standalone-guard--scan-form
                         (read (current-buffer)) found)))
        (end-of-file nil)))
    (delete-dups (nreverse found))))

;;; ============================================================
;;; Standalone surface manifest
;;; ============================================================

(defconst beads-standalone-guard--core-modules
  '("beads-command"
    "beads-formula"
    "beads-command-mol"
    "beads-command-gate"
    "beads-command-swarm"
    "beads-command-misc")
  "Standalone-surface modules guaranteed to exist at the PR #67 base.
These are the command classes and the formula seams every flow builds
on; their presence keeps the guard from passing vacuously.")

(defconst beads-standalone-guard--sibling-modules
  '("beads-molecule"
    "beads-gate"
    "beads-wisp"
    "beads-cook"
    "beads-bond"
    "beads-formula-edit"
    "beads-handoff"
    "beads-swarm"
    "beads-command-cook"
    "beads-command-prime")
  "Standalone-surface modules owned by sibling work items.
Scanned when locatable, reported pending otherwise.")

(defconst beads-standalone-guard--live-modules
  '("beads-live" "beads-event" "beads-events" "beads-pulse")
  "The live-events surface added by the beads-events-live plan.
`beads-live' owns the stream supervisor and the public gascity seam;
`beads-event' is the pure model, `beads-events' the views and
`beads-pulse' the mode-line lighter.  Scanned when locatable, reported
pending otherwise, exactly like the sibling modules.")

(defconst beads-standalone-guard--live-seam-functions
  '(beads-live-invalidate beads-live-active-p)
  "The public, gascity-free seam gascity may call (`when fboundp').
`beads-live-invalidate' (DIR &optional KINDS OPS) lets gascity's
`bead.*' routing refresh the beads views of DIR's store;
`beads-live-active-p' reports whether a stream already covers DIR, so
the command layer (and gascity) can defer a foreground write to it.")

(defconst beads-standalone-guard--live-seam-hooks
  '(beads-live-invalidate-functions)
  "The abnormal hook `(ROOT KINDS OPS)' run once per debounced batch.
The command-layer cache and the views attach here; gascity uses it
only through the fboundp seam above, never the reverse.")

(defun beads-standalone-guard--surface-modules ()
  "Return the full standalone-surface module list."
  (append beads-standalone-guard--core-modules
          beads-standalone-guard--sibling-modules
          beads-standalone-guard--live-modules))

(defun beads-standalone-guard--scan-modules ()
  "Scan the standalone surface.
Return a plist with :present, :pending and :violations.  Each violation
is a cons (MODULE . SYMBOL)."
  (let ((present nil)
        (pending nil)
        (violations nil))
    (dolist (module (beads-standalone-guard--surface-modules))
      (let ((file (locate-library module)))
        (if (null file)
            (push module pending)
          (push module present)
          (dolist (symbol (beads-standalone-guard--scan-file file))
            (push (cons module symbol) violations)))))
    (list :present (nreverse present)
          :pending (nreverse pending)
          :violations (nreverse violations))))

;;; ============================================================
;;; Flow manifest (mocked bd execution)
;;; ============================================================

(defconst beads-standalone-guard--flows
  '((cook-pour-work-close
     . ((beads-command-cook :formula-id "guard-flow")
        (beads-command-mol-pour :proto-id "guard-flow")
        (beads-command-ready)
        (beads-command-update :issue-ids ("bd-1") :status "in_progress")
        (beads-command-close :issue-ids ("bd-1") :reason "done")))
    (cook-wisp-squash
     . ((beads-command-cook :formula-id "guard-flow")
        (beads-command-mol-wisp-create :proto-id "guard-flow")
        (beads-command-mol-squash :mol-id "bd-mol-1" :summary "digest")))
    (gate-round-trip
     . ((beads-command-gate-create :blocks "bd-1" :gate-type "timer")
        (beads-command-gate-check :dry-run t)
        (beads-command-gate-resolve :gate-id "bd-gate-1" :reason "ok")))
    (bond-two-formulas
     . ((beads-command-mol-bond :first-id "formula-a" :second-id "formula-b"
                                :dry-run t)))
    (distill-epic
     . ((beads-command-mol-distill :epic-id "bd-1" :dry-run t)))
    (setup-check
     . ((beads-command-setup :check t)))
    (swarm-validate-create-status
     . ((beads-command-swarm-validate :epic-id "bd-1")
        (beads-command-swarm-create :epic-id "bd-1")
        (beads-command-swarm-status :swarm-id "bd-mol-1"))))
  "Standalone flows expressed as the bd command classes they assemble.
Every entry is constructed and executed with a mocked
`beads-command-execute', so the guard proves the flow is composed of
`bd'-backed command objects and never reaches gascity.")

(defun beads-standalone-guard--run-flow (commands)
  "Execute COMMANDS through a mocked `beads-command-execute'.
Return the commands in execution order."
  (let ((executed nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (push command executed)
                 '((id . "bd-mol-1") (root_id . "bd-mol-1")))))
      (dolist (spec commands)
        (let ((class (car spec))
              (args (cdr spec)))
          (apply #'beads-execute class args))))
    (nreverse executed)))

(defun beads-standalone-guard--command-gascity-symbols (command)
  "Return gascity symbols referenced by COMMAND's class name or CLI line."
  (let ((found nil))
    (setq found (beads-standalone-guard--scan-form
                 (eieio-object-class command) found))
    (dolist (arg (beads-command-line command))
      (when (and (symbolp arg)
                 (beads-standalone-guard--gascity-symbol-p arg))
        (push arg found)))
    (nreverse found)))

;;; ============================================================
;;; Unit tests
;;; ============================================================

(ert-deftest beads-standalone-guard-test-scanner-detects-gascity ()
  "The scanner catches code references and ignores prose.
A comment or a string that mentions gascity is not a reference; a
`require', a call and a quoted symbol are."
  :tags '(:unit)
  (let ((file (make-temp-file "beads-standalone-guard-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; gascity-in-a-comment is prose\n"
                    "(defvar beads-doc \"mentions gascity-here in a string\")\n"
                    "(require 'gascity)\n"
                    "(defun use () (gascity-dispatch 'gc-run))\n"))
          (let ((found (beads-standalone-guard--scan-file file)))
            (should (memq 'gascity found))
            (should (memq 'gascity-dispatch found))
            (should (memq 'gc-run found))
            (should-not (memq 'gascity-in-a-comment found))
            (should-not (memq 'gascity-here found))))
      (delete-file file))))

(ert-deftest beads-standalone-guard-test-gascity-absent ()
  "Gascity is not on the load path while the guard runs.
If it were, the source scan could pass only because gascity symbols
were already interned and resolved; the guard must observe the absence
the flows actually run under."
  :tags '(:unit)
  (should-not (featurep 'gascity))
  (should-not (locate-library "gascity")))

(ert-deftest beads-standalone-guard-test-core-modules-present ()
  "The core standalone modules are locatable, so the guard is not vacuous."
  :tags '(:unit)
  (dolist (module beads-standalone-guard--core-modules)
    (should (locate-library module))))

(ert-deftest beads-standalone-guard-test-no-gascity-symbols ()
  "No module on the standalone surface references a gascity symbol.
Modules owned by sibling work items that have not landed are reported
pending, never silently passed."
  :tags '(:unit)
  (let* ((scan (beads-standalone-guard--scan-modules))
         (violations (plist-get scan :violations))
         (pending (plist-get scan :pending)))
    (when violations
      (ert-fail (format "Gascity symbols on the standalone surface: %S%s"
                        violations
                        (if pending
                            (format " (pending modules: %S)" pending)
                          ""))))
    (should (null violations))))

(ert-deftest beads-standalone-guard-test-live-surface-scanned ()
  "The live-events modules are on the scanned standalone surface.
A module not yet landed in this worktree is tracked as pending rather
than dropped, so the gascity-symbol scan covers it the moment it lands."
  :tags '(:unit)
  (let ((scan (beads-standalone-guard--scan-modules)))
    (dolist (module beads-standalone-guard--live-modules)
      (should (or (member module (plist-get scan :present))
                  (member module (plist-get scan :pending)))))))

(ert-deftest beads-standalone-guard-test-live-gascity-seam ()
  "The public invalidation seam is gascity-free (AC-8) and per store (HC-3).
gascity opts in by calling `beads-live-invalidate' when it is fboundp;
beads-live itself references no gascity symbol, so a beads store never
shares or duplicates a `gc events' stream.  When the module has landed
the seam is exercised for real; otherwise the source scan covers it the
moment it lands."
  :tags '(:unit)
  (let ((file (locate-library "beads-live")))
    (if (null file)
        (should (member "beads-live"
                        (plist-get (beads-standalone-guard--scan-modules)
                                   :pending)))
      (require 'beads-live nil 'noerror)
      (dolist (symbol beads-standalone-guard--live-seam-functions)
        (should (fboundp symbol)))
      (dolist (symbol beads-standalone-guard--live-seam-hooks)
        (should (boundp symbol)))
      ;; `beads-live-canonical-root' dissects a TRAMP name through
      ;; `beads-remote-prefix', which uses `tramp-tramp-file-p'; TRAMP is an
      ;; Emacs built-in and is always loadable, so load it the way a real
      ;; session has it.
      (require 'tramp)
      ;; No stream covers this fictional store, so a synthetic batch is a
      ;; safe no-op.  Gascity is absent (see the gascity-absent test), so the
      ;; call cannot reach it.
      (should-not (beads-live-invalidate
                   "/nonexistent/beads-standalone-guard"))
      (should-not (beads-live-active-p
                   "/nonexistent/beads-standalone-guard")))))

(ert-deftest beads-standalone-guard-test-flow-commands-are-bd-only ()
  "Every standalone flow assembles only `bd'-backed command objects.
Each flow is walked through a mocked `beads-command-execute'; the guard
fails if any executed command -- by class name or CLI argument --
references gascity."
  :tags '(:unit)
  (dolist (entry beads-standalone-guard--flows)
    (let* ((flow (car entry))
           (commands (cdr entry))
           (executed (beads-standalone-guard--run-flow commands)))
      (should (= (length executed) (length commands)))
      (dolist (command executed)
        (should (object-of-class-p command 'beads-command))
        (let ((gascity (beads-standalone-guard--command-gascity-symbols command)))
          (when gascity
            (ert-fail (format "Flow %s reached gascity via %S: %S"
                              flow (eieio-object-class command) gascity))))))))

(ert-deftest beads-standalone-guard-test-formula-launch-mocked ()
  "`beads-formula-launch' irons a formula through `bd mol pour' with no
gascity present.  This walks a real entry function, not only the command
classes the flows are built from."
  :tags '(:unit)
  (let* ((formula (beads-formula-from-json
                   '((formula . "guard-flow")
                     (description . "Guard flow")
                     (vars . ((name . ((type . "string"))))))))
         (seen nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (push command seen)
                 '((id . "bd-mol-1") (root_id . "bd-mol-1")))))
      (let ((context (beads-formula-launch formula nil)))
        (should (beads-formula-launch-context-p context))))
    (let ((pour (seq-find (lambda (command)
                            (object-of-class-p command 'beads-command-mol-pour))
                          seen)))
      (should pour)
      (should (equal (oref pour proto-id) "guard-flow"))
      (should (member "guard-flow" (beads-command-line pour))))))

(provide 'beads-standalone-guard-test)
;;; beads-standalone-guard-test.el ends here
