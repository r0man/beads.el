;;; beads-bond.el --- Bond flow, preview and bond-point attachment -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The bonding flow (design.md §4.1/§4.2/§5.3/§7.2/§9, REQ-SF-050,
;; REQ-SF-051, REQ-SF-052).  Bonding combines two protos, molecules or
;; formulas into a compound with `bd mol bond' (WI-SF-08).  The command
;; class (`beads-command-mol-bond') stays in `beads-command-mol.el';
;; this module owns the flow, the dry-run preview and the bond-point
;; attachment seam.
;;
;; Public ABI:
;;
;; - `beads-bond-operand-functions' is a hook of candidate providers.
;;   Each function is called with no arguments and returns a list of
;;   `(NAME . KIND)' candidates, where KIND is `formula', `proto' or
;;   `molecule'.  The default value is `beads-bond--default-operands',
;;   which contributes every formula name and every molecule id.  An
;;   extension (for example Gas City) adds its own providers with
;;   `add-hook'; an empty hook still degrades to free-text entry.
;; - `beads-bond-operand-reader' is the public `(PROMPT) -> id'
;;   completion over the collected candidates; it accepts a free-text
;;   id when nothing matches, so a proto that is not listable is still
;;   reachable by name.
;; - `beads-bond-command' turns a flow scope into the
;;   `beads-command-mol-bond' object (the pure, testable seam).
;; - `beads-bond-preview' renders the client-side explanation plus the
;;   real `bd mol bond --dry-run' output in a read-only buffer.
;; - `beads-bond-run' applies the bond exactly as shown.
;; - `beads-bond' / `beads-bond-for' open the flow.  A formula detail
;;   view passes the formula and the picked bond point; the molecule
;;   view passes its root id as the pre-filled source operand.
;;
;; Bond points: a formula may declare `compose.bond_points' (id,
;; description, before_step/after_step, parallel).  `bd mol bond' has
;; no flag for an attachment site, so the flow *targets* a named bond
;; point by recording it, showing it in the preview, and using its
;; `parallel' flag as the default bond type.  WI-SF-05 adds the typed
;; `bond-points' slot to `beads-formula'; this module consumes it when
;; present and degrades gracefully when it is not, so it works before
;; that module lands.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'transient)
(require 'beads-prefix)
(require 'beads-command)
(require 'beads-command-mol)
(require 'beads-command-formula)
(require 'beads-command-list)
(require 'beads-types)

;;; ============================================================
;;; Small helpers
;;; ============================================================

(defconst beads-bond-types '("sequential" "parallel" "conditional")
  "Bond types understood by `bd mol bond', in cycle order.")

(defconst beads-bond-phases '(nil "pour" "ephemeral")
  "Phase override cycle: follow the target, pour, or ephemeral.")

(defun beads-bond--blank-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (and (stringp value) (string-empty-p value))))

(defun beads-bond--scope-set (scope &rest keys-values)
  "Return a copy of SCOPE with KEYS-VALUES plist entries set.
SCOPE is never mutated."
  (let ((copy (copy-sequence scope)))
    (while keys-values
      (setq copy (plist-put copy (pop keys-values) (pop keys-values))))
    copy))

;;; ============================================================
;;; Operand discovery and completion
;;; ============================================================

(defun beads-bond--formula-candidates ()
  "Return `(NAME . formula)' candidates for every known formula."
  (ignore-errors
    (mapcar (lambda (summary)
              (cons (oref summary name) 'formula))
            (beads-command-execute
             (beads-command-formula-list :json t)))))

(defun beads-bond--molecule-candidates ()
  "Return `(ID . molecule)' candidates for every molecule in the store."
  (ignore-errors
    (mapcar (lambda (issue)
              (cons (oref issue id) 'molecule))
            (beads-command-execute
             (beads-command-list :issue-type "molecule" :all t :json t)))))

(defun beads-bond--default-operands ()
  "Default operand providers: all formula names, then all molecule ids.
Protos are not listable through `bd', but a cooked proto has the same
id as its formula, so the formula candidates cover them.  A free-text
id is still accepted by `beads-bond-operand-reader'."
  (append (beads-bond--formula-candidates)
          (beads-bond--molecule-candidates)))

(defvar beads-bond-operand-functions (list #'beads-bond--default-operands)
  "Hook of functions returning bond operand candidates.
Each function is called with no arguments and returns a list of
`(NAME . KIND)' pairs; KIND is `formula', `proto' or `molecule'.
The default value contributes every formula and molecule.  Extensions
`add-hook' their own providers to add targets (for example a
Gas-City-specific store listing).  Candidates are de-duplicated by
name, first provider wins, so an extension cannot shadow a formula.")

(defun beads-bond-operand-candidates ()
  "Collect and de-duplicate the candidates from the provider hook.
Return a list of `(NAME . KIND)' pairs, preserving provider order."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (function beads-bond-operand-functions)
      (dolist (candidate (ignore-errors (funcall function)))
        (let ((name (car candidate)))
          (unless (or (beads-bond--blank-p name)
                      (gethash name seen))
            (puthash name t seen)
            (push candidate result)))))
    (nreverse result)))

(defun beads-bond-operand-kind (name)
  "Return the provider-reported kind of operand NAME, or nil."
  (cdr (assoc name (beads-bond-operand-candidates))))

(defvar beads-bond-operand-history nil
  "Minibuffer history for bond operand entry.")

;;;###autoload
(defun beads-bond-operand-reader (prompt)
  "Read a bond operand with PROMPT over the provider candidates.
Return the id string.  Candidates are rendered as \"NAME (KIND)\" but
the id alone is returned; when the user types an unmatched id it is
accepted verbatim (a proto is not listable, so free entry must work)."
  (let* ((candidates (beads-bond-operand-candidates))
         (collection (mapcar (lambda (candidate)
                               (cons (format "%s (%s)"
                                             (car candidate)
                                             (cdr candidate))
                                     (car candidate)))
                             candidates)))
    (completing-read prompt collection nil nil nil
                     'beads-bond-operand-history)))

;;; ============================================================
;;; Bond points
;;; ============================================================

(defun beads-bond-bond-points (formula)
  "Return FORMULA's declared bond points as a list, or nil.
FORMULA may be a `beads-formula' object (WI-SF-05 adds the
`bond-points' slot), a plist carrying `:bond-points', or an alist
carrying `bond_points'.  When the typed slot does not exist yet this
returns nil instead of signalling, so the flow works without WI-SF-05."
  (cond
   ((null formula) nil)
   ((eieio-object-p formula)
    (let ((slot (cond ((slot-exists-p formula 'bond-points) 'bond-points)
                      ((slot-exists-p formula 'bond_points) 'bond_points))))
      (when slot (eieio-oref formula slot))))
   ((consp formula)
    (or (plist-get formula :bond-points)
        (alist-get 'bond_points formula)
        (alist-get 'bond-points formula)))
   (t nil)))

(defun beads-bond--bp-get (bond-point field &optional default)
  "Return FIELD from BOND-POINT, which may be an object, alist or plist.
FIELD is a symbol; the hyphenated and underscored spellings are both
tried, so `before-step' and `before_step' resolve to the same slot."
  (cond
   ((null bond-point) default)
   ((eieio-object-p bond-point)
    (let* ((name (symbol-name field))
           (slots (list field
                        (intern (replace-regexp-in-string "_" "-" name))
                        (intern (replace-regexp-in-string "-" "_" name))))
           (slot (cl-find-if (lambda (candidate)
                               (slot-exists-p bond-point candidate))
                             slots)))
      (if slot (eieio-oref bond-point slot) default)))
   ((consp bond-point)
    (let* ((name (symbol-name field))
           (variants (list name
                           (replace-regexp-in-string "_" "-" name)
                           (replace-regexp-in-string "-" "_" name))))
      (or
       ;; A plist: (:id "entry" :before-step "design").
       (and (keywordp (car bond-point)) (plist-get bond-point field))
       ;; A bare (KEY . VALUE) pair, whichever spelling or type.
       (let ((key (car bond-point)))
         (when (and (not (consp key))
                    (member (format "%s" key) variants))
           (cdr bond-point)))
       ;; An alist of entries, string or symbol keys.
       (catch 'found
         (dolist (entry bond-point)
           (when (and (consp entry)
                      (member (format "%s" (car entry)) variants))
             (throw 'found (cdr entry))))
         nil)
       default)))
   (t default)))

(defun beads-bond--bond-point-id (bond-point)
  "Return BOND-POINT's declared id, or nil."
  (beads-bond--bp-get bond-point 'id))

(defun beads-bond--bond-point-description (bond-point)
  "Return BOND-POINT's description, or nil."
  (beads-bond--bp-get bond-point 'description))

(defun beads-bond--bond-point-before (bond-point)
  "Return the step BOND-POINT attaches before, or nil."
  (beads-bond--bp-get bond-point 'before_step))

(defun beads-bond--bond-point-after (bond-point)
  "Return the step BOND-POINT attaches after, or nil."
  (beads-bond--bp-get bond-point 'after_step))

(defun beads-bond--bond-point-parallel-p (bond-point)
  "Return non-nil when BOND-POINT declares parallel composition."
  (beads-bond--bp-get bond-point 'parallel))

(defun beads-bond--find-bond-point (id bond-points)
  "Return the bond point in BOND-POINTS whose id is ID, or nil."
  (when (and id bond-points)
    (cl-find id bond-points :key #'beads-bond--bond-point-id :test #'equal)))

(defun beads-bond--bond-point-describe (bond-point)
  "Return a one-line human description of BOND-POINT."
  (let ((id (beads-bond--bond-point-id bond-point))
        (description (beads-bond--bond-point-description bond-point))
        (before (beads-bond--bond-point-before bond-point))
        (after (beads-bond--bond-point-after bond-point))
        (parallel (beads-bond--bond-point-parallel-p bond-point)))
    (format "%s%s%s%s"
            (or id "?")
            (if (beads-bond--blank-p description) "" (format " — %s" description))
            (cond (before (format " (before step %s)" before))
                  (after (format " (after step %s)" after))
                  (t ""))
            (if parallel " [parallel]" ""))))

(defun beads-bond--bond-point-type (bond-point bond-points)
  "Return the default bond type for BOND-POINT within BOND-POINTS.
A parallel attachment site seeds a parallel bond; a missing or
non-parallel site seeds sequential."
  (let ((match (beads-bond--find-bond-point bond-point bond-points)))
    (if (beads-bond--bond-point-parallel-p match) "parallel" "sequential")))

(defun beads-bond--formula-for (name)
  "Resolve NAME to a `beads-formula' object, or nil.
Molecule ids and free text that are not formulas resolve to nil."
  (when (not (beads-bond--blank-p name))
    (ignore-errors
      (beads-command-execute
       (beads-command-formula-show :formula-name name :json t)))))

;;; ============================================================
;;; Command assembly
;;; ============================================================

(defun beads-bond--var-args (vars)
  "Return VARS as a list of \"name=value\" strings for `bd mol bond'."
  (mapcar (lambda (cell) (format "%s=%s" (car cell) (cdr cell)))
          (append vars nil)))

(defun beads-bond-command (scope &optional dry-run json)
  "Build the `beads-command-mol-bond' object for SCOPE.
DRY-RUN adds `--dry-run'; JSON requests structured output.  SCOPE is
a plist with the flow state: `:first', `:second', `:type', `:as',
`:phase', `:ref' and `:vars'."
  (beads-command-mol-bond
   :first-id (plist-get scope :first)
   :second-id (plist-get scope :second)
   :dry-run (and dry-run t)
   :bond-type (or (plist-get scope :type) "sequential")
   :as (plist-get scope :as)
   :pour (equal (plist-get scope :phase) "pour")
   :ephemeral (equal (plist-get scope :phase) "ephemeral")
   :ref (plist-get scope :ref)
   :var (beads-bond--var-args (plist-get scope :vars))
   :json (and json t)))

;;; ============================================================
;;; Validation
;;; ============================================================

(defun beads-bond--formula-pair-p (scope)
  "Return non-nil when both operands of SCOPE are known formulas/protos."
  (let ((first (plist-get scope :first-kind))
        (second (plist-get scope :second-kind)))
    (and (memq first '(formula proto))
         (memq second '(formula proto)))))

(defun beads-bond--ready-p (scope)
  "Return non-nil when both operands of SCOPE are set."
  (and (not (beads-bond--blank-p (plist-get scope :first)))
       (not (beads-bond--blank-p (plist-get scope :second)))))

(defun beads-bond-validate (scope)
  "Return a list of validation warnings for SCOPE.
An empty list means the flow is ready.  The result name applies only
to a proto + proto bond; an unknown bond point is reported too."
  (let ((warnings nil))
    (unless (not (beads-bond--blank-p (plist-get scope :first)))
      (push "A (source) is required" warnings))
    (unless (not (beads-bond--blank-p (plist-get scope :second)))
      (push "B (target) is required" warnings))
    (when (and (plist-get scope :as)
               (plist-get scope :first-kind)
               (plist-get scope :second-kind)
               (not (beads-bond--formula-pair-p scope)))
      (push "Result name applies only to a proto + proto bond" warnings))
    (when (and (plist-get scope :bond-point)
               (not (beads-bond--find-bond-point
                     (plist-get scope :bond-point)
                     (plist-get scope :bond-points))))
      (push (format "Unknown bond point: %s" (plist-get scope :bond-point))
            warnings))
    (nreverse warnings)))

;;; ============================================================
;;; Labels
;;; ============================================================

(defun beads-bond--operand-label (value &optional kind)
  "Return the display label for operand VALUE (KIND when known)."
  (cond
   ((beads-bond--blank-p value) "(pick)")
   (kind (format "%s [%s]" value kind))
   (t value)))

(defun beads-bond--next-type (current)
  "Return the bond type after CURRENT in `beads-bond-types'."
  (or (cadr (member current beads-bond-types))
      (car beads-bond-types)))

(defun beads-bond--next-phase (current)
  "Return the phase after CURRENT in `beads-bond-phases'."
  (or (cadr (member current beads-bond-phases))
      (car beads-bond-phases)))

(defun beads-bond--type-label (scope)
  "Return the bond-type label for SCOPE."
  (or (plist-get scope :type) "sequential"))

(defun beads-bond--phase-label (scope)
  "Return the phase-override label for SCOPE."
  (pcase (plist-get scope :phase)
    ("pour" "pour (persistent)")
    ("ephemeral" "ephemeral (vapor)")
    (_ "follow target")))

(defun beads-bond--as-label (scope)
  "Return the result-name label for SCOPE."
  (or (plist-get scope :as) "(auto)"))

(defun beads-bond--ref-label (scope)
  "Return the dynamic-ref label for SCOPE."
  (or (plist-get scope :ref) "(none)"))

(defun beads-bond--vars-label (scope)
  "Return the variable-substitution label for SCOPE."
  (let ((vars (plist-get scope :vars)))
    (if vars
        (mapconcat (lambda (cell) (format "%s=%s" (car cell) (cdr cell)))
                   vars " ")
      "(none)")))

(defun beads-bond--bond-point-label (scope)
  "Return the bond-point label for SCOPE."
  (let ((point (plist-get scope :bond-point)))
    (if (beads-bond--blank-p point)
        "(none)"
      (let ((match (beads-bond--find-bond-point
                    point (plist-get scope :bond-points))))
        (if match
            (beads-bond--bond-point-describe match)
          point)))))

(defun beads-bond--state-sentence (scope)
  "Return the one-line operand/type state for SCOPE."
  (format "A %s · B %s · %s"
          (beads-bond--operand-label (plist-get scope :first)
                                     (plist-get scope :first-kind))
          (beads-bond--operand-label (plist-get scope :second)
                                     (plist-get scope :second-kind))
          (beads-bond--type-label scope)))

(defun beads-bond--header-sentence (scope)
  "Return the one-line bond header for SCOPE."
  (concat "Bond (mol bond) — " (beads-bond--state-sentence scope)))

(defun beads-bond--footer (scope)
  "Return the footer line for SCOPE (warnings or ready)."
  (let ((warnings (beads-bond-validate scope)))
    (if warnings
        (mapconcat (lambda (warning) (concat "⚠ " warning)) warnings " · ")
      "✓ ready — P preview · s bond")))

;;; ============================================================
;;; Preview
;;; ============================================================

(defun beads-bond--relation-sentence (scope)
  "Return the operand-relation sentence for SCOPE."
  (pcase (beads-bond--type-label scope)
    ("parallel" "A and B run in parallel.")
    ("conditional" "B runs only if A fails.")
    (_ "B depends on A; A closes → B unblocks.")))

(defun beads-bond-preview-text (scope)
  "Return the client-side preview lines for SCOPE.
These are the lines this module can derive without touching the
store; `beads-bond--dry-run-output' adds the `bd' dry run."
  (let ((first (or (plist-get scope :first) "?"))
        (second (or (plist-get scope :second) "?"))
        (point (plist-get scope :bond-point)))
    (append
     (list (format "Bond preview — %s + %s (%s, dry-run)"
                   first second (beads-bond--type-label scope))
           (format "  %s" (beads-bond--relation-sentence scope))
           (format "  Phase: %s" (beads-bond--phase-label scope))
           (format "  Result name: %s" (beads-bond--as-label scope))
           (format "  Ref: %s" (beads-bond--ref-label scope))
           (format "  Vars: %s" (beads-bond--vars-label scope)))
     (when (not (beads-bond--blank-p point))
       (list (format "  Bond point: %s" (beads-bond--bond-point-label scope)))))))

(defun beads-bond--dry-run-output (scope)
  "Return the human-readable `bd mol bond --dry-run' output for SCOPE.
A command failure is rendered as an error line rather than signalled,
so the preview buffer always stays usable."
  (condition-case err
      (beads-command-execute (beads-bond-command scope t nil))
    (error (format "Error: %s" (error-message-string err)))))

(defconst beads-bond-preview-buffer-name "*beads-bond: preview*"
  "Name of the bond dry-run preview buffer.")

(defvar-local beads-bond-preview--scope nil
  "The flow scope the bond preview buffer previews.")

(defun beads-bond--preview-paint (buffer scope)
  "Paint BUFFER's preview sections for SCOPE; return BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s  (s bond · q bury)\n"
                      (car (beads-bond-preview-text scope)))
              (make-string 72 ?─) "\n")
      (dolist (line (cdr (beads-bond-preview-text scope)))
        (insert line "\n"))
      (insert "\n" (beads-bond--dry-run-output scope) "\n")
      (goto-char (point-min))))
  buffer)

(defun beads-bond-preview (scope)
  "Open the read-only dry-run preview buffer for SCOPE.
The preview is never a gate: `s' in the buffer, and in the transient,
applies the bond with or without opening it."
  (let ((buffer (get-buffer-create beads-bond-preview-buffer-name)))
    (with-current-buffer buffer
      (beads-bond-preview-mode)
      (setq beads-bond-preview--scope scope)
      (beads-bond--preview-paint buffer scope))
    (pop-to-buffer buffer)))

(defun beads-bond-preview-run ()
  "Apply exactly the bond this preview buffer previews."
  (interactive)
  (unless beads-bond-preview--scope
    (user-error "Nothing to bond from this preview"))
  (beads-bond-run beads-bond-preview--scope))

(defvar-keymap beads-bond-preview-mode-map
  :doc "Keymap of the bond preview buffer.\n`s' bonds, `q' buries."
  "s" #'beads-bond-preview-run
  "q" #'quit-window)

(define-derived-mode beads-bond-preview-mode special-mode "Beads-Bond-Preview"
  "Read-only preview of a pending `bd mol bond' (mockup §9b).
The buffer shows the client-side bond explanation followed by the
real `bd mol bond --dry-run' output.  `s' applies exactly what was
previewed; `q' buries.  The preview never gates the transient's `s'."
  (setq-local truncate-lines nil))

;;; ============================================================
;;; Run
;;; ============================================================

(defun beads-bond-run (scope)
  "Apply the bond described by SCOPE and return the parsed result.
Signals `beads-validation-error' when the flow is not ready and lets
`beads-command-error' propagate on a `bd' failure."
  (unless (beads-bond--ready-p scope)
    (user-error "Both operands are required (A and B)"))
  (let* ((result (beads-command-execute (beads-bond-command scope nil t)))
         (id (alist-get 'result_id result))
         (type (alist-get 'result_type result))
         (spawned (alist-get 'spawned result)))
    (message "Bonded%s — %s%s"
             (if type (format " (%s)" type) "")
             (or id "done")
             (if spawned (format ", spawned %s" spawned) ""))
    result))

;;; ============================================================
;;; Transient suffixes
;;; ============================================================

(defun beads-bond--resetup (scope)
  "Re-setup the bond transient with SCOPE, carrying the current values."
  (transient-setup 'beads-bond--transient nil nil :scope scope))

(transient-define-suffix beads-bond--pick-first ()
  "Pick the A (source) operand and rebuild the menu in place."
  :description (lambda (_obj)
                 (concat "A (source): "
                         (beads-bond--operand-label
                          (plist-get (transient-scope) :first)
                          (plist-get (transient-scope) :first-kind))))
  (interactive)
  (let* ((scope (transient-scope))
         (id (beads-bond-operand-reader "A (source): "))
         (kind (beads-bond-operand-kind id))
         (bond-points (beads-bond-bond-points (beads-bond--formula-for id))))
    (beads-bond--resetup
     (beads-bond--scope-set scope
                            :first id :first-kind kind
                            :bond-points bond-points))))

(transient-define-suffix beads-bond--pick-second ()
  "Pick the B (target) operand and rebuild the menu in place."
  :description (lambda (_obj)
                 (concat "B (target): "
                         (beads-bond--operand-label
                          (plist-get (transient-scope) :second)
                          (plist-get (transient-scope) :second-kind))))
  (interactive)
  (let* ((scope (transient-scope))
         (id (beads-bond-operand-reader "B (target): "))
         (kind (beads-bond-operand-kind id)))
    (beads-bond--resetup
     (beads-bond--scope-set scope :second id :second-kind kind))))

(transient-define-suffix beads-bond--cycle-type ()
  "Cycle sequential → parallel → conditional."
  :description (lambda (_obj)
                 (concat "Type: " (beads-bond--type-label (transient-scope))))
  (interactive)
  (let* ((scope (transient-scope))
         (next (beads-bond--next-type (beads-bond--type-label scope))))
    (beads-bond--resetup (beads-bond--scope-set scope :type next))))

(transient-define-suffix beads-bond--cycle-phase ()
  "Cycle follow target → pour → ephemeral."
  :description (lambda (_obj)
                 (concat "Phase: " (beads-bond--phase-label (transient-scope))))
  (interactive)
  (let* ((scope (transient-scope))
         (next (beads-bond--next-phase (plist-get scope :phase))))
    (beads-bond--resetup (beads-bond--scope-set scope :phase next))))

(transient-define-suffix beads-bond--pick-as ()
  "Set the compound result name (`--as', proto + proto only)."
  :description (lambda (_obj)
                 (concat "Result name: "
                         (beads-bond--as-label (transient-scope))))
  (interactive)
  (let* ((scope (transient-scope))
         (name (read-string "Result name: " (plist-get scope :as))))
    (beads-bond--resetup (beads-bond--scope-set scope :as name))))

(transient-define-suffix beads-bond--pick-ref ()
  "Set the dynamic child reference (`--ref', supports {{var}})."
  :description (lambda (_obj)
                 (concat "Ref: " (beads-bond--ref-label (transient-scope))))
  (interactive)
  (let* ((scope (transient-scope))
         (ref (read-string "Ref (e.g. arm-{{name}}): " (plist-get scope :ref))))
    (beads-bond--resetup (beads-bond--scope-set scope :ref ref))))

(transient-define-suffix beads-bond--add-var ()
  "Add or replace a `key=value' variable for `--var' substitution."
  :description (lambda (_obj)
                 (concat "Vars: " (beads-bond--vars-label (transient-scope))))
  (interactive)
  (let* ((scope (transient-scope))
         (entry (read-string "Variable (key=value): "))
         (split (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" entry)))
    (unless split
      (user-error "Expected key=value, got %S" entry))
    (let* ((key (match-string 1 entry))
           (value (match-string 2 entry))
           (vars (cons (cons key value)
                       (cl-remove key (plist-get scope :vars)
                                  :key #'car :test #'equal))))
      (beads-bond--resetup (beads-bond--scope-set scope :vars vars)))))

(transient-define-suffix beads-bond--pick-bond-point ()
  "Target a named bond point on the source formula."
  :description (lambda (_obj)
                 (concat "Bond point: "
                         (beads-bond--bond-point-label (transient-scope))))
  (interactive)
  (let* ((scope (transient-scope))
         (points (plist-get scope :bond-points))
         (names (mapcar #'beads-bond--bond-point-id points))
         (id (if names
                 (completing-read "Bond point: " names nil t)
               (read-string "Bond point id: ")))
         (type (beads-bond--bond-point-type id points)))
    (beads-bond--resetup
     (beads-bond--scope-set scope :bond-point id
                            :type (or type (beads-bond--type-label scope))))))

(transient-define-suffix beads-bond--preview ()
  "Open the dry-run preview for the current flow."
  (interactive)
  (beads-bond-preview (transient-scope)))

(transient-define-suffix beads-bond--apply ()
  "Apply the bond exactly as shown (never gated by the preview)."
  (interactive)
  (beads-bond-run (transient-scope)))

;;; ============================================================
;;; Transient layout
;;; ============================================================

(defun beads-bond--children-specs (_scope)
  "Return the layout specs for the bond transient.
The menu reads the live scope through `transient-scope' so the
A/B/options lines and the footer recompute on every redraw; the
argument is kept for the setup-children contract."
  (list
   (vector "Bond (mol bond)"
           ;; Quoted lambda forms, never runtime closures: transient
           ;; 0.13.8 embeds an `:info' value unquoted into the form it
           ;; `eval's, and on Emacs 29.4 a `(closure ...)' list is then
           ;; called as a function (see the beads-sling note).  A quoted
           ;; lambda survives the `eval' on 29.4 and 31 alike, and reads
           ;; the live scope through `transient-scope'.
           (list :info
                 '(lambda ()
                    (beads-bond--state-sentence (transient-scope))))
           (list :info
                 '(lambda ()
                    (beads-bond--footer (transient-scope)))))
   (vector "Operands"
           (list "A" 'beads-bond--pick-first)
           (list "B" 'beads-bond--pick-second))
   (vector "Options"
           (list "t" 'beads-bond--cycle-type)
           (list "p" 'beads-bond--cycle-phase)
           (list "a" 'beads-bond--pick-as)
           (list "r" 'beads-bond--pick-ref)
           (list "v" 'beads-bond--add-var)
           (list "o" 'beads-bond--pick-bond-point))
   (vector "Actions"
           (list "P" "Dry-run preview" 'beads-bond--preview)
           (list "s" "Bond" 'beads-bond--apply)
           (list "q" "Quit" 'transient-quit-one))))

(defun beads-bond--setup-children (_children)
  "Parse `beads-bond--children-specs' for the live scope."
  (transient-parse-suffixes
   'beads-bond--transient
   (beads-bond--children-specs (transient-scope))))

(beads-define-prefix beads-bond--transient ()
  "Bond two protos, molecules or formulas (REQ-SF-050, REQ-SF-051).
One entry point covers operand picking, bond type, phase override,
dynamic ref/vars, bond-point targeting and the dry-run preview.  `s'
applies with or without a preview; `P' opens the full preview."
  [ :class transient-subgroups :setup-children beads-bond--setup-children ])

;;; ============================================================
;;; Entry points
;;; ============================================================

(defun beads-bond--context-first ()
  "Return the operand to pre-fill from the current buffer, or nil.
The molecule view exposes `beads-molecule--root'; the formula detail
exposes `beads-formula-show--formula-name'.  Both are read with
`bound-and-true-p' so this module does not require their owners."
  (cond
   ((bound-and-true-p beads-molecule--root) beads-molecule--root)
   ((bound-and-true-p beads-formula-show--formula-name)
    beads-formula-show--formula-name)
   (t nil)))

(defun beads-bond--interactive-args ()
  "Return the pre-filled `(FIRST BOND-POINT)' for `beads-bond'."
  (list (beads-bond--context-first) nil))

;;;###autoload
(defun beads-bond (&optional first bond-point)
  "Open the bond flow, optionally pre-filling FIRST and BOND-POINT.
FIRST is a formula, proto or molecule id; BOND-POINT is the id of a
declared `compose.bond_points' attachment site on FIRST.  Called
interactively, FIRST is derived from the buffer at point (molecule
root or the formula being inspected)."
  (interactive (beads-bond--interactive-args))
  (let* ((points (beads-bond-bond-points (beads-bond--formula-for first)))
         (scope (list :first first
                      :first-kind (beads-bond-operand-kind first)
                      :second nil
                      :second-kind nil
                      :type (beads-bond--bond-point-type bond-point points)
                      :as nil
                      :phase nil
                      :ref nil
                      :vars nil
                      :bond-point bond-point
                      :bond-points points)))
    (transient-setup 'beads-bond--transient nil nil :scope scope)))

(defun beads-bond-for (first &optional bond-point)
  "Open the bond flow with FIRST and BOND-POINT, for programmatic callers.
This is the non-interactive seam used by the formula detail (bond at a
named bond point) and the molecule view (`b' pre-fills its root)."
  (beads-bond first bond-point))

(provide 'beads-bond)
;;; beads-bond.el ends here
