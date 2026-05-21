;;; beads-ralph-launcher.el --- Configure-and-launch panel for Ralph -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Singleton vui buffer `*beads-ralph-launcher*' that replaces the
;; `read-char-choice' minibuffer confirm AND the standalone dry-run
;; buffer for the Ralph autonomous-iteration loop (bde-deqx.5).
;;
;; Composition (top-down): header → already-running banner (when an
;; incumbent controller exists for this root) → cap-reached banner
;; (when the concurrent-loops cap is reached) → editable parameter
;; rows (max-iterations, max-budget-usd-per-iter, max-budget-usd,
;; max-turns, permission-mode, worktree, sentinel) → iter-1 prompt
;; preview (front-and-center) → spawn argv preview (collapsed behind a
;; [shell] toggle) → action bar ([L]aunch · [E]dit prompt ·
;; [S]ave-as-template · [q]Cancel).
;;
;; State model: buffer-local vars hold canonical state
;; (`--root-id', `--kind', `--issue', `--params', `--prompt-override',
;; `--shell-expanded', `--baseline-params').  Field `:on-change'
;; callbacks mutate buffer-local state and trigger an explicit
;; `--render' (= remount).  This matches the cockpit's pattern; pure
;; `vui-set-state' is deliberately NOT used because the [E] prompt-edit
;; callback is asynchronous and already needs the remount path.
;;
;; Launch path: `beads-ralph-launcher-launch' calls
;; `beads-agent-ralph-start' directly (NOT `beads-agent-ralph-launch',
;; which would re-trigger the dialog this panel replaces).  Force
;; relaunch on an incumbent goes through
;; `beads-agent-ralph-force-relaunch' (bde-deqx.3) so a money-spending
;; claude is never silently orphaned.
;;
;; Persistence: saved per-root templates live in
;; `beads-agent-ralph-launch-templates', written via
;; `customize-save-variable' so they survive Emacs restarts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vui)
(require 'vui-components)

(require 'beads-agent-ralph)
(require 'beads-agent-ralph-confirm)
(require 'beads-agent-ralph-dashboard)
(require 'beads-agent-prompt-edit)

;; Promoted public helpers from `beads-agent' — autoloaded, so a
;; runtime `require' here would force-load the whole agent module
;; just for two functions.  Declared at the top so the byte
;; compiler is satisfied for every call site below.
(declare-function beads-read-issue-id "beads-agent")
(declare-function beads-issue-read "beads-command-list" (issue-id))

;;; Customization

(defgroup beads-ralph-launcher nil
  "Configure-and-launch panel for the Ralph autonomous-iteration loop."
  :group 'beads
  :prefix "beads-ralph-launcher-")

(defcustom beads-ralph-launcher-buffer-name "*beads-ralph-launcher*"
  "Name of the singleton launcher buffer."
  :type 'string
  :group 'beads-ralph-launcher)

(defcustom beads-ralph-launcher-default-params nil
  "Plist of launcher default params overriding the per-key defcustoms.
Recognised keys: `:max-iterations', `:max-budget-usd-per-iter',
`:max-budget-usd', `:max-turns', `:permission-mode', `:worktree',
`:sentinel'.  Any key absent here falls back to its per-key defcustom
in `beads-agent-ralph'."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'beads-ralph-launcher)

(defcustom beads-agent-ralph-launch-templates nil
  "Alist of per-root saved launch templates: ((ROOT-ID . PARAMS-PLIST) ...).
Populated by the launcher's `[S]ave-as-template' action via
`customize-save-variable' so templates survive Emacs restarts.
Lookup on launcher open: if a template exists for ROOT-ID it seeds the
parameter rows; otherwise the rows fall back to
`beads-ralph-launcher-default-params' + per-key defcustoms.

The value-type is the same plist shape as
`beads-ralph-launcher-default-params'; unrecognised keys are
preserved across save/load but ignored by the launcher."
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'beads-ralph-launcher)

;;; Faces

(defface beads-ralph-launcher-header-face
  '((t :weight bold))
  "Face for the launcher's top header line."
  :group 'beads-ralph-launcher)

(defface beads-ralph-launcher-section-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for section labels (Parameters, Prompt preview, Spawn argv)."
  :group 'beads-ralph-launcher)

(defface beads-ralph-launcher-row-label-face
  '((t :inherit shadow))
  "Face for the left-hand label column of a parameter row."
  :group 'beads-ralph-launcher)

(defface beads-ralph-launcher-banner-warning-face
  '((t :inherit warning :weight bold))
  "Face for the `Already running' banner."
  :group 'beads-ralph-launcher)

(defface beads-ralph-launcher-banner-error-face
  '((t :inherit error :weight bold))
  "Face for the `cap reached' banner."
  :group 'beads-ralph-launcher)

(defface beads-ralph-launcher-preview-face
  '((t :inherit shadow))
  "Face for the prompt / argv preview blocks."
  :group 'beads-ralph-launcher)

;;; Buffer-local state

(defvar-local beads-ralph-launcher--root-id nil
  "Root id (a bd id string) the launcher is targeting.")

(defvar-local beads-ralph-launcher--kind nil
  "Either `issue' or `epic'.
Controls header rendering and the `:kind' threaded into
`beads-agent-ralph-start'.")

(defvar-local beads-ralph-launcher--issue nil
  "Optional `beads-issue' object.
Used to populate the header subtitle line with the issue's title,
priority, and status.")

(defvar-local beads-ralph-launcher--children nil
  "Optional list of `beads-issue' children (epic case).
Shown read-only in the ready-children block.")

(defvar-local beads-ralph-launcher--params nil
  "Plist of the launcher's current editable params.
Keys: `:max-iterations', `:max-budget-usd-per-iter',
`:max-budget-usd', `:max-turns', `:permission-mode', `:worktree',
`:sentinel'.  Mutated by row `:on-change' callbacks; threaded into
the component as a prop.")

(defvar-local beads-ralph-launcher--baseline-params nil
  "Snapshot of `--params' at open time or last `[S]ave-as-template'.
Used for dirty-state detection on quit and for the `*' header marker.")

(defvar-local beads-ralph-launcher--prompt-override nil
  "Optional override string for the iter-1 prompt.
When non-nil it is passed through `:prompt' to
`beads-agent-ralph-start' AND displayed in the prompt preview pane in
place of the rendered template.  Populated by the `[E]dit prompt'
callback.")

(defvar-local beads-ralph-launcher--shell-expanded nil
  "Whether the spawn argv preview pane is expanded.
Collapsed by default; the prompt preview is the headline pane.")

;;; Pure helpers

(defconst beads-ralph-launcher--param-keys
  '(:max-iterations :max-budget-usd-per-iter :max-budget-usd
    :max-turns :permission-mode :worktree :sentinel)
  "Canonical ordered list of the seven launcher param keys.
Single source of truth: `--default-params' seeds them in order,
`--params-equal-p' walks them for dirty detection,
`--params-to-start-args' iterates them when projecting to start
args, and the parameter-rows block renders them in this order.")

(defconst beads-ralph-launcher--permission-modes
  '("bypassPermissions" "acceptEdits" "plan" "default")
  "Choices for the permission-mode row.
Matches the `claude --permission-mode' options Ralph supports.")

(defun beads-ralph-launcher--default-params ()
  "Return the effective default params plist.
Per-key defcustoms in `beads-agent-ralph' provide the base;
`beads-ralph-launcher-default-params' overrides per-key.  The seven
keys in the result match `beads-ralph-launcher--param-keys' and
appear in that order."
  (let ((base
         (list :max-iterations beads-agent-ralph-max-iterations
               :max-budget-usd-per-iter beads-agent-ralph-max-budget-usd-per-iter
               :max-budget-usd beads-agent-ralph-max-budget-usd
               :max-turns beads-agent-ralph-max-turns
               :permission-mode beads-agent-ralph-permission-mode
               :worktree t
               :sentinel beads-agent-ralph-sentinel)))
    (beads-ralph-launcher--merge-params
     base beads-ralph-launcher-default-params)))

(defun beads-ralph-launcher--merge-params (base override)
  "Return a fresh plist: BASE with each key from OVERRIDE applied.
A key present in OVERRIDE wins even when its value is nil; a key
absent from OVERRIDE keeps BASE's value.  Neither input is mutated;
the caller may continue using BASE after the call."
  (let ((result (copy-sequence base))
        (tail override))
    (while tail
      (setq result (plist-put result (car tail) (cadr tail)))
      (setq tail (cddr tail)))
    result))

(defun beads-ralph-launcher--params-equal-p (a b)
  "Return non-nil when plists A and B agree on every canonical key.
Walks `beads-ralph-launcher--param-keys'; extra keys in A or B are
ignored — only the seven canonical launcher keys matter for the
dirty-state comparison."
  (cl-every (lambda (k) (equal (plist-get a k) (plist-get b k)))
            beads-ralph-launcher--param-keys))

(defun beads-ralph-launcher--params-for (root-id)
  "Return the params plist to use when opening the launcher on ROOT-ID.
A saved template for ROOT-ID (from `beads-agent-ralph-launch-templates')
takes precedence over the default params."
  (or (cdr (assoc root-id beads-agent-ralph-launch-templates))
      (beads-ralph-launcher--default-params)))

(defun beads-ralph-launcher--save-template (root-id params)
  "Persist PARAMS as the launch template for ROOT-ID.
Replaces any existing template for ROOT-ID; writes to user-customize
via `customize-save-variable' so the change survives Emacs restart."
  (let ((templates
         (cl-remove root-id beads-agent-ralph-launch-templates
                    :key #'car :test #'equal)))
    (setq beads-agent-ralph-launch-templates
          (cons (cons root-id (copy-sequence params)) templates)))
  (customize-save-variable 'beads-agent-ralph-launch-templates
                           beads-agent-ralph-launch-templates))

(defun beads-ralph-launcher--incumbent (root-id)
  "Return a live (non-terminal) registered controller for ROOT-ID, or nil."
  (when-let* ((ctrl (beads-agent-ralph-controller-for-root root-id)))
    (and (not (beads-agent-ralph--terminal-p ctrl)) ctrl)))

(defun beads-ralph-launcher--params-to-start-args
    (root-id kind params prompt-override)
  "Translate PARAMS plist to a `beads-agent-ralph-start' args plist.
ROOT-ID becomes `:issue'; KIND becomes `:kind'; PROMPT-OVERRIDE (when
non-nil) becomes `:prompt'.  Nil-valued numeric params are dropped so
`-start' falls back to its defcustom defaults; explicit values
override.

The `worktree' param is currently informational only — toggling it
neither forces nor suppresses a worktree because `-start' delegates
the decision to `beads-git-should-use-worktree-p'.  Threading
worktree intent through `-start' is a follow-up."
  (let ((args (list :issue root-id :kind kind)))
    ;; Project only the keys `beads-agent-ralph-start' understands.
    ;; `:worktree' and `:sentinel' are intentionally not threaded;
    ;; see the docstring's "currently informational" note.
    (dolist (k '(:max-iterations :max-budget-usd-per-iter
                 :max-budget-usd :max-turns :permission-mode))
      (when-let* ((v (plist-get params k)))
        (setq args (plist-put args k v))))
    (when prompt-override
      (setq args (plist-put args :prompt prompt-override)))
    args))

(defun beads-ralph-launcher--params-to-preview-args
    (root-id kind params prompt-override)
  "Build the args plist the argv preview ingests for ROOT-ID/KIND.
Return a plist matching `beads-agent-ralph--preview-argv's contract
\(`:prompt', `:project-dir', `:permission-mode', `:max-budget-usd',
`:max-turns', `:extra-args').  PARAMS is the launcher's params plist;
PROMPT-OVERRIDE replaces the rendered iter-1 prompt when non-nil."
  (let* ((effective-budget
          (or (plist-get params :max-budget-usd-per-iter)
              (plist-get params :max-budget-usd)))
         (prompt-text
          (or prompt-override
              (beads-agent-ralph--preview-iter1-prompt root-id kind nil))))
    (list :prompt prompt-text
          :project-dir default-directory
          :permission-mode (plist-get params :permission-mode)
          :max-budget-usd effective-budget
          :max-turns (plist-get params :max-turns))))

(defun beads-ralph-launcher--quote-block (text)
  "Indent TEXT two spaces and trim a trailing newline.
Used for the prompt and argv preview blocks."
  (let* ((trimmed (string-trim-right text))
         (lines (split-string trimmed "\n")))
    (mapconcat (lambda (l) (concat "  " l)) lines "\n")))

;;; Component composition

(defun beads-ralph-launcher--header-block
    (root-id kind issue dirty-p template-loaded-p)
  "Render the launcher header for ROOT-ID.
KIND is `issue' or `epic'; ISSUE is an optional `beads-issue' whose
title / priority / status are folded into a subtitle line.

DIRTY-P prefixes the title with the conventional `*' marker so the
user can see at a glance that the params have unsaved edits relative
to the baseline.  TEMPLATE-LOADED-P appends a `[template]' suffix so
the user knows the rows started from a saved per-root preset rather
than the defaults."
  (let* ((priority (and issue (oref issue priority)))
         (status (or (and issue (oref issue status)) ""))
         (title (or (and issue (oref issue title)) "")))
    (vui-vstack
     (vui-text
      (format "%sLaunch Ralph on %s  (%s%s%s)%s"
              (if dirty-p "*" "")
              root-id
              (symbol-name kind)
              (if priority (format " · P%d" priority) "")
              (if (not (string-empty-p status)) (concat " · " status) "")
              (if template-loaded-p "  [template]" ""))
      :face 'beads-ralph-launcher-header-face)
     (when (not (string-empty-p title))
       (vui-text (concat "  " title) :face 'shadow)))))

(defun beads-ralph-launcher--already-running-block (incumbent)
  "Banner shown when an incumbent loop exists for the target root.
Returns nil when INCUMBENT is nil so the parent `vui-vstack' skips it."
  (when incumbent
    (vui-vstack
     (vui-text
      (format "  Already running for this root  (%s, iter %d/%d, $%.2f)"
              (oref incumbent status)
              (or (oref incumbent iteration) 0)
              (or (oref incumbent max-iterations) 0)
              (or (oref incumbent cumulative-cost-usd) 0.0))
      :face 'beads-ralph-launcher-banner-warning-face)
     (vui-hstack
      :spacing 2
      (vui-text "  ")
      (vui-button "[S]witch to dashboard"
        :face 'beads-ralph-launcher-banner-warning-face
        :on-click (lambda () (beads-ralph-launcher-switch-to-incumbent)))
      (vui-button "[F]orce relaunch"
        :face 'beads-ralph-launcher-banner-warning-face
        :on-click (lambda () (beads-ralph-launcher-force-relaunch)))
      (vui-button "[C]ancel"
        :face 'beads-ralph-launcher-banner-warning-face
        :on-click (lambda () (beads-ralph-launcher-quit)))))))

(defun beads-ralph-launcher--cap-reached-block (cap-reached-p)
  "Banner shown when CAP-REACHED-P indicates the loop cap was hit.
A nil CAP-REACHED-P renders nothing."
  (when cap-reached-p
    (vui-text
     (format "  Max concurrent Ralph loops (%d) reached.  Stop a running loop first."
             beads-agent-ralph-max-concurrent-loops)
     :face 'beads-ralph-launcher-banner-error-face)))

(defun beads-ralph-launcher--ready-children-block (kind children)
  "Optional ready-children listing for an epic root.
Returns nil for non-epic KIND or when CHILDREN is empty."
  (when (and (eq kind 'epic) children)
    (vui-vstack
     (vui-text (format "Ready children (%d shown)" (length children))
               :face 'beads-ralph-launcher-section-face)
     (apply #'vui-vstack
            (mapcar
             (lambda (issue)
               (let ((id (oref issue id))
                     (p (oref issue priority))
                     (type (or (oref issue issue-type) ""))
                     (title (or (oref issue title) "")))
                 (vui-text
                  (format "  • %-14s %-4s %-8s %s"
                          id
                          (if p (format "P%d" p) "--")
                          type
                          title)
                  :face 'shadow)))
             children)))))

(defun beads-ralph-launcher--row-label (label)
  "Render the left-hand LABEL column of a parameter row."
  (vui-text (format "  %-26s" label)
            :face 'beads-ralph-launcher-row-label-face))

(defun beads-ralph-launcher--integer-row (key label value &optional min)
  "Editable integer row labelled LABEL for params KEY with current VALUE.
Optional MIN bounds the parser so e.g. a max-iterations cannot be
set negative through the launcher; nil leaves the parser
unbounded."
  (vui-hstack
   :spacing 2
   (beads-ralph-launcher--row-label label)
   (apply #'vui-integer-field
          :value (or value 0)
          :size 10
          :on-change (lambda (new)
                       (beads-ralph-launcher--set-param key new))
          (when min (list :min min)))))

(defun beads-ralph-launcher--number-row (key label value &optional min)
  "Editable number (int/float) row labelled LABEL for params KEY, current VALUE.
A nil current value is shown as 0; the `--params-to-start-args'
serializer drops zero / nil values so `-start' picks its defcustom.
Optional MIN bounds the parser (e.g. budgets cannot be negative)."
  (vui-hstack
   :spacing 2
   (beads-ralph-launcher--row-label label)
   (apply #'vui-number-field
          :value (or value 0)
          :size 10
          :on-change (lambda (new)
                       (beads-ralph-launcher--set-param key new))
          (when min (list :min min)))))

(defun beads-ralph-launcher--select-row (key label options value)
  "Select row for params KEY labelled LABEL.
OPTIONS is a list of strings; VALUE is the current selection."
  (vui-hstack
   :spacing 2
   (beads-ralph-launcher--row-label label)
   (vui-select
    :value value
    :options (mapcar (lambda (o) (cons o o)) options)
    :on-change (lambda (new)
                 (beads-ralph-launcher--set-param key new)))))

(defun beads-ralph-launcher--checkbox-row (key label checked)
  "Checkbox row for params KEY labelled LABEL.
CHECKED is the current boolean."
  (vui-hstack
   :spacing 2
   (beads-ralph-launcher--row-label label)
   (vui-checkbox
    :checked (not (null checked))
    :on-change (lambda (new)
                 (beads-ralph-launcher--set-param key new)))))

(defun beads-ralph-launcher--field-row (key label value)
  "Plain text field row for params KEY labelled LABEL with current VALUE.
Used for free-form strings like the sentinel."
  (vui-hstack
   :spacing 2
   (beads-ralph-launcher--row-label label)
   (vui-field
    :value (or value "")
    :size 36
    :on-change (lambda (new)
                 (beads-ralph-launcher--set-param key new)))))

(defun beads-ralph-launcher--parameters-block (params)
  "Render the editable parameter rows for PARAMS."
  (vui-vstack
   (vui-text "Parameters" :face 'beads-ralph-launcher-section-face)
   (beads-ralph-launcher--integer-row
    :max-iterations "max-iterations"
    (plist-get params :max-iterations) 1)
   (beads-ralph-launcher--number-row
    :max-budget-usd-per-iter "max-budget-usd-per-iter"
    (plist-get params :max-budget-usd-per-iter) 0)
   (beads-ralph-launcher--number-row
    :max-budget-usd "max-budget-usd (total)"
    (plist-get params :max-budget-usd) 0)
   (beads-ralph-launcher--integer-row
    :max-turns "max-turns"
    (plist-get params :max-turns) 1)
   (beads-ralph-launcher--select-row
    :permission-mode "permission-mode"
    beads-ralph-launcher--permission-modes
    (plist-get params :permission-mode))
   (beads-ralph-launcher--checkbox-row
    :worktree "worktree" (plist-get params :worktree))
   (beads-ralph-launcher--field-row
    :sentinel "sentinel" (plist-get params :sentinel))))

(defun beads-ralph-launcher--prompt-preview-block (root-id kind prompt-override)
  "Iter-1 prompt preview pane for ROOT-ID/KIND.
Inline `[E]dit' button delegates to `beads-ralph-launcher-edit-prompt'.
PROMPT-OVERRIDE displays in place of the rendered template when
non-nil."
  (let* ((text
          (condition-case err
              (or prompt-override
                  (beads-agent-ralph--preview-iter1-prompt root-id kind nil))
            (error (format "<preview unavailable: %S>" err)))))
    (vui-vstack
     (vui-hstack
      :spacing 2
      (vui-text "Iter-1 prompt preview" :face 'beads-ralph-launcher-section-face)
      (vui-button "[E]dit"
        :on-click (lambda () (beads-ralph-launcher-edit-prompt))))
     (vui-text (beads-ralph-launcher--quote-block text)
               :face 'beads-ralph-launcher-preview-face))))

(defun beads-ralph-launcher--argv-preview-block
    (root-id kind params prompt-override shell-expanded)
  "Spawn argv preview pane for ROOT-ID/KIND.
Collapsed under a `[shell]' checkbox; reveals when SHELL-EXPANDED.
PARAMS feeds into argv assembly; PROMPT-OVERRIDE replaces the
rendered iter-1 prompt when non-nil."
  (vui-vstack
   (vui-hstack
    :spacing 2
    (vui-text "Spawn argv" :face 'beads-ralph-launcher-section-face)
    (vui-checkbox
     :checked shell-expanded
     :label "[shell]"
     :on-change (lambda (new)
                  (beads-ralph-launcher--set-shell-expanded new))))
   (when shell-expanded
     ;; The argv preview chain (params → preview-args → preview-argv
     ;; → shell-string) reaches into `beads-agent-ralph' internals;
     ;; any breakage there should degrade the pane rather than
     ;; bubble up to the launcher-wide error boundary.
     (let* ((text
             (condition-case err
                 (let* ((args (beads-ralph-launcher--params-to-preview-args
                               root-id kind params prompt-override))
                        (argv (beads-agent-ralph--preview-argv args)))
                   (beads-agent-ralph--argv-to-shell-string argv))
               (error (format "<argv preview unavailable: %S>" err)))))
       (vui-text (beads-ralph-launcher--quote-block text)
                 :face 'beads-ralph-launcher-preview-face)))))

(defun beads-ralph-launcher--action-bar (incumbent cap-reached-p)
  "Bottom action bar and key legend.
The Launch button is disabled when an INCUMBENT exists or
CAP-REACHED-P is non-nil; the other buttons are always enabled.
A second row beneath the buttons spells out the full keymap so
users don't have to TAB onto a button to discover the shortcuts."
  (vui-vstack
   (vui-hstack
    :spacing 3
    (vui-text "  ")
    (vui-button "[L]aunch"
      :disabled (or (and incumbent t) cap-reached-p)
      :face (if (or incumbent cap-reached-p) 'shadow 'success)
      :on-click (lambda () (beads-ralph-launcher-launch)))
    (vui-button "[E]dit prompt"
      :on-click (lambda () (beads-ralph-launcher-edit-prompt)))
    (vui-button "[S]ave-as-template"
      :on-click (lambda () (beads-ralph-launcher-save-template)))
    (vui-button "[q]Cancel"
      :on-click (lambda () (beads-ralph-launcher-quit))))
   (vui-text
    "  Keys: [L]aunch [E]dit [S]ave-template [F]orce-relaunch [g]refresh [?]help [q]uit"
    :face 'shadow)))

(vui-defcomponent beads-ralph-launcher--root
    (root-id kind issue child-issues params prompt-override shell-expanded
             incumbent cap-reached-p dirty-p template-loaded-p)
  "Top-level launcher composition.
All state lives in props; the buffer-local state owned by the
launcher mode is threaded in by `beads-ralph-launcher--render'.
The prop is named `child-issues' rather than `children' so it does
not collide with `vui-defcomponent's implicit `children' binding."
  :render
  (vui-error-boundary
   :id 'beads-ralph-launcher
   :fallback
   (lambda (err)
     (vui-vstack
      (vui-text (format "Launcher render error: %S" err) :face 'error)
      (vui-text "(re-render with g; see *Messages*)" :face 'shadow)))
   :children
   (list
    (vui-vstack
     :spacing 1
     (beads-ralph-launcher--header-block
      root-id kind issue dirty-p template-loaded-p)
     (beads-ralph-launcher--already-running-block incumbent)
     (beads-ralph-launcher--cap-reached-block cap-reached-p)
     (beads-ralph-launcher--ready-children-block kind child-issues)
     (beads-ralph-launcher--parameters-block params)
     (beads-ralph-launcher--prompt-preview-block root-id kind prompt-override)
     (beads-ralph-launcher--argv-preview-block
      root-id kind params prompt-override shell-expanded)
     (beads-ralph-launcher--action-bar incumbent cap-reached-p)))))

;;; Render driver + state mutators

(defun beads-ralph-launcher--render (buffer)
  "Mount the launcher component into BUFFER from buffer-local state.
`inhibit-modification-hooks' is bound to t across `vui-mount' to
suppress the widget machinery's after-change handlers — the launcher
embeds `vui-field' / `vui-integer-field' / etc., which install
buffer-local widget overlays whose `after-change-functions' would
fire against an `erase-buffer' inside `vui-mount' and look up
already-invalidated field markers (`wrong-type-argument
`number-or-marker-p' nil')."
  (with-current-buffer buffer
    (unless (eq major-mode 'beads-ralph-launcher-mode)
      (beads-ralph-launcher-mode))
    (let ((inhibit-modification-hooks t)
          (dirty-p
           (not (beads-ralph-launcher--params-equal-p
                 beads-ralph-launcher--params
                 beads-ralph-launcher--baseline-params)))
          (template-loaded-p
           (and beads-ralph-launcher--root-id
                (assoc beads-ralph-launcher--root-id
                       beads-agent-ralph-launch-templates)
                t)))
      (vui-mount
       (vui-component 'beads-ralph-launcher--root
                      :root-id beads-ralph-launcher--root-id
                      :kind beads-ralph-launcher--kind
                      :issue beads-ralph-launcher--issue
                      :child-issues beads-ralph-launcher--children
                      :params beads-ralph-launcher--params
                      :prompt-override beads-ralph-launcher--prompt-override
                      :shell-expanded beads-ralph-launcher--shell-expanded
                      :incumbent (beads-ralph-launcher--incumbent
                                  beads-ralph-launcher--root-id)
                      :cap-reached-p (beads-agent-ralph-cap-reached-p)
                      :dirty-p dirty-p
                      :template-loaded-p template-loaded-p)
       (buffer-name buffer)))))

(defun beads-ralph-launcher--set-param (key value)
  "Set buffer-local `--params' KEY to VALUE; remount."
  (setq-local beads-ralph-launcher--params
              (plist-put (copy-sequence beads-ralph-launcher--params)
                         key value))
  (beads-ralph-launcher--render (current-buffer)))

(defun beads-ralph-launcher--set-shell-expanded (value)
  "Set the shell-preview toggle to VALUE (coerced to boolean); remount."
  (setq-local beads-ralph-launcher--shell-expanded (not (null value)))
  (beads-ralph-launcher--render (current-buffer)))

;;; Interactive commands

(defun beads-ralph-launcher--ensure-mode ()
  "Refuse interactive commands invoked outside a launcher buffer."
  (unless (eq major-mode 'beads-ralph-launcher-mode)
    (user-error "Not in a Ralph launcher buffer")))

(defun beads-ralph-launcher-launch ()
  "Launch the loop with the current launcher params.
Refuses with `user-error' when an incumbent loop is live or when the
concurrent-loops cap is reached."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (when (beads-ralph-launcher--incumbent beads-ralph-launcher--root-id)
    (user-error
     "Already running for %s; use [F]orce relaunch or [S]witch"
     beads-ralph-launcher--root-id))
  (when (beads-agent-ralph-cap-reached-p)
    (user-error
     "Max concurrent Ralph loops (%d) reached"
     beads-agent-ralph-max-concurrent-loops))
  (let* ((root-id beads-ralph-launcher--root-id)
         (kind beads-ralph-launcher--kind)
         (start-args (beads-ralph-launcher--params-to-start-args
                      root-id kind
                      beads-ralph-launcher--params
                      beads-ralph-launcher--prompt-override))
         (buf (current-buffer))
         (result (apply #'beads-agent-ralph-start start-args)))
    (when (buffer-live-p buf)
      (let ((win (get-buffer-window buf)))
        (if win (quit-window nil win) (bury-buffer buf))))
    ;; `-start' returns (CONTROLLER . DASHBOARD-BUFFER); pop the
    ;; dashboard so the per-loop view becomes the user's focus.
    (when (and (consp result) (buffer-live-p (cdr result)))
      (pop-to-buffer (cdr result)))
    result))

(defun beads-ralph-launcher-edit-prompt ()
  "Pop the prompt editor; on save, update `--prompt-override' and remount."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (let* ((root-id beads-ralph-launcher--root-id)
         (kind beads-ralph-launcher--kind)
         (override beads-ralph-launcher--prompt-override)
         (current
          (or override
              (beads-agent-ralph--preview-iter1-prompt root-id kind nil)))
         (launcher-buf (current-buffer)))
    (beads-agent-prompt-edit-show
     root-id
     nil
     current
     "Ralph"
     ;; Callback dispatch (the prompt-edit contract):
     ;;   - Cancel:  SYS=nil AND USER=nil — leave override untouched.
     ;;   - Confirm with non-blank USER: set override.
     ;;   - Confirm with blank USER: clear override (revert to
     ;;     template).  USER is `""' here, not nil — confirm always
     ;;     passes a string for USER even when the region is empty.
     (lambda (sys user)
       (when (buffer-live-p launcher-buf)
         (with-current-buffer launcher-buf
           (cond
            ((and (null sys) (null user))
             nil)
            ((and (stringp user)
                  (not (string-empty-p (string-trim user))))
             (setq-local beads-ralph-launcher--prompt-override user)
             (beads-ralph-launcher--render launcher-buf))
            ((stringp user)
             (setq-local beads-ralph-launcher--prompt-override nil)
             (beads-ralph-launcher--render launcher-buf))))))) ))

(defun beads-ralph-launcher-save-template ()
  "Save the current params as the per-root template; reset baseline."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (let ((root-id beads-ralph-launcher--root-id)
        (params beads-ralph-launcher--params))
    (beads-ralph-launcher--save-template root-id params)
    (setq-local beads-ralph-launcher--baseline-params
                (copy-sequence params))
    (message "Saved launch template for %s" root-id)))

(defun beads-ralph-launcher-quit ()
  "Bury the launcher; confirm when params differ from the baseline."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (if (beads-ralph-launcher--params-equal-p
       beads-ralph-launcher--params
       beads-ralph-launcher--baseline-params)
      (quit-window)
    (when (yes-or-no-p
           (format "Discard unsaved edits to launch params for %s? "
                   beads-ralph-launcher--root-id))
      (quit-window))))

(defun beads-ralph-launcher-refresh ()
  "Force a re-render of the launcher."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (beads-ralph-launcher--render (current-buffer)))

(defun beads-ralph-launcher-help ()
  "Echo the launcher's keymap legend.
Mirrors the action-bar legend so `?' and the buffer footer agree."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (message
   (concat
    "Launcher keys: [L]aunch [E]dit-prompt [S]ave-template "
    "[F]orce-relaunch [g]refresh [?]help [q]uit · "
    "TAB / S-TAB to navigate rows")))

(defun beads-ralph-launcher-switch-to-incumbent ()
  "Switch to the dashboard of the loop already running for this root."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (let* ((root-id beads-ralph-launcher--root-id)
         (ctrl (beads-ralph-launcher--incumbent root-id)))
    (unless ctrl
      (user-error "No live loop for %s" root-id))
    (let ((win (get-buffer-window (current-buffer))))
      (when win (quit-window nil win)))
    (beads-agent-ralph-dashboard-mount ctrl)))

(defun beads-ralph-launcher-force-relaunch ()
  "Stop the incumbent (if any) and launch with current params.
Delegates to `beads-agent-ralph-force-relaunch'; on stop-timeout the
launcher stays open with an error message."
  (interactive)
  (beads-ralph-launcher--ensure-mode)
  (let* ((root-id beads-ralph-launcher--root-id)
         (kind beads-ralph-launcher--kind)
         (start-args (beads-ralph-launcher--params-to-start-args
                      root-id kind
                      beads-ralph-launcher--params
                      beads-ralph-launcher--prompt-override))
         (buf (current-buffer)))
    (condition-case err
        (let ((result (apply #'beads-agent-ralph-force-relaunch start-args)))
          (when (buffer-live-p buf)
            (let ((win (get-buffer-window buf)))
              (if win (quit-window nil win) (bury-buffer buf))))
          (when (and (consp result) (buffer-live-p (cdr result)))
            (pop-to-buffer (cdr result)))
          result)
      (user-error
       (message "beads-ralph-launcher: %s" (error-message-string err))
       nil))))

;;; Mode + keymap

(defvar beads-ralph-launcher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "L") #'beads-ralph-launcher-launch)
    (define-key map (kbd "E") #'beads-ralph-launcher-edit-prompt)
    (define-key map (kbd "S") #'beads-ralph-launcher-save-template)
    (define-key map (kbd "F") #'beads-ralph-launcher-force-relaunch)
    (define-key map (kbd "C") #'beads-ralph-launcher-quit)
    (define-key map (kbd "g") #'beads-ralph-launcher-refresh)
    (define-key map (kbd "?") #'beads-ralph-launcher-help)
    (define-key map (kbd "q") #'beads-ralph-launcher-quit)
    map)
  "Keymap for `beads-ralph-launcher-mode'.
Mode-level keys mirror the action-bar buttons so users do not need to
TAB onto a button to confirm.  `RET' is deliberately not bound at the
mode level — only the Launch button's `:on-click' spawns a loop, so
`RET' on any other widget cannot trigger a launch by accident.")

(define-derived-mode beads-ralph-launcher-mode vui-mode
  "Beads-Launcher"
  "Major mode for the Ralph launcher panel.

\\{beads-ralph-launcher-mode-map}"
  (setq truncate-lines nil))

;;; Entry point

;;;###autoload
(defun beads-ralph-launcher (root-id &rest args)
  "Open the singleton Ralph launcher for ROOT-ID.
ARGS is a plist; recognised keys:

  :kind          `issue' (default) or `epic'.
  :issue         A `beads-issue' object whose title / priority /
                 status populate the header (optional).
  :children      List of child `beads-issue' objects (epic case)
                 shown read-only in the ready-children block.

The launcher buffer is the singleton `beads-ralph-launcher-buffer-name'.
Re-opening with a DIFFERENT ROOT-ID re-seeds buffer-local state
\(params, prompt-override, shell-expanded toggle) from the new root's
saved template / defaults.  Re-opening with the SAME ROOT-ID preserves
in-flight edits and only refreshes the optional `:issue' / `:children'
metadata.  An empty or non-string ROOT-ID raises `user-error'.

When called interactively, the root id is resolved via
`beads-read-issue-id', which prefers buffer context (list mode,
show mode, Ralph epic browser, beads-section, the `*beads-show…*'
buffer-name) and only prompts with completion as a fallback —
matching `beads-ralph-launch-at-point' so both entry points behave
identically when invoked from a beads buffer."
  (interactive
   (list (beads-read-issue-id)))
  (unless (and (stringp root-id) (not (string-empty-p root-id)))
    (user-error "beads-ralph-launcher: ROOT-ID must be a non-empty string"))
  (let* ((buf (get-buffer-create beads-ralph-launcher-buffer-name))
         (same-root-p
          (and (buffer-live-p buf)
               (with-current-buffer buf
                 (and (eq major-mode 'beads-ralph-launcher-mode)
                      (equal root-id
                             beads-ralph-launcher--root-id))))))
    (with-current-buffer buf
      (unless (eq major-mode 'beads-ralph-launcher-mode)
        (beads-ralph-launcher-mode))
      ;; Re-opening on the same root-id preserves any in-flight
      ;; param edits / prompt-override / shell-expanded toggle.  Only
      ;; the optional :issue / :children metadata is refreshed
      ;; (caller may have a more up-to-date issue object now).
      (setq-local beads-ralph-launcher--issue (plist-get args :issue))
      (setq-local beads-ralph-launcher--children (plist-get args :children))
      (unless same-root-p
        (setq-local beads-ralph-launcher--root-id root-id)
        (setq-local beads-ralph-launcher--kind
                    (or (plist-get args :kind) 'issue))
        (let ((params (beads-ralph-launcher--params-for root-id)))
          (setq-local beads-ralph-launcher--params (copy-sequence params))
          (setq-local beads-ralph-launcher--baseline-params
                      (copy-sequence params)))
        (setq-local beads-ralph-launcher--prompt-override nil)
        (setq-local beads-ralph-launcher--shell-expanded nil))
      (beads-ralph-launcher--render buf))
    (pop-to-buffer buf)
    buf))

;;; Context-sensitive launch entry point

;;;###autoload
(defun beads-ralph-launch-at-point ()
  "Open the Ralph launcher for the issue or epic at point.

Resolution order:

  1. Detect an id from the current buffer context via
     `beads-read-issue-id' (which inspects `beads-list-mode',
     `beads-show-mode', the Ralph epic browser, beads-section
     buffers, and the `*beads-show…*' buffer-name format).
  2. If no context, prompt with completion.
  3. Fetch the issue via `bd show --json' to read its
     `issue_type'.  When the type is the literal string \"epic\",
     the launcher opens with kind set to `epic'; otherwise to
     `issue'.

The resolved `beads-issue' object is forwarded to the launcher so
its header populates without a second round trip to bd."
  (interactive)
  (let ((id (beads-read-issue-id)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (user-error "beads-ralph-launch-at-point: no issue id detected"))
    ;; `beads-issue-read' normally signals on a missing issue (because
    ;; `bd show' exits non-zero and `beads-execute' surfaces the
    ;; failure), but if the CLI ever returns an empty result set the
    ;; helper degrades to nil.  Refuse that explicitly rather than
    ;; letting `oref' fail with an opaque "wrong-type-argument".
    (let ((issue (beads-issue-read id)))
      (unless issue
        (user-error "beads-ralph-launch-at-point: no such issue %S" id))
      (let ((kind (if (equal (oref issue issue-type) "epic")
                      'epic
                    'issue)))
        (beads-ralph-launcher id :kind kind :issue issue)))))

(provide 'beads-ralph-launcher)

;;; beads-ralph-launcher.el ends here
