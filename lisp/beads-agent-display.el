;;; beads-agent-display.el --- Display helpers for agent sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the central display helper used by every UI
;; surface that renders a single AI agent session (issue list, dashboard,
;; agent list, show buffer, mode-line, transient/prompt-edit headers).
;;
;; The helper, `beads-agent-display-format-session', resolves the role
;; icon (or single letter under TTY) via `beads-agent-type-icon-or-letter'
;; and composes it with outcome marks (`✓'/`✗') and the appropriate face
;; so callers do not need to know the rendering details.
;;
;; Rendering matrix:
;;
;;   State     GUI render   TTY render   Face
;;   --------  -----------  -----------  ----------------------------
;;   running   🦅           T            beads-list-agent-working
;;   touched   dim 🦅       dim T        shadow
;;   stopped   dim 🦅       dim T        shadow
;;   finished  ✓🦅          ✓T           beads-list-agent-finished
;;   failed    ✗🦅          ✗T           beads-list-agent-failed
;;
;; The outcome marks `✓' (U+2713) and `✗' (U+2717) are single-cell
;; ASCII-adjacent glyphs that render in TTY too, so the
;; shape-disambiguated status survives the GUI fallback.

;;; Code:

(require 'cl-lib)
(require 'beads-agent-type)

;; Session accessors live in beads-agent-backend.el; callers load that
;; module before this helper runs.  Declared here to keep
;; `beads-agent-display.el' load-cheap and avoid pulling sesman into
;; the dependency graph of pure display tests.
(declare-function beads-agent-session-type-name
                  "beads-agent-backend" (session))
(declare-function beads-agent-session-instance-number
                  "beads-agent-backend" (session))
(declare-function beads-agent--get-sessions-focused-on-issue
                  "beads-agent-backend" (issue-id))
(declare-function beads-agent--get-sessions-touching-issue
                  "beads-agent-backend" (issue-id))
(declare-function beads-agent--get-sessions-for-issue
                  "beads-agent-backend" (issue-id))
(declare-function beads-agent--get-issue-outcome
                  "beads-agent-backend" (issue-id))

;; Faces are defined in `beads-command-list.el'; we reference them by
;; symbol here without requiring their definition module.
(defvar beads-list-agent-working)
(defvar beads-list-agent-finished)
(defvar beads-list-agent-failed)

(defconst beads-agent-display--outcome-mark-finished "✓"
  "Outcome prefix for finished agent sessions (U+2713).
Single-cell, ASCII-adjacent — renders in TTY too.")

(defconst beads-agent-display--outcome-mark-failed "✗"
  "Outcome prefix for failed agent sessions (U+2717).
Single-cell, ASCII-adjacent — renders in TTY too.")

(defun beads-agent-display--state-face (state)
  "Return the face symbol to apply for STATE."
  (pcase state
    ((or 'touched 'stopped) 'shadow)
    ('finished 'beads-list-agent-finished)
    ('failed 'beads-list-agent-failed)
    (_ 'beads-list-agent-working)))

(defun beads-agent-display--state-prefix (state)
  "Return the outcome prefix string for STATE, or empty when none."
  (pcase state
    ('finished beads-agent-display--outcome-mark-finished)
    ('failed beads-agent-display--outcome-mark-failed)
    (_ "")))

(defun beads-agent-display--state-words (state)
  "Return the human-readable description for STATE."
  (pcase state
    ('touched "touched but focused elsewhere")
    ('stopped "stopped")
    ('finished "finished")
    ('failed "failed")
    (_ "focused")))

(defun beads-agent-display--glyph (type-name)
  "Return the visible identifier glyph for the agent type named TYPE-NAME.
Looks up the registered `beads-agent-type' and delegates to
`beads-agent-type-icon-or-letter'.  Falls back to the first
character of TYPE-NAME when no type is registered, and to `●'
when TYPE-NAME is nil or empty."
  (let ((type (and type-name (beads-agent-type-get type-name))))
    (cond
     (type (beads-agent-type-icon-or-letter type))
     ((and (stringp type-name) (> (length type-name) 0))
      (substring type-name 0 1))
     (t "●"))))

(defun beads-agent-display--format (type-name instance-n outcome brief)
  "Build the propertized identifier string from raw type info.
TYPE-NAME is the agent type name (e.g. \"Task\") or nil.
INSTANCE-N is the session instance number or nil.
OUTCOME is the state symbol (`running'/`touched'/`stopped'/
`finished'/`failed').
BRIEF when non-nil forces the `#N' suffix off."
  (let* ((state (or outcome 'running))
         (glyph (beads-agent-display--glyph type-name))
         (prefix (beads-agent-display--state-prefix state))
         (face (beads-agent-display--state-face state))
         (show-instance (and (not brief)
                             beads-agent-display-show-instance
                             (integerp instance-n)))
         (suffix (if show-instance (format "#%d" instance-n) ""))
         (body (concat prefix glyph suffix))
         (state-words (beads-agent-display--state-words state))
         (echo-instance (and (integerp instance-n)
                             (format " #%d" instance-n)))
         (help-echo (format "%s agent%s: %s"
                            (or type-name "Unknown")
                            (or echo-instance "")
                            state-words)))
    (propertize body 'face face 'help-echo help-echo)))

;;;###autoload
(defun beads-agent-display-format-session (session &optional outcome brief)
  "Format SESSION's identifier as icon (or letter) with optional outcome mark.

SESSION is a `beads-agent-session' object.

OUTCOME indicates the session's state relative to the rendering surface:
  - nil or `running': active and focused on the issue (default)
  - `touched'       : touched the issue but focused elsewhere
  - `stopped'       : session terminated without a recorded outcome
                      (rendered with `shadow' face, no prefix)
  - `finished'      : terminal success; prefixed with `✓'
  - `failed'        : terminal failure; prefixed with `✗'

When BRIEF is non-nil the `#N' instance suffix is forced off even
if `beads-agent-display-show-instance' is non-nil.  Otherwise the
`#N' suffix is appended iff `beads-agent-display-show-instance' is
non-nil AND SESSION has an integer `instance-number'.

The returned string carries two text properties:
  `face'      — the state's face (see the matrix in commentary)
  `help-echo' — \"<Type> agent[ #N]: <state-in-words>\""
  (beads-agent-display--format
   (beads-agent-session-type-name session)
   (beads-agent-session-instance-number session)
   outcome
   brief))

(defun beads-agent-display--outcome-parts (outcome)
  "Decompose OUTCOME into a (TYPE-NAME . STATE-SYM) pair.
OUTCOME is the value returned by `beads-agent--get-issue-outcome'.
It may be:
  - nil                          : no outcome on record
  - a bare symbol (`finished'/`failed')
  - a cons cell `(TYPE-NAME . OUTCOME-SYM)' from the typed storage

Returns a cons cell `(TYPE-NAME . STATE-SYM)' where TYPE-NAME may be
nil (legacy `bare-symbol' form) and STATE-SYM is `finished' or `failed'.
Returns nil when OUTCOME does not resolve to a recognised state, so
callers can skip outcome rendering entirely."
  (let* ((state (cond ((symbolp outcome) outcome)
                      ((consp outcome) (cdr outcome))))
         (type-name (and (consp outcome) (car outcome))))
    (and (memq state '(finished failed))
         (cons type-name state))))

;;;###autoload
(defun beads-agent-display-format-issue-agents (issue-id)
  "Return the agent-badge string for ISSUE-ID, or empty string.

Renders one role glyph per focused session on the issue, joined by a
faint space separator.  Under GUI Emacs the glyph is the role icon,
under TTY it is the type's single letter.  Finished and failed
outcomes prefix the glyph with `✓' or `✗' so the status stays
shape-distinguishable in TTY too.

Behaviour note: touched-only sessions (sessions that touched ISSUE-ID
but are focused elsewhere) are intentionally omitted from the visible
badges to keep the cell legible — their count surfaces only in the
cell `help-echo'.  This is a deliberate change from the previous
inline tilde-prefixed indicator (e.g. `~P'); see the design doc under
`.designs/agent-icon-display/' for rationale.

Returns \"\" when there are no focused sessions and no outcome.
Functions in `beads-agent-backend' are guarded via `fboundp' so the
display module stays load-cheap and usable from contexts where the
backend has not been required."
  (let* ((focused (and (fboundp 'beads-agent--get-sessions-focused-on-issue)
                       (beads-agent--get-sessions-focused-on-issue issue-id)))
         (touched (and (fboundp 'beads-agent--get-sessions-touching-issue)
                       (beads-agent--get-sessions-touching-issue issue-id)))
         (touched-only (cl-set-difference touched focused))
         (legacy-sessions (and (not focused) (not touched)
                               (fboundp 'beads-agent--get-sessions-for-issue)
                               (beads-agent--get-sessions-for-issue issue-id)))
         (outcome (and (fboundp 'beads-agent--get-issue-outcome)
                       (beads-agent--get-issue-outcome issue-id)))
         (outcome-parts (beads-agent-display--outcome-parts outcome))
         (separator (propertize " " 'face 'shadow)))
    (cond
     (focused
      ;; NOTE: `propertize' below applies one `help-echo' across the whole
      ;; joined badge string, intentionally OVERWRITING the per-session
      ;; tooltips that `beads-agent-display-format-session' set on each
      ;; glyph.  The trade-off is deliberate: in a column-sized cell the
      ;; user cannot reliably hover a single glyph (mouse positioning is
      ;; ambiguous with adjacent emoji), so a single aggregate summary
      ;; ("Focused: N, Touched: M") is more useful than per-glyph
      ;; tooltips that are effectively unreachable.  Surfaces that need
      ;; per-glyph tooltips (agent list, show buffer) call
      ;; `beads-agent-display-format-session' directly without this
      ;; wrapper.
      (let* ((indicators
              (mapcar (lambda (session)
                        (beads-agent-display-format-session session nil t))
                      focused))
             (body (mapconcat #'identity indicators separator)))
        (propertize body
                    'help-echo (format "Focused: %d, Touched: %d"
                                       (length focused)
                                       (length touched-only)))))
     (legacy-sessions
      ;; Same per-glyph vs aggregate help-echo trade-off as the focused
      ;; branch above.
      (let* ((indicators
              (mapcar (lambda (session)
                        (beads-agent-display-format-session session nil t))
                      legacy-sessions))
             (body (mapconcat #'identity indicators separator)))
        (propertize body
                    'help-echo (format "%d agent%s working"
                                       (length legacy-sessions)
                                       (if (= (length legacy-sessions) 1) "" "s")))))
     (outcome-parts
      (beads-agent-display-format-type-name (car outcome-parts)
                                            (cdr outcome-parts)))
     (t ""))))

;;;###autoload
(defun beads-agent-display-format-type-name (type-name &optional outcome)
  "Format an identifier from TYPE-NAME alone, without a live session.
TYPE-NAME is a string naming the agent type (e.g. \"Task\") or nil.
OUTCOME is the state symbol — typically `finished' or `failed' — used
for outcome rendering (`✓🦅'/`✗🦅' in GUI, `✓T'/`✗T' in TTY).  Returns
the same shape of propertized string as `beads-agent-display-format-session'.

This entry point exists for surfaces that have a type identifier
but no live session (e.g. the per-issue outcome cell in the issue
list, where the session has already terminated)."
  (beads-agent-display--format type-name nil outcome t))

(provide 'beads-agent-display)

;;; beads-agent-display.el ends here
