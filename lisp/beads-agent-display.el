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
;;   running   👷           T            beads-list-agent-working
;;   touched   dim 👷       dim T        shadow
;;   finished  ✓👷          ✓T           beads-list-agent-finished
;;   failed    ✗👷          ✗T           beads-list-agent-failed
;;
;; The outcome marks `✓' (U+2713) and `✗' (U+2717) are single-cell
;; ASCII-adjacent glyphs that render in TTY too, so the
;; shape-disambiguated status survives the GUI fallback.

;;; Code:

(require 'beads-agent-type)

;; Session accessors live in beads-agent-backend.el; callers load that
;; module before this helper runs.  Declared here to keep
;; `beads-agent-display.el' load-cheap and avoid pulling sesman into
;; the dependency graph of pure display tests.
(declare-function beads-agent-session-type-name
                  "beads-agent-backend" (session))
(declare-function beads-agent-session-instance-number
                  "beads-agent-backend" (session))

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
    ('touched 'shadow)
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
OUTCOME is the state symbol (`running'/`touched'/`finished'/`failed').
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
         (echo-instance (and (memq state '(running touched))
                             (integerp instance-n)
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

;;;###autoload
(defun beads-agent-display-format-type-name (type-name &optional outcome)
  "Format an identifier from TYPE-NAME alone, without a live session.
TYPE-NAME is a string naming the agent type (e.g. \"Task\") or nil.
OUTCOME is the state symbol — typically `finished' or `failed' — used
for outcome rendering (`✓👷'/`✗👷' in GUI, `✓T'/`✗T' in TTY).  Returns
the same shape of propertized string as `beads-agent-display-format-session'.

This entry point exists for surfaces that have a type identifier
but no live session (e.g. the per-issue outcome cell in the issue
list, where the session has already terminated)."
  (beads-agent-display--format type-name nil outcome t))

(provide 'beads-agent-display)

;;; beads-agent-display.el ends here
