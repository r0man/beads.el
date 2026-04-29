;;; beads-command-compact.el --- Compact command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd admin compact'
;; operations AND the distinct top-level `bd compact' command.  The
;; admin-compact cluster manages semantic database compaction for old
;; closed issues; the top-level `bd compact' squashes Dolt commit
;; history older than N days.  Both live in this file because of
;; naming coherence -- they share the `compact' word.
;;
;; TWO DIFFERENT 'COMPACT' COMMANDS:
;;   - `bd admin compact' (4 mode classes below): semantic issue
;;     summarization for old closed issues (graceful decay).
;;   - `bd compact'       (`beads-command-compact' near the bottom):
;;     squashes Dolt commit history older than --days, preserving
;;     recent commits via cherry-pick.  This is a Dolt-storage
;;     operation, NOT semantic issue compaction.
;;
;; The user-facing transient `beads-compact' (transient-define-prefix
;; near the bottom) routes to the admin-compact mode menu.  The
;; top-level `bd compact' is exposed via `beads-compact-commits'
;; (manual transient name to avoid colliding with `beads-compact').
;;
;; INTENTIONAL DESIGN: 5-class cluster targeting `admin.compact'
;; ===============================================================
;;
;; `bd admin compact' has mutually-exclusive operating modes selected
;; by --analyze / --apply / --auto / --dolt / --stats flags.  Rather
;; than expose a single class with a `--mode' selector, this file (and
;; `beads-command-admin.el' for the parent) ships ONE class per mode so
;; the transient UX surfaces only the slots that are relevant to the
;; chosen mode.  All five classes set `:cli-command "admin compact"',
;; which is intentional collision; cli-audit reports flag this as a
;; collision but it is not drift.
;;
;; The cluster:
;;   - beads-command-compact-stats     (--stats)    : statistics display
;;   - beads-command-compact-analyze   (--analyze)  : export candidates
;;   - beads-command-compact-apply     (--apply)    : accept summary
;;   - beads-command-compact-auto      (--auto)     : AI-powered (legacy)
;;   - beads-command-admin-compact     (--dolt etc) : full/dolt-mode form
;;
;; Each class carries ONLY the flags from `bd admin compact --help'
;; that are meaningful in its mode; it inherits global flags from
;; `beads-command-global-options'.  Sibling-only slots intentionally
;; do not appear and should not be added to keep mode-specific UX
;; clean.  Future audits should treat the cluster as ONE unit.
;;
;; Usage:
;;   (beads-execute 'beads-command-compact-stats)                    ; Show stats
;;   (beads-execute 'beads-command-compact-analyze :tier 1 :json t) ; Get candidates
;;   (beads-execute 'beads-command-compact-apply :id "bd-1" :summary "Summary text")

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads-util")
(declare-function beads-check-executable "beads-util")
(defvar beads-auto-refresh)

;;; ============================================================
;;; Command Class: beads-command-compact-stats
;;; ============================================================

(beads-defcommand beads-command-compact-stats (beads-command-global-options)
  ()
  :transient :manual
  :documentation "Represents bd admin compact --stats command.
Shows compaction statistics including tier 1 and tier 2 candidates."
  :cli-command "admin compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-stats))
  "Build command line for stats _COMMAND."
  (append (cl-call-next-method)
          '("--stats")))

;;; ============================================================
;;; Command Class: beads-command-compact-analyze
;;; ============================================================

;;;###autoload (autoload 'beads-compact-analyze "beads-command-compact" nil t)
(beads-defcommand beads-command-compact-analyze (beads-command-global-options)
  ((tier
    :type (or null string integer)
    :short-option "t"
    :prompt "Tier (1 or 2): "
    :choices '("1" "2")
    :group "Analyze Options"
    :level 1
    :order 1)
   (limit
    :type (or null string integer)
    :short-option "l"
    :prompt "Limit (0 = no limit): "
    :group "Analyze Options"
    :level 1
    :order 2))
  :documentation "Analyze and export compaction candidates.

Exports candidates for agent review in JSON format.  Use with
--json flag for structured output.  Specify --tier for tier 1
or tier 2 candidates."
  :cli-command "admin compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-analyze))
  "Build command line for analyze _COMMAND."
  (append (cl-call-next-method)
          '("--analyze")))

;;; ============================================================
;;; Command Class: beads-command-compact-apply
;;; ============================================================

;;;###autoload (autoload 'beads-compact-apply "beads-command-compact" nil t)
(beads-defcommand beads-command-compact-apply (beads-command-global-options)
  ((issue-id
    :long-option "id"
    :type (or null string)
    :short-option "i"
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Apply Options"
    :level 1
    :order 1
    :required t)
   (summary
    :type (or null string)
    :short-option "s"
    :prompt "Summary file path: "
    :group "Apply Options"
    :level 1
    :order 2
    :required t)
   (force
    :type boolean
    :short-option "f"
    :group "Apply Options"
    :level 2
    :order 3))
  :documentation "Apply agent-provided summary to compact an issue.

Accepts a summary file for the specified issue.  Use '-' as the
summary path to read from stdin."
  :cli-command "admin compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-apply))
  "Build command line for apply _COMMAND."
  (append (cl-call-next-method)
          '("--apply")))

;; Validate override removed: the base method checks :required slots
;; automatically via beads-command-validate-slots.

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-compact-apply))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (beads--invalidate-completion-cache)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-compact-auto
;;; ============================================================

;;;###autoload (autoload 'beads-compact-auto "beads-command-compact" nil t)
(beads-defcommand beads-command-compact-auto (beads-command-global-options)
  ((issue-id
    :long-option "id"
    :type (or null string)
    :short-option "i"
    :prompt "Issue ID (or leave empty for all): "
    :reader beads-reader-issue-id
    :group "Auto Options"
    :level 1
    :order 1)
   (all
    :type boolean
    :short-option "a"
    :group "Auto Options"
    :level 1
    :order 2)
   (tier
    :type (or null string integer)
    :short-option "t"
    :prompt "Tier (1 or 2): "
    :choices '("1" "2")
    :group "Auto Options"
    :level 1
    :order 3)
   (batch-size
    :type (or null string integer)
    :short-option "b"
    :prompt "Batch size: "
    :group "Auto Options"
    :level 3
    :order 4)
   (workers
    :type (or null string integer)
    :short-option "w"
    :group "Auto Options"
    :level 3
    :order 5)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Auto Options"
    :level 1
    :order 6)
   (force
    :type boolean
    :short-option "f"
    :group "Auto Options"
    :level 3
    :order 7))
  :documentation "AI-powered automatic compaction (legacy).

Requires ANTHROPIC_API_KEY environment variable.  Use --dry-run
to preview candidates before compacting.  Specify --id for a
single issue or --all for all eligible candidates."
  :cli-command "admin compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-auto))
  "Build command line for auto _COMMAND."
  (append (cl-call-next-method)
          '("--auto")))

(cl-defmethod beads-command-validate ((command beads-command-compact-auto))
  "Validate auto COMMAND."
  (with-slots (issue-id all force) command
    (cond
     ;; --force requires --id
     ((and force (not issue-id))
      "--force requires --id to be specified")
     ;; Either --id or --all should be specified
     ((and (not issue-id) (not all))
      "Specify --id for a single issue or --all for all candidates")
     (t nil))))

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-compact-auto))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (beads--invalidate-completion-cache)
  (cl-call-next-method))

;;; ============================================================
;;; Transient Menus
;;; ============================================================

;;;###autoload (autoload 'beads-compact-show-stats "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-stats "beads-compact-show-stats"
  "Show compaction statistics.

Displays counts of tier 1 and tier 2 compaction candidates,
tombstone information, and other database statistics."
  beads-option-global-section)

;; Note: Transient menus `beads-compact-analyze', `beads-compact-apply',
;; and `beads-compact-auto' are auto-generated by their respective
;; `beads-defcommand' above (default :transient t behavior).

;;; ============================================================
;;; Parent Transient Menu
;;; ============================================================

;;;###autoload (autoload 'beads-compact "beads-command-compact" nil t)
(transient-define-prefix beads-compact ()
  "Compact old closed issues using semantic summarization.

Compaction reduces database size by summarizing closed issues that
are no longer actively referenced.  This is permanent graceful decay.

Modes:
  Analyze: Export candidates for agent review
  Apply: Accept agent-provided summary
  Auto: AI-powered compaction (legacy, requires API key)

Tiers:
  Tier 1: Semantic compression (30 days closed, 70% reduction)
  Tier 2: Ultra compression (90 days closed, 95% reduction)"
  ["Compact Commands"
   ("s" "Show statistics" beads-compact-show-stats)
   ("z" "Analyze candidates" beads-compact-analyze)
   ("a" "Apply summary" beads-compact-apply)
   ("A" "Auto compact (legacy)" beads-compact-auto)
   ("D" "Compact Dolt commits" beads-compact-commits)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

;;; ============================================================
;;; Command Class: beads-command-compact (top-level bd compact)
;;; ============================================================
;;
;; This class targets the TOP-LEVEL `bd compact' command (Dolt commit
;; squash), which is *distinct* from the `bd admin compact' cluster
;; above.  Confusing names, distinct semantics:
;;
;;   - `bd compact'        squashes Dolt commit history (storage)
;;   - `bd admin compact'  semantic issue summarization (data decay)
;;
;; For semantic compaction, use the `beads-command-compact-*'
;; siblings.  For full history squash, use `beads-flatten'.
;;
;; The auto-generated transient name `beads-compact' is already taken
;; by the admin-compact mode menu above, so this class uses
;; :transient :manual and a manually-named transient
;; `beads-compact-commits'.

;;;###autoload (autoload 'beads-compact-commits "beads-command-compact" nil t)
(beads-defcommand beads-command-compact (beads-command-global-options)
  ((days
    :type (or null integer string)
    :prompt "Keep commits newer than (days, default 30): "
    :group "Options"
    :level 1
    :order 1
    :documentation "Keep commits newer than N days (default 30)")
   (dry-run
    :type boolean
    :group "Options"
    :level 1
    :order 2
    :documentation "Preview without making changes")
   (force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 1
    :order 3
    :documentation "Confirm commit squash"))
  :documentation "Represents top-level bd compact command.
Squashes Dolt commits older than N days into a single commit,
preserving recent commits via cherry-pick.  Distinct from
`bd admin compact' (semantic issue summarization)."
  :transient :manual
  :cli-command "compact")

;;;###autoload (autoload 'beads-compact-commits "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact "beads-compact-commits"
  "Squash Dolt commits older than N days into a single commit.

Recent commits (within the retention window) are preserved via
cherry-pick.  This reduces Dolt storage overhead from auto-commit
history while keeping recent change tracking intact.

For semantic issue compaction (summarizing closed issues), use
`beads-compact' (the admin-compact menu).  For full history squash,
use `beads-flatten'."
  beads-option-global-section)

(provide 'beads-command-compact)
;;; beads-command-compact.el ends here
