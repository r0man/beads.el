;;; beads-command-compact.el --- Compact command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd compact' operations.
;; The compact command manages database compaction for old closed issues.
;;
;; Compact has multiple modes:
;; - Prune: Remove expired tombstones from issues.jsonl
;; - Analyze: Export candidates for agent review (JSON output)
;; - Apply: Accept agent-provided summary
;; - Auto: AI-powered compaction (legacy, requires ANTHROPIC_API_KEY)
;; - Stats: Show compaction statistics
;;
;; Usage:
;;   (beads-command-compact-stats!)                    ; Show stats
;;   (beads-command-compact-analyze! :tier 1 :json t) ; Get candidates
;;   (beads-command-compact-apply! :id "bd-1" :summary "Summary text")

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads-check-executable "beads")
(defvar beads-auto-refresh)

;;; ============================================================
;;; Command Class: beads-command-compact-stats
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-compact-stats (beads-command-json)
    ()
    :documentation "Represents bd compact --stats command.
  Shows compaction statistics including tier 1 and tier 2 candidates."))

(cl-defmethod beads-command-subcommand ((_command beads-command-compact-stats))
  "Return \"compact\" as the CLI subcommand."
  "compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-stats))
  "Build command line for stats _COMMAND."
  (append (cl-call-next-method)
          '("--stats")))

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-compact-stats))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-compact-prune
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-compact-prune (beads-command-json)
    ((older-than
      :initarg :older-than
      :type (or null string)
      :initform nil
      :documentation "Prune tombstones older than N days (default: 30)."
      :long-option "older-than"
      :option-type :integer
      :key "o"
      :transient "--older-than"
      :class transient-option
      :argument "--older-than="
      :prompt "Days old (default 30): "
      :transient-group "Prune Options"
      :level 1
      :order 1)
     (dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Preview without making changes."
      :long-option "dry-run"
      :option-type :boolean
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Prune Options"
      :level 1
      :order 2))
    :documentation "Represents bd compact --prune command.
  Removes expired tombstones from issues.jsonl."))

(cl-defmethod beads-command-subcommand ((_command beads-command-compact-prune))
  "Return \"compact\" as the CLI subcommand."
  "compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-prune))
  "Build command line for prune _COMMAND."
  (append (cl-call-next-method)
          '("--prune")))

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-compact-prune))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-compact-purge
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-compact-purge (beads-command-json)
    ((dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Preview without making changes."
      :long-option "dry-run"
      :option-type :boolean
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Purge Options"
      :level 1
      :order 1))
    :documentation "Represents bd compact --purge-tombstones command.
  Removes tombstones by dependency analysis (more aggressive than prune)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-compact-purge))
  "Return \"compact\" as the CLI subcommand."
  "compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-purge))
  "Build command line for purge _COMMAND."
  (append (cl-call-next-method)
          '("--purge-tombstones")))

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-compact-purge))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-compact-analyze
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-compact-analyze (beads-command-json)
    ((tier
      :initarg :tier
      :type (or null string)
      :initform nil
      :documentation "Compaction tier (1 or 2, default: 1)."
      :long-option "tier"
      :option-type :integer
      :key "t"
      :transient "--tier"
      :class transient-option
      :argument "--tier="
      :prompt "Tier (1 or 2): "
      :choices '("1" "2")
      :transient-group "Analyze Options"
      :level 1
      :order 1)
     (limit
      :initarg :limit
      :type (or null string)
      :initform nil
      :documentation "Limit number of candidates (0 = no limit)."
      :long-option "limit"
      :option-type :integer
      :key "l"
      :transient "--limit"
      :class transient-option
      :argument "--limit="
      :prompt "Limit (0 = no limit): "
      :transient-group "Analyze Options"
      :level 1
      :order 2))
    :documentation "Represents bd compact --analyze command.
  Exports compaction candidates for agent review."))

(cl-defmethod beads-command-subcommand ((_command beads-command-compact-analyze))
  "Return \"compact\" as the CLI subcommand."
  "compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-analyze))
  "Build command line for analyze _COMMAND."
  (append (cl-call-next-method)
          '("--analyze")))

;;; ============================================================
;;; Command Class: beads-command-compact-apply
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-compact-apply (beads-command-json)
    ((issue-id
      :initarg :issue-id
      :type (or null string)
      :initform nil
      :documentation "Issue ID to compact."
      :long-option "id"
      :option-type :string
      :key "i"
      :transient "--id (required)"
      :class transient-option
      :argument "--id="
      :prompt "Issue ID: "
      :transient-group "Apply Options"
      :level 1
      :order 1
      :required t)
     (summary
      :initarg :summary
      :type (or null string)
      :initform nil
      :documentation "Path to summary file (use '-' for stdin)."
      :long-option "summary"
      :option-type :string
      :key "s"
      :transient "--summary (required)"
      :class transient-option
      :argument "--summary="
      :prompt "Summary file path: "
      :transient-group "Apply Options"
      :level 1
      :order 2
      :required t))
    :documentation "Represents bd compact --apply command.
  Accepts agent-provided summary for an issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-compact-apply))
  "Return \"compact\" as the CLI subcommand."
  "compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-apply))
  "Build command line for apply _COMMAND."
  (append (cl-call-next-method)
          '("--apply")))

(cl-defmethod beads-command-validate ((command beads-command-compact-apply))
  "Validate apply COMMAND.  Requires issue-id and summary."
  (with-slots (issue-id summary) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((string-empty-p issue-id) "Issue ID cannot be empty")
     ((not summary) "Summary file path is required")
     ((string-empty-p summary) "Summary file path cannot be empty")
     (t nil))))

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-compact-apply))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (beads--invalidate-completion-cache)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-compact-auto
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-compact-auto (beads-command-json)
    ((issue-id
      :initarg :issue-id
      :type (or null string)
      :initform nil
      :documentation "Compact specific issue."
      :long-option "id"
      :option-type :string
      :key "i"
      :transient "--id"
      :class transient-option
      :argument "--id="
      :prompt "Issue ID (or leave empty for all): "
      :transient-group "Auto Options"
      :level 1
      :order 1)
     (all
      :initarg :all
      :type boolean
      :initform nil
      :documentation "Process all candidates."
      :long-option "all"
      :option-type :boolean
      :key "a"
      :transient "--all"
      :class transient-switch
      :argument "--all"
      :transient-group "Auto Options"
      :level 1
      :order 2)
     (tier
      :initarg :tier
      :type (or null string)
      :initform nil
      :documentation "Compaction tier (1 or 2, default: 1)."
      :long-option "tier"
      :option-type :integer
      :key "t"
      :transient "--tier"
      :class transient-option
      :argument "--tier="
      :prompt "Tier (1 or 2): "
      :choices '("1" "2")
      :transient-group "Auto Options"
      :level 1
      :order 3)
     (batch-size
      :initarg :batch-size
      :type (or null string)
      :initform nil
      :documentation "Issues per batch (default: 10)."
      :long-option "batch-size"
      :option-type :integer
      :key "b"
      :transient "--batch-size"
      :class transient-option
      :argument "--batch-size="
      :prompt "Batch size: "
      :transient-group "Auto Options"
      :level 3
      :order 4)
     (workers
      :initarg :workers
      :type (or null string)
      :initform nil
      :documentation "Parallel workers (default: 5)."
      :long-option "workers"
      :option-type :integer
      :key "w"
      :transient "--workers"
      :class transient-option
      :argument "--workers="
      :prompt "Workers: "
      :transient-group "Auto Options"
      :level 3
      :order 5)
     (dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Preview without compacting."
      :long-option "dry-run"
      :option-type :boolean
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Auto Options"
      :level 1
      :order 6)
     (force
      :initarg :force
      :type boolean
      :initform nil
      :documentation "Force compact (bypass checks, requires --id)."
      :long-option "force"
      :option-type :boolean
      :key "f"
      :transient "--force"
      :class transient-switch
      :argument "--force"
      :transient-group "Auto Options"
      :level 3
      :order 7))
    :documentation "Represents bd compact --auto command.
  AI-powered compaction (legacy, requires ANTHROPIC_API_KEY)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-compact-auto))
  "Return \"compact\" as the CLI subcommand."
  "compact")

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

;;;###autoload (autoload 'beads-compact-stats "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-stats "beads-compact-stats"
  "Show compaction statistics.

Displays counts of tier 1 and tier 2 compaction candidates,
tombstone information, and other database statistics."
  beads-option-global-section)

;;;###autoload (autoload 'beads-compact-prune "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-prune "beads-compact-prune"
  "Prune expired tombstones from issues.jsonl.

Age-based pruning removes tombstones older than N days (default 30).
Use --dry-run to preview what would be pruned."
  beads-option-global-section)

;;;###autoload (autoload 'beads-compact-purge "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-purge "beads-compact-purge"
  "Purge tombstones by dependency analysis.

More aggressive than prune - removes any tombstone that no open
issues depend on, regardless of age.  Also cleans stale deps
from closed issues to tombstones."
  beads-option-global-section)

;;;###autoload (autoload 'beads-compact-analyze "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-analyze "beads-compact-analyze"
  "Analyze and export compaction candidates.

Exports candidates for agent review in JSON format.  Use with
--json flag for structured output.  Specify --tier for tier 1
or tier 2 candidates."
  beads-option-global-section)

;;;###autoload (autoload 'beads-compact-apply "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-apply "beads-compact-apply"
  "Apply agent-provided summary to compact an issue.

Accepts a summary file for the specified issue.  Use '-' as the
summary path to read from stdin."
  beads-option-global-section)

;;;###autoload (autoload 'beads-compact-auto "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-auto "beads-compact-auto"
  "AI-powered automatic compaction (legacy).

Requires ANTHROPIC_API_KEY environment variable.  Use --dry-run
to preview candidates before compacting.  Specify --id for a
single issue or --all for all eligible candidates."
  beads-option-global-section)

;;; ============================================================
;;; Parent Transient Menu
;;; ============================================================

;;;###autoload (autoload 'beads-compact "beads-command-compact" nil t)
(transient-define-prefix beads-compact ()
  "Compact old closed issues using semantic summarization.

Compaction reduces database size by summarizing closed issues that
are no longer actively referenced.  This is permanent graceful decay.

Modes:
  Prune: Remove expired tombstones (age-based)
  Purge: Remove tombstones (dependency-based, more aggressive)
  Analyze: Export candidates for agent review
  Apply: Accept agent-provided summary
  Auto: AI-powered compaction (legacy, requires API key)

Tiers:
  Tier 1: Semantic compression (30 days closed, 70% reduction)
  Tier 2: Ultra compression (90 days closed, 95% reduction)"
  ["Compact Commands"
   ("s" "Show statistics" beads-compact-stats)
   ("p" "Prune tombstones (age-based)" beads-compact-prune)
   ("P" "Purge tombstones (dependency-based)" beads-compact-purge)
   ("z" "Analyze candidates" beads-compact-analyze)
   ("a" "Apply summary" beads-compact-apply)
   ("A" "Auto compact (legacy)" beads-compact-auto)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-command-compact)
;;; beads-command-compact.el ends here
