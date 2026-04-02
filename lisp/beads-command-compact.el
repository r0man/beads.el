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
(require 'beads-reader)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads-check-executable "beads")
(defvar beads-auto-refresh)

;;; ============================================================
;;; Command Class: beads-command-compact-stats
;;; ============================================================

(beads-defcommand beads-command-compact-stats (beads-command-global-options)
  ()
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

(beads-defcommand beads-command-compact-analyze (beads-command-global-options)
  ((tier
    :option-type :integer
    :key "t"
    :prompt "Tier (1 or 2): "
    :choices '("1" "2")
    :transient-group "Analyze Options"
    :level 1
    :order 1)
   (limit
    :option-type :integer
    :key "l"
    :prompt "Limit (0 = no limit): "
    :transient-group "Analyze Options"
    :level 1
    :order 2))
  :documentation "Represents bd admin compact --analyze command.
Exports compaction candidates for agent review."
  :cli-command "admin compact")

(cl-defmethod beads-command-line ((_command beads-command-compact-analyze))
  "Build command line for analyze _COMMAND."
  (append (cl-call-next-method)
          '("--analyze")))

;;; ============================================================
;;; Command Class: beads-command-compact-apply
;;; ============================================================

(beads-defcommand beads-command-compact-apply (beads-command-global-options)
  ((issue-id
    :long-option "id"
    :option-type :string
    :key "i"
    :prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Apply Options"
    :level 1
    :order 1
    :required t)
   (summary
    :option-type :string
    :key "s"
    :prompt "Summary file path: "
    :transient-group "Apply Options"
    :level 1
    :order 2
    :required t))
  :documentation "Represents bd admin compact --apply command.
Accepts agent-provided summary for an issue."
  :cli-command "admin compact")

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

(beads-defcommand beads-command-compact-auto (beads-command-global-options)
  ((issue-id
    :long-option "id"
    :option-type :string
    :key "i"
    :prompt "Issue ID (or leave empty for all): "
    :transient-reader beads-reader-issue-id
    :transient-group "Auto Options"
    :level 1
    :order 1)
   (all
    :option-type :boolean
    :key "a"
    :transient-group "Auto Options"
    :level 1
    :order 2)
   (tier
    :option-type :integer
    :key "t"
    :prompt "Tier (1 or 2): "
    :choices '("1" "2")
    :transient-group "Auto Options"
    :level 1
    :order 3)
   (batch-size
    :option-type :integer
    :key "b"
    :prompt "Batch size: "
    :transient-group "Auto Options"
    :level 3
    :order 4)
   (workers
    :option-type :integer
    :key "w"
    :transient-group "Auto Options"
    :level 3
    :order 5)
   (dry-run
    :option-type :boolean
    :key "n"
    :transient-group "Auto Options"
    :level 1
    :order 6)
   (force
    :option-type :boolean
    :key "f"
    :transient-group "Auto Options"
    :level 3
    :order 7))
  :documentation "Represents bd admin compact --auto command.
AI-powered compaction (legacy, requires ANTHROPIC_API_KEY)."
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

;;;###autoload (autoload 'beads-compact-stats "beads-command-compact" nil t)
(beads-meta-define-transient beads-command-compact-stats "beads-compact-stats"
  "Show compaction statistics.

Displays counts of tier 1 and tier 2 compaction candidates,
tombstone information, and other database statistics."
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
  Analyze: Export candidates for agent review
  Apply: Accept agent-provided summary
  Auto: AI-powered compaction (legacy, requires API key)

Tiers:
  Tier 1: Semantic compression (30 days closed, 70% reduction)
  Tier 2: Ultra compression (90 days closed, 95% reduction)"
  ["Compact Commands"
   ("s" "Show statistics" beads-compact-stats)
   ("z" "Analyze candidates" beads-compact-analyze)
   ("a" "Apply summary" beads-compact-apply)
   ("A" "Auto compact (legacy)" beads-compact-auto)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-command-compact)
;;; beads-command-compact.el ends here
