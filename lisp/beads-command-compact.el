;;; beads-command-compact.el --- Compact command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd compact' operations.
;; The compact command manages database compaction for old closed issues.
;; The class includes full slot metadata for automatic transient menu
;; generation via `beads-defcommand'.
;;
;; Compact has multiple modes:
;; - Analyze: Export candidates for agent review (JSON output)
;; - Apply: Accept agent-provided summary
;; - Auto: AI-powered compaction (legacy, requires ANTHROPIC_API_KEY)
;; - Stats: Show compaction statistics
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
    :required t))
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
   ("A" "Auto compact (legacy)" beads-compact-auto)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-command-compact)
;;; beads-command-compact.el ends here
