;;; beads-compact.el --- Compact old issues via semantic summarization -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; Provides transient menu interface for the `bd compact` command.
;; Compaction reduces database size by semantically summarizing closed
;; issues that are no longer actively referenced. This is permanent
;; graceful decay - original content is discarded.
;;
;; Compression tiers:
;;   - Tier 1: Semantic compression (30 days closed, 70% reduction)
;;   - Tier 2: Ultra compression (90 days closed, 95% reduction)
;;
;; Features:
;;   - Preview mode with --dry-run
;;   - Statistics display with --stats
;;   - Force compact specific issues with --force
;;   - Parallel processing with configurable workers
;;   - Batch processing with configurable batch size
;;   - Context-aware: can compact current issue from show buffer
;;
;; Usage:
;;   M-x beads-compact RET
;;
;; The transient menu provides access to all compact options:
;;   --all: Process all eligible candidates
;;   --dry-run: Preview without compacting
;;   --stats: Show compaction statistics
;;   --id: Compact specific issue
;;   --force: Force compact (bypass checks, requires --id)
;;   --tier: Compaction tier (1 or 2)
;;   --batch-size: Issues per batch
;;   --workers: Parallel workers

;;; Code:

(require 'beads)
(require 'transient)

;;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-show--issue-id "beads-show")

;;; Transient State Variables

(defvar beads-compact--all nil
  "Whether to process all eligible candidates.")

(defvar beads-compact--dry-run nil
  "Whether to preview without compacting.")

(defvar beads-compact--stats nil
  "Whether to show compaction statistics only.")

(defvar beads-compact--issue-id nil
  "Specific issue ID to compact.")

(defvar beads-compact--force nil
  "Whether to force compact (bypass checks).")

(defvar beads-compact--tier nil
  "Compaction tier (1 or 2).")

(defvar beads-compact--batch-size nil
  "Issues per batch for processing.")

(defvar beads-compact--workers nil
  "Number of parallel workers.")

;;; Utility Functions

(defun beads-compact--reset-state ()
  "Reset compact transient state."
  (setq beads-compact--all nil
        beads-compact--dry-run nil
        beads-compact--stats nil
        beads-compact--issue-id nil
        beads-compact--force nil
        beads-compact--tier nil
        beads-compact--batch-size nil
        beads-compact--workers nil))

(defun beads-compact--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (and (fboundp 'beads-list--current-issue-id)
              (derived-mode-p 'beads-list-mode))
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (and (boundp 'beads-show--issue-id)
              (derived-mode-p 'beads-show-mode))
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N*)
   (when (string-match "\\*beads-show: \\([^ ]+\\)\\*"
                       (buffer-name))
     (match-string 1 (buffer-name)))))

(defun beads-compact--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-compact--format-bool (value)
  "Format boolean VALUE for display in transient menu."
  (if value
      (propertize " [enabled]" 'face 'transient-value)
    (propertize " [disabled]" 'face 'transient-inactive-value)))

(defun beads-compact--validate-tier ()
  "Validate that tier is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-compact--tier
             (not (member beads-compact--tier '(1 2))))
    "Tier must be 1 or 2"))

(defun beads-compact--validate-batch-size ()
  "Validate that batch-size is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-compact--batch-size
             (or (not (integerp beads-compact--batch-size))
                 (<= beads-compact--batch-size 0)))
    "Batch size must be a positive integer"))

(defun beads-compact--validate-workers ()
  "Validate that workers is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-compact--workers
             (or (not (integerp beads-compact--workers))
                 (<= beads-compact--workers 0)))
    "Workers must be a positive integer"))

(defun beads-compact--validate-force-requires-id ()
  "Validate that --force is only used with --id.
Returns error message string if invalid, nil if valid."
  (when (and beads-compact--force
             (or (null beads-compact--issue-id)
                 (string-empty-p (string-trim beads-compact--issue-id))))
    "--force requires --id to be specified"))

(defun beads-compact--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-compact--validate-tier)
              (beads-compact--validate-batch-size)
              (beads-compact--validate-workers)
              (beads-compact--validate-force-requires-id))))

;;; Infix Commands

(transient-define-infix beads-compact--infix-all ()
  "Toggle --all flag to process all eligible candidates."
  :class 'transient-switch
  :description (lambda ()
                 (concat "Process all candidates (--all)"
                         (beads-compact--format-bool
                          beads-compact--all)))
  :key "a"
  :argument "--all"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-compact--all (not beads-compact--all))
            beads-compact--all))

(transient-define-infix beads-compact--infix-dry-run ()
  "Toggle --dry-run flag for preview mode."
  :class 'transient-switch
  :description (lambda ()
                 (concat "Preview without compacting (--dry-run)"
                         (beads-compact--format-bool
                          beads-compact--dry-run)))
  :key "d"
  :argument "--dry-run"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-compact--dry-run (not beads-compact--dry-run))
            beads-compact--dry-run))

(transient-define-infix beads-compact--infix-stats ()
  "Toggle --stats flag to show statistics only."
  :class 'transient-switch
  :description (lambda ()
                 (concat "Show statistics only (--stats)"
                         (beads-compact--format-bool
                          beads-compact--stats)))
  :key "s"
  :argument "--stats"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-compact--stats (not beads-compact--stats))
            beads-compact--stats))

(transient-define-infix beads-compact--infix-id ()
  "Set specific issue ID to compact."
  :class 'transient-option
  :description (lambda ()
                 (concat "Specific issue (--id)"
                         (beads-compact--format-value
                          beads-compact--issue-id)))
  :key "i"
  :argument "id="
  :prompt "Issue ID: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (completing-read
                       "Issue ID to compact: "
                       (beads--issue-completion-table)
                       nil nil beads-compact--issue-id)))
              (setq beads-compact--issue-id id)
              id)))

(transient-define-infix beads-compact--infix-force ()
  "Toggle --force flag to bypass checks (requires --id)."
  :class 'transient-switch
  :description (lambda ()
                 (concat "Force compact, bypass checks (--force)"
                         (beads-compact--format-bool
                          beads-compact--force)))
  :key "f"
  :argument "--force"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-compact--force (not beads-compact--force))
            beads-compact--force))

(transient-define-infix beads-compact--infix-tier ()
  "Set compaction tier (1 or 2)."
  :class 'transient-option
  :description (lambda ()
                 (concat "Compaction tier (--tier, default: 1)"
                         (beads-compact--format-value
                          beads-compact--tier)))
  :key "t"
  :argument "tier="
  :prompt "Tier (1 or 2): "
  :choices '("1" "2")
  :reader (lambda (_prompt _initial-input _history)
            (let ((tier (completing-read
                         "Compaction tier (1 or 2): "
                         '("1" "2")
                         nil t
                         (when beads-compact--tier
                           (number-to-string beads-compact--tier)))))
              (setq beads-compact--tier (string-to-number tier))
              tier)))

(transient-define-infix beads-compact--infix-batch-size ()
  "Set batch size for processing."
  :class 'transient-option
  :description (lambda ()
                 (concat "Batch size (--batch-size, default: 10)"
                         (beads-compact--format-value
                          beads-compact--batch-size)))
  :key "b"
  :argument "batch-size="
  :prompt "Batch size: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((size (read-number
                         "Batch size: "
                         beads-compact--batch-size)))
              (setq beads-compact--batch-size size)
              (number-to-string size))))

(transient-define-infix beads-compact--infix-workers ()
  "Set number of parallel workers."
  :class 'transient-option
  :description (lambda ()
                 (concat "Parallel workers (--workers, default: 5)"
                         (beads-compact--format-value
                          beads-compact--workers)))
  :key "w"
  :argument "workers="
  :prompt "Workers: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((workers (read-number
                            "Number of workers: "
                            beads-compact--workers)))
              (setq beads-compact--workers workers)
              (number-to-string workers))))

;;; Suffix Commands

(defun beads-compact--build-args ()
  "Build command arguments from current state.
Returns list of arguments for bd compact command."
  (let ((args nil))
    ;; Boolean flags
    (when beads-compact--all
      (setq args (append args (list "--all"))))
    (when beads-compact--dry-run
      (setq args (append args (list "--dry-run"))))
    (when beads-compact--stats
      (setq args (append args (list "--stats"))))
    (when beads-compact--force
      (setq args (append args (list "--force"))))

    ;; Options with values
    (when (and beads-compact--issue-id
               (not (string-empty-p (string-trim beads-compact--issue-id))))
      (setq args (append args (list "--id" beads-compact--issue-id))))
    (when beads-compact--tier
      (setq args (append args (list "--tier"
                                   (number-to-string beads-compact--tier)))))
    (when beads-compact--batch-size
      (setq args (append args (list "--batch-size"
                                   (number-to-string
                                    beads-compact--batch-size)))))
    (when beads-compact--workers
      (setq args (append args (list "--workers"
                                   (number-to-string
                                    beads-compact--workers)))))
    args))

(defun beads-compact--execute-internal (args)
  "Execute bd compact with ARGS and display results."
  (condition-case err
      (let* ((output (with-temp-buffer
                       (let ((exit-code
                              (apply #'call-process
                                     beads-executable nil t nil
                                     "compact" args)))
                         (unless (zerop exit-code)
                           (error "Command failed: %s" (buffer-string)))
                         (buffer-string))))
             (buf (get-buffer-create "*beads-compact*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "Beads Compact Results"
                               'face 'bold)
                    "\n")
            (insert (propertize (make-string 50 ?═) 'face 'shadow)
                    "\n\n")
            (insert output)
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)))
        (display-buffer buf)
        (if beads-compact--dry-run
            (message "Dry run completed (see *beads-compact* buffer)")
          (if beads-compact--stats
              (message "Statistics displayed")
            (message "Compact operation completed")))
        nil)
    (error
     (beads--error "Failed to compact: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-compact--execute ()
  "Execute the bd compact command."
  :key "c"
  :description "Compact issues"
  (interactive)
  (let ((errors (beads-compact--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      ;; Confirmation for destructive operations
      (when (and (not beads-compact--dry-run)
                 (not beads-compact--stats)
                 (not (y-or-n-p "This will permanently compact issues. Continue? ")))
        (user-error "Compact operation cancelled"))
      (let ((args (beads-compact--build-args)))
        (beads-compact--execute-internal args)
        (beads-compact--reset-state)))))

(transient-define-suffix beads-compact--preview ()
  "Preview what would be compacted (dry-run)."
  :key "p"
  :description "Preview candidates"
  :transient t
  (interactive)
  (let ((errors (beads-compact--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (let ((saved-dry-run beads-compact--dry-run))
        (setq beads-compact--dry-run t)
        (let ((args (beads-compact--build-args)))
          (beads-compact--execute-internal args))
        (setq beads-compact--dry-run saved-dry-run)))))

(transient-define-suffix beads-compact--reset ()
  "Reset all parameters."
  :key "R"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-compact--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-compact "beads-compact" nil t)
(transient-define-prefix beads-compact (&optional issue-id)
  "Compact old closed issues using semantic summarization.

Compaction reduces database size by summarizing closed issues that
are no longer actively referenced. This is permanent graceful decay
- original content is discarded.

Compression tiers:
  - Tier 1: Semantic compression (30 days closed, 70% reduction)
  - Tier 2: Ultra compression (90 days closed, 95% reduction)

If ISSUE-ID is provided, set it as the initial --id value."
  :value (lambda () nil)
  (interactive
   (list (beads-compact--detect-issue-id)))
  (beads-check-executable)
  ;; Set initial issue-id if detected from context
  (when (and issue-id (not beads-compact--issue-id))
    (setq beads-compact--issue-id issue-id))
  ["Compact Options"
   ["Mode"
    (beads-compact--infix-all)
    (beads-compact--infix-dry-run)
    (beads-compact--infix-stats)]
   ["Target"
    (beads-compact--infix-id)
    (beads-compact--infix-force)]
   ["Configuration"
    (beads-compact--infix-tier)
    (beads-compact--infix-batch-size)
    (beads-compact--infix-workers)]]
  ["Actions"
   ("c" "Compact issues" beads-compact--execute)
   ("p" "Preview candidates" beads-compact--preview)
   ("R" "Reset fields" beads-compact--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-compact)
;;; beads-compact.el ends here
