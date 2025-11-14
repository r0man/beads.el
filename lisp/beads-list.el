;;; beads-list.el --- Tabulated list mode for Beads issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Keywords: tools, project, issues

;;; Commentary:

;; beads-list.el provides tabulated-list-mode buffers for displaying
;; Beads issues in a sortable, colorized table format.
;;
;; Commands:
;;   M-x beads-list    ; Show transient menu with filters, then list issues
;;   M-x beads-ready   ; Show ready work
;;   M-x beads-blocked ; Show blocked issues
;;
;; The beads-list command now uses a transient menu interface that
;; allows setting advanced filter parameters before executing the
;; bd list command.  The transient menu supports all bd list flags
;; including status, priority, type, date ranges, text search, labels,
;; and more.
;;
;; Key bindings in beads-list-mode (after displaying results):
;;   n/p     - Next/previous issue
;;   RET     - Show issue details
;;   g       - Refresh buffer
;;   q       - Quit buffer
;;   m/u     - Mark/unmark issue
;;   U       - Unmark all issues
;;   * !     - Mark all issues (ibuffer-style)
;;   * u     - Unmark all issues (ibuffer-style)
;;   c/+     - Create new issue
;;   e       - Edit/update issue at point
;;   d/k     - Close issue at point
;;   o       - Reopen issue at point
;;   D       - Delete issue at point (destructive)
;;   w       - Copy issue ID to kill ring
;;   S       - Sort by column
;;   B s     - Bulk update status for marked issues
;;   B p     - Bulk update priority for marked issues
;;   B c     - Bulk close marked issues
;;   B o     - Bulk reopen marked issues

;;; Code:

(require 'beads)
(require 'beads-types)
(require 'beads-command)
(require 'beads-option)
(require 'transient)

;;; Forward Declarations

(declare-function beads-update "beads-update" (&optional issue-id))

;;; Customization

(defgroup beads-list nil
  "Tabulated list display for Beads issues."
  :group 'beads
  :prefix "beads-list-")

(defcustom beads-list-id-width 18
  "Width of ID column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-status-width 11
  "Width of Status column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-priority-width 8
  "Width of Priority column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-type-width 10
  "Width of Type column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-title-width 60
  "Width of Title column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-created-width 18
  "Width of Created column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-updated-width 18
  "Width of Updated column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-date-format 'absolute
  "Format for displaying creation dates in issue lists.

Options:
  `absolute'  - Absolute date and time (2025-10-20 16:36)
  `relative'  - Relative time (2 hours ago, 3 days ago)
  `iso'       - Full ISO 8601 timestamp (2025-10-20T16:36:52Z)
  `date-only' - Date without time (2025-10-20)
  string      - Custom format string for `format-time-string'

The `absolute' format sorts correctly in chronological order."
  :type '(choice (const :tag "Absolute (YYYY-MM-DD HH:MM)" absolute)
                 (const :tag "Relative (X hours/days ago)" relative)
                 (const :tag "ISO 8601 (full timestamp)" iso)
                 (const :tag "Date only (YYYY-MM-DD)" date-only)
                 (string :tag "Custom format-time-string"))
  :group 'beads-list)

;;; Faces

(defface beads-list-status-open
  '((t :inherit font-lock-keyword-face))
  "Face for open status."
  :group 'beads-list)

(defface beads-list-status-in-progress
  '((t :inherit font-lock-warning-face))
  "Face for in_progress status."
  :group 'beads-list)

(defface beads-list-status-blocked
  '((t :inherit error))
  "Face for blocked status."
  :group 'beads-list)

(defface beads-list-status-closed
  '((t :inherit shadow))
  "Face for closed status."
  :group 'beads-list)

(defface beads-list-priority-critical
  '((t :inherit error :weight bold))
  "Face for priority 0 (critical)."
  :group 'beads-list)

(defface beads-list-priority-high
  '((t :inherit warning :weight bold))
  "Face for priority 1 (high)."
  :group 'beads-list)

(defface beads-list-priority-medium
  '((t :inherit default))
  "Face for priority 2 (medium)."
  :group 'beads-list)

(defface beads-list-priority-low
  '((t :inherit shadow))
  "Face for priority 3-4 (low/backlog)."
  :group 'beads-list)

;;; Variables

(defvar-local beads-list--command nil
  "The bd command used to populate this buffer (list, ready, or blocked).")

(defvar-local beads-list--raw-issues nil
  "List of beads-issue objects for the current buffer.")

(defvar-local beads-list--marked-issues nil
  "List of marked issue IDs.")

(defvar-local beads-list--filter nil
  "Current beads-issue-filter object (nil means no filter).
Use for client-side filtering in the buffer.")

;;; Utilities

(defun beads-list--status-face (status)
  "Return face for STATUS."
  (pcase status
    ("open" 'beads-list-status-open)
    ("in_progress" 'beads-list-status-in-progress)
    ("blocked" 'beads-list-status-blocked)
    ("closed" 'beads-list-status-closed)
    (_ 'default)))

(defun beads-list--priority-face (priority)
  "Return face for PRIORITY."
  (pcase priority
    (0 'beads-list-priority-critical)
    (1 'beads-list-priority-high)
    (2 'beads-list-priority-medium)
    ((or 3 4) 'beads-list-priority-low)
    (_ 'default)))

(defun beads-list--format-status (status)
  "Format STATUS with appropriate face."
  (let ((status-str (or status "")))
    (propertize status-str 'face (beads-list--status-face status-str))))

(defun beads-list--format-priority (priority)
  "Format PRIORITY with appropriate face."
  (let ((priority-str (if priority (format "P%d" priority) "")))
    (propertize priority-str 'face (beads-list--priority-face priority))))

(defun beads-list--format-date (iso-timestamp)
  "Format ISO-TIMESTAMP according to `beads-list-date-format'.
ISO-TIMESTAMP should be an ISO 8601 string like
'2025-10-20T16:36:52.648609367Z'.  Returns a formatted string based on
the value of `beads-list-date-format'."
  (if (or (not iso-timestamp) (string-empty-p iso-timestamp))
      ""
    (let ((time (date-to-time iso-timestamp)))
      (pcase beads-list-date-format
        ('absolute
         (format-time-string "%Y-%m-%d %H:%M" time t))
        ('relative
         (let* ((now (current-time))
                (diff (time-subtract now time))
                (seconds (time-to-seconds diff)))
           (cond
            ((< seconds 60)
             (format "%ds ago" (floor seconds)))
            ((< seconds 3600)
             (format "%dm ago" (floor (/ seconds 60))))
            ((< seconds 86400)
             (let ((hours (floor (/ seconds 3600))))
               (format "%dh ago" hours)))
            ((< seconds 604800)
             (let ((days (floor (/ seconds 86400))))
               (format "%dd ago" days)))
            ((< seconds 2592000)
             (let ((weeks (floor (/ seconds 604800))))
               (format "%dw ago" weeks)))
            ((< seconds 31536000)
             (let ((months (floor (/ seconds 2592000))))
               (format "%dmo ago" months)))
            (t
             (let ((years (floor (/ seconds 31536000))))
               (format "%dy ago" years))))))
        ('iso
         (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))
        ('date-only
         (format-time-string "%Y-%m-%d" time))
        ((pred stringp)
         (format-time-string beads-list-date-format time))
        (_
         (format-time-string "%Y-%m-%d %H:%M" time))))))

(defun beads-list--issue-to-entry (issue)
  "Convert ISSUE (beads-issue object) to tabulated-list entry."
  (let* ((id (oref issue id))
         (title (or (oref issue title) ""))
         (status (oref issue status))
         (priority (oref issue priority))
         (type (or (oref issue issue-type) ""))
         (created (oref issue created-at))
         (created-str (beads-list--format-date created))
         (updated (oref issue updated-at))
         (updated-str (beads-list--format-date updated)))
    (list id
          (vector id
                  type
                  (beads-list--format-status status)
                  (beads-list--format-priority priority)
                  title
                  created-str
                  updated-str))))

(defun beads-list--populate-buffer (issues command &optional filter)
  "Populate current buffer with ISSUES using COMMAND for refresh.
Optional FILTER is a beads-issue-filter object for context."
  (setq beads-list--command command
        beads-list--raw-issues issues
        beads-list--filter filter)
  (setq tabulated-list-entries
        (mapcar #'beads-list--issue-to-entry issues))
  (tabulated-list-print t))

(defun beads-list--current-issue-id ()
  "Return the ID of the issue at point, or nil."
  (tabulated-list-get-id))

(defun beads-list--get-issue-by-id (id)
  "Return beads-issue object for ID from current buffer's raw issues."
  (seq-find (lambda (issue)
              (string= (oref issue id) id))
            beads-list--raw-issues))

;;; CLI Integration

(defun beads-issue-read (issue-id)
  "Read a beads issue by ISSUE-ID from the CLI.
Uses the bd show command with --json flag to fetch the issue.
Runs in the directory specified by `default-directory'.
Returns a beads-issue object or signals an error if not found."
  (interactive "sIssue ID: ")
  (let ((result (beads-command-show! :issue-ids (list issue-id))))
    ;; beads-command-show! returns a list, so unwrap single result
    (if (listp result)
        (car result)
      result)))

(defun beads-issue-list (&optional status)
  "List beads issues from the CLI.
Uses the bd list command with --json flag to fetch all issues.
If STATUS is provided, filters by that status.
Runs in the directory specified by `default-directory'.
Returns a list of beads-issue objects."
  (interactive)
  (if status
      (beads-command-list! :status status)
    (beads-command-list!)))

(defun beads-blocked-issue-list ()
  "List blocked beads issues from the CLI.
Uses the bd blocked command with --json flag.
Runs in the directory specified by `default-directory'.
Returns a list of beads-blocked-issue objects."
  (interactive)
  (beads-command-blocked!))

(defun beads-issue-ready (&optional limit)
  "Get ready work from the CLI.
Uses the bd ready command with --json flag.
If LIMIT is provided, limits the number of results.
Runs in the directory specified by `default-directory'.
Returns a list of beads-issue objects."
  (interactive)
  (if limit
      (beads-command-ready! :limit limit)
    (beads-command-ready!)))

;;; Transient Menu Integration

(defun beads-list--parse-transient-args (args)
  "Parse transient ARGS list into a beads-issue-filter object.
Returns a beads-issue-filter object with all applicable filters set."
  (let ((filter (beads-issue-filter)))
    ;; Boolean switches
    (when (member "--all" args)
      (oset filter all t))
    (when (member "--no-assignee" args)
      (oset filter no-assignee t))
    (when (member "--empty-description" args)
      (oset filter empty-description t))
    (when (member "--no-labels" args)
      (oset filter no-labels t))
    (when (member "--long" args)
      (oset filter long t))
    ;; String options
    (when-let ((assignee (transient-arg-value "--assignee=" args)))
      (oset filter assignee assignee))
    (when-let ((closed-after (transient-arg-value "--closed-after=" args)))
      (oset filter closed-after closed-after))
    (when-let ((closed-before (transient-arg-value "--closed-before=" args)))
      (oset filter closed-before closed-before))
    (when-let ((created-after (transient-arg-value "--created-after=" args)))
      (oset filter created-after created-after))
    (when-let ((created-before (transient-arg-value
                                 "--created-before=" args)))
      (oset filter created-before created-before))
    (when-let ((desc-contains (transient-arg-value
                                "--desc-contains=" args)))
      (oset filter description-contains desc-contains))
    (when-let ((format (transient-arg-value "--format=" args)))
      (oset filter format format))
    (when-let ((id (transient-arg-value "--id=" args)))
      (oset filter ids id))
    (when-let ((notes-contains (transient-arg-value
                                 "--notes-contains=" args)))
      (oset filter notes-contains notes-contains))
    (when-let ((status (transient-arg-value "--status=" args)))
      (oset filter status status))
    (when-let ((title (transient-arg-value "--title=" args)))
      (oset filter title-search title))
    (when-let ((title-contains (transient-arg-value
                                 "--title-contains=" args)))
      (oset filter title-contains title-contains))
    (when-let ((type (transient-arg-value "--type=" args)))
      (oset filter issue-type type))
    (when-let ((updated-after (transient-arg-value
                                "--updated-after=" args)))
      (oset filter updated-after updated-after))
    (when-let ((updated-before (transient-arg-value
                                 "--updated-before=" args)))
      (oset filter updated-before updated-before))
    ;; Repeatable options (collect all values)
    (let ((label-values nil)
          (label-any-values nil))
      (dolist (arg args)
        (when (string-prefix-p "--label=" arg)
          (push (substring arg (length "--label=")) label-values))
        (when (string-prefix-p "--label-any=" arg)
          (push (substring arg (length "--label-any=")) label-any-values)))
      (when label-values
        (oset filter labels (nreverse label-values)))
      (when label-any-values
        (oset filter labels-any (nreverse label-any-values))))
    ;; Numeric options
    (when-let ((limit-str (transient-arg-value "--limit=" args)))
      (oset filter limit (string-to-number limit-str)))
    (when-let ((priority-str (transient-arg-value "--priority=" args)))
      (oset filter priority (string-to-number priority-str)))
    (when-let ((priority-min-str (transient-arg-value
                                    "--priority-min=" args)))
      (oset filter priority-min (string-to-number priority-min-str)))
    (when-let ((priority-max-str (transient-arg-value
                                    "--priority-max=" args)))
      (oset filter priority-max (string-to-number priority-max-str)))
    filter))

;;; Transient Suffix Commands

(transient-define-suffix beads-list--transient-execute ()
  "Execute the bd list command with current filter parameters."
  :key "x"
  :description "List issues"
  (interactive)
  (let* ((args (transient-args 'beads-list))
         (filter (beads-list--parse-transient-args args))
         (cmd-args (beads-issue-filter-to-args filter)))
    (condition-case err
        (let* ((result (apply #'beads--run-command "list" cmd-args))
               (issue-objects (when (vectorp result)
                                (mapcar #'beads-issue-from-json (append result nil))))
               (buffer (get-buffer-create "*beads-list*"))
               (project-dir default-directory))
          (with-current-buffer buffer
            (beads-list-mode)
            (setq default-directory project-dir)
            (if (not issue-objects)
                (progn
                  (setq tabulated-list-entries nil)
                  (tabulated-list-print t)
                  (message "No issues found"))
              (beads-list--populate-buffer issue-objects 'list filter)
              (message "Found %d issue%s"
                       (length issue-objects)
                       (if (= (length issue-objects) 1) "" "s"))))
          (pop-to-buffer buffer))
      (error
       (message "Failed to list issues: %s"
                (error-message-string err))))))

(transient-define-suffix beads-list--transient-reset ()
  "Reset all filter parameters to their default values."
  :key "r"
  :description "Reset all filters"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all filters? ")
    (transient-reset)
    (transient--redisplay)
    (message "All filters reset")))

(transient-define-suffix beads-list--transient-preview ()
  "Preview the bd list command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-list))
         (filter (beads-list--parse-transient-args args))
         (cmd-args (beads-issue-filter-to-args filter))
         (cmd (apply #'beads--build-command "list" cmd-args))
         (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
    (message "Command: %s" cmd-string)))

;;; Transient Groups

(transient-define-group beads-list--basic-filters-section
  [:level 1 "Basic Filters"
          (beads-option-list-status)
          (beads-option-list-priority)
          (beads-option-list-type)
          (beads-option-list-assignee)])

(transient-define-group beads-list--text-search-section
  [:level 2 "Text Search"
          (beads-option-list-title)
          (beads-option-list-title-contains)
          (beads-option-list-desc-contains)
          (beads-option-list-notes-contains)])

(transient-define-group beads-list--date-filters-section
  [:level 3 "Date Filters"
          (beads-option-list-created-after)
          (beads-option-list-created-before)
          (beads-option-list-updated-after)
          (beads-option-list-updated-before)
          (beads-option-list-closed-after)
          (beads-option-list-closed-before)])

(transient-define-group beads-list--advanced-filters-section
  [:level 4 "Advanced Filters"
          (beads-option-list-priority-min)
          (beads-option-list-priority-max)
          (beads-option-list-label)
          (beads-option-list-label-any)
          (beads-option-list-id)
          (beads-option-list-no-assignee)
          (beads-option-list-empty-description)
          (beads-option-list-no-labels)])

(transient-define-group beads-list--output-options-section
  [:level 5 "Output Options"
          (beads-option-list-limit)
          (beads-option-list-long)
          (beads-option-list-format)
          (beads-option-list-all)])

;;; Main Transient

;;;###autoload (autoload 'beads-list "beads-list" nil t)
(transient-define-prefix beads-list ()
  "List issues in Beads with filter options.

This transient menu provides an interactive interface for setting
filter parameters for the bd list command.  All filters are
optional.

Transient levels control which filter groups are visible
(cycle with C-x l):
  Level 1: Basic filters (status, priority, type, assignee)
  Level 2: Text search (title, description, notes)
  Level 3: Date filters (created, updated, closed)        [default]
  Level 4: Advanced filters (priority ranges, labels, etc.)
  Level 5: Output options (limit, long format, etc.)
  Level 7: Global options (actor, db, json flags, etc.)"
  beads-list--basic-filters-section
  beads-list--text-search-section
  beads-list--date-filters-section
  beads-list--advanced-filters-section
  beads-list--output-options-section
  beads-option-global-section
  ["Actions"
   ("x" "List issues" beads-list--transient-execute)
   ("P" "Preview command" beads-list--transient-preview)
   ("R" "Reset all filters" beads-list--transient-reset)])

;;; Commands

(defun beads-list-refresh ()
  "Refresh the current issue list buffer."
  (interactive)
  (unless beads-list--command
    (user-error "No command associated with this buffer"))
  (let* ((cmd-args (when beads-list--filter
                     (beads-issue-filter-to-args beads-list--filter)))
         (issues (pcase beads-list--command
                   ('list
                    (let ((result (apply #'beads--run-command "list" cmd-args)))
                      (when (vectorp result)
                        (mapcar #'beads-issue-from-json (append result nil)))))
                   ('ready
                    (beads-issue-ready))
                   ('blocked
                    (beads-blocked-issue-list))
                   (_ (error "Unknown command: %s" beads-list--command))))
         (pos (point)))
    (if (not issues)
        (progn
          (setq tabulated-list-entries nil)
          (tabulated-list-print t)
          (message "No issues found"))
      (beads-list--populate-buffer issues beads-list--command beads-list--filter)
      (goto-char pos)
      (message "Refreshed %d issue%s"
               (length issues)
               (if (= (length issues) 1) "" "s")))))

(defun beads-list-show ()
  "Show details for the issue at point."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (let ((project-dir default-directory))
        (require 'beads-show)
        ;; Preserve project context when showing issue
        (let ((default-directory project-dir))
          (beads-show id)))
    (user-error "No issue at point")))

(defun beads-list-quit ()
  "Quit the current issue list buffer."
  (interactive)
  (quit-window t))

(defun beads-list-next ()
  "Move to next issue."
  (interactive)
  (forward-line 1))

(defun beads-list-previous ()
  "Move to previous issue."
  (interactive)
  (forward-line -1))

(defun beads-list-mark ()
  "Mark the issue at point."
  (interactive)
  (when-let* ((id (beads-list--current-issue-id)))
    (unless (member id beads-list--marked-issues)
      (push id beads-list--marked-issues))
    (tabulated-list-put-tag ">" t)))

(defun beads-list-unmark ()
  "Unmark the issue at point."
  (interactive)
  (when-let* ((id (beads-list--current-issue-id)))
    (setq beads-list--marked-issues
          (delete id beads-list--marked-issues))
    (tabulated-list-put-tag " " t)))

(defun beads-list-mark-all ()
  "Mark all issues in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (beads-list--current-issue-id)
        (beads-list-mark))
      (forward-line 0))))  ; Don't advance, beads-list-mark does that

(defun beads-list-unmark-all ()
  "Unmark all issues in the current buffer."
  (interactive)
  (setq beads-list--marked-issues nil)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (beads-list--current-issue-id)
        (tabulated-list-put-tag " "))
      (forward-line 1))))

(defun beads-list-create ()
  "Create a new issue using the beads-create transient menu."
  (interactive)
  (require 'beads-create)
  (call-interactively #'beads-create))

(defun beads-list-update ()
  "Update the issue at point using the beads-update transient menu."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-update)
        ;; beads-update will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-update))
    (user-error "No issue at point")))

(defun beads-list-close ()
  "Close the issue at point using the beads-close transient menu."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-close)
        ;; beads-close will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-close))
    (user-error "No issue at point")))

(defun beads-list-delete ()
  "Delete the issue at point using the beads-delete transient menu."
  (interactive)
  (let ((id (beads-list--current-issue-id)))
    (if id
        (progn
          (require 'beads-delete)
          ;; beads-delete will auto-detect the issue ID from beads-list context
          (beads-delete id))
      (user-error "No issue at point"))))

(defun beads-list-reopen ()
  "Reopen the issue at point using the beads-reopen transient menu."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-reopen)
        ;; beads-reopen will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-reopen))
    (user-error "No issue at point")))

(defun beads-list-copy-id ()
  "Copy the issue ID at point to the kill ring."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (kill-new id)
        (message "Copied issue ID: %s" id))
    (user-error "No issue at point")))

(defun beads-list-sort ()
  "Sort the issue list by column.
Uses tabulated-list built-in sorting."
  (interactive)
  (call-interactively #'tabulated-list-sort))

(defun beads-list-filter ()
  "Open beads-list transient menu with current filter pre-selected.
If in a beads-list buffer, the current filter is used to pre-populate the
transient menu options."
  (interactive)
  (when (and (boundp 'beads-list--filter) beads-list--filter)
    ;; Convert current filter to transient args
    (let ((args (beads-issue-filter-to-args beads-list--filter)))
      ;; Set the transient value with current filter
      (put 'beads-list 'transient--value args)
      ;; Also add to history for persistence
      (put 'beads-list 'transient--history (list args))))
  ;; Open the transient menu
  (call-interactively #'beads-list))


;;; Bulk Operations

(defun beads-list-bulk-update-status ()
  "Update status for all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((status (completing-read
                   (format "Set status for %d issue(s): "
                          (length beads-list--marked-issues))
                   '("open" "in_progress" "blocked")
                   nil t)))
      (when (and (not (string-empty-p status))
                 (y-or-n-p (format "Update status to '%s' for %d issue(s)? "
                                  status (length beads-list--marked-issues))))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads-command-execute
                   (beads-command-update
                    :issue-ids (list id)
                    :status status))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to update %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Updated %d issue(s), %d failed" success-count fail-count))))))

(defun beads-list-bulk-update-priority ()
  "Update priority for all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let* ((choices '(("0 - Critical" . 0)
                     ("1 - High" . 1)
                     ("2 - Medium" . 2)
                     ("3 - Low" . 3)
                     ("4 - Backlog" . 4)))
           (selection (completing-read
                      (format "Set priority for %d issue(s): "
                             (length beads-list--marked-issues))
                      choices nil t))
           (priority (cdr (assoc selection choices))))
      (when (and priority
                 (y-or-n-p (format "Update priority to %d for %d issue(s)? "
                                  priority (length beads-list--marked-issues))))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads-command-execute
                   (beads-command-update
                    :issue-ids (list id)
                    :priority priority))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to update %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Updated %d issue(s), %d failed" success-count fail-count))))))

(defun beads-list-bulk-close ()
  "Close all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((reason (read-string
                   (format "Reason for closing %d issue(s): "
                          (length beads-list--marked-issues)))))
      (when (y-or-n-p (format "Close %d issue(s)? "
                             (length beads-list--marked-issues)))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads-command-execute
                   (beads-command-close
                    :issue-ids (list id)
                    :reason (when (and reason (not (string-empty-p (string-trim reason))))
                             reason)))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to close %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Closed %d issue(s), %d failed"
                   success-count fail-count))))))

(defun beads-list-bulk-reopen ()
  "Reopen all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((reason (read-string
                   (format "Reason for reopening %d issue(s): "
                          (length beads-list--marked-issues)))))
      (when (y-or-n-p (format "Reopen %d issue(s)? "
                             (length beads-list--marked-issues)))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (if (and reason (not (string-empty-p (string-trim reason))))
                      (beads--run-command "reopen" id "--reason" reason)
                    (beads--run-command "reopen" id))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to reopen %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Reopened %d issue(s), %d failed"
                   success-count fail-count))))))

;;; Mode Definition

(defvar beads-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation (following standard conventions)
    (define-key map (kbd "n") #'beads-list-next)
    (define-key map (kbd "p") #'beads-list-previous)
    (define-key map (kbd "RET") #'beads-list-show)

    ;; Refresh/quit (like Magit, dired)
    (define-key map (kbd "g") #'beads-list-refresh)
    (define-key map (kbd "q") #'beads-list-quit)

    ;; Marking (like dired/ibuffer)
    (define-key map (kbd "m") #'beads-list-mark)
    (define-key map (kbd "u") #'beads-list-unmark)
    (define-key map (kbd "U") #'beads-list-unmark-all)
    (define-key map (kbd "* !") #'beads-list-mark-all)     ; ibuffer-style
    (define-key map (kbd "* *") #'beads-list-mark-all)     ; alternative
    (define-key map (kbd "* u") #'beads-list-unmark-all)   ; ibuffer-style

    ;; CRUD operations (following Emacs conventions)
    (define-key map (kbd "c") #'beads-list-create)         ; create (like many modes)
    (define-key map (kbd "+") #'beads-list-create)         ; alternative
    (define-key map (kbd "e") #'beads-list-update)         ; edit (more intuitive)
    (define-key map (kbd "d") #'beads-list-close)          ; delete/done (mark for closing)
    (define-key map (kbd "k") #'beads-list-close)          ; kill (alternative)
    (define-key map (kbd "o") #'beads-list-reopen)         ; open/reopen closed issue
    (define-key map (kbd "D") #'beads-list-delete)         ; delete permanently (destructive)

    ;; Utilities
    (define-key map (kbd "w") #'beads-list-copy-id)        ; copy (like eww, info)
    (define-key map (kbd "S") #'beads-list-sort)           ; sort menu
    (define-key map (kbd "l") #'beads-list-filter)         ; filter (open transient with current filter)

    ;; Bulk operations (like Magit) - create prefix map for B
    (let ((bulk-map (make-sparse-keymap)))
      (define-key bulk-map (kbd "s") #'beads-list-bulk-update-status)
      (define-key bulk-map (kbd "p") #'beads-list-bulk-update-priority)
      (define-key bulk-map (kbd "c") #'beads-list-bulk-close)
      (define-key bulk-map (kbd "o") #'beads-list-bulk-reopen)
      (define-key map (kbd "B") bulk-map))
    map)
  "Keymap for `beads-list-mode'.")

(define-derived-mode beads-list-mode tabulated-list-mode "Beads-List"
  "Major mode for displaying Beads issues in a tabulated list.

\\{beads-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "ID" beads-list-id-width t)
                (list "Type" beads-list-type-width t)
                (list "Status" beads-list-status-width t)
                (list "Priority" beads-list-priority-width t
                      :right-align t)
                (list "Title" beads-list-title-width t)
                (list "Created" beads-list-created-width t)
                (list "Updated" beads-list-updated-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Created" t))
  (tabulated-list-init-header))

;;; Public Commands

;;;###autoload
(defun beads-ready ()
  "Display ready Beads issues in a tabulated list."
  (interactive)
  (beads-check-executable)
  (let ((issues (beads-issue-ready))
        (buffer (get-buffer-create "*beads-ready*"))
        (project-dir default-directory))
    (with-current-buffer buffer
      (beads-list-mode)
      (setq default-directory project-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No ready issues"))
            (message "No ready issues found"))
        (beads-list--populate-buffer issues 'ready)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d ready issue%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")))))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun beads-blocked ()
  "Display blocked Beads issues in a tabulated list."
  (interactive)
  (beads-check-executable)
  (let ((issues (beads-blocked-issue-list))
        (buffer (get-buffer-create "*beads-blocked*"))
        (project-dir default-directory))
    (with-current-buffer buffer
      (beads-list-mode)
      (setq default-directory project-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No blocked issues"))
            (message "No blocked issues found"))
        (beads-list--populate-buffer issues 'blocked)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d blocked issue%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")))))))
    (pop-to-buffer buffer)))

;;; Footer

(provide 'beads-list)
;;; beads-list.el ends here
