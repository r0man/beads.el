;;; beads-completion.el --- Completion support for beads -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides completion table, custom completion style, and
;; annotation functions for beads issue selection.
;;
;; Key features:
;; - Custom completion style `beads-issue-title' that matches both ID and title
;; - Text properties on candidates for title-aware matching
;; - Annotation function showing [P#] [type] status - Title
;; - Group function for sorting by status
;; - Works with all completion UIs (Vertico, Ivy, Helm, default)
;;
;; The candidates remain pure IDs, but title is stored as a text property
;; for matching.  This means history and saved completions remain clean.

;;; Code:

(require 'beads-command)
(require 'beads-worktree-types)
(require 'seq)
(require 'subr-x)

;; Forward declarations
(defvar beads-completion-show-unavailable-backends)
(declare-function beads-list-execute "beads-command-list" (&rest args))
(declare-function beads--get-database-path "beads-util" ())

;;; Completion Cache

(defvar beads-completion--cache nil
  "Cache for issue list.
Format: (DB-PATH TIMESTAMP . ISSUES-LIST), keyed by database path
to avoid stale data when switching between projects.")

(defvar beads-completion--cache-ttl 5
  "Time-to-live for completion cache in seconds.")

(defun beads-completion--get-cached-issues ()
  "Get cached issue list, refreshing if stale.
Returns all issues including closed ones so that commands like
`beads-reopen' and `beads-delete' can complete on any issue.
Cache is keyed by database path to avoid stale data when switching
between projects.
On fetch failure, returns previous cached data (if any) with a warning."
  (let ((now (float-time))
        (db (beads--get-database-path)))
    (when (or (null beads-completion--cache)
              (not (equal db (car beads-completion--cache)))
              (> (- now (cadr beads-completion--cache))
                 beads-completion--cache-ttl))
      (condition-case err
          (setq beads-completion--cache
                (list db now (beads-list-execute :all t)))
        (error
         ;; Keep existing cache data on error (stale data is better than none)
         ;; Only show warning if we have stale data to return
         (when beads-completion--cache
           (message "Warning: Failed to refresh issues: %s (using cached data)"
                    (error-message-string err))))))
    (nth 2 beads-completion--cache)))

(defun beads-completion-invalidate-cache ()
  "Invalidate the completion cache."
  (setq beads-completion--cache nil))

;;; Issue Sorting

(defun beads-completion--status-priority (status)
  "Return numeric priority for STATUS (lower = higher priority).
Order: in_progress (0) > open (1) > blocked (2) > closed (3)."
  (pcase status
    ("in_progress" 0)
    ("open" 1)
    ("blocked" 2)
    ("closed" 3)
    (_ 4)))

(defun beads-completion--sort-issues (issues)
  "Sort ISSUES by status priority, then by issue priority.
Status order: in_progress > open > blocked > closed.
Within same status, higher priority (lower number) comes first.
Handles both lists and vectors (converts to list if needed)."
  ;; Use seq-sort to handle both lists and vectors, always returns a list
  (seq-sort
   (lambda (a b)
     (let ((status-a (beads-completion--status-priority (oref a status)))
           (status-b (beads-completion--status-priority (oref b status))))
       (if (= status-a status-b)
           ;; Same status: sort by priority (lower = higher priority)
           (< (or (oref a priority) 4) (or (oref b priority) 4))
         ;; Different status: sort by status priority
         (< status-a status-b))))
   issues))

;;; Issue Completion Table

(defun beads-completion-issue-table ()
  "Return completion table for issue IDs with title-aware matching.
Issues are sorted by status priority: in_progress > open > blocked > closed,
then by issue priority within each status."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-issue)
          (annotation-function . beads-completion--issue-annotate)
          (group-function . beads-completion--issue-group))
      (let ((issues (beads-completion--sort-issues
                     (beads-completion--get-cached-issues))))
        (complete-with-action
         action
         (mapcar (lambda (i)
                   (propertize (oref i id)
                               'beads-title (oref i title)
                               'beads-issue i))
                 issues)
         string pred)))))

;;; Issue Annotation Function

(defun beads-completion--issue-annotate (candidate)
  "Annotate issue CANDIDATE with [P#] [type] status - title."
  (condition-case nil
      (let ((issue (get-text-property 0 'beads-issue candidate)))
        (when issue
          (let ((status (oref issue status))
                (title (oref issue title))
                (priority (oref issue priority))
                (type (oref issue issue-type)))
            (format " [P%s] [%s] %s - %s"
                    priority
                    (or type "task")
                    (propertize (upcase status)
                                'face (pcase status
                                        ("open" 'success)
                                        ("in_progress" 'warning)
                                        ("blocked" 'error)
                                        ("closed" 'shadow)
                                        (_ 'default)))
                    (beads-completion--truncate-string title 50)))))
    (error "")))

;;; Issue Group Function

(defun beads-completion--issue-group (candidate transform)
  "Group issue CANDIDATE by status.  If TRANSFORM is non-nil, return CANDIDATE."
  (if transform
      candidate
    (let ((issue (get-text-property 0 'beads-issue candidate)))
      (if issue
          (pcase (oref issue status)
            ("open" "Open")
            ("in_progress" "In Progress")
            ("blocked" "Blocked")
            ("closed" "Closed")
            (_ "Other"))
        "Other"))))

;;; Issue Completion Style

(defun beads-completion--issue-style-try (string table pred point)
  "Try completion of STRING with title-aware matching.
TABLE is the completion table, PRED is the predicate, POINT is the position.
Return nil if no matches, t if STRING is exact unique match, single match
string if only one match, or STRING itself if multiple matches."
  (let ((matches (beads-completion--issue-style-all string table pred point)))
    (cond
     ((null matches) nil)
     ((and (= (length matches) 1)
           (string= string (substring-no-properties (car matches))))
      t)
     ((= (length matches) 1)
      ;; Return plain string - properties cause issues with completion machinery
      (substring-no-properties (car matches)))
     (t string))))  ;; Multiple matches - return input unchanged

(defun beads-completion--issue-style-all (string table pred point)
  "Return issue completions matching STRING against ID or title.
TABLE is the completion table, PRED is the predicate, POINT is ignored."
  (ignore point)
  (let* ((all (all-completions "" table pred))
         (pattern (regexp-quote string))
         (case-fold-search t))
    ;; Keep text properties - needed for annotation and grouping functions
    (seq-filter
     (lambda (candidate)
       (let ((title (get-text-property 0 'beads-title candidate)))
         (or (string-match-p pattern candidate)
             (and title (string-match-p pattern title)))))
     all)))

;;; Shared Utilities

(defun beads-completion--truncate-string (str max-len)
  "Truncate STR to MAX-LEN characters with ellipsis."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 3)) "...")
    (or str "")))

;;; Registration

;; Add custom completion style (idempotent - won't duplicate)
(unless (assq 'beads-issue-title completion-styles-alist)
  (add-to-list 'completion-styles-alist
               '(beads-issue-title
                 beads-completion--issue-style-try
                 beads-completion--issue-style-all
                 "Match beads issue by ID or title.")))

(defun beads-completion-read-issue (prompt &optional predicate require-match
                                           initial-input history default)
  "Read an issue ID with title-aware completion.
PROMPT is the prompt string.  PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HISTORY, and DEFAULT are passed to `completing-read'.
This function enables the custom `beads-issue-title' completion style
for matching on both issue ID and title."
  (let ((completion-category-overrides
         (cons '(beads-issue (styles beads-issue-title basic))
               completion-category-overrides)))
    (completing-read prompt (beads-completion-issue-table)
                     predicate require-match initial-input history default)))

;;; Completion-at-Point (CAPF) Support

(defun beads-completion-at-point ()
  "Completion-at-point function for beads issue IDs.
Detects partial issue ID at point and offers completions.
Triggers when there are 2+ characters starting with a letter."
  (let ((case-fold-search nil))
    (save-excursion
      ;; Move backward to find start of potential issue ID
      (skip-chars-backward "a-zA-Z0-9._-")
      (let ((start (point)))
        (skip-chars-forward "a-zA-Z0-9._-")
        (let* ((end (point))
               (len (- end start)))
          ;; Trigger with 2+ chars starting with a letter
          (when (and (>= len 2)
                     (save-excursion
                       (goto-char start)
                       (looking-at "[a-zA-Z]")))
            (list start end
                  (beads-completion-issue-table)
                  :exclusive 'no)))))))

(defun beads-completion--setup ()
  "Add beads CAPF to current buffer."
  (unless (memq #'beads-completion-at-point completion-at-point-functions)
    (setq-local completion-at-point-functions
                (cons #'beads-completion-at-point completion-at-point-functions))))

(defun beads-completion--teardown ()
  "Remove beads CAPF from current buffer."
  (setq-local completion-at-point-functions
              (remq #'beads-completion-at-point completion-at-point-functions)))

;;;###autoload
(define-minor-mode beads-completion-mode
  "Global minor mode for beads issue ID in-buffer completion.
When enabled, issue IDs can be completed in any buffer using
\\[completion-at-point] (typically M-TAB or C-M-i)."
  :global t
  :group 'beads
  :lighter nil
  (if beads-completion-mode
      (progn
        ;; Add to all existing buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (beads-completion--setup)))
        ;; Add to future buffers
        (add-hook 'after-change-major-mode-hook #'beads-completion--setup))
    ;; Remove from all buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (beads-completion--teardown)))
    (remove-hook 'after-change-major-mode-hook #'beads-completion--setup)))

;;; Backend Completion Support

(defun beads-completion-backend-table ()
  "Return completion table for AI agent backends with annotations.
This table provides metadata for rich completion experiences with Vertico,
Ivy, or other completion frameworks.

Which backends appear in the table depends on the value of
`beads-completion-show-unavailable-backends':
- When non-nil (default): all registered backends are shown
- When nil: only available backends are shown

The completion category is `beads-agent-backend', which allows
marginalia to automatically use the annotation function.  The custom
completion style allows matching on backend description as well as name."
  (require 'beads-agent-backend)
  (require 'beads-custom)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-agent-backend)
          (annotation-function . beads-completion--backend-annotate)
          (group-function . beads-completion--backend-group))
      (let ((backends (if beads-completion-show-unavailable-backends
                          (beads-agent--get-all-backends)
                        (beads-agent--get-available-backends))))
        (complete-with-action
         action
         (mapcar (lambda (b)
                   (propertize (oref b name)
                               'beads-backend b
                               'beads-description (oref b description)
                               'beads-available (beads-agent-backend-available-p b)))
                 backends)
         string pred)))))

(defun beads-completion--backend-annotate (candidate)
  "Annotate backend CANDIDATE with priority, availability, and description.
Returns a string like \" [P10] Available - MCP-based Claude Code integration\"."
  (condition-case nil
      (let* ((backend (get-text-property 0 'beads-backend candidate))
             (available (get-text-property 0 'beads-available candidate)))
        (when backend
          (let ((priority (oref backend priority))
                (description (oref backend description)))
            (concat
             (format " [P%d] %s"
                     priority
                     (if available
                         (propertize "Available" 'face 'success)
                       (propertize "Unavailable" 'face 'shadow)))
             (when (and description (not (string-empty-p description)))
               (format " - %s" description))))))
    (error "")))

(defun beads-completion--backend-group (candidate transform)
  "Group backend CANDIDATE by availability.
If TRANSFORM is non-nil, return CANDIDATE unchanged.
Otherwise, return the group name (\"Available\" or \"Unavailable\")."
  (if transform
      candidate
    (let ((available (get-text-property 0 'beads-available candidate)))
      (if available "Available" "Unavailable"))))

;;; Backend Completion Style

(defun beads-completion--backend-style-try (string table pred point)
  "Try completion of STRING with description-aware matching.
TABLE is the completion table, PRED is the predicate, POINT is the position.
Return nil if no matches, t if STRING is exact unique match, single match
string if only one match, or STRING itself if multiple matches."
  (let ((matches (beads-completion--backend-style-all string table pred point)))
    (cond
     ((null matches) nil)
     ((and (= (length matches) 1)
           (string= string (substring-no-properties (car matches))))
      t)
     ((= (length matches) 1)
      ;; Return plain string - properties cause issues with completion machinery
      (substring-no-properties (car matches)))
     (t string))))  ;; Multiple matches - return input unchanged

(defun beads-completion--backend-style-all (string table pred point)
  "Return backend completions matching STRING against name or description.
TABLE is the completion table, PRED is the predicate, POINT is ignored."
  (ignore point)
  (let* ((all (all-completions "" table pred))
         (pattern (regexp-quote string))
         (case-fold-search t))
    ;; Keep text properties - needed for annotation and grouping functions
    (seq-filter
     (lambda (candidate)
       (let ((description (get-text-property 0 'beads-description candidate)))
         (or (string-match-p pattern candidate)
             (and description (string-match-p pattern description)))))
     all)))

;; Add backend completion style (idempotent - won't duplicate)
(unless (assq 'beads-backend-description completion-styles-alist)
  (add-to-list 'completion-styles-alist
               '(beads-backend-description
                 beads-completion--backend-style-try
                 beads-completion--backend-style-all
                 "Match beads backend by name or description.")))

(defun beads-completion-read-backend (prompt &optional predicate require-match
                                              initial-input history default)
  "Read a backend name with description-aware completion.
PROMPT is the prompt string.  PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HISTORY, and DEFAULT are passed to `completing-read'.
This function enables the custom `beads-backend-description' completion style
for matching on both backend name and description."
  (let ((completion-category-overrides
         (cons '(beads-agent-backend (styles beads-backend-description basic))
               completion-category-overrides)))
    (completing-read prompt (beads-completion-backend-table)
                     predicate require-match initial-input history default)))

;;; Worktree Completion Support

(defvar beads-completion--worktree-cache nil
  "Cache for worktree list.  Format: (TIMESTAMP . WORKTREES-LIST).")

(defvar beads-completion--worktree-cache-ttl 5
  "Time-to-live for worktree completion cache in seconds.")

(defun beads-completion--get-cached-worktrees ()
  "Get cached worktree list, refreshing if stale.
On fetch failure, returns previous cached data (if any) with a warning."
  (let ((now (float-time)))
    (when (or (null beads-completion--worktree-cache)
              (> (- now (car beads-completion--worktree-cache))
                 beads-completion--worktree-cache-ttl))
      (condition-case err
          (progn
            (require 'beads-command-worktree)
            (setq beads-completion--worktree-cache
                  (cons now (beads-execute 'beads-command-worktree-list))))
        (error
         ;; Keep existing cache data on error (stale data is better than none)
         ;; Only show warning if we have stale data to return
         (when beads-completion--worktree-cache
           (message "Warning: Failed to refresh worktrees: %s (using cached data)"
                    (error-message-string err))))))
    (cdr beads-completion--worktree-cache)))

(defun beads-completion-invalidate-worktree-cache ()
  "Invalidate the worktree completion cache."
  (setq beads-completion--worktree-cache nil))

(defun beads-completion--current-worktree (worktrees)
  "Return the worktree from WORKTREES that contains `default-directory'.
Resolves symlinks via `file-truename' and uses longest-prefix matching
so nested worktrees resolve to the innermost match.  Returns nil when
no worktree's path encloses `default-directory'."
  (let* ((dir (file-name-as-directory
               (file-truename default-directory)))
         (best nil)
         (best-len 0))
    (dolist (wt worktrees)
      (let ((path (oref wt path)))
        (when (and path (not (string-empty-p path)))
          (let* ((wt-dir (file-name-as-directory (file-truename path)))
                 (len (length wt-dir)))
            (when (and (> len best-len)
                       (string-prefix-p wt-dir dir))
              (setq best wt
                    best-len len))))))
    best))

(defun beads-completion-worktree-table ()
  "Return completion table for worktree names with branch and state annotations.
This table provides metadata for rich completion experiences with Vertico,
Ivy, or other completion frameworks.

The completion category is `beads-worktree', which allows marginalia
to automatically use the annotation function.  The custom completion
style allows matching on worktree name, branch, or beads state.

When `default-directory' lies inside one of the listed worktrees, that
worktree is hoisted to the front of the candidate list and tagged with
the `beads-is-current' text property so `beads-completion--worktree-group'
renders it under the `Current Worktree' group."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-worktree)
          (annotation-function . beads-completion--worktree-annotate)
          (group-function . beads-completion--worktree-group))
      (let* ((worktrees (beads-completion--get-cached-worktrees))
             (current (beads-completion--current-worktree worktrees))
             (ordered (if current
                          (cons current (delq current (copy-sequence worktrees)))
                        worktrees)))
        (complete-with-action
         action
         (mapcar (lambda (wt)
                   (propertize (oref wt name)
                               'beads-worktree wt
                               'beads-branch (oref wt branch)
                               'beads-state (oref wt beads-state)
                               'beads-is-main (oref wt is-main)
                               'beads-is-current (eq wt current)))
                 ordered)
         string pred)))))

(defun beads-completion--worktree-annotate (candidate)
  "Annotate worktree CANDIDATE with branch and beads state.
Returns a string like \" [main] shared - /path/to/worktree\"."
  (condition-case nil
      (let ((worktree (get-text-property 0 'beads-worktree candidate)))
        (when worktree
          (let ((branch (oref worktree branch))
                (state (oref worktree beads-state))
                (is-main (oref worktree is-main))
                (path (oref worktree path)))
            (concat
             (when branch
               (format " [%s]" (propertize branch 'face 'font-lock-keyword-face)))
             (format " %s"
                     (propertize (or state "none")
                                 'face (pcase state
                                         ("shared" 'success)
                                         ("redirect" 'font-lock-type-face)
                                         ("local" 'warning)
                                         (_ 'shadow))))
             (when is-main
               (format " %s" (propertize "(main)" 'face 'font-lock-constant-face)))
             (when path
               (format " - %s" (beads-completion--truncate-string path 40)))))))
    (error "")))

(defun beads-completion--worktree-group (candidate transform)
  "Group worktree CANDIDATE for completion display.
If TRANSFORM is non-nil, return CANDIDATE unchanged.

Otherwise, return the group name.  The candidate marked with the
`beads-is-current' text property goes into \"Current Worktree\";
remaining candidates fall through to a beads-state grouping
(\"Shared\", \"Redirect\", \"Local\", or \"None\")."
  (if transform
      candidate
    (cond
     ((get-text-property 0 'beads-is-current candidate)
      "Current Worktree")
     (t
      (let ((state (get-text-property 0 'beads-state candidate)))
        (pcase state
          ("shared" "Shared (Main Repository)")
          ("redirect" "Redirect (Linked Worktrees)")
          ("local" "Local (Independent)")
          (_ "None (No Beads)")))))))

;;; Worktree Completion Style

(defun beads-completion--worktree-style-try (string table pred point)
  "Try completion of STRING with worktree-aware matching.
TABLE is the completion table, PRED is the predicate, POINT is the position.
Return nil if no matches, t if STRING is exact unique match, single match
string if only one match, or STRING itself if multiple matches."
  (let ((matches (beads-completion--worktree-style-all string table pred point)))
    (cond
     ((null matches) nil)
     ((and (= (length matches) 1)
           (string= string (substring-no-properties (car matches))))
      t)
     ((= (length matches) 1)
      ;; Return plain string - properties cause issues with completion machinery
      (substring-no-properties (car matches)))
     (t string))))  ;; Multiple matches - return input unchanged

(defun beads-completion--worktree-style-all (string table pred point)
  "Return worktree completions matching STRING against name, branch, or state.
TABLE is the completion table, PRED is the predicate, POINT is ignored."
  (ignore point)
  (let* ((all (all-completions "" table pred))
         (pattern (regexp-quote string))
         (case-fold-search t))
    ;; Keep text properties - needed for annotation and grouping functions
    (seq-filter
     (lambda (candidate)
       (let ((branch (get-text-property 0 'beads-branch candidate))
             (state (get-text-property 0 'beads-state candidate)))
         (or (string-match-p pattern candidate)
             (and branch (string-match-p pattern branch))
             (and state (string-match-p pattern state)))))
     all)))

;; Add worktree completion style (idempotent - won't duplicate)
(unless (assq 'beads-worktree-name completion-styles-alist)
  (add-to-list 'completion-styles-alist
               '(beads-worktree-name
                 beads-completion--worktree-style-try
                 beads-completion--worktree-style-all
                 "Match beads worktree by name, branch, or state.")))

(defun beads-completion-read-worktree (prompt &optional predicate require-match
                                               initial-input history default)
  "Read a worktree name with branch and state-aware completion.
PROMPT is the prompt string.  PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HISTORY, and DEFAULT are passed to `completing-read'.
This function enables the custom `beads-worktree-name' completion style
for matching on worktree name, branch, or beads state."
  (let ((completion-category-overrides
         (cons '(beads-worktree (styles beads-worktree-name basic))
               completion-category-overrides)))
    (completing-read prompt (beads-completion-worktree-table)
                     predicate require-match initial-input history default)))

;;; Combined Worktree Name Completion
;;
;; For creating new worktrees, show existing worktrees first (to avoid
;; duplicates), then issues sorted by status priority.

(defun beads-completion-worktree-name-table ()
  "Return completion table for new worktree names.
Combines:
1. Existing worktree names (to avoid duplicates) - grouped as \"Existing\"
2. Issue IDs sorted by status: in_progress > open > blocked > closed

This is used when creating a new worktree, where the name is typically
an issue ID.  Shows existing worktrees first so user can see what
already exists."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-worktree-name)
          (annotation-function . beads-completion--worktree-name-annotate)
          (group-function . beads-completion--worktree-name-group))
      (let* ((worktrees (beads-completion--get-cached-worktrees))
             (issues (beads-completion--sort-issues
                      (beads-completion--get-cached-issues)))
             ;; Build candidates: worktrees first, then issues
             (worktree-candidates
              (mapcar (lambda (wt)
                        (propertize (oref wt name)
                                    'beads-type 'worktree
                                    'beads-worktree wt
                                    'beads-branch (oref wt branch)
                                    'beads-state (oref wt beads-state)))
                      worktrees))
             (issue-candidates
              (mapcar (lambda (i)
                        (propertize (oref i id)
                                    'beads-type 'issue
                                    'beads-issue i
                                    'beads-title (oref i title)
                                    'beads-status (oref i status)))
                      issues))
             (all-candidates (append worktree-candidates issue-candidates)))
        (complete-with-action action all-candidates string pred)))))

(defun beads-completion--worktree-name-annotate (candidate)
  "Annotate worktree name CANDIDATE based on type (worktree or issue)."
  (condition-case nil
      (let ((type (get-text-property 0 'beads-type candidate)))
        (pcase type
          ('worktree
           (let* ((wt (get-text-property 0 'beads-worktree candidate))
                  (branch (and wt (oref wt branch)))
                  (state (and wt (oref wt beads-state))))
             (concat
              (propertize " [EXISTS]" 'face 'warning)
              (when branch (format " [%s]" branch))
              (when state (format " %s" state)))))
          ('issue
           (let ((issue (get-text-property 0 'beads-issue candidate)))
             (when issue
               (let ((status (oref issue status))
                     (title (oref issue title))
                     (priority (oref issue priority)))
                 (format " [P%s] %s - %s"
                         priority
                         (propertize (upcase status)
                                     'face (pcase status
                                             ("in_progress" 'warning)
                                             ("open" 'success)
                                             ("blocked" 'error)
                                             ("closed" 'shadow)
                                             (_ 'default)))
                         (beads-completion--truncate-string title 40))))))
          (_ nil)))
    (error "")))

(defun beads-completion--worktree-name-group (candidate transform)
  "Group worktree name CANDIDATE by type.
If TRANSFORM is non-nil, return CANDIDATE."
  (if transform
      candidate
    (let ((type (get-text-property 0 'beads-type candidate)))
      (pcase type
        ('worktree "Existing Worktrees")
        ('issue
         (let ((status (get-text-property 0 'beads-status candidate)))
           (pcase status
             ("in_progress" "In Progress Issues")
             ("open" "Open Issues")
             ("blocked" "Blocked Issues")
             ("closed" "Closed Issues")
             (_ "Other Issues"))))
        (_ "Other")))))

;;; Worktree Name Completion Style

(defun beads-completion--worktree-name-style-try (string table pred point)
  "Try completion of STRING for worktree names.
TABLE is the completion table, PRED is the predicate, POINT is the position."
  (let ((matches (beads-completion--worktree-name-style-all string table pred point)))
    (cond
     ((null matches) nil)
     ((and (= (length matches) 1)
           (string= string (substring-no-properties (car matches))))
      t)
     ((= (length matches) 1)
      (substring-no-properties (car matches)))
     (t string))))

(defun beads-completion--worktree-name-style-all (string table pred point)
  "Return worktree name completions matching STRING.
TABLE is the completion table, PRED is the predicate, POINT is ignored.
Matches on candidate name, issue title, or worktree branch."
  (ignore point)
  (let* ((all (all-completions "" table pred))
         (pattern (regexp-quote string))
         (case-fold-search t))
    (seq-filter
     (lambda (candidate)
       (let ((title (get-text-property 0 'beads-title candidate))
             (branch (get-text-property 0 'beads-branch candidate)))
         (or (string-match-p pattern candidate)
             (and title (string-match-p pattern title))
             (and branch (string-match-p pattern branch)))))
     all)))

;; Add worktree name completion style (idempotent - won't duplicate)
(unless (assq 'beads-worktree-name-create completion-styles-alist)
  (add-to-list 'completion-styles-alist
               '(beads-worktree-name-create
                 beads-completion--worktree-name-style-try
                 beads-completion--worktree-name-style-all
                 "Match worktree name by name, issue title, or branch.")))

(defun beads-completion-read-worktree-name (prompt &optional predicate require-match
                                                    initial-input history default)
  "Read a worktree name for creation with combined completion.
Shows existing worktrees first, then issues sorted by status priority.
PROMPT is the prompt string.  PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HISTORY, and DEFAULT are passed to `completing-read'."
  (let ((completion-category-overrides
         (cons '(beads-worktree-name (styles beads-worktree-name-create basic))
               completion-category-overrides)))
    (completing-read prompt (beads-completion-worktree-name-table)
                     predicate require-match initial-input history default)))

;;; Git Branch Completion
;;
;; Idiomatic completion for git branches used by the worktree-create
;; --branch prompt.  Mirrors the worktree-table pattern: a single
;; `git for-each-ref' call per invocation, NUL-separated record
;; parsing, candidates propertized with branch metadata, and a
;; programmed completion table with category/annotation/group metadata.

(defun beads-completion--get-git-branches ()
  "Return list of git branch records ordered for grouped completion.
Each record is a plist with :name, :current-p, :upstream, :date, and
:subject fields, sourced from a single `git for-each-ref' invocation.

Records are ordered so that grouped completion UIs render the
`Current Branch' group first: the current branch (if any), then
`main'/`master' (when not the current branch), then the remaining
branches in committerdate order (newest first).

Returns nil on error or when outside a git repository."
  (condition-case nil
      (with-temp-buffer
        (when (zerop (call-process
                      "git" nil t nil
                      "for-each-ref"
                      "--sort=-committerdate"
                      (concat "--format="
                              "%(refname:short)%00"
                              "%(HEAD)%00"
                              "%(upstream:short)%00"
                              "%(committerdate:relative)%00"
                              "%(contents:subject)")
                      "refs/heads"))
          (let ((records nil))
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
                     (fields (split-string line "\0")))
                (when (and (>= (length fields) 5)
                           (not (string-empty-p (nth 0 fields))))
                  (push (list :name (nth 0 fields)
                              :current-p (string= "*" (nth 1 fields))
                              :upstream (let ((u (nth 2 fields)))
                                          (and (not (string-empty-p u)) u))
                              :date (nth 3 fields)
                              :subject (nth 4 fields))
                        records)))
              (forward-line 1))
            (let ((ordered (nreverse records))
                  current main others)
              (dolist (r ordered)
                (cond
                 ((plist-get r :current-p)
                  (setq current r))
                 ((member (plist-get r :name) '("main" "master"))
                  (push r main))
                 (t
                  (push r others))))
              (append (and current (list current))
                      (nreverse main)
                      (nreverse others))))))
    (error nil)))

(defun beads-completion-branch-table ()
  "Return completion table for git branches with rich metadata.
Candidates are branch-name strings propertized with
`beads-branch-current', `beads-branch-upstream', `beads-branch-date',
and `beads-branch-subject' text properties.  Provides metadata for the
`beads-branch' category with annotation and grouping functions."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-branch)
          (annotation-function . beads-completion--branch-annotate)
          (group-function . beads-completion--branch-group))
      (let ((branches (beads-completion--get-git-branches)))
        (complete-with-action
         action
         (mapcar (lambda (b)
                   (propertize (plist-get b :name)
                               'beads-branch-current (plist-get b :current-p)
                               'beads-branch-upstream (plist-get b :upstream)
                               'beads-branch-date (plist-get b :date)
                               'beads-branch-subject (plist-get b :subject)))
                 branches)
         string pred)))))

(defun beads-completion--branch-annotate (candidate)
  "Annotate branch CANDIDATE with date, upstream, and subject."
  (condition-case nil
      (let ((date (get-text-property 0 'beads-branch-date candidate))
            (upstream (get-text-property 0 'beads-branch-upstream candidate))
            (subject (get-text-property 0 'beads-branch-subject candidate)))
        (concat
         (when (and date (not (string-empty-p date)))
           (format " %s"
                   (propertize date 'face 'font-lock-comment-face)))
         (when upstream
           (format " %s"
                   (propertize upstream 'face 'font-lock-keyword-face)))
         (when (and subject (not (string-empty-p subject)))
           (format "  %s"
                   (propertize (beads-completion--truncate-string subject 50)
                               'face 'font-lock-comment-face)))))
    (error "")))

(defun beads-completion--branch-group (candidate transform)
  "Group branch CANDIDATE into Current / Main / Other Branches.
If TRANSFORM is non-nil, return CANDIDATE unchanged."
  (if transform
      candidate
    (cond
     ((get-text-property 0 'beads-branch-current candidate)
      "Current Branch")
     ((member candidate '("main" "master"))
      "Main")
     (t "Other Branches"))))

(defun beads-completion-read-branch (prompt &optional predicate require-match
                                            initial-input history default)
  "Read a git branch name with rich completion.
PROMPT is the prompt string.  PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HISTORY, and DEFAULT are passed to `completing-read'.  Uses the
`beads-branch' completion category so Marginalia, embark, and other
completion frameworks can attach category-specific behavior."
  (let ((completion-category-overrides
         (cons '(beads-branch (styles basic))
               completion-category-overrides)))
    (completing-read prompt (beads-completion-branch-table)
                     predicate require-match initial-input history default)))

;;; Agent Worktree Selection
;;
;; For agent spawning, provide smart completion with:
;; 1. "Current directory" - run agent in current project directory (default)
;; 2. <issue-id> - create new worktree+branch for the issue (annotated)
;; 3. Existing worktrees - grouped together

(defconst beads-completion--current-dir-value "Current directory"
  "Special value indicating agent should run in current project directory.")

(defun beads-completion-agent-worktree-table (&optional issue-id)
  "Return completion table for agent worktree selection.
Combines:
1. \"Current directory\" - run in current project (default/first)
2. ISSUE-ID - create new worktree+branch (when ISSUE-ID given)
3. Existing worktree names - grouped as \"Existing Worktrees\"

ISSUE-ID, when non-nil, adds a \"Create New\" candidate for that issue.
The candidate text is the bare ISSUE-ID (propertized with
`beads-agent-wt-type' and `beads-issue-id'); callers identify it via
those text properties rather than parsing the candidate string.

This is used when spawning an agent to select where to run it."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-agent-worktree)
          (annotation-function . beads-completion--agent-worktree-annotate)
          (group-function . beads-completion--agent-worktree-group))
      (let* ((worktrees (beads-completion--get-cached-worktrees))
             ;; Build candidates: current dir first, then create, then worktrees
             (current-dir-candidate
              (propertize beads-completion--current-dir-value
                          'beads-agent-wt-type 'none))
             (create-candidate
              (when issue-id
                (propertize issue-id
                            'beads-agent-wt-type 'create
                            'beads-issue-id issue-id)))
             (worktree-candidates
              (mapcar (lambda (wt)
                        (propertize (oref wt name)
                                    'beads-agent-wt-type 'worktree
                                    'beads-worktree wt
                                    'beads-branch (oref wt branch)
                                    'beads-state (oref wt beads-state)))
                      worktrees))
             (all-candidates
              (append (list current-dir-candidate)
                      (when create-candidate (list create-candidate))
                      worktree-candidates)))
        (complete-with-action action all-candidates string pred)))))

(defun beads-completion--agent-worktree-annotate (candidate)
  "Annotate agent worktree CANDIDATE based on type."
  (condition-case nil
      (let ((type (get-text-property 0 'beads-agent-wt-type candidate)))
        (pcase type
          ('none
           (propertize " (run in current project)" 'face 'font-lock-comment-face))
          ('create
           (propertize " (new worktree + branch)" 'face 'font-lock-comment-face))
          ('worktree
           (let* ((wt (get-text-property 0 'beads-worktree candidate))
                  (branch (and wt (oref wt branch)))
                  (state (and wt (oref wt beads-state))))
             (concat
              (when branch (format " [%s]" branch))
              (when state (format " %s" state)))))
          (_ nil)))
    (error "")))

(defun beads-completion--agent-worktree-group (candidate transform)
  "Group agent worktree CANDIDATE by type.
If TRANSFORM is non-nil, return CANDIDATE."
  (if transform
      candidate
    (let ((type (get-text-property 0 'beads-agent-wt-type candidate)))
      (pcase type
        ('none "Default")
        ('create "Create New")
        ('worktree "Existing Worktrees")
        (_ "Other")))))

(defun beads-completion-read-agent-worktree (prompt &optional predicate require-match
                                                     initial-input history default)
  "Read agent worktree selection with smart ordering.
Shows \"Current directory\" first, then a create candidate for the
DEFAULT issue ID (rendered as the bare id with a \" (new worktree +
branch)\" annotation), then existing worktrees.
PROMPT is the prompt string.  PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HISTORY, and DEFAULT are passed to `completing-read'.
DEFAULT is also used as the issue-id for the \"Create New\" candidate.
Returns the selected value, or nil if \"Current directory\" was selected.

The returned string is one of:
- nil, if \"Current directory\" was selected;
- an existing worktree name, matched by `beads-worktree-find-by-name';
- the bare issue-id (which is also the candidate text for the
  create entry) — callers pass this straight to worktree setup."
  (let* ((completion-category-overrides
          (cons '(beads-agent-worktree (styles basic))
                completion-category-overrides))
         (result (completing-read prompt
                                  (beads-completion-agent-worktree-table default)
                                  predicate require-match initial-input history
                                  beads-completion--current-dir-value)))
    (if (string= result beads-completion--current-dir-value)
        nil
      result)))

;;; Marginalia Integration

;; When marginalia is available, register richer annotators for beads
;; completion categories.  The existing `annotation-function' metadata
;; provides fallback annotations without marginalia.
;;
;; Registered via `with-eval-after-load' so marginalia remains optional.

(eval-when-compile (require 'marginalia nil t))

(defvar marginalia-annotators)
(declare-function marginalia--truncate "marginalia" (str width))

(defun beads-completion--marginalia-annotate-issue (cand)
  "Marginalia annotator for beads issue candidates.
CAND is the candidate string with beads-issue text property."
  (when-let ((issue (get-text-property 0 'beads-issue cand)))
    (let ((status (oref issue status))
          (priority (oref issue priority))
          (type (or (oref issue issue-type) "task"))
          (title (or (oref issue title) "")))
      (marginalia--fields
       ((format "P%s" (or priority "?"))
        :face (pcase (or priority 9)
                (0 'error)
                (1 'warning)
                (_ 'shadow))
        :width 3)
       (type :face 'font-lock-type-face :width 8 :truncate 8)
       (status
        :face (pcase status
                ("open" 'success)
                ("in_progress" 'warning)
                ("blocked" 'error)
                ("closed" 'shadow)
                (_ 'default))
        :width 12)
       (title :width 60 :truncate 60)))))

(defun beads-completion--marginalia-annotate-backend (cand)
  "Marginalia annotator for beads agent backend candidates.
CAND is the candidate string with beads-backend text property."
  (when-let ((backend (get-text-property 0 'beads-backend cand)))
    (let ((available (get-text-property 0 'beads-available cand))
          (priority (oref backend priority))
          (description (or (oref backend description) "")))
      (marginalia--fields
       ((format "P%s" priority) :face 'shadow :width 4)
       ((if available "available" "unavailable")
        :face (if available 'success 'shadow)
        :width 12)
       (description :width 50 :truncate 50)))))

(defun beads-completion--marginalia-annotate-worktree (cand)
  "Marginalia annotator for beads worktree candidates.
CAND is the candidate string with beads-worktree text property."
  (when-let ((worktree (get-text-property 0 'beads-worktree cand)))
    (let ((branch (or (oref worktree branch) ""))
          (state (or (oref worktree beads-state) "none"))
          (is-main (oref worktree is-main))
          (path (or (oref worktree path) "")))
      (marginalia--fields
       (branch :face 'font-lock-keyword-face :width 20 :truncate 20)
       (state
        :face (pcase state
                ("shared" 'success)
                ("redirect" 'font-lock-type-face)
                ("local" 'warning)
                (_ 'shadow))
        :width 8)
       ((if is-main "main" "") :face 'font-lock-constant-face :width 5)
       (path :face 'shadow :width 40 :truncate 40)))))

(defun beads-completion--marginalia-annotate-worktree-name (cand)
  "Marginalia annotator for combined worktree-name candidates.
CAND may have either beads-type of worktree (existing worktree) or
issue (issue ID), or no type."
  (let ((type (get-text-property 0 'beads-type cand)))
    (pcase type
      ('worktree
       (when-let ((wt (get-text-property 0 'beads-worktree cand)))
         (let ((branch (or (oref wt branch) ""))
               (state (or (oref wt beads-state) "none")))
           (marginalia--fields
            ((propertize "[EXISTS]" 'face 'warning) :width 8)
            (branch :face 'font-lock-keyword-face :width 20 :truncate 20)
            (state :face 'shadow :width 8)))))
      ('issue
       (when-let ((issue (get-text-property 0 'beads-issue cand)))
         (let ((status (oref issue status))
               (priority (oref issue priority))
               (title (or (oref issue title) "")))
           (marginalia--fields
            ((format "P%s" (or priority "?")) :face 'shadow :width 3)
            (status
             :face (pcase status
                     ("in_progress" 'warning)
                     ("open" 'success)
                     ("blocked" 'error)
                     ("closed" 'shadow)
                     (_ 'default))
             :width 12)
            (title :width 50 :truncate 50)))))
      (_ nil))))

(defun beads-completion--marginalia-annotate-branch (cand)
  "Marginalia annotator for beads branch candidates.
CAND is a branch-name string propertized with branch metadata."
  (let ((date (or (get-text-property 0 'beads-branch-date cand) ""))
        (upstream (or (get-text-property 0 'beads-branch-upstream cand) ""))
        (subject (or (get-text-property 0 'beads-branch-subject cand) "")))
    (marginalia--fields
     (date :face 'font-lock-comment-face :width 15 :truncate 15)
     (upstream :face 'font-lock-keyword-face :width 20 :truncate 20)
     (subject :face 'font-lock-comment-face :width 50 :truncate 50))))

(defun beads-completion--marginalia-annotate-agent-worktree (cand)
  "Marginalia annotator for combined agent-worktree candidates.
CAND may have beads-agent-wt-type of none, worktree, or issue."
  (let ((type (get-text-property 0 'beads-agent-wt-type cand)))
    (pcase type
      ('none
       (marginalia--fields
        ("(current project)" :face 'font-lock-comment-face :width 20)))
      ('worktree
       (when-let ((wt (get-text-property 0 'beads-worktree cand)))
         (let ((branch (or (oref wt branch) ""))
               (state (or (oref wt beads-state) "none")))
           (marginalia--fields
            ((propertize "[EXISTS]" 'face 'warning) :width 8)
            (branch :face 'font-lock-keyword-face :width 20 :truncate 20)
            (state :face 'shadow :width 8)))))
      ('issue
       (when-let ((issue (get-text-property 0 'beads-issue cand)))
         (let ((status (oref issue status))
               (priority (oref issue priority))
               (title (or (oref issue title) "")))
           (marginalia--fields
            ((format "P%s" (or priority "?")) :face 'shadow :width 3)
            (status
             :face (pcase status
                     ("in_progress" 'warning)
                     ("open" 'success)
                     ("blocked" 'error)
                     ("closed" 'shadow)
                     (_ 'default))
             :width 12)
            (title :width 50 :truncate 50)))))
      (_ nil))))

(defun beads-completion-setup-marginalia ()
  "Register beads completion categories with marginalia.
Call this after loading marginalia to enable richer annotations
in beads completion interfaces.  For example:

  (with-eval-after-load \\='marginalia
    (beads-completion-setup-marginalia))"
  (add-to-list 'marginalia-annotators
               '(beads-issue
                 beads-completion--marginalia-annotate-issue
                 none))
  (add-to-list 'marginalia-annotators
               '(beads-agent-backend
                 beads-completion--marginalia-annotate-backend
                 none))
  (add-to-list 'marginalia-annotators
               '(beads-worktree
                 beads-completion--marginalia-annotate-worktree
                 none))
  (add-to-list 'marginalia-annotators
               '(beads-worktree-name
                 beads-completion--marginalia-annotate-worktree-name
                 none))
  (add-to-list 'marginalia-annotators
               '(beads-agent-worktree
                 beads-completion--marginalia-annotate-agent-worktree
                 none))
  (add-to-list 'marginalia-annotators
               '(beads-branch
                 beads-completion--marginalia-annotate-branch
                 none)))


(provide 'beads-completion)
;;; beads-completion.el ends here
