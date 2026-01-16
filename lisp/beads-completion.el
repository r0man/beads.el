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

;; Forward declaration for customization variable defined in beads-custom.el
(defvar beads-completion-show-unavailable-backends)

;;; Completion Cache

(defvar beads-completion--cache nil
  "Cache for issue list.  Format: (TIMESTAMP . ISSUES-LIST).")

(defvar beads-completion--cache-ttl 5
  "Time-to-live for completion cache in seconds.")

(defun beads-completion--get-cached-issues ()
  "Get cached issue list, refreshing if stale.
On fetch failure, returns previous cached data (if any) with a warning."
  (let ((now (float-time)))
    (when (or (null beads-completion--cache)
              (> (- now (car beads-completion--cache))
                 beads-completion--cache-ttl))
      (condition-case err
          (setq beads-completion--cache
                (cons now (beads-command-list!)))
        (error
         ;; Keep existing cache data on error (stale data is better than none)
         ;; Only show warning if we have stale data to return
         (when beads-completion--cache
           (message "Warning: Failed to refresh issues: %s (using cached data)"
                    (error-message-string err))))))
    (cdr beads-completion--cache)))

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
                  (cons now (beads-command-worktree-list!))))
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

(defun beads-completion-worktree-table ()
  "Return completion table for worktree names with branch and state annotations.
This table provides metadata for rich completion experiences with Vertico,
Ivy, or other completion frameworks.

The completion category is `beads-worktree', which allows marginalia
to automatically use the annotation function.  The custom completion
style allows matching on worktree name, branch, or beads state."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-worktree)
          (annotation-function . beads-completion--worktree-annotate)
          (group-function . beads-completion--worktree-group))
      (let ((worktrees (beads-completion--get-cached-worktrees)))
        (complete-with-action
         action
         (mapcar (lambda (wt)
                   (propertize (oref wt name)
                               'beads-worktree wt
                               'beads-branch (oref wt branch)
                               'beads-state (oref wt beads-state)
                               'beads-is-main (oref wt is-main)))
                 worktrees)
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
  "Group worktree CANDIDATE by beads state.
If TRANSFORM is non-nil, return CANDIDATE unchanged.
Otherwise, return the group name (\"Shared\", \"Redirect\", \"Local\", or \"None\")."
  (if transform
      candidate
    (let ((state (get-text-property 0 'beads-state candidate)))
      (pcase state
        ("shared" "Shared (Main Repository)")
        ("redirect" "Redirect (Linked Worktrees)")
        ("local" "Local (Independent)")
        (_ "None (No Beads)")))))

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

;;; Agent Worktree Selection
;;
;; For agent spawning, provide smart completion with:
;; 1. "(no worktree)" - run agent in current project directory (default)
;; 2. Existing worktrees - grouped together
;; 3. Issue IDs - sorted by status priority

(defconst beads-completion--no-worktree-value "(no worktree)"
  "Special value indicating agent should run in current project directory.")

(defun beads-completion-agent-worktree-table ()
  "Return completion table for agent worktree selection.
Combines:
1. \"(no worktree)\" special option - run in current project (default/first)
2. Existing worktree names - grouped as \"Existing Worktrees\"
3. Issue IDs sorted by status: in_progress > open > blocked > closed

This is used when spawning an agent to select where to run it.
The default \"(no worktree)\" option runs in the current project directory."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-agent-worktree)
          (annotation-function . beads-completion--agent-worktree-annotate)
          (group-function . beads-completion--agent-worktree-group))
      (let* ((worktrees (beads-completion--get-cached-worktrees))
             (issues (beads-completion--sort-issues
                      (beads-completion--get-cached-issues)))
             ;; Build candidates: (no worktree) first, then worktrees, then issues
             (no-wt-candidate
              (propertize beads-completion--no-worktree-value
                          'beads-agent-wt-type 'none))
             (worktree-candidates
              (mapcar (lambda (wt)
                        (propertize (oref wt name)
                                    'beads-agent-wt-type 'worktree
                                    'beads-worktree wt
                                    'beads-branch (oref wt branch)
                                    'beads-state (oref wt beads-state)))
                      worktrees))
             (issue-candidates
              (mapcar (lambda (i)
                        (propertize (oref i id)
                                    'beads-agent-wt-type 'issue
                                    'beads-issue i
                                    'beads-title (oref i title)
                                    'beads-status (oref i status)))
                      issues))
             (all-candidates (cons no-wt-candidate
                                   (append worktree-candidates issue-candidates))))
        (complete-with-action action all-candidates string pred)))))

(defun beads-completion--agent-worktree-annotate (candidate)
  "Annotate agent worktree CANDIDATE based on type."
  (condition-case nil
      (let ((type (get-text-property 0 'beads-agent-wt-type candidate)))
        (pcase type
          ('none
           (propertize " (run in current project)" 'face 'font-lock-comment-face))
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

(defun beads-completion--agent-worktree-group (candidate transform)
  "Group agent worktree CANDIDATE by type.
If TRANSFORM is non-nil, return CANDIDATE."
  (if transform
      candidate
    (let ((type (get-text-property 0 'beads-agent-wt-type candidate)))
      (pcase type
        ('none "Default")
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

(defun beads-completion-read-agent-worktree (prompt &optional predicate require-match
                                                     initial-input history default)
  "Read agent worktree selection with smart ordering.
Shows \"(no worktree)\" first, then existing worktrees, then issues.
PROMPT is the prompt string.  PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HISTORY, and DEFAULT are passed to `completing-read'.
Returns the selected value, or nil if \"(no worktree)\" was selected."
  (let* ((completion-category-overrides
          (cons '(beads-agent-worktree (styles beads-worktree-name-create basic))
                completion-category-overrides))
         (result (completing-read prompt (beads-completion-agent-worktree-table)
                                  predicate require-match initial-input history
                                  (or default beads-completion--no-worktree-value))))
    (if (string= result beads-completion--no-worktree-value)
        nil
      result)))

(provide 'beads-completion)
;;; beads-completion.el ends here
