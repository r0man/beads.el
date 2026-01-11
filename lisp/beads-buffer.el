;;; beads-buffer.el --- Centralized buffer naming for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides centralized buffer naming logic for all beads.el
;; buffers.  It ensures consistent naming across list, show, agent, and
;; utility buffers, with support for:
;;
;; - Project prefix in brackets: All buffers include [PROJECT] or [PROJECT@BRANCH]
;; - Branch disambiguation: When not on main branch, shows [PROJECT@BRANCH]
;; - Issue ID: Show and agent buffers can include issue context
;; - Title truncation: Long titles are truncated with ellipsis
;;
;; Buffer name formats (bracket-based):
;;
;; List buffers:
;;   *beads-list[PROJECT]*
;;   *beads-list[PROJECT@BRANCH]*                (on feature branch)
;;   *beads-ready[PROJECT]*
;;   *beads-blocked[PROJECT]*
;;   *beads-list[PROJECT] FILTER*
;;
;; Show buffers:
;;   *beads-show[PROJECT]/ISSUE-ID*
;;   *beads-show[PROJECT]/ISSUE-ID TITLE*
;;   *beads-show[PROJECT@BRANCH]/ISSUE-ID TITLE*  (on feature branch)
;;
;; Agent buffers:
;;   *beads-agent[PROJECT]/TYPE#N*
;;   *beads-agent[PROJECT@BRANCH]/TYPE#N*        (on feature branch)
;;   *beads-agent[PROJECT]/TYPE#N ISSUE-ID TITLE*  (with issue)
;;
;;   Where TYPE is the agent type (Task, Plan, Review, etc.)
;;
;; Utility buffers:
;;   *beads-stats[PROJECT]*
;;   *beads-graph[PROJECT]*
;;   *beads-labels[PROJECT]*
;;   *beads-dep-tree[PROJECT]/ISSUE-ID*
;;   etc.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function beads-git-get-project-name "beads-git")
(declare-function beads-git-get-branch "beads-git")
(declare-function beads-git-in-worktree-p "beads-git")
(declare-function beads-git-find-project-root "beads-git")

;;; Constants

(defconst beads-buffer-max-title-length 30
  "Maximum length for issue titles in buffer names.
Longer titles are truncated with ellipsis.")

(defconst beads-buffer--main-branch-names '("main" "master")
  "List of branch names considered to be the main branch.")

;;; Helper Functions

(defun beads-buffer--truncate-title (title)
  "Truncate TITLE to `beads-buffer-max-title-length' chars.
Adds ellipsis if truncated.  Returns empty string if TITLE is nil."
  (if (or (null title) (string-empty-p title))
      ""
    (if (<= (length title) beads-buffer-max-title-length)
        title
      (concat (substring title 0 (- beads-buffer-max-title-length 3)) "..."))))

(defun beads-buffer--sanitize-branch (branch)
  "Sanitize BRANCH for use in buffer names.
Replaces `/` with `-` to avoid ambiguity with path delimiters.
Branch names like `feature/auth` become `feature-auth`."
  (when branch
    (replace-regexp-in-string "/" "-" branch)))

(defun beads-buffer-is-main-branch-p (&optional branch)
  "Return non-nil if BRANCH is a main branch name.
If BRANCH is nil, checks the current branch.
Main branches are defined in `beads-buffer--main-branch-names'."
  (let ((br (or branch (beads-git-get-branch))))
    (member br beads-buffer--main-branch-names)))

(defun beads-buffer-branch-is-issue-p (&optional branch)
  "Return non-nil if BRANCH matches an issue ID pattern.
Issue IDs match patterns like bd-123, worker-1, beads-abc123, etc.
The pattern requires a prefix, hyphen, and suffix containing at least
one digit.  If BRANCH is nil, check the current branch."
  (let ((br (or branch (beads-git-get-branch))))
    ;; Issue IDs have: prefix-suffix where suffix contains at least one digit
    ;; E.g., bd-123, worker-1, beads-abc123, proj-42xyz
    (and br (string-match-p "\\`[a-zA-Z]+-[a-zA-Z0-9]*[0-9][a-zA-Z0-9]*\\'" br))))

;;; Project Context

(defun beads-buffer-project-context (&optional project branch)
  "Build project context string for buffer names.
PROJECT is the project name (defaults to current project).
BRANCH is the branch name (defaults to auto-detected when PROJECT is nil).

When PROJECT is explicitly provided, BRANCH must also be provided
to include branch context.  When PROJECT is nil, branch is auto-detected
from the current git state.

Returns:
  \"PROJECT\" if on main branch or branch is not provided
  \"PROJECT@BRANCH\" if on feature branch (branch sanitized)"
  (let* ((auto-detect-p (null project))
         (proj (or project (beads-git-get-project-name) "unknown"))
         (br (if auto-detect-p
                 (unless (beads-buffer-is-main-branch-p)
                   (beads-buffer--sanitize-branch (beads-git-get-branch)))
               branch)))
    (if br
        (format "%s@%s" proj br)
      proj)))

;;; List Buffer Names

(defun beads-buffer-list (&optional type filter project branch)
  "Generate list buffer name.
TYPE is one of: nil/\"list\", \"ready\", \"blocked\".
FILTER is optional filter info (e.g., \"label=backend\", \"open\").
PROJECT and BRANCH are optional overrides.

Examples:
  (beads-buffer-list)
    => \"*beads-list[myproject]*\"
  (beads-buffer-list \"ready\")
    => \"*beads-ready[myproject]*\"
  (beads-buffer-list nil \"label=api\")
    => \"*beads-list[myproject] label=api*\"
  (beads-buffer-list nil nil \"proj\" \"feature\")
    => \"*beads-list[proj@feature]*\""
  (let* ((context (beads-buffer-project-context project branch))
         (buf-type (cond
                    ((null type) "list")
                    ((string= type "list") "list")
                    ((string= type "ready") "ready")
                    ((string= type "blocked") "blocked")
                    (t "list")))
         (base (format "*beads-%s[%s]*" buf-type context)))
    (if (and filter (not (string-empty-p filter)))
        (format "%s %s*" (substring base 0 -1) filter)
      base)))

;;; Show Buffer Names

(defun beads-buffer-show (issue-id &optional title project branch)
  "Generate show buffer name for ISSUE-ID.
TITLE is the issue title (truncated if too long).
PROJECT and BRANCH are optional overrides."
  (let* ((context (beads-buffer-project-context project branch))
         (truncated-title (beads-buffer--truncate-title title)))
    (if (string-empty-p truncated-title)
        (format "*beads-show[%s]/%s*" context issue-id)
      (format "*beads-show[%s]/%s %s*" context issue-id truncated-title))))

;;; Agent Buffer Names

(defun beads-buffer-agent (type instance
                           &optional issue-id title
                           project branch)
  "Generate agent buffer name.
TYPE is the agent type (e.g., \"Task\", \"Plan\", \"Review\").
INSTANCE is the instance number.
ISSUE-ID and TITLE are optional issue context.
PROJECT and BRANCH are optional overrides.

Examples:
  (beads-buffer-agent \"Task\" 1)
    => \"*beads-agent[myproject]/Task#1*\"
  (beads-buffer-agent \"Plan\" 2 \"bd-42\" \"Fix login\")
    => \"*beads-agent[myproject]/Plan#2 bd-42 Fix login*\"
  (beads-buffer-agent \"Review\" 1 nil nil \"p\" \"feat\")
    => \"*beads-agent[p@feat]/Review#1*\""
  (let* ((context (beads-buffer-project-context project branch))
         (base (format "*beads-agent[%s]/%s#%d" context type instance)))
    (if issue-id
        (let ((truncated-title (beads-buffer--truncate-title title)))
          (if (string-empty-p truncated-title)
              (format "%s %s*" base issue-id)
            (format "%s %s %s*" base issue-id truncated-title)))
      (format "%s*" base))))

;;; Utility Buffer Names

(defun beads-buffer-utility (type &optional suffix project branch)
  "Generate utility buffer name.
TYPE is the buffer type (e.g., \"stats\", \"graph\", \"labels\").
SUFFIX is optional additional context (e.g., issue ID for dep-tree).
PROJECT and BRANCH are optional overrides.

Examples:
  (beads-buffer-utility \"stats\")
    => \"*beads-stats[myproject]*\"
  (beads-buffer-utility \"dep-tree\" \"bd-42\")
    => \"*beads-dep-tree[myproject]/bd-42*\""
  (let ((context (beads-buffer-project-context project branch)))
    (if (and suffix (not (string-empty-p suffix)))
        (format "*beads-%s[%s]/%s*" type context suffix)
      (format "*beads-%s[%s]*" type context))))

;;; Parse Functions

(defun beads-buffer-parse-list (buffer-name)
  "Parse BUFFER-NAME as a list buffer name.
Returns plist with :type, :project, :branch, :filter, or nil."
  (when (string-match
         (concat "\\`\\*beads-\\(list\\|ready\\|blocked\\)"
                 "\\[\\([^]@]+\\)\\(?:@\\([^]]+\\)\\)?\\]"
                 "\\(?: \\(.+\\)\\)?\\*\\'")
         buffer-name)
    (list :type (match-string 1 buffer-name)
          :project (match-string 2 buffer-name)
          :branch (match-string 3 buffer-name)
          :filter (match-string 4 buffer-name))))

(defun beads-buffer-parse-show (buffer-name)
  "Parse BUFFER-NAME as a show buffer name.
Returns plist with :project, :branch, :issue-id, :title, or nil."
  (when (string-match
         (concat "\\`\\*beads-show"
                 "\\[\\([^]@]+\\)\\(?:@\\([^]]+\\)\\)?\\]"
                 "/\\([^ *]+\\)"
                 "\\(?: \\(.+\\)\\)?\\*\\'")
         buffer-name)
    (list :project (match-string 1 buffer-name)
          :branch (match-string 2 buffer-name)
          :issue-id (match-string 3 buffer-name)
          :title (match-string 4 buffer-name))))

(defun beads-buffer-parse-agent (buffer-name)
  "Parse BUFFER-NAME as an agent buffer name.
Returns plist with :project, :branch, :type, :instance,
:issue-id, :title.  Returns nil if not an agent buffer or if BUFFER-NAME is nil."
  (when (and buffer-name
             (string-match
              (concat "\\`\\*beads-agent"
                      "\\[\\([^]@]+\\)\\(?:@\\([^]]+\\)\\)?\\]"
                      "/\\([^#]+\\)"                ; type
                      "#\\([0-9]+\\)"               ; #N
                      "\\(?: \\([^ *]+\\)\\)?"      ; issue-id
                      "\\(?: \\(.+\\)\\)?"          ; title
                      "\\*\\'")
              buffer-name))
    (list :project (match-string 1 buffer-name)
          :branch (match-string 2 buffer-name)
          :type (match-string 3 buffer-name)
          :instance (string-to-number (match-string 4 buffer-name))
          :issue-id (match-string 5 buffer-name)
          :title (match-string 6 buffer-name))))

(defun beads-buffer-parse-utility (buffer-name)
  "Parse BUFFER-NAME as a utility buffer name.
Returns plist with :type, :project, :branch, :suffix, or nil."
  (when (string-match
         (concat "\\`\\*beads-\\([a-z-]+\\)"
                 "\\[\\([^]@]+\\)\\(?:@\\([^]]+\\)\\)?\\]"
                 "\\(?:/\\([^*]+\\)\\)?"      ; /suffix
                 "\\*\\'")
         buffer-name)
    (let ((type (match-string 1 buffer-name)))
      ;; Exclude known non-utility types
      (unless (member type '("list" "ready" "blocked" "show" "agent"))
        (list :type type
              :project (match-string 2 buffer-name)
              :branch (match-string 3 buffer-name)
              :suffix (match-string 4 buffer-name))))))

;;; Predicates

(defun beads-buffer-list-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads list buffer."
  (and (beads-buffer-parse-list buffer-name) t))

(defun beads-buffer-show-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads show buffer."
  (and (beads-buffer-parse-show buffer-name) t))

(defun beads-buffer-agent-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads agent buffer."
  (and (beads-buffer-parse-agent buffer-name) t))

(defun beads-buffer-utility-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads utility buffer."
  (and (beads-buffer-parse-utility buffer-name) t))

(defun beads-buffer-beads-p (buffer-name)
  "Return non-nil if BUFFER-NAME is any type of beads buffer."
  (or (beads-buffer-list-p buffer-name)
      (beads-buffer-show-p buffer-name)
      (beads-buffer-agent-p buffer-name)
      (beads-buffer-utility-p buffer-name)))

;;; Buffer Finding

(defun beads-buffer-find-list-buffers (&optional project)
  "Find all list buffers, optionally filtered by PROJECT."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-parse-list name)))
         (or (null project)
             (string= project (plist-get parsed :project))))))
   (buffer-list)))

(defun beads-buffer-find-show-buffers (&optional project issue-id)
  "Find show buffers, optionally filtered by PROJECT and/or ISSUE-ID."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-parse-show name)))
         (and (or (null project)
                  (string= project (plist-get parsed :project)))
              (or (null issue-id)
                  (string= issue-id (plist-get parsed :issue-id)))))))
   (buffer-list)))

(defun beads-buffer-find-agent-buffers (&optional project type)
  "Find agent buffers, optionally filtered by PROJECT and/or TYPE."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-parse-agent name)))
         (and (or (null project)
                  (string= project (plist-get parsed :project)))
              (or (null type)
                  (string= type (plist-get parsed :type)))))))
   (buffer-list)))

(defun beads-buffer-find-utility-buffers (&optional project type)
  "Find utility buffers, optionally filtered by PROJECT and/or TYPE."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-parse-utility name)))
         (and (or (null project)
                  (string= project (plist-get parsed :project)))
              (or (null type)
                  (string= type (plist-get parsed :type)))))))
   (buffer-list)))

;;; Backward Compatibility Aliases

;; These aliases maintain compatibility during migration
(defalias 'beads-buffer-name-list 'beads-buffer-list)
(defalias 'beads-buffer-name-show 'beads-buffer-show)
(defalias 'beads-buffer-name-agent 'beads-buffer-agent)
(defalias 'beads-buffer-name-utility 'beads-buffer-utility)
(defalias 'beads-buffer-name-parse-list 'beads-buffer-parse-list)
(defalias 'beads-buffer-name-parse-show 'beads-buffer-parse-show)
(defalias 'beads-buffer-name-parse-agent 'beads-buffer-parse-agent)
(defalias 'beads-buffer-name-parse-utility 'beads-buffer-parse-utility)
(defalias 'beads-buffer-name-list-p 'beads-buffer-list-p)
(defalias 'beads-buffer-name-show-p 'beads-buffer-show-p)
(defalias 'beads-buffer-name-agent-p 'beads-buffer-agent-p)
(defalias 'beads-buffer-name-utility-p 'beads-buffer-utility-p)
(defalias 'beads-buffer-name-beads-p 'beads-buffer-beads-p)
(defalias 'beads-buffer-name-find-list-buffers 'beads-buffer-find-list-buffers)
(defalias 'beads-buffer-name-find-show-buffers 'beads-buffer-find-show-buffers)
(defalias 'beads-buffer-name-find-agent-buffers 'beads-buffer-find-agent-buffers)
(defalias 'beads-buffer-name-find-utility-buffers 'beads-buffer-find-utility-buffers)

;;; Display Functions
;;
;; These functions provide consistent buffer display behavior across beads.el.
;; They use `display-buffer' with appropriate actions to:
;; - Reuse existing windows showing the same mode
;; - Avoid replacing the current window when showing detail views from lists
;; - Provide predictable window management

(defun beads-buffer-display-detail (buffer mode)
  "Display BUFFER as a detail view, reusing windows showing MODE.
This is for showing detail views (like issue details) from list views.
The current window is preserved and BUFFER appears in another window.

Display strategy:
1. Reuse a window already showing MODE
2. Use some other window (not current)
3. Pop up a new window if needed

MODE should be the major mode symbol (e.g., `beads-show-mode')."
  (pop-to-buffer buffer
                 `((display-buffer-reuse-mode-window
                    display-buffer-use-some-window)
                   (mode . ,mode)
                   (inhibit-same-window . t))))

(defun beads-buffer-display-same-or-reuse (buffer)
  "Display BUFFER, reusing its window if visible, else in current window.
This is for showing list views or when you want to replace the current buffer.

Display strategy:
1. If BUFFER is visible, select its window
2. Otherwise, display in current window"
  (if-let ((window (get-buffer-window buffer)))
      (select-window window)
    (switch-to-buffer buffer)))

(defun beads-buffer-display-other-window (buffer)
  "Display BUFFER in another window, never the current one.
This is for commands that explicitly request display in a different window.

Display strategy:
1. Reuse a window already showing BUFFER
2. Use some other existing window
3. Pop up a new window if needed"
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-use-some-window
                    display-buffer-pop-up-window)
                   (inhibit-same-window . t))))

(provide 'beads-buffer)
;; Also provide the old name for backward compatibility
(provide 'beads-buffer-name)
;;; beads-buffer.el ends here
