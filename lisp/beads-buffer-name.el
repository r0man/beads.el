;;; beads-buffer-name.el --- Centralized buffer naming for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides centralized buffer naming logic for all beads.el
;; buffers.  It ensures consistent naming across list, show, agent, and
;; utility buffers, with support for:
;;
;; - Project prefix: All buffers include project name
;; - Worktree disambiguation: When in a git worktree, both worktree name
;;   and branch are shown (e.g., PROJECT/WORKTREE@BRANCH).  The worktree
;;   directory name provides filesystem-level identity (unique on disk),
;;   while the branch name provides semantic context (what you're working on).
;;   Branch names containing "/" are sanitized to "-" (e.g., feature/auth
;;   becomes feature-auth) to avoid ambiguity with path delimiters.
;; - Issue ID: Show and agent buffers can include issue context
;; - Title truncation: Long titles are truncated with ellipsis
;;
;; Buffer name formats:
;;
;; List buffers:
;;   *beads-list: PROJECT*
;;   *beads-list: PROJECT/WORKTREE@BRANCH*       (in worktree)
;;   *beads-ready: PROJECT*
;;   *beads-blocked: PROJECT*
;;   *beads-list: PROJECT label=LABEL*
;;   *beads-list: PROJECT STATUS*
;;
;; Show buffers:
;;   *beads-show: PROJECT/ISSUE-ID TITLE*
;;   *beads-show: PROJECT/WORKTREE@BRANCH/ISSUE-ID TITLE*  (in worktree)
;;
;; Agent buffers:
;;   *beads-agent: PROJECT/TYPE:BACKEND#N*
;;   *beads-agent: PROJECT/WORKTREE@BRANCH/TYPE:BACKEND#N*      (in worktree)
;;   *beads-agent: PROJECT/TYPE:BACKEND#N ISSUE-ID TITLE*       (with issue)
;;
;;   Where TYPE is the agent type (Task, Plan, Review, etc.) and
;;   BACKEND is the agent backend (claude-code, claudemacs, etc.)
;;
;; Utility buffers:
;;   *beads-stats: PROJECT*
;;   *beads-graph: PROJECT*
;;   *beads-labels: PROJECT*
;;   etc.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function beads-git-get-project-name "beads-git")
(declare-function beads-git-get-branch "beads-git")
(declare-function beads-git-in-worktree-p "beads-git")
(declare-function beads-git-find-project-root "beads-git")

;;; Constants

(defconst beads-buffer-name-max-title-length 30
  "Maximum length for issue titles in buffer names.
Longer titles are truncated with ellipsis.")

;;; Helper Functions

(defun beads-buffer-name--truncate-title (title)
  "Truncate TITLE to `beads-buffer-name-max-title-length' chars.
Adds ellipsis if truncated.  Returns empty string if TITLE is nil."
  (if (or (null title) (string-empty-p title))
      ""
    (if (<= (length title) beads-buffer-name-max-title-length)
        title
      (concat (substring title 0 (- beads-buffer-name-max-title-length 3)) "..."))))

(defun beads-buffer-name--sanitize-branch (branch)
  "Sanitize BRANCH for use in buffer names.
Replaces `/` with `-` to avoid ambiguity with path delimiters.
Branch names like `feature/auth` become `feature-auth`."
  (when branch
    (replace-regexp-in-string "/" "-" branch)))

(defun beads-buffer-name--get-worktree-name ()
  "Return worktree directory basename if in a git worktree, nil otherwise."
  (when (beads-git-in-worktree-p)
    (when-let ((root (beads-git-find-project-root)))
      (file-name-nondirectory (directory-file-name root)))))

(defun beads-buffer-name--get-worktree-info ()
  "Return cons of (WORKTREE . BRANCH) if in a git worktree, nil otherwise.
WORKTREE is the directory basename, BRANCH is the sanitized branch name."
  (when (beads-git-in-worktree-p)
    (cons (beads-buffer-name--get-worktree-name)
          (beads-buffer-name--sanitize-branch (beads-git-get-branch)))))

;;; Project Prefix

(defun beads-buffer-name--project-prefix (&optional project worktree branch)
  "Build project prefix string for buffer names.
PROJECT is the project name (defaults to current project).
WORKTREE is the worktree directory name (defaults to auto-detected).
BRANCH is the branch name (defaults to auto-detected).
Branch names are sanitized to replace `/` with `-`.

Returns:
  \"PROJECT\" if not in worktree
  \"PROJECT/WORKTREE@BRANCH\" if in worktree"
  (let* ((proj (or project (beads-git-get-project-name) "unknown"))
         (wt-info (unless (or worktree branch)
                    (beads-buffer-name--get-worktree-info)))
         (wt (or worktree (car wt-info)))
         (br (or (beads-buffer-name--sanitize-branch branch) (cdr wt-info))))
    (if (and wt br)
        (format "%s/%s@%s" proj wt br)
      proj)))

;;; List Buffer Names

(defun beads-buffer-name-list (&optional type filter project worktree branch)
  "Generate list buffer name.
TYPE is one of: nil/\"list\", \"ready\", \"blocked\".
FILTER is optional filter info (e.g., \"label=backend\", \"open\").
PROJECT, WORKTREE, and BRANCH are optional overrides.

Examples:
  (beads-buffer-name-list)
    => \"*beads-list: myproject*\"
  (beads-buffer-name-list \"ready\")
    => \"*beads-ready: myproject*\"
  (beads-buffer-name-list nil \"label=api\")
    => \"*beads-list: myproject label=api*\"
  (beads-buffer-name-list nil nil \"proj\" \"proj-wt\" \"feature\")
    => \"*beads-list: proj/proj-wt@feature*\""
  (let* ((prefix (beads-buffer-name--project-prefix project worktree branch))
         (buf-type (cond
                    ((null type) "list")
                    ((string= type "list") "list")
                    ((string= type "ready") "ready")
                    ((string= type "blocked") "blocked")
                    (t "list")))
         (base (format "*beads-%s: %s*" buf-type prefix)))
    (if (and filter (not (string-empty-p filter)))
        (format "%s %s*" (substring base 0 -1) filter)
      base)))

;;; Show Buffer Names

(defun beads-buffer-name-show (issue-id &optional title project worktree branch)
  "Generate show buffer name for ISSUE-ID.
TITLE is the issue title (truncated if too long).
PROJECT, WORKTREE, and BRANCH are optional overrides."
  (let* ((prefix (beads-buffer-name--project-prefix project worktree branch))
         (truncated-title (beads-buffer-name--truncate-title title)))
    (if (string-empty-p truncated-title)
        (format "*beads-show: %s/%s*" prefix issue-id)
      (format "*beads-show: %s/%s %s*" prefix issue-id truncated-title))))

;;; Agent Buffer Names

(defun beads-buffer-name-agent (type backend instance
                                 &optional issue-id title
                                 project worktree branch)
  "Generate agent buffer name.
TYPE is the agent type (e.g., \"Task\", \"Plan\", \"Review\").
BACKEND is the agent backend (e.g., \"claude-code\", \"claudemacs\").
INSTANCE is the instance number.
ISSUE-ID and TITLE are optional issue context.
PROJECT, WORKTREE, and BRANCH are optional overrides.

Examples:
  (beads-buffer-name-agent \"Task\" \"claude-code\" 1)
    => \"*beads-agent: myproject/Task:claude-code#1*\"
  (beads-buffer-name-agent \"Plan\" \"claudemacs\" 2 \"bd-42\" \"Fix login\")
    => \"*beads-agent: myproject/Plan:claudemacs#2 bd-42 Fix login*\"
  (beads-buffer-name-agent \"Review\" \"cc\" 1 nil nil \"p\" \"wt\" \"feat\")
    => \"*beads-agent: p/wt@feat/Review:cc#1*\""
  (let* ((prefix (beads-buffer-name--project-prefix project worktree branch))
         (base (format "*beads-agent: %s/%s:%s#%d" prefix type backend instance)))
    (if issue-id
        (let ((truncated-title (beads-buffer-name--truncate-title title)))
          (if (string-empty-p truncated-title)
              (format "%s %s*" base issue-id)
            (format "%s %s %s*" base issue-id truncated-title)))
      (format "%s*" base))))

;;; Utility Buffer Names

(defun beads-buffer-name-utility (type &optional suffix project worktree branch)
  "Generate utility buffer name.
TYPE is the buffer type (e.g., \"stats\", \"graph\", \"labels\").
SUFFIX is optional additional context (e.g., issue ID for dep-tree).
PROJECT, WORKTREE, and BRANCH are optional overrides.

Examples:
  (beads-buffer-name-utility \"stats\")
    => \"*beads-stats: myproject*\"
  (beads-buffer-name-utility \"dep-tree\" \"bd-42\")
    => \"*beads-dep-tree: myproject/bd-42*\""
  (let ((prefix (beads-buffer-name--project-prefix project worktree branch)))
    (if (and suffix (not (string-empty-p suffix)))
        (format "*beads-%s: %s/%s*" type prefix suffix)
      (format "*beads-%s: %s*" type prefix))))

;;; Parse Functions

(defun beads-buffer-name-parse-list (buffer-name)
  "Parse BUFFER-NAME as a list buffer name.
Returns plist with :type, :project, :worktree, :branch, :filter, or nil."
  (when (string-match
         "\\`\\*beads-\\(list\\|ready\\|blocked\\): \\([^/*@ ]+\\)\\(?:/\\([^@*]+\\)@\\([^* ]+\\)\\)?\\(?: \\(.+\\)\\)?\\*\\'"
         buffer-name)
    (list :type (match-string 1 buffer-name)
          :project (match-string 2 buffer-name)
          :worktree (match-string 3 buffer-name)
          :branch (match-string 4 buffer-name)
          :filter (match-string 5 buffer-name))))

(defun beads-buffer-name-parse-show (buffer-name)
  "Parse BUFFER-NAME as a show buffer name.
Returns plist with :project, :worktree, :branch, :issue-id, :title, or nil."
  (when (string-match
         "\\`\\*beads-show: \\([^/]+\\)/\\(?:\\([^@]+\\)@\\([^/]+\\)/\\)?\\([^ *]+\\)\\(?: \\(.+\\)\\)?\\*\\'"
         buffer-name)
    (list :project (match-string 1 buffer-name)
          :worktree (match-string 2 buffer-name)
          :branch (match-string 3 buffer-name)
          :issue-id (match-string 4 buffer-name)
          :title (match-string 5 buffer-name))))

(defun beads-buffer-name-parse-agent (buffer-name)
  "Parse BUFFER-NAME as an agent buffer name.
Returns plist with :project, :worktree, :branch, :type, :backend, :instance,
:issue-id, :title.  Returns nil if not an agent buffer or if BUFFER-NAME is nil."
  (when (and buffer-name
             (string-match
              (concat "\\`\\*beads-agent: "
                      "\\([^/]+\\)"                ; project
                      "/\\(?:\\([^@]+\\)@\\([^/]+\\)/\\)?"  ; /WORKTREE@BRANCH/ (optional)
                      "\\([^:]+\\)"                ; type
                      ":\\([^#]+\\)"               ; :backend
                      "#\\([0-9]+\\)"              ; #N
                      "\\(?: \\([^ *]+\\)\\)?"     ; issue-id
                      "\\(?: \\(.+\\)\\)?"         ; title
                      "\\*\\'")
              buffer-name))
    (list :project (match-string 1 buffer-name)
          :worktree (match-string 2 buffer-name)
          :branch (match-string 3 buffer-name)
          :type (match-string 4 buffer-name)
          :backend (match-string 5 buffer-name)
          :instance (string-to-number (match-string 6 buffer-name))
          :issue-id (match-string 7 buffer-name)
          :title (match-string 8 buffer-name))))

(defun beads-buffer-name-parse-utility (buffer-name)
  "Parse BUFFER-NAME as a utility buffer name.
Returns plist with :type, :project, :worktree, :branch, :suffix, or nil."
  (when (string-match
         (concat "\\`\\*beads-\\([a-z-]+\\): "
                 "\\([^/*@ ]+\\)"             ; project
                 "\\(?:/\\([^@*]+\\)@\\([^/*]+\\)\\)?"  ; /WORKTREE@BRANCH (optional)
                 "\\(?:/\\([^*]+\\)\\)?"      ; /suffix
                 "\\*\\'")
         buffer-name)
    (let ((type (match-string 1 buffer-name)))
      ;; Exclude known non-utility types
      (unless (member type '("list" "ready" "blocked" "show" "agent"))
        (list :type type
              :project (match-string 2 buffer-name)
              :worktree (match-string 3 buffer-name)
              :branch (match-string 4 buffer-name)
              :suffix (match-string 5 buffer-name))))))

;;; Predicates

(defun beads-buffer-name-list-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads list buffer."
  (and (beads-buffer-name-parse-list buffer-name) t))

(defun beads-buffer-name-show-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads show buffer."
  (and (beads-buffer-name-parse-show buffer-name) t))

(defun beads-buffer-name-agent-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads agent buffer."
  (and (beads-buffer-name-parse-agent buffer-name) t))

(defun beads-buffer-name-utility-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads utility buffer."
  (and (beads-buffer-name-parse-utility buffer-name) t))

(defun beads-buffer-name-beads-p (buffer-name)
  "Return non-nil if BUFFER-NAME is any type of beads buffer."
  (or (beads-buffer-name-list-p buffer-name)
      (beads-buffer-name-show-p buffer-name)
      (beads-buffer-name-agent-p buffer-name)
      (beads-buffer-name-utility-p buffer-name)))

;;; Buffer Finding

(defun beads-buffer-name-find-list-buffers (&optional project)
  "Find all list buffers, optionally filtered by PROJECT."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-name-parse-list name)))
         (or (null project)
             (string= project (plist-get parsed :project))))))
   (buffer-list)))

(defun beads-buffer-name-find-show-buffers (&optional project issue-id)
  "Find show buffers, optionally filtered by PROJECT and/or ISSUE-ID."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-name-parse-show name)))
         (and (or (null project)
                  (string= project (plist-get parsed :project)))
              (or (null issue-id)
                  (string= issue-id (plist-get parsed :issue-id)))))))
   (buffer-list)))

(defun beads-buffer-name-find-agent-buffers (&optional project backend)
  "Find agent buffers, optionally filtered by PROJECT and/or BACKEND."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-name-parse-agent name)))
         (and (or (null project)
                  (string= project (plist-get parsed :project)))
              (or (null backend)
                  (string= backend (plist-get parsed :backend)))))))
   (buffer-list)))

(defun beads-buffer-name-find-utility-buffers (&optional project type)
  "Find utility buffers, optionally filtered by PROJECT and/or TYPE."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (when-let ((parsed (beads-buffer-name-parse-utility name)))
         (and (or (null project)
                  (string= project (plist-get parsed :project)))
              (or (null type)
                  (string= type (plist-get parsed :type)))))))
   (buffer-list)))

(provide 'beads-buffer-name)
;;; beads-buffer-name.el ends here
