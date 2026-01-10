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
;; - Worktree disambiguation: When in a worktree, include worktree name
;; - Issue ID: Show and agent buffers can include issue context
;; - Title truncation: Long titles are truncated with ellipsis
;;
;; Buffer name formats:
;;
;; List buffers:
;;   *beads-list: PROJECT*
;;   *beads-list: PROJECT@WORKTREE*
;;   *beads-ready: PROJECT*
;;   *beads-blocked: PROJECT*
;;   *beads-list: PROJECT label=LABEL*
;;   *beads-list: PROJECT STATUS*
;;
;; Show buffers:
;;   *beads-show: PROJECT/ISSUE-ID TITLE*
;;   *beads-show: PROJECT@WORKTREE/ISSUE-ID TITLE*
;;
;; Agent buffers:
;;   *beads-agent: PROJECT/BACKEND#N*
;;   *beads-agent: PROJECT@WORKTREE/BACKEND#N*
;;   *beads-agent: PROJECT/BACKEND#N ISSUE-ID TITLE*
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

(defun beads-buffer-name--get-worktree-name ()
  "Return worktree name if in a git worktree, nil otherwise.
The worktree name is the basename of the worktree directory."
  (when (beads-git-in-worktree-p)
    (when-let ((root (beads-git-find-project-root)))
      (file-name-nondirectory (directory-file-name root)))))

;;; Project Prefix

(defun beads-buffer-name--project-prefix (&optional project worktree)
  "Build project prefix string for buffer names.
PROJECT is the project name (defaults to current project).
WORKTREE is the worktree name (defaults to auto-detected).

Returns:
  \"PROJECT\" if not in worktree
  \"PROJECT@WORKTREE\" if in worktree"
  (let* ((proj (or project (beads-git-get-project-name) "unknown"))
         (wt (or worktree (beads-buffer-name--get-worktree-name))))
    (if wt
        (format "%s@%s" proj wt)
      proj)))

;;; List Buffer Names

(defun beads-buffer-name-list (&optional type filter project worktree)
  "Generate list buffer name.
TYPE is one of: nil/\"list\", \"ready\", \"blocked\".
FILTER is optional filter info (e.g., \"label=backend\", \"open\").
PROJECT and WORKTREE are optional overrides.

Examples:
  (beads-buffer-name-list)
    => \"*beads-list: myproject*\"
  (beads-buffer-name-list \"ready\")
    => \"*beads-ready: myproject*\"
  (beads-buffer-name-list nil \"label=api\")
    => \"*beads-list: myproject label=api*\""
  (let* ((prefix (beads-buffer-name--project-prefix project worktree))
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

(defun beads-buffer-name-show (issue-id &optional title project worktree)
  "Generate show buffer name for ISSUE-ID.
TITLE is the issue title (truncated if too long).
PROJECT and WORKTREE are optional overrides."
  (let* ((prefix (beads-buffer-name--project-prefix project worktree))
         (truncated-title (beads-buffer-name--truncate-title title)))
    (if (string-empty-p truncated-title)
        (format "*beads-show: %s/%s*" prefix issue-id)
      (format "*beads-show: %s/%s %s*" prefix issue-id truncated-title))))

;;; Agent Buffer Names

(defun beads-buffer-name-agent (backend instance &optional issue-id title project worktree)
  "Generate agent buffer name.
BACKEND is the agent backend type (e.g., \"claude-code\", \"claudemacs\").
INSTANCE is the instance number.
ISSUE-ID and TITLE are optional issue context.
PROJECT and WORKTREE are optional overrides.

Examples:
  (beads-buffer-name-agent \"claude-code\" 1)
    => \"*beads-agent: myproject/claude-code#1*\"
  (beads-buffer-name-agent \"claudemacs\" 2 \"bd-42\" \"Fix login\")
    => \"*beads-agent: myproject/claudemacs#2 bd-42 Fix login*\""
  (let* ((prefix (beads-buffer-name--project-prefix project worktree))
         (base (format "*beads-agent: %s/%s#%d" prefix backend instance)))
    (if issue-id
        (let ((truncated-title (beads-buffer-name--truncate-title title)))
          (if (string-empty-p truncated-title)
              (format "%s %s*" base issue-id)
            (format "%s %s %s*" base issue-id truncated-title)))
      (format "%s*" base))))

;;; Utility Buffer Names

(defun beads-buffer-name-utility (type &optional suffix project worktree)
  "Generate utility buffer name.
TYPE is the buffer type (e.g., \"stats\", \"graph\", \"labels\").
SUFFIX is optional additional context (e.g., issue ID for dep-tree).
PROJECT and WORKTREE are optional overrides.

Examples:
  (beads-buffer-name-utility \"stats\")
    => \"*beads-stats: myproject*\"
  (beads-buffer-name-utility \"dep-tree\" \"bd-42\")
    => \"*beads-dep-tree: myproject/bd-42*\""
  (let ((prefix (beads-buffer-name--project-prefix project worktree)))
    (if (and suffix (not (string-empty-p suffix)))
        (format "*beads-%s: %s/%s*" type prefix suffix)
      (format "*beads-%s: %s*" type prefix))))

;;; Parse Functions

(defun beads-buffer-name-parse-list (buffer-name)
  "Parse BUFFER-NAME as a list buffer name.
Returns plist with :type, :project, :worktree, :filter, or nil."
  (when (string-match
         "\\`\\*beads-\\(list\\|ready\\|blocked\\): \\([^@* ]+\\)\\(?:@\\([^* ]+\\)\\)?\\(?: \\(.+\\)\\)?\\*\\'"
         buffer-name)
    (list :type (match-string 1 buffer-name)
          :project (match-string 2 buffer-name)
          :worktree (match-string 3 buffer-name)
          :filter (match-string 4 buffer-name))))

(defun beads-buffer-name-parse-show (buffer-name)
  "Parse BUFFER-NAME as a show buffer name.
Returns plist with :project, :worktree, :issue-id, :title, or nil."
  (when (string-match
         "\\`\\*beads-show: \\([^@/]+\\)\\(?:@\\([^/]+\\)\\)?/\\([^ *]+\\)\\(?: \\(.+\\)\\)?\\*\\'"
         buffer-name)
    (list :project (match-string 1 buffer-name)
          :worktree (match-string 2 buffer-name)
          :issue-id (match-string 3 buffer-name)
          :title (match-string 4 buffer-name))))

(defun beads-buffer-name-parse-agent (buffer-name)
  "Parse BUFFER-NAME as an agent buffer name.
Returns plist with :project, :worktree, :backend, :instance,
:issue-id, :title.  Returns nil if not an agent buffer."
  (when (string-match
         (concat "\\`\\*beads-agent: "
                 "\\([^@/]+\\)"               ; project
                 "\\(?:@\\([^/]+\\)\\)?"      ; @worktree
                 "/\\([^#]+\\)"               ; /backend
                 "#\\([0-9]+\\)"              ; #N
                 "\\(?: \\([^ *]+\\)\\)?"     ; issue-id
                 "\\(?: \\(.+\\)\\)?"         ; title
                 "\\*\\'")
         buffer-name)
    (list :project (match-string 1 buffer-name)
          :worktree (match-string 2 buffer-name)
          :backend (match-string 3 buffer-name)
          :instance (string-to-number (match-string 4 buffer-name))
          :issue-id (match-string 5 buffer-name)
          :title (match-string 6 buffer-name))))

(defun beads-buffer-name-parse-utility (buffer-name)
  "Parse BUFFER-NAME as a utility buffer name.
Returns plist with :type, :project, :worktree, :suffix, or nil."
  (when (string-match
         (concat "\\`\\*beads-\\([a-z-]+\\): "
                 "\\([^@/*]+\\)"              ; project
                 "\\(?:@\\([^/*]+\\)\\)?"     ; @worktree
                 "\\(?:/\\([^*]+\\)\\)?"      ; /suffix
                 "\\*\\'")
         buffer-name)
    (let ((type (match-string 1 buffer-name)))
      ;; Exclude known non-utility types
      (unless (member type '("list" "ready" "blocked" "show" "agent"))
        (list :type type
              :project (match-string 2 buffer-name)
              :worktree (match-string 3 buffer-name)
              :suffix (match-string 4 buffer-name))))))

;;; Predicates

(defun beads-buffer-name-list-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads list buffer."
  (not (null (beads-buffer-name-parse-list buffer-name))))

(defun beads-buffer-name-show-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads show buffer."
  (not (null (beads-buffer-name-parse-show buffer-name))))

(defun beads-buffer-name-agent-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads agent buffer."
  (not (null (beads-buffer-name-parse-agent buffer-name))))

(defun beads-buffer-name-utility-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a beads utility buffer."
  (not (null (beads-buffer-name-parse-utility buffer-name))))

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

(provide 'beads-buffer-name)
;;; beads-buffer-name.el ends here
