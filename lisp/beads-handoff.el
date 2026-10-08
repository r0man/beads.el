;;; beads-handoff.el --- Agent hand-off and operational context -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module adds three user-visible surfaces around the work loop
;; (WI-SF-11):
;;
;; - `beads-handoff-agent' / `beads-handoff' — hand a molecule, formula
;;   or issue off to an AI agent.  The work is described by a
;;   `beads-handoff' work plist and rendered into the issue-envelope
;;   user prompt by `beads-handoff-envelope' (a seam gascity may
;;   override).  The envelope bridges to the 4-arity
;;   `beads-agent-backend-start' so the launch reuses the existing
;;   backend registry and terminal subsystem.
;;
;; - `beads-context' — the sectioned operational-context view: `bd
;;   prime', persistent memories, `bd setup' recipe status and the
;;   active `agent.profile' policy.  Its buffer also carries the
;;   `bd context' identity/repo header.
;;
;; - `beads-context-prime' and `beads-setup-status' — focused
;;   sub-views of the same data.
;;
;; The command class for `bd prime' lives in `beads-command-prime.el'
;; (split out of `beads-command-misc.el' in the same work item); the
;; `bd context' transient is named `beads-bd-context' because
;; `beads-context' is this view.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-command)
(require 'beads-command-misc)
(require 'beads-command-prime)
(require 'beads-buffer)
(require 'beads-meta)
(require 'beads-util)
(require 'beads-agent-backend)
(require 'beads-agent-type)
(require 'beads-agent-types)
(require 'beads-prefix)
(require 'transient)

(declare-function beads-reader-agent-backend "beads-reader")
(declare-function beads-reader-issue-id "beads-reader")
(declare-function beads-command-config-get "beads-command-config")
(require 'beads-command-config)

;;; ============================================================
;;; Hand-off work objects
;;; ============================================================

(defconst beads-handoff-work-kinds '(issue molecule formula)
  "Recognized WORK kinds handed to `beads-handoff-envelope'.")

(defun beads-handoff--make-work (kind id &optional title issue)
  "Build a hand-off WORK plist.
KIND is a symbol from `beads-handoff-work-kinds', ID names the work,
TITLE is optional display text and ISSUE, when non-nil, is the
`beads-issue' object the agent will work."
  (list :kind kind :id id :title title :issue issue))

(defun beads-handoff-issue (issue)
  "Return a hand-off WORK plist for the `beads-issue' ISSUE."
  (beads-handoff--make-work 'issue (oref issue id) (oref issue title) issue))

(defun beads-handoff-molecule (id &optional title)
  "Return a hand-off WORK plist for molecule ID with optional TITLE."
  (beads-handoff--make-work 'molecule id title nil))

(defun beads-handoff-formula (name &optional title)
  "Return a hand-off WORK plist for formula NAME with optional TITLE."
  (beads-handoff--make-work 'formula name title nil))

(defun beads-handoff--title-suffix (title)
  "Return \": TITLE\" for a non-empty TITLE, otherwise the empty string."
  (if (and title (not (string-empty-p title)))
      (format ": %s" title)
    ""))

(defun beads-handoff--envelope-text (work)
  "Return the issue-envelope text describing WORK.
WORK is a hand-off work plist; the text tells the agent what it was
handed and the exact `bd' commands that continue the work."
  (let* ((kind (plist-get work :kind))
         (id (or (plist-get work :id) ""))
         (title (plist-get work :title))
         (heading
          (pcase kind
            ('molecule (format "Molecule %s%s." id
                               (beads-handoff--title-suffix title)))
            ('formula (format "Formula %s%s." id
                              (beads-handoff--title-suffix title)))
            (_ (format "Issue %s%s." id
                       (beads-handoff--title-suffix title)))))
         (guidance
          (pcase kind
            ('molecule
             (format "Use `bd mol current %s' and `bd ready --mol %s' to find the next ready step; close each step with `bd close <id> --reason ...'."
                     id id))
            ('formula
             (format "Instantiate it with `bd mol pour %s --var ...', then work the resulting molecule." id))
            (_
             (format "Inspect it with `bd show %s' and close it with `bd close %s --reason ...'."
                     id id)))))
    (concat heading "\n" guidance)))

(defvar beads-handoff--context nil
  "Prime/memories excerpt injected into the hand-off user prompt.
Bound dynamically by `beads-handoff-start' from its CONTEXT argument.")

(defun beads-handoff--user-prompt (work user-prompt)
  "Build the hand-off user prompt for WORK and USER-PROMPT.
The issue envelope always comes first; a non-empty USER-PROMPT (the
agent type's task template) and the optional context excerpt follow."
  (let ((parts (list (beads-handoff--envelope-text work))))
    (when (and user-prompt (not (string-empty-p user-prompt)))
      (push user-prompt parts))
    (when (and beads-handoff--context
               (not (string-empty-p beads-handoff--context)))
      (push (format "Context:\n%s" beads-handoff--context) parts))
    (mapconcat #'identity (nreverse parts) "\n\n")))

(cl-defgeneric beads-handoff-envelope (work system-prompt user-prompt)
  "Return the `beads-agent-backend-start' start-args for WORK.
The value is the triple (ISSUE SYSTEM-PROMPT USER-PROMPT) accepted
by the 4-arity `beads-agent-backend-start'.  WORK is a hand-off work
plist, SYSTEM-PROMPT the role text (or nil) and USER-PROMPT the
agent type's task template (or nil).  Extensions (gascity) may
override this to enrich the envelope.")

(cl-defmethod beads-handoff-envelope (work system-prompt user-prompt)
  "Return (ISSUE SYSTEM-PROMPT USER-PROMPT) for WORK."
  (list (plist-get work :issue)
        system-prompt
        (beads-handoff--user-prompt work user-prompt)))

;;; ============================================================
;;; Hand-off start
;;; ============================================================

(defun beads-handoff--resolve-backend (name)
  "Return the registered backend named NAME.
Signals `user-error' when NAME is nil or unknown."
  (or (and name (beads-agent--get-backend name))
      (user-error "No AI agent backend named %s%s"
                  (or name "nil")
                  (if (and name (not (beads-agent--get-backend name)))
                      " (is it registered? see `beads-agent--register-backend')"
                    ""))))

(defun beads-handoff--resolve-type (name)
  "Return the agent type named NAME, defaulting to \"Task\"."
  (or (and name (beads-agent-type-get name))
      (beads-agent-type-get "Task")
      (user-error "No agent type registered (asked for %s)"
                  (or name "Task"))))

(defun beads-handoff-start (work backend-name &optional agent-type-name context)
  "Hand WORK off to an agent through the BACKEND-NAME backend.
AGENT-TYPE-NAME names the registered agent type (default \"Task\").
CONTEXT, when non-nil, is a prime/memories excerpt appended to the
user prompt.  Returns the (BACKEND-SESSION . BUFFER) cons from
`beads-agent-backend-start'; the caller owns buffer display."
  (let* ((backend (beads-handoff--resolve-backend backend-name))
         (type (beads-handoff--resolve-type agent-type-name))
         (issue (plist-get work :issue))
         (system-prompt (beads-agent-type-system-prompt type issue))
         (user-prompt (beads-agent-type-build-user-prompt type issue))
         (beads-handoff--context context)
         (args (beads-handoff-envelope work system-prompt user-prompt))
         (result (apply #'beads-agent-backend-start backend args)))
    (message "Handed %s %s off to %s"
             (plist-get work :kind) (plist-get work :id) backend-name)
    result))

;;; ============================================================
;;; Hand-off transient
;;; ============================================================

(defvar beads-handoff--work nil
  "Hand-off work plist selected in the `beads-handoff' menu.")

(defvar beads-handoff--backend nil
  "Backend name selected in the `beads-handoff' menu.")

(defvar beads-handoff--type-name "Task"
  "Agent type name selected in the `beads-handoff' menu.")

(defvar beads-handoff--worktree nil
  "Working directory override for the hand-off, or nil.")

(defvar beads-handoff--context-mode 'none
  "Context excerpt mode for the hand-off: `none', `prime' or `memories'.")

(defun beads-handoff--context-excerpt ()
  "Return the excerpt selected by `beads-handoff--context-mode'."
  (pcase beads-handoff--context-mode
    ('prime (beads-context--prime-text t))
    ('memories (beads-context--memories-text))
    (_ nil)))

(defun beads-handoff--format-header ()
  "Format the `beads-handoff' transient header."
  (format "Hand off%s                    (s start · q quit)"
          (if beads-handoff--work
              (format " — %s %s"
                      (plist-get beads-handoff--work :kind)
                      (plist-get beads-handoff--work :id))
            "")))

(defun beads-handoff--read-work ()
  "Read a `beads-issue' and return its hand-off work plist."
  (require 'beads-reader)
  (let* ((id (beads-reader-issue-id "Hand off issue: " nil nil))
         (data (beads-execute 'beads-command-show :issue-ids (list id)))
         (issue (cond ((vectorp data) (aref data 0))
                      ((listp data) (car data))
                      (t data))))
    (unless (and issue (object-of-class-p issue 'beads-issue))
      (user-error "Could not load issue %s for hand-off" id))
    (beads-handoff-issue issue)))

(transient-define-suffix beads-handoff--set-work ()
  "Choose the issue to hand off."
  :key "i"
  :description "Issue to hand off"
  (interactive)
  (setq beads-handoff--work (beads-handoff--read-work))
  (transient--redisplay))

(transient-define-suffix beads-handoff--select-backend ()
  "Choose the agent backend."
  :key "b"
  :description (lambda () (format "Backend: %s" (or beads-handoff--backend "auto")))
  (interactive)
  (require 'beads-reader)
  (setq beads-handoff--backend (beads-reader-agent-backend nil nil nil))
  (transient--redisplay))

(defun beads-handoff--cycle-type ()
  "Cycle `beads-handoff--type-name' through the registered types."
  (let* ((names (beads-agent-type-names))
         (current (cl-position (downcase beads-handoff--type-name) names
                               :test #'equal))
         (next (if current
                   (nth (mod (1+ current) (length names)) names)
                 (car names))))
    (setq beads-handoff--type-name (or next "Task"))))

(transient-define-suffix beads-handoff--cycle-type-suffix ()
  "Cycle the agent type (Task → Review → Plan → ...)."
  :key "t"
  :description (lambda () (format "Agent type: %s" beads-handoff--type-name))
  (interactive)
  (beads-handoff--cycle-type)
  (transient--redisplay))

(transient-define-suffix beads-handoff--toggle-worktree ()
  "Toggle worktree isolation by prompting for a directory."
  :key "w"
  :description (lambda ()
                 (format "Worktree: %s"
                         (or beads-handoff--worktree "derived/none")))
  (interactive)
  (setq beads-handoff--worktree
        (read-directory-name "Worktree directory (empty for none): " nil nil t))
  (when (string-empty-p beads-handoff--worktree)
    (setq beads-handoff--worktree nil))
  (transient--redisplay))

(transient-define-suffix beads-handoff--toggle-context ()
  "Toggle the context excerpt appended to the user prompt."
  :key "c"
  :description (lambda () (format "Context: %s" beads-handoff--context-mode))
  (interactive)
  (setq beads-handoff--context-mode
        (pcase beads-handoff--context-mode
          ('none 'prime)
          ('prime 'memories)
          (_ 'none)))
  (transient--redisplay))

(transient-define-suffix beads-handoff--preview ()
  "Preview the issue-envelope user prompt."
  :key "U"
  :description "Preview user prompt"
  (interactive)
  (unless beads-handoff--work
    (user-error "Choose an issue first (i)"))
  (let* ((type (beads-handoff--resolve-type beads-handoff--type-name))
         (issue (plist-get beads-handoff--work :issue))
         (envelope (beads-handoff--user-prompt
                    beads-handoff--work
                    (beads-agent-type-build-user-prompt type issue))))
    (with-current-buffer (get-buffer-create "*beads-handoff-preview*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert envelope)
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(transient-define-suffix beads-handoff--start ()
  "Start the agent for the selected work."
  :key "s"
  :description "Start agent"
  (interactive)
  (unless beads-handoff--work
    (setq beads-handoff--work (beads-handoff--read-work)))
  (let ((backend (or beads-handoff--backend
                     (progn (require 'beads-reader)
                            (setq beads-handoff--backend
                                  (beads-reader-agent-backend nil nil nil)))))
        (default-directory (or (and beads-handoff--worktree
                                    (file-directory-p beads-handoff--worktree)
                                    beads-handoff--worktree)
                               default-directory)))
    (beads-handoff-start beads-handoff--work backend beads-handoff--type-name
                         (beads-handoff--context-excerpt))))

;;;###autoload (autoload 'beads-handoff "beads-handoff" nil t)
(beads-define-prefix beads-handoff (&optional work)
  "Hand work off to an AI agent.

Choose the issue (i), backend (b), agent type (t), optional worktree
(w) and context excerpt (c), preview the envelope (U), then start (s)."
  ["Hand-off"
   (beads-handoff--set-work)
   (beads-handoff--select-backend)
   (beads-handoff--cycle-type-suffix)
   (beads-handoff--toggle-worktree)
   (beads-handoff--toggle-context)]
  ["Prompt"
   (beads-handoff--preview)
   (beads-handoff--start)]
  ["Other"
   ("q" "Quit" transient-quit-one)]
  (interactive (list nil))
  (setq beads-handoff--work work)
  (transient-setup 'beads-handoff))

;;;###autoload
(defun beads-handoff-agent (&optional work backend-name agent-type-name)
  "Hand WORK off to an AI agent through BACKEND-NAME.
WORK defaults to the issue at point (or a prompted issue),
BACKEND-NAME to the user's chosen registered backend and
AGENT-TYPE-NAME to \"Task\".  Interactively this opens the
`beads-handoff' menu."
  (interactive)
  (if (called-interactively-p 'any)
      (beads-handoff work)
    (let* ((work (or work (beads-handoff--read-work)))
           (backend (or backend-name
                        (progn (require 'beads-reader)
                               (beads-reader-agent-backend nil nil nil)))))
      (beads-handoff-start work backend agent-type-name))))

;;; ============================================================
;;; Operational context view
;;; ============================================================

(defconst beads-context--prime-full-args '(:full t)
  "`bd prime' flags used for the Prime section.")

(defun beads-context--directory-args ()
  "Return a `:directory' initarg list for the current store, if any.
The view is store-scoped: a buffer-local `beads-store-directory'
becomes `--directory', matching every other view."
  (when (and (boundp 'beads-store-directory) beads-store-directory)
    (list :directory (directory-file-name beads-store-directory))))

(defun beads-context--execute (class &rest args)
  "Construct CLASS with ARGS (plus the store directory) and execute it."
  (beads-command-execute
   (apply class (append (beads-context--directory-args) args))))

(defun beads-context--prime-text (&optional full)
  "Return the `bd prime' text.  FULL forces the full CLI output."
  (beads-context--execute 'beads-command-prime
                          :full (if full t nil)))

(defun beads-context--memories-text ()
  "Return the `bd prime --memories-only' text."
  (beads-context--execute 'beads-command-prime :memories-only t))

(defun beads-context--setup-list ()
  "Return the `bd setup --list' text."
  (beads-context--execute 'beads-command-setup :list t))

(defun beads-context--setup-check (recipe)
  "Return the `bd setup RECIPE --check' text."
  (beads-context--execute 'beads-command-setup :editor recipe :check t))

(defun beads-context--policy ()
  "Return the active agent policy string.
`BD_AGENT_PROFILE' wins over the `agent.profile' config key; the
default is \"conservative\"."
  (or (let ((env (getenv "BD_AGENT_PROFILE")))
        (and env (not (string-empty-p env)) env))
      (or (ignore-errors
            (let ((data (beads-context--execute 'beads-command-config-get
                                                :key "agent.profile")))
              (alist-get 'value data)))
          "conservative")))

(defun beads-context--setup-recipe-names (list-output)
  "Extract recipe names from `bd setup --list' LIST-OUTPUT.
Returns the names in display order; an absent or unparsable list
yields nil."
  (let (names)
    (dolist (line (split-string (or list-output "") "\n"))
      (when (string-match "\\`[[:space:]]+\\([a-z][a-z0-9-]*\\)[[:space:]]+\\S-" line)
        (push (match-string 1 line) names)))
    (nreverse names)))

(defun beads-context--setup-status-line (check-output)
  "Classify `bd setup RECIPE --check' CHECK-OUTPUT.
Returns \"installed\", \"stale\", \"missing\" or the first non-empty
output line when it cannot be classified."
  (let ((text (or check-output "")))
    (cond
     ((string-match-p "\\b\\(stale\\|hash mismatch\\)\\b" text) "stale")
     ((string-match-p "✗\\|\\bnot installed\\b\\|\\bmissing\\b" text) "missing")
     ((string-match-p "✓\\|\\binstalled\\b\\|\\bcurrent\\b" text) "installed")
     (t (or (car (seq-filter (lambda (l) (not (string-empty-p l)))
                             (split-string text "\n")))
            "unknown")))))

(defun beads-context--collect ()
  "Collect the operational-context data as a plist.
Each fetch goes through `beads-command-execute' so the view can be
unit-tested with a mocked executor."
  (let* ((setup-list (beads-context--setup-list))
         (recipes (beads-context--setup-recipe-names setup-list))
         (checks (mapcar (lambda (recipe)
                           (cons recipe
                                 (condition-case err
                                     (beads-context--setup-check recipe)
                                   (error (error-message-string err)))))
                         recipes)))
    (list :prime (beads-context--prime-text)
          :memories (beads-context--memories-text)
          :setup-list setup-list
          :setup-checks checks
          :policy (beads-context--policy))))

(defun beads-context--format-setup (data)
  "Format the setup section of the context DATA plist."
  (let ((checks (plist-get data :setup-checks)))
    (concat
     (or (plist-get data :setup-list) "Available recipes:\n")
     (when checks
       (concat "\nChecks\n"
               (mapconcat
                (lambda (cell)
                  (format "  %s: %s" (car cell)
                          (beads-context--setup-status-line (cdr cell))))
                checks "\n"))))))

(defun beads-context--format-header (data)
  "Format the header line of the context DATA plist."
  (format "Context — %s  (policy: %s)"
          (or (and (fboundp 'beads--project-name-for-root)
                   (ignore-errors (beads--project-name-for-root default-directory)))
              "beads")
          (or (plist-get data :policy) "conservative")))

(defun beads-context--format-policy (data)
  "Format the policy section of the context DATA plist."
  (format "agent.profile: %s\n(BD_AGENT_PROFILE overrides the config key; the view never writes policy.)"
          (or (plist-get data :policy) "conservative")))

(defun beads-context--insert-section (title content)
  "Insert a section TITLE and CONTENT into the current buffer."
  (insert (propertize (concat "▾ " title "\n")
                      'face 'bold
                      'beads-context-section title)
          (or content "(none)\n")
          "\n"))

(defvar-local beads-context--data nil
  "Last collected operational-context plist for the buffer.")

(defvar-local beads-context--directory nil
  "Store directory this context buffer is scoped to, or nil.")

(defun beads-context--render (data)
  "Render the operational-context DATA plist into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (beads-context--format-header data)
                        'face 'bold)
            "\n\n")
    (beads-context--insert-section "Prime (bd prime)" (plist-get data :prime))
    (beads-context--insert-section "Memories (bd prime --memories-only)"
                                   (plist-get data :memories))
    (beads-context--insert-section "Setup status" (beads-context--format-setup data))
    (beads-context--insert-section "Policy" (beads-context--format-policy data))
    (goto-char (point-min))
    (setq beads-context--data data)))

(defvar-keymap beads-context-mode-map
  :parent special-mode-map
  "g" #'beads-context-refresh
  "c" #'beads-context-copy-prime
  "q" #'quit-window)

(define-derived-mode beads-context-mode special-mode "Beads-Context"
  "Major mode for the `beads-context' operational-context view.

\\{beads-context-mode-map}"
  :interactive nil
  (setq-local revert-buffer-function #'beads-context--revert))

(defun beads-context--get-or-create-buffer (&optional directory)
  "Return the (reused) context buffer for DIRECTORY."
  (let* ((project (or (and directory
                           (ignore-errors
                             (beads--project-name-for-root directory)))
                      (ignore-errors
                        (beads--project-name-for-root default-directory))
                      "beads"))
         (name (format "*beads-context[%s]*" project))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-context-mode)
        (beads-context-mode))
      (when directory
        (setq default-directory (file-name-as-directory directory))
        (setq-local beads-store-directory directory))
      (setq beads-context--directory directory))
    buffer))

(defun beads-context--revert (&rest _)
  "Revert the context buffer by re-collecting its data."
  (beads-context--render (beads-context--collect)))

;;;###autoload
(defun beads-context (&optional directory)
  "Show the beads operational context.
Renders `bd prime', persistent memories, `bd setup' recipe status and
the active `agent.profile' policy in a sectioned buffer.  DIRECTORY,
when non-nil, scopes the reads to that store."
  (interactive)
  (let* ((directory (or directory
                        (and (boundp 'beads-store-directory)
                             beads-store-directory
                             (directory-file-name beads-store-directory))))
         (buffer (beads-context--get-or-create-buffer directory)))
    (with-current-buffer buffer
      (beads-context--render (beads-context--collect)))
    (pop-to-buffer buffer)))

(defun beads-context-refresh ()
  "Refresh the `beads-context' buffer in place."
  (interactive)
  (beads-context--render (beads-context--collect)))

(defun beads-context-copy-prime ()
  "Copy the buffer's `bd prime' text to the kill ring."
  (interactive)
  (let ((prime (plist-get beads-context--data :prime)))
    (unless prime
      (user-error "No prime text to copy"))
    (kill-new prime)
    (message "Copied bd prime context to the kill ring")))

;;;###autoload
(defun beads-context-prime ()
  "Show the `bd prime' context on its own."
  (interactive)
  (beads-context))

;;;###autoload
(defun beads-setup-status ()
  "Show `bd setup' recipe status and the active policy."
  (interactive)
  (beads-context))

(provide 'beads-handoff)
;;; beads-handoff.el ends here
