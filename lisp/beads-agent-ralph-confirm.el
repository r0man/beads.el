;;; beads-agent-ralph-confirm.el --- Confirm-start dialog + dry-run buffer for Ralph -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module wraps Ralph's spawn entry point with a confirm-start
;; dialog and a dry-run buffer:
;;
;;   - `beads-agent-ralph--read-confirm-choice' surfaces a
;;     `read-char-choice' [y]/[n]/[d]ry-run prompt summarising the
;;     loop bindings (iterations, cost cap, permission mode, worktree).
;;   - `beads-agent-ralph--show-dry-run-buffer' opens a buffer with
;;     two editable panes (prompt + argv) and an action bar so the
;;     user can review the spawn before committing.
;;   - `beads-agent-ralph-launch' is the new public entry: it runs
;;     the dialog when `beads-agent-ralph-confirm-start' is non-nil and
;;     then delegates to `beads-agent-ralph-start'.
;;
;; The dry-run buffer persists prompt edits to
;; `beads-agent-ralph-prompt-file' so the user's overrides survive
;; across runs for a given root id.  An argv-overrides defvar
;; captures per-root argv tweaks for the next spawn.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)

(require 'beads-agent-ralph)
(require 'beads-agent-ralph-stream)

;;; Customization

(defcustom beads-agent-ralph-confirm-start t
  "When non-nil, `beads-agent-ralph-launch' confirms before spawning.
The user is prompted with `[y]/[n]/[d]ry-run' showing a one-line
summary of iteration count, per-iter cost cap, permission mode, and
working directory.  Set to nil to skip the dialog in scripted launches."
  :type 'boolean
  :group 'beads-agent-ralph)

;;; Per-root argv overrides

(defvar beads-agent-ralph-argv-overrides nil
  "Alist mapping ROOT-ID -> list of extra argv strings.
Populated by the dry-run buffer's [Apply argv overrides] button.
Threaded into `beads-agent-ralph-launch' on the next spawn so the
overrides persist for that root for the duration of the Emacs
session.  Reset by `beads-agent-ralph--clear-argv-overrides'.")

(defun beads-agent-ralph--clear-argv-overrides (root-id)
  "Drop any argv overrides registered for ROOT-ID."
  (setq beads-agent-ralph-argv-overrides
        (assoc-delete-all root-id beads-agent-ralph-argv-overrides)))

(defun beads-agent-ralph--argv-overrides-for (root-id)
  "Return the argv-overrides list for ROOT-ID, or nil if none."
  (cdr (assoc root-id beads-agent-ralph-argv-overrides)))

;;; Argv preview
;;
;; The preview mirrors `beads-agent-ralph--stream-spawn' argv assembly
;; so the dry-run buffer shows what claude will actually see.  Kept in
;; sync manually: the test suite asserts both renderers agree on the
;; baseline flags.

(defun beads-agent-ralph--preview-argv (args)
  "Render the claude argv that would be spawned for ARGS.
ARGS is a plist mirroring the `beads-agent-ralph--stream-spawn'
contract: :prompt, :project-dir, :max-budget-usd, :max-turns,
:permission-mode, :mcp-config, :extra-args, :program.

Returns a list of strings ready for `(string-join ARGV \" \")' or
for `make-process :command'."
  (let* ((prompt (or (plist-get args :prompt) ""))
         (project-dir (or (plist-get args :project-dir) default-directory))
         (program (or (plist-get args :program) "claude"))
         (permission-mode (or (plist-get args :permission-mode)
                              beads-agent-ralph-permission-mode))
         (max-budget (plist-get args :max-budget-usd))
         (max-turns (plist-get args :max-turns))
         (mcp-config (plist-get args :mcp-config))
         (extra-args (plist-get args :extra-args)))
    (append
     (list program
           "--print"
           "--output-format" "stream-json"
           "--verbose"
           "--include-partial-messages"
           "--no-session-persistence"
           "--permission-mode" permission-mode
           "--add-dir" (expand-file-name project-dir))
     (when max-budget
       (list "--max-budget-usd" (format "%s" max-budget)))
     (when max-turns
       (list "--max-turns" (format "%s" max-turns)))
     (when mcp-config
       (list "--mcp-config" mcp-config))
     extra-args
     (list "--" prompt))))

(defun beads-agent-ralph--argv-to-shell-string (argv)
  "Format ARGV as a shell-quoted multi-line string for display.
Each token is `shell-quote-argument'-quoted and joined with line
continuations so the user can read which flag is which."
  (mapconcat #'shell-quote-argument argv " \\\n  "))

;;; Iter-1 prompt preview

(defun beads-agent-ralph--preview-iter1-prompt (root-id kind prompt-override)
  "Render the iter-1 prompt that would be sent for ROOT-ID.
KIND is `issue' or `epic'.  PROMPT-OVERRIDE, when non-nil, takes
precedence over `beads-agent-ralph-prompt-file' and
`beads-agent-ralph-prompt'.

Synthesises a stand-in controller and issue so the existing
`beads-agent-ralph--render-prompt' machinery produces the same output
as the first real iteration -- minus dynamic fields (VERIFY-TAIL,
PRIOR-FALSE-CLAIMS, GIT-DIFF-STAT) that are zero on iter 1 anyway."
  (let* ((controller (beads-agent-ralph--controller
                      :root-id root-id
                      :root-kind kind
                      :iteration 1
                      :max-iterations 50
                      :current-issue-id root-id
                      :prompt-template prompt-override
                      :status 'idle))
         (issue (beads-issue
                 :id root-id
                 :title "(preview)"
                 :description "(preview — actual issue text resolved at spawn)"
                 :status "open"
                 :acceptance-criteria "- [ ] (preview)")))
    (beads-agent-ralph--render-prompt controller issue nil)))

;;; Confirm-start dialog

(defun beads-agent-ralph--summary-line (args)
  "Render the one-line summary of ARGS shown beneath the confirm-start prompt."
  (let* ((max-iter (or (plist-get args :max-iterations)
                       beads-agent-ralph-max-iterations))
         (max-budget (or (plist-get args :max-budget-usd)
                         beads-agent-ralph-max-budget-usd-per-iter))
         (permission (or (plist-get args :permission-mode)
                         beads-agent-ralph-permission-mode))
         (project-dir (or (plist-get args :worktree-dir)
                          (plist-get args :project-dir)
                          default-directory))
         (budget-str (if (numberp max-budget)
                         (format "$%.2f/iter cap" max-budget)
                       "no /iter cap")))
    (format "%d iters · %s · %s · %s"
            max-iter budget-str permission
            (abbreviate-file-name (expand-file-name project-dir)))))

(defun beads-agent-ralph--header-line (root-id kind child-count)
  "Render the question line of the confirm-start prompt.
ROOT-ID is the bd id; KIND is `issue' or `epic'; CHILD-COUNT is the
number of open child issues when KIND is `epic', else nil."
  (cond
   ((and (eq kind 'epic) (numberp child-count))
    (format "Start Ralph on %s (epic, %d open children)?"
            root-id child-count))
   ((eq kind 'epic)
    (format "Start Ralph on %s (epic)?" root-id))
   (t
    (format "Start Ralph on %s?" root-id))))

(defun beads-agent-ralph--read-confirm-choice (root-id kind child-count args)
  "Run the [y]/[n]/[d]ry-run prompt for ROOT-ID and return the choice.
Returns one of the symbols `spawn', `cancel', or `dry-run'.

KIND is `issue' or `epic'; CHILD-COUNT is shown for epics; ARGS is the
plist forwarded to the summary line so the user sees max-iterations,
budget, permission, and working dir before answering.

Implemented with `read-char-choice' so the prompt stays one line in
the minibuffer; a tab-completing read-string would be heavier than the
choice deserves."
  (let* ((header (beads-agent-ralph--header-line root-id kind child-count))
         (summary (beads-agent-ralph--summary-line args))
         ;; Two-line prompt: header on top, summary + answer hint below.
         (prompt (format "%s\n  %s  [y]/[n]/[d]ry-run: "
                         header summary))
         (choice (read-char-choice prompt '(?y ?n ?d))))
    (pcase choice
      (?y 'spawn)
      (?n 'cancel)
      (?d 'dry-run))))

;;; Dry-run buffer

(defconst beads-agent-ralph--dry-run-buffer-name
  "*beads-agent-ralph-dry-run*"
  "Buffer name for the dry-run preview.")

(defvar-local beads-agent-ralph--dry-run-args nil
  "Plist forwarded to `beads-agent-ralph-start' when the user confirms.
Set when the dry-run buffer is created; read by the [Spawn now]
button and the prompt-edit-save callback so they share the same
context as the dialog that opened them.")

(defvar-local beads-agent-ralph--dry-run-prompt-begin nil
  "Buffer position where the prompt pane begins (after its banner).")

(defvar-local beads-agent-ralph--dry-run-prompt-end nil
  "Buffer position where the prompt pane ends.")

(defvar-local beads-agent-ralph--dry-run-argv-begin nil
  "Buffer position where the argv pane begins (after its banner).")

(defvar-local beads-agent-ralph--dry-run-argv-end nil
  "Buffer position where the argv pane ends.")

(defun beads-agent-ralph--prompt-file-for (root-id)
  "Return the effective `beads-agent-ralph-prompt-file' for ROOT-ID.
Substitutes `<ROOT-ID>' if present; falls back to the in-tree default
path under `.beads/scratch/ralph/<root>.prompt.md' when the defcustom
is nil so the dry-run buffer always has a target for save."
  (let ((tpl (or beads-agent-ralph-prompt-file
                 ".beads/scratch/ralph/<ROOT-ID>.prompt.md")))
    (replace-regexp-in-string
     (regexp-quote "<ROOT-ID>") (or root-id "") tpl t t)))

(defvar beads-agent-ralph-dry-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-agent-ralph-dry-run-spawn)
    (define-key map (kbd "C-c C-k") #'beads-agent-ralph-dry-run-cancel)
    (define-key map (kbd "e")       #'beads-agent-ralph-dry-run-edit-prompt)
    map)
  "Keymap for `beads-agent-ralph-dry-run-mode'.")

(define-derived-mode beads-agent-ralph-dry-run-mode special-mode
  "Ralph-DryRun"
  "Major mode for the Ralph dry-run preview buffer.
Two panes -- the iter-1 prompt and the resolved claude argv -- are
displayed read-only at first; the user can review what would be sent
and either spawn (\\[beads-agent-ralph-dry-run-spawn]), pop the
prompt into an editable buffer (e), or cancel
\\[beads-agent-ralph-dry-run-cancel])."
  (setq-local truncate-lines nil)
  (setq-local revert-buffer-function
              (lambda (&rest _) (beads-agent-ralph--dry-run-refresh))))

(defun beads-agent-ralph--dry-run-insert-banner (text)
  "Insert TEXT as a section banner with face `mode-line-emphasis'."
  (insert (propertize text 'face 'mode-line-emphasis))
  (insert "\n"))

(defun beads-agent-ralph--dry-run-render-buffer (args)
  "Render the dry-run buffer's contents from ARGS, in-place.
Assumes the buffer is current and writable."
  (let* ((inhibit-read-only t)
         (root-id (plist-get args :root-id))
         (kind (or (plist-get args :kind) 'issue))
         (prompt-override (plist-get args :prompt))
         (prompt-file (beads-agent-ralph--prompt-file-for root-id))
         (prompt-text (beads-agent-ralph--preview-iter1-prompt
                       root-id kind prompt-override))
         (argv (beads-agent-ralph--preview-argv
                (plist-put (copy-sequence args) :prompt prompt-text))))
    (erase-buffer)
    (beads-agent-ralph--dry-run-insert-banner
     (format "Ralph dry-run for %s (%s)" root-id kind))
    (insert "\n"
            (propertize
             (format
              "Press C-c C-c (or the [Spawn now] button) to start the loop.
Press e to edit the prompt; saves to %s.
Press C-c C-k to cancel without spawning.\n\n" prompt-file)
             'face 'shadow))
    (insert-button "[Spawn now]"
                   'action (lambda (_b) (beads-agent-ralph-dry-run-spawn))
                   'follow-link t)
    (insert "  ")
    (insert-button "[Apply argv overrides]"
                   'action
                   (lambda (_b) (beads-agent-ralph-dry-run-apply-argv-overrides))
                   'follow-link t)
    (insert "  ")
    (insert-button "[Cancel]"
                   'action (lambda (_b) (beads-agent-ralph-dry-run-cancel))
                   'follow-link t)
    (insert "\n\n")
    (beads-agent-ralph--dry-run-insert-banner "PROMPT (iter 1)")
    (setq beads-agent-ralph--dry-run-prompt-begin (point))
    (insert prompt-text)
    (unless (bolp) (insert "\n"))
    (setq beads-agent-ralph--dry-run-prompt-end (point))
    (insert "\n")
    (beads-agent-ralph--dry-run-insert-banner "ARGV (claude command line)")
    (setq beads-agent-ralph--dry-run-argv-begin (point))
    (insert (beads-agent-ralph--argv-to-shell-string argv) "\n")
    (setq beads-agent-ralph--dry-run-argv-end (point))
    (insert (propertize
             (format
              "\nEdits to the prompt persist to %s; delete that file to revert to the defcustom default.\n"
              prompt-file)
             'face 'shadow))
    (goto-char (point-min))))

(defun beads-agent-ralph--dry-run-refresh ()
  "Re-render the current dry-run buffer from its stored ARGS."
  (when beads-agent-ralph--dry-run-args
    (beads-agent-ralph--dry-run-render-buffer
     beads-agent-ralph--dry-run-args)))

(defun beads-agent-ralph--show-dry-run-buffer (args)
  "Pop the dry-run preview for ARGS and return its buffer.
ARGS is the plist of `beads-agent-ralph-start' arguments.  The buffer
is created (or reset if it already exists) and displayed; the caller
returns control to the user without spawning."
  (let ((buf (get-buffer-create beads-agent-ralph--dry-run-buffer-name)))
    (with-current-buffer buf
      (beads-agent-ralph-dry-run-mode)
      (setq beads-agent-ralph--dry-run-args (copy-sequence args))
      (beads-agent-ralph--dry-run-render-buffer args))
    (pop-to-buffer buf)
    buf))

;;; Dry-run actions

(defun beads-agent-ralph-dry-run-spawn ()
  "Spawn the loop using the saved dry-run ARGS, then bury the buffer."
  (interactive)
  (let ((args (and (boundp 'beads-agent-ralph--dry-run-args)
                   beads-agent-ralph--dry-run-args)))
    (unless args
      (user-error "No dry-run state in this buffer"))
    (let ((overrides
           (beads-agent-ralph--argv-overrides-for
            (plist-get args :root-id))))
      (when overrides
        (setq args (plist-put (copy-sequence args) :extra-args overrides))))
    (let ((buf (current-buffer)))
      (quit-window nil (get-buffer-window buf))
      (apply #'beads-agent-ralph-start args))))

(defun beads-agent-ralph-dry-run-cancel ()
  "Cancel the dry-run and bury the buffer without spawning."
  (interactive)
  (message "beads-agent-ralph: dry-run cancelled")
  (quit-window))

(defun beads-agent-ralph-dry-run-edit-prompt ()
  "Pop the prompt pane into an editable buffer; save writes to the prompt file."
  (interactive)
  (let* ((args beads-agent-ralph--dry-run-args)
         (root-id (plist-get args :root-id))
         (path (beads-agent-ralph--prompt-file-for root-id))
         (current-text
          (when (and beads-agent-ralph--dry-run-prompt-begin
                     beads-agent-ralph--dry-run-prompt-end)
            (buffer-substring-no-properties
             beads-agent-ralph--dry-run-prompt-begin
             beads-agent-ralph--dry-run-prompt-end)))
         (parent-buf (current-buffer)))
    (let ((edit-buf (find-file-noselect path)))
      (with-current-buffer edit-buf
        (when (and current-text (zerop (buffer-size)))
          (insert current-text))
        (setq-local beads-agent-ralph--dry-run-parent-buffer parent-buf)
        (add-hook 'after-save-hook
                  #'beads-agent-ralph--prompt-edit-after-save nil t))
      (pop-to-buffer edit-buf))))

(defvar-local beads-agent-ralph--dry-run-parent-buffer nil
  "Dry-run buffer to refresh after the prompt edit buffer is saved.")

(defun beads-agent-ralph--prompt-edit-after-save ()
  "Refresh the parent dry-run buffer after the prompt file is saved."
  (when (and (boundp 'beads-agent-ralph--dry-run-parent-buffer)
             beads-agent-ralph--dry-run-parent-buffer
             (buffer-live-p beads-agent-ralph--dry-run-parent-buffer))
    (with-current-buffer beads-agent-ralph--dry-run-parent-buffer
      (beads-agent-ralph--dry-run-refresh))))

(defun beads-agent-ralph-dry-run-apply-argv-overrides ()
  "Capture argv pane edits as per-root extra-args overrides."
  (interactive)
  (let* ((args beads-agent-ralph--dry-run-args)
         (root-id (plist-get args :root-id))
         (current
          (when (and beads-agent-ralph--dry-run-argv-begin
                     beads-agent-ralph--dry-run-argv-end)
            (buffer-substring-no-properties
             beads-agent-ralph--dry-run-argv-begin
             beads-agent-ralph--dry-run-argv-end)))
         (baseline-argv (beads-agent-ralph--preview-argv args))
         (baseline-text (beads-agent-ralph--argv-to-shell-string
                         baseline-argv))
         (overrides
          ;; Trim trailing whitespace so the trailing "\n" the buffer
          ;; carries doesn't count as a difference.
          (when (and current
                     (not (equal (string-trim current)
                                 (string-trim baseline-text))))
            (split-string-shell-command (string-trim current)))))
    (cond
     ((null overrides)
      (beads-agent-ralph--clear-argv-overrides root-id)
      (message "beads-agent-ralph: argv overrides cleared for %s" root-id))
     (t
      (setf (alist-get root-id beads-agent-ralph-argv-overrides nil nil #'equal)
            overrides)
      (message "beads-agent-ralph: argv overrides applied for %s (%d tokens)"
               root-id (length overrides))))))

;;; Public entry

;;;###autoload
(defun beads-agent-ralph-launch (&rest args)
  "Launch a Ralph loop with optional confirmation dialog.

ARGS is the plist forwarded to `beads-agent-ralph-start' on spawn.
Additional keys consumed by the dialog itself:

  :child-count INTEGER — number of open children for an epic root,
                shown in the prompt summary.

When `beads-agent-ralph-confirm-start' is non-nil and Emacs is
interactive, prompts the user for [y]/[n]/[d]ry-run.  Behaviour:

  y → spawn now via `beads-agent-ralph-start' and return its result.
  n → cancel; return nil.
  d → pop *beads-agent-ralph-dry-run* and return the buffer.  The
      user spawns later from the buffer's [Spawn now] button.

When confirm-start is nil OR Emacs is non-interactive (e.g. tests),
spawns directly without a prompt -- the confirmation is opt-in UX,
not a safety guard."
  (let* ((root-id (or (plist-get args :issue)
                      (error "beads-agent-ralph-launch: :issue required")))
         (root-id (cond ((stringp root-id) root-id)
                        ((and (eieio-object-p root-id)
                              (beads-issue-p root-id))
                         (oref root-id id))
                        (t (error "Bad :issue type"))))
         (kind (or (plist-get args :kind) 'issue))
         (child-count (plist-get args :child-count))
         (start-args (copy-sequence args)))
    (cl-remf start-args :child-count)
    (setq start-args (plist-put start-args :issue root-id))
    (cond
     ((not (and beads-agent-ralph-confirm-start
                (not noninteractive)))
      (apply #'beads-agent-ralph-start start-args))
     (t
      (pcase (beads-agent-ralph--read-confirm-choice
              root-id kind child-count start-args)
        ('spawn (apply #'beads-agent-ralph-start start-args))
        ('cancel
         (message "beads-agent-ralph: launch cancelled by user")
         nil)
        ('dry-run
         (beads-agent-ralph--show-dry-run-buffer
          (plist-put start-args :root-id root-id))))))))

(provide 'beads-agent-ralph-confirm)

;;; beads-agent-ralph-confirm.el ends here
