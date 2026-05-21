;;; beads-agent-prompt-edit.el --- Edit agent prompts before launch -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Two-region prompt editor (Phase 1a-ii, bde-xle9.2).  Every agent
;; start routes through `beads-agent-prompt-edit-show', which pops a
;; buffer with two editable regions separated by read-only marked
;; headings:
;;
;;   ## System prompt
;;   <editable role/identity>
;;
;;   ## User prompt
;;   <editable task>
;;
;; Both heading lines carry `read-only', `front-sticky',
;; `rear-nonsticky', and a unique `beads-prompt-section' text property
;; (`system-heading' / `user-heading').  Parsing uses
;; `text-property-search-forward' on that marker, NOT a regex on the
;; literal heading text — pasting Markdown that itself contains
;; "## User prompt" into a region does not desync the parser.
;;
;; Callback contract: confirm calls (funcall callback SYS USER) where
;; SYS is a string, or nil when the system region is blank ("use the
;; backend's built-in identity"); USER is the user-region string.
;; Cancel calls (funcall callback nil nil).  The (nil nil) pair is the
;; cancel sentinel; "no system override, real user" is (nil "text").

;;; Code:

(require 'beads-agent-type)
(require 'beads-buffer)
(require 'subr-x)

;;; Buffer-local Variables

(defvar-local beads-agent-prompt-edit--callback nil
  "Callback invoked as (SYS USER); (nil nil) is the cancel sentinel.")

(defvar-local beads-agent-prompt-edit--issue-id nil
  "Issue ID for the prompt being edited.")

(defvar-local beads-agent-prompt-edit--agent-type nil
  "Agent type name for display purposes.")

;;; Region markers

(defconst beads-agent-prompt-edit--system-heading "## System prompt"
  "Literal text of the system-region heading line.")

(defconst beads-agent-prompt-edit--user-heading "## User prompt"
  "Literal text of the user-region heading line.")

;;; Keymap

(defvar beads-agent-prompt-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-agent-prompt-edit-confirm)
    (define-key map (kbd "C-c C-k") #'beads-agent-prompt-edit-cancel)
    map)
  "Keymap for `beads-agent-prompt-edit-mode'.")

;;; Mode Definition

(define-derived-mode beads-agent-prompt-edit-mode text-mode "Beads-Prompt"
  "Major mode for editing agent prompts before launch.

\\<beads-agent-prompt-edit-mode-map>
Press \\[beads-agent-prompt-edit-confirm] to confirm and launch the agent.
Press \\[beads-agent-prompt-edit-cancel] to cancel without launching."
  :group 'beads-agent
  (setq-local header-line-format
              '(:eval (beads-agent-prompt-edit--header-line))))

(defun beads-agent-prompt-edit--header-line ()
  "Generate header line for prompt edit buffer.
Prefixes the agent type name with its role icon (or single-letter
fallback under TTY) when the type is registered, per
`beads-agent-type-icon-or-letter'."
  (let* ((type-name (or beads-agent-prompt-edit--agent-type "Agent"))
         (type (beads-agent-type-get type-name))
         (glyph (and type (beads-agent-type-icon-or-letter type)))
         (prefix (if (and glyph (not (string= glyph type-name)))
                     (concat glyph " ")
                   "")))
    (format " %s%s prompt for %s  |  C-c C-c: Confirm  |  C-c C-k: Cancel"
            prefix
            type-name
            (or beads-agent-prompt-edit--issue-id "issue"))))

;;; Buffer Management

(defun beads-agent-prompt-edit--buffer-name (issue-id)
  "Generate buffer name for prompt editing for ISSUE-ID."
  (beads-buffer-utility "prompt-edit" issue-id))

(defun beads-agent-prompt-edit--insert-heading (text section)
  "Insert a read-only heading line TEXT marked with SECTION.
SECTION is `system-heading' or `user-heading'.  The whole line plus
its trailing newline is read-only, `front-sticky' (so text typed at
its start cannot merge into it) and `rear-nonsticky' (so text typed
on the next line is editable)."
  (let ((start (point)))
    (insert text "\n")
    (add-text-properties
     start (point)
     (list 'read-only t
           'front-sticky t
           'rear-nonsticky t
           'beads-prompt-section section
           'face 'font-lock-keyword-face))))

(defun beads-agent-prompt-edit-show (issue-id system-prompt user-prompt
                                              agent-type-name callback)
  "Show the two-region prompt editor for ISSUE-ID.
SYSTEM-PROMPT is the initial role/identity text (string, or nil for
an empty system region).  USER-PROMPT is the initial task text.
AGENT-TYPE-NAME is shown in the header.  CALLBACK is invoked as
\(SYS USER): on confirm with SYS the edited system text or nil when
that region is blank, and USER the edited task; on cancel with both
nil (the (nil nil) cancel sentinel)."
  (let* ((buf-name (beads-agent-prompt-edit--buffer-name issue-id))
         (buf (get-buffer-create buf-name))
         ;; Capture the caller's cwd (worktree/project dir) so the
         ;; editor buffer resolves the right .beads at confirm time.
         (dir default-directory))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (beads-agent-prompt-edit-mode)
        (erase-buffer)
        (beads-agent-prompt-edit--insert-heading
         beads-agent-prompt-edit--system-heading 'system-heading)
        (insert (or system-prompt "") "\n\n")
        (beads-agent-prompt-edit--insert-heading
         beads-agent-prompt-edit--user-heading 'user-heading)
        (insert (or user-prompt "")))
      (goto-char (point-max))
      (setq-local default-directory dir)
      (setq beads-agent-prompt-edit--callback callback)
      (setq beads-agent-prompt-edit--issue-id issue-id)
      (setq beads-agent-prompt-edit--agent-type agent-type-name)
      (set-buffer-modified-p nil))
    (pop-to-buffer buf)
    (message "Edit prompt, then C-c C-c to confirm or C-c C-k to cancel")))

;;; Region extraction

(defun beads-agent-prompt-edit--region-bounds (section)
  "Return (START . END) of the editable region after SECTION's heading.
SECTION is `system-heading' or `user-heading'.  The region runs from
the character after that heading's newline up to (but excluding) the
next `beads-prompt-section' heading, or `point-max'.  Parsing is by
the text property marker, never the heading text, so a pasted
\"## User prompt\" inside a region does not desync."
  (save-excursion
    (goto-char (point-min))
    (let ((m (text-property-search-forward 'beads-prompt-section section t)))
      (unless m
        (error "Prompt-edit buffer corrupted: %s marker missing" section))
      (let* ((start (prop-match-end m))
             (next (save-excursion
                     (goto-char start)
                     (text-property-search-forward
                      'beads-prompt-section nil
                      (lambda (_ v) v))))
             (end (if next (prop-match-beginning next) (point-max))))
        (cons start end)))))

(defun beads-agent-prompt-edit--trim-blank-lines (s)
  "Strip leading and trailing blank lines from S, internals verbatim.
Returns the trimmed string (internal whitespace — fenced code
blocks, indented lists — is preserved exactly)."
  (replace-regexp-in-string
   "\\`\\(?:[ \t]*\n\\)+" ""
   (replace-regexp-in-string "\\(?:\n[ \t]*\\)+\\'" "" s)))

(defun beads-agent-prompt-edit--region-string (section)
  "Return SECTION's editable region, blank lines trimmed, or \"\"."
  (let* ((b (beads-agent-prompt-edit--region-bounds section))
         (raw (buffer-substring-no-properties (car b) (cdr b))))
    (beads-agent-prompt-edit--trim-blank-lines raw)))

;;; Commands

(defun beads-agent-prompt-edit-confirm ()
  "Confirm both regions and launch the agent.
A blank system region yields SYS=nil (use the backend's built-in
identity)."
  (interactive)
  (let* ((sys-raw (beads-agent-prompt-edit--region-string 'system-heading))
         (user (beads-agent-prompt-edit--region-string 'user-heading))
         (sys (if (string-empty-p sys-raw) nil sys-raw))
         (callback beads-agent-prompt-edit--callback))
    ;; Clean up the prompt-edit buffer BEFORE invoking the callback.
    ;; The callback may spawn async work that captures `(current-buffer)'
    ;; as its caller-buffer; if we kill this buffer afterward (e.g. via
    ;; unwind-protect) the async result is silently dropped.  See bde-d3eg.
    (beads-agent-prompt-edit--cleanup)
    (when callback
      (funcall callback sys user))))

(defun beads-agent-prompt-edit-cancel ()
  "Cancel prompt editing without launching the agent."
  (interactive)
  (let ((callback beads-agent-prompt-edit--callback))
    (beads-agent-prompt-edit--cleanup)
    (when callback
      ;; Cancel sentinel: (nil nil) — both system and user nil.
      (funcall callback nil nil))
    (message "Agent launch cancelled")))

(defun beads-agent-prompt-edit--cleanup ()
  "Clean up prompt edit buffer."
  (let ((buf (current-buffer)))
    (quit-window t)
    (when (buffer-live-p buf)
      (let ((kill-buffer-query-functions nil)
            (inhibit-read-only t))
        (kill-buffer buf)))))

(provide 'beads-agent-prompt-edit)
;;; beads-agent-prompt-edit.el ends here
