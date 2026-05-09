;;; beads-dashboard.el --- Magit-idiomatic dashboard for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `beads-dashboard' is a Magit-idiomatic project pulse buffer for the
;; beads issue tracker.  Sections (defined in
;; `beads-dashboard-sections.el') load asynchronously and are wrapped
;; in `vui-error-boundary' so a per-section failure never blanks the
;; dashboard.  See `beads-dashboard-mode-map' for the keymap.

;;; Code:

(require 'eieio)
(require 'vui)
(require 'beads-section)
(require 'beads-command)
(require 'beads-command-dep)
(require 'beads-dashboard-sections)
(require 'beads-status)

(declare-function beads-actions-claim "beads-actions")
(declare-function beads-show "beads-command-show")
(declare-function beads-git-find-project-root "beads-git")
(declare-function beads--get-database-path "beads-util")
(declare-function transient--prefix "transient")

;;; Variables

(defvar beads-dashboard--buffer-name "*beads-dashboard*"
  "Default buffer name when no project root is discoverable.")

(defcustom beads-dashboard-auto-refresh-interval 30
  "Seconds of Emacs idle time between auto-refresh ticks.
Auto-refresh is OFF by default and toggled with `r' in the dashboard."
  :type 'number
  :group 'beads)

(defcustom beads-dashboard-default-collapsed
  '((blocked . t) (epics . t) (closed . t) (federation . t))
  "Alist mapping section keys to t when they should default-collapsed.
Sections collapsed by default do not fetch their data until expanded."
  :type '(alist :key-type symbol :value-type boolean)
  :group 'beads)

(defvar beads-dashboard--visibility-cache nil
  "Session-scoped alist mapping project root -> collapsed alist.
Restored on buffer init so collapse state survives close-and-reopen.")

;;; Project Identity

(defun beads-dashboard--project-root ()
  "Return the canonical project root or nil."
  (let ((root (ignore-errors (beads-git-find-project-root))))
    (when root (file-name-as-directory (expand-file-name root)))))

(defun beads-dashboard--buffer-name-for (root)
  "Return the dashboard buffer name for project ROOT."
  (if root
      (format "*beads-dashboard<%s>*"
              (file-name-nondirectory (directory-file-name root)))
    beads-dashboard--buffer-name))

(defun beads-dashboard--load-visibility (root)
  "Return cached collapse alist for ROOT, falling back to defaults."
  (or (cdr (assoc root beads-dashboard--visibility-cache))
      (copy-alist beads-dashboard-default-collapsed)))

(defun beads-dashboard--save-visibility (root collapsed)
  "Persist COLLAPSED alist for project ROOT in the session cache."
  (when root
    (setf (alist-get root beads-dashboard--visibility-cache nil nil #'equal)
          collapsed)))

;;; Header / Modeline

(defun beads-dashboard--format-relative-time (epoch)
  "Format EPOCH (a `float-time' result) as a short relative timestamp."
  (when epoch
    (let* ((delta (- (float-time) epoch))
           (delta (max 0 (round delta))))
      (cond
       ((< delta 60) (format "%ds ago" delta))
       ((< delta 3600) (format "%dm ago" (/ delta 60)))
       (t (format "%dh ago" (/ delta 3600)))))))

(defun beads-dashboard--update-mode-line (buffer last-refresh policy)
  "Refresh the modeline for BUFFER from LAST-REFRESH and POLICY."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((rel (or (beads-dashboard--format-relative-time last-refresh)
                      "never"))
             (backend (or (plist-get policy :backend) 'unknown))
             (mc (or (plist-get policy :max-concurrent)
                     (beads-command--policy-max-concurrent))))
        (setq mode-line-misc-info
              (list (format " · refreshed %s · dolt:%s · cap:%s"
                            rel backend mc))))
      (force-mode-line-update))))

(defun beads-dashboard--header-vnode (root db)
  "Return the dashboard header vnode for project ROOT and DB path."
  (let ((project-name (if root
                          (file-name-nondirectory (directory-file-name root))
                        "unknown")))
    (vui-hstack :spacing 0
      (vui-text "Beads-Dashboard" :face 'bold)
      (vui-text " — " :face 'shadow)
      (vui-text project-name :face 'font-lock-constant-face)
      (when db
        (vui-text (format " (%s)" db) :face 'shadow)))))

(defun beads-dashboard--footer-vnode ()
  "Return the dashboard footer vnode (key hints)."
  (vui-text
   "KEYS: n/p item · M-n/M-p section · TAB toggle · M-1..M-4 depth · g refresh · c claim · b blocker · RET visit · q quit"
   :face 'shadow))

;;; Section Builder

(defun beads-dashboard--section (key title loader render-ready
                                     collapsed-alist generation
                                     buffer
                                     &rest plist)
  "Return a `beads-dashboard--section' vui-component vnode.
KEY is the symbolic section key (`stats', `in-flight', etc.) used as
the vnode `:key' for vui reconciliation, in `collapsed-alist' lookups,
and as part of the `:async-key' (combined with GENERATION so refresh
invalidates the cache).  TITLE is the heading.  LOADER is a function
of `(resolve reject)'.  RENDER-READY is called with the parsed data.
COLLAPSED-ALIST is the root component's collapse state.  BUFFER is
the dashboard buffer captured for the toggle callback.  PLIST
forwards `:icon', `:render-empty', `:render-error', `:force-render',
and `:hide-count'."
  (let ((collapsed (cdr (assq key collapsed-alist))))
    (vui-component 'beads-dashboard--section
      :key key
      :title title
      :section-key key
      :icon (plist-get plist :icon)
      ;; Including `collapsed' in the async-key forces `vui-use-async'
      ;; to re-run the loader on the first expand — otherwise the
      ;; collapsed section's no-op result (nil) would be cached and
      ;; survive expansion.
      :async-key (list key generation collapsed)
      :load (if collapsed (lambda (resolve _reject) (funcall resolve nil)) loader)
      :render-ready render-ready
      :render-empty (plist-get plist :render-empty)
      :render-error (plist-get plist :render-error)
      :force-render (plist-get plist :force-render)
      :hide-count (plist-get plist :hide-count)
      :collapsed collapsed
      :on-toggle (beads-dashboard--make-toggle key buffer))))

(defun beads-dashboard--make-toggle (key buffer)
  "Return a click callback to toggle section KEY on BUFFER."
  (lambda ()
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((collapsed (beads-dashboard--root-state :collapsed))
               (now (cdr (assq key collapsed)))
               (next (cons (cons key (not now))
                           (assq-delete-all key collapsed))))
          (vui-set-state :collapsed next)
          (beads-dashboard--save-visibility
           (beads-dashboard--project-root) next))))))

;;; Root Component

(vui-defcomponent beads-dashboard--root
    (project-root db-path)
  "Root vui component for the beads dashboard.
PROJECT-ROOT keys the visibility cache and labels the header.
DB-PATH is resolved once at mount and threaded through so the
header does not stat the filesystem on every reconcile.
Sections receive collapse state as a prop because per-component
`:state' does not survive `vui-mount' reliably."
  :state ((collapsed (beads-dashboard--load-visibility project-root))
          (auto-refresh nil)
          (last-refresh (float-time))
          (generation 0))
  :render
  (let ((buffer (current-buffer)))
    (vui-use-effect (auto-refresh)
      (when auto-refresh
        (let ((timer (run-with-idle-timer
                      beads-dashboard-auto-refresh-interval t
                      #'beads-dashboard--idle-refresh buffer)))
          (lambda () (when (timerp timer) (cancel-timer timer))))))
    (vui-use-effect (last-refresh)
      (beads-dashboard--update-mode-line
       buffer last-refresh beads-command--policy)
      nil)
    (vui-vstack
      :spacing 1
      (beads-dashboard--header-vnode project-root db-path)
      (beads-dashboard--section
       'stats "Stats"
       (beads-dashboard--stats-loader)
       #'beads-dashboard-render-stats
       collapsed generation buffer
       :icon "📊"
       :force-render t
       ;; Stats data is an alist, not a list of issues — `length' is meaningless.
       :hide-count t)
      (beads-dashboard--section
       'stale "Stale in-progress"
       (beads-dashboard--stale-loader)
       (lambda (data) (beads-dashboard--render-issue-list (or data '())))
       collapsed generation buffer
       :icon "⚠")
      (beads-dashboard--section
       'orphans "Orphaned dependencies"
       (beads-dashboard--orphans-loader)
       (lambda (data) (beads-dashboard--render-issue-list (or data '())))
       collapsed generation buffer
       :icon "⚠")
      (beads-dashboard--section
       'in-flight "In progress"
       (beads-dashboard--in-flight-loader)
       #'beads-dashboard-render-in-flight
       collapsed generation buffer
       :icon "🛠"
       :render-empty (lambda () (beads-dashboard--empty-line "No work claimed.")))
      (beads-dashboard--section
       'ready "Ready"
       (beads-dashboard--ready-loader)
       #'beads-dashboard-render-ready
       collapsed generation buffer
       :icon "✅"
       :render-empty (lambda () (beads-dashboard--empty-line "Nothing ready.")))
      (beads-dashboard--section
       'blocked "Blocked"
       (beads-dashboard--blocked-loader)
       #'beads-dashboard-render-blocked
       collapsed generation buffer
       :icon "🔒")
      (beads-dashboard--section
       'epics "Epic Progress"
       (beads-dashboard--epic-loader)
       #'beads-dashboard-render-epic
       collapsed generation buffer
       :icon "🎯")
      (beads-dashboard--section
       'closed "Recently Closed"
       (beads-dashboard--closed-loader)
       #'beads-dashboard-render-closed
       collapsed generation buffer
       :icon "📦"
       :render-empty (lambda () (beads-dashboard--empty-line "Nothing closed yet.")))
      (when (eq (plist-get beads-command--policy :backend) 'server)
        (beads-dashboard--section
         'federation "Federation"
         (beads-dashboard--federation-loader)
         #'beads-dashboard-render-federation
         collapsed generation buffer
         :icon "🌐"))
      (beads-dashboard--footer-vnode))))

;;; Refresh / Idle

(defun beads-dashboard--root-state (key)
  "Return the value of KEY in the dashboard root vui state, or nil."
  (and vui--root-instance
       (plist-get (vui-instance-state vui--root-instance) key)))

(defun beads-dashboard--bump (key value)
  "Apply VALUE to the root state slot KEY in the current buffer.
Skips the rerender when VALUE equals the current slot — guards against
no-op writes from idle ticks and back-to-back refreshes."
  (when vui--root-instance
    (let ((state (vui-instance-state vui--root-instance)))
      (unless (equal (plist-get state key) value)
        (setf (vui-instance-state vui--root-instance)
              (plist-put state key value))
        (vui--rerender-instance vui--root-instance)))))

(defun beads-dashboard-refresh ()
  "Refresh stale dashboard sections by bumping the generation counter."
  (interactive)
  (when (eq major-mode 'beads-dashboard-mode)
    (beads-dashboard--bump :generation
                           (1+ (or (beads-dashboard--root-state :generation) 0)))
    (beads-dashboard--bump :last-refresh (float-time))
    (message "Beads-Dashboard refreshed.")))

(defun beads-dashboard-hard-refresh ()
  "Hard refresh: invalidate cache, re-probe policy, re-fetch."
  (interactive)
  (clrhash beads-command--single-flight)
  (beads-command--policy-probe
   (lambda (_p)
     (beads-dashboard-refresh))))

(defun beads-dashboard-toggle-auto-refresh ()
  "Toggle 30s idle auto-refresh for the current dashboard buffer."
  (interactive)
  (when (eq major-mode 'beads-dashboard-mode)
    (let ((cur (beads-dashboard--root-state :auto-refresh)))
      (beads-dashboard--bump :auto-refresh (not cur))
      (message "Beads-Dashboard auto-refresh: %s" (if cur "off" "on")))))

(defun beads-dashboard--idle-refresh (buffer)
  "Idle callback that refreshes BUFFER without preempting transients."
  (when (and (buffer-live-p buffer)
             (not (bound-and-true-p transient--prefix)))
    (with-current-buffer buffer
      (when (eq major-mode 'beads-dashboard-mode)
        (beads-dashboard-refresh)))))

;;; Section Toggle / Visibility Depth

(defconst beads-dashboard--header-glyph-re
  (regexp-opt (list beads-section-glyph-expanded
                    beads-section-glyph-collapsed))
  "Regexp matching the leading glyph on a section header line.")

(defun beads-dashboard--find-section-header (&optional pos)
  "Find the section header (widget) for the section containing POS.
POS defaults to point.  Returns a marker pointing at the header
button, or nil when none can be located."
  (save-excursion
    (when pos (goto-char pos))
    ;; Walk backwards line-by-line until the line starts with a
    ;; section glyph.  Stop if we wander past the buffer top.
    (forward-line 0)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (if (looking-at beads-dashboard--header-glyph-re)
            (setq found (point-marker))
          (forward-line -1)))
      ;; Edge case: bobp with the very first line being a header.
      (when (and (not found) (looking-at beads-dashboard--header-glyph-re))
        (setq found (point-marker)))
      found)))

(defun beads-dashboard-toggle-section ()
  "Toggle the section enclosing point (Magit-style).

Walks backwards until a section header line (one starting with the
chevron glyph) is found, then activates the header widget on that
line, which fires its on-click handler and flips the root collapse
state for the section."
  (interactive)
  (let ((header (beads-dashboard--find-section-header)))
    (cond
     (header
      (let ((widget (save-excursion
                      (goto-char header)
                      ;; Walk forward on the same line to land on the
                      ;; header button (the glyph itself is just text).
                      (let ((eol (line-end-position))
                            (w nil))
                        (while (and (not w) (< (point) eol))
                          (setq w (widget-at (point)))
                          (unless w (forward-char 1)))
                        w))))
        (when widget
          (widget-apply widget :action))))
     ((widget-at (point))
      (widget-apply (widget-at (point)) :action)))))

;;; Navigation (Magit-idiomatic: n/p, M-n/M-p)

(defun beads-dashboard--header-line-p ()
  "Return non-nil when the current line begins with a section glyph."
  (save-excursion
    (forward-line 0)
    (looking-at beads-dashboard--header-glyph-re)))

(defun beads-dashboard--issue-line-p ()
  "Return non-nil when the current line carries a `beads-section' property.
The property is stamped on every issue/epic-status line by
`beads-section--issue-line-vnode' (and the dashboard's epic helper)."
  (save-excursion
    (forward-line 0)
    (let ((eol (line-end-position))
          (found nil))
      (while (and (not found) (< (point) eol))
        (when (get-text-property (point) 'beads-section)
          (setq found t))
        (forward-char 1))
      found)))

(defun beads-dashboard--selectable-line-p ()
  "Return non-nil when the current line is a section header or an issue line."
  (or (beads-dashboard--header-line-p)
      (beads-dashboard--issue-line-p)))

(defun beads-dashboard--move-to-line (predicate direction)
  "Move point to the start of the next line satisfying PREDICATE.
DIRECTION is +1 to move forward, -1 to move backward.  Stops at the
target line, leaving point on the first non-glyph character so RET
visit / TAB toggle land on the widget.  Returns t when a line was
found, nil when point did not move."
  (let ((origin (point))
        (found nil))
    (forward-line direction)
    (while (and (not (if (> direction 0) (eobp) (bobp)))
                (not (funcall predicate)))
      (forward-line direction))
    (when (funcall predicate)
      (setq found t)
      (forward-line 0)
      ;; Skip past the leading chevron + space so widget-at works
      ;; immediately on the visible header text.
      (when (looking-at beads-dashboard--header-glyph-re)
        (skip-chars-forward
         (concat beads-section-glyph-expanded
                 beads-section-glyph-collapsed " ")))
      ;; For issue lines, jump to the first widget character on the line.
      (when (and (beads-dashboard--issue-line-p) (not (widget-at)))
        (let ((eol (line-end-position)))
          (while (and (not (widget-at)) (< (point) eol))
            (forward-char 1)))))
    (unless found (goto-char origin))
    found))

(defun beads-dashboard-next-item ()
  "Move to the next item — section header or issue line."
  (interactive)
  (or (beads-dashboard--move-to-line
       #'beads-dashboard--selectable-line-p +1)
      (user-error "No next item")))

(defun beads-dashboard-previous-item ()
  "Move to the previous item — section header or issue line."
  (interactive)
  (or (beads-dashboard--move-to-line
       #'beads-dashboard--selectable-line-p -1)
      (user-error "No previous item")))

(defun beads-dashboard-next-section ()
  "Move to the next section header, skipping past issue lines."
  (interactive)
  (or (beads-dashboard--move-to-line
       #'beads-dashboard--header-line-p +1)
      (user-error "No next section")))

(defun beads-dashboard-previous-section ()
  "Move to the previous section header, skipping past issue lines."
  (interactive)
  (or (beads-dashboard--move-to-line
       #'beads-dashboard--header-line-p -1)
      (user-error "No previous section")))

(defun beads-dashboard--set-depth (n)
  "Show only N levels deep, expanding the first N section groups."
  (let* ((order '(stats stale orphans in-flight ready blocked epics closed federation))
         (visible (seq-take order n))
         (next (mapcar
                (lambda (k)
                  (cons k (not (memq k visible))))
                order)))
    (beads-dashboard--bump :collapsed next)
    (beads-dashboard--save-visibility
     (beads-dashboard--project-root) next)))

(defun beads-dashboard-depth-1 ()
  "Show only the top-level sections."
  (interactive) (beads-dashboard--set-depth 1))

(defun beads-dashboard-depth-2 ()
  "Show top two levels of sections."
  (interactive) (beads-dashboard--set-depth 2))

(defun beads-dashboard-depth-3 ()
  "Show top three levels of sections."
  (interactive) (beads-dashboard--set-depth 3))

(defun beads-dashboard-depth-4 ()
  "Show top four levels of sections."
  (interactive) (beads-dashboard--set-depth 4))

;;; Mode

(defvar-keymap beads-dashboard-mode-map
  :parent beads-section-mode-map
  "TAB" #'beads-dashboard-toggle-section
  "g"   #'beads-dashboard-refresh-dispatch
  "r"   #'beads-dashboard-toggle-auto-refresh
  "c"   #'beads-dashboard-claim-at-point
  "b"   #'beads-dashboard-jump-to-blocker
  "n"   #'beads-dashboard-next-item
  "p"   #'beads-dashboard-previous-item
  "M-n" #'beads-dashboard-next-section
  "M-p" #'beads-dashboard-previous-section
  ;; Bind both `RET' (TTY/C-m) and `<return>' (GUI) so visit fires in
  ;; both terminal and windowed Emacs.
  "RET"      #'beads-dashboard-visit-at-point
  "<return>" #'beads-dashboard-visit-at-point
  "M-1" #'beads-dashboard-depth-1
  "M-2" #'beads-dashboard-depth-2
  "M-3" #'beads-dashboard-depth-3
  "M-4" #'beads-dashboard-depth-4
  "q"   #'quit-window)

(defun beads-dashboard-refresh-dispatch (&optional arg)
  "Refresh the dashboard.  With prefix ARG, do a hard refresh.

`g'      → `beads-dashboard-refresh' (TTL-aware soft refresh)
`C-u g'  → `beads-dashboard-hard-refresh' (clear cache + re-probe policy)"
  (interactive "P")
  (if arg (beads-dashboard-hard-refresh) (beads-dashboard-refresh)))

(define-derived-mode beads-dashboard-mode beads-section-mode "Beads-Dashboard"
  "Major mode for the Magit-idiomatic beads dashboard.

Derived from `beads-section-mode' so the existing `beads-section'
text-property contract is preserved (RET to visit, eldoc, etc.).

\\{beads-dashboard-mode-map}"
  :interactive nil
  (setq-local revert-buffer-function #'beads-dashboard--revert)
  (setq-local mode-line-misc-info nil))

(defun beads-dashboard--revert (_ignore _noconfirm)
  "Revert the dashboard buffer (no-prompt revert hook)."
  (beads-dashboard-refresh))

;;; Issue Actions

(defun beads-dashboard-claim-at-point ()
  "Claim the issue at point in the dashboard.

`beads-actions-claim' is a zero-argument interactive command that
discovers the issue via `beads-issue-at-point' (which itself looks
up the `beads-section' text-property contract that our issue lines
honour)."
  (interactive)
  (cond
   ((fboundp 'beads-actions-claim)
    (call-interactively #'beads-actions-claim))
   ((beads-section-issue-id-at-point)
    (message "beads-actions-claim not available; issue at point: %s"
             (beads-section-issue-id-at-point)))
   (t (user-error "No issue at point"))))

(declare-function beads-command-dep-list "beads-command-dep")

(defun beads-dashboard--blocker-of (deps)
  "Return the first blocker from DEPS, a list of `beads-dependency' instances.
A blocker is a non-closed dependency with type=blocks (parent-child
links are structural, not blocking).  Prefers in-progress deps over
plain open ones so the user lands on the active blocker.  Returns nil
when no blocker exists."
  (let* ((blocking (seq-filter
                    (lambda (d)
                      (and (slot-boundp d 'type)
                           (equal (oref d type) beads-dep-blocks)
                           (slot-boundp d 'status)
                           (not (equal (oref d status) beads-status-closed))))
                    deps))
         (in-progress (seq-find
                       (lambda (d)
                         (equal (oref d status) beads-status-in-progress))
                       blocking)))
    (or in-progress (car blocking))))

(defun beads-dashboard-jump-to-blocker ()
  "Jump to the active blocker of the issue at point.

Runs `bd dep list <id> --json' asynchronously, picks the first
non-closed `blocks'-type dependency (preferring in-progress), and
opens it via `beads-show'.  Falls back to visiting the original
issue when no blocker exists.  Errors are reported via `message'."
  (interactive)
  (let ((id (beads-section-issue-id-at-point)))
    (cond
     ((not id) (user-error "No issue at point"))
     (t
      (let ((cmd (beads-command-dep-list :issue-id id :json t)))
        (beads-command-execute-async
         cmd
         (lambda (deps)
           (if-let ((blocker (beads-dashboard--blocker-of deps)))
               (beads-show (oref blocker depends-on-id))
             (message "No active blocker for %s; visiting issue." id)
             (beads-show id)))
         (lambda (err)
           (message "Could not resolve blocker: %s"
                    (if (consp err) (car err) err))
           (beads-show id))
         :timeout beads-command-async-timeout))))))

(defun beads-dashboard-visit-at-point ()
  "Visit the issue at point, or activate the widget under point.

Issue lines are vui-buttons whose `:on-click' opens the issue via
`beads-show'.  When point lands on whitespace inside a button or on
a section header, fall back to the widget at point so RET still
does something sensible."
  (interactive)
  (cond
   ((beads-section-issue-id-at-point)
    (beads-show (beads-section-issue-id-at-point)))
   ((widget-at (point))
    (widget-apply (widget-at (point)) :action))
   (t (user-error "No issue or button at point"))))

;;; Entry Point

;;;###autoload
(defun beads-dashboard ()
  "Open or refresh the Magit-idiomatic beads dashboard for this project."
  (interactive)
  (let* ((root (beads-dashboard--project-root))
         (buf-name (beads-dashboard--buffer-name-for root))
         (buf (get-buffer-create buf-name))
         (db (ignore-errors (beads--get-database-path))))
    (with-current-buffer buf
      (unless (eq major-mode 'beads-dashboard-mode)
        (beads-dashboard-mode)))
    ;; Probe the policy lazily (cached after first run).
    (unless beads-command--policy
      (beads-command--policy-probe (lambda (_p) (ignore))))
    (with-current-buffer buf
      (vui-mount
       (vui-component 'beads-dashboard--root
                      :project-root root
                      :db-path db)
       (buffer-name)))
    (pop-to-buffer buf)))

(provide 'beads-dashboard)
;;; beads-dashboard.el ends here
