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
(require 'beads-agent-keys)

(declare-function beads-actions-claim "beads-actions")
(declare-function beads-show "beads-command-show")
(declare-function beads--get-database-path "beads-util")
(declare-function beads--project-root "beads-util")
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
  '((federation . t))
  "Alist mapping section keys to t when they should default-collapsed.
Sections collapsed by default do not fetch their data until expanded.
Defaults expand every actionable section so the project pulse is
visible at a glance; only Federation (server-mode only) starts
collapsed because most projects do not use it."
  :type '(alist :key-type symbol :value-type boolean)
  :group 'beads)

(defvar beads-dashboard--visibility-cache nil
  "Session-scoped alist mapping project root -> collapsed alist.
Restored on buffer init so collapse state survives close-and-reopen.")

(defvar beads-dashboard--extra-cache nil
  "Session-scoped alist mapping project root -> extra-rows alist.
Each project's entry maps SECTION-KEY -> non-negative integer or the
symbol `all'.  Restored on buffer init so per-section `+'/`-'/`*'
overrides persist across `g' (soft refresh) and across
close-and-reopen.  `C-u g' (hard refresh) clears the project's entry.")

(defvar-local beads-dashboard--last-good-data nil
  "Buffer-local hash table mapping SECTION-KEY -> last good payload.
Powers stale-while-revalidate rendering: during a soft refresh the
section keeps the previously-fetched payload on screen until the new
fetch lands, avoiding a full `Loading…' flicker on agent state
changes.  Cleared by `beads-dashboard-hard-refresh' so the user can
force the loading skeleton back on demand.")

;;; Project Identity

(defun beads-dashboard--project-root ()
  "Return the canonical project root or nil.
Thin wrapper over `beads--project-root', which prefers VC/git
detection and falls back to a marker walk so non-git beads projects
and Gas City workspaces (`.beads'/`city.toml'/`.gc'/`pack.toml') are
recognized.  See `beads--project-root-markers' for the marker set."
  (beads--project-root))

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

(defun beads-dashboard--load-extra (root)
  "Return cached extra-rows alist for ROOT, or nil for a fresh session."
  (cdr (assoc root beads-dashboard--extra-cache)))

(defun beads-dashboard--save-extra (root extra)
  "Persist EXTRA alist for project ROOT in the session cache.
Passing nil for EXTRA clears the entry — used by `C-u g' to reset
per-section load-more state."
  (when root
    (if extra
        (setf (alist-get root beads-dashboard--extra-cache nil nil #'equal)
              extra)
      (setf beads-dashboard--extra-cache
            (assoc-delete-all root beads-dashboard--extra-cache)))))

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

(defun beads-dashboard--update-mode-line (buffer last-refresh policy
                                                 &optional auto-refresh)
  "Refresh the modeline for BUFFER from LAST-REFRESH and POLICY.
AUTO-REFRESH is the current auto-refresh state — when non-nil the
strip ends with `· auto:on' so the user has a visible cue that the
idle timer is armed.  Omitted when auto-refresh is off to keep the
strip short on the common path."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((rel (or (beads-dashboard--format-relative-time last-refresh)
                      "never"))
             (backend (or (plist-get policy :backend) 'unknown))
             (mc (or (plist-get policy :max-concurrent)
                     (beads-command--policy-max-concurrent))))
        (setq mode-line-misc-info
              (list (format " · refreshed %s · dolt:%s · cap:%s%s"
                            rel backend mc
                            (if auto-refresh " · auto:on" "")))))
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
   "KEYS: n/p item · M-n/M-p section · TAB toggle · M-0..M-4 depth · +/-/* rows · g refresh · c claim · b blocker · a agent · RET visit · q quit"
   :face 'shadow))

(defun beads-dashboard--no-project-vnode ()
  "Return a one-line vnode shown when no beads project is discoverable.
The dashboard's per-section loaders all run `bd' from
`default-directory'; without a project root every loader fails with
the same `No such file or directory' message, so we short-circuit
and render this hint instead (see bde-w9xa)."
  (vui-vstack
    :spacing 1
    (vui-text "  No beads project found here." :face 'warning)
    (vui-text
     "  Run `M-x beads-init' in this directory, or visit a project that has a .beads/ folder."
     :face 'shadow)))

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
`:hide-count', and `:extra-rows' (the latter is mixed into the
:async-key so pressing `+'/`-'/`*' invalidates the cached payload and
re-runs the loader with the new fetch limit)."
  (let ((collapsed (cdr (assq key collapsed-alist)))
        (extra-rows (plist-get plist :extra-rows)))
    (vui-component 'beads-dashboard--section
      :key key
      :title title
      :section-key key
      :icon (plist-get plist :icon)
      ;; Including `collapsed' in the async-key forces `vui-use-async'
      ;; to re-run the loader on the first expand — otherwise the
      ;; collapsed section's no-op result (nil) would be cached and
      ;; survive expansion.  EXTRA-ROWS is included so a `+' press
      ;; (which raises the CLI fetch limit) invalidates the cache and
      ;; pulls the additional rows from bd.
      :async-key (list key generation collapsed extra-rows)
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
          (extra     (beads-dashboard--load-extra project-root))
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
    (vui-use-effect (last-refresh auto-refresh)
      (beads-dashboard--update-mode-line
       buffer last-refresh beads-command--policy auto-refresh)
      nil)
    (if (null project-root)
        ;; Short-circuit when invoked outside a beads project: render
        ;; one helpful message instead of fanning out N section loaders
        ;; that all fail with the same `No such file or directory'.
        ;; See bde-w9xa.
        (vui-vstack
          :spacing 1
          (beads-dashboard--header-vnode project-root db-path)
          (beads-dashboard--no-project-vnode)
          (beads-dashboard--footer-vnode))
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
      ;; Recently Closed sits right under Stats so the first thing the
      ;; user sees is the project's most recent forward motion — a
      ;; daily feedback signal.  In-progress / Ready / Blocked /
      ;; Epic Progress follow as actionable work; hygiene buckets
      ;; (Stale, Orphans, Federation) sink to the bottom.
      (let ((rx (cdr (assq 'closed extra))))
        (beads-dashboard--section
         'closed "Recently Closed"
         (beads-dashboard--closed-loader
          (beads-dashboard--effective-fetch-limit rx))
         (lambda (data) (beads-dashboard-render-closed data 'closed rx))
         collapsed generation buffer
         :icon "📦"
         :extra-rows rx
         :render-empty
         (lambda (sk) (beads-dashboard--empty-line "Nothing closed yet." sk))))
      (let ((rx (cdr (assq 'in-flight extra))))
        (beads-dashboard--section
         'in-flight "In progress"
         (beads-dashboard--in-flight-loader
          (beads-dashboard--effective-fetch-limit rx))
         (lambda (data) (beads-dashboard-render-in-flight data 'in-flight rx))
         collapsed generation buffer
         :icon "🚧"
         :extra-rows rx
         :render-empty
         (lambda (sk) (beads-dashboard--empty-line "No work claimed." sk))))
      (let ((rx (cdr (assq 'ready extra))))
        (beads-dashboard--section
         'ready "Ready"
         (beads-dashboard--ready-loader
          (beads-dashboard--effective-fetch-limit rx))
         (lambda (data) (beads-dashboard-render-ready data 'ready rx))
         collapsed generation buffer
         :icon "✅"
         :extra-rows rx
         :render-empty
         (lambda (sk) (beads-dashboard--empty-line "Nothing ready." sk))))
      (let ((rx (cdr (assq 'blocked extra))))
        (beads-dashboard--section
         'blocked "Blocked"
         (beads-dashboard--blocked-loader)
         (lambda (data) (beads-dashboard-render-blocked data 'blocked rx))
         collapsed generation buffer
         :icon "🔒"
         :extra-rows rx))
      (let ((rx (cdr (assq 'epics extra))))
        (beads-dashboard--section
         'epics "Epic Progress"
         (beads-dashboard--epic-loader)
         (lambda (data) (beads-dashboard-render-epic data 'epics rx))
         collapsed generation buffer
         :icon "🎯"
         :extra-rows rx))
      (let ((rx (cdr (assq 'stale extra))))
        (beads-dashboard--section
         'stale "Stale in-progress"
         (beads-dashboard--stale-loader
          (beads-dashboard--effective-fetch-limit rx))
         (lambda (data)
           (beads-dashboard--render-issue-list (or data '()) 'stale rx))
         collapsed generation buffer
         :icon "💤"
         :extra-rows rx))
      (let ((rx (cdr (assq 'orphans extra))))
        (beads-dashboard--section
         'orphans "Orphaned dependencies"
         (beads-dashboard--orphans-loader)
         (lambda (data)
           (beads-dashboard--render-issue-list (or data '()) 'orphans rx))
         collapsed generation buffer
         :icon "🩹"
         :extra-rows rx))
      (when (eq (plist-get beads-command--policy :backend) 'server)
        (beads-dashboard--section
         'federation "Federation"
         (beads-dashboard--federation-loader)
         #'beads-dashboard-render-federation
         collapsed generation buffer
         :icon "🌐"))
      (beads-dashboard--footer-vnode)))))

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
  "Hard refresh: invalidate cache, re-probe policy, reset extras, re-fetch.
Resetting the per-section `+'/`-'/`*' overrides matches the semantics
of `C-u g' as a \"fresh dashboard\" gesture; soft `g'
\(`beads-dashboard-refresh') leaves them untouched.

The `derived-mode-p' guard exists so calling this from outside a
dashboard buffer (e.g. an `after-change-functions' hook or
`global-set-key' binding) still clears the single-flight cache and
re-probes the policy without raising on the missing root state."
  (interactive)
  (clrhash beads-command--single-flight)
  (when (derived-mode-p 'beads-dashboard-mode)
    (let ((root (beads-dashboard--project-root)))
      (beads-dashboard--save-extra root nil)
      (beads-dashboard--bump :extra nil))
    ;; Hard refresh = drop the stale-while-revalidate cache too so the
    ;; user sees the `Loading…' skeleton on demand.  Soft `g' keeps
    ;; the cache populated.
    (when (hash-table-p beads-dashboard--last-good-data)
      (clrhash beads-dashboard--last-good-data)))
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

;;; Per-Section Load More

(defun beads-dashboard--section-key-on-line ()
  "Return the first non-nil `beads-dashboard-section-key' on the current line.
Scans from BOL to EOL and returns the first symbolic key it finds.
Returns nil when no row or header on the current line carries the
property — used by `beads-dashboard--section-at-point' as a more
forgiving lookup than reading the property exactly at point."
  (save-excursion
    (forward-line 0)
    (let ((eol (line-end-position))
          (key nil))
      (while (and (not key) (< (point) eol))
        (setq key (get-text-property (point) 'beads-dashboard-section-key))
        (unless key (forward-char 1)))
      key)))

(defun beads-dashboard--section-at-point ()
  "Return the symbolic section key for the section enclosing point.

Lookup is layered so the +/-/* commands work from anywhere in a
section:

  1. The `beads-dashboard-section-key' text property exactly at point
     (fast path — covers landing on a stamped widget).
  2. Any `beads-dashboard-section-key' property elsewhere on the
     current line — covers issue / epic rows where point sits in a
     gap, the empty-state / loading line, and the section header
     itself (the header label is stamped by
     `beads-dashboard--section-header').
  3. The same scan applied to the section header line found by
     walking backwards — covers truly blank lines between sections."
  (or (get-text-property (point) 'beads-dashboard-section-key)
      (beads-dashboard--section-key-on-line)
      (save-excursion
        (when-let* ((header (beads-dashboard--find-section-header)))
          (goto-char header)
          (beads-dashboard--section-key-on-line)))
      (user-error "Not inside a beads-dashboard section")))

(defun beads-dashboard--bump-extra (key delta-or-symbol)
  "Adjust the extra-rows entry for KEY and rerender.
DELTA-OR-SYMBOL is either an integer delta (positive or negative;
applied to the current count, floored at zero, which removes the
entry), the symbol `all' (set to the unlimited sentinel), or the
symbol `reset' (remove the entry entirely)."
  (let* ((extra (or (beads-dashboard--root-state :extra) '()))
         (cur   (cdr (assq key extra)))
         (next-val
          (pcase delta-or-symbol
            ('all 'all)
            ('reset nil)
            ((pred integerp)
             (let* ((base (cond ((eq cur 'all) 0)  ; `*' then `-' starts fresh
                                ((integerp cur) cur)
                                (t 0)))
                    (sum (+ base delta-or-symbol)))
               (if (<= sum 0) nil sum)))
            (_ (error "Beads-dashboard--bump-extra: unsupported %S"
                      delta-or-symbol))))
         (stripped (assq-delete-all key extra))
         (next (if next-val
                   (cons (cons key next-val) stripped)
                 stripped)))
    (beads-dashboard--bump :extra next)
    (beads-dashboard--save-extra
     (beads-dashboard--project-root) next)))

(defun beads-dashboard--issue-id-at-line ()
  "Return the issue id stamped via `beads-section' on the current line, or nil.
Scans the whole line so it catches the id no matter where in the
button label point sits."
  (save-excursion
    (forward-line 0)
    (let ((eol (line-end-position))
          (id nil))
      (while (and (not id) (< (point) eol))
        (when-let* ((sec (get-text-property (point) 'beads-section))
                    (_ (object-of-class-p sec 'beads-issue-section))
                    (issue (and (slot-boundp sec 'issue) (oref sec issue))))
          (setq id (and (slot-boundp issue 'id) (oref issue id))))
        (unless id (forward-char 1)))
      id)))

(defun beads-dashboard--goto-issue-line (issue-id)
  "Move point to the line whose `beads-section' stamp matches ISSUE-ID.
Returns t when found, nil when no such line exists in the buffer.
Leaves point past the leading indentation so the row's widget is
under point."
  (let ((target nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not target) (not (eobp)))
        (when (equal issue-id (beads-dashboard--issue-id-at-line))
          (setq target (point)))
        (unless target (forward-line 1))))
    (when target
      (goto-char target)
      (forward-line 0)
      (skip-chars-forward " ")
      t)))

(defun beads-dashboard--goto-section-header (section-key)
  "Move point to the header line of SECTION-KEY, returning t on success.
Lands past the chevron so the header widget is under point."
  (let ((target nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not target) (not (eobp)))
        (when (and (beads-dashboard--header-line-p)
                   (eq section-key
                       (beads-dashboard--section-key-on-line)))
          (setq target (point)))
        (unless target (forward-line 1))))
    (when target
      (goto-char target)
      (forward-line 0)
      (when (looking-at beads-dashboard--header-glyph-re)
        (skip-chars-forward
         (concat beads-section-glyph-expanded
                 beads-section-glyph-collapsed " ")))
      t)))

(defconst beads-dashboard--more-line-re
  "and [0-9]+ more (\\+)"
  "Regexp matching the distinctive suffix of a `… and N more (+)' row.
Single source of truth for the format produced by
`beads-dashboard--more-line'; update both together if the label
template ever changes.")

(defun beads-dashboard--more-line-text-p ()
  "Return non-nil when the current line's text matches the more-line format."
  (string-match-p beads-dashboard--more-line-re
                  (buffer-substring (line-beginning-position)
                                    (line-end-position))))

(defun beads-dashboard--on-more-line-p ()
  "Return non-nil when the current line is a `… and N more (+)' button.
A more-line is the only row that carries a
`beads-dashboard-section-key' property without a `beads-section'
issue object and whose text matches `beads-dashboard--more-line-re'."
  (save-excursion
    (forward-line 0)
    (and (beads-dashboard--section-key-on-line)
         (not (beads-dashboard--issue-id-at-line))
         (beads-dashboard--more-line-text-p))))

(defun beads-dashboard--goto-more-line (section-key)
  "Move point to the `… and N more' line for SECTION-KEY, returning t on hit."
  (let ((target nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not target) (not (eobp)))
        (when (and (eq section-key
                       (beads-dashboard--section-key-on-line))
                   (not (beads-dashboard--header-line-p))
                   (not (beads-dashboard--issue-id-at-line))
                   (beads-dashboard--more-line-text-p))
          (setq target (point)))
        (unless target (forward-line 1))))
    (when target
      (goto-char target)
      (forward-line 0)
      (skip-chars-forward " ")
      t)))

(defun beads-dashboard--restore-point (section-key issue-id on-more-line)
  "Best-effort re-anchor of point after a +/-/* rerender.

Priority order:
  1. ISSUE-ID line — only when the user was anchored to a specific
     issue/epic row.
  2. The `… and N more' line for SECTION-KEY — only when the user
     was on a more-line before the bump and a more-line still exists
     for that section after the bump (so successive `+' presses can
     be repeated by holding the same point).
  3. The SECTION-KEY header — last resort so the user always lands
     inside the section they acted on, never in a sibling section."
  (cond
   ((and issue-id (beads-dashboard--goto-issue-line issue-id)))
   ((and on-more-line (beads-dashboard--goto-more-line section-key)))
   (t (beads-dashboard--goto-section-header section-key))))

(defun beads-dashboard--with-point-restore (section-key thunk)
  "Capture point context, run THUNK, then re-anchor point inside SECTION-KEY.

The synchronous rerender driven by THUNK transitions the section
through a `Loading…' skeleton (the original issue rows briefly
disappear), so vui's path/index cursor-restore can land on a widget
in a sibling section.  We fight that by capturing the issue-id (or
\"was on more-line\" state) at point first, then re-anchoring after
the synchronous rerender, after a short timer (cached loads), and
after a longer timer (cold fetches).  Each retry is idempotent — if
point is already on the right line, the goto is a no-op.

Latency trade-off: the 0.5s retry covers a typical local `bd' CLI
fetch.  A cold fetch on a slow disk or huge repo (or remote Dolt)
can exceed that window; in that pathological case point may be left
on the loading-skeleton row until the user scrolls.  We do not chain
arbitrarily long retries because (a) it would keep idle CPU work
running long after the user has moved on, and (b) magit's
load-more-style flows have the same trade-off and we mirror it."
  (let ((issue-id (beads-dashboard--issue-id-at-line))
        (on-more-line (beads-dashboard--on-more-line-p))
        (buf (current-buffer)))
    (funcall thunk)
    ;; Synchronous re-anchor: we are still in the dashboard buffer.
    (beads-dashboard--restore-point section-key issue-id on-more-line)
    (cl-labels
        ((retry ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (derived-mode-p 'beads-dashboard-mode)
                 (beads-dashboard--restore-point
                  section-key issue-id on-more-line))))))
      ;; Cached-load path — async resolves on the next tick.
      (run-with-timer 0.05 nil #'retry)
      ;; Cold-load path — bd CLI typically returns within a second.
      (run-with-timer 0.5 nil #'retry))))

(defun beads-dashboard-load-more ()
  "Show `beads-dashboard-section-batch' more rows for the section at point.
Section is resolved via the `beads-dashboard-section-key' text
property at point or anywhere on the current line, falling back to
the section header.  Point is re-anchored after the rerender so it
stays on the same issue (or on the section header) instead of
jumping to a sibling section.  Persists across `g' refresh; reset by
`C-u g'."
  (interactive)
  (let ((key (beads-dashboard--section-at-point)))
    (beads-dashboard--with-point-restore
     key
     (lambda ()
       (beads-dashboard--bump-extra
        key beads-dashboard-section-batch)))))

(defun beads-dashboard-load-less ()
  "Hide `beads-dashboard-section-batch' rows for the section at point.
Floors at the default limit (clears the section's entry).  Point is
re-anchored after the rerender."
  (interactive)
  (let ((key (beads-dashboard--section-at-point)))
    (beads-dashboard--with-point-restore
     key
     (lambda ()
       (beads-dashboard--bump-extra
        key (- beads-dashboard-section-batch))))))

(defun beads-dashboard-load-all ()
  "Show every available row for the section at point.
Bumps the underlying CLI fetch limit to 0 (unlimited per `bd' CLI
semantics) so the section header shows the true count rather than the
CLI's default cap.  Point is re-anchored after the rerender."
  (interactive)
  (let ((key (beads-dashboard--section-at-point)))
    (beads-dashboard--with-point-restore
     key
     (lambda ()
       (beads-dashboard--bump-extra key 'all)))))

(defun beads-dashboard--set-depth (n)
  "Show only N levels deep, expanding the first N section groups."
  (let* ((order '(stats closed in-flight ready blocked epics stale orphans federation))
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

(defun beads-dashboard-depth-all ()
  "Expand every section.
The natural inverse of \\[beads-dashboard-depth-1] (`M-1' shows only
the first); since the dashboard has more sections (9) than depth
keys (M-1..M-4 only cover the first 4), this gives the user a
one-shot way to expand everything without TAB-ing each section."
  (interactive)
  ;; `most-positive-fixnum' is well above any plausible section count
  ;; and avoids hard-coding the order length here.  `seq-take' clamps
  ;; gracefully when N exceeds the list length.
  (beads-dashboard--set-depth most-positive-fixnum))

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
  "M-0" #'beads-dashboard-depth-all
  ;; Per-section row-count overrides.  `-' shadows the parent map's
  ;; `negative-argument' inside dashboard mode only — no dashboard
  ;; command takes a numeric prefix, so the binding is unambiguous.
  "+"   #'beads-dashboard-load-more
  "-"   #'beads-dashboard-load-less
  "*"   #'beads-dashboard-load-all
  ;; Share the `a' prefix with list/show modes; the issue at point is
  ;; resolved via the section text-property branch of
  ;; `beads-agent--detect-issue-id'.
  "a"   beads-agent-prefix-map
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

Long issue titles are truncated rather than wrapped so each issue
stays on a single row when the buffer is shown in a narrow window
\(e.g. a side-by-side `C-x 3' split).  Toggle per-buffer with
\\[toggle-truncate-lines]; eldoc-on-hover still shows full titles.

\\{beads-dashboard-mode-map}"
  :interactive nil
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'beads-dashboard--revert)
  (setq-local mode-line-misc-info nil)
  (setq-local beads-dashboard--last-good-data
              (make-hash-table :test 'equal)))

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

;;; Agent State Hook Integration

;; Defined in `beads-agent-backend.el'.  Forward-declared so the
;; dashboard byte-compiles without pulling in the agent subsystem.
(defvar beads-agent-state-change-hook)

(defun beads-dashboard--on-agent-state-change (_action _session)
  "Refresh every live dashboard buffer after an agent state change.
ACTION and SESSION are provided by `beads-agent-state-change-hook'.
Called when an agent is started, stopped, finished, or failed so the
In-progress section (and any agent-badge group on issue rows) reflects
the new state without the user having to press \\[beads-dashboard-refresh]."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'beads-dashboard-mode)))
      (with-current-buffer buffer
        (condition-case nil
            (beads-dashboard-refresh)
          (error nil))))))

(add-hook 'beads-agent-state-change-hook
          #'beads-dashboard--on-agent-state-change t)

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
