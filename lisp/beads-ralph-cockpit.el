;;; beads-ralph-cockpit.el --- Vui multi-loop overview for Ralph -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, vc

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Singleton vui buffer `*beads-ralph-cockpit*' that shows every live
;; Ralph controller in one table, with one-key actions to switch /
;; stop / pause / resume / kill / GC.  Reads the public controller
;; registry promoted in bde-deqx.2; this module owns the multi-loop
;; index UX and delegates per-loop actions back to the existing
;; controller entry points (`beads-agent-ralph-stop' /
;; `beads-agent-ralph--pause' / `beads-agent-ralph-kill-iter') so the
;; controller state machine remains the single source of truth.
;;
;; Refresh model: subscribes to `beads-agent-ralph-state-change-functions'
;; with a 0.05s idle-timer debounce so a burst of transitions across
;; several controllers collapses into one re-render; runs a 1s
;; repeating timer for cost-so-far and elapsed-time updates that is
;; suspended when no controller is in `running' or `cooling-down'.
;;
;; Status badge model: the controller has six raw statuses (idle,
;; running, cooling-down, auto-paused, done, stopped, failed); the
;; cockpit collapses them into five badge groups for the header
;; counters and filter chips: `active' (running OR cooling-down),
;; `paused' (auto-paused), `done', `stopped', `failed'.  `idle' is
;; an internal transitional state never displayed; controllers in
;; `idle' are bucketed as `active' so they remain visible while the
;; first iteration spins up.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vui)

(require 'beads-agent-ralph)
(require 'beads-agent-ralph-dashboard)

(declare-function beads-ralph-epic-browser "beads-ralph-epic-browser")

;;; Customization

(defgroup beads-ralph-cockpit nil
  "Multi-loop overview for the Ralph autonomous-iteration loop."
  :group 'beads
  :prefix "beads-ralph-cockpit-")

(defcustom beads-ralph-cockpit-buffer-name "*beads-ralph-cockpit*"
  "Name of the singleton cockpit buffer."
  :type 'string
  :group 'beads-ralph-cockpit)

(defcustom beads-ralph-cockpit-render-debounce 0.05
  "Idle seconds used to coalesce re-renders from state-change events.
A burst of state transitions across controllers collapses into a
single re-render so the cockpit does not thrash when several
loops change status in quick succession."
  :type 'number
  :group 'beads-ralph-cockpit)

(defcustom beads-ralph-cockpit-refresh-interval 1.0
  "Seconds between cockpit timer refreshes while loops are active.
The timer is suspended when no controller is in `running' or
`cooling-down' so an idle Emacs with the cockpit open does not
tick."
  :type 'number
  :group 'beads-ralph-cockpit)

;;; Faces

(defface beads-ralph-cockpit-header-face
  '((t :weight bold))
  "Face for the cockpit's top header line."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-separator-face
  '((t :inherit shadow))
  "Face for the horizontal separator lines."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-id-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the loop's root-id column."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-kind-face
  '((t :inherit font-lock-type-face))
  "Face for the kind column (issue/epic)."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-badge-active-face
  '((t :inherit success :weight bold))
  "Face for the `active' badge."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-badge-paused-face
  '((t :inherit warning :weight bold))
  "Face for the `paused' badge."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-badge-done-face
  '((t :inherit shadow :weight bold))
  "Face for the `done' badge."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-badge-stopped-face
  '((t :inherit shadow :weight bold))
  "Face for the `stopped' badge."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-badge-failed-face
  '((t :inherit error :weight bold))
  "Face for the `failed' badge."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-cost-face
  '((t :inherit font-lock-string-face))
  "Face for the cost column."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-elapsed-face
  '((t :inherit shadow))
  "Face for the elapsed-time column."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-contested-face
  '((t :inherit error))
  "Face for the inline contested-claim marker."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-filter-on-face
  '((t :inherit success))
  "Face for a filter chip currently included."
  :group 'beads-ralph-cockpit)

(defface beads-ralph-cockpit-filter-off-face
  '((t :inherit shadow :strike-through t))
  "Face for a filter chip currently excluded."
  :group 'beads-ralph-cockpit)

;;; Buffer-local state

(defvar-local beads-ralph-cockpit--filters
    '(active paused done stopped failed)
  "Badge groups currently shown.  A subset of the canonical 5.")

(defvar-local beads-ralph-cockpit--rerender-timer nil
  "Pending idle-timer for a debounced re-render, or nil.")

(defvar-local beads-ralph-cockpit--refresh-timer nil
  "1s repeating timer for cost/elapsed updates, or nil when suspended.")

;;; Pure row data

(defconst beads-ralph-cockpit--badge-groups
  '(active paused done stopped failed)
  "Canonical ordering of the 5 badge groups used by the filter bar.")

(defun beads-ralph-cockpit--badge-group (status)
  "Return the badge group (one of `beads-ralph-cockpit--badge-groups') for STATUS.
Collapses the controller's 6 raw statuses to 5 groups; an unknown
status falls into `active' so a typo never hides a live loop."
  (pcase status
    ((or 'running 'cooling-down 'idle) 'active)
    ((or 'auto-paused 'paused) 'paused)
    ('done 'done)
    ('stopped 'stopped)
    ('failed 'failed)
    (_ 'active)))

(defun beads-ralph-cockpit--badge-annotation (status)
  "Return a short annotation string for STATUS, or nil.
Used to distinguish the collapsed sub-states inside a badge group:
`cooling-down' renders as \"active (cooling)\", `auto-paused' as
\"paused (auto)\"."
  (pcase status
    ('cooling-down "cooling")
    ('auto-paused "auto")
    (_ nil)))

(defun beads-ralph-cockpit--badge-label (status)
  "Return the rendered badge label for STATUS (group plus annotation)."
  (let* ((group (beads-ralph-cockpit--badge-group status))
         (annot (beads-ralph-cockpit--badge-annotation status)))
    (if annot
        (format "%s (%s)" group annot)
      (symbol-name group))))

(defun beads-ralph-cockpit--badge-face (status)
  "Return the face symbol for STATUS's badge group."
  (pcase (beads-ralph-cockpit--badge-group status)
    ('active 'beads-ralph-cockpit-badge-active-face)
    ('paused 'beads-ralph-cockpit-badge-paused-face)
    ('done 'beads-ralph-cockpit-badge-done-face)
    ('stopped 'beads-ralph-cockpit-badge-stopped-face)
    ('failed 'beads-ralph-cockpit-badge-failed-face)))

(defun beads-ralph-cockpit--format-elapsed (started-at)
  "Return a human-readable elapsed-time string since STARTED-AT.
Reuses the dashboard's formatter so the cockpit and dashboard
agree on the canonical units."
  (beads-agent-ralph-dashboard--format-elapsed started-at))

(defun beads-ralph-cockpit--format-cost (cost)
  "Return COST formatted as \"$X.XXXX\" or \"$—\" when nil."
  (if (numberp cost)
      (format "$%.4f" cost)
    "$—"))

(defconst beads-ralph-cockpit--contested-regexp
  "\\`Claim failed for "
  "Regexp matching the banner-log text for a contested claim.
Kept in sync with the banner pushed in `beads-agent-ralph.el' on a
failed `bd update --claim'.")

(defun beads-ralph-cockpit--contested-p (controller)
  "Return non-nil when CONTROLLER's most recent notice is a contested claim.
The controller pushes a `notice'-severity banner with text
matching `beads-ralph-cockpit--contested-regexp' when a
`bd update --claim' loses the race.  The cockpit surfaces this on
the row so the user notices when two loops compete for one issue."
  (let ((log (oref controller banner-log)))
    (when log
      (let ((top (car log)))
        (and (eq (plist-get top :severity) 'notice)
             (let ((text (plist-get top :text)))
               (and (stringp text)
                    (string-match-p beads-ralph-cockpit--contested-regexp
                                    text))))))))

(defun beads-ralph-cockpit--row (controller)
  "Return a plist of row data extracted from CONTROLLER.
Pure: takes a controller, returns a plist with everything the row
renderer needs.  Kept separate from rendering so tests can drive
it without standing up vui."
  (list :root-id (or (oref controller root-id) "?")
        :kind (or (oref controller root-kind) 'issue)
        :status (or (oref controller status) 'idle)
        :iter (or (oref controller iteration) 0)
        :max-iter (or (oref controller max-iterations) 0)
        :cost (oref controller cumulative-cost-usd)
        :started-at (oref controller started-at)
        :current (oref controller current-issue-id)
        :contested (beads-ralph-cockpit--contested-p controller)))

;;; Filters

(defun beads-ralph-cockpit--filtered (controllers filters)
  "Return CONTROLLERS whose badge group is in FILTERS."
  (cl-remove-if-not
   (lambda (c)
     (memq (beads-ralph-cockpit--badge-group (oref c status)) filters))
   controllers))

(defun beads-ralph-cockpit--badge-counts (controllers)
  "Return an alist (GROUP . COUNT) for CONTROLLERS.
Includes every entry in `beads-ralph-cockpit--badge-groups' so the
header line shows zeros for empty groups instead of dropping them."
  (mapcar (lambda (group)
            (cons group
                  (cl-count-if
                   (lambda (c)
                     (eq group
                         (beads-ralph-cockpit--badge-group
                          (oref c status))))
                   controllers)))
          beads-ralph-cockpit--badge-groups))

;;; Render: header

(defun beads-ralph-cockpit--header-line (controllers)
  "Return the cockpit header string for CONTROLLERS."
  (let* ((total (length controllers))
         (counts (beads-ralph-cockpit--badge-counts controllers))
         (parts (mapconcat
                 (lambda (cell)
                   (format "%d %s" (cdr cell) (car cell)))
                 counts " · ")))
    (format "Ralph Cockpit — %d loops (%s)" total parts)))

;;; Render: filter bar

(defun beads-ralph-cockpit--filter-chip (group included)
  "Return a string for one filter chip.
GROUP is one of `beads-ralph-cockpit--badge-groups'; INCLUDED is
non-nil when the chip is currently in `--filters'.

Each chip renders as \"[GROUP ✓]\" or \"[GROUP ✗]\".  The chip
group is recovered at point by parsing the line text in
`--filter-group-at-point' rather than via a text property,
because `vui-text' does not preserve custom text properties
across mounts (`face' survives; arbitrary keys do not)."
  (propertize (format "[%s %s]" (symbol-name group) (if included "✓" "✗"))
              'face (if included
                        'beads-ralph-cockpit-filter-on-face
                      'beads-ralph-cockpit-filter-off-face)))

(defconst beads-ralph-cockpit--filter-bar-prefix "Filter: "
  "Literal prefix used to identify the filter-bar line at point.")

(defun beads-ralph-cockpit--filter-bar (filters)
  "Return the filter-bar vnode rendered from FILTERS.
The line is `Filter: [active ✓] [paused ✓] …'; the prefix is
matched by `--filter-group-at-point' to detect the bar."
  (vui-text
   (concat
    beads-ralph-cockpit--filter-bar-prefix
    (mapconcat
     (lambda (group)
       (beads-ralph-cockpit--filter-chip group (memq group filters)))
     beads-ralph-cockpit--badge-groups
     " "))))

;;; Render: row

(defun beads-ralph-cockpit--row-vnode (controller)
  "Return a vnode for one CONTROLLER row.
Each column is propertized with its own face so the row reads at
a glance.  The current-issue and contested-claim trailers render
inline."
  (let* ((row (beads-ralph-cockpit--row controller))
         (id (plist-get row :root-id))
         (kind (symbol-name (plist-get row :kind)))
         (badge (beads-ralph-cockpit--badge-label (plist-get row :status)))
         (badge-face (beads-ralph-cockpit--badge-face
                      (plist-get row :status)))
         (iter (format "iter %d/%d"
                       (plist-get row :iter)
                       (plist-get row :max-iter)))
         (cost (beads-ralph-cockpit--format-cost (plist-get row :cost)))
         (elapsed (beads-ralph-cockpit--format-elapsed
                   (plist-get row :started-at)))
         (current (or (plist-get row :current) "—"))
         (contested (plist-get row :contested))
         (line (concat
                "  "
                (propertize (format "%-12s" id)
                            'face 'beads-ralph-cockpit-id-face)
                " "
                (propertize (format "%-5s" kind)
                            'face 'beads-ralph-cockpit-kind-face)
                " "
                (propertize (format "%-18s" badge)
                            'face badge-face)
                " "
                (propertize (format "%-10s" iter)
                            'face 'default)
                " "
                (propertize (format "%-8s" cost)
                            'face 'beads-ralph-cockpit-cost-face)
                " "
                (propertize (format "%-5s" elapsed)
                            'face 'beads-ralph-cockpit-elapsed-face)
                " "
                (propertize current 'face 'default)
                (when contested
                  (concat "  "
                          (propertize "claim-contested → skipped"
                                      'face
                                      'beads-ralph-cockpit-contested-face))))))
    (vui-text line)))

(defun beads-ralph-cockpit--separator ()
  "Return the horizontal separator vnode."
  (vui-text (make-string 80 ?─)
            :face 'beads-ralph-cockpit-separator-face))

(defun beads-ralph-cockpit--action-bar ()
  "Return the action-bar legend vnode.
The legend must mirror the bindings in
`beads-ralph-cockpit-mode-map'; `n' / `M-p' navigate rows
\(`p' is taken by pause)."
  (vui-text
   (concat
    "[SPC]switch · [s]top · [p]ause · [r]esume · [k]ill · "
    "[n]/[M-p]move · [N]ew · [G]c-done · [f]ilter · [g]refresh · [q]uit")
   :face 'shadow))

(defun beads-ralph-cockpit--rows-region (controllers filters)
  "Return the vnode for the body of the loop table.
Renders either the filtered rows or a `(no matching loops)' hint."
  (let ((visible (beads-ralph-cockpit--filtered controllers filters)))
    (if (null visible)
        (vui-text "  (no matching loops)" :face 'shadow)
      (apply #'vui-vstack
             (mapcar #'beads-ralph-cockpit--row-vnode visible)))))

(vui-defcomponent beads-ralph-cockpit--root (controllers filters)
  "Top-level cockpit composition.
CONTROLLERS is the full registry (already a fresh copy); FILTERS
is the active badge-group subset."
  :render
  (vui-error-boundary
   :id 'beads-ralph-cockpit
   :fallback
   (lambda (err)
     (vui-vstack
       (vui-text (format "Cockpit render error: %S" err) :face 'error)
       (vui-text "(re-render will retry; see *Messages*)" :face 'shadow)))
   :children
   (list
    (vui-vstack
      (vui-text (beads-ralph-cockpit--header-line controllers)
                :face 'beads-ralph-cockpit-header-face)
      (beads-ralph-cockpit--separator)
      (beads-ralph-cockpit--filter-bar filters)
      (vui-text "")
      (beads-ralph-cockpit--rows-region controllers filters)
      (vui-text "")
      (beads-ralph-cockpit--action-bar)))))

;;; Render driver

(defun beads-ralph-cockpit--render (buffer)
  "Mount the cockpit component into BUFFER from the current registry."
  (with-current-buffer buffer
    (unless (eq major-mode 'beads-ralph-cockpit-mode)
      (beads-ralph-cockpit-mode))
    (add-hook 'kill-buffer-hook
              #'beads-ralph-cockpit--kill-buffer-cleanup
              nil t)
    (let ((controllers (beads-agent-ralph-controllers))
          (filters beads-ralph-cockpit--filters))
      (vui-mount
       (vui-component 'beads-ralph-cockpit--root
                      :controllers controllers
                      :filters filters)
       (buffer-name buffer))
      ;; The refresh timer state depends on whether any loop is
      ;; currently in running/cooling-down; reconcile on every render
      ;; so a controller transitioning into running re-arms the timer.
      (beads-ralph-cockpit--reconcile-refresh-timer buffer))))

;;; Point navigation / row resolution

(defun beads-ralph-cockpit--root-id-at-point ()
  "Return the root-id on the current line, or nil.
Cockpit rows start with a two-space indent then the id."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  \\([A-Za-z0-9.-]+\\)\\s-")
      (let ((id (match-string-no-properties 1)))
        (when (beads-agent-ralph-controller-for-root id)
          id)))))

(defun beads-ralph-cockpit--controller-at-point ()
  "Return the controller at point, or signal a `user-error'."
  (let ((id (beads-ralph-cockpit--root-id-at-point)))
    (unless id
      (user-error "No loop at point"))
    (or (beads-agent-ralph-controller-for-root id)
        (user-error "No registered controller for %s" id))))

;;; Filter chip at point

(defun beads-ralph-cockpit--filter-group-at-point ()
  "Return the filter group symbol the column at point lies inside, or nil.
Parses the filter-bar line text (not text properties — see the
note in `--filter-chip') for chips of the form \"[GROUP ✓/✗]\"
and returns the GROUP whose column range covers point."
  (save-excursion
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line (buffer-substring-no-properties bol eol))
           (col (- (point) bol)))
      (when (string-prefix-p beads-ralph-cockpit--filter-bar-prefix line)
        (let ((start 0)
              (hit nil))
          (while (and (not hit)
                      (string-match "\\[\\([a-z]+\\) [✓✗]\\]" line start))
            (let ((cs (match-beginning 0))
                  (ce (match-end 0)))
              (if (and (>= col cs) (< col ce))
                  (setq hit (intern (match-string 1 line)))
                (setq start ce))))
          (and hit
               (memq hit beads-ralph-cockpit--badge-groups)
               hit))))))

;;; Interactive commands

(defun beads-ralph-cockpit-refresh ()
  "Force a re-render of the cockpit buffer."
  (interactive)
  (beads-ralph-cockpit--render (current-buffer)))

(defun beads-ralph-cockpit-switch ()
  "Switch to the per-loop dashboard for the row at point."
  (interactive)
  (let ((ctrl (beads-ralph-cockpit--controller-at-point)))
    (beads-agent-ralph-dashboard-mount ctrl)))

(defun beads-ralph-cockpit-stop ()
  "Stop the loop at point.
Refuses with a `user-error' when the controller is already in a
terminal state — calling `beads-agent-ralph-stop' on a terminal
controller would re-arm its eviction timer and extend retention."
  (interactive)
  (let ((ctrl (beads-ralph-cockpit--controller-at-point)))
    (when (beads-agent-ralph--terminal-p ctrl)
      (user-error "%s is already %s; nothing to stop"
                  (oref ctrl root-id) (oref ctrl status)))
    (beads-agent-ralph-stop ctrl)
    (beads-ralph-cockpit-refresh)))

(defun beads-ralph-cockpit-pause ()
  "Pause the loop at point.
Refuses on terminal controllers — pausing `done' / `stopped' /
`failed' would incorrectly resurrect them into `auto-paused'."
  (interactive)
  (let ((ctrl (beads-ralph-cockpit--controller-at-point)))
    (when (beads-agent-ralph--terminal-p ctrl)
      (user-error "%s is %s; cannot pause a terminal controller"
                  (oref ctrl root-id) (oref ctrl status)))
    (beads-agent-ralph--pause ctrl "Paused by user")
    (beads-ralph-cockpit-refresh)))

(defun beads-ralph-cockpit-resume ()
  "Resume the loop at point from `auto-paused' or `stopped'.
Mirrors the dashboard's resume contract (`auto-paused' and the
user-initiated `stopped' are the only resumable states; the
controller never enters a separate `paused' status)."
  (interactive)
  (let ((ctrl (beads-ralph-cockpit--controller-at-point)))
    (unless (memq (oref ctrl status) '(auto-paused stopped))
      (user-error "%s is %s; only auto-paused or stopped can be resumed"
                  (oref ctrl root-id) (oref ctrl status)))
    ;; A resumed-from-stopped controller has an eviction timer armed
    ;; by `--terminate'; cancel it so the resumed loop is not silently
    ;; unregistered partway through.
    (beads-agent-ralph--cancel-eviction-timer ctrl)
    (oset ctrl done-reason nil)
    (beads-agent-ralph--set-status ctrl 'cooling-down)
    (beads-agent-ralph--schedule-next-iteration
     ctrl (lambda ()
            (beads-agent-ralph--set-status ctrl 'running)
            (beads-agent-ralph--run-iteration ctrl)))
    (beads-ralph-cockpit-refresh)))

(defun beads-ralph-cockpit-kill-iter ()
  "Kill the in-flight iteration of the loop at point.
A no-op (in `beads-agent-ralph-kill-iter') when no stream is in
flight, so calling on cooling-down / paused is safe; refuses on
terminal controllers to avoid confusing the user."
  (interactive)
  (let ((ctrl (beads-ralph-cockpit--controller-at-point)))
    (when (beads-agent-ralph--terminal-p ctrl)
      (user-error "%s is %s; no iteration to kill"
                  (oref ctrl root-id) (oref ctrl status)))
    (beads-agent-ralph-kill-iter ctrl)
    (beads-ralph-cockpit-refresh)))

(defun beads-ralph-cockpit-new-loop ()
  "Open the epic browser to pick an epic to launch on."
  (interactive)
  (if (fboundp 'beads-ralph-epic-browser)
      (call-interactively 'beads-ralph-epic-browser)
    (user-error
     "`beads-ralph-epic-browser' not available; require it first")))

(defun beads-ralph-cockpit-gc-done ()
  "Remove terminal (done / stopped / failed) controllers from the registry.
Cancels each evicted controller's pending eviction timer first
(set by `--terminate' on the way into the terminal state) so a
late tick cannot fire against an already-unregistered controller."
  (interactive)
  (let ((removed 0))
    (dolist (c (beads-agent-ralph-controllers))
      (when (beads-agent-ralph--terminal-p c)
        (beads-agent-ralph--cancel-eviction-timer c)
        (beads-agent-ralph--unregister-controller c)
        (cl-incf removed)))
    (message "Cockpit GC: dropped %d terminal controller(s)" removed)
    (beads-ralph-cockpit-refresh)))

(defun beads-ralph-cockpit-toggle-filter ()
  "Toggle the filter chip at point, or cycle the next chip if no chip."
  (interactive)
  (let ((group (beads-ralph-cockpit--filter-group-at-point)))
    (unless group
      (user-error "Point not on a filter chip"))
    (if (memq group beads-ralph-cockpit--filters)
        (setq-local beads-ralph-cockpit--filters
                    (remq group beads-ralph-cockpit--filters))
      (setq-local beads-ralph-cockpit--filters
                  (cons group beads-ralph-cockpit--filters)))
    (beads-ralph-cockpit-refresh)))

(defun beads-ralph-cockpit-next-row ()
  "Move point to the next row."
  (interactive)
  (forward-line 1))

(defun beads-ralph-cockpit-previous-row ()
  "Move point to the previous row."
  (interactive)
  (forward-line -1))

;;; State-change subscription + debounce

(defun beads-ralph-cockpit--flush-rerender (buffer)
  "Re-render BUFFER if it is still live and showing the cockpit."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local beads-ralph-cockpit--rerender-timer nil)
      (when (eq major-mode 'beads-ralph-cockpit-mode)
        (condition-case err
            (beads-ralph-cockpit--render buffer)
          (error
           (message "beads-ralph-cockpit: render errored: %S" err)))))))

(defun beads-ralph-cockpit--schedule-rerender (buffer)
  "Schedule a debounced re-render of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq major-mode 'beads-ralph-cockpit-mode)
                 (not beads-ralph-cockpit--rerender-timer))
        (setq-local beads-ralph-cockpit--rerender-timer
                    (run-with-idle-timer
                     beads-ralph-cockpit-render-debounce
                     nil
                     #'beads-ralph-cockpit--flush-rerender
                     buffer))))))

(defun beads-ralph-cockpit--on-state-change (_controller _status)
  "Hook on `beads-agent-ralph-state-change-functions'."
  (let ((buf (get-buffer beads-ralph-cockpit-buffer-name)))
    (when buf
      (beads-ralph-cockpit--schedule-rerender buf))))

(add-hook 'beads-agent-ralph-state-change-functions
          #'beads-ralph-cockpit--on-state-change)

;;; 1s refresh timer

(defun beads-ralph-cockpit--any-active-p ()
  "Return non-nil when at least one controller is running or cooling-down.
Read through the public accessor so this module does not depend on
the registry's internal representation."
  (cl-some (lambda (c)
             (memq (oref c status) '(running cooling-down)))
           (beads-agent-ralph-controllers)))

(defun beads-ralph-cockpit--reconcile-refresh-timer (buffer)
  "Start or stop the 1s refresh timer based on registry activity."
  (with-current-buffer buffer
    (cond
     ((and (beads-ralph-cockpit--any-active-p)
           (not beads-ralph-cockpit--refresh-timer))
      (setq-local beads-ralph-cockpit--refresh-timer
                  (run-at-time beads-ralph-cockpit-refresh-interval
                               beads-ralph-cockpit-refresh-interval
                               #'beads-ralph-cockpit--refresh-tick
                               buffer)))
     ((and (not (beads-ralph-cockpit--any-active-p))
           beads-ralph-cockpit--refresh-timer)
      (cancel-timer beads-ralph-cockpit--refresh-timer)
      (setq-local beads-ralph-cockpit--refresh-timer nil)))))

(defun beads-ralph-cockpit--refresh-tick (buffer)
  "Timer callback: re-render BUFFER unless dead.
On a dead buffer cancels every timer pointing at this function so a
late tick after a kill-buffer that bypassed `kill-buffer-hook'
\(e.g. `kill-emacs') cannot keep firing."
  (if (not (buffer-live-p buffer))
      (cancel-function-timers #'beads-ralph-cockpit--refresh-tick)
    (with-current-buffer buffer
      (when (eq major-mode 'beads-ralph-cockpit-mode)
        (condition-case err
            (beads-ralph-cockpit--render buffer)
          (error
           (message "beads-ralph-cockpit: tick errored: %S" err)))))))

(defun beads-ralph-cockpit--kill-buffer-cleanup ()
  "Cancel pending timers when the cockpit buffer is killed."
  (when (timerp beads-ralph-cockpit--rerender-timer)
    (cancel-timer beads-ralph-cockpit--rerender-timer))
  (when (timerp beads-ralph-cockpit--refresh-timer)
    (cancel-timer beads-ralph-cockpit--refresh-timer)))

;;; Mode + keymap

(defvar beads-ralph-cockpit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'beads-ralph-cockpit-switch)
    (define-key map (kbd "RET") #'beads-ralph-cockpit-switch)
    (define-key map (kbd "s") #'beads-ralph-cockpit-stop)
    (define-key map (kbd "p") #'beads-ralph-cockpit-pause)
    (define-key map (kbd "r") #'beads-ralph-cockpit-resume)
    (define-key map (kbd "k") #'beads-ralph-cockpit-kill-iter)
    (define-key map (kbd "N") #'beads-ralph-cockpit-new-loop)
    (define-key map (kbd "G") #'beads-ralph-cockpit-gc-done)
    (define-key map (kbd "f") #'beads-ralph-cockpit-toggle-filter)
    (define-key map (kbd "g") #'beads-ralph-cockpit-refresh)
    (define-key map (kbd "n") #'beads-ralph-cockpit-next-row)
    (define-key map (kbd "M-p") #'beads-ralph-cockpit-previous-row)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-ralph-cockpit-mode'.
Note: `p' is bound to pause (matching the per-loop dashboard); use
`M-p' for the previous-row motion since `p' is taken.")

(define-derived-mode beads-ralph-cockpit-mode vui-mode
  "Beads-Cockpit"
  "Major mode for the Ralph multi-loop cockpit.

\\{beads-ralph-cockpit-mode-map}"
  (setq truncate-lines t))

;;; Entry point

;;;###autoload
(defun beads-ralph-cockpit ()
  "Open the singleton Ralph cockpit buffer."
  (interactive)
  (let ((buf (get-buffer-create beads-ralph-cockpit-buffer-name)))
    (beads-ralph-cockpit--render buf)
    (pop-to-buffer buf)
    buf))

(provide 'beads-ralph-cockpit)

;;; beads-ralph-cockpit.el ends here
