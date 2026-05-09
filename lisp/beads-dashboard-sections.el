;;; beads-dashboard-sections.el --- Async sections for beads-dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Per-section UI primitives for `beads-dashboard'.  Provides the
;; `beads-dashboard--section' helper component (dispatches
;; loading/empty/error/ready states inside a `vui-error-boundary') and
;; the loader/render functions wired up by the dashboard root.  All
;; loaders go through `beads-command-execute-async' so the global
;; concurrency policy applies uniformly.

;;; Code:

(require 'eieio)
(require 'vui)
(require 'beads-command)
(require 'beads-section)
(require 'beads-types)

;; Verified `--json' command classes consumed by the dashboard.
(require 'beads-command-blocked)
(require 'beads-command-epic)
(require 'beads-command-federation)
(require 'beads-command-list)
(require 'beads-command-misc)
(require 'beads-command-ready)
(require 'beads-command-stale)
(require 'beads-command-status)

;;; Customisation

(defcustom beads-dashboard-section-limit 10
  "Maximum number of issues rendered per dashboard section.

When a section has more issues than this limit, only the first N are
rendered and a `… and M more' line is appended.  Set to nil to render
every issue (the section header always shows the full count).

Applies to: Stale, Orphans, In Flight, Ready, Blocked, Recently
Closed, and the Epic Progress list."
  :type '(choice (integer :tag "Limit")
                 (const :tag "Unlimited" nil))
  :group 'beads)

;;; Helper Component

(defun beads-dashboard--toggle-glyph (collapsed)
  "Return the section toggle glyph for COLLAPSED state."
  (if collapsed
      beads-section-glyph-collapsed
    beads-section-glyph-expanded))

(defun beads-dashboard--section-header (title icon collapsed count on-toggle)
  "Return a vui vnode for a section header.
TITLE is the section name.  ICON is an optional decorative prefix.
COLLAPSED governs the chevron glyph.  COUNT, when non-nil, is shown
as a parenthesized count.  ON-TOGGLE is invoked on click."
  (let* ((glyph (beads-dashboard--toggle-glyph collapsed))
         (head  (concat glyph
                        (if icon (concat " " icon) "")
                        " " title
                        (if count (format " (%d)" count) ""))))
    (vui-button head
      :no-decoration t
      :face 'bold
      :help-echo nil
      :on-click (or on-toggle (lambda () (ignore))))))

(defun beads-dashboard--error-line (err)
  "Return a vnode rendering ERR as a single dimmed error line."
  (vui-text (format "  Error: %s"
                    (cond
                     ((stringp err) err)
                     ((and (listp err) (stringp (car err))) (car err))
                     (t (format "%S" err))))
            :face 'error))

(defun beads-dashboard--loading-line ()
  "Return a vnode for the loading skeleton.
Renders a single dimmed line so the section reserves space and the
buffer does not reflow when data arrives."
  (vui-text "  Loading…" :face 'shadow))

(defun beads-dashboard--empty-line (&optional message)
  "Return a vnode for an empty state with optional MESSAGE."
  (vui-text (concat "  " (or message "Nothing to show."))
            :face 'shadow))

(defun beads-dashboard--data-empty-p (data)
  "Return non-nil when DATA represents an empty list or vector payload."
  (or (null data)
      (and (sequencep data) (zerop (length data)))))

(vui-defcomponent beads-dashboard--section
    (title async-key load render-ready render-empty render-error
           collapsed on-toggle icon force-render hide-count section-key)
  "Async dashboard section dispatching loading/empty/error/ready states.

PROPS:
  :title        Section heading text.
  :section-key  Symbolic key (e.g. \\='ready, \\='blocked) stamped on every
                rendered line as a `beads-dashboard-section-key' text
                property so `beads-dashboard-toggle-section' can find
                the enclosing section regardless of point.
  :async-key    Key passed to `vui-use-async'.  Bump to invalidate.
  :load         Loader (lambda (resolve reject) ...) for `vui-use-async'.
  :render-ready (lambda (data)) returning a vnode for the populated state.
  :render-empty (lambda ()) returning a vnode when data is empty.
  :render-error (lambda (err))  returning a vnode for the error state.
  :collapsed    Non-nil to render only the header.
  :on-toggle    Click callback for the header chevron.
  :icon         Optional icon string shown before the title.
  :force-render When non-nil, render-ready is called even for empty data
                (used by the stats strip where 0 is a meaningful value).
  :hide-count   When non-nil, suppress the (N) count in the header
                (use for non-list data like the stats alist, and for
                collapsed sections whose count would just be 0).

The loader is invoked through `vui-use-async' and is expected to call
RESOLVE with the parsed payload or REJECT with an error condition.
The whole component is wrapped in `vui-error-boundary' so a render-time
failure in one section never blanks the dashboard."
  :render
  (vui-error-boundary
   :id (list 'beads-dashboard-section async-key)
   :fallback
   (lambda (err)
     (vui-vstack
       (beads-dashboard--section-header title icon collapsed nil on-toggle)
       (unless collapsed
         (beads-dashboard--error-line
          (if (consp err) (cadr err) (format "%S" err))))))
   :children
   (list
    (let* ((async (vui-use-async async-key load))
           (status (plist-get async :status))
           (data   (plist-get async :data))
           (err    (plist-get async :error))
           ;; Collapsed sections install a no-op loader returning nil,
           ;; so their length would be 0 — suppress the count until expanded.
           (count  (cond
                    (hide-count nil)
                    (collapsed nil)
                    ((eq status 'ready)
                     (cond ((listp data) (length data))
                           ((vectorp data) (length data))
                           (t nil)))
                    (t nil))))
      (vui-vstack
        (beads-dashboard--section-header title icon collapsed count on-toggle)
        (unless collapsed
          (pcase status
            ('pending (beads-dashboard--loading-line))
            ('error
             (if render-error
                 (funcall render-error err)
               (beads-dashboard--error-line err)))
            ('ready
             (cond
              ((and (not force-render)
                    (beads-dashboard--data-empty-p data))
               (if render-empty
                   (funcall render-empty)
                 (beads-dashboard--empty-line)))
              (t (if render-ready
                     (funcall render-ready data)
                   (vui-text (format "  %S" data) :face 'shadow))))))))))))

;;; Section Spec Helpers

(defun beads-dashboard--make-loader (cmd cache-key)
  "Return a `vui-use-async' loader thunk to run CMD asynchronously.
CMD is a `beads-command' instance.  CACHE-KEY is forwarded to
`beads-command-execute-async' for single-flight coalescing."
  (lambda (resolve reject)
    (condition-case spawn-err
        (beads-command-execute-async
         cmd resolve reject
         :queue 'auto
         :cache-key cache-key
         :timeout beads-command-async-timeout)
      (error (funcall reject (error-message-string spawn-err))))))

(defun beads-dashboard--more-line (hidden)
  "Return a dimmed `… and N more' line when HIDDEN > 0, else nil."
  (when (and hidden (> hidden 0))
    (vui-text (format "  … and %d more" hidden) :face 'shadow)))

(defun beads-dashboard--limited-vstack (items render-fn &optional extra-leading-rows)
  "Render ITEMS via RENDER-FN respecting `beads-dashboard-section-limit'.
EXTRA-LEADING-ROWS, when non-nil, is a function called with the visible
items that returns a list of vnodes used in place of the per-item
mapcar — useful for grouped layouts that interleave header rows."
  (let* ((limit beads-dashboard-section-limit)
         (total (length items))
         (visible (if (and limit (> total limit))
                      (seq-take items limit)
                    items))
         (rows (if extra-leading-rows
                   (funcall extra-leading-rows visible)
                 (mapcar render-fn visible)))
         (more (beads-dashboard--more-line
                (and limit (- total (length visible))))))
    (apply #'vui-vstack
           (append rows (when more (list more))))))

(defun beads-dashboard--render-issue-list (issues)
  "Return a vstack of issue line vnodes, one per ISSUES element.
Truncated to `beads-dashboard-section-limit' (when set), with a
trailing `… and N more' line indicating the elided count."
  (beads-dashboard--limited-vstack
   issues #'beads-section--issue-line-vnode))

(defun beads-dashboard--issue-not-blocker-p (issue)
  "Return non-nil when ISSUE is not actively blocked.
Filters statuses that the existing `beads-section' code excludes from
the blocked list (e.g., \"hooked\" / \"in_progress\")."
  (let ((status (oref issue status)))
    (not (member status (list "hooked" beads-status-in-progress)))))


;;; Stats Strip

(defun beads-dashboard--stats-loader ()
  "Return a `vui-use-async' loader for `bd stats --json'."
  (beads-dashboard--make-loader
   (beads-command-status :json t) '(stats)))

(defun beads-dashboard-render-stats (data)
  "Render the stats strip from DATA, the parsed `bd stats' JSON.
DATA is an alist with a `summary' subobject; we surface the most
informative counters as a single horizontal line of buttons."
  (let* ((summary (cond
                   ((listp data) (alist-get 'summary data))
                   (t nil)))
         (open    (or (alist-get 'open_issues summary) 0))
         (ready   (or (alist-get 'ready_issues summary) 0))
         (inprog  (or (alist-get 'in_progress_issues summary) 0))
         (blocked (or (alist-get 'blocked_issues summary) 0))
         (closed  (or (alist-get 'closed_issues summary) 0)))
    (vui-hstack
      :spacing 2
      (vui-text (format "Open %d" open) :face 'font-lock-keyword-face)
      (vui-text (format "Ready %d" ready) :face 'success)
      (vui-text (format "In-progress %d" inprog) :face 'font-lock-function-name-face)
      (vui-text (format "Blocked %d" blocked) :face 'warning)
      (vui-text (format "Closed %d" closed) :face 'shadow))))

;;; Attention: Stale and Orphans

(defun beads-dashboard--stale-loader ()
  "Return a loader for `bd stale --json'."
  (beads-dashboard--make-loader
   (beads-command-stale :json t) '(stale)))

(defun beads-dashboard--orphans-loader ()
  "Return a loader for `bd orphans --json'."
  (beads-dashboard--make-loader
   (beads-command-orphans :json t) '(orphans)))

;;; In Flight

(defun beads-dashboard--in-flight-loader ()
  "Return a loader for in-progress issues."
  (beads-dashboard--make-loader
   (beads-command-list :status beads-status-in-progress :json t)
   '(list in_progress)))

(defun beads-dashboard-render-in-flight (issues)
  "Render the In Flight section from ISSUES, grouped by assignee.
Honours `beads-dashboard-section-limit' across all assignees."
  (beads-dashboard--limited-vstack
   issues nil
   (lambda (visible)
     (mapcan
      (lambda (group)
        (cons (vui-text (car group) :face 'font-lock-variable-name-face)
              (mapcar #'beads-section--issue-line-vnode (cdr group))))
      (seq-group-by
       (lambda (i)
         (or (and (slot-exists-p i 'assignee)
                  (slot-boundp i 'assignee)
                  (oref i assignee))
             "unassigned"))
       visible)))))

;;; Ready

(defun beads-dashboard--ready-loader ()
  "Return a loader for `bd ready --json'."
  (beads-dashboard--make-loader
   (beads-command-ready :json t) '(ready)))

(defun beads-dashboard-render-ready (issues)
  "Render the Ready section from ISSUES, sorted by priority."
  (beads-dashboard--render-issue-list
   (seq-sort-by (lambda (i) (or (oref i priority) 99)) #'< issues)))

;;; Blocked

(defun beads-dashboard--blocked-loader ()
  "Return a loader for `bd blocked --json'."
  (beads-dashboard--make-loader
   (beads-command-blocked :json t) '(blocked)))

(defun beads-dashboard-render-blocked (issues)
  "Render the Blocked section from ISSUES, filtering active work."
  (beads-dashboard--render-issue-list
   (seq-filter #'beads-dashboard--issue-not-blocker-p issues)))

;;; Recently Closed

(defun beads-dashboard--closed-loader ()
  "Return a loader for recently closed issues.
`bd list --sort closed' already returns most-recent first."
  (beads-dashboard--make-loader
   (beads-command-list :status beads-status-closed
                       :sort "closed"
                       :limit (or beads-dashboard-section-limit 25)
                       :json t)
   '(list closed)))

(defun beads-dashboard-render-closed (issues)
  "Render the Recently Closed section from ISSUES.
The CLI already sorted by closed-at descending; we just trim to the
section limit and append a `… and N more' line when truncated."
  (beads-dashboard--render-issue-list issues))

;;; Epic Progress

(defun beads-dashboard--epic-loader ()
  "Return a loader for `bd epic status --json'."
  (beads-dashboard--make-loader
   (beads-command-epic-status :json t) '(epic-status)))

(defun beads-dashboard-render-epic (data)
  "Render the Epic Progress section from DATA.
DATA is a list of `beads-epic-status' instances; each carries an
`epic' slot (a `beads-issue') plus completion counters."
  (cond
   ((null data) (beads-dashboard--empty-line "No epics open."))
   ((listp data)
    (beads-dashboard--limited-vstack
     data #'beads-dashboard--epic-status-line-vnode))
   (t (vui-text (format "  %S" data) :face 'shadow))))

(defun beads-dashboard--epic-status-line-vnode (status)
  "Render one `beads-epic-status' STATUS as a clickable button line.
Includes the epic id, completion percentage, and title.  Carries the
`beads-section' text-property contract via the underlying
`beads-section--issue-line-vnode' so RET/c/b/eldoc still work."
  (let* ((epic (and (slot-boundp status 'epic) (oref status epic)))
         (total (and (slot-boundp status 'total-children)
                     (oref status total-children)))
         (closed (and (slot-boundp status 'closed-children)
                      (oref status closed-children)))
         (pct (cond
               ((not (and (numberp total) (numberp closed) (> total 0))) 0)
               (t (round (* 100.0 (/ (float closed) total))))))
         (id (and epic (oref epic id)))
         (title (and epic (oref epic title))))
    (cond
     ((not epic) (vui-text "  (malformed epic-status entry)" :face 'shadow))
     (t
      (beads-section--plain-button
       (beads-section--propertize
        (format "  %-14s %3d%%  %d/%d  %s"
                (or id "?") pct (or closed 0) (or total 0)
                (or title ""))
        (beads-issue-section :issue epic))
       (let ((eid id))
         (lambda () (beads-show eid))))))))

;;; Federation

(defun beads-dashboard--federation-loader ()
  "Return a loader for `bd federation status --json'."
  (beads-dashboard--make-loader
   (beads-command-federation-status :json t) '(federation-status)))

(defun beads-dashboard-render-federation (data)
  "Render the Federation section from DATA."
  (cond
   ((null data) (beads-dashboard--empty-line "No federation peers."))
   ((listp data)
    (apply #'vui-vstack
           (mapcar
            (lambda (entry)
              (vui-text
               (format "  %s" (if (listp entry) (prin1-to-string entry) entry))))
            (if (listp (car-safe data)) data (list data)))))
   (t (vui-text (format "  %S" data) :face 'shadow))))

(provide 'beads-dashboard-sections)
;;; beads-dashboard-sections.el ends here
