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

;; Forward declaration: defined in `beads-dashboard.el', which requires
;; this file (the dependency cannot be reversed).  Called from
;; `beads-dashboard--more-line' on click.
(declare-function beads-dashboard--bump-extra "beads-dashboard"
                  (key delta-or-symbol))

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

(defcustom beads-dashboard-section-batch 10
  "Rows added to a section per `beads-dashboard-load-more' invocation.
Also the step used by `beads-dashboard-load-less'."
  :type 'integer
  :group 'beads)

;;; Effective Limit Resolution

(defun beads-dashboard--effective-display-limit (extra-for-section)
  "Return the visible row count for a section, or nil for unlimited.
EXTRA-FOR-SECTION is nil (no override), a non-negative integer
\(additional rows on top of `beads-dashboard-section-limit'), or the
symbol `all' (unlimited).  Returns nil when the base limit is itself
nil or when EXTRA-FOR-SECTION is `all'."
  (cond
   ((eq extra-for-section 'all) nil)
   ((null beads-dashboard-section-limit) nil)
   (t (+ beads-dashboard-section-limit (or extra-for-section 0)))))

(defun beads-dashboard--effective-fetch-limit (extra-for-section)
  "Return the CLI `--limit' value for a section, or nil for CLI default.
Returns:
  nil  — when EXTRA-FOR-SECTION is nil; the loader passes no --limit
         so the bd CLI default applies (e.g. `bd ready' → 100, `bd
         stale' → 50).  This preserves the dashboard's headline counts
         at rest and keeps the `… and N more' affordance live without
         a heavier upfront fetch.
  0    — for any non-nil EXTRA-FOR-SECTION (integer or the `all'
         sentinel); bd CLIs interpret 0 as unlimited.  Once the user
         has invoked `+'/`-'/`*' on a section, they have asked the
         dashboard to grow beyond the CLI cap, so we fetch the full
         dataset and let `beads-dashboard--limited-vstack' do the
         truncation locally.  This makes the header count monotonic
         under `+' (it reveals the true total instead of dropping
         when the new fetch-limit is below the CLI default) and is
         the only honest way to render the misleading-(100) Ready
         section."
  (cond
   ((null extra-for-section) nil)
   (t 0)))

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
                property so per-section commands
                (`beads-dashboard-load-more', `-load-less', `-load-all',
                `beads-dashboard-toggle-section') can find the
                enclosing section regardless of point.
  :async-key    Key passed to `vui-use-async'.  Bump to invalidate.
                Must include the per-section effective fetch-limit so
                pressing `+' actually re-runs the loader rather than
                re-using a stale smaller payload.
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

(defun beads-dashboard--more-line (section-key hidden)
  "Return a clickable `… and N more' button for SECTION-KEY, or nil.
Returns nil when HIDDEN is nil or zero so callers can unconditionally
append the result.  Clicking the line invokes the same load-more
action that `+' triggers; the section is identified by SECTION-KEY
stored as a text property on the button label."
  (when (and hidden (> hidden 0))
    (let ((label (propertize
                  (format "  … and %d more (+)" hidden)
                  'face 'shadow
                  'beads-dashboard-section-key section-key)))
      (beads-section--plain-button
       label
       (let ((key section-key))
         (lambda ()
           (beads-dashboard--bump-extra
            key beads-dashboard-section-batch)))))))

(defun beads-dashboard--limited-vstack (items render-fn
                                              &optional extra-rows
                                              section-key
                                              extra-leading-rows)
  "Render ITEMS via RENDER-FN respecting display limit + EXTRA-ROWS.
EXTRA-ROWS is nil, a non-negative integer (additional rows on top of
`beads-dashboard-section-limit'), or the symbol `all' (unlimited).
SECTION-KEY is stamped on the trailing `… and N more' button so a
click on it routes to the right section.
EXTRA-LEADING-ROWS, when non-nil, is a function called with the visible
items that returns a list of vnodes used in place of the per-item
mapcar — useful for grouped layouts that interleave header rows.
Per-item RENDER-FN is invoked as (funcall RENDER-FN ITEM SECTION-KEY)
so callers can stamp the section-key onto each row."
  (let* ((limit  (beads-dashboard--effective-display-limit extra-rows))
         (total  (length items))
         (visible (if (and limit (> total limit))
                      (seq-take items limit)
                    items))
         (rows (if extra-leading-rows
                   (funcall extra-leading-rows visible)
                 (mapcar (lambda (it) (funcall render-fn it section-key))
                         visible)))
         (more (beads-dashboard--more-line
                section-key
                (and limit (- total (length visible))))))
    (apply #'vui-vstack
           (append rows (when more (list more))))))

(defun beads-dashboard--render-issue-list (issues &optional section-key extra-rows)
  "Return a vstack of issue line vnodes, one per ISSUES element.
Truncated to the section's effective display limit (computed from
`beads-dashboard-section-limit' plus EXTRA-ROWS), with a trailing
`… and N more' button when more rows are available.  Each row carries
SECTION-KEY as a `beads-dashboard-section-key' text property so the
load-more commands can resolve the enclosing section in O(1)."
  (beads-dashboard--limited-vstack
   issues
   (lambda (issue sk)
     (beads-section--issue-line-vnode
      issue (when sk (list 'beads-dashboard-section-key sk))))
   extra-rows section-key))

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

(defun beads-dashboard--stale-loader (&optional fetch-limit)
  "Return a loader for `bd stale --json'.
FETCH-LIMIT, when non-nil, is forwarded as `--limit' (0 = unlimited)."
  (beads-dashboard--make-loader
   (if fetch-limit
       (beads-command-stale :limit fetch-limit :json t)
     (beads-command-stale :json t))
   '(stale)))

(defun beads-dashboard--orphans-loader ()
  "Return a loader for `bd orphans --json'.
`bd orphans' has no `--limit'; truncation happens locally."
  (beads-dashboard--make-loader
   (beads-command-orphans :json t) '(orphans)))

;;; In Flight

(defun beads-dashboard--in-flight-loader (&optional fetch-limit)
  "Return a loader for in-progress issues.
FETCH-LIMIT, when non-nil, is forwarded as `--limit' to `bd list'."
  (beads-dashboard--make-loader
   (if fetch-limit
       (beads-command-list :status beads-status-in-progress
                           :limit fetch-limit :json t)
     (beads-command-list :status beads-status-in-progress :json t))
   '(list in_progress)))

(defun beads-dashboard-render-in-flight (issues &optional section-key extra-rows)
  "Render the In Flight section from ISSUES as a flat issue list.
SECTION-KEY is stamped on rows; EXTRA-ROWS controls the visible count."
  (beads-dashboard--render-issue-list issues section-key extra-rows))

;;; Ready

(defun beads-dashboard--ready-loader (&optional fetch-limit)
  "Return a loader for `bd ready --json'.
FETCH-LIMIT, when non-nil, is forwarded as `--limit'.  Without this
`bd ready' defaults to --limit 100, which makes the section header's
\(N) count cap out at 100 even when more ready issues exist."
  (beads-dashboard--make-loader
   (if fetch-limit
       (beads-command-ready :limit fetch-limit :json t)
     (beads-command-ready :json t))
   '(ready)))

(defun beads-dashboard-render-ready (issues &optional section-key extra-rows)
  "Render the Ready section from ISSUES, sorted by priority.
SECTION-KEY is stamped on rows; EXTRA-ROWS controls the visible count."
  (beads-dashboard--render-issue-list
   (seq-sort-by (lambda (i) (or (oref i priority) 99)) #'< issues)
   section-key extra-rows))

;;; Blocked

(defun beads-dashboard--blocked-loader ()
  "Return a loader for `bd blocked --json'.
`bd blocked' has no `--limit'; truncation happens locally."
  (beads-dashboard--make-loader
   (beads-command-blocked :json t) '(blocked)))

(defun beads-dashboard-render-blocked (issues &optional section-key extra-rows)
  "Render the Blocked section from ISSUES, filtering active work.
SECTION-KEY is stamped on rows; EXTRA-ROWS controls the visible count."
  (beads-dashboard--render-issue-list
   (seq-filter #'beads-dashboard--issue-not-blocker-p issues)
   section-key extra-rows))

;;; Recently Closed

(defun beads-dashboard--closed-loader (&optional fetch-limit)
  "Return a loader for recently closed issues.
`bd list --sort closed' already returns most-recent first.  FETCH-LIMIT,
when non-nil, is forwarded as `--limit'; otherwise a sensible default
\(25) is used so we never over-fetch closed history."
  (beads-dashboard--make-loader
   (beads-command-list :status beads-status-closed
                       :sort "closed"
                       :limit (or fetch-limit
                                  (or beads-dashboard-section-limit 25))
                       :json t)
   '(list closed)))

(defun beads-dashboard-render-closed (issues &optional section-key extra-rows)
  "Render the Recently Closed section from ISSUES.
The CLI already sorted by closed-at descending; we just trim to the
effective display limit and append a `… and N more' button when
truncated.  SECTION-KEY and EXTRA-ROWS thread through the standard
issue-list path."
  (beads-dashboard--render-issue-list issues section-key extra-rows))

;;; Epic Progress

(defun beads-dashboard--epic-loader ()
  "Return a loader for `bd epic status --json'."
  (beads-dashboard--make-loader
   (beads-command-epic-status :json t) '(epic-status)))

(defun beads-dashboard-render-epic (data &optional section-key extra-rows)
  "Render the Epic Progress section from DATA.
DATA is a list of `beads-epic-status' instances; each carries an
`epic' slot (a `beads-issue') plus completion counters.  SECTION-KEY
is stamped on rows so load-more commands work here too; EXTRA-ROWS
controls the visible count."
  (cond
   ((null data) (beads-dashboard--empty-line "No epics open."))
   ((listp data)
    (beads-dashboard--limited-vstack
     data
     (lambda (status sk) (beads-dashboard--epic-status-line-vnode status sk))
     extra-rows section-key))
   (t (vui-text (format "  %S" data) :face 'shadow))))

(defun beads-dashboard--epic-status-line-vnode (status &optional section-key)
  "Render one `beads-epic-status' STATUS as a clickable button line.
Includes the epic id, completion percentage, and title.  Carries the
`beads-section' text-property contract via `beads-section--propertize'
so RET/c/b/eldoc still work; SECTION-KEY, when non-nil, is stamped as
a `beads-dashboard-section-key' text property so the load-more
commands can resolve the enclosing section."
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
        (beads-issue-section :issue epic)
        (when section-key
          (list 'beads-dashboard-section-key section-key)))
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
