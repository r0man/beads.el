;;; beads-swarm.el --- Swarm fleet list, status board, worker lanes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is the standalone swarm porcelain (WI-SF-16).  It adds
;; two views over the existing `bd swarm' command classes:
;;
;;   * `beads-swarm-list-view' — the fleet list (`beads-swarm-list-mode',
;;     a `tabulated-list-mode') over `beads-command-swarm-list'.  It can
;;     show every swarm or only the active ones, filter by id/title, and
;;     jumps to the status board, a swarm molecule, or its epic.
;;
;;   * `beads-swarm-status-view' — the sectioned status board over
;;     `beads-command-swarm-status': a header with progress and counts,
;;     the Completed / Active (with assignee) / Ready / Blocked groups,
;;     and the worker lanes.
;;
;; Worker lanes are the only derived layer.  Each distinct active
;; assignee is asked for its in-progress beads through the
;; `beads-swarm-worker-source' seam (standalone default:
;; `bd list --assignee <a> --status in_progress'), and each lane is
;; classified `active', `idle-slot', or `over-committed' against the
;; swarm's active steps with `max_parallelism' headroom from the
;; validate payload.
;;
;; The module is standalone: it never requires gascity, and it consumes
;; only the `bd swarm --json' payloads.  Reads go through
;; `beads-command-execute-async' with a per-view `:cache-key'; every
;; group/section renders independently, so a failed fetch shows an
;; error line and leaves the rest of the board usable.
;;
;; This file is additive.  The typed swarm result classes and the
;; `:result' declarations on the command classes land separately
;; (WI-SF-15); the accessors here read both raw JSON alists and any
;; future typed objects.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(require 'beads-util)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-command-swarm)
(require 'beads-command-update)
(require 'beads-command-show)
(require 'beads-completion)
(require 'beads-faces)
(require 'beads-pager)
(require 'beads-thing)

(declare-function beads-show "beads-command-show")
(declare-function beads-dispatch "beads")
(declare-function beads-handoff-agent "beads-handoff"
                  (&optional work backend-name agent-type-name))
(declare-function beads-handoff-issue "beads-handoff" (issue))
(declare-function beads-execute "beads-command")
(declare-function beads-git-get-branch "beads-git")
(declare-function beads-mode--install-navigation-keys "beads-buffer")
(declare-function beads-check-executable "beads-util")
(declare-function beads-store-resolve "beads-util")
(declare-function beads-store-project-root "beads-util")
(declare-function beads--project-root "beads-util")
(declare-function beads--project-name-for-root "beads-util")

(defvar beads-store-directory)
(defvar beads-executable)

;;; Customization

(defgroup beads-swarm nil
  "Swarm fleet list, status board and worker lanes."
  :group 'beads
  :prefix "beads-swarm-")

(defcustom beads-swarm-async-timeout 30
  "Seconds before an asynchronous swarm fetch is abandoned."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-list-id-width 16
  "Width of the Swarm column in the fleet list."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-list-epic-width 24
  "Width of the Epic column in the fleet list."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-list-coordinator-width 14
  "Width of the Coordinator column in the fleet list."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-list-progress-width 10
  "Width of the Progress column in the fleet list."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-list-count-width 6
  "Width of the numeric count columns in the fleet list."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-list-status-width 9
  "Width of the Status column in the fleet list."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-list-active-only nil
  "When non-nil the fleet list starts filtered to active swarms."
  :type 'boolean
  :group 'beads-swarm)

(defcustom beads-swarm-window-size 25
  "Rows shown per status-board group before the window clips it."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-window-step 25
  "Rows added by `]' when widening a clipped status-board group."
  :type 'natnum
  :group 'beads-swarm)

(defcustom beads-swarm-large-epic-threshold 50
  "Issue count above which the status board comments on its size."
  :type 'natnum
  :group 'beads-swarm)

;;; Faces

;; Defined here rather than in `beads-faces.el' so the swarm view is
;; self-contained; every face derives from the canonical palette.

(defface beads-face-swarm-coordinator
  '((t :inherit beads-face-key))
  "Face for the swarm coordinator name.")

(defface beads-face-swarm-lane
  '((t :inherit beads-face-section))
  "Face for a worker lane label.")

(defface beads-face-swarm-saturated
  '((t :inherit beads-face-warning))
  "Face for a saturated worker lane (no headroom left).")

(defface beads-face-swarm-idle
  '((t :inherit shadow))
  "Face for an idle worker lane (assigned but not on a swarm step).")

;;; Data access

(defun beads-swarm--field (obj key &optional default)
  "Return KEY from OBJ, or DEFAULT.
OBJ may be an alist with symbol or string keys (the `json-read'
shape), a plist, an EIEIO object, a hash table, or a vector whose
first element is such an object.  This lets the swarm views read raw
`bd --json' alists today and typed result objects later."
  (cond
   ((null obj) default)
   ((hash-table-p obj) (gethash key obj default))
   ((and (fboundp 'eieio-object-p) (eieio-object-p obj))
    ;; Typed result slots use hyphenated names (`ready-fronts'), while
    ;; callers pass the raw JSON key (`ready_fronts'); accept either.
    (let ((slot (or (and (slot-exists-p obj key) key)
                    (let ((hyphenated
                           (intern (replace-regexp-in-string
                                    "_" "-" (symbol-name key)))))
                      (and (slot-exists-p obj hyphenated) hyphenated)))))
      (if slot (slot-value obj slot) default)))
   ((and (consp obj) (keywordp (car obj)))
    (if (plist-member obj key) (plist-get obj key) default))
   ((listp obj)
    (if (consp (car obj))
        (let ((pair (or (assq key obj)
                        (assoc (symbol-name key) obj))))
          (if pair (cdr pair) default))
      default))
   ((vectorp obj)
    (if (> (length obj) 0)
        (beads-swarm--field (aref obj 0) key default)
      default))
   (t default)))

(defun beads-swarm--field-string (obj key)
  "Return KEY from OBJ as a string, or the empty string.
Convenience for rendering: nil and non-strings become \"\"."
  (let ((value (beads-swarm--field obj key)))
    (if (stringp value) value (if (null value) "" (format "%s" value)))))

(defun beads-swarm--as-list (value)
  "Return VALUE as a list.
JSON arrays parse to vectors under the project's `json-array-type';
this normalizes vectors and single objects to a list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun beads-swarm--error-string (err)
  "Return a display string for async error ERR.
ERR is the shape `beads-command-execute-async' delivers: a string, a
condition list whose car is a symbol, or a message-first list such as
\"Command failed with exit code 1\" plus a plist."
  (cond
   ((null err) "Unknown error")
   ((stringp err) err)
   ((and (consp err) (stringp (car err))) (car err))
   ((and (consp err) (symbolp (car err))) (error-message-string err))
   (t (format "%s" err))))

;; `beads-swarm-domain-error-p' is the single canonical detector and lives
;; in `beads-command-swarm.el' (WI-SF-15), next to the command classes it
;; describes; `beads-swarm.el' requires that module and does not fork it.

(defun beads-swarm--payload-error (parsed)
  "Return the `error' string carried by PARSED, or nil.
Unlike `beads-swarm-domain-error-p' this is not restricted to the
recognised create/validate domain states: the list and status views
treat any `{error: ...}' payload as an error to surface."
  (let ((err (beads-swarm--field parsed 'error)))
    (and (stringp err) (not (string-empty-p err)) err)))

(defun beads-swarm--list-items (result)
  "Return the swarm items in a `bd swarm list --json' RESULT.
Returns nil for a domain-error payload."
  (cond
   ((beads-swarm--payload-error result) nil)
   ;; A `:result (list-of beads-swarm-list-item)' already gives a list of
   ;; typed items; accept it directly (WI-SF-15/WI-SF-16 boundary).
   ((and (listp result) (consp result)
         (fboundp 'eieio-object-p) (eieio-object-p (car result)))
    result)
   ((and (fboundp 'eieio-object-p) (eieio-object-p result)) (list result))
   ((beads-swarm--field result 'swarms)
    (beads-swarm--as-list (beads-swarm--field result 'swarms)))
   ((vectorp result) (append result nil))
   (t nil)))

(cl-defgeneric beads-swarm-display (item)
  "Return a normalized plist describing swarm list/status ITEM.
The fields are :id, :title, :epic-id, :epic-title, :coordinator,
:status, :progress-percent, :total-issues, :completed-issues and
:active-issues.  Downstream packages may specialise this method to
enrich the row, for example with a city or rig label.")

(cl-defmethod beads-swarm-display (item)
  "Return the normalized plist for swarm list/status ITEM."
  (list :id (beads-swarm--field item 'id)
        :title (beads-swarm--field item 'title)
        :epic-id (beads-swarm--field item 'epic_id)
        :epic-title (beads-swarm--field item 'epic_title)
        :coordinator (beads-swarm--field item 'coordinator)
        :status (beads-swarm--field item 'status)
        :progress-percent (beads-swarm--field item 'progress_percent)
        :total-issues (beads-swarm--field item 'total_issues)
        :completed-issues (beads-swarm--field item 'completed_issues)
        :active-issues (beads-swarm--field item 'active_issues)))

;;; Worker lane seam

(defun beads-swarm--worker-plist (assignee issues)
  "Return the normalized lane plist for ASSIGNEE over ISSUES.
ISSUES is the parsed `bd list' result; :beads is the list of issue
objects, :ids their ids, and :active the in-progress count."
  (let* ((beads (beads-swarm--as-list issues))
         (ids (delq nil (mapcar (lambda (issue)
                                  (beads-swarm--field issue 'id))
                                beads))))
    (list :assignee assignee
          :beads beads
          :ids ids
          :active (length ids))))

(cl-defgeneric beads-swarm-worker-source (assignee)
  "Return the synchronous worker lane plist for ASSIGNEE.
Standalone default: `bd list --assignee ASSIGNEE --status
in_progress'.  Downstream packages override this with live session or
pool state; the default never requires them.")

(cl-defmethod beads-swarm-worker-source (assignee)
  "Return the worker lane plist for ASSIGNEE from `bd list'."
  (require 'beads-command-list)
  (beads-swarm--worker-plist
   assignee
   (beads-execute 'beads-command-list
                  :assignee assignee :status "in_progress")))

(defun beads-swarm--worker-source-async (assignee on-success &optional on-error)
  "Fetch ASSIGNEE's in-progress beads asynchronously.
ON-SUCCESS receives the lane plist from `beads-swarm--worker-plist';
ON-ERROR receives the condition when supplied.  This is the default
value of `beads-swarm-worker-source-function' and never blocks."
  (require 'beads-command-list)
  (let ((cmd (beads-command-list :assignee assignee :status "in_progress")))
    (oset cmd json t)
    (beads-command-execute-async
     cmd
     (lambda (issues)
       (funcall on-success (beads-swarm--worker-plist assignee issues)))
     (lambda (err)
       (when on-error (funcall on-error err)))
     :queue 'auto
     :cache-key (list 'swarm-workers assignee))))

(defvar beads-swarm-worker-source-function #'beads-swarm--worker-source-async
  "Function used by the status board to fetch a worker lane.
Called as (FUNCTION ASSIGNEE ON-SUCCESS &optional ON-ERROR).  The
default is `beads-swarm--worker-source-async'; downstream packages
that own live session state may replace it, or specialise
`beads-swarm-worker-source' for synchronous callers.")

(defun beads-swarm--lane-state (worker active-ids)
  "Classify a worker lane.
WORKER is the lane plist; ACTIVE-IDS is the list of the swarm's
active step ids.  Returns one of `over-committed', `idle-slot',
`active' or `idle'."
  (let* ((ids (or (plist-get worker :ids) nil))
         (on-swarm (seq-intersection ids active-ids #'equal)))
    (cond
     ((> (length on-swarm) 1) 'over-committed)
     ((and ids (null on-swarm)) 'idle-slot)
     (ids 'active)
     (t 'idle))))

(defun beads-swarm--headroom (max-parallelism active-count)
  "Return (HEADROOM . SATURATED) for MAX-PARALLELISM and ACTIVE-COUNT.
HEADROOM is clipped at zero; SATURATED is non-nil when the pool is at
or past its maximum."
  (let* ((max (or max-parallelism 0))
         (raw (- max (or active-count 0))))
    (if (or (< raw 0) (and (= raw 0) (> max 0)))
        (cons 0 t)
      (cons raw nil))))

;;; Async plumbing

(defun beads-swarm--execute-async (class cache-key on-success on-error
                                         &rest args)
  "Construct CLASS with ARGS and execute it asynchronously.
CACHE-KEY coalesces concurrent identical requests; ON-SUCCESS and
ON-ERROR receive the parsed result and the condition.  The command
runs with JSON forced on and the global concurrency cap."
  (let ((cmd (apply #'make-instance class args)))
    (oset cmd json t)
    (beads-command-execute-async
     cmd on-success on-error
     :queue 'auto
     :cache-key cache-key
     :timeout beads-swarm-async-timeout)))

;;; Common store scoping

(defun beads-swarm--store-context (directory)
  "Return (STORE . PROJECT-DIR) for DIRECTORY.
STORE is the resolved bead store, or nil when it resolves from
`default-directory'.  PROJECT-DIR is the directory that names the
buffer."
  (let* ((store (or (beads-store-resolve directory) beads-store-directory))
         (project-dir (if store
                          (beads-store-project-root store)
                        (or (beads--project-root) default-directory))))
    (cons store project-dir)))

;;; Fleet list: mode and state

(defvar-local beads-swarm-list--items nil
  "Swarm display plists shown by the current fleet list.")

(defvar-local beads-swarm-list--all beads-swarm-list-active-only
  "When non-nil the fleet list shows every swarm, not only active ones.")

(defvar-local beads-swarm-list--filter nil
  "Regexp filter over id/title/epic title, or nil.")

(defvar-local beads-swarm-list--error nil
  "Fetch error string for the current fleet list, or nil.")

(defvar-local beads-swarm-list--directory nil
  "Store directory the current fleet list is scoped to.")

(defvar-keymap beads-swarm-list-mode-map
  :doc "Keymap for `beads-swarm-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'beads-swarm-list-status
  "v" #'beads-swarm-list-validate
  "c" #'beads-swarm-list-create
  "w" #'beads-swarm-list-worker-lanes
  "W" #'beads-swarm-list-worker-lanes
  "x" #'beads-swarm-list-jump-swarm
  "E" #'beads-swarm-list-jump-epic
  "f" #'beads-swarm-list-filter
  "a" #'beads-swarm-list-toggle-all
  "g" #'beads-swarm-list-refresh
  "q" #'beads-swarm-list-quit)

;;; Fleet list: rendering

(defun beads-swarm-list--active-p (display)
  "Return non-nil when DISPLAY names an unfinished swarm."
  (not (member (plist-get display :status)
               '("closed" "done" "completed"))))

(defun beads-swarm-list--match-p (display filter)
  "Return non-nil when DISPLAY matches FILTER.
FILTER is a case-insensitive regexp matched against id, title and
epic title; nil matches everything."
  (or (null filter)
      (string-match-p filter (or (plist-get display :id) ""))
      (string-match-p filter (or (plist-get display :title) ""))
      (string-match-p filter (or (plist-get display :epic-title) ""))))

(defun beads-swarm-list--visible (items)
  "Filter ITEMS by the current all/active toggle and text filter."
  (let ((filter (and beads-swarm-list--filter
                     (downcase beads-swarm-list--filter))))
    (seq-filter
     (lambda (display)
       (and (or beads-swarm-list--all
                (beads-swarm-list--active-p display))
            (beads-swarm-list--match-p
             (if filter
                 (list :id (downcase (or (plist-get display :id) ""))
                       :title (downcase (or (plist-get display :title) ""))
                       :epic-title (downcase
                                    (or (plist-get display :epic-title) "")))
               display)
             filter)))
     items)))

(defun beads-swarm-list--progress-cell (display)
  "Format the progress cell for DISPLAY."
  (let ((done (or (plist-get display :completed-issues) 0))
        (total (or (plist-get display :total-issues) 0))
        (percent (plist-get display :progress-percent)))
    (format "%d/%d %s"
            done total
            (if (numberp percent) (format "%d%%" percent) ""))))

(defun beads-swarm-list--entry (display)
  "Return a `tabulated-list-entries' row for DISPLAY."
  (list (plist-get display :id)
        (vector (or (plist-get display :id) "")
                (or (plist-get display :epic-title) "")
                (or (plist-get display :coordinator) "")
                (beads-swarm-list--progress-cell display)
                (number-to-string (or (plist-get display :active-issues) 0))
                (number-to-string (or (plist-get display :total-issues) 0))
                (or (plist-get display :status) ""))))

(defun beads-swarm-list--render ()
  "Render the current fleet list from `beads-swarm-list--items'."
  (let ((entries (mapcar #'beads-swarm-list--entry
                         (beads-swarm-list--visible
                          beads-swarm-list--items))))
    (beads-pager-set-entries entries)
    (force-mode-line-update)))

(define-derived-mode beads-swarm-list-mode tabulated-list-mode "Beads-Swarm"
  "Major mode for the Beads swarm fleet list.

\\{beads-swarm-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "Swarm" beads-swarm-list-id-width t)
                (list "Epic" beads-swarm-list-epic-width t)
                (list "Coordinator" beads-swarm-list-coordinator-width t)
                (list "Progress" beads-swarm-list-progress-width t)
                (list "Active" beads-swarm-list-count-width t :right-align t)
                (list "Tasks" beads-swarm-list-count-width t :right-align t)
                (list "Status" beads-swarm-list-status-width t)))
  (setq tabulated-list-padding 1)
  (setq-local tabulated-list-entries nil)
  (setq header-line-format
        '(:eval (beads-swarm-list--header-line)))
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (beads-pager-mode 1)
  (beads-mode--install-navigation-keys beads-swarm-list-mode-map))

(defun beads-swarm-list--header-line ()
  "Return the fleet list header line."
  (let ((count (length beads-swarm-list--items)))
    (format "Swarms (%d%s)%s"
            count
            (if beads-swarm-list--all " all" " active")
            (cond
             (beads-swarm-list--error
              (propertize (format "  error: %s" beads-swarm-list--error)
                          'face 'beads-face-error))
             (beads-swarm-list--filter
              (format "  filter: %s" beads-swarm-list--filter))
             (t "")))))

;;; Fleet list: loading and entry point

(defun beads-swarm-list--display-result (buffer result)
  "Store RESULT in BUFFER's fleet list and re-render."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((err (beads-swarm--payload-error result)))
        (setq beads-swarm-list--error err)
        (setq beads-swarm-list--items
              (unless err
                (mapcar #'beads-swarm-display
                        (beads-swarm--list-items result))))
        (beads-swarm-list--render)))))

(defun beads-swarm-list--display-error (buffer err)
  "Show fetch error ERR in BUFFER's fleet list."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq beads-swarm-list--error (beads-swarm--error-string err))
      (setq beads-swarm-list--items nil)
      (beads-swarm-list--render))))

(defun beads-swarm-list--load (buffer)
  "Fetch the swarm fleet and render it into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq beads-swarm-list--error nil)
      (beads-swarm-list--render)
      (beads-swarm--execute-async
       'beads-command-swarm-list
       (list 'swarm-list)
       (lambda (result) (beads-swarm-list--display-result buffer result))
       (lambda (err) (beads-swarm-list--display-error buffer err))))))

(defun beads-swarm-list-buffer (&optional directory)
  "Return the fleet list buffer for DIRECTORY, creating it if needed."
  (let* ((context (beads-swarm--store-context directory))
         (project-dir (cdr context))
         (proj-name (beads--project-name-for-root project-dir))
         (buffer (get-buffer-create
                  (beads-buffer-utility "swarm-list" nil proj-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-swarm-list-mode)
        (beads-swarm-list-mode))
      (setq default-directory (file-name-as-directory project-dir))
      (setq beads-swarm-list--directory (car context))
      (setq-local beads-store-directory (car context))
      (beads-swarm-list--render))
    buffer))

;;;###autoload
(cl-defun beads-swarm-list-view (&key directory)
  "Display the swarm fleet list.
With DIRECTORY non-nil, list the bead store at DIRECTORY and scope the
buffer to it; without it, a call from a store-scoped buffer inherits
its store, else the store resolves from `default-directory'."
  (interactive)
  (beads-check-executable)
  (let ((buffer (beads-swarm-list-buffer directory)))
    (with-current-buffer buffer
      (beads-swarm-list--load buffer))
    (pop-to-buffer buffer)
    buffer))

(defun beads-swarm-list-refresh ()
  "Refresh the swarm fleet list in place."
  (interactive)
  (beads-swarm-list--load (current-buffer)))

(defun beads-swarm-list-quit ()
  "Bury the swarm fleet list buffer."
  (interactive)
  (quit-window))

(defun beads-swarm-list-toggle-all ()
  "Toggle the fleet list between active-only and all swarms."
  (interactive)
  (setq beads-swarm-list--all (not beads-swarm-list--all))
  (beads-swarm-list--render))

(defun beads-swarm-list-filter (filter)
  "Filter the fleet list to rows matching regExp FILTER.
With prefix argument or empty input, clear the filter."
  (interactive
   (list (unless current-prefix-arg
           (read-regexp "Filter swarms (blank clears): "))))
  (setq beads-swarm-list--filter
        (and filter (not (string-empty-p filter)) filter))
  (beads-swarm-list--render))

(defun beads-swarm-list--id-at-point ()
  "Return the swarm id on the current fleet list row."
  (or (tabulated-list-get-id) (user-error "No swarm on this line")))

(defun beads-swarm-list-status ()
  "Open the status board for the swarm on the current row."
  (interactive)
  (beads-swarm-status-view (beads-swarm-list--id-at-point)))

(defun beads-swarm-list-worker-lanes ()
  "Open the status board for the swarm at point with worker lanes."
  (interactive)
  (beads-swarm-status-view (beads-swarm-list--id-at-point) :show-lanes t))

(defun beads-swarm-list--display-for-id (id)
  "Return the fleet row DISPLAY for swarm ID, or nil."
  (seq-find (lambda (display) (equal (plist-get display :id) id))
            beads-swarm-list--items))

(defun beads-swarm-list-jump-swarm ()
  "Open the swarm molecule for the row at point."
  (interactive)
  (beads-show (beads-swarm-list--id-at-point)))

(defun beads-swarm-list-jump-epic ()
  "Open the epic for the row at point."
  (interactive)
  (let* ((id (beads-swarm-list--id-at-point))
         (display (beads-swarm-list--display-for-id id))
         (epic (and display (plist-get display :epic-id))))
    (unless epic (user-error "No epic recorded for swarm %s" id))
    (beads-show epic)))

(defun beads-swarm-list-validate ()
  "Open the validate view for the epic of the swarm at point.
The validate view itself is provided by WI-SF-17; this only
dispatches to it when present."
  (interactive)
  (let* ((id (beads-swarm-list--id-at-point))
         (display (beads-swarm-list--display-for-id id))
         (epic (and display (plist-get display :epic-id))))
    (beads-swarm--validate-dispatch epic)))

(defun beads-swarm-list-create ()
  "Create a swarm for the epic of the swarm at point.
The create flow itself is provided by WI-SF-18; this only dispatches
to it when present."
  (interactive)
  (let* ((id (beads-swarm-list--id-at-point))
         (display (beads-swarm-list--display-for-id id))
         (epic (and display (plist-get display :epic-id))))
    (beads-swarm--create-dispatch epic)))

;;; Forward dispatch to later swarm WIs

(defun beads-swarm--validate-dispatch (epic-id)
  "Open the validate view for EPIC-ID, or report that it is absent."
  (cond
   ((null epic-id) (user-error "No epic to validate"))
   ((fboundp 'beads-swarm-waves) (beads-swarm-waves epic-id))
   (t (user-error "Swarm validate view is not available (WI-SF-17)"))))

(defun beads-swarm--create-dispatch (epic-id)
  "Create a swarm for EPIC-ID, or report that the flow is absent."
  (cond
   ((null epic-id) (user-error "No epic to create a swarm from"))
   ((fboundp 'beads-swarm-create-flow) (beads-swarm-create-flow epic-id))
   (t (user-error "Swarm create flow is not available (WI-SF-18)"))))

;;; Status board: state

(defvar-local beads-swarm--id nil
  "Swarm or epic id shown by the current status board.")

(defvar-local beads-swarm--epic-id nil
  "Epic id resolved from the status payload, or nil.")

(defvar-local beads-swarm--status nil
  "Parsed `bd swarm status --json' result for the current board.")

(defvar-local beads-swarm--status-error nil
  "Status fetch error string, or nil.")

(defvar-local beads-swarm--validate nil
  "Parsed `bd swarm validate --json' result, or nil.")

(defvar-local beads-swarm--validate-error nil
  "Validate fetch error string, or nil.")

(defvar-local beads-swarm--lanes nil
  "Hash of assignee to worker lane plist for the current board.")

(defvar-local beads-swarm--lanes-error nil
  "Worker lane fetch error string, or nil.")

(defvar-local beads-swarm--collapsed nil
  "Hash of section key to non-nil when the group is collapsed.")

(defvar-local beads-swarm--show-lanes nil
  "When non-nil the status board renders the worker lanes.")

(defvar-local beads-swarm--window beads-swarm-window-size
  "Rows shown per status-board group before clipping.")

(defvar-local beads-swarm--directory nil
  "Store directory the current status board is scoped to.")

(defvar-keymap beads-swarm-status-mode-map
  :doc "Keymap for `beads-swarm-status-mode'."
  "RET" #'beads-swarm-status-visit
  "W" #'beads-swarm-status-toggle-lanes
  "V" #'beads-swarm-status-validate
  "]" #'beads-swarm-status-widen
  "[" #'beads-swarm-status-narrow
  "g" #'beads-swarm-status-refresh
  "q" #'beads-swarm-status-quit)

;;; Status board: pure helpers

(defun beads-swarm--progress-bar (percent width)
  "Return a progress bar that is WIDTH cells long for PERCENT (0-100)."
  (let* ((pct (max 0 (min 100 (or percent 0))))
         (filled (round (* width (/ pct 100.0)))))
    (concat (propertize (make-string filled ?█) 'face 'beads-face-success)
            (propertize (make-string (- width filled) ?░) 'face 'shadow))))

(defun beads-swarm--group-key (group)
  "Return the collapsed-hash key for status GROUP."
  (pcase group
    ('completed 'completed)
    ('active 'active)
    ('ready 'ready)
    ('blocked 'blocked)
    (_ 'other)))

(defun beads-swarm--group-glyph (group)
  "Return the state glyph for status GROUP."
  (pcase group
    ('completed "✓")
    ('active "◐")
    ('ready "○")
    ('blocked "⊘")
    (_ "·")))

(defun beads-swarm--group-face (group)
  "Return the face for status GROUP's glyph."
  (pcase group
    ('completed 'beads-face-status-closed)
    ('active 'beads-face-status-in-progress)
    ('ready 'beads-face-status-open)
    ('blocked 'beads-face-status-blocked)
    (_ 'default)))

(defun beads-swarm--issue-row (group item)
  "Return a rendered row for GROUP's ITEM.
The row carries a `beads-thing' so movement and RET work like other
beads views."
  (let* ((id (or (beads-swarm--field item 'id) ""))
         (title (or (beads-swarm--field item 'title) ""))
         (assignee (beads-swarm--field item 'assignee))
         (blocked-by (beads-swarm--as-list (beads-swarm--field item 'blocked_by)))
         (glyph (beads-swarm--group-glyph group))
         (annotation
          (cond
           ((and (eq group 'active) (stringp assignee))
            (propertize (format "  [%s]" assignee)
                        'face 'beads-face-swarm-coordinator))
           ((and (eq group 'blocked) blocked-by)
            (propertize (format "  ⊘ blocked by %s"
                                (mapconcat #'identity blocked-by ", "))
                        'face 'beads-face-swarm-idle))
           (t ""))))
    (beads-thing-propertize
     (concat "  " (propertize glyph 'face (beads-swarm--group-face group))
             " " (propertize id 'face 'beads-face-id)
             "  " title annotation)
     (list :kind 'issue :id id))))

;;; Status board: rendering

(defun beads-swarm--render-group (title group items)
  "Render the TITLE group with ITEMS for status GROUP.
Returns the inserted text.  A large group is clipped to
`beads-swarm--window' rows."
  (let* ((key (beads-swarm--group-key group))
         (collapsed (gethash key beads-swarm--collapsed))
         (count (length items))
         (shown (if (> count beads-swarm--window)
                    (seq-take items beads-swarm--window)
                  items))
         (hidden (- count (length shown))))
    (concat
     (beads-thing-propertize
      (concat (if collapsed "▸ " "▾ ")
              (propertize (format "%s (%d)" title count) 'face
                          'beads-face-section))
      (list :kind 'section :toggle (lambda ()
                                     (beads-swarm-status-toggle-group key))))
     "\n"
     (unless collapsed
       (concat
        (mapconcat (lambda (item) (concat (beads-swarm--issue-row group item) "\n"))
                   shown "")
        (when (> hidden 0)
          (propertize (format "    … %d more (] to widen)\n" hidden)
                      'face 'beads-face-swarm-idle)))))))

(defun beads-swarm--render-lane (assignee worker active-ids headroom)
  "Render the worker lane for ASSIGNEE.
WORKER is the lane plist, ACTIVE-IDS the swarm's active step ids and
HEADROOM the (HEADROOM . SATURATED) from the validate payload."
  (let* ((state (beads-swarm--lane-state worker active-ids))
         (on-swarm (seq-intersection (or (plist-get worker :ids) nil)
                                     active-ids #'equal))
         (count (length (or (plist-get worker :ids) nil)))
         (state-label
          (pcase state
            ('active (propertize "active" 'face 'beads-face-success))
            ('idle-slot (propertize "idle-slot" 'face 'beads-face-swarm-idle))
            ('over-committed (propertize "over-committed"
                                         'face 'beads-face-error))
            (_ (propertize "idle" 'face 'beads-face-swarm-idle))))
         (saturated (cdr headroom)))
    (concat
     "  " (propertize "★" 'face 'beads-face-swarm-coordinator)
     " " (propertize assignee 'face 'beads-face-swarm-lane)
     "  " state-label
     (propertize (format "  steps:%d beads:%d" (length on-swarm) count)
                 'face 'shadow)
     (when (and saturated (eq state 'active))
       (propertize "  ▰ saturated" 'face 'beads-face-swarm-saturated))
     "\n")))

(defun beads-swarm--render-lanes ()
  "Render the worker lanes section.
Returns the empty string when there is no lane data yet."
  (let* ((active (beads-swarm--field beads-swarm--status 'active))
         (assignees (delete-dups
                     (delq nil
                           (mapcar (lambda (item)
                                     (beads-swarm--field item 'assignee))
                                   (beads-swarm--as-list active))))))
    (if (and (null assignees) (null beads-swarm--lanes))
        ""
      (let* ((max-parallelism
              (beads-swarm--field beads-swarm--validate 'max_parallelism))
             (active-count
              (or (beads-swarm--field beads-swarm--status 'active_count)
                  (length assignees)))
             (headroom (beads-swarm--headroom max-parallelism active-count)))
        (concat
         (beads-thing-propertize
          (concat "▾ "
                  (propertize (format "Worker lanes (%d)" (length assignees))
                              'face 'beads-face-section))
          (list :kind 'section))
         "\n"
         (if beads-swarm--lanes-error
             (propertize (format "    error: %s\n" beads-swarm--lanes-error)
                         'face 'beads-face-error)
           (mapconcat
            (lambda (assignee)
              (let ((worker (gethash assignee beads-swarm--lanes)))
                (if worker
                    (beads-swarm--render-lane assignee worker
                                              (beads-swarm--active-ids)
                                              headroom)
                  (propertize (format "  %s  loading…\n" assignee)
                              'face 'shadow))))
            (sort (copy-sequence assignees) #'string<)))
         (propertize
          (format "  headroom: %d%s\n"
                  (car headroom)
                  (if (cdr headroom) " (saturated)" ""))
          'face 'shadow))))))

(defun beads-swarm--active-ids ()
  "Return the ids of the swarm's active steps."
  (delq nil
        (mapcar (lambda (item) (beads-swarm--field item 'id))
                (beads-swarm--as-list
                 (beads-swarm--field beads-swarm--status 'active)))))

(defun beads-swarm--render-header ()
  "Render the status board header line."
  (let* ((status beads-swarm--status)
         (epic-title (or (beads-swarm--field status 'epic_title)
                         (beads-swarm--field status 'epic-title)
                         beads-swarm--id))
         (epic-id (or beads-swarm--epic-id ""))
         (percent (or (beads-swarm--field status 'progress_percent) 0))
         (total (or (beads-swarm--field status 'total_issues) 0))
         (done (or (beads-swarm--field status 'completed_issues)
                   (length (beads-swarm--as-list
                            (beads-swarm--field status 'completed)))))
         (active-count (or (beads-swarm--field status 'active_count)
                           (length (beads-swarm--as-list
                                    (beads-swarm--field status 'active)))))
         (ready-count (or (beads-swarm--field status 'ready_count)
                          (length (beads-swarm--as-list
                                   (beads-swarm--field status 'ready)))))
         (blocked-count (or (beads-swarm--field status 'blocked_count)
                            (length (beads-swarm--as-list
                                     (beads-swarm--field status 'blocked))))))
    (concat
     (propertize (format "Swarm status: %s" epic-title) 'face 'beads-face-header)
     "\n"
     (propertize (format "  id: %s" beads-swarm--id) 'face 'beads-face-id)
     (if (string-empty-p epic-id)
         ""
       (propertize (format "  epic: %s" epic-id) 'face 'beads-face-id))
     "\n"
     (format "  %s %3d%%  %d/%d done  active:%d  ready:%d  blocked:%d\n"
             (beads-swarm--progress-bar percent 20)
             (if (numberp percent) percent 0)
             done total active-count ready-count blocked-count)
     (when (and (> total beads-swarm-large-epic-threshold)
                (null beads-swarm--status-error))
       (propertize (format "  large epic: showing %d rows per group\n"
                           beads-swarm--window)
                   'face 'shadow)))))

(defun beads-swarm--render ()
  "Render the whole status board from its buffer-local data."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (beads-swarm--render-header))
    (insert "\n")
    (if beads-swarm--status-error
        (insert (propertize (format "Error: %s\n" beads-swarm--status-error)
                            'face 'beads-face-error))
      (let ((status beads-swarm--status))
        (insert (beads-swarm--render-group
                 "Completed" 'completed
                 (beads-swarm--as-list (beads-swarm--field status 'completed))))
        (insert (beads-swarm--render-group
                 "Active" 'active
                 (beads-swarm--as-list (beads-swarm--field status 'active))))
        (insert (beads-swarm--render-group
                 "Ready" 'ready
                 (beads-swarm--as-list (beads-swarm--field status 'ready))))
        (insert (beads-swarm--render-group
                 "Blocked" 'blocked
                 (beads-swarm--as-list (beads-swarm--field status 'blocked))))))
    (when beads-swarm--validate-error
      (insert (propertize (format "\nValidate error: %s\n"
                                  beads-swarm--validate-error)
                          'face 'beads-face-error)))
    (insert "\n")
    (insert (beads-swarm--render-lanes))
    (goto-char (point-min))))

;;; Status board: loading

(defun beads-swarm-status--display-status (buffer result)
  "Store status RESULT in BUFFER and continue loading its sections."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((err (beads-swarm--payload-error result)))
        (if err
            (progn
              (setq beads-swarm--status nil
                    beads-swarm--status-error err)
              (beads-swarm--render))
          (setq beads-swarm--status result
                beads-swarm--status-error nil
                beads-swarm--epic-id (beads-swarm--field result 'epic_id))
          (beads-swarm--render)
          (beads-swarm-status--load-validate buffer)
          (when beads-swarm--show-lanes
            (beads-swarm-status--load-lanes buffer)))))))

(defun beads-swarm-status--display-status-error (buffer err)
  "Record status fetch error ERR in BUFFER and re-render."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq beads-swarm--status-error (beads-swarm--error-string err))
      (beads-swarm--render))))

(defun beads-swarm-status--load-validate (buffer)
  "Fetch validation data for BUFFER's resolved epic."
  (let ((epic beads-swarm--epic-id))
    (when (and (buffer-live-p buffer) epic)
      (beads-swarm--execute-async
       'beads-command-swarm-validate
       (list 'swarm-validate epic)
       (lambda (result)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((err (or (beads-swarm-domain-error-p result)
                            (beads-swarm--payload-error result))))
               (setq beads-swarm--validate (unless err result)
                     beads-swarm--validate-error err))
             (beads-swarm--render))))
       (lambda (err)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq beads-swarm--validate-error
                   (beads-swarm--error-string err))
             (beads-swarm--render))))
       :epic-id epic))))

(defun beads-swarm-status--load-lanes (buffer)
  "Fetch the worker lanes for BUFFER's active assignees."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq beads-swarm--lanes (make-hash-table :test #'equal)
            beads-swarm--lanes-error nil
            beads-swarm--show-lanes t)
      (let ((assignees (delete-dups
                        (delq nil
                              (mapcar (lambda (item)
                                        (beads-swarm--field item 'assignee))
                                      (beads-swarm--as-list
                                       (beads-swarm--field
                                        beads-swarm--status 'active)))))))
        (if (null assignees)
            (beads-swarm--render)
          (dolist (assignee assignees)
            (funcall beads-swarm-worker-source-function
                     assignee
                     (lambda (worker)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (puthash assignee worker beads-swarm--lanes)
                           (beads-swarm--render))))
                     (lambda (err)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (setq beads-swarm--lanes-error
                                 (beads-swarm--error-string err))
                           (beads-swarm--render)))))))))))

(defun beads-swarm-status--load (buffer)
  "Fetch the swarm status and render it into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq beads-swarm--status nil
            beads-swarm--status-error nil
            beads-swarm--validate nil
            beads-swarm--validate-error nil
            beads-swarm--lanes (make-hash-table :test #'equal)
            beads-swarm--lanes-error nil
            beads-swarm--epic-id nil)
      (beads-swarm--render)
      (beads-swarm--execute-async
       'beads-command-swarm-status
       (list 'swarm-status beads-swarm--id)
       (lambda (result) (beads-swarm-status--display-status buffer result))
       (lambda (err) (beads-swarm-status--display-status-error buffer err))
       :swarm-id beads-swarm--id))))

;;; Status board: mode and entry point

(define-derived-mode beads-swarm-status-mode special-mode "Beads-Swarm-Status"
  "Major mode for the Beads swarm status board.

\\{beads-swarm-status-mode-map}"
  (setq buffer-read-only t)
  (setq beads-swarm--collapsed (make-hash-table :test #'eq))
  (setq beads-swarm--lanes (make-hash-table :test #'equal))
  (hl-line-mode 1)
  (add-hook 'beads-thing-toggle-functions
            #'beads-swarm-status--toggle-thing nil t)
  (beads-mode--install-navigation-keys beads-swarm-status-mode-map))

(defun beads-swarm-status-buffer (id &optional directory)
  "Return the status board buffer for ID scoped to DIRECTORY."
  (let* ((context (beads-swarm--store-context directory))
         (store (car context))
         (project-dir (cdr context))
         (proj-name (beads--project-name-for-root project-dir))
         (buffer (get-buffer-create
                  (beads-buffer-utility "swarm-status" id proj-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-swarm-status-mode)
        (beads-swarm-status-mode))
      (setq default-directory (file-name-as-directory project-dir))
      (setq beads-swarm--directory store)
      (setq-local beads-store-directory store)
      (setq beads-swarm--id id))
    buffer))

;;;###autoload
(cl-defun beads-swarm-status-view (id &key directory show-lanes)
  "Display the swarm status board for ID.
ID is an epic or a swarm molecule.  With DIRECTORY non-nil, scope the
buffer to that bead store.  With SHOW-LANES non-nil, load the worker
lanes as well."
  (interactive (list (read-string "Swarm or epic id: ")))
  (beads-check-executable)
  (let ((buffer (beads-swarm-status-buffer id directory)))
    (with-current-buffer buffer
      (setq beads-swarm--show-lanes show-lanes)
      (beads-swarm-status--load buffer))
    (pop-to-buffer buffer)
    buffer))

(defun beads-swarm-status-refresh ()
  "Refresh the swarm status board in place."
  (interactive)
  (beads-swarm-status--load (current-buffer)))

(defun beads-swarm-status-quit ()
  "Bury the swarm status board buffer."
  (interactive)
  (quit-window))

(defun beads-swarm-status-widen ()
  "Show more rows per status-board group."
  (interactive)
  (setq beads-swarm--window (+ beads-swarm--window beads-swarm-window-step))
  (beads-swarm--render))

(defun beads-swarm-status-narrow ()
  "Show fewer rows per status-board group."
  (interactive)
  (setq beads-swarm--window
        (max beads-swarm-window-step (- beads-swarm--window beads-swarm-window-step)))
  (beads-swarm--render))

(defun beads-swarm-status-toggle-group (key)
  "Toggle the collapsed state of group KEY and re-render."
  (puthash key (not (gethash key beads-swarm--collapsed))
           beads-swarm--collapsed)
  (beads-swarm--render))

(defun beads-swarm-status--toggle-thing (thing)
  "Toggle the section at THING, if it has one."
  (let ((fn (and (consp thing) (keywordp (car thing))
                 (plist-get thing :toggle))))
    (when fn (funcall fn) t)))

(defun beads-swarm-status-toggle-lanes ()
  "Toggle the worker lanes on the status board."
  (interactive)
  (if beads-swarm--show-lanes
      (progn
        (setq beads-swarm--show-lanes nil)
        (beads-swarm--render))
    (beads-swarm-status--load-lanes (current-buffer))))

(defun beads-swarm-status-validate ()
  "Open the validate view for this board's epic."
  (interactive)
  (beads-swarm--validate-dispatch beads-swarm--epic-id))

(defun beads-swarm-status--issue-id-at-point ()
  "Return the issue id on the current status-board line, or nil."
  (let ((thing (beads-thing-at)))
    (and (consp thing) (eq (beads-thing-kind thing) 'issue)
         (plist-get thing :id))))

(defun beads-swarm-status-visit ()
  "Visit the issue or toggle the group at point."
  (interactive)
  (let ((thing (beads-thing-at)))
    (cond
     ((null thing) (user-error "Nothing here"))
     ((eq (beads-thing-kind thing) 'section)
      (beads-swarm-status--toggle-thing thing))
     (t
      (let ((id (beads-swarm-status--issue-id-at-point)))
        (if id (beads-show id) (user-error "No issue on this line")))))))

;;; ============================================================
;;; Validate / ready-fronts view (WI-SF-17, REQ-SF-093)
;;; ============================================================

(defvar-local beads-swarm-waves--analysis nil
  "Parsed `bd swarm validate --json' result for the current view.")
(defvar-local beads-swarm-waves--error nil
  "Validate fetch error string, or nil.")
(defvar-local beads-swarm-waves--epic-id nil
  "Epic id shown by the current waves view.")
(defvar-local beads-swarm-waves--directory nil
  "Store directory the current waves view is scoped to.")
(defvar-local beads-swarm-waves--verbose nil
  "When non-nil the waves view renders the per-issue graph.")

(defvar-keymap beads-swarm-waves-mode-map
  :doc "Keymap for `beads-swarm-waves-mode'."
  :parent special-mode-map
  "RET" #'beads-swarm-waves-visit
  "v" #'beads-swarm-waves-toggle-verbose
  "g" #'beads-swarm-waves-refresh
  "q" #'beads-swarm-waves-quit)

(define-derived-mode beads-swarm-waves-mode special-mode "Beads-Swarm-Waves"
  "Major mode for the swarm validate / ready-fronts (waves) view."
  :interactive nil
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (beads-swarm-waves-refresh))))

(defun beads-swarm-waves--waves (analysis)
  "Return ANALYSIS's ready fronts as a list of (:wave :issues) plists."
  (mapcar (lambda (front)
            (list :wave (or (beads-swarm--field front 'wave) 0)
                  :issues (append (beads-swarm--field front 'issues) nil)))
          (beads-swarm--as-list (beads-swarm--field analysis 'ready_fronts))))

(defun beads-swarm-waves--insert-thing (text thing)
  "Insert TEXT propertized with THING for beads-thing navigation."
  (insert (propertize text 'beads-thing thing)))

(defun beads-swarm-waves--render ()
  "Render the current waves view from its analysis payload."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if beads-swarm-waves--error
        (insert (format "Swarm validate error: %s\n" beads-swarm-waves--error))
      (let ((a beads-swarm-waves--analysis))
        (insert (format "Swarm validate — %s\n\n"
                        (or (beads-swarm--field a 'epic_title)
                            beads-swarm-waves--epic-id)))
        (insert (format "Swarmable: %s\n"
                        (if (beads-swarm--field a 'swarmable) "yes" "NO")))
        (insert (format (concat "Issues: %s   Closed: %s   "
                                "Max parallelism: %s   Estimated sessions: %s\n\n")
                        (or (beads-swarm--field a 'total_issues) "—")
                        (or (beads-swarm--field a 'closed_issues) "—")
                        (or (beads-swarm--field a 'max_parallelism) "—")
                        (or (beads-swarm--field a 'estimated_sessions) "—")))
        (insert "Ready fronts (parallel waves)\n")
        (let ((waves (beads-swarm-waves--waves a)))
          (if (null waves)
              (insert "  (none)\n")
            (dolist (wave waves)
              (beads-swarm-waves--insert-thing
               (format "  Wave %s: %s\n" (plist-get wave :wave)
                       (string-join
                        (mapcar (lambda (id) (format "%s" id))
                                (plist-get wave :issues))
                        ", "))
               (list :kind 'wave :wave (plist-get wave :wave))))))
        (insert "\n")
        (dolist (key '(warnings errors))
          (let ((items (beads-swarm--as-list (beads-swarm--field a key))))
            (insert (format "%s (%d)\n" (capitalize (symbol-name key))
                            (length items)))
            (if (null items)
                (insert "  (none)\n")
              (dolist (item items)
                (insert (format "  %s\n"
                                (if (stringp item) item (format "%s" item))))))))
        (when beads-swarm-waves--verbose
          (insert "\nPer-issue graph\n")
          (dolist (node (beads-swarm--as-list (beads-swarm--field a 'issues)))
            (let ((id (beads-swarm--field node 'id)))
              (beads-swarm-waves--insert-thing
               (format "  %-18s wave=%s  depends_on=%s\n"
                       (or id "?")
                       (or (beads-swarm--field node 'wave) "—")
                       (or (beads-swarm--field node 'depends_on) "—"))
               (list :kind 'issue :id id)))))))))

(defun beads-swarm-waves--load (buffer)
  "Fetch validation data for BUFFER's epic and render it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (beads-swarm--execute-async
       'beads-command-swarm-validate
       (list 'swarm-validate beads-swarm-waves--epic-id)
       (lambda (result)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((err (or (beads-swarm-domain-error-p result)
                            (beads-swarm--payload-error result))))
               (setq beads-swarm-waves--analysis (unless err result)
                     beads-swarm-waves--error err))
             (beads-swarm-waves--render))))
       (lambda (err)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq beads-swarm-waves--error (beads-swarm--error-string err))
             (beads-swarm-waves--render))))
       :epic-id beads-swarm-waves--epic-id))))

(defun beads-swarm-waves--id-at-point ()
  "Return the issue id on the current waves line, or nil."
  (let ((thing (beads-thing-at)))
    (and (consp thing) (eq (beads-thing-kind thing) 'issue)
         (plist-get thing :id))))

(defun beads-swarm-waves-visit ()
  "Visit the per-issue node at point."
  (interactive)
  (if-let* ((id (beads-swarm-waves--id-at-point)))
      (beads-show id)
    (user-error "No issue on this line")))

(defun beads-swarm-waves-toggle-verbose ()
  "Toggle the per-issue graph in the waves view."
  (interactive)
  (setq beads-swarm-waves--verbose (not beads-swarm-waves--verbose))
  (beads-swarm-waves--render))

(defun beads-swarm-waves-refresh ()
  "Reload the waves view from `bd swarm validate'."
  (interactive)
  (beads-swarm-waves--load (current-buffer)))

(defun beads-swarm-waves-quit ()
  "Bury the waves view."
  (interactive)
  (quit-window))

;;;###autoload
(cl-defun beads-swarm-waves (epic-id &key directory)
  "Open the validate / ready-fronts (waves) view for EPIC-ID.
With DIRECTORY, scope the view to that bead store instead of the one
resolved from `default-directory'."
  (interactive (list (beads-completion-read-issue "Epic: " nil t)))
  (require 'beads-command-swarm)
  (beads-check-executable)
  (let* ((store (or (beads-store-resolve directory) beads-store-directory))
         (default-directory (or store default-directory))
         (project-root (if store
                           (beads-store-project-root store)
                         (or (beads--project-root) default-directory)))
         (buffer (get-buffer-create
                  (beads-buffer-utility "swarm-waves" epic-id project-root))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-swarm-waves-mode)
        (beads-swarm-waves-mode))
      (setq-local beads-swarm-waves--epic-id epic-id
                  beads-swarm-waves--directory directory
                  beads-store-directory store)
      (beads-swarm-waves--load buffer))
    (pop-to-buffer buffer)
    buffer))

;;; ============================================================
;;; Create, coordinator and step actions (WI-SF-18, REQ-SF-090/095)
;;; ============================================================

(defun beads-swarm-create-flow (&optional epic-id)
  "Create a swarm for EPIC-ID, validating first (WI-SF-18).
Reports a non-swarmable epic instead of creating, and surfaces the
`already exists' domain state instead of a raw error."
  (interactive (list (beads-completion-read-issue "Epic: " nil t)))
  (unless epic-id (user-error "No epic to swarm"))
  (let ((preview (beads-execute 'beads-command-swarm-validate :epic-id epic-id)))
    (when (and preview (not (beads-swarm--field preview 'swarmable)))
      (user-error "Epic %s is not swarmable: %s" epic-id
                  (beads-swarm--field preview 'errors)))
    (let* ((input (read-string "Coordinator (blank = none): "))
           (coordinator (and (not (string-empty-p (string-trim input)))
                             (string-trim input)))
           (force (yes-or-no-p "Force create if a swarm already exists? "))
           (created (beads-execute 'beads-command-swarm-create
                                   :epic-id epic-id
                                   :coordinator coordinator
                                   :force force)))
      (if (beads-swarm-domain-error-p created)
          (user-error "Swarm create: %s" (beads-swarm--field created 'error))
        (message "Swarm created: %s" (beads-swarm--field created 'swarm_id))
        (beads-swarm-status-view epic-id)))))

(defun beads-swarm-coordinator (swarm-id &optional new-coordinator)
  "Set NEW-COORDINATOR as the coordinator (assignee) of SWARM-ID (WI-SF-18)."
  (interactive
   (list (beads-completion-read-issue "Swarm or epic: " nil t)
         (read-string "Coordinator: ")))
  (beads-execute 'beads-command-update
                 :issue-ids (list swarm-id) :assignee new-coordinator)
  (message "Coordinator of %s set to %s" swarm-id new-coordinator))

(defun beads-swarm-assign (issue-id assignee)
  "Assign ISSUE-ID (a swarm step) to ASSIGNEE (WI-SF-18)."
  (interactive
   (list (beads-completion-read-issue "Step: " nil t)
         (read-string "Assignee: ")))
  (beads-execute 'beads-command-update
                 :issue-ids (list issue-id) :assignee assignee)
  (message "Assigned %s to %s" issue-id assignee))

(defun beads-swarm-claim (issue-id)
  "Claim swarm step ISSUE-ID (WI-SF-18)."
  (interactive (list (beads-completion-read-issue "Step: " nil t)))
  (beads-execute 'beads-command-update :issue-ids (list issue-id) :claim t)
  (message "Claimed %s" issue-id))

(defun beads-swarm-status--require-issue ()
  "Return the issue id at point, or signal."
  (or (beads-swarm-status--issue-id-at-point)
      (user-error "No issue on this line")))

(defun beads-swarm-status-assign ()
  "Assign the step at point (WI-SF-18)."
  (interactive)
  (beads-swarm-assign (beads-swarm-status--require-issue)
                      (read-string "Assignee: "))
  (beads-swarm-status-refresh))

(defun beads-swarm-status-claim ()
  "Claim the step at point (WI-SF-18)."
  (interactive)
  (beads-swarm-claim (beads-swarm-status--require-issue))
  (beads-swarm-status-refresh))

(defun beads-swarm-status-coordinator ()
  "Set the coordinator for the board's swarm/epic (WI-SF-18)."
  (interactive)
  (beads-swarm-coordinator (or beads-swarm--id (beads-swarm-status--require-issue))
                           (read-string "Coordinator: "))
  (beads-swarm-status-refresh))

(defun beads-swarm-status-handoff ()
  "Hand the step at point off to an agent (WI-SF-18)."
  (interactive)
  (let* ((id (beads-swarm-status--require-issue))
         (issue (beads-execute 'beads-command-show :issue-ids (list id))))
    (if (fboundp 'beads-handoff-agent)
        (beads-handoff-agent (beads-handoff-issue issue))
      (user-error "Hand-off is not available"))))

(define-key beads-swarm-status-mode-map (kbd "a") #'beads-swarm-status-assign)
(define-key beads-swarm-status-mode-map (kbd "c") #'beads-swarm-status-claim)
(define-key beads-swarm-status-mode-map (kbd "C") #'beads-swarm-status-coordinator)
(define-key beads-swarm-status-mode-map (kbd "h") #'beads-swarm-status-handoff)

(provide 'beads-swarm)
;;; beads-swarm.el ends here
