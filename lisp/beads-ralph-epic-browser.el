;;; beads-ralph-epic-browser.el --- Minimum vui epic browser for Ralph -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, vc

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Phase 1 minimum vui epic browser (bde-deqx.6).  Singleton buffer
;; `*beads-epics*' deriving from `vui-mode' that lists epics and lets
;; the user launch a Ralph loop on one.  No expand/collapse, no
;; filtering, no inline children — those polish features are deferred
;; to bde-deqx.10 (Phase 2 full version).
;;
;; Data source: a single `bd epic status --json' call (no per-epic
;; fan-out for ready counts in Phase 1).
;;
;; Ralph badge: when `beads-agent-ralph-controller-for-root' returns a
;; live controller for an epic's id, the row gets an inline annotation
;; with the current iteration and max-iterations.
;;
;; State-change refresh: subscribes to
;; `beads-agent-ralph-state-change-functions' and re-renders the
;; browser buffer through a 0.05s idle-timer debounce, mirroring the
;; per-loop dashboard's coalescing behaviour.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vui)

(require 'beads-agent-ralph)
(require 'beads-command-epic)
(require 'beads-types)
(require 'beads-util)

;; Forward declarations
(declare-function beads-show "beads-command-show")
(declare-function beads-execute "beads-command")
(declare-function beads-agent-ralph-launch "beads-agent-ralph-confirm")
;; `beads-ralph-launcher' is the Phase 1 launcher panel introduced by
;; bde-deqx.5.  The contract this module assumes is the one specified
;; in that bead's design: `(beads-ralph-launcher ROOT-ID &key kind)'.
;; The fallback below uses the existing `beads-agent-ralph-launch'
;; with the same root-id and kind so behaviour is consistent before
;; and after deqx.5 lands.
(declare-function beads-ralph-launcher "beads-ralph-launcher" (root-id &rest args))
(declare-function beads-ralph-launch-at-point "beads-ralph-launcher")

;;; Customization

(defgroup beads-ralph-epic-browser nil
  "Minimum vui epic browser for Ralph."
  :group 'beads
  :prefix "beads-ralph-epic-browser-")

(defcustom beads-ralph-epic-browser-buffer-name "*beads-epics*"
  "Name of the singleton epic-browser buffer."
  :type 'string
  :group 'beads-ralph-epic-browser)

(defcustom beads-ralph-epic-browser-render-debounce 0.05
  "Idle-timer delay (seconds) used to coalesce re-renders.
Matches the debounce used by the per-loop Ralph dashboard so a
burst of controller state-change events collapses into a single
re-render."
  :type 'number
  :group 'beads-ralph-epic-browser)

;;; Faces

(defface beads-ralph-epic-browser-header-face
  '((t :weight bold))
  "Face for the browser's top header line."
  :group 'beads-ralph-epic-browser)

(defface beads-ralph-epic-browser-separator-face
  '((t :inherit shadow))
  "Face for the horizontal separator under the header."
  :group 'beads-ralph-epic-browser)

(defface beads-ralph-epic-browser-id-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for epic IDs."
  :group 'beads-ralph-epic-browser)

(defface beads-ralph-epic-browser-title-face
  '((t :inherit default))
  "Face for epic titles."
  :group 'beads-ralph-epic-browser)

(defface beads-ralph-epic-browser-priority-face
  '((t :inherit font-lock-builtin-face))
  "Face for the priority column (P0, P1, ...)."
  :group 'beads-ralph-epic-browser)

(defface beads-ralph-epic-browser-status-face
  '((t :inherit shadow))
  "Face for the epic status column."
  :group 'beads-ralph-epic-browser)

(defface beads-ralph-epic-browser-progress-face
  '((t :inherit success))
  "Face for the progress fraction."
  :group 'beads-ralph-epic-browser)

(defface beads-ralph-epic-browser-ralph-badge-face
  '((t :inherit warning :weight bold))
  "Face for the `Ralph running' badge."
  :group 'beads-ralph-epic-browser)

;;; Buffer-local state
;;
;; The browser is a singleton, but its data and point-tracking still
;; live buffer-local so a regenerated buffer (after `kill-buffer')
;; starts clean.

(defvar-local beads-ralph-epic-browser--epics nil
  "List of `beads-epic-status' instances the browser last rendered.")

(defvar-local beads-ralph-epic-browser--rerender-timer nil
  "Pending idle-timer for a debounced re-render, or nil.")

;;; Row data extraction
;;
;; Pure functions that turn a `beads-epic-status' into the strings the
;; row component renders.  Kept independent so tests can drive them
;; with fixture instances without standing up the whole vui mount.

(defun beads-ralph-epic-browser--row-id (epic-status)
  "Return the epic id string from EPIC-STATUS."
  (oref (oref epic-status epic) id))

(defun beads-ralph-epic-browser--row-title (epic-status)
  "Return the epic title from EPIC-STATUS, or the empty string.
Embedded control whitespace (newlines, tabs) is collapsed to a
single space so a stray newline in the title cannot break the
row layout."
  (let ((title (or (oref (oref epic-status epic) title) "")))
    (replace-regexp-in-string "[\n\t\r]+" " " title)))

(defun beads-ralph-epic-browser--row-priority (epic-status)
  "Return the priority column string (e.g. \"P1\") for EPIC-STATUS."
  (let ((p (oref (oref epic-status epic) priority)))
    (if (integerp p) (format "P%d" p) "P?")))

(defun beads-ralph-epic-browser--row-status (epic-status)
  "Return the abbreviated status string for EPIC-STATUS.
`in_progress' is shortened to `in_prog' to fit a compact status
column; all other statuses pass through unchanged."
  (let ((s (oref (oref epic-status epic) status)))
    (cond ((null s) "")
          ((string= s "in_progress") "in_prog")
          (t s))))

(defun beads-ralph-epic-browser--row-progress (epic-status)
  "Return the progress fraction (\"closed/total\") for EPIC-STATUS."
  (format "%d/%d"
          (oref epic-status closed-children)
          (oref epic-status total-children)))

(defun beads-ralph-epic-browser--row-ralph-badge (epic-status)
  "Return the `Ralph running' badge string for EPIC-STATUS, or nil.
Reads the public controller registry; nil when no live controller is
bound to the epic's id."
  (when-let* ((id (beads-ralph-epic-browser--row-id epic-status))
              (controller (beads-agent-ralph-controller-for-root id)))
    (format "[Ralph running, iter %d/%d]"
            (oref controller iteration)
            (oref controller max-iterations))))

;;; Header

(defun beads-ralph-epic-browser--ralph-running-count (epics)
  "Return how many EPICS have a live Ralph controller."
  (cl-count-if (lambda (es)
                 (beads-agent-ralph-controller-for-root
                  (beads-ralph-epic-browser--row-id es)))
               epics))

(defun beads-ralph-epic-browser--header-line (epics)
  "Return the browser header string for EPICS."
  (let* ((total (length epics))
         (running (beads-ralph-epic-browser--ralph-running-count epics)))
    (format "Epics — %d total · %d Ralph running" total running)))

;;; Row + composition

(defun beads-ralph-epic-browser--row-vnode (epic-status)
  "Return a vui vnode for one EPIC-STATUS row.
Each column is propertized with its own face so the row reads at
a glance even at a distance — id in constant-face, title in
default, priority in builtin-face, status in shadow, progress in
success, and the Ralph badge in warning-bold."
  (let* ((id (beads-ralph-epic-browser--row-id epic-status))
         (title (truncate-string-to-width
                 (beads-ralph-epic-browser--row-title epic-status)
                 40 nil ?\s "…"))
         (priority (beads-ralph-epic-browser--row-priority epic-status))
         (status (beads-ralph-epic-browser--row-status epic-status))
         (progress (beads-ralph-epic-browser--row-progress epic-status))
         (badge (beads-ralph-epic-browser--row-ralph-badge epic-status))
         (line (concat
                "  "
                (propertize (format "%-10s" id)
                            'face 'beads-ralph-epic-browser-id-face)
                " "
                (propertize (format "%-40s" title)
                            'face 'beads-ralph-epic-browser-title-face)
                " "
                (propertize (format "%-3s" priority)
                            'face 'beads-ralph-epic-browser-priority-face)
                " "
                (propertize (format "%-8s" status)
                            'face 'beads-ralph-epic-browser-status-face)
                " "
                (propertize progress
                            'face 'beads-ralph-epic-browser-progress-face)
                (when badge
                  (concat "  "
                          (propertize
                           badge
                           'face
                           'beads-ralph-epic-browser-ralph-badge-face))))))
    (vui-text line)))

(defun beads-ralph-epic-browser--action-bar ()
  "Return the action-bar legend vnode.
The legend must mirror the bindings in
`beads-ralph-epic-browser-mode-map'."
  (vui-text "[R]un · [d]etails · [g]refresh · [q]uit"
            :face 'shadow))

(defun beads-ralph-epic-browser--separator ()
  "Return the horizontal separator vnode displayed under the header."
  (vui-text (make-string 64 ?─)
            :face 'beads-ralph-epic-browser-separator-face))

(vui-defcomponent beads-ralph-epic-browser--root (epics)
  "Top-level epic-browser composition.
EPICS is a list of `beads-epic-status' instances to render."
  :render
  (vui-error-boundary
   :id 'beads-ralph-epic-browser
   :fallback
   (lambda (err)
     (vui-vstack
       (vui-text (format "Epic browser render error: %S" err)
                 :face 'error)
       (vui-text "(re-render will retry; see *Messages*)"
                 :face 'shadow)))
   :children
   (list
    (vui-vstack
      (vui-text (beads-ralph-epic-browser--header-line epics)
                :face 'beads-ralph-epic-browser-header-face)
      (beads-ralph-epic-browser--separator)
      (if (null epics)
          (vui-text "  (no epics)" :face 'shadow)
        (apply #'vui-vstack
               (mapcar #'beads-ralph-epic-browser--row-vnode epics)))
      (vui-text "")
      (beads-ralph-epic-browser--action-bar)))))

;;; Render driver

(defun beads-ralph-epic-browser--render (buffer epics)
  "Mount the epic-browser component into BUFFER with EPICS.
Installs the buffer's `kill-buffer-hook' once so a buffer kill
mid-flight cancels any pending re-render timer."
  (with-current-buffer buffer
    (unless (eq major-mode 'beads-ralph-epic-browser-mode)
      (beads-ralph-epic-browser-mode))
    (setq-local beads-ralph-epic-browser--epics epics)
    (add-hook 'kill-buffer-hook
              #'beads-ralph-epic-browser--cancel-pending-rerender
              nil t)
    (vui-mount
     (vui-component 'beads-ralph-epic-browser--root :epics epics)
     (buffer-name buffer))))

(defun beads-ralph-epic-browser--fetch-epics ()
  "Fetch the current list of `beads-epic-status' instances via bd."
  (beads-execute 'beads-command-epic-status))

;;; Point navigation
;;
;; vui owns the buffer contents, so we recover the epic-id at point
;; by parsing the line.  Each row begins with two-space indent then
;; the epic id; the action-bar legend at the bottom does not match.

(defun beads-ralph-epic-browser--epic-id-at-point ()
  "Return the epic-id on the current line, or nil if point is not on a row.
A row starts with the literal two-space indent emitted by
`--row-vnode'.  The match is then cross-checked against the
buffer-local epic list so the action-bar legend, header, and
separator lines never resolve to an id."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  \\([a-z0-9.-]+\\)\\s-")
      (let ((id (match-string-no-properties 1)))
        (when (cl-find id beads-ralph-epic-browser--epics
                       :test (lambda (i es)
                               (string= i (beads-ralph-epic-browser--row-id es))))
          id)))))

;;; Interactive commands

(defun beads-ralph-epic-browser--epic-id-at-point-or-error ()
  "Return the epic id at point, or signal a `user-error'."
  (or (beads-ralph-epic-browser--epic-id-at-point)
      (user-error "No epic at point")))

(defun beads-ralph-epic-browser-show ()
  "Show the epic at point via `beads-show'."
  (interactive)
  (beads-show (beads-ralph-epic-browser--epic-id-at-point-or-error)))

(defun beads-ralph-epic-browser-launch ()
  "Open the Ralph launcher on the epic at point.
If `beads-ralph-launcher' (bde-deqx.5) is not yet loaded, falls
back to the existing `beads-agent-ralph-launch' confirm dialog."
  (interactive)
  (let ((id (beads-ralph-epic-browser--epic-id-at-point-or-error)))
    (if (fboundp 'beads-ralph-launcher)
        (beads-ralph-launcher id :kind 'epic)
      (beads-agent-ralph-launch :issue id :kind 'epic))))

(defun beads-ralph-epic-browser-refresh ()
  "Refresh the epic browser from `bd epic status --json'."
  (interactive)
  (let ((buf (current-buffer)))
    (beads-ralph-epic-browser--render buf
                                      (beads-ralph-epic-browser--fetch-epics))))

(defun beads-ralph-epic-browser-next ()
  "Move point to the next epic row."
  (interactive)
  (forward-line 1))

(defun beads-ralph-epic-browser-previous ()
  "Move point to the previous epic row."
  (interactive)
  (forward-line -1))

;;; State-change subscription

(defun beads-ralph-epic-browser--flush-rerender (buffer)
  "Re-render BUFFER if it is still live and showing the browser."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local beads-ralph-epic-browser--rerender-timer nil)
      (when (and (eq major-mode 'beads-ralph-epic-browser-mode)
                 beads-ralph-epic-browser--epics)
        (condition-case err
            (beads-ralph-epic-browser--render
             buffer beads-ralph-epic-browser--epics)
          (error
           (message "beads-ralph-epic-browser: render errored: %S" err)))))))

(defun beads-ralph-epic-browser--schedule-rerender (buffer)
  "Schedule a debounced re-render of BUFFER.
No-op when BUFFER is not in `beads-ralph-epic-browser-mode' so a
buffer that merely happens to share the singleton name (created
by an unrelated caller) is not touched."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq major-mode 'beads-ralph-epic-browser-mode)
                 (not beads-ralph-epic-browser--rerender-timer))
        (setq-local beads-ralph-epic-browser--rerender-timer
                    (run-with-idle-timer
                     beads-ralph-epic-browser-render-debounce
                     nil
                     #'beads-ralph-epic-browser--flush-rerender
                     buffer))))))

(defun beads-ralph-epic-browser--cancel-pending-rerender ()
  "Cancel the pending re-render timer for the current buffer, if any.
Used by the buffer's `kill-buffer-hook' so a buffer kill mid-flight
does not leave a queued idle-timer firing against a dead buffer."
  (when (timerp beads-ralph-epic-browser--rerender-timer)
    (cancel-timer beads-ralph-epic-browser--rerender-timer))
  (setq-local beads-ralph-epic-browser--rerender-timer nil))

(defun beads-ralph-epic-browser--on-controller-state-change
    (_controller _status)
  "Hook into `beads-agent-ralph-state-change-functions'.
Schedules a debounced re-render of the singleton browser buffer.
The hook ignores its arguments and refreshes the badge by reading
the controller registry afresh."
  (let ((buf (get-buffer beads-ralph-epic-browser-buffer-name)))
    (when buf
      (beads-ralph-epic-browser--schedule-rerender buf))))

(add-hook 'beads-agent-ralph-state-change-functions
          #'beads-ralph-epic-browser--on-controller-state-change)

;;; Mode + keymap

(defvar beads-ralph-epic-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") #'beads-ralph-epic-browser-launch)
    ;; Aliased to `beads-ralph-launch-at-point' for consistency with
    ;; `beads-list-mode' and `beads-show-mode', which both bind `L'
    ;; to the same context-sensitive entry point.  Here it routes
    ;; through the public launcher resolver so the kind is computed
    ;; from `issue_type' rather than hard-coded to `'epic.
    (define-key map (kbd "L") #'beads-ralph-launch-at-point)
    (define-key map (kbd "d") #'beads-ralph-epic-browser-show)
    (define-key map (kbd "RET") #'beads-ralph-epic-browser-show)
    (define-key map (kbd "g") #'beads-ralph-epic-browser-refresh)
    (define-key map (kbd "n") #'beads-ralph-epic-browser-next)
    (define-key map (kbd "p") #'beads-ralph-epic-browser-previous)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-ralph-epic-browser-mode'.")

(define-derived-mode beads-ralph-epic-browser-mode vui-mode
  "Beads-Epics"
  "Major mode for the Phase 1 minimum vui epic browser.

\\{beads-ralph-epic-browser-mode-map}"
  (setq truncate-lines t))

;;; Entry point

;;;###autoload
(defun beads-ralph-epic-browser ()
  "Open the Phase 1 minimum vui epic browser.
Fetches epics via `bd epic status --json' and renders them into
the singleton buffer `*beads-epics*'."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (epics (beads-ralph-epic-browser--fetch-epics))
         (buf (get-buffer-create beads-ralph-epic-browser-buffer-name)))
    (with-current-buffer buf
      (setq default-directory caller-dir))
    (beads-ralph-epic-browser--render buf epics)
    ;; `vui-mount' calls `switch-to-buffer' inside `--render', which
    ;; takes over the current window.  Re-invoke `pop-to-buffer' so
    ;; the user lands in a side window (matching the rest of the
    ;; beads.el porcelain) rather than displacing what they were
    ;; looking at.
    (pop-to-buffer buf)
    buf))

(provide 'beads-ralph-epic-browser)

;;; beads-ralph-epic-browser.el ends here
