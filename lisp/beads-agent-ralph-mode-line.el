;;; beads-agent-ralph-mode-line.el --- Mode-line + desktop notifications for Ralph -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module surfaces Ralph's loop state outside the dashboard buffer:
;; a mode-line indicator visible while any controller is running, and
;; desktop notifications fired on state transitions per the user's
;; `beads-agent-ralph-notify' policy.
;;
;; Integration follows the pattern in `beads-agent.el':
;;
;;   - The indicator is a `mode-line-misc-info' entry, gated by a global
;;     minor mode, added once on first controller start and removed when
;;     no controller is left (with a sticky tail so terminal states
;;     remain briefly visible).
;;   - State transitions are observed via the
;;     `beads-agent-ralph-state-change-functions' abnormal hook, which
;;     the controller already runs on every status change.
;;
;; The width-adaptive cost formatter keeps the indicator a constant
;; width across threshold crossings so the mode-line does not jitter.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'format-spec)
(require 'subr-x)

(require 'beads-agent-ralph)

;; `notifications' is part of the dbus stack; tests run in batch where
;; it is unavailable.  Load it best-effort so the require does not
;; fail; the call site guards with `fboundp'.
(require 'notifications nil t)

;;; Customization

(defgroup beads-agent-ralph-mode-line nil
  "Mode-line indicator and desktop notifications for the Ralph loop.
The customization variables themselves live in `beads-agent-ralph'
proper (so they stay near the controller); this group exists so
users can find the mode-line subsystem by name in `customize-apropos'."
  :group 'beads-agent-ralph)

(defface beads-agent-ralph-mode-line-running
  '((t :inherit success))
  "Face for the indicator while the controller is running normally."
  :group 'beads-agent-ralph-mode-line)

(defface beads-agent-ralph-mode-line-stalled
  '((t :inherit warning))
  "Face for the indicator while the controller is auto-paused.
Includes stall detection and lying-agent detection."
  :group 'beads-agent-ralph-mode-line)

(defface beads-agent-ralph-mode-line-failed
  '((t :inherit error))
  "Face for the indicator after a terminal failure."
  :group 'beads-agent-ralph-mode-line)

;;; Controller Registry
;;
;; We track active controllers in a simple list so the mode-line
;; indicator can know whether to render itself.  The list is kept
;; ordered newest-first; the most recently started controller is
;; surfaced when many are active.  Sticky entries (terminal controllers
;; lingering for the user to notice) live alongside live ones, tagged
;; with a `:sticky-until' timestamp; a single timer sweeps them out.

(defvar beads-agent-ralph--mode-line-registry nil
  "List of plists describing controllers visible in the mode-line.
Each entry is (:controller CONTROLLER :sticky-until TIME-OR-NIL).
A non-nil `:sticky-until' marks a terminated controller whose
indicator should remain visible until that time.  The list is
ordered newest-first; the head is the controller the indicator
displays.")

(defvar beads-agent-ralph--mode-line-sticky-timer nil
  "Timer that sweeps expired sticky entries from the registry.
Re-armed by `beads-agent-ralph--mode-line-mark-sticky'.")

(defun beads-agent-ralph--mode-line-registry-clear ()
  "Reset the registry and cancel any pending sticky timer.
Intended for tests; the production path never resets unconditionally."
  (setq beads-agent-ralph--mode-line-registry nil)
  (when (timerp beads-agent-ralph--mode-line-sticky-timer)
    (cancel-timer beads-agent-ralph--mode-line-sticky-timer))
  (setq beads-agent-ralph--mode-line-sticky-timer nil))

(defun beads-agent-ralph--mode-line-find (controller)
  "Return the registry entry for CONTROLLER, or nil."
  (cl-find controller beads-agent-ralph--mode-line-registry
           :key (lambda (entry) (plist-get entry :controller))))

(defun beads-agent-ralph--mode-line-register (controller)
  "Ensure CONTROLLER appears at the head of the registry, non-sticky.
A re-register (e.g. resume after auto-pause) clears any sticky tail."
  (setq beads-agent-ralph--mode-line-registry
        (cl-remove controller beads-agent-ralph--mode-line-registry
                   :key (lambda (entry) (plist-get entry :controller))))
  (push (list :controller controller :sticky-until nil)
        beads-agent-ralph--mode-line-registry)
  (beads-agent-ralph--mode-line-enable))

(defun beads-agent-ralph--mode-line-mark-sticky (controller)
  "Mark CONTROLLER's registry entry sticky and schedule a sweep.
The entry is removed when `current-time' exceeds `:sticky-until'."
  (when-let ((entry (beads-agent-ralph--mode-line-find controller)))
    (let ((seconds beads-agent-ralph-mode-line-sticky-seconds))
      (if (or (null seconds) (<= seconds 0))
          (beads-agent-ralph--mode-line-deregister controller)
        (setf (plist-get entry :sticky-until)
              (time-add (current-time) seconds))
        (beads-agent-ralph--mode-line-arm-sweeper seconds)))))

(defun beads-agent-ralph--mode-line-deregister (controller)
  "Remove CONTROLLER from the registry, disabling the mode-line if empty."
  (setq beads-agent-ralph--mode-line-registry
        (cl-remove controller beads-agent-ralph--mode-line-registry
                   :key (lambda (entry) (plist-get entry :controller))))
  (unless beads-agent-ralph--mode-line-registry
    (when (timerp beads-agent-ralph--mode-line-sticky-timer)
      (cancel-timer beads-agent-ralph--mode-line-sticky-timer))
    (setq beads-agent-ralph--mode-line-sticky-timer nil)
    (beads-agent-ralph--mode-line-disable)))

(defun beads-agent-ralph--mode-line-sweep-expired ()
  "Drop sticky entries whose `:sticky-until' is in the past."
  (let ((now (current-time)))
    (setq beads-agent-ralph--mode-line-registry
          (cl-remove-if (lambda (entry)
                          (let ((sticky (plist-get entry :sticky-until)))
                            (and sticky (time-less-p sticky now))))
                        beads-agent-ralph--mode-line-registry)))
  (unless beads-agent-ralph--mode-line-registry
    (when (timerp beads-agent-ralph--mode-line-sticky-timer)
      (cancel-timer beads-agent-ralph--mode-line-sticky-timer))
    (setq beads-agent-ralph--mode-line-sticky-timer nil)
    (beads-agent-ralph--mode-line-disable))
  (force-mode-line-update t))

(defun beads-agent-ralph--mode-line-arm-sweeper (seconds)
  "Schedule the sticky-sweep timer to fire in SECONDS, replacing any pending one."
  (when (timerp beads-agent-ralph--mode-line-sticky-timer)
    (cancel-timer beads-agent-ralph--mode-line-sticky-timer))
  (setq beads-agent-ralph--mode-line-sticky-timer
        (run-at-time seconds nil
                     #'beads-agent-ralph--mode-line-sweep-expired)))

(defun beads-agent-ralph--mode-line-active-controller ()
  "Return the controller the indicator should display, or nil."
  (when-let ((entry (car beads-agent-ralph--mode-line-registry)))
    (plist-get entry :controller)))

;;; Cost formatter

(defun beads-agent-ralph--format-cost (cost-usd)
  "Format COST-USD as a 4-char-wide string after the `$' prefix.
Width is held constant across threshold crossings to avoid mode-line
jitter.  Returns just the digits-and-suffix part: callers prepend `$'.

  < $10       → \"X.XX\"   (e.g. \"4.17\")
  < $100      → \"XX.X\"   (e.g. \"87.3\")
  < $1000     → \"XXX \"   (e.g. \"487 \")
  < $10000    → \"X.Xk\"   (e.g. \"1.2k\")
  ≥ $10000    → \"XXk \"   (e.g. \"12k \")"
  (let ((c (or cost-usd 0)))
    (cond
     ;; Thresholds are pre-rounded so a value that `format' would round
     ;; up across a bucket boundary lands in the wider-format bucket.
     ((< c 9.995)   (format "%.2f" c))
     ((< c 99.95)   (format "%.1f" c))
     ((< c 1000)    (format "%-4d" (truncate c)))
     ((< c 9999.5)  (format "%.1fk" (/ c 1000.0)))
     (t
      (let ((k (truncate (/ c 1000.0))))
        (if (< k 100)
            (format "%dk " k)
          ;; Beyond 99k we deliberately overflow rather than truncate
          ;; data: better a 5-char indicator once than a misleading one.
          (format "%dk" k)))))))

;;; Indicator rendering

(defun beads-agent-ralph--mode-line-face-for-status (status)
  "Return the face symbol the indicator should use for STATUS."
  (pcase status
    ('auto-paused 'beads-agent-ralph-mode-line-stalled)
    ('failed      'beads-agent-ralph-mode-line-failed)
    ((or 'stopped 'done) 'beads-agent-ralph-mode-line-running)
    (_            'beads-agent-ralph-mode-line-running)))

(defun beads-agent-ralph--mode-line-string ()
  "Return the mode-line indicator string, or nil when none should show.
Reads the head of the registry; the controller's iteration, max, and
cumulative cost are formatted into \"Ralph N/M $X.XX\" with a face
matching `status'."
  (when-let ((controller (beads-agent-ralph--mode-line-active-controller)))
    (let* ((iter (oref controller iteration))
           (max-iter (oref controller max-iterations))
           (cost (oref controller cumulative-cost-usd))
           (status (oref controller status))
           (face (beads-agent-ralph--mode-line-face-for-status status)))
      (propertize (format "Ralph %d/%d $%s"
                          (or iter 0)
                          (or max-iter 0)
                          (beads-agent-ralph--format-cost cost))
                  'face face
                  'help-echo (format "Ralph %s · %s · status: %s"
                                     (oref controller root-id)
                                     (oref controller root-kind)
                                     status)))))

;;; Mode-line minor mode
;;
;; Mirrors the `mode-line-misc-info' add-if-absent / delete-specific-
;; entry idiom used by `beads-agent-mode-line-mode' in `beads-agent.el'.
;; The mode is enabled on first controller start and disabled when the
;; registry empties so the entry only lives in `mode-line-misc-info'
;; while there's something to show.

;;;###autoload
(defvar beads-agent-ralph-mode-line
  '(:eval (beads-agent-ralph--mode-line-string))
  "Mode-line construct for the Ralph indicator.
Add to `mode-line-misc-info' (this module does it automatically when
`beads-agent-ralph-mode-line-mode' is enabled).")

;;;###autoload
(put 'beads-agent-ralph-mode-line 'risky-local-variable t)

(defconst beads-agent-ralph--mode-line-misc-info-entry
  '(beads-agent-ralph-mode-line-mode (" " beads-agent-ralph-mode-line))
  "Entry added to `mode-line-misc-info' by `beads-agent-ralph-mode-line-mode'.
Uses the minor-mode conditional form so the indicator only renders
while the mode is enabled, and we can identify it precisely for removal.")

;;;###autoload
(define-minor-mode beads-agent-ralph-mode-line-mode
  "Minor mode that surfaces the Ralph loop indicator in the mode-line.
Enabled automatically on the first controller start; disabled when no
controller is left to display.  Users can toggle manually; the
controller-driven add/remove is idempotent against the user's choice."
  :global t
  :lighter nil
  :group 'beads-agent-ralph-mode-line
  (if beads-agent-ralph-mode-line-mode
      (unless (member beads-agent-ralph--mode-line-misc-info-entry
                      mode-line-misc-info)
        (setq mode-line-misc-info
              (append mode-line-misc-info
                      (list beads-agent-ralph--mode-line-misc-info-entry))))
    (setq mode-line-misc-info
          (delete beads-agent-ralph--mode-line-misc-info-entry
                  mode-line-misc-info))))

(defun beads-agent-ralph--mode-line-enable ()
  "Idempotently enable `beads-agent-ralph-mode-line-mode'."
  (unless beads-agent-ralph-mode-line-mode
    (beads-agent-ralph-mode-line-mode 1)))

(defun beads-agent-ralph--mode-line-disable ()
  "Idempotently disable `beads-agent-ralph-mode-line-mode'."
  (when beads-agent-ralph-mode-line-mode
    (beads-agent-ralph-mode-line-mode -1)))

;;; Notification format helpers

(defun beads-agent-ralph--format-elapsed (started-at)
  "Format wall-clock elapsed since STARTED-AT as a compact string.
Returns e.g. \"47s\", \"22m\", \"1h34m\".  STARTED-AT may be nil, in
which case \"?\" is returned so the notification body still renders."
  (if (null started-at)
      "?"
    (let* ((elapsed (truncate (float-time (time-subtract (current-time)
                                                         started-at))))
           (h (/ elapsed 3600))
           (m (/ (mod elapsed 3600) 60))
           (s (mod elapsed 60)))
      (cond
       ((> h 0) (format "%dh%02dm" h m))
       ((> m 0) (format "%dm" m))
       (t       (format "%ds" s))))))

(defun beads-agent-ralph--notification-reason-text (controller)
  "Return human-readable reason text for CONTROLLER, or nil if none.
The natural-success reason (`sentinel') is suppressed so a clean
`done' body matches the spec example
\"done · bd-42 closed · 12 iters · $4.17 · 1h34m\" without a
redundant trailing reason segment."
  (pcase (oref controller done-reason)
    ('sentinel    nil)
    ('closed      "issue closed externally")
    ('epic-empty  "no ready children")
    ('budget      "budget")
    ('stop        "user stop")
    ('failed      "failure")
    (_
     (cond
      ((> (oref controller consecutive-stalls) 0)
       (format "stalled %d×" (oref controller consecutive-stalls)))
      ((> (oref controller false-claim-count) 0)
       (format "false claim %d×" (oref controller false-claim-count)))
      (t nil)))))

(defun beads-agent-ralph--notification-title (controller)
  "Render the notification title string for CONTROLLER."
  (format beads-agent-ralph-notification-title-format
          (oref controller root-id)))

(defun beads-agent-ralph--notification-body (controller new-status)
  "Render the notification body for CONTROLLER landing in NEW-STATUS."
  (let ((reason (beads-agent-ralph--notification-reason-text controller)))
    (format-spec
     beads-agent-ralph-notification-body-format
     `((?v . ,(symbol-name new-status))
       (?r . ,(oref controller root-id))
       (?s . ,(if (eq new-status 'done) "closed" "open"))
       (?n . ,(number-to-string (or (oref controller iteration) 0)))
       (?c . ,(beads-agent-ralph--format-cost
               (oref controller cumulative-cost-usd)))
       (?e . ,(beads-agent-ralph--format-elapsed
               (oref controller started-at)))
       (?R . ,(if reason (concat " · " reason) ""))))))

;;; Notification policy

(defconst beads-agent-ralph--terminal-statuses
  '(done stopped failed auto-paused)
  "Statuses that count as terminal for notification policy.
Note: `auto-paused' is recoverable but still interrupts the loop, so
it triggers the on-stop policy.  Pure failure-only policy excludes
`done' from this set; see `beads-agent-ralph--should-notify-p'.")

(defun beads-agent-ralph--should-notify-p (new-status)
  "Return non-nil when the Ralph notify policy fires for NEW-STATUS.
Reads `beads-agent-ralph-notify'."
  (pcase beads-agent-ralph-notify
    ('always t)
    ('never nil)
    ('on-stop
     (memq new-status beads-agent-ralph--terminal-statuses))
    ('on-failure
     (memq new-status '(stopped failed auto-paused)))
    (_ nil)))

(defun beads-agent-ralph--send-desktop-notification (title body)
  "Send TITLE/BODY through `notifications-notify' when available.
Falls back to `message' + `ding' in batch or when D-Bus is missing."
  (if (and (fboundp 'notifications-notify)
           (not noninteractive))
      (condition-case _err
          (notifications-notify :title title :body body)
        (error
         (message "%s: %s" title body)
         (ding)))
    (message "%s: %s" title body)
    (unless noninteractive (ding))))

(defun beads-agent-ralph--maybe-notify (controller new-status)
  "Notify (per policy) for CONTROLLER transitioning to NEW-STATUS."
  (when (beads-agent-ralph--should-notify-p new-status)
    (let ((title (beads-agent-ralph--notification-title controller))
          (body  (beads-agent-ralph--notification-body controller new-status)))
      (beads-agent-ralph--send-desktop-notification title body))))

;;; State-change observer
;;
;; Bridges the controller's abnormal hook to the registry and notify
;; subsystem.  Installed unconditionally on load; the policy defcustoms
;; gate the user-visible side-effects.

(defun beads-agent-ralph--mode-line-on-state-change (controller new-status)
  "React to CONTROLLER moving into NEW-STATUS.
Registers/de-registers the controller in the mode-line registry and
fires a desktop notification per `beads-agent-ralph-notify'."
  (pcase new-status
    ((or 'running 'cooling-down 'auto-paused)
     (beads-agent-ralph--mode-line-register controller))
    ((or 'done 'stopped 'failed)
     (beads-agent-ralph--mode-line-register controller)
     (beads-agent-ralph--mode-line-mark-sticky controller))
    (_ nil))
  (beads-agent-ralph--maybe-notify controller new-status)
  (force-mode-line-update t))

(add-hook 'beads-agent-ralph-state-change-functions
          #'beads-agent-ralph--mode-line-on-state-change)

(provide 'beads-agent-ralph-mode-line)

;;; beads-agent-ralph-mode-line.el ends here
