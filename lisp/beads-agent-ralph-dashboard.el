;;; beads-agent-ralph-dashboard.el --- Vui dashboard for the Ralph loop -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; The live dashboard for a running `beads-agent-ralph' controller.
;; One buffer per loop, named `*beads-agent-ralph: <root-id>*'.
;; Three regions arranged top-to-bottom (vui-vstack), weighted roughly
;; 10% / 20% / 70%:
;;
;;   Header           one dense line + an expandable second line.
;;   Iterations table column set chosen at start, locked for the run.
;;   Live stream      events from the in-flight stream, coalesced and
;;                    debounced.  Replay mode swaps the source to a
;;                    historical iter's captured NDJSON.
;;   Banner / action  severity-ranked banner + 72-col action bar.
;;
;; The dashboard subscribes to the stream via the existing subscriber
;; mechanism and re-renders on event arrival (rate-limited by
;; `beads-agent-ralph-render-debounce').  Process / sentinel state
;; remains owned by `beads-agent-ralph.el'; the dashboard only reads.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vui)
(require 'beads-agent-ralph)
(require 'beads-agent-ralph-stream)

;;; Customization

(defcustom beads-agent-ralph-render-debounce 0.05
  "Idle seconds before the dashboard flushes pending stream events.
The filter never dispatches synchronously; it sets
`pending-render' on the stream and the flush coalesces all events
within this idle window.  0.05 keeps the dashboard reactive without
re-rendering for every NDJSON line.  Increase if rendering is the
bottleneck on a particularly chatty run."
  :type 'number
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-include-hook-events nil
  "Non-nil to render hook-event types in the live stream.
Hook events (claude-code internal events not in the message taxonomy)
are normally hidden; set to t when debugging stream behaviour."
  :type 'boolean
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-inline-line-cap 50
  "Maximum lines of a tool_result rendered inline in the live stream.
Longer outputs are capped with an `expand → side buffer' hint instead
of inflating the dashboard."
  :type 'integer
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-inline-collapse-cap 10
  "Lines of a tool_result shown by default before the expand affordance.
Up to this many lines render inline; the rest are reachable through
the `expand' action."
  :type 'integer
  :group 'beads-agent-ralph)

;;; Buffer naming

(defun beads-agent-ralph-dashboard--buffer-name (root-id)
  "Return the dashboard buffer name for ROOT-ID."
  (format "*beads-agent-ralph: %s*" (or root-id "?")))

(defun beads-agent-ralph-dashboard--side-buffer-name (root-id iter tool)
  "Return the side buffer name for ROOT-ID's expanded tool output.
ITER is the iteration index, TOOL is the tool-use block name."
  (format "*beads-agent-ralph stream %s/%s %s*"
          (or root-id "?") (or iter "?") (or tool "tool")))

;;; Event taxonomy

(defun beads-agent-ralph-dashboard--event-type (event)
  "Return EVENT's effective type symbol/string for taxonomy dispatch.
Coerces plist or alist representations; nil when type slot is absent."
  (and (listp event)
       (or (plist-get event :type)
           (cdr (assoc "type" event)))))

(defun beads-agent-ralph-dashboard--event-subtype (event)
  "Return EVENT's :subtype or :__synthesized-from-partials marker."
  (and (listp event)
       (or (plist-get event :subtype)
           (cdr (assoc "subtype" event)))))

(defun beads-agent-ralph-dashboard--type-eq (event tag)
  "Compare EVENT's type field against TAG (a string), tolerating symbols."
  (let ((type (beads-agent-ralph-dashboard--event-type event)))
    (or (equal type tag) (eq type (intern tag)))))

(defun beads-agent-ralph-dashboard--subtype-eq (event tag)
  "Compare EVENT's subtype field against TAG (a string)."
  (let ((sub (beads-agent-ralph-dashboard--event-subtype event)))
    (or (equal sub tag) (eq sub (intern tag)))))

(defun beads-agent-ralph-dashboard--assistant-blocks (event)
  "Return the assistant message content blocks from EVENT.
Handles both NDJSON (plist) and JSONL-read (alist) shapes."
  (let* ((msg (or (plist-get event :message)
                  (cdr (assoc "message" event))))
         (content (and (listp msg)
                       (or (plist-get msg :content)
                           (cdr (assoc "content" msg))))))
    (when (listp content) content)))

(defun beads-agent-ralph-dashboard--block-field (block field-keyword field-string)
  "Read FIELD-KEYWORD (or FIELD-STRING) from BLOCK regardless of plist/alist."
  (or (plist-get block field-keyword)
      (cdr (assoc field-string block))))

(defun beads-agent-ralph-dashboard--block-type (block)
  "Return BLOCK's :type field as a string."
  (let ((bt (beads-agent-ralph-dashboard--block-field block :type "type")))
    (cond
     ((stringp bt) bt)
     ((symbolp bt) (symbol-name bt))
     (t nil))))

(defun beads-agent-ralph-dashboard--cap-text-lines (text)
  "Cap TEXT to a previewable form per inline-line-cap rules.
Returns a plist (:preview STRING :truncated BOOL :total INT) so the
renderer can show the expand affordance when truncated."
  (let* ((lines (split-string (or text "") "\n"))
         (total (length lines))
         (collapse beads-agent-ralph-inline-collapse-cap)
         (cap beads-agent-ralph-inline-line-cap)
         (head-count (min total collapse))
         (truncated (> total cap))
         (preview-lines (if truncated
                            (cl-subseq lines 0 head-count)
                          (cl-subseq lines 0 head-count))))
    (list :preview (mapconcat #'identity preview-lines "\n")
          :truncated truncated
          :total total)))

;;; Block renderers

(defun beads-agent-ralph-dashboard--render-text-block (block)
  "Return a vnode for an assistant text BLOCK."
  (vui-text
   (or (beads-agent-ralph-dashboard--block-field block :text "text") "")))

(defun beads-agent-ralph-dashboard--render-thinking-block (block)
  "Return a collapsed vnode for an assistant thinking BLOCK."
  (let* ((text (or (beads-agent-ralph-dashboard--block-field
                    block :text "text") ""))
         (preview (truncate-string-to-width
                   (replace-regexp-in-string "\n" " " text)
                   80 nil ?\s "…")))
    (vui-text (format "  · thinking: %s" preview) :face 'shadow)))

(defun beads-agent-ralph-dashboard--render-tool-use-block (block)
  "Return a vnode card for a tool_use BLOCK."
  (let* ((name (or (beads-agent-ralph-dashboard--block-field
                    block :name "name") "?"))
         (input (or (beads-agent-ralph-dashboard--block-field
                     block :input "input") nil))
         (input-preview
          (cond
           ((null input) "")
           ((stringp input)
            (truncate-string-to-width input 80 nil nil "…"))
           ((listp input)
            (truncate-string-to-width
             (replace-regexp-in-string
              "\n" " "
              (or (plist-get input :command)
                  (plist-get input :file_path)
                  (plist-get input :path)
                  (format "%S" input)))
             80 nil nil "…"))
           (t ""))))
    (vui-text (format "  ▶ %s(%s)" name input-preview))))

(defun beads-agent-ralph-dashboard--render-tool-result-block (block)
  "Return a vnode card for a tool_result BLOCK with cap+expand affordance."
  (let* ((content (beads-agent-ralph-dashboard--block-field
                   block :content "content"))
         (text (cond
                ((stringp content) content)
                ((listp content)
                 (mapconcat
                  (lambda (sub)
                    (or (beads-agent-ralph-dashboard--block-field
                         sub :text "text")
                        ""))
                  content "\n"))
                (t "")))
         (cap (beads-agent-ralph-dashboard--cap-text-lines text)))
    (apply
     #'vui-vstack
     (vui-text "  ◆ tool result:")
     (vui-text (plist-get cap :preview) :face 'shadow)
     (when (plist-get cap :truncated)
       (list (vui-text (format "  (… %d lines total; expand for full output)"
                               (plist-get cap :total))
                       :face 'shadow))))))

(defun beads-agent-ralph-dashboard--render-block (block)
  "Dispatch BLOCK to the right block renderer."
  (pcase (beads-agent-ralph-dashboard--block-type block)
    ("text"        (beads-agent-ralph-dashboard--render-text-block block))
    ("thinking"    (beads-agent-ralph-dashboard--render-thinking-block block))
    ("tool_use"    (beads-agent-ralph-dashboard--render-tool-use-block block))
    ("tool_result" (beads-agent-ralph-dashboard--render-tool-result-block
                    block))
    (_             (vui-text (format "  ? %S" block) :face 'shadow))))

;;; Event renderers

(defun beads-agent-ralph-dashboard--render-system-init (event)
  "Return a vnode for a `system.init' EVENT (model, tools)."
  (let* ((model (or (plist-get event :model) (cdr (assoc "model" event))))
         (tools (or (plist-get event :tools) (cdr (assoc "tools" event))))
         (tool-count (and (listp tools) (length tools))))
    (vui-text
     (format "▸ Session · model %s · %d tools"
             (or model "?") (or tool-count 0))
     :face 'shadow)))

(defun beads-agent-ralph-dashboard--render-result (event)
  "Return a vnode for a final `result' EVENT (cost, duration)."
  (let* ((cost (or (plist-get event :total_cost_usd)
                   (plist-get event :cost_usd)
                   (cdr (assoc "total_cost_usd" event))
                   (cdr (assoc "cost_usd" event))))
         (dur (or (plist-get event :duration_ms)
                  (cdr (assoc "duration_ms" event))))
         (sub (beads-agent-ralph-dashboard--event-subtype event)))
    (vui-text
     (format "● Result%s · cost $%s · duration %sms"
             (if sub (format " (%s)" sub) "")
             (if (numberp cost) (format "%.4f" cost) "?")
             (or dur "?"))
     :face 'success)))

(defun beads-agent-ralph-dashboard--render-event (event)
  "Top-level dispatcher.  Return a vnode for EVENT or nil to skip."
  (cond
   ((or (beads-agent-ralph-dashboard--type-eq event "stream_event")
        (eq (beads-agent-ralph-dashboard--event-type event) 'stream_event))
    ;; Partial events are bucketed by the parser; the synthesized
    ;; `assistant' event is rendered.  Skip the envelope verbatim to
    ;; avoid duplicate rendering.
    nil)
   ((and (beads-agent-ralph-dashboard--type-eq event "system")
         (beads-agent-ralph-dashboard--subtype-eq event "init"))
    (beads-agent-ralph-dashboard--render-system-init event))
   ((and (beads-agent-ralph-dashboard--type-eq event "system")
         (beads-agent-ralph-dashboard--subtype-eq event "api_retry"))
    (vui-text "⚠ API retry" :face 'warning))
   ((beads-agent-ralph-dashboard--type-eq event "assistant")
    (let ((blocks (beads-agent-ralph-dashboard--assistant-blocks event)))
      (if (null blocks)
          (vui-text "(empty assistant message)" :face 'shadow)
        (apply #'vui-vstack
               (mapcar #'beads-agent-ralph-dashboard--render-block blocks)))))
   ((beads-agent-ralph-dashboard--type-eq event "user")
    (let ((blocks (beads-agent-ralph-dashboard--assistant-blocks event)))
      (if (null blocks)
          (vui-text "(empty user message)" :face 'shadow)
        (apply #'vui-vstack
               (mapcar #'beads-agent-ralph-dashboard--render-block blocks)))))
   ((beads-agent-ralph-dashboard--type-eq event "result")
    (beads-agent-ralph-dashboard--render-result event))
   ((beads-agent-ralph-dashboard--type-eq event "error")
    (vui-text (format "✗ %S"
                      (or (plist-get event :raw)
                          (cdr (assoc "raw" event))
                          event))
              :face 'error))
   ((and (not beads-agent-ralph-include-hook-events)
         (let ((type (beads-agent-ralph-dashboard--event-type event)))
           (and type (string-prefix-p "hook" (format "%s" type)))))
    nil)
   (t
    (vui-text (format "  · %S" event) :face 'shadow))))

;;; Header

(defun beads-agent-ralph-dashboard--format-elapsed (started-at)
  "Return human-readable elapsed time since STARTED-AT (a list time)."
  (cond
   ((null started-at) "?")
   (t
    (let* ((seconds (truncate (float-time
                               (time-subtract (current-time) started-at))))
           (hours (/ seconds 3600))
           (mins (% (/ seconds 60) 60))
           (secs (% seconds 60)))
      (cond
       ((> hours 0) (format "%dh%dm" hours mins))
       ((> mins 0) (format "%dm" mins))
       (t (format "%ds" secs)))))))

(defun beads-agent-ralph-dashboard--header-line (controller)
  "Return the single dense header line for CONTROLLER."
  (format "Ralph · %s · %s · %s · iter %d/%d · $%.4f"
          (or (oref controller current-issue-id)
              (oref controller root-id) "?")
          (or (oref controller status) "?")
          (beads-agent-ralph-dashboard--format-elapsed
           (oref controller started-at))
          (oref controller iteration)
          (oref controller max-iterations)
          (oref controller cumulative-cost-usd)))

(defun beads-agent-ralph-dashboard--secondary-line (controller)
  "Return the secondary header detail line.
Surfaces model, backend kind, sentinel state.  Rendered when the
header expansion key is toggled."
  (let ((stream (oref controller current-stream)))
    (format "  model=%s · kind=%s · sentinel=%s"
            (or (oref controller model) "default")
            (or (oref controller root-kind) "?")
            (cond
             ((null stream) "—")
             ((and (slot-boundp stream 'sentinel-hit)
                   (oref stream sentinel-hit)) "HIT")
             (t "waiting")))))

;;; Banner severity

(defun beads-agent-ralph-dashboard--top-banner (controller)
  "Return the highest-severity banner record from CONTROLLER, or nil.
Severity ordering: error > warning > notice > info."
  (let ((log (oref controller banner-log)))
    (when log
      (car
       (sort (copy-sequence log)
             (lambda (a b)
               (> (or (cdr (assq (plist-get a :severity)
                                 beads-agent-ralph--banner-severity-rank))
                      0)
                  (or (cdr (assq (plist-get b :severity)
                                 beads-agent-ralph--banner-severity-rank))
                      0))))))))

(defun beads-agent-ralph-dashboard--banner-line (controller)
  "Return the top banner as a vnode, or nil if no banners."
  (when-let ((entry (beads-agent-ralph-dashboard--top-banner controller)))
    (let ((face (pcase (plist-get entry :severity)
                  ('error 'error) ('warning 'warning)
                  ('notice 'shadow) (_ 'success))))
      (vui-text (format "▶ %s" (plist-get entry :text))
                :face face))))

;;; Iterations table

(defun beads-agent-ralph-dashboard--iter-row (iter idx)
  "Return one row vnode for ITER (a `--iteration') at IDX."
  (let* ((status (oref iter status))
         (sentinel (oref iter sentinel-hit))
         (root-closed-mismatch
          ;; Forensic glyph: agent claimed completion but the loop did
          ;; not terminate via `closed' — keep a single-char trail.
          (and sentinel (not (eq status 'finished))))
         (glyph (if root-closed-mismatch "!" " "))
         (status-mark (pcase status
                        ('finished "✓ closed")
                        ('failed   "✗ failed")
                        ('stopped  "■ stopped")
                        (_         "▶ live")))
         (cost (if (oref iter cost-usd)
                   (format "$%.4f" (oref iter cost-usd))
                 "$—"))
         (summary (or (oref iter summary) "")))
    (vui-text
     (format "  %s#%-3d %-12s %-10s %-9s %s"
             glyph idx (or (oref iter issue-id) "?")
             status-mark cost
             (if (> (length summary) 60)
                 (concat (substring summary 0 60) "…")
               summary)))))

(defun beads-agent-ralph-dashboard--iter-table (controller)
  "Return the iterations table for CONTROLLER, newest-first."
  (let* ((history (oref controller history))
         (len (length history))
         (rows (cl-loop for iter in history
                        for i downfrom len
                        collect (beads-agent-ralph-dashboard--iter-row
                                 iter i))))
    (apply #'vui-vstack
           (if rows
               (cons (vui-text "Iterations (newest first):" :face 'bold)
                     rows)
             (list (vui-text "Iterations: (none yet)" :face 'shadow))))))

;;; Live stream region

(defun beads-agent-ralph-dashboard--live-stream-events (controller)
  "Return CONTROLLER's current stream events in receive order, or nil."
  (let ((stream (oref controller current-stream)))
    (when (and stream (slot-boundp stream 'events))
      (reverse (oref stream events)))))

(defun beads-agent-ralph-dashboard--live-stream (controller)
  "Return the live stream region vnode for CONTROLLER."
  (let* ((events (beads-agent-ralph-dashboard--live-stream-events controller))
         (rendered (cl-loop for event in events
                            for vnode = (beads-agent-ralph-dashboard--render-event
                                         event)
                            when vnode collect vnode)))
    (apply #'vui-vstack
           (vui-text "Live stream:" :face 'bold)
           (or rendered
               (list (vui-text "  (waiting for first event)"
                               :face 'shadow))))))

;;; Action bar

(defun beads-agent-ralph-dashboard--action-bar ()
  "Return the action-bar legend vnode."
  (vui-text "[s]top [k]ill [p]ause [r]esume [e]dit [b]udget [v]iew [B]anners [?]"
            :face 'shadow))

;;; Root composition

(vui-defcomponent beads-agent-ralph-dashboard--root
    (controller)
  "Top-level Ralph dashboard composition.
CONTROLLER is the live `beads-agent-ralph--controller' object."
  :render
  (vui-error-boundary
   :id (list 'beads-agent-ralph-dashboard (oref controller root-id))
   :fallback
   (lambda (err)
     (vui-vstack
       (vui-text (format "Ralph dashboard render error: %S" err)
                 :face 'error)
       (vui-text "(re-render will retry; see *Messages*)"
                 :face 'shadow)))
   :children
   (list
    (vui-vstack
      (vui-text (beads-agent-ralph-dashboard--header-line controller))
      (vui-text (beads-agent-ralph-dashboard--secondary-line controller)
                :face 'shadow)
      (vui-text "")
      (or (beads-agent-ralph-dashboard--banner-line controller)
          (vui-text ""))
      (vui-text "")
      (beads-agent-ralph-dashboard--iter-table controller)
      (vui-text "")
      (beads-agent-ralph-dashboard--live-stream controller)
      (vui-text "")
      (beads-agent-ralph-dashboard--action-bar)))))

;;; Buffer + mode

(defvar-local beads-agent-ralph-dashboard--controller nil
  "Controller bound to this dashboard buffer.")

(defvar beads-agent-ralph-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'beads-agent-ralph-dashboard-stop)
    (define-key map (kbd "k") #'beads-agent-ralph-dashboard-kill-iter)
    (define-key map (kbd "p") #'beads-agent-ralph-dashboard-pause)
    (define-key map (kbd "r") #'beads-agent-ralph-dashboard-resume)
    (define-key map (kbd "v") #'beads-agent-ralph-dashboard-view-issue)
    (define-key map (kbd "B") #'beads-agent-ralph-dashboard-banner-log)
    (define-key map (kbd "g") #'beads-agent-ralph-dashboard-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'beads-agent-ralph-dashboard-help)
    map)
  "Keymap for `beads-agent-ralph-dashboard-mode'.")

(define-derived-mode beads-agent-ralph-dashboard-mode vui-mode
  "Ralph Dashboard"
  "Major mode for the Ralph loop dashboard.
Derived from `vui-mode' so `vui-mount' preserves our keymap and
text-property contract; otherwise vui would switch the buffer back
to `vui-mode' on every re-render.
Keybindings reflect the action bar legend at the foot of the buffer.")

(defun beads-agent-ralph-dashboard-refresh ()
  "Force a re-render of the current dashboard buffer."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (beads-agent-ralph-dashboard-render
     beads-agent-ralph-dashboard--controller)))

(defun beads-agent-ralph-dashboard-stop ()
  "Stop the controller bound to this dashboard."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (beads-agent-ralph-stop beads-agent-ralph-dashboard--controller)
    (beads-agent-ralph-dashboard-refresh)))

(defun beads-agent-ralph-dashboard-kill-iter ()
  "Stop the in-flight iteration; same effect as `stop' currently."
  (interactive)
  (beads-agent-ralph-dashboard-stop))

(defun beads-agent-ralph-dashboard-pause ()
  "Pause the loop after the in-flight iteration completes."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (beads-agent-ralph--pause
     beads-agent-ralph-dashboard--controller
     "Paused by user")
    (beads-agent-ralph-dashboard-refresh)))

(defun beads-agent-ralph-dashboard-resume ()
  "Resume from `auto-paused' / `paused' status."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (let ((c beads-agent-ralph-dashboard--controller))
      (when (memq (oref c status) '(auto-paused stopped))
        (beads-agent-ralph--set-status c 'cooling-down)
        (beads-agent-ralph--schedule-next-iteration
         c (lambda ()
             (beads-agent-ralph--set-status c 'running)
             (beads-agent-ralph--run-iteration c))))
      (beads-agent-ralph-dashboard-refresh))))

(defun beads-agent-ralph-dashboard-view-issue ()
  "Open the current issue in a beads-show buffer."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (let ((id (oref beads-agent-ralph-dashboard--controller current-issue-id)))
      (when (and id (fboundp 'beads-show))
        (funcall 'beads-show id)))))

(defun beads-agent-ralph-dashboard-banner-log ()
  "Pop a buffer with the full banner history."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (let* ((c beads-agent-ralph-dashboard--controller)
           (buf (get-buffer-create
                 (format "*beads-agent-ralph banners: %s*"
                         (oref c root-id)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Banner history (newest first):\n\n")
          (dolist (entry (oref c banner-log))
            (insert (format "  [%-7s] %s\n"
                            (plist-get entry :severity)
                            (plist-get entry :text))))
          (setq-local buffer-read-only t)))
      (pop-to-buffer buf))))

(defun beads-agent-ralph-dashboard-help ()
  "Show the full key legend in the echo area."
  (interactive)
  (message
   "Ralph keys: [s]top [k]ill [p]ause [r]esume [v]iew issue [B]anners [g]refresh [q]uit"))

;;; Mount + re-render machinery

(defvar beads-agent-ralph-dashboard--pending-rerender nil
  "Set of controllers with a queued re-render flush.
Holds a `(controller . timer)' cons per pending flush; the cons is
removed when the timer fires.  Used by
`beads-agent-ralph-dashboard--schedule-rerender' to coalesce filter
events into one render per debounce window.")

(defun beads-agent-ralph-dashboard--flush-rerender (controller)
  "Render CONTROLLER's dashboard if still alive."
  (let* ((buf (get-buffer
               (beads-agent-ralph-dashboard--buffer-name
                (oref controller root-id))))
         (entry (assq controller beads-agent-ralph-dashboard--pending-rerender)))
    (setq beads-agent-ralph-dashboard--pending-rerender
          (assq-delete-all controller
                           beads-agent-ralph-dashboard--pending-rerender))
    (when entry
      (let ((timer (cdr entry)))
        (when (timerp timer) (cancel-timer timer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (condition-case err
            (beads-agent-ralph-dashboard-render controller)
          (error
           (message "beads-agent-ralph-dashboard: render errored: %S" err)
           (when (timerp (cdr entry))
             ;; Re-arm once at a longer delay so a transient error
             ;; doesn't loop hot.
             (run-with-idle-timer
              0.5 nil
              #'beads-agent-ralph-dashboard--flush-rerender controller))))))))

(defun beads-agent-ralph-dashboard--schedule-rerender (controller)
  "Schedule a debounced re-render of CONTROLLER's dashboard."
  (unless (assq controller beads-agent-ralph-dashboard--pending-rerender)
    (let ((timer (run-with-idle-timer
                  beads-agent-ralph-render-debounce
                  nil
                  #'beads-agent-ralph-dashboard--flush-rerender
                  controller)))
      (push (cons controller timer)
            beads-agent-ralph-dashboard--pending-rerender))))

(defun beads-agent-ralph-dashboard--detach-stream (controller)
  "Detach CONTROLLER's current stream so a dashboard kill cannot trip it.

When the user kills the dashboard buffer mid-run we must:
  1. Replace the stream's process sentinel with `ignore' so the
     pending sentinel call cannot transition the stream into
     `finished/failed' against a buffer that no longer exists;
  2. `delete-process' the stream so its file descriptors and IO
     subscribers go away cleanly;
  3. Unsubscribe the dashboard's renderer from the stream so a late
     filter call (between detach and delete) does not run our render
     pipeline against a dead buffer.

This is intentionally idempotent.  The controller's
`current-stream' slot is left in place because the controller's own
sentinel-driven path (e.g. graceful stop, normal completion) still
needs to see the stream object; we only neutralise its IO."
  (let ((stream (oref controller current-stream)))
    (when (and stream (slot-boundp stream 'process))
      (let ((proc (oref stream process)))
        (when (processp proc)
          (set-process-sentinel proc #'ignore)
          (set-process-filter proc #'ignore)
          (when (process-live-p proc)
            (delete-process proc)))
        ;; Drop our dashboard subscriber so a final flush after delete
        ;; does not call us back.
        (when (fboundp 'beads-agent-ralph--stream-unsubscribe)
          (beads-agent-ralph--stream-unsubscribe stream 'controller)
          (beads-agent-ralph--stream-unsubscribe stream 'persist))
        ;; Mark stopped so consumers that read the slot see a sane state.
        (when (slot-boundp stream 'status)
          (oset stream status 'stopped))))))

(defun beads-agent-ralph-dashboard--kill-buffer-cleanup ()
  "Buffer-kill hook for `beads-agent-ralph-dashboard-mode' buffers.
Cancels any pending re-render and detaches the controller's stream
so the live process cannot trip a sentinel transition against a
buffer that no longer exists."
  (when beads-agent-ralph-dashboard--controller
    (let* ((controller beads-agent-ralph-dashboard--controller)
           (entry (assq controller
                        beads-agent-ralph-dashboard--pending-rerender)))
      (when entry
        (let ((timer (cdr entry)))
          (when (timerp timer) (cancel-timer timer)))
        (setq beads-agent-ralph-dashboard--pending-rerender
              (assq-delete-all controller
                               beads-agent-ralph-dashboard--pending-rerender)))
      ;; Only detach the stream when the controller is still in flight;
      ;; a finished/failed/stopped controller has nothing to clean up
      ;; and we shouldn't kill a downstream process that the next
      ;; iteration's resume cycle expects.
      (when (memq (oref controller status) '(running cooling-down))
        (beads-agent-ralph-dashboard--detach-stream controller)))))

(defun beads-agent-ralph-dashboard-render (controller)
  "Render CONTROLLER into its dashboard buffer (sync)."
  (let* ((buf (get-buffer-create
               (beads-agent-ralph-dashboard--buffer-name
                (oref controller root-id)))))
    (with-current-buffer buf
      (unless (eq major-mode 'beads-agent-ralph-dashboard-mode)
        (beads-agent-ralph-dashboard-mode))
      (setq-local beads-agent-ralph-dashboard--controller controller)
      ;; Install the buffer-kill hook once; subsequent mounts re-add
      ;; harmlessly because `add-hook' dedups by `equal'.
      (add-hook 'kill-buffer-hook
                #'beads-agent-ralph-dashboard--kill-buffer-cleanup
                nil t)
      (vui-mount
       (vui-component 'beads-agent-ralph-dashboard--root
                      :controller controller)
       (buffer-name)))
    buf))

;;;###autoload
(defun beads-agent-ralph-dashboard-mount (controller)
  "Open CONTROLLER's dashboard buffer and install the re-render hook.
Returns the dashboard buffer.  Subsequent state changes on CONTROLLER
re-render through `beads-agent-ralph-dashboard-rerender-function'."
  (let ((buf (beads-agent-ralph-dashboard-render controller)))
    ;; Hook into the controller's existing render channel — the
    ;; controller calls this function (when set) on every state
    ;; transition, so we get free coupling.
    (setq beads-agent-ralph-dashboard-rerender-function
          #'beads-agent-ralph-dashboard--schedule-rerender)
    (pop-to-buffer buf)
    buf))

(provide 'beads-agent-ralph-dashboard)

;;; beads-agent-ralph-dashboard.el ends here
