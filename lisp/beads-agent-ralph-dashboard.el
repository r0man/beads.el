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

(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)
(require 'vui)
(require 'beads-agent-ralph)
(require 'beads-agent-ralph-stream)

;;; Customization

(defcustom beads-agent-ralph-render-debounce 0.15
  "Idle seconds before the dashboard flushes pending stream events.
The filter never dispatches synchronously; it sets
`pending-render' on the stream and the flush coalesces all events
within this idle window.  0.15 keeps the dashboard reactive without
re-rendering for every NDJSON line; on a chatty run, 50ms caused
constant flicker and lost cursor position.  Drop to ~0.05 for
real-time debugging, or raise further if rendering is the
bottleneck."
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
renderer can show the expand affordance when truncated.

When total lines exceed `beads-agent-ralph-inline-line-cap', preview is
the first `beads-agent-ralph-inline-collapse-cap' lines and :truncated
is t.  Otherwise preview is the full text and :truncated is nil."
  (let* ((lines (split-string (or text "") "\n"))
         (total (length lines))
         (collapse beads-agent-ralph-inline-collapse-cap)
         (cap beads-agent-ralph-inline-line-cap)
         (truncated (> total cap))
         (preview-lines (if truncated
                            (cl-subseq lines 0 (min total collapse))
                          lines)))
    (list :preview (mapconcat #'identity preview-lines "\n")
          :truncated truncated
          :total total)))

;;; Block renderers

(defun beads-agent-ralph-dashboard--render-text-block (block)
  "Return a vnode for an assistant text BLOCK."
  (vui-text
   (or (beads-agent-ralph-dashboard--block-field block :text "text") "")))

(defun beads-agent-ralph-dashboard--render-thinking-block (block)
  "Return a collapsed vnode for an assistant thinking BLOCK.
Returns nil when the thinking text is empty or whitespace-only so the
caller can drop the vnode (avoids 80-space padded `thinking:' lines
when claude streams an empty thinking delta)."
  (let ((text (or (beads-agent-ralph-dashboard--block-field
                   block :text "text") "")))
    (unless (string-blank-p text)
      (let ((preview (truncate-string-to-width
                      (replace-regexp-in-string "\n" " " text)
                      80 nil ?\s "…")))
        (vui-text (format "  · thinking: %s" preview) :face 'shadow)))))

(defun beads-agent-ralph-dashboard--tool-input-summary (input)
  "Return a short, semantically-useful one-line summary of tool INPUT.
For Bash-style :command strings, keep only the first command (split on
`;', `&&', `||', or newline) so multi-step pipelines don't get cut off
mid-second-step at the character cap.  For Read/Edit/Write tools, prefer
the path field over the verbose plist printer."
  (cond
   ((null input) "")
   ((stringp input)
    (truncate-string-to-width input 80 nil nil "…"))
   ((listp input)
    (let* ((cmd (or (plist-get input :command)
                    (plist-get input :file_path)
                    (plist-get input :path)
                    (format "%S" input)))
           (first (car (split-string cmd
                                     "\n\\|;\\|&&\\|||"
                                     t "[ \t]+"))))
      (truncate-string-to-width
       (replace-regexp-in-string "[ \t]+" " " (or first cmd))
       80 nil nil "…")))
   (t "")))

(defun beads-agent-ralph-dashboard--render-tool-use-block (block)
  "Return a vnode card for a tool_use BLOCK."
  (let ((name (or (beads-agent-ralph-dashboard--block-field
                   block :name "name") "?"))
        (input (or (beads-agent-ralph-dashboard--block-field
                    block :input "input") nil)))
    (vui-text (format "  ▶ %s(%s)"
                      name
                      (beads-agent-ralph-dashboard--tool-input-summary input)))))

(defun beads-agent-ralph-dashboard--render-tool-result-block (block)
  "Return a vnode card for a tool_result BLOCK with cap+expand affordance.
Strips ANSI/VT100 escape sequences from the text first so raw CSI
codes (e.g. `\\[K' from `guix substitute' progress lines) don't leak
into the dashboard."
  (let* ((content (beads-agent-ralph-dashboard--block-field
                   block :content "content"))
         (raw-text (cond
                    ((stringp content) content)
                    ((listp content)
                     (mapconcat
                      (lambda (sub)
                        (or (beads-agent-ralph-dashboard--block-field
                             sub :text "text")
                            ""))
                      content "\n"))
                    (t "")))
         (text (ansi-color-filter-apply raw-text))
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
   ;; Per-turn pacing partials from claude (`system/status status=requesting'
   ;; arrive between every tool call) carry no information beyond `the
   ;; agent is about to do something' — drop them to cut stream noise.
   ((and (beads-agent-ralph-dashboard--type-eq event "system")
         (beads-agent-ralph-dashboard--subtype-eq event "status"))
    nil)
   ;; Per-turn rate-limit pings are pacing partials.  Keep them only when
   ;; their status is anything other than `allowed' (i.e. throttle warnings).
   ((and (beads-agent-ralph-dashboard--type-eq event "rate_limit_event")
         (let ((info (or (plist-get event :rate_limit_info)
                         (cdr (assoc "rate_limit_info" event)))))
           (and (listp info)
                (member (or (plist-get info :status)
                            (cdr (assoc "status" info)))
                        '("allowed" allowed)))))
    nil)
   ((beads-agent-ralph-dashboard--type-eq event "assistant")
    (let* ((blocks (beads-agent-ralph-dashboard--assistant-blocks event))
           (rendered (delq nil (mapcar
                                #'beads-agent-ralph-dashboard--render-block
                                blocks))))
      (cond
       ((null blocks)   (vui-text "(empty assistant message)" :face 'shadow))
       ((null rendered) nil)
       (t (apply #'vui-vstack rendered)))))
   ((beads-agent-ralph-dashboard--type-eq event "user")
    (let* ((blocks (beads-agent-ralph-dashboard--assistant-blocks event))
           (rendered (delq nil (mapcar
                                #'beads-agent-ralph-dashboard--render-block
                                blocks))))
      (cond
       ((null blocks)   (vui-text "(empty user message)" :face 'shadow))
       ((null rendered) nil)
       (t (apply #'vui-vstack rendered)))))
   ((beads-agent-ralph-dashboard--type-eq event "result")
    (beads-agent-ralph-dashboard--render-result event))
   ((beads-agent-ralph-dashboard--type-eq event "error")
    (vui-text (format "✗ %S"
                      (or (plist-get event :raw)
                          (cdr (assoc "raw" event))
                          event))
              :face 'error))
   ((and (not beads-agent-ralph-include-hook-events)
         (let ((type (beads-agent-ralph-dashboard--event-type event))
               (sub  (beads-agent-ralph-dashboard--event-subtype event)))
           (or (and type (string-prefix-p "hook" (format "%s" type)))
               (and sub  (string-prefix-p "hook" (format "%s" sub))))))
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
  "Return the secondary header detail line for CONTROLLER.
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
  "Return the top banner from CONTROLLER as a vnode, or nil if no banners."
  (when-let ((entry (beads-agent-ralph-dashboard--top-banner controller)))
    (let ((face (pcase (plist-get entry :severity)
                  ('error 'error) ('warning 'warning)
                  ('notice 'shadow) (_ 'success))))
      (vui-text (format "▶ %s" (plist-get entry :text))
                :face face))))

;;; Iterations table

(defun beads-agent-ralph-dashboard--format-duration (millis)
  "Format MILLIS as a compact wall-clock (`12s', `3m12s', `1h2m')."
  (if (or (null millis) (not (numberp millis)) (<= millis 0))
      "—"
    (let* ((secs (truncate (/ millis 1000.0)))
           (h (/ secs 3600))
           (m (% (/ secs 60) 60))
           (s (% secs 60)))
      (cond ((> h 0) (format "%dh%dm" h m))
            ((> m 0) (format "%dm%02ds" m s))
            (t       (format "%ds" s))))))

(defun beads-agent-ralph-dashboard--iter-cell (text width face issue-id)
  "Render one TEXT cell padded to WIDTH chars with FACE.
ISSUE-ID is attached as the `:beads-ralph-iter-issue-id' text property
so `beads-agent-ralph-dashboard-row-activate' can route RET on the row
to the iteration's bd issue."
  (vui-text (format (format "%%-%ds" width) text)
            :face face
            :beads-ralph-iter-issue-id issue-id))

(defun beads-agent-ralph-dashboard--iter-row (iter idx)
  "Return one row vnode for ITER (a `--iteration') at IDX.
The row is a `vui-hstack' of fixed-width cells (glyph, index,
issue-id, status, cost, wall-time, tools, summary).  Every cell
carries the iteration's issue-id as a text property so a single
`RET' anywhere on the row activates the right target."
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
         (status-face (pcase status
                        ('finished 'success)
                        ('failed   'error)
                        ('stopped  'warning)
                        (_         'shadow)))
         (cost (if (oref iter cost-usd)
                   (format "$%.2f" (oref iter cost-usd))
                 "$—"))
         (wall (beads-agent-ralph-dashboard--format-duration
                (oref iter duration-ms)))
         (tools (or (oref iter tool-call-count) 0))
         (summary (or (oref iter summary) ""))
         (issue-id (or (oref iter issue-id) "?"))
         (summary-cell (if (> (length summary) 60)
                           (concat (substring summary 0 60) "…")
                         summary)))
    (vui-hstack
     :spacing 1
     (beads-agent-ralph-dashboard--iter-cell
      (format "  %s#%d" glyph idx) 6 nil issue-id)
     (beads-agent-ralph-dashboard--iter-cell issue-id 12 nil issue-id)
     (beads-agent-ralph-dashboard--iter-cell status-mark 10 status-face issue-id)
     (beads-agent-ralph-dashboard--iter-cell cost 8 nil issue-id)
     (beads-agent-ralph-dashboard--iter-cell wall 7 nil issue-id)
     (beads-agent-ralph-dashboard--iter-cell (format "%dt" tools) 5 nil issue-id)
     (beads-agent-ralph-dashboard--iter-cell summary-cell 60 'shadow issue-id))))

(defun beads-agent-ralph-dashboard--live-iter-row (controller)
  "Return a placeholder row for the in-flight iteration, or nil.
Shown while CONTROLLER's `current-stream' is bound — i.e. an iteration
is mid-spawn — so the table is never empty during a live run.  Counts
tool_use events seen so far and shows wall-clock since the stream
started."
  (let ((stream (oref controller current-stream))
        (status (oref controller status)))
    (when (and stream
               (memq status '(running idle))
               (slot-boundp stream 'events))
      (let* ((events (oref stream events))
             ;; Count tool_use BLOCKS across deduped assistant events so
             ;; per-block partials (which the renderer drops) don't inflate
             ;; the live-row tool count beyond what the user actually sees.
             (tools (cl-loop
                     for e in (beads-agent-ralph-dashboard--dedupe-assistant-events
                               (reverse events))
                     sum (cl-count-if
                          (lambda (b)
                            (string= "tool_use"
                                     (beads-agent-ralph-dashboard--block-type
                                      b)))
                          (beads-agent-ralph-dashboard--assistant-blocks e))))
             (started (and (slot-boundp stream 'started-at)
                           (oref stream started-at)))
             (wall (beads-agent-ralph-dashboard--format-elapsed started))
             (idx (oref controller iteration))
             (issue (or (oref controller current-issue-id)
                        (oref controller root-id) "?")))
        (vui-hstack
         :spacing 1
         (beads-agent-ralph-dashboard--iter-cell
          (format "  ▸#%d" idx) 6 'shadow issue)
         (beads-agent-ralph-dashboard--iter-cell issue 12 'shadow issue)
         (beads-agent-ralph-dashboard--iter-cell "▶ live" 10 'shadow issue)
         (beads-agent-ralph-dashboard--iter-cell "$—" 8 'shadow issue)
         (beads-agent-ralph-dashboard--iter-cell wall 7 'shadow issue)
         (beads-agent-ralph-dashboard--iter-cell (format "%dt" tools) 5 'shadow issue)
         (beads-agent-ralph-dashboard--iter-cell "(in-flight)" 60 'shadow issue))))))

(defun beads-agent-ralph-dashboard--iter-table (controller)
  "Return the iterations table for CONTROLLER, newest-first.
Synthesises a placeholder live row when an iteration is in-flight so
the table is never empty during a run."
  (let* ((history (oref controller history))
         (len (length history))
         (rows (cl-loop for iter in history
                        for i downfrom len
                        collect (beads-agent-ralph-dashboard--iter-row
                                 iter i)))
         (live (beads-agent-ralph-dashboard--live-iter-row controller))
         (all-rows (delq nil (cons live rows))))
    (apply #'vui-vstack
           (or all-rows
               (list (vui-text "  (none yet)" :face 'shadow))))))

;;; Live stream region

(defun beads-agent-ralph-dashboard--live-stream-events (controller)
  "Return CONTROLLER's current stream events in receive order, or nil."
  (let ((stream (oref controller current-stream)))
    (when (and stream (slot-boundp stream 'events))
      (reverse (oref stream events)))))

(defun beads-agent-ralph-dashboard--assistant-message-id (event)
  "Return the message id of an `assistant'/`user' EVENT, or nil."
  (let ((msg (or (plist-get event :message)
                 (cdr (assoc "message" event)))))
    (and (listp msg)
         (or (plist-get msg :id) (cdr (assoc "id" msg))))))

(defun beads-agent-ralph-dashboard--dedupe-assistant-events (events)
  "Return EVENTS with redundant partial assistant events removed.
With `--include-partial-messages', Claude's SDK emits one assistant
event per content block as it streams in (each carrying the same
`:message :id' but only a single block in `:content'), and our stream
parser appends one synthesised assistant on message_stop that
combines every block.  Rendering all of them shows each tool_use
and thinking block twice or more.

Policy: when any synthesised event exists for a given message id,
keep only that synth event (it has the full block list); otherwise
keep the first real assistant for the id and drop later ones."
  (let ((has-synth (make-hash-table :test 'equal))
        (seen-id   (make-hash-table :test 'equal)))
    (dolist (event events)
      (when (and (beads-agent-ralph-dashboard--type-eq event "assistant")
                 (plist-get event :__synthesized-from-partials))
        (when-let ((id (beads-agent-ralph-dashboard--assistant-message-id
                        event)))
          (puthash id t has-synth))))
    (cl-loop for event in events
             for is-asst = (beads-agent-ralph-dashboard--type-eq
                            event "assistant")
             for id = (and is-asst
                           (beads-agent-ralph-dashboard--assistant-message-id
                            event))
             for synth = (and is-asst
                              (plist-get event :__synthesized-from-partials))
             for keep = (cond
                         ((not is-asst) t)
                         ((null id) t)
                         ((gethash id has-synth) synth)
                         ((gethash id seen-id) nil)
                         (t (puthash id t seen-id) t))
             when keep collect event)))

(defun beads-agent-ralph-dashboard--terminal-panel (controller)
  "Return the terminated-loop panel vnode for CONTROLLER.
Replaces the live-stream feed once the controller is in a terminal
status (`done', `stopped', `failed').  Surfaces the `done-reason'
crumb so the user understands why the stream is empty, and offers a
button that opens the last iteration's bd issue."
  (let* ((reason (or (oref controller done-reason)
                     (oref controller status)))
         (last-id (or (oref controller current-issue-id)
                      (oref controller root-id))))
    (vui-vstack
     (vui-text (format "  Loop terminated: %s" reason) :face 'bold)
     (vui-hstack
      :spacing 1
      (vui-text "  ")
      (vui-button "[v]iew last issue"
        :no-decoration t
        :on-click #'beads-agent-ralph-dashboard-view-issue
        :help-echo (and last-id (format "Open %s in beads-show" last-id)))
      (vui-button "[B]anners"
        :no-decoration t
        :on-click #'beads-agent-ralph-dashboard-banner-log
        :help-echo "Open the banner history buffer")))))

(defun beads-agent-ralph-dashboard--live-stream (controller)
  "Return the live stream region vnode for CONTROLLER.
When the controller is in a terminal status, shows a terminated panel
instead of the misleading `waiting for first event' placeholder."
  (if (beads-agent-ralph--terminal-p controller)
      (beads-agent-ralph-dashboard--terminal-panel controller)
    (let* ((events (beads-agent-ralph-dashboard--live-stream-events controller))
           (events (beads-agent-ralph-dashboard--dedupe-assistant-events events))
           (rendered (cl-loop for event in events
                              for vnode = (beads-agent-ralph-dashboard--render-event
                                           event)
                              when vnode collect vnode)))
      (apply #'vui-vstack
             (or rendered
                 (list (vui-text "  (waiting for first event)"
                                 :face 'shadow)))))))

;;; Section divider

(defun beads-agent-ralph-dashboard--divider (label)
  "Return a `── LABEL ───' divider vnode roughly 78 chars wide."
  (let* ((prefix (format "── %s " label))
         (fill (max 0 (- 78 (string-width prefix)))))
    (vui-text (concat prefix (make-string fill ?─))
              :face 'shadow)))

;;; Action bar

(defun beads-agent-ralph-dashboard--action-bar (controller)
  "Return the action-bar vnode for CONTROLLER.
Each action is a `vui-button' so it is mouse-clickable and reflects
disabled state directly (stop/pause grey out once the loop is
terminal; kill greys out when there is no in-flight stream; resume
greys out unless the loop is recoverable).  The button labels keep
the bracketed-shortcut letter so they line up with the keymap in
`beads-agent-ralph-dashboard-mode-map' and the body of
`beads-agent-ralph-dashboard-help'."
  (let* ((status (oref controller status))
         (terminal-p (memq status beads-agent-ralph--terminal-statuses))
         (resumable-p (memq status '(auto-paused stopped)))
         (in-flight-p (and (oref controller current-stream)
                           (not terminal-p))))
    (vui-hstack
     :spacing 1
     (vui-text "  ")
     (vui-button "[s]top"
       :no-decoration t
       :disabled terminal-p
       :on-click #'beads-agent-ralph-dashboard-stop)
     (vui-button "[k]ill"
       :no-decoration t
       :disabled (not in-flight-p)
       :on-click #'beads-agent-ralph-dashboard-kill-iter)
     (vui-button "[p]ause"
       :no-decoration t
       :disabled terminal-p
       :on-click #'beads-agent-ralph-dashboard-pause)
     (vui-button "[r]esume"
       :no-decoration t
       :disabled (not resumable-p)
       :on-click #'beads-agent-ralph-dashboard-resume)
     (vui-button "[v]iew"
       :no-decoration t
       :on-click #'beads-agent-ralph-dashboard-view-issue)
     (vui-button "[P]rompt"
       :no-decoration t
       :on-click #'beads-agent-ralph-dashboard-view-prompt)
     (vui-button "[B]anners"
       :no-decoration t
       :on-click #'beads-agent-ralph-dashboard-banner-log)
     (vui-button "[g]refresh"
       :no-decoration t
       :on-click #'beads-agent-ralph-dashboard-refresh)
     (vui-button "[q]uit"
       :no-decoration t
       :on-click (lambda () (quit-window)))
     (vui-button "[?]"
       :no-decoration t
       :on-click #'beads-agent-ralph-dashboard-help))))

;;; Root composition

(vui-defcomponent beads-agent-ralph-dashboard--root
    (controller)
  "Top-level Ralph dashboard composition.
CONTROLLER is the live `beads-agent-ralph--controller' object.
The dense header line is pinned via `header-line-format' (see
`beads-agent-ralph-dashboard-mode'); the body keeps the secondary
detail line, banner, iteration table, live stream, and action bar
separated by labelled rulers so scrolled views stay oriented."
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
      (vui-text (beads-agent-ralph-dashboard--secondary-line controller)
                :face 'shadow)
      (or (beads-agent-ralph-dashboard--banner-line controller)
          (vui-text ""))
      (beads-agent-ralph-dashboard--divider "Iterations")
      (beads-agent-ralph-dashboard--iter-table controller)
      (beads-agent-ralph-dashboard--divider "Live stream")
      (beads-agent-ralph-dashboard--live-stream controller)
      (beads-agent-ralph-dashboard--divider "Actions")
      (beads-agent-ralph-dashboard--action-bar controller)))))

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
    (define-key map (kbd "P") #'beads-agent-ralph-dashboard-view-prompt)
    (define-key map (kbd "B") #'beads-agent-ralph-dashboard-banner-log)
    (define-key map (kbd "g") #'beads-agent-ralph-dashboard-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'beads-agent-ralph-dashboard-help)
    (define-key map (kbd "RET") #'beads-agent-ralph-dashboard-row-activate)
    map)
  "Keymap for `beads-agent-ralph-dashboard-mode'.")

(define-derived-mode beads-agent-ralph-dashboard-mode vui-mode
  "Ralph Dashboard"
  "Major mode for the Ralph loop dashboard.
Derived from `vui-mode' so `vui-mount' preserves our keymap and
text-property contract; otherwise vui would switch the buffer back
to `vui-mode' on every re-render.
Keybindings reflect the action bar legend at the foot of the buffer.
The dense status line is pinned via `header-line-format' so it stays
visible when the live stream scrolls off-screen."
  (setq-local
   header-line-format
   '(:eval
     (when (and (boundp 'beads-agent-ralph-dashboard--controller)
                beads-agent-ralph-dashboard--controller)
       (beads-agent-ralph-dashboard--header-line
        beads-agent-ralph-dashboard--controller)))))

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
  "Abort the in-flight iteration but keep the loop alive.
Delegates to `beads-agent-ralph-kill-iter', which signals the current
stream the same way `stop' does but does NOT set the controller's
`done-reason'.  When the sentinel fires, `on-stream-finish' detects
the user-killed latch, skips stall/lying detection for the iter, and
schedules the next iteration via the normal continue path.
Distinct from `[s]top', which terminates the whole loop."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (beads-agent-ralph-kill-iter beads-agent-ralph-dashboard--controller)
    (beads-agent-ralph-dashboard-refresh)))

(defun beads-agent-ralph-dashboard-pause ()
  "Pause the loop after the in-flight iteration completes."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (beads-agent-ralph--pause
     beads-agent-ralph-dashboard--controller
     "Paused by user")
    (beads-agent-ralph-dashboard-refresh)))

(defun beads-agent-ralph-dashboard-resume ()
  "Resume from `auto-paused' / `paused' / `stopped' status.
Clears any terminal `done-reason' crumb left over from a prior
`beads-agent-ralph-stop'; otherwise the first stream-finish after
resume would hit the `(eq done-reason \\='stop)' branch in
`--on-stream-finish' and terminate the loop again after a single
iteration (bde-7943)."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (let ((c beads-agent-ralph-dashboard--controller))
      (when (memq (oref c status) '(auto-paused stopped))
        (oset c done-reason nil)
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

(defun beads-agent-ralph-dashboard--issue-id-at-point ()
  "Return the iteration issue-id text-property somewhere on this line.
Each iteration row cell carries `:beads-ralph-iter-issue-id'; the
space gutter between cells does not, so scan the whole line."
  (let ((p (line-beginning-position))
        (eol (line-end-position))
        (id nil))
    (while (and (not id) (< p eol))
      (setq id (get-text-property p :beads-ralph-iter-issue-id))
      (setq p (or (next-single-property-change
                   p :beads-ralph-iter-issue-id nil eol)
                  eol)))
    id))

(defun beads-agent-ralph-dashboard-row-activate ()
  "Open the bd issue for the iteration row at point.
Reads `:beads-ralph-iter-issue-id' from the line and routes to
`beads-show'.  Off a row this falls back to `view-issue', which
opens the controller's current issue."
  (interactive)
  (let ((id (beads-agent-ralph-dashboard--issue-id-at-point)))
    (cond
     ((and id (fboundp 'beads-show))
      (funcall 'beads-show id))
     (t
      (beads-agent-ralph-dashboard-view-issue)))))

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

(defun beads-agent-ralph-dashboard-view-prompt ()
  "Pop a buffer showing the resolved iteration prompt template.
Resolves the controller's effective template the same way the spawn
path does (slot → `beads-agent-ralph-prompt-file' → defcustom) so the
user sees the exact source that drives each iteration's prompt.  Not
the rendered prompt for one specific iteration — that view would
require re-fetching bd state and is the kind of thing a future
`P i' (per-iter prompt) command should grow into."
  (interactive)
  (when beads-agent-ralph-dashboard--controller
    (let* ((c beads-agent-ralph-dashboard--controller)
           (template (or (and (fboundp 'beads-agent-ralph--effective-template)
                              (beads-agent-ralph--effective-template c))
                         (oref c prompt-template)
                         "(no template resolved)"))
           (buf (get-buffer-create
                 (format "*beads-agent-ralph prompt: %s*"
                         (oref c root-id)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Iteration prompt template for %s\n"
                          (oref c root-id)))
          (insert (format "Source: %s\n\n"
                          (cond ((oref c prompt-template) "controller slot")
                                (beads-agent-ralph-prompt-file
                                 (format "file %s"
                                         beads-agent-ralph-prompt-file))
                                (t "defcustom `beads-agent-ralph-prompt'"))))
          (insert template))
        (goto-char (point-min))
        (setq-local buffer-read-only t))
      (pop-to-buffer buf))))

(defun beads-agent-ralph-dashboard-help ()
  "Show the full key legend in the echo area.
Keep the message in sync with the action-bar legend rendered by
`beads-agent-ralph-dashboard--action-bar' so the user can press `?'
and see the same set of keys advertised at the bottom of the buffer."
  (interactive)
  (message
   "Ralph keys: [s]top [k]ill [p]ause [r]esume [v]iew issue [P]rompt [B]anners [g]refresh [?]help [q]uit"))

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
  3. Unsubscribe the controller and persistence subscribers from the
     stream so a late filter call (between detach and delete) does not
     fire their callbacks against a half-torn-down loop.

This is intentionally idempotent.  Note that detaching alone leaves
the controller pointed at a dead stream with `status' still
`running'; the caller (`--kill-buffer-cleanup') is responsible for
driving the controller to a terminal state afterwards (see
`beads-agent-ralph--terminate')."
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
Behaviour splits on controller status (bde-deqx.3):

- Terminal (`done' / `stopped' / `failed'): the user closing a
  finished loop's dashboard is the natural signal to evict.  Cancel
  the controller's pending eviction timer (scheduled by `--terminate'
  on the way into the terminal state) and unregister immediately, so
  the registry-retention timeout becomes the worst-case fallback, not
  the common path.

- Live (`running' / `cooling-down' / `auto-paused' / `idle'): leave
  the controller alive in the registry and let the loop continue
  headless.  The user can re-mount the dashboard later through the
  cockpit (`bde-deqx.4').  This is the behavioural change in
  bde-deqx.3 -- previously `kill-buffer' drove the loop to terminal,
  which orphaned a money-spending claude process whenever the user
  merely wanted to reclaim screen real-estate.  Stopping a loop is
  done through `[s]' on the dashboard, not by killing the buffer.

Pending re-render timers are cancelled in both branches: the buffer
is going away and there is nothing left to render into."
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
      (when (beads-agent-ralph--terminal-p controller)
        (beads-agent-ralph--cancel-eviction-timer controller)
        (beads-agent-ralph--unregister-controller controller)))))

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
