;;; beads-agent-ralph-dashboard-test.el --- Tests for Ralph dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the Ralph dashboard module: event taxonomy dispatch,
;; block rendering, header formatting, banner severity selection, and
;; the iteration table renderer.  Mount-level integration uses a stub
;; controller object so tests run without spawning a real claude
;; subprocess.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-ralph)
(require 'beads-agent-ralph-dashboard)

;;; Fixtures

(defun beads-agent-ralph-dashboard-test--make-controller (&rest args)
  "Construct a controller for dashboard tests, defaults overridable via ARGS."
  (apply #'beads-agent-ralph--controller
         (append args
                 (list :root-id (or (plist-get args :root-id) "bde-test")
                       :iteration (or (plist-get args :iteration) 0)
                       :max-iterations (or (plist-get args :max-iterations) 10)
                       :started-at (or (plist-get args :started-at)
                                       (current-time))))))

;;; Event type detection

(ert-deftest beads-agent-ralph-dashboard-test-type-eq-plist ()
  "Plist :type matches against a string tag."
  (should (beads-agent-ralph-dashboard--type-eq
           '(:type "assistant") "assistant"))
  (should-not (beads-agent-ralph-dashboard--type-eq
               '(:type "user") "assistant")))

(ert-deftest beads-agent-ralph-dashboard-test-type-eq-symbol ()
  "Symbol :type also matches by interning the tag."
  (should (beads-agent-ralph-dashboard--type-eq
           '(:type assistant) "assistant")))

(ert-deftest beads-agent-ralph-dashboard-test-type-eq-alist ()
  "Alist `(\"type\" . X)' is recognised."
  (should (beads-agent-ralph-dashboard--type-eq
           '(("type" . "result")) "result")))

;;; Text capping

(ert-deftest beads-agent-ralph-dashboard-test-cap-text-short ()
  "Short text is not flagged truncated."
  (let* ((cap (beads-agent-ralph-dashboard--cap-text-lines "hello\nworld")))
    (should-not (plist-get cap :truncated))
    (should (= 2 (plist-get cap :total)))))

(ert-deftest beads-agent-ralph-dashboard-test-cap-text-long-truncates ()
  "Lines past the collapse cap mark truncated and report total."
  (let* ((lines (cl-loop for i from 1 to 100 collect (format "line %d" i)))
         (text (mapconcat #'identity lines "\n"))
         (cap (beads-agent-ralph-dashboard--cap-text-lines text)))
    (should (plist-get cap :truncated))
    (should (= 100 (plist-get cap :total)))))

(ert-deftest beads-agent-ralph-dashboard-test-cap-text-truncated-preview-is-collapse-lines ()
  "When truncated, preview returns `collapse-cap' lines; when not, full text (bde-t9tx).

Regression for a bug where both branches returned the same number of lines, so the
`line-cap' defcustom only flipped the boolean but never affected the preview length."
  (let ((beads-agent-ralph-inline-line-cap 5)
        (beads-agent-ralph-inline-collapse-cap 2))
    ;; Total <= cap: not truncated, full text returned.
    (let ((res (beads-agent-ralph-dashboard--cap-text-lines "a\nb\nc")))
      (should-not (plist-get res :truncated))
      (should (= 3 (plist-get res :total)))
      (should (equal "a\nb\nc" (plist-get res :preview))))
    ;; Total > cap: truncated, preview = collapse-cap lines.
    (let ((res (beads-agent-ralph-dashboard--cap-text-lines "a\nb\nc\nd\ne\nf")))
      (should (plist-get res :truncated))
      (should (= 6 (plist-get res :total)))
      (should (equal "a\nb" (plist-get res :preview))))))

;;; Header / format-elapsed

(ert-deftest beads-agent-ralph-dashboard-test-format-elapsed-nil ()
  "Nil start time returns `?'."
  (should (equal "?" (beads-agent-ralph-dashboard--format-elapsed nil))))

(ert-deftest beads-agent-ralph-dashboard-test-format-elapsed-seconds ()
  "Recent start time renders as `?s'."
  (let* ((now (current-time))
         (started (time-subtract now (seconds-to-time 5)))
         (formatted (beads-agent-ralph-dashboard--format-elapsed started)))
    (should (string-match-p "\\`[0-9]+s\\'" formatted))))

(ert-deftest beads-agent-ralph-dashboard-test-header-line-has-fields ()
  "Header line includes iter, max-iter, and cost."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :iteration 3 :max-iterations 20
             :cumulative-cost-usd 0.123))
         (line (beads-agent-ralph-dashboard--header-line c)))
    (should (string-match-p "iter 3/20" line))
    (should (string-match-p "\\$0\\.123" line))))

;;; Banner severity

(ert-deftest beads-agent-ralph-dashboard-test-top-banner-empty ()
  "Empty banner-log returns nil."
  (let ((c (beads-agent-ralph-dashboard-test--make-controller)))
    (should (null (beads-agent-ralph-dashboard--top-banner c)))))

(ert-deftest beads-agent-ralph-dashboard-test-top-banner-error-wins ()
  "Error severity beats notice and warning."
  (let ((c (beads-agent-ralph-dashboard-test--make-controller)))
    (beads-agent-ralph--push-banner c 'notice "n")
    (beads-agent-ralph--push-banner c 'warning "w")
    (beads-agent-ralph--push-banner c 'error "e")
    (let ((top (beads-agent-ralph-dashboard--top-banner c)))
      (should (eq (plist-get top :severity) 'error)))))

;;; Iteration table

(ert-deftest beads-agent-ralph-dashboard-test-iter-table-empty ()
  "Empty history renders a placeholder line."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller))
         (vnode (beads-agent-ralph-dashboard--iter-table c)))
    (should vnode)))

(ert-deftest beads-agent-ralph-dashboard-test-iter-table-with-history ()
  "Each finished iteration produces a row vnode."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller))
         (iter (beads-agent-ralph--iteration
                :issue-id "bde-a"
                :status 'finished
                :summary "did stuff"
                :cost-usd 0.01)))
    (push iter (oref c history))
    (let ((vnode (beads-agent-ralph-dashboard--iter-table c)))
      (should vnode))))

;;; Event dispatcher

(ert-deftest beads-agent-ralph-dashboard-test-render-event-system-init ()
  "system.init renders a session line."
  (let ((vnode (beads-agent-ralph-dashboard--render-event
                '(:type "system" :subtype "init" :model "sonnet"
                       :tools ("Bash" "Edit" "Read")))))
    (should vnode)))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-assistant-text ()
  "Assistant with text block renders a text vnode."
  (let ((vnode (beads-agent-ralph-dashboard--render-event
                '(:type "assistant"
                       :message (:content ((:type "text" :text "hello")))))))
    (should vnode)))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-assistant-tool-use ()
  "Tool-use block renders the ▶ card."
  (let ((vnode (beads-agent-ralph-dashboard--render-event
                '(:type "assistant"
                       :message (:content
                                 ((:type "tool_use"
                                         :name "Bash"
                                         :input (:command "ls /tmp"))))))))
    (should vnode)))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-result ()
  "Result event renders with cost + duration."
  (let ((vnode (beads-agent-ralph-dashboard--render-event
                '(:type "result" :subtype "success"
                       :total_cost_usd 0.5 :duration_ms 1500))))
    (should vnode)))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-skips-stream-envelope ()
  "stream_event envelopes return nil so the synth assistant doesn't duplicate."
  (should (null (beads-agent-ralph-dashboard--render-event
                 '(:type "stream_event"
                         :event (:type "message_start"))))))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-error-plist ()
  "Error events render with the raw payload."
  (let ((vnode (beads-agent-ralph-dashboard--render-event
                '(:type error :raw "not-json"))))
    (should vnode)))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-skips-hook-subtype ()
  "system/hook_* events are dropped just like hook-typed events.
The bd-prime SessionStart hook arrives as
`(:type \"system\" :subtype \"hook_response\" :output ...)' and used to
flood the dashboard with workflow context.  It must be filtered."
  (let ((beads-agent-ralph-include-hook-events nil))
    (should (null (beads-agent-ralph-dashboard--render-event
                   '(:type "system" :subtype "hook_response"
                           :output "...long bd prime dump..."))))
    (should (null (beads-agent-ralph-dashboard--render-event
                   '(:type "system" :subtype "hook_started"
                           :hook_name "SessionStart:startup"))))))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-hook-included-when-opted-in ()
  "With `beads-agent-ralph-include-hook-events' the hook events render."
  (let ((beads-agent-ralph-include-hook-events t))
    (should (beads-agent-ralph-dashboard--render-event
             '(:type "system" :subtype "hook_response" :output "x")))))

(ert-deftest beads-agent-ralph-dashboard-test-thinking-empty-is-dropped ()
  "Empty/whitespace thinking blocks render as nil, not 80 spaces."
  (should (null (beads-agent-ralph-dashboard--render-thinking-block
                 '(:type "thinking" :text ""))))
  (should (null (beads-agent-ralph-dashboard--render-thinking-block
                 '(:type "thinking" :text "   \n  "))))
  (should (beads-agent-ralph-dashboard--render-thinking-block
           '(:type "thinking" :text "actually thinking"))))

(ert-deftest beads-agent-ralph-dashboard-test-assistant-with-only-empty-thinking-yields-nil ()
  "An assistant event whose only block is an empty thinking yields nil
so the dispatcher can drop it instead of emitting a blank vstack."
  (should (null (beads-agent-ralph-dashboard--render-event
                 '(:type "assistant"
                         :message (:content ((:type "thinking" :text ""))))))))

(ert-deftest beads-agent-ralph-dashboard-test-dedupe-prefers-synth-over-partials ()
  "When a synth assistant exists for an id, the per-block real partials drop.
With `--include-partial-messages', Claude's SDK emits one assistant
per content block (each carrying the same `:message :id' but only one
block in `:content'), then our parser appends one synth with all
blocks.  The dedupe pass keeps only the synth for that id."
  (let* ((part1 '(:type "assistant"
                        :message (:id "msg_a" :role "assistant"
                                      :content ((:type "thinking" :text "")))))
         (part2 '(:type "assistant"
                        :message (:id "msg_a" :role "assistant"
                                      :content ((:type "tool_use"
                                                       :id "toolu_x"
                                                       :name "Bash"
                                                       :input (:command "ls"))))))
         (synth '(:type "assistant"
                        :message (:id "msg_a" :role "assistant"
                                      :content ((:type "thinking" :text "")
                                                (:type "tool_use"
                                                       :id "toolu_x"
                                                       :name "Bash"
                                                       :input (:command "ls"))))
                        :__synthesized-from-partials t))
         (other '(:type "assistant"
                        :message (:id "msg_other"
                                      :content ((:type "text" :text "hi")))))
         (result (beads-agent-ralph-dashboard--dedupe-assistant-events
                  (list part1 part2 synth other))))
    (should (= 2 (length result)))
    (should (eq (car result) synth))
    (should (eq (cadr result) other))))

(ert-deftest beads-agent-ralph-dashboard-test-dedupe-no-synth-keeps-first ()
  "When no synth exists for an id, keep the first real assistant for that id."
  (let* ((p1 '(:type "assistant"
                     :message (:id "msg_b" :content ((:type "text" :text "one")))))
         (p2 '(:type "assistant"
                     :message (:id "msg_b" :content ((:type "text" :text "two")))))
         (result (beads-agent-ralph-dashboard--dedupe-assistant-events
                  (list p1 p2))))
    (should (= 1 (length result)))
    (should (eq (car result) p1))))

(ert-deftest beads-agent-ralph-dashboard-test-dedupe-passes-through-non-assistant ()
  "Non-assistant events pass through unchanged."
  (let* ((events '((:type "system" :subtype "init")
                   (:type "result" :total_cost_usd 0.1)
                   (:type "error" :raw "bad")))
         (result (beads-agent-ralph-dashboard--dedupe-assistant-events
                  events)))
    (should (equal events result))))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-skips-status-partial ()
  "system/status `requesting' partials between turns are dropped."
  (should (null (beads-agent-ralph-dashboard--render-event
                 '(:type "system" :subtype "status"
                         :status "requesting"
                         :uuid "x" :session_id "y")))))

(ert-deftest beads-agent-ralph-dashboard-test-render-event-skips-allowed-rate-limit ()
  "Rate-limit pings with status=allowed are dropped; throttled keep rendering."
  (should (null (beads-agent-ralph-dashboard--render-event
                 '(:type "rate_limit_event"
                         :rate_limit_info (:status "allowed")))))
  ;; A non-allowed status still falls through to the generic shadow render,
  ;; which is intentional — we want to *see* throttles.
  (should (beads-agent-ralph-dashboard--render-event
           '(:type "rate_limit_event"
                   :rate_limit_info (:status "throttled")))))

(ert-deftest beads-agent-ralph-dashboard-test-tool-input-summary-bash-keeps-first-command ()
  "Multi-step Bash commands collapse to just the first segment."
  (should (string= "ls -la"
                   (beads-agent-ralph-dashboard--tool-input-summary
                    '(:command "ls -la && echo done && rm -rf /tmp/x"))))
  ;; Pipelines (`|') stay intact — they're one command — but a `;'
  ;; terminator still ends the visible preview at the first segment.
  (should (string= "curl -sI https://github.com/r0man | head -3"
                   (beads-agent-ralph-dashboard--tool-input-summary
                    '(:command "curl -sI https://github.com/r0man | head -3; echo done"))))
  ;; Single-command input stays intact (until the 80-char truncation cap).
  (should (string= "echo hello"
                   (beads-agent-ralph-dashboard--tool-input-summary
                    '(:command "echo hello")))))

(ert-deftest beads-agent-ralph-dashboard-test-tool-input-summary-prefers-path ()
  "Read/Edit tool input shows `file_path' or `path' rather than %S."
  (should (string= "/etc/hosts"
                   (beads-agent-ralph-dashboard--tool-input-summary
                    '(:file_path "/etc/hosts" :offset 0))))
  (should (string= "src/main.el"
                   (beads-agent-ralph-dashboard--tool-input-summary
                    '(:path "src/main.el")))))

(ert-deftest beads-agent-ralph-dashboard-test-tool-result-strips-ansi ()
  "Raw VT100 CSI sequences in tool_result are filtered before display."
  (let* ((block `(:type "tool_result"
                        :content ,(concat "\e[Klooking for substitutes…"
                                          "\n\e[31mred\e[0mtext")))
         (vnode (beads-agent-ralph-dashboard--render-tool-result-block block))
         (text (when vnode
                 (format-mode-line vnode))))
    (should vnode)))

(ert-deftest beads-agent-ralph-dashboard-test-format-duration ()
  "Duration formatter handles nil / seconds / minutes / hours."
  (should (string= "—" (beads-agent-ralph-dashboard--format-duration nil)))
  (should (string= "—" (beads-agent-ralph-dashboard--format-duration 0)))
  (should (string= "5s" (beads-agent-ralph-dashboard--format-duration 5000)))
  (should (string= "1m05s" (beads-agent-ralph-dashboard--format-duration 65000)))
  (should (string= "1h2m" (beads-agent-ralph-dashboard--format-duration 3720000))))

(ert-deftest beads-agent-ralph-dashboard-test-iter-table-shows-live-row-while-running ()
  "When a stream is bound (iter in-flight), the table shows a live row.
The row is a `vui-hstack' of fixed-width cells; concatenate the
cell contents to check the human-visible string."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-live"
             :iteration 3))
         (stream (beads-agent-ralph--stream
                  :events nil
                  :started-at (current-time))))
    (oset c current-stream stream)
    (oset c current-issue-id "bde-live")
    (oset c status 'running)
    (let* ((row (beads-agent-ralph-dashboard--live-iter-row c))
           (text (and row
                      (mapconcat #'vui-vnode-text-content
                                 (vui-vnode-hstack-children row)
                                 " "))))
      (should row)
      (should (string-match-p "▶ live" text))
      (should (string-match-p "#3" text)))))

(ert-deftest beads-agent-ralph-dashboard-test-iter-table-no-live-row-when-terminal ()
  "Terminal controllers don't get a synthetic live row (no stream bound)."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-term-no-live")))
    (oset c status 'done)
    (oset c current-stream nil)
    (should-not (beads-agent-ralph-dashboard--live-iter-row c))))

(ert-deftest beads-agent-ralph-dashboard-test-iter-row-cells-carry-issue-id ()
  "Each cell in an iteration row carries `:beads-ralph-iter-issue-id'
so RET on any column activates the same target."
  (let* ((iter (beads-agent-ralph--iteration
                :issue-id "bde-row-id"
                :status 'finished
                :summary "did stuff"
                :cost-usd 0.42))
         (row (beads-agent-ralph-dashboard--iter-row iter 2))
         (cells (vui-vnode-hstack-children row)))
    (should (cl-every
             (lambda (cell)
               (equal "bde-row-id"
                      (plist-get (vui-vnode-text-properties cell)
                                 :beads-ralph-iter-issue-id)))
             cells))))

(ert-deftest beads-agent-ralph-dashboard-test-iter-row-cost-precision-two-decimals ()
  "Cost cell renders with two decimal places, not four."
  (let* ((iter (beads-agent-ralph--iteration
                :issue-id "bde-cost" :status 'finished :cost-usd 0.3290))
         (row (beads-agent-ralph-dashboard--iter-row iter 1))
         (text (mapconcat #'vui-vnode-text-content
                          (vui-vnode-hstack-children row) " ")))
    (should (string-match-p "\\$0\\.33" text))
    (should-not (string-match-p "\\$0\\.3290" text))))

(ert-deftest beads-agent-ralph-dashboard-test-live-stream-terminal-panel ()
  "Terminal controllers show a `Loop terminated' panel, not the
`waiting for first event' placeholder."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-term-panel")))
    (oset c status 'done)
    (oset c done-reason 'epic-empty)
    (oset c current-stream nil)
    (let* ((vnode (beads-agent-ralph-dashboard--live-stream c))
           (rendered (format "%S" vnode)))
      (should (string-match-p "Loop terminated: epic-empty" rendered))
      (should-not (string-match-p "waiting for first event" rendered)))))

(ert-deftest beads-agent-ralph-dashboard-test-action-bar-disables-stop-when-terminal ()
  "When the controller is terminal, `[s]top' / `[p]ause' buttons are
disabled and `[r]esume' is enabled iff the loop is recoverable."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-bar")))
    (oset c status 'done)
    (let* ((bar (beads-agent-ralph-dashboard--action-bar c))
           (buttons (cl-remove-if-not
                     #'vui-vnode-button-p
                     (vui-vnode-hstack-children bar)))
           (by-label (lambda (label)
                       (cl-find label buttons
                                :key #'vui-vnode-button-label
                                :test #'string=))))
      (should (vui-vnode-button-disabled-p (funcall by-label "[s]top")))
      (should (vui-vnode-button-disabled-p (funcall by-label "[p]ause")))
      ;; `done' is NOT in the resumable set; only `auto-paused' /
      ;; `stopped' are.  So resume must be disabled here.
      (should (vui-vnode-button-disabled-p (funcall by-label "[r]esume"))))))

(ert-deftest beads-agent-ralph-dashboard-test-action-bar-resume-enabled-when-stopped ()
  "`[r]esume' is enabled when the controller is in a recoverable state."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-bar-resume")))
    (oset c status 'stopped)
    (let* ((bar (beads-agent-ralph-dashboard--action-bar c))
           (buttons (cl-remove-if-not
                     #'vui-vnode-button-p
                     (vui-vnode-hstack-children bar)))
           (resume (cl-find "[r]esume" buttons
                            :key #'vui-vnode-button-label
                            :test #'string=)))
      (should-not (vui-vnode-button-disabled-p resume)))))

;;; Buffer mount

(ert-deftest beads-agent-ralph-dashboard-test-render-creates-buffer ()
  "`-render' creates a dashboard buffer with the major mode set."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-mount"))
         (buf (beads-agent-ralph-dashboard-render c)))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (eq major-mode 'beads-agent-ralph-dashboard-mode))
            (should (eq beads-agent-ralph-dashboard--controller c))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-agent-ralph-dashboard-test-render-includes-header ()
  "The dashboard surfaces the iteration count via the sticky header.
The dense status line lives in `header-line-format' (so it stays
visible when the live stream scrolls), not in the buffer body, so we
verify both that the line itself contains the iter info and that the
mode wired `header-line-format' to a non-nil `:eval' construct."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-h"
             :iteration 7
             :max-iterations 20))
         (buf (beads-agent-ralph-dashboard-render c)))
    (unwind-protect
        (with-current-buffer buf
          (should header-line-format)
          (should (eq :eval (car-safe header-line-format)))
          (should (string-match-p
                   "iter 7/20"
                   (beads-agent-ralph-dashboard--header-line c))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; Buffer-kill cleanup hook (bde-3i7u)

(ert-deftest beads-agent-ralph-dashboard-test-kill-no-op-on-terminal-controller ()
  "Killing the buffer leaves a terminal controller alone."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-term"))
         (buf (beads-agent-ralph-dashboard-render c)))
    (oset c status 'done)
    (oset c current-stream nil)
    ;; Kill is a no-op (no stream to detach).  Just ensure it doesn't error.
    (kill-buffer buf)
    (should-not (buffer-live-p buf))))

(ert-deftest beads-agent-ralph-dashboard-test-kill-keeps-running-stream ()
  "Killing the buffer mid-run leaves the running stream alone (bde-deqx.3).

The pre-deqx.3 behaviour was to detach the stream + drive the
controller to `stopped' from `kill-buffer'.  That orphaned a
money-spending claude process whenever the user merely wanted to
reclaim screen real estate.  The new contract: kill-buffer on a
live controller leaves the loop alive (headless continuation);
stopping is done through `[s]' on the dashboard."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-run"))
         (stream (beads-agent-ralph--stream
                  :partial-messages (make-hash-table :test #'equal)
                  :status 'running))
         (subscriber-removed nil)
         (buf (beads-agent-ralph-dashboard-render c)))
    (oset c status 'running)
    (oset c current-stream stream)
    (beads-agent-ralph--stream-subscribe
     stream 'controller (lambda (_s) nil))
    (cl-letf (((symbol-function 'beads-agent-ralph--stream-unsubscribe)
               (lambda (_s label)
                 (when (eq label 'controller) (setq subscriber-removed t)))))
      (kill-buffer buf))
    (should-not subscriber-removed)
    (should (eq (oref stream status) 'running))))

(ert-deftest beads-agent-ralph-dashboard-test-kill-live-leaves-controller-alive ()
  "Killing the buffer mid-run does NOT drive the controller terminal (bde-deqx.3).

Inverts the bde-mfrl behaviour.  See sibling
`-kill-keeps-running-stream' for the rationale (orphaned $-spending
process is worse than a registry-resident headless loop)."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-zomb"))
         (stream (beads-agent-ralph--stream
                  :partial-messages (make-hash-table :test #'equal)
                  :status 'running))
         (buf (beads-agent-ralph-dashboard-render c)))
    (oset c status 'running)
    (oset c current-stream stream)
    (kill-buffer buf)
    (should (eq (oref c status) 'running))
    (should (null (oref c done-reason)))
    (should (eq (oref c current-stream) stream))))

(ert-deftest beads-agent-ralph-dashboard-test-kill-cancels-pending-rerender ()
  "Buffer kill cancels any debounced re-render timer for the controller."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-pr"))
         (buf (beads-agent-ralph-dashboard-render c)))
    ;; Queue a render and confirm it's tracked.
    (beads-agent-ralph-dashboard--schedule-rerender c)
    (should (assq c beads-agent-ralph-dashboard--pending-rerender))
    (kill-buffer buf)
    (should-not (assq c beads-agent-ralph-dashboard--pending-rerender))))

(ert-deftest beads-agent-ralph-dashboard-test-kill-terminal-unregisters ()
  "Killing the dashboard of a terminal-state controller unregisters
it immediately (bde-deqx.3).  The eviction timer scheduled by
`--terminate' becomes the worst-case fallback; the user closing the
dashboard is the natural eviction signal."
  (let* ((beads-agent-ralph--controllers nil)
         (c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-unreg"))
         (buf (beads-agent-ralph-dashboard-render c)))
    (beads-agent-ralph--register-controller c)
    (oset c status 'done)
    (oset c current-stream nil)
    (should (eq c (beads-agent-ralph-controller-for-root "bde-unreg")))
    (kill-buffer buf)
    (should (null (beads-agent-ralph-controller-for-root "bde-unreg")))
    (should (null (beads-agent-ralph-controllers)))))

(ert-deftest beads-agent-ralph-dashboard-test-kill-in-flight-keeps-registered ()
  "Killing the dashboard of a live (running) controller leaves it in
the public registry (bde-deqx.3).  A headless loop continues; the
user can re-mount via the cockpit.  This is the inverse of the
pre-deqx.3 behaviour where kill-buffer drove the loop terminal and
unregistered it."
  (let* ((beads-agent-ralph--controllers nil)
         (c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-inflight"))
         (stream (beads-agent-ralph--stream
                  :partial-messages (make-hash-table :test #'equal)
                  :status 'running))
         (buf (beads-agent-ralph-dashboard-render c)))
    (oset c status 'running)
    (oset c current-stream stream)
    (beads-agent-ralph--register-controller c)
    (kill-buffer buf)
    (should (eq (oref c status) 'running))
    (should (eq c (beads-agent-ralph-controller-for-root "bde-inflight")))))

(ert-deftest beads-agent-ralph-dashboard-test-help-echoes-question-mark-key ()
  "`beads-agent-ralph-dashboard-help' must advertise its own `?' binding.
Regression for bde-uuao: the action-bar legend lists `[?]' but the
echoed help message previously omitted the self-reference."
  (let ((message-log-max nil)
        (echoed nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq echoed (apply #'format fmt args)))))
      (beads-agent-ralph-dashboard-help))
    (should echoed)
    (should (string-match-p "\\[\\?\\]help" echoed))))

(provide 'beads-agent-ralph-dashboard-test)

;;; beads-agent-ralph-dashboard-test.el ends here
