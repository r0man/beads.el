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
  "The rendered buffer contains the header's iteration count."
  (let* ((c (beads-agent-ralph-dashboard-test--make-controller
             :root-id "bde-h"
             :iteration 7
             :max-iterations 20))
         (buf (beads-agent-ralph-dashboard-render c)))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "iter 7/20" (buffer-string))))
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

(ert-deftest beads-agent-ralph-dashboard-test-kill-detaches-running-stream ()
  "Killing the buffer mid-run detaches the stream's IO."
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
    (should subscriber-removed)
    (should (eq (oref stream status) 'stopped))))

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
