;;; beads-agent-ralph-stream-test.el --- Tests for beads-agent-ralph-stream -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the Ralph stream module.  This file is the shared
;; home for stream parser fixtures (bde-48i7) and partial-message
;; reassembly tests (bde-9b18).  Tests use cl-letf to rebind the
;; public `beads-agent-ralph--stream-spawn' seam where they need a
;; live stream object without spawning a real claude binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-ralph-stream)

;;; Fixtures

(defun beads-agent-ralph-stream-test--make-stream ()
  "Return a minimal stream object suitable for filter/parse tests.
Does not call `make-process'; tests drive the filter and parser
helpers directly."
  (beads-agent-ralph--stream
   :partial-messages (make-hash-table :test #'equal)
   :status 'starting))

(defun beads-agent-ralph-stream-test--feed (stream lines)
  "Feed each NDJSON LINE through the parser on STREAM.
LINES is a list of single-line strings (no trailing newline)."
  (dolist (line lines)
    (beads-agent-ralph--stream-parse-line stream line)))

;;; Partial-message reassembly (bde-9b18)

(ert-deftest beads-agent-ralph-stream-test-partial-text-reassembly ()
  "Text deltas across content_block_delta events accumulate into one block."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\",\"role\":\"assistant\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\", world\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":0}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}"))
    ;; A synthesized assistant event should be present in events.
    (let* ((events (oref stream events))
           (synth (cl-find-if
                   (lambda (ev) (plist-get ev :__synthesized-from-partials))
                   events)))
      (should synth)
      (let* ((message (plist-get synth :message))
             (content (plist-get message :content))
             (first-block (car content)))
        (should (equal (plist-get message :id) "msg_1"))
        (should (or (equal (plist-get first-block :type) "text")
                    (eq (plist-get first-block :type) 'text)))
        (should (equal (plist-get first-block :text) "Hello, world"))))))

(ert-deftest beads-agent-ralph-stream-test-partial-thinking-reassembly ()
  "Thinking deltas accumulate into a thinking block via partial reassembly."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"msg_t\",\"role\":\"assistant\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"text\":\"\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"reasoning \"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"continued\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":0}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}"))
    (let* ((synth (cl-find-if
                   (lambda (ev) (plist-get ev :__synthesized-from-partials))
                   (oref stream events)))
           (block (car (plist-get (plist-get synth :message) :content))))
      (should synth)
      (should (or (equal (plist-get block :type) "thinking")
                  (eq (plist-get block :type) 'thinking)))
      (should (equal (plist-get block :text) "reasoning continued")))))

(ert-deftest beads-agent-ralph-stream-test-partial-tool-use-input-json ()
  "input_json_delta chunks reassemble into a parsed :input plist."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"msg_tu\",\"role\":\"assistant\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_1\",\"name\":\"Bash\",\"input\":{}}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"command\\\":\\\"\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"bd show bde-1\\\"}\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":0}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}"))
    (let* ((synth (cl-find-if
                   (lambda (ev) (plist-get ev :__synthesized-from-partials))
                   (oref stream events)))
           (block (car (plist-get (plist-get synth :message) :content))))
      (should synth)
      (should (or (equal (plist-get block :type) "tool_use")
                  (eq (plist-get block :type) 'tool_use)))
      (should (equal (plist-get block :name) "Bash"))
      (should (equal (plist-get block :id) "tu_1"))
      (should (equal (plist-get (plist-get block :input) :command)
                     "bd show bde-1")))))

(ert-deftest beads-agent-ralph-stream-test-partial-multi-block-ordering ()
  "Two content blocks emit at their respective indices in order."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"msg_m\",\"role\":\"assistant\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"first\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":0}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"second\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":1}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}"))
    (let* ((synth (cl-find-if
                   (lambda (ev) (plist-get ev :__synthesized-from-partials))
                   (oref stream events)))
           (content (plist-get (plist-get synth :message) :content)))
      (should (= (length content) 2))
      (should (equal (plist-get (nth 0 content) :text) "first"))
      (should (equal (plist-get (nth 1 content) :text) "second")))))

(ert-deftest beads-agent-ralph-stream-test-partial-stop-reason-captured ()
  "message_delta stop_reason is captured into the synthesized message."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"msg_d\",\"role\":\"assistant\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":0}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}"))
    (let* ((synth (cl-find-if
                   (lambda (ev) (plist-get ev :__synthesized-from-partials))
                   (oref stream events))))
      (should synth)
      (should (equal (plist-get (plist-get synth :message) :stop_reason)
                     "end_turn")))))

(ert-deftest beads-agent-ralph-stream-test-partial-key-fallback-recent ()
  "A delta arriving without an explicit id routes to the most recent message id."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"msg_r\",\"role\":\"assistant\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}}"
       ;; Delta without message id — should bucket onto msg_r via :current-message-id.
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"recovered\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":0}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}"))
    (let* ((synth (cl-find-if
                   (lambda (ev) (plist-get ev :__synthesized-from-partials))
                   (oref stream events))))
      (should synth)
      (should (equal (plist-get (plist-get synth :message) :id) "msg_r"))
      (should (equal (plist-get (car (plist-get (plist-get synth :message) :content))
                                :text)
                     "recovered")))))

(ert-deftest beads-agent-ralph-stream-test-partial-no-double-emit ()
  "A non-partial assistant event arriving after the synthesized one stays distinct.
Both events end up on the stream's `events' list — the synthesized
one carries the `:__synthesized-from-partials' marker so callers can
prefer the canonical event when both are present."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"msg_n\",\"role\":\"assistant\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"partial-text\"}}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_stop\",\"index\":0}}"
       "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}"
       "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_n\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"final-text\"}]}}"))
    (let* ((events (oref stream events))
           (synth (cl-find-if
                   (lambda (ev) (plist-get ev :__synthesized-from-partials))
                   events))
           (final
            (cl-find-if
             (lambda (ev)
               (and (not (plist-get ev :__synthesized-from-partials))
                    (let ((type (plist-get ev :type)))
                      (or (equal type "assistant") (eq type 'assistant)))))
             events)))
      (should synth)
      (should final)
      (should-not (eq synth final)))))

(ert-deftest beads-agent-ralph-stream-test-non-partial-event-untouched ()
  "Plain assistant events bypass partial-message handling entirely."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"assistant\",\"message\":{\"id\":\"msg_x\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"direct\"}]}}"))
    ;; No synthesized event; the assistant event itself is on the events list.
    (should-not
     (cl-some (lambda (ev) (plist-get ev :__synthesized-from-partials))
              (oref stream events)))
    (should (equal 1 (length (oref stream events))))))

(ert-deftest beads-agent-ralph-stream-test-partial-event-without-id ()
  "Envelopes with no derivable key absorb silently without crashing."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     ;; No prior message_start; delta has no message id and no fallbacks.
     '("{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"orphan\"}}}"))
    ;; Should still land on `events' for forensics — just no synthesis.
    (should (= 1 (length (oref stream events))))
    (should-not
     (cl-some (lambda (ev) (plist-get ev :__synthesized-from-partials))
              (oref stream events)))))

;;; Parser fixture tests (bde-48i7)

(ert-deftest beads-agent-ralph-stream-test-filter-byte-by-byte ()
  "Feeding NDJSON one byte at a time through `-filter' produces the same events.
This is the canonical test for the partial-line reassembly: the
filter must accumulate bytes across calls and only emit events on
newline boundaries."
  (let* ((stream (beads-agent-ralph-stream-test--make-stream))
         (line "{\"type\":\"assistant\",\"message\":{\"id\":\"x\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]}}\n"))
    (dolist (ch (string-to-list line))
      (beads-agent-ralph--stream-filter stream (string ch)))
    (should (= 1 (length (oref stream events))))
    (let ((evt (car (oref stream events))))
      (should (equal "assistant" (plist-get evt :type))))))

(ert-deftest beads-agent-ralph-stream-test-filter-utf8-split-across-chunks ()
  "Multibyte chars split across kernel chunks reassemble cleanly.
Because `make-process' uses :coding utf-8-unix Emacs handles the
boundary; we still test that the filter accepts the result without
corrupting it."
  (let* ((stream (beads-agent-ralph-stream-test--make-stream))
         (line "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"héllo 日本\"}]}}\n")
         (mid (/ (length line) 2)))
    ;; Split at a multibyte boundary deliberately.
    (beads-agent-ralph--stream-filter stream (substring line 0 mid))
    (beads-agent-ralph--stream-filter stream (substring line mid))
    (should (= 1 (length (oref stream events))))
    (let* ((evt (car (oref stream events)))
           (text (plist-get (car (plist-get (plist-get evt :message)
                                            :content))
                            :text)))
      (should (string-match-p "héllo" text))
      (should (string-match-p "日本" text)))))

(ert-deftest beads-agent-ralph-stream-test-unknown-event-preserved ()
  "Unknown event types are preserved verbatim on the events list."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"future_thing\",\"payload\":42}"))
    (let ((evt (car (oref stream events))))
      (should (equal "future_thing" (plist-get evt :type)))
      (should (= 42 (plist-get evt :payload))))))

(ert-deftest beads-agent-ralph-stream-test-malformed-line-error-event ()
  "Malformed NDJSON becomes an error-typed placeholder event."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed
     stream
     '("not-valid-json"))
    (let ((evt (car (oref stream events))))
      (should (eq 'error (plist-get evt :type)))
      (should (equal "not-valid-json" (plist-get evt :raw))))))

(ert-deftest beads-agent-ralph-stream-test-status-transitions-to-running-on-first-event ()
  "The starting status transitions to running once an event lands."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (should (eq (oref stream status) 'starting))
    (beads-agent-ralph-stream-test--feed
     stream
     '("{\"type\":\"system\",\"subtype\":\"init\"}"))
    (should (eq (oref stream status) 'running))))

(ert-deftest beads-agent-ralph-stream-test-empty-line-ignored ()
  "Empty lines do not produce an event."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph-stream-test--feed stream '(""))
    (should (null (oref stream events)))))

(ert-deftest beads-agent-ralph-stream-test-trailing-line-buffered ()
  "Bytes received without a terminating newline are buffered."
  (let ((stream (beads-agent-ralph-stream-test--make-stream)))
    (beads-agent-ralph--stream-filter stream "{\"type\":\"assistant\"")
    (should (null (oref stream events)))
    (should (string-match-p "\"type\":\"assistant\""
                            (oref stream partial-line)))
    (beads-agent-ralph--stream-filter stream "}\n")
    (should (= 1 (length (oref stream events))))
    (should (equal "" (oref stream partial-line)))))

;;; Subscriber dispatch

(ert-deftest beads-agent-ralph-stream-test-subscriber-receives-event ()
  "Subscribers fire on dispatch."
  (let* ((stream (beads-agent-ralph-stream-test--make-stream))
         (called 0))
    (beads-agent-ralph--stream-subscribe
     stream 'test (lambda (_s) (cl-incf called)))
    (beads-agent-ralph--stream-dispatch stream)
    (should (= 1 called))))

(ert-deftest beads-agent-ralph-stream-test-subscriber-error-isolated ()
  "An error in one subscriber does not break the others."
  (let* ((stream (beads-agent-ralph-stream-test--make-stream))
         (called nil))
    (beads-agent-ralph--stream-subscribe
     stream 'bad (lambda (_s) (error "boom")))
    (beads-agent-ralph--stream-subscribe
     stream 'good (lambda (_s) (setq called t)))
    (beads-agent-ralph--stream-dispatch stream)
    (should called)))

(ert-deftest beads-agent-ralph-stream-test-unsubscribe ()
  "Unsubscribe removes the labelled callback."
  (let* ((stream (beads-agent-ralph-stream-test--make-stream))
         (called nil))
    (beads-agent-ralph--stream-subscribe
     stream 'x (lambda (_s) (setq called t)))
    (beads-agent-ralph--stream-unsubscribe stream 'x)
    (beads-agent-ralph--stream-dispatch stream)
    (should-not called)))

;;; Stderr tail

(ert-deftest beads-agent-ralph-stream-test-stderr-tail-bounded ()
  "Stderr tail is bounded by `beads-agent-ralph-stderr-tail-max'."
  (let* ((beads-agent-ralph-stderr-tail-max 5)
         (stream (beads-agent-ralph-stream-test--make-stream)))
    (dotimes (i 10)
      (beads-agent-ralph--stream-stderr-filter
       stream (format "line %d\n" i)))
    (should (<= (length (oref stream stderr-tail)) 5))))

;;; Spawn failure (bde-qc6k)

(ert-deftest beads-agent-ralph-stream-test-spawn-failure-cleans-stderr-pipe ()
  "When `make-process' raises, the stderr pipe is deleted (bde-qc6k).

Without the cleanup, the stderr pipe-process created as `make-process's
:stderr argument leaks: it is live before `make-process' fails and the
caller has no handle to delete it.  Mirrors the bde-km3r fix."
  (let* ((before (length (process-list)))
         (stderr-name "bde-qc6k-stderr-leak-test-stderr"))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (error "intentional spawn failure"))))
      (should-error
       (beads-agent-ralph--stream-spawn
        :prompt "p"
        :name "bde-qc6k-stderr-leak-test"
        :program "claude")))
    ;; No leftover stderr pipe and no net new processes from this test.
    (should-not (cl-find-if (lambda (p)
                              (string= (process-name p) stderr-name))
                            (process-list)))
    (should (= before (length (process-list))))))

(provide 'beads-agent-ralph-stream-test)

;;; beads-agent-ralph-stream-test.el ends here
