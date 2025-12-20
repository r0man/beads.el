;;; beads-agent-mock.el --- Mock backend for testing -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, testing

;;; Commentary:

;; This module provides a mock backend for testing beads-agent without
;; requiring real AI agent infrastructure.  It simulates all protocol
;; methods and maintains internal state for test verification.
;;
;; Usage:
;;   (require 'beads-agent-mock)
;;
;;   ;; Register the mock backend
;;   (beads-agent-mock-register)
;;
;;   ;; Configure mock behavior
;;   (setq beads-agent-mock-available t)  ; Control availability
;;
;;   ;; After tests, check state
;;   (beads-agent-mock-sessions)          ; Returns tracked sessions
;;
;;   ;; Cleanup
;;   (beads-agent-mock-reset)             ; Clear all state

;;; Code:

(require 'beads-agent-backend)
(require 'cl-lib)

;;; Configuration

(defvar beads-agent-mock-available t
  "Whether the mock backend should report as available.
Set to nil to test unavailable backend scenarios.")

(defvar beads-agent-mock-start-should-error nil
  "When non-nil, `beads-agent-backend-start' will signal an error.
Set to a string for custom error message.")

(defvar beads-agent-mock-stop-should-error nil
  "When non-nil, `beads-agent-backend-stop' will signal an error.
Set to a string for custom error message.")

;;; Internal State

(defvar beads-agent-mock--sessions (make-hash-table :test #'equal)
  "Hash table of mock sessions, keyed by session handle.")

(defvar beads-agent-mock--session-counter 0
  "Counter for generating unique session handles.")

(defvar beads-agent-mock--start-calls nil
  "List of (issue prompt) pairs from start calls, most recent first.")

(defvar beads-agent-mock--stop-calls nil
  "List of session objects from stop calls, most recent first.")

(defvar beads-agent-mock--sent-prompts nil
  "List of (session prompt) pairs from send-prompt calls, most recent first.")

;;; Mock Backend Class

(defclass beads-agent-backend-mock (beads-agent-backend)
  ((name :initform "mock")
   (priority :initform 100)
   (description :initform "Testing backend (not for production use)"))
  :documentation "Mock backend for testing beads-agent.
Simulates all protocol methods without external dependencies.")

;;; Mock Session Handle

(defclass beads-agent-mock-session-handle ()
  ((id
    :initarg :id
    :type string
    :documentation "Unique handle identifier.")
   (issue
    :initarg :issue
    :documentation "The issue object passed to start.")
   (prompt
    :initarg :prompt
    :type string
    :documentation "The initial prompt.")
   (active
    :initarg :active
    :initform t
    :type boolean
    :documentation "Whether this session is active.")
   (buffer
    :initarg :buffer
    :initform nil
    :documentation "The Emacs buffer for this mock session.
Created during start to allow testing buffer renaming."))
  :documentation "Handle returned by mock backend start.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-mock))
  "Return `beads-agent-mock-available' value."
  beads-agent-mock-available)

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-mock) issue prompt)
  "Start mock session with ISSUE and PROMPT.
Returns cons cell (BACKEND-SESSION . BUFFER).  Signals error if
`beads-agent-mock-start-should-error' is set.
Creates a temporary buffer for testing buffer renaming."
  (when beads-agent-mock-start-should-error
    (error (if (stringp beads-agent-mock-start-should-error)
               beads-agent-mock-start-should-error
             "Mock start error")))
  ;; Record the call
  (push (list issue prompt) beads-agent-mock--start-calls)
  ;; Create and track session handle with a real buffer
  (let* ((handle-id (format "mock-session-%d"
                            (cl-incf beads-agent-mock--session-counter)))
         ;; Create a buffer for this mock session
         (buffer-name (format "*mock-agent-%s*" handle-id))
         (buffer (get-buffer-create buffer-name))
         (handle (beads-agent-mock-session-handle
                  :id handle-id
                  :issue issue
                  :prompt prompt
                  :active t
                  :buffer buffer)))
    (puthash handle-id handle beads-agent-mock--sessions)
    ;; Return (backend-session . buffer)
    (cons handle buffer)))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-mock) session)
  "Stop mock SESSION.
Marks the session handle as inactive and kills the buffer."
  (when beads-agent-mock-stop-should-error
    (error (if (stringp beads-agent-mock-stop-should-error)
               beads-agent-mock-stop-should-error
             "Mock stop error")))
  ;; Record the call
  (push session beads-agent-mock--stop-calls)
  ;; Mark session as inactive and kill buffer
  (when-let* ((backend-session (oref session backend-session))
              (handle-id (oref backend-session id))
              (handle (gethash handle-id beads-agent-mock--sessions)))
    (oset handle active nil)
    ;; Kill the buffer if it exists
    (when-let ((buffer (oref handle buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-mock) session)
  "Check if mock SESSION is still active."
  (when-let* ((backend-session (oref session backend-session))
              (handle-id (oref backend-session id))
              (handle (gethash handle-id beads-agent-mock--sessions)))
    (oref handle active)))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((backend beads-agent-backend-mock) session)
  "Switch to buffer for mock SESSION via BACKEND."
  (let ((buffer (or (beads-agent-backend-get-buffer backend session)
                    ;; Try to find by expected name (recovery for old sessions)
                    (get-buffer (beads-agent--generate-buffer-name-for-session
                                 session)))))
    (cond
     ((and buffer (buffer-live-p buffer))
      ;; Store buffer if we recovered it by name
      (unless (beads-agent-session-buffer session)
        (beads-agent-session-set-buffer session buffer))
      (beads-agent--pop-to-buffer-other-window buffer))
     (buffer
      (user-error "Agent buffer has been killed"))
     (t
      (user-error "No buffer found for session %s" (oref session id))))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-mock) session prompt)
  "Send PROMPT to mock SESSION."
  (push (list session prompt) beads-agent-mock--sent-prompts)
  (message "Mock: sent prompt to session"))

(cl-defmethod beads-agent-backend-get-buffer
    ((_backend beads-agent-backend-mock) session)
  "Return the buffer for mock SESSION.
First checks if the session has a stored buffer (after renaming),
then falls back to the mock handle's original buffer."
  ;; First try the session's stored buffer (set after renaming)
  (let ((stored-buffer (beads-agent-session-buffer session)))
    (if (and stored-buffer (buffer-live-p stored-buffer))
        stored-buffer
      ;; Fall back to the mock handle's buffer
      (when-let* ((backend-session (oref session backend-session))
                  (handle-id (oref backend-session id))
                  (handle (gethash handle-id beads-agent-mock--sessions))
                  (buffer (oref handle buffer)))
        (when (buffer-live-p buffer)
          buffer)))))

;;; Public API for Tests

(defvar beads-agent-mock--instance nil
  "Singleton mock backend instance.")

(defun beads-agent-mock-get-instance ()
  "Get or create the singleton mock backend instance."
  (unless beads-agent-mock--instance
    (setq beads-agent-mock--instance
          (beads-agent-backend-mock)))
  beads-agent-mock--instance)

(defun beads-agent-mock-register ()
  "Register the mock backend for testing.
Returns the backend instance."
  (let ((backend (beads-agent-mock-get-instance)))
    (beads-agent--register-backend backend)
    backend))

(defun beads-agent-mock-unregister ()
  "Unregister the mock backend."
  (setq beads-agent--backends
        (cl-remove-if (lambda (b)
                        (equal (oref b name) "mock"))
                      beads-agent--backends)))

(defun beads-agent-mock-reset ()
  "Reset all mock state.
Clears sessions (killing buffers), call logs, and configuration."
  (setq beads-agent-mock-available t)
  (setq beads-agent-mock-start-should-error nil)
  (setq beads-agent-mock-stop-should-error nil)
  (setq beads-agent-mock--session-counter 0)
  ;; Kill all mock session buffers before clearing
  (maphash (lambda (_id handle)
             (when-let ((buffer (oref handle buffer)))
               (when (buffer-live-p buffer)
                 (kill-buffer buffer))))
           beads-agent-mock--sessions)
  (clrhash beads-agent-mock--sessions)
  (setq beads-agent-mock--start-calls nil)
  (setq beads-agent-mock--stop-calls nil)
  (setq beads-agent-mock--sent-prompts nil))

(defun beads-agent-mock-sessions ()
  "Return list of all tracked mock session handles."
  (hash-table-values beads-agent-mock--sessions))

(defun beads-agent-mock-active-sessions ()
  "Return list of active mock session handles."
  (cl-remove-if-not (lambda (h) (oref h active))
                    (beads-agent-mock-sessions)))

(defun beads-agent-mock-start-calls ()
  "Return list of (issue prompt) from start invocations, most recent first."
  beads-agent-mock--start-calls)

(defun beads-agent-mock-stop-calls ()
  "Return list of sessions from stop invocations, most recent first."
  beads-agent-mock--stop-calls)

(defun beads-agent-mock-sent-prompts ()
  "Return list of (session prompt) from send-prompt invocations."
  beads-agent-mock--sent-prompts)

;;; Test Assertion Helpers

(defun beads-agent-mock-assert-start-called (&optional times)
  "Assert that start was called TIMES (default 1) times."
  (let ((times (or times 1))
        (actual (length beads-agent-mock--start-calls)))
    (unless (= actual times)
      (error "Expected %d start call(s), got %d" times actual))))

(defun beads-agent-mock-assert-stop-called (&optional times)
  "Assert that stop was called TIMES (default 1) times."
  (let ((times (or times 1))
        (actual (length beads-agent-mock--stop-calls)))
    (unless (= actual times)
      (error "Expected %d stop call(s), got %d" times actual))))

(defun beads-agent-mock-assert-no-sessions ()
  "Assert that no mock sessions exist."
  (let ((count (hash-table-count beads-agent-mock--sessions)))
    (unless (zerop count)
      (error "Expected 0 sessions, got %d" count))))

(defun beads-agent-mock-assert-session-count (expected)
  "Assert that EXPECTED mock sessions exist."
  (let ((actual (hash-table-count beads-agent-mock--sessions)))
    (unless (= actual expected)
      (error "Expected %d session(s), got %d" expected actual))))

(provide 'beads-agent-mock)

;;; beads-agent-mock.el ends here
