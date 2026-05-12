;;; beads-agent-ralph-backend.el --- beads-agent-backend subclass for Ralph -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module defines `beads-agent-backend-ralph', the
;; `beads-agent-backend' subclass that makes the Ralph loop look like
;; any other agent backend to the rest of beads.el (transient menus,
;; sesman integration, mode-line, state-change-hook, …).
;;
;; Mapping of protocol methods to Ralph internals:
;;
;;   available-p       claude executable on `exec-path'
;;   start             spawn controller; return (controller . dashboard-buffer)
;;   stop              `beads-agent-ralph-stop' — graceful, prompts user
;;   stop-async        same as stop, never blocks
;;   send-prompt       UNSUPPORTED — signals user-error
;;   session-active-p  controller status in (running cooling-down)
;;   switch-to-buffer  switch to the dashboard buffer
;;
;; The controller entry points (`beads-agent-ralph-start',
;; `beads-agent-ralph-stop', `beads-agent-ralph-show-history') are
;; defined in `beads-agent-ralph.el' by the controller per-iteration
;; sequence task (bde-q3b4).  This module forward-declares them so it
;; byte-compiles standalone; calls happen at runtime, not load time.

;;; Code:

(require 'eieio)
(require 'cl-lib)

(require 'beads-agent-backend)
(require 'beads-agent-ralph)

;;; External Function Declarations

;; Defined in beads-agent-ralph.el by the controller per-iteration
;; sequence task (bde-q3b4).  These are runtime-resolved.
(declare-function beads-agent-ralph-start "beads-agent-ralph")
(declare-function beads-agent-ralph-stop "beads-agent-ralph")

;;; Backend Class

(defclass beads-agent-backend-ralph (beads-agent-backend)
  ((name :initform "ralph")
   (priority :initform 60)
   (description :initform "Ralph Wiggum overnight iteration loop"))
  :documentation "Backend wrapping the Ralph controller as a beads-agent-backend.

Unlike interactive backends (claude-code, eca, etc.), Ralph runs an
autonomous iteration loop: a single `start' call spawns a controller
that drives many `claude --print' subprocesses to completion.  The
backend session handle is the `beads-agent-ralph--controller'; the
buffer is the dashboard buffer.")

;;; Protocol

(cl-defmethod beads-agent-backend-available-p
  ((_backend beads-agent-backend-ralph))
  "Return non-nil if the Ralph backend can run.
Requires the `claude' executable on the variable `exec-path'."
  (and (executable-find "claude") t))

(cl-defmethod beads-agent-backend-start
  ((_backend beads-agent-backend-ralph) issue prompt)
  "Start a Ralph loop for ISSUE with the rendered PROMPT.

Delegates to `beads-agent-ralph-start' (defined in
`beads-agent-ralph.el' by the controller per-iteration sequence
task).  Returns a cons cell (CONTROLLER . DASHBOARD-BUFFER) where
CONTROLLER is the `beads-agent-ralph--controller' and DASHBOARD-BUFFER
is the vui dashboard buffer.

Signals an error if `beads-agent-ralph-start' has not been loaded
yet.  Tests stub the entry point via `cl-letf'."
  (unless (fboundp 'beads-agent-ralph-start)
    (error "Ralph entry point `beads-agent-ralph-start' is not loaded"))
  (beads-agent-ralph-start :issue issue :prompt prompt))

(cl-defmethod beads-agent-backend-stop
  ((_backend beads-agent-backend-ralph) session)
  "Stop the Ralph SESSION (graceful).

Delegates to `beads-agent-ralph-stop', which prompt the user whether
to stop now or after the current iteration.  Use
`beads-agent-backend-stop-async' to avoid blocking."
  (unless (fboundp 'beads-agent-ralph-stop)
    (error "Ralph entry point `beads-agent-ralph-stop' is not loaded"))
  (let ((controller (oref session backend-session)))
    (beads-agent-ralph-stop controller)))

(cl-defmethod beads-agent-backend-stop-async
  ((backend beads-agent-backend-ralph) session callback)
  "Stop Ralph SESSION on BACKEND asynchronously, invoking CALLBACK when complete.

The default super implementation calls sync stop via `run-at-time';
that is suitable for Ralph because the controller's stop machinery
itself does the heavy lifting asynchronously (it kills the in-flight
stream and awaits the sentinel)."
  (cl-call-next-method backend session callback))

(cl-defmethod beads-agent-backend-send-prompt
  ((_backend beads-agent-backend-ralph) _session _prompt)
  "Reject SESSION ad-hoc PROMPT injection on BACKEND: Ralph drives its own loop.

Signal a `user-error' rather than silently doing nothing.  Users
wanting to change what the loop is doing should edit the template via
the dashboard's edit-prompt affordance."
  (user-error
   "Ralph loops don't accept ad-hoc prompts; edit the prompt template instead"))

(cl-defmethod beads-agent-backend-session-active-p
  ((_backend beads-agent-backend-ralph) session)
  "Return non-nil while the Ralph controller for SESSION is running.

The states `running' and `cooling-down' both count as active so the
dashboard remains visible between iterations.  `auto-paused' counts as
INACTIVE because the loop is waiting on a resume keystroke; the session
can be swept by sesman without ambiguity."
  (when-let ((controller (oref session backend-session)))
    (memq (oref controller status) '(running cooling-down))))

(cl-defmethod beads-agent-backend-switch-to-buffer
  ((backend beads-agent-backend-ralph) session)
  "Switch to the Ralph dashboard buffer for SESSION on BACKEND.

Reuses the default `beads-agent-backend-get-buffer' which returns the
buffer stored on the session by the framework when start completes."
  (if-let ((buffer (beads-agent-backend-get-buffer backend session)))
      (if (buffer-live-p buffer)
          (beads-agent--pop-to-buffer-other-window buffer)
        (user-error "Ralph dashboard buffer has been killed"))
    (user-error "No dashboard buffer for Ralph session %s"
                (oref session id))))

;;; Registration
;;
;; Register on load so the backend appears in completion/transient
;; surfaces.  The framework re-registers idempotently if loaded twice.

(beads-agent--register-backend (beads-agent-backend-ralph))

(provide 'beads-agent-ralph-backend)

;;; beads-agent-ralph-backend.el ends here
