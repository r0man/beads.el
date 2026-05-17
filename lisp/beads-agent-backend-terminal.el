;;; beads-agent-backend-terminal.el --- Terminal-spawned agent backend -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Phase 1b (bde-xle9.3): an abstract `beads-agent-backend-terminal'
;; that spawns the agent CLI directly into a `beads-terminal',
;; owning the buffer name from spawn — collision-free by construction,
;; the structural fix for the bde-h93r wrapper-buffer hijack (for
;; users who opt in to this backend).
;;
;; The abstract intermediate defines DEFAULT `cl-defmethod's for every
;; `beads-agent-backend' generic that lacks an abstract-base default
;; (available-p / start / stop / session-active-p / switch-to-buffer /
;; send-prompt).  `stop-async' / `session-name' / `get-buffer' are
;; inherited from `beads-agent-backend' (they have base defaults).
;; This is the gate that prevents a `no-applicable-method' the first
;; time the orchestrator queries availability.
;;
;; `beads-agent-backend-claude' is the single concrete in this phase:
;; the `claude' CLI in a terminal.  It ships opt-in — it is NOT
;; registered as any per-type default here (that flip is deferred).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'beads-agent-backend)
(require 'beads-terminal)

;;; Defcustom

(defcustom beads-agent-default-terminal 'beads-terminal-auto
  "Default `beads-terminal' subclass symbol for agent spawning.
Governs the agent terminal spawn path (the class-valued knob),
distinct from the symbol-valued `beads-terminal-backend' which
governs one-shot `bd' command execution.  Both coexist for one
release; see NEWS."
  :type 'symbol
  :group 'beads-terminal)

;;; Abstract intermediate

(defclass beads-agent-backend-terminal (beads-agent-backend)
  ((command
    :initarg :command
    :type string
    :documentation "CLI executable name; resolved via `executable-find'.")
   (cli-agent-name
    :initarg :cli-agent-name
    :initform nil
    :type (or string null)
    :documentation "Named CLI agent, or nil.  Emitted only with `agent-flag'.")
   (agent-flag
    :initarg :agent-flag
    :initform nil
    :type (or string null)
    :documentation "Flag introducing a named agent, or nil for none.
nil means no agent flag (claude has none).")
   (user-prompt-position
    :initarg :user-prompt-position
    :initform 'positional
    :type (member positional stdin none)
    :documentation "Where the user prompt goes.
`positional' appends it as the argv tail.  `stdin' is REJECTED
before spawn in this release (send-input is deferred).  `none'
omits it.")
   (system-prompt-flag
    :initarg :system-prompt-flag
    :initform "--append-system-prompt"
    :type (or string null)
    :documentation "Flag carrying the system prompt, or nil.
`--append-system-prompt' (over `--system-prompt') preserves Claude
Code's built-in tool-use scaffolding.")
   (system-prompt-position
    :initarg :system-prompt-position
    :initform 'flag
    :type (member flag env none)
    :documentation "How the system prompt is delivered: argv `flag',
`env' variable, or `none'.")
   (system-prompt-env
    :initarg :system-prompt-env
    :initform nil
    :type (or string null)
    :documentation "Env var name for the system prompt when
`system-prompt-position' is `env', else nil.")
   (env
    :initarg :env
    :initform nil
    :type list
    :documentation "Alist of (NAME . VALUE) ADDED to (not replacing)
`process-environment'.  TERM must NOT be set here — the terminal
package owns TERM/terminfo; a user-supplied TERM shadows it and
breaks rendering.")
   (terminal
    :initarg :terminal
    :initform nil
    :type symbol
    :documentation "`beads-terminal' subclass symbol, or nil.
nil resolves late to `beads-agent-default-terminal'.  Not validated
at `make-instance'; an unknown class signals a clear error at spawn."))
  :abstract t
  :documentation "Abstract terminal-spawned agent backend.
Defines defaults for every backend generic so a concrete that only
sets slots is fully functional.")

;;; Argv construction

(defun beads-agent-backend-terminal-build-argv (backend system-prompt
                                                        user-prompt extra-args)
  "Build the argv list for BACKEND.
Appends, in order: the command; the (agent-flag cli-agent-name)
pair when both are non-nil; the (system-prompt-flag SYSTEM-PROMPT)
pair when `system-prompt-position' is `flag', the flag is non-nil
and SYSTEM-PROMPT is non-empty; EXTRA-ARGS; then USER-PROMPT when
`user-prompt-position' is `positional' and USER-PROMPT is non-empty.
The non-empty predicate is the same `(and s (not (string-empty-p
s)))' used by `beads-agent-backend--combine-prompt' so an empty
system prompt is treated as absent everywhere."
  (let ((argv (list (oref backend command)))
        (agent-flag (oref backend agent-flag))
        (cli-agent-name (oref backend cli-agent-name)))
    (when (and agent-flag cli-agent-name)
      (setq argv (append argv (list agent-flag cli-agent-name))))
    (when (and (eq (oref backend system-prompt-position) 'flag)
               (oref backend system-prompt-flag)
               system-prompt
               (not (string-empty-p system-prompt)))
      (setq argv (append argv (list (oref backend system-prompt-flag)
                                    system-prompt))))
    (when extra-args
      (setq argv (append argv extra-args)))
    (when (and (eq (oref backend user-prompt-position) 'positional)
               user-prompt
               (not (string-empty-p user-prompt)))
      (setq argv (append argv (list user-prompt))))
    argv))

;;; Terminal resolution

(defun beads-agent-backend-terminal--resolve (backend)
  "Return a `beads-terminal' instance for BACKEND's terminal slot.
nil resolves to `beads-agent-default-terminal'.  Signals a clear
`error' when the resolved class symbol is not a `beads-terminal'."
  (let ((sym (or (oref backend terminal) beads-agent-default-terminal)))
    (unless (and sym (class-p sym)
                 (child-of-class-p sym 'beads-terminal))
      (error "Unknown terminal class: %S" sym))
    (make-instance sym)))

(defun beads-agent-backend-terminal--buffer-name (working-dir)
  "Return a fresh, collision-free agent buffer name for WORKING-DIR.
`generate-new-buffer-name' guarantees the name is unowned, so the
terminal owns it from spawn and no later rename is needed."
  (generate-new-buffer-name
   (format "*beads-agent[%s]*"
           (file-name-nondirectory (directory-file-name working-dir)))))

;;; Protocol defaults

(cl-defmethod beads-agent-backend-available-p
  ((backend beads-agent-backend-terminal))
  "Return non-nil when BACKEND's command is on PATH."
  (and (executable-find (oref backend command)) t))

(cl-defmethod beads-agent-backend-start
  ((backend beads-agent-backend-terminal) _issue system-prompt user-prompt)
  "Spawn BACKEND's CLI in a terminal with SYSTEM-PROMPT / USER-PROMPT.
ISSUE is unused (terminal backends are per working directory).
`stdin' user-prompt delivery is rejected BEFORE any spawn.  Returns
\(nil . BUFFER); the buffer name is owned from spawn so the
orchestrator's later `rename-buffer' is a harmless no-op."
  (when (eq (oref backend user-prompt-position) 'stdin)
    (error "Stdin user-prompt delivery not implemented in this release"))
  (unless (executable-find (oref backend command))
    (error "Command not found in PATH: %s" (oref backend command)))
  (let* ((working-dir default-directory)
         (terminal (beads-agent-backend-terminal--resolve backend))
         (argv (beads-agent-backend-terminal-build-argv
                backend system-prompt user-prompt nil))
         (buf-name (beads-agent-backend-terminal--buffer-name working-dir))
         (env (oref backend env))
         (buffer (beads-terminal-spawn terminal buf-name argv
                                       working-dir env)))
    (cons nil buffer)))

(cl-defmethod beads-agent-backend-stop
  ((backend beads-agent-backend-terminal) session)
  "Stop SESSION on BACKEND: delete the buffer process, kill the buffer."
  (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
    (when (buffer-live-p buffer)
      (when-let ((proc (get-buffer-process buffer)))
        (when (process-live-p proc)
          (delete-process proc)))
      (kill-buffer buffer))))

(cl-defmethod beads-agent-backend-session-active-p
  ((backend beads-agent-backend-terminal) session)
  "Return non-nil when SESSION's buffer process is live on BACKEND."
  (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
    (and (buffer-live-p buffer)
         (when-let ((proc (get-buffer-process buffer)))
           (process-live-p proc))
         t)))

(cl-defmethod beads-agent-backend-switch-to-buffer
  ((backend beads-agent-backend-terminal) session)
  "Pop to SESSION's terminal buffer (BACKEND resolves the buffer)."
  (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(cl-defmethod beads-agent-backend-send-prompt
  ((_backend beads-agent-backend-terminal) _session _prompt)
  "Signal the deferred send-input limitation (see NEWS)."
  (error "Stdin/send-prompt delivery not implemented in this release"))

;;; Concrete: claude CLI in a terminal (opt-in)

(defclass beads-agent-backend-claude (beads-agent-backend-terminal)
  ((name :initform "claude")
   (priority :initform 30)
   (description :initform "claude CLI spawned directly into a terminal")
   (command :initform "claude")
   (user-prompt-position :initform 'positional))
  :documentation "The `claude' CLI spawned directly into a terminal.
Collision-free by construction; ships opt-in (NOT a per-type
default in this release).  `agent-flag' is intentionally unset —
claude has no named-agent flag.")

;;; Registration

;; Register the terminal-spawned claude backend so it is selectable.
;; This is OPT-IN: the per-type backend defcustoms
;; (`beads-agent-{task,review,plan,qa}-backend') are NOT flipped, so a
;; user who never customised their backend gets exactly the prior
;; behaviour.  Registration only makes "claude" available to choose.
(beads-agent--register-backend (beads-agent-backend-claude))

(provide 'beads-agent-backend-terminal)
;;; beads-agent-backend-terminal.el ends here
