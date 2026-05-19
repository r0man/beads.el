;;; beads-terminal.el --- Terminal subsystem for beads agents -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, terminals

;;; Commentary:

;; EIEIO terminal subsystem: an abstract `beads-terminal' with five
;; concrete terminals (vterm, ghostel, eat, ansi-term, term) plus an
;; `beads-terminal-auto' that walks them in priority order.  This is
;; Phase 1b of the terminal-spawned agent backend epic (bde-xle9.3)
;; and is purely additive — nothing else changes behaviour by loading
;; this file.
;;
;; The per-terminal `beads-terminal-spawn' methods wrap the proven
;; function-based runners in `beads-command.el' (`--run-{vterm,eat,
;; term}') *verbatim except for the cd/exit wrapper*: beads adds no
;; `cd <dir> && … ; exit' of its own.  The working directory is set by
;; `let'-binding `default-directory' around the package's
;; process-spawning call (and, for buffer-exec terminals, making the
;; target buffer current within that binding).  The terminal package's
;; own intrinsic exec shim (vterm-shell, term-exec's /bin/sh -c) is
;; out of beads' control and acceptable.
;;
;; The `beads-terminal' defgroup is owned here; `beads-command.el'
;; re-`require's this file and keeps using the symbol-valued
;; `beads-terminal-backend' defcustom for one-shot `bd' command
;; execution.  For one release there are two terminal knobs in the
;; same group; see NEWS.  `beads-terminal--symbol->class' bridges the
;; old symbol vocabulary so the Phase 3 unification is mechanical.

;;; Code:

(require 'cl-lib)
(require 'eieio)

;; Dynamic variables owned by optional terminal packages.  Declared
;; here so the compiler treats the `let'-bindings below as dynamic
;; (the packages may be absent at compile time).
(defvar vterm-shell)
(defvar vterm-buffer-name)
(defvar vterm-kill-buffer-on-exit)
(defvar ghostel-shell)
(defvar ghostel-buffer-name)
(defvar ghostel-kill-buffer-on-exit)
(defvar eat-kill-buffer-on-exit)
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile switches))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))

;;; Customization

(defgroup beads-terminal nil
  "Terminal settings for beads command and agent execution."
  :group 'beads
  :prefix "beads-terminal-")

(defcustom beads-terminal-backend nil
  "Backend to use for interactive one-shot `bd' command execution.

Deprecated (Phase 3): this symbol-valued knob is superseded by the
unified `beads-terminal' spawn vocabulary.  It is still honoured for
back-compat — `beads-command--run-in-terminal' resolves it through
`beads-terminal--symbol->class' and delegates to
`beads-terminal-spawn' — but new configuration should prefer the
class-valued terminal selection (`beads-agent-default-terminal' for
agents).  Expect this symbol-valued option to be removed in a future
release.

This knob governs `beads-command--run-in-terminal' ONLY (running a
single `bd' command in a terminal).  The agent spawn path is
governed by the class-valued `beads-agent-default-terminal'.

When nil (auto-detect), tries backends in order: vterm, eat, term.
The first available backend is used.

Available backends:
- nil: Auto-detect best available backend (vterm > eat > term).
- `vterm': Use vterm (libvterm-based terminal).
- `eat': Use Eat (Emulate A Terminal).
- `term': Use built-in `term-mode' terminal emulator."
  :type '(choice (const :tag "Auto-detect (vterm > eat > term)" nil)
                 (const :tag "Vterm (requires vterm package)" vterm)
                 (const :tag "Eat (requires eat package)" eat)
                 (const :tag "Term mode (built-in)" term))
  :group 'beads-terminal)

;;; Abstract Base Class

(defclass beads-terminal ()
  ((name
    :initarg :name
    :type string
    :documentation "Human-readable terminal name (e.g., \"vterm\").")
   (priority
    :initarg :priority
    :type integer
    :initform 50
    :documentation "Auto-selection priority (lower = preferred).
`beads-terminal-auto' walks registered terminals in ascending
priority order and spawns into the first available one."))
  :abstract t
  :documentation "Abstract base class for terminal emulators.
Concrete subclasses wrap a specific terminal package's spawn
mechanism.  Register instances with `beads-terminal-register'.")

;;; Protocol

(cl-defgeneric beads-terminal-available-p (terminal)
  "Return non-nil if TERMINAL's underlying package is usable now.
Must be conservative: only return non-nil when a real spawn would
succeed (package loaded AND required symbols bound).")

(cl-defgeneric beads-terminal-spawn (terminal buffer-name argv working-dir env)
  "Spawn ARGV in a TERMINAL buffer named BUFFER-NAME.
ARGV is a list whose car is the program and cdr the arguments.
WORKING-DIR is the cwd; it is established by `let'-binding
`default-directory' around the package's process-spawning call —
beads adds no `cd … ; exit' wrapper of its own.
ENV is an alist of (NAME . VALUE) strings *added to* (not replacing)
`process-environment'.  TERM is never set via ENV; the terminal
package owns TERM/terminfo.
Returns the live spawn buffer.  The buffer name is owned from spawn
\(no rename), so the caller need not rely on any post-spawn rename.")

(cl-defgeneric beads-terminal-send-input (terminal buffer text)
  "Send TEXT to the process in TERMINAL's BUFFER.
Deferred in this release; the default signals an error.  The real
future seam is `vterm-send-string' / `term-send-string' / etc."
  (ignore terminal buffer text)
  (error "Stdin delivery not implemented in this release"))

;;; Helpers

(defun beads-terminal--apply-env (env)
  "Return a `process-environment' with ENV alist entries prepended.
ENV is an alist of (NAME . VALUE).  A NAME of \"TERM\" is dropped
\(the terminal package owns TERM/terminfo)."
  (append
   (delq nil
         (mapcar (lambda (cell)
                   (let ((name (car cell)) (val (cdr cell)))
                     (unless (equal name "TERM")
                       (format "%s=%s" name val))))
                 env))
   process-environment))

(defun beads-terminal--kill-stale-process (buf)
  "Delete any live process in BUF so a fresh exec can take over."
  (when-let ((proc (get-buffer-process buf)))
    (when (process-live-p proc)
      (delete-process proc))))

;;; Concrete: vterm (priority 10)

(defclass beads-terminal-vterm (beads-terminal)
  ((name :initform "vterm")
   (priority :initform 10))
  :documentation "vterm (libvterm) terminal.  Highest priority.")

(cl-defmethod beads-terminal-available-p ((_t beads-terminal-vterm))
  "Return non-nil when the vterm package is loadable."
  (and (require 'vterm nil t) t))

(cl-defmethod beads-terminal-spawn ((_t beads-terminal-vterm)
                                    buffer-name argv working-dir env)
  "Spawn ARGV via vterm into BUFFER-NAME (mirrors `--run-vterm').
Name ownership is via the dynamic `vterm-buffer-name'; vterm makes
its own buffer (do NOT `get-buffer-create' first)."
  (unless (require 'vterm nil t)
    (user-error "Vterm package not installed"))
  (let* ((default-directory working-dir)
         (process-environment (beads-terminal--apply-env env))
         (vterm-shell (mapconcat #'shell-quote-argument argv " "))
         (vterm-buffer-name buffer-name)
         (buf (vterm buffer-name)))
    ;; Long-lived process: keep the buffer after exit (the sentinel
    ;; reads this buffer-local value when the process terminates).
    (with-current-buffer buf
      (setq-local vterm-kill-buffer-on-exit nil))
    buf))

;;; Concrete: ghostel (priority 5) — strict availability

;; Priority 5 (below vterm's 10) makes ghostel the first choice for
;; the `auto' terminal: ghostel -> vterm -> eat -> ansi-term -> term.
(defclass beads-terminal-ghostel (beads-terminal)
  ((name :initform "ghostel")
   (priority :initform 5))
  :documentation "ghostel (libghostty-vt) terminal.
Availability is strict: the elisp can be present while the native
module is absent, so `beads-terminal-available-p' requires every
needed symbol to be bound, not merely `featurep'.")

(defun beads-terminal--ghostel-functional-p ()
  "Return non-nil only when ghostel is genuinely usable.
Requires the feature AND every symbol the spawn path touches to be
bound (the native module is loaded at runtime via
`ghostel-download-module', so the elisp alone is not enough).
The spawn path uses `ghostel-exec' (ghostel's public exec API) and
overrides `ghostel-kill-buffer-on-exit' buffer-locally."
  (and (featurep 'ghostel)
       (boundp 'ghostel-kill-buffer-on-exit)
       (fboundp 'ghostel-exec)))

(cl-defmethod beads-terminal-available-p ((_t beads-terminal-ghostel))
  "Return non-nil only when ghostel is fully functional."
  (beads-terminal--ghostel-functional-p))

(cl-defmethod beads-terminal-spawn ((_t beads-terminal-ghostel)
                                    buffer-name argv working-dir env)
  "Spawn ARGV via ghostel into BUFFER-NAME (mirrors `--run-eat').
Uses `ghostel-exec' — ghostel's public exec API — exactly as the
eat/term backends use `eat-exec'/`term-exec': PROGRAM is the argv
head, ARGS its tail.  `ghostel-shell' must NOT be used here: it is
a single interactive-shell PROGRAM path, so feeding it a joined
command line makes ghostel exec a program literally named
\"claude --append-system-prompt ...\", which fails instantly.
Name ownership is via the pre-named `get-buffer-create' buffer
passed to `ghostel-exec'.  Signals an actionable `user-error' when
the native module is missing, mirroring `--run-vterm''s error."
  (unless (beads-terminal--ghostel-functional-p)
    (user-error
     "Ghostel native module not loaded.  Run: M-x ghostel-download-module"))
  (let* ((default-directory working-dir)
         (process-environment (beads-terminal--apply-env env))
         (buf (get-buffer-create buffer-name)))
    ;; ghostel defaults `ghostel-kill-buffer-on-exit' to t and
    ;; `ghostel--sentinel' kills the buffer on process exit (it reads
    ;; the value buffer-locally via `with-current-buffer').  Keep a
    ;; long-lived agent buffer alive race-free with two layers:
    ;;  1. Dynamically bind the GLOBAL to nil across `ghostel-exec' so
    ;;     a process that exits during spawn — before any buffer-local
    ;;     can exist — has the sentinel fall back to this nil.
    ;;  2. `ghostel-exec' switches major mode, and `ghostel-mode'
    ;;     (a `define-derived-mode') runs `kill-all-local-variables',
    ;;     which would wipe a buffer-local set beforehand.  So set the
    ;;     buffer-local AFTER it returns; this persists for the buffer's
    ;;     life and covers the normal async exit.
    (let ((ghostel-kill-buffer-on-exit nil))
      (ghostel-exec buf (car argv) (cdr argv)))
    (with-current-buffer buf
      (setq-local ghostel-kill-buffer-on-exit nil))
    buf))

;;; Concrete: eat (priority 20)

(defclass beads-terminal-eat (beads-terminal)
  ((name :initform "eat")
   (priority :initform 20))
  :documentation "Eat (Emulate A Terminal), pure-elisp terminal.")

(cl-defmethod beads-terminal-available-p ((_t beads-terminal-eat))
  "Return non-nil when the eat package is loadable."
  (and (require 'eat nil t) t))

(cl-defmethod beads-terminal-spawn ((_t beads-terminal-eat)
                                    buffer-name argv working-dir env)
  "Spawn ARGV via eat into BUFFER-NAME (mirrors `--run-eat').
Name ownership is via the pre-named `get-buffer-create' buffer
passed to `eat-exec'.  `eat-mode' is ensured FIRST (mandatory —
`eat-exec' on a fundamental buffer won't wire the display)."
  (unless (require 'eat nil t)
    (user-error "Eat package not installed"))
  (let* ((default-directory working-dir)
         (process-environment (beads-terminal--apply-env env))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'eat-mode)
        (eat-mode))
      (beads-terminal--kill-stale-process buf)
      (eat-exec buf buffer-name (car argv) nil (cdr argv))
      (setq-local eat-kill-buffer-on-exit nil))
    buf))

;;; Concrete: ansi-term (priority 40)

(defclass beads-terminal-ansi-term (beads-terminal)
  ((name :initform "ansi-term")
   (priority :initform 40))
  :documentation "Built-in `term-mode' terminal via `term-exec'.")

(defun beads-terminal--term-spawn (buffer-name argv working-dir env)
  "Spawn ARGV in a `term-mode' BUFFER-NAME (mirrors `--run-term').
Order matters: mode -> exec -> char-mode (switching mode after exec
detaches the filter).  `term-sentinel' does not kill the buffer, so
the buffer survives process exit with no extra knob."
  (require 'term)
  (let* ((default-directory working-dir)
         (process-environment (beads-terminal--apply-env env))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'term-mode)
        (term-mode))
      (beads-terminal--kill-stale-process buf)
      (erase-buffer)
      (term-exec buf buffer-name (car argv) nil (cdr argv))
      (term-char-mode))
    buf))

(cl-defmethod beads-terminal-available-p ((_t beads-terminal-ansi-term))
  "Always available — `term' is built in."
  t)

(cl-defmethod beads-terminal-spawn ((_t beads-terminal-ansi-term)
                                    buffer-name argv working-dir env)
  "Spawn ARGV via built-in `term-exec' into BUFFER-NAME."
  (beads-terminal--term-spawn buffer-name argv working-dir env))

;;; Concrete: term (priority 50) — the absolute built-in floor

(defclass beads-terminal-term (beads-terminal)
  ((name :initform "term")
   (priority :initform 50))
  :documentation "Built-in terminal floor; identical path to ansi-term.
`term' and `ansi-term' differ only as labels here; `term' is the
absolute always-available fallback (char-mode `term-exec').")

(cl-defmethod beads-terminal-available-p ((_t beads-terminal-term))
  "Always available — `term' is built in."
  t)

(cl-defmethod beads-terminal-spawn ((_t beads-terminal-term)
                                    buffer-name argv working-dir env)
  "Spawn ARGV via built-in `term-exec' into BUFFER-NAME."
  (beads-terminal--term-spawn buffer-name argv working-dir env))

;;; Registry

(defvar beads-terminal--registry nil
  "Hash table mapping terminal names (strings) to instances.
Use `beads-terminal-register', `beads-terminal-get', and
`beads-terminal-list' to access.  `beads-terminal--clear-registry'
sets it to nil; `beads-terminal--ensure-registry' reallocates it
lazily — the save-pointer / clear / restore test idiom relies on
this (a fresh table per clear, never an in-place mutation).")

(defun beads-terminal--ensure-registry ()
  "Ensure the terminal registry hash table exists."
  (unless beads-terminal--registry
    (setq beads-terminal--registry (make-hash-table :test #'equal))))

(defun beads-terminal--clear-registry ()
  "Drop the terminal registry (a fresh table is allocated lazily)."
  (setq beads-terminal--registry nil))

(defun beads-terminal-register (terminal)
  "Register TERMINAL instance under its `name' slot."
  (beads-terminal--ensure-registry)
  (puthash (oref terminal name) terminal beads-terminal--registry)
  terminal)

(defun beads-terminal-get (name)
  "Return the registered terminal named NAME, or nil."
  (beads-terminal--ensure-registry)
  (gethash name beads-terminal--registry))

(defun beads-terminal-list ()
  "Return registered terminals sorted by ascending `priority'."
  (beads-terminal--ensure-registry)
  (let (terminals)
    (maphash (lambda (_k v) (push v terminals)) beads-terminal--registry)
    (sort terminals (lambda (a b)
                      (< (oref a priority) (oref b priority))))))

;;; Auto terminal — walks priority order

(defclass beads-terminal-auto (beads-terminal)
  ((name :initform "auto")
   (priority :initform 0))
  :documentation "Meta-terminal: delegates to the lowest-priority
available registered concrete terminal.  Never spawns itself.")

(defun beads-terminal--first-available ()
  "Return the lowest-priority available concrete terminal, or nil.
`beads-terminal-auto' itself is skipped to avoid recursion."
  (cl-find-if (lambda (term)
                (and (not (cl-typep term 'beads-terminal-auto))
                     (beads-terminal-available-p term)))
              (beads-terminal-list)))

(cl-defmethod beads-terminal-available-p ((_t beads-terminal-auto))
  "Return non-nil iff at least one concrete terminal is available."
  (and (beads-terminal--first-available) t))

(cl-defmethod beads-terminal-spawn ((_t beads-terminal-auto)
                                    buffer-name argv working-dir env)
  "Delegate to the lowest-priority available concrete terminal.
BUFFER-NAME, ARGV, WORKING-DIR and ENV are forwarded unchanged.
Signals a clear `error' when every registered concrete is
unavailable."
  (let ((term (beads-terminal--first-available)))
    (unless term
      (error "No terminal backend available \
(install vterm/eat or use the built-in term)"))
    (beads-terminal-spawn term buffer-name argv working-dir env)))

;;; Symbol -> class bridge (Phase 3 mechanical unification)

(defun beads-terminal--symbol->class (symbol)
  "Map a legacy `beads-terminal-backend' SYMBOL to a class symbol.
nil -> `beads-terminal-auto'; vterm/eat/term -> the matching
concrete.  Used so the Phase 3 runner unification is mechanical."
  (pcase symbol
    ('nil 'beads-terminal-auto)
    ('vterm 'beads-terminal-vterm)
    ('eat 'beads-terminal-eat)
    ('term 'beads-terminal-term)
    (_ (error "Unknown beads-terminal-backend symbol: %S" symbol))))

;;; Built-in registration

(defvar beads-terminal--builtin-registered nil
  "Non-nil once the built-in terminals have been registered.")

(defun beads-terminal-register-builtin ()
  "Register all built-in terminals.  Idempotent."
  (unless beads-terminal--builtin-registered
    (beads-terminal-register (beads-terminal-auto))
    (beads-terminal-register (beads-terminal-vterm))
    (beads-terminal-register (beads-terminal-ghostel))
    (beads-terminal-register (beads-terminal-eat))
    (beads-terminal-register (beads-terminal-ansi-term))
    (beads-terminal-register (beads-terminal-term))
    (setq beads-terminal--builtin-registered t)))

(beads-terminal-register-builtin)

(provide 'beads-terminal)
;;; beads-terminal.el ends here
