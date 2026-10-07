;;; beads-terminal-tmux.el --- Terminal backend + tmux attach -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of beads.el.

;;; Commentary:

;; Interactive-command support for beads, built on beads.el's terminal
;; module.  `beads-terminal-spawn' provides the vterm / eat / term
;; backends; this module selects one via `beads-terminal-tmux-backend'
;; and hands it an argv to run.
;;
;; The one action the read-only porcelain needs is attaching to an
;; agent's tmux session: `beads-terminal-tmux-attach' opens `env -u
;; TMUX tmux attach-session -t SESSION' in a terminal buffer.  (`env -u
;; TMUX' lets the attach nest when Emacs itself runs inside tmux.)
;; Session and socket names are shell-quoted before interpolation.
;;
;; Nothing here blocks Emacs (dashboard-v3 D9, §8.3 R2): every tmux call
;; the attach and the status mirror make is ONE asynchronous host script
;; (`beads-terminal-tmux--run-async') — a local `sh -c' for a local store, a
;; local no-pty `ssh -T' pipe for a remote one (beads' ControlMaster,
;; the pure PATH fragment, so a Guix profile tmux is found), never
;; `process-file' over TRAMP.  The attach runs one pre-step on the host
;; (`beads-terminal-tmux--attach-script'): does the session exist, where
;; is tmux, does the host have terminfo for the TERM the local backend
;; advertises, does the agent's directory exist, and turn the session's
;; status bar off — then opens the terminal from its callback.  The
;; terminal itself is a LOCAL process (beads.el's local-argv contract):
;; for a remote city the tmux command is wrapped into a local `ssh -t
;; HOST …' argv (`beads-terminal-tmux--attach-argv' via
;; `beads-remote-ssh-argv') carrying the tmux path the pre-step found
;; — `ssh HOST cmd' is no login shell, so profile PATHs may be absent
;; there.  When the host lacks the backend TERM's terminfo the remote
;; command forces `beads-terminal-tmux-remote-term' via its env prefix —
;; host-side only, the local terminal's TERM is never touched (gce-25q).
;; The status-mirror timer runs in the terminal buffer, whose
;; `default-directory' is LOCAL (it hosts a local ssh); the remote
;; context is carried buffer-locally (`beads-terminal-tmux--status-directory').
;; The synchronous helpers (`beads-terminal-tmux--tmux',
;; `beads-terminal-tmux-session-exists-p', `beads-terminal-tmux-pane-cwd')
;; remain for user-initiated one-offs such as Dired's pane-cwd fallback.
;;
;; The attach buffer is also a place the user READS bead ids — agent
;; transcripts are full of them — so it is wired for beads.el's eldoc
;; (`beads-terminal-tmux--beads-integrate'): the id-prefix allowlist and a
;; (`beads-terminal-tmux--beads-integrate').  Its pinned remote
;; `default-directory' would otherwise send `project-mode-line' up the
;; host's directory tree on every redisplay, so the attach keeps the
;; buffer's `default-directory' local wherever the pre-step allows it.
;;
;; Keys belong to the pty: a terminal buffer is a full-screen program,
;; and the backend's own keymap is only the buffer's LOCAL map, which
;; every enabled minor-mode map outranks.  A global minor mode binding
;; the same key wins and the program never sees it — with
;; `pixel-scroll-precision-mode' on, PageUp/PageDown scroll the Emacs
;; window (which shows only the visible screen, tmux being on the
;; alternate screen) instead of paging tmux's copy-mode.
;; `beads-terminal-tmux--unshadow-keys' hands each mode in
;; `beads-terminal-tmux-unshadow-minor-modes' an empty keymap through the
;; buffer's `minor-mode-overriding-map-alist', so the backend's
;; forwarding wins in beads terminals and nowhere else.
;;
;; Scrolling is explicit, not a shadowing of live keys (D1): `C-c s'
;; toggles `beads-terminal-tmux-scroll-mode', a buffer-local sub-mode that
;; enters tmux copy mode (`C-b [', the only bytes the agent sees on
;; entry) and translates the Emacs scroll keys of its map to copy-mode
;; byte sequences through the pure table
;; `beads-terminal-tmux--scroll-sequence', sent by the per-backend
;; raw-key adapter `beads-terminal-tmux--send-raw' (vterm, term, eat,
;; ghostel with a control-byte/escape-sequence split — E6).  The agent
;; receives no keys while the mode is on (E9); q/Esc leave both.  The
;; toggle is optimistic + self-healing: a re-toggle sends q first,
;; then re-enters.  On backends that do not report the mouse (vterm,
;; term) a wheel-only minor mode is armed on attach — no `C-c s'
;; needed: a notch is injected as the SGR mouse event tmux would have
;; received, so tmux's own copy-mode handling runs (D2, ga-eqpxs),
;; while the explicit scroll mode keeps the key translation.  On
;; ghostel/eat the native tmux passthrough wins, so no wheel mode is
;; armed.  The attach pre-step, one
;; async round trip, also ensures the session's tmux `mouse' option
;; and a copy-mode wheel-to-bottom binding (D3), restored on teardown
;; (`beads-terminal-tmux-ensure-mouse'), and the status mirror's segment
;; shows a [scroll] marker while the sub-mode is active.
;;
;; Attaching is idempotent: when the agent's terminal buffer is already
;; open with a live process, `beads-terminal-tmux-run' raises that window
;; instead of starting a second backend process in it (which would
;; otherwise error, e.g. ghostel's "already has a running ghostel
;; process").
;;
;; Mouse scrolling (DESIGN-agent-scrolling.md D2/D3): the same pre-step
;; also turns the session's tmux `mouse' option on (session-scoped) and
;; installs one copy-mode `WheelDownPane' binding that leaves copy mode
;; when it is already at the bottom — wheeling through the transcript
;; ends back at the live tail.  Both ride the pre-step's one host round
;; trip, and the teardown (the kill-buffer hook below) restores them
;; alongside the `status' override, so an external `tmux attach' sees
;; tmux's defaults.  Gated by `beads-terminal-tmux-ensure-mouse'.
;;
;; One status line, not two: a tmux client inside an Emacs buffer shows
;; both tmux's own status bar and the Emacs mode line.  On attach,
;; `beads-terminal-tmux--status-install' turns the session's tmux status bar
;; off (scoped to that session) and mirrors its information — the friendly
;; name from `status-left' and the window list — in a buffer-local mode
;; line segment, refreshed on a timer.  Killing the buffer cancels the
;; timer and removes the tmux override (`set-option -u'), so an external
;; `tmux attach' sees its bar again.  Honour `beads-terminal-tmux-mode-line-status'
;; to disable the whole behaviour.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'beads-terminal)
(require 'beads-remote)

;; The scroll sub-mode's per-backend raw-key adapters (see the scroll
;; section below).  Each is called only when the buffer's major mode is
;; that backend's, which means the package is loaded; the declares only
;; satisfy the byte-compile gate (soft `require' would load the world).
(declare-function vterm-send-string "vterm" (string &optional use-all-inputs))
(declare-function term-send-raw-string "term" (string))
(declare-function eat-self-input "eat" (n &optional e))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function beads-show-at-point "beads-command-show" ())

;; beads.el's buffer-local eldoc contract.  Both are referenced by name
;; and `boundp'-guarded, so this file byte-compiles with
;; `--warnings-as-errors'.
(defvar beads-eldoc-directory)
(defvar beads-issue-id-prefixes)

;;; Customization

(defcustom beads-terminal-tmux-backend nil
  "Terminal backend for attaching to a tmux session.
The actual spawn is delegated to beads.el's terminal module
\(`beads-terminal-spawn'); this choice selects which backend class it
uses.  When nil, auto-detect the best available backend: ghostel when
it is installed (attempted/loaded, not merely already `featurep' — see
`beads-terminal-tmux--ghostel-available-p'), otherwise vterm > eat > term.

- nil:   auto-detect (ghostel when installed, then vterm, eat, term).
- vterm: requires the `vterm' package.
- eat:   requires the `eat' package.
- term:  the built-in `term-mode' (always available)."
  :type '(choice (const :tag "Auto-detect (ghostel > vterm > eat > term)" nil)
                 (const :tag "Vterm (requires vterm package)" vterm)
                 (const :tag "Eat (requires eat package)" eat)
                 (const :tag "Term mode (built-in)" term))
  :group 'beads-terminal)

(defcustom beads-terminal-tmux-remote-term "xterm-256color"
  "Fallback TERM for the remote side of a tmux attach, or nil for none.
A remote attach runs `ssh -t HOST … tmux attach …', and ssh forwards
the TERM the local terminal backend advertises (e.g. ghostel's
\"xterm-ghostty\").  A host with no terminfo entry for that name makes
the remote tmux client exit instantly; when the host appears to lack
the entry (a best-effort probe, `beads-remote-terminfo-p'), this TERM
is forced onto the remote command line instead.  Purely local attaches
never touch TERM — the terminal backend owns it."
  :type '(choice (const :tag "Never force a TERM" nil)
                 (string :tag "TERM name"))
  :group 'beads-terminal)

(defcustom beads-terminal-tmux-unshadow-minor-modes '(pixel-scroll-precision-mode)
  "Global minor modes whose keymaps are neutralised in terminal buffers.
Each named mode is given an empty keymap in the terminal buffer's
`minor-mode-overriding-map-alist', so its bindings are suppressed in
that buffer only and the backend's own key forwarding wins.  Set to nil
to touch no keymaps at all."
  :type '(repeat symbol)
  :group 'beads-terminal)

(defcustom beads-terminal-tmux-mode-line-status t
  "When non-nil, mirror a tmux session's status bar in the mode line.
On attaching, the session's own tmux status bar is turned off (scoped
to the session) and the same information — the friendly session name
from `status-left' and the window list — is rendered as a buffer-local
mode-line segment.  The change is reverted when the terminal buffer is
killed.  Set to nil to leave tmux's status bar untouched."
  :type 'boolean
  :group 'beads-terminal)

(defcustom beads-terminal-tmux-ensure-mouse t
  "When non-nil, ensure tmux mouse scrolling for attached sessions.
The attach pre-step turns the session's tmux `mouse' option on
\(session-scoped) and installs a copy-mode `WheelDownPane' binding that
leaves copy mode when it is already at the bottom.  Killing the
terminal buffer restores both.  Set to nil to leave tmux's mouse
configuration untouched."
  :type 'boolean
  :group 'beads-terminal)

(defcustom beads-terminal-tmux-preload-idle 10
  "Seconds of idle before the terminal backend preload, or nil to disable.
The preload loads the backend library ahead of the first attach, on
genuine idle time, so the one-off cost is not paid while typing."
  :type '(choice (number :tag "Seconds") (const :tag "Never" nil))
  :group 'beads-terminal)

(defcustom beads-terminal-tmux-status-interval 5
  "Seconds between refreshes of the tmux status mode-line segment.
Only consulted when `beads-terminal-tmux-mode-line-status' is non-nil;
values at or below zero fall back to 5."
  :type 'number
  :group 'beads-terminal)

(defface beads-terminal-tmux-session-face
  '((t :inherit bold))
  "Face for the session name in a tmux attach status segment."
  :group 'beads-terminal)

(defface beads-terminal-tmux-active-window-face
  '((t :inherit highlight))
  "Face for the active window in a tmux attach status segment."
  :group 'beads-terminal)

;;; Backend selection

(defun beads-terminal-tmux--ghostel-available-p ()
  "Return non-nil when ghostel is installed and can actually be used.
beads.el's own check for ghostel is strict about the package being
ALREADY loaded (`featurep'), so an `auto' backend would resolve
differently depending on whether some earlier command happened to load
ghostel — the load-order bug this fixes (ga-eqpxs).  Probing here,
from a local `default-directory' (never a TRAMP one), makes the choice
deterministic: ghostel when it is installed, otherwise the beads.el
priority walk.  Beads owns this policy (gce-25q/ga-eqpxs)."
  (let ((default-directory (file-name-as-directory temporary-file-directory)))
    (and (ignore-errors (require 'ghostel nil t))
         (ignore-errors
           (beads-terminal-available-p
            (make-instance 'beads-terminal-ghostel))))))

(defun beads-terminal-tmux--backend-class ()
  "Return the `beads-terminal' class for `beads-terminal-tmux-backend'.
Maps the user's backend choice to a concrete beads terminal class.
Unset means auto: ghostel when it is installed (probed, not merely
`featurep' — see `beads-terminal-tmux--ghostel-available-p'), otherwise
`beads-terminal-auto' (which walks vterm > eat > term)."
  (pcase beads-terminal-tmux-backend
    ('vterm 'beads-terminal-vterm)
    ('eat   'beads-terminal-eat)
    ('term  'beads-terminal-term)
    (_      (if (beads-terminal-tmux--ghostel-available-p)
                'beads-terminal-ghostel
              'beads-terminal-auto))))

(defun beads-terminal-tmux--client-term ()
  "Return the TERM the selected terminal backend advertises, or nil.
This is the value ssh forwards on a remote attach: the backend exports
TERM to the processes it spawns, and beads.el's env contract keeps
callers from overriding it locally.  The `auto' backend resolves to
the first available concrete terminal exactly as `beads-terminal-spawn'
would (priority order over `beads-terminal-list').  Each backend's
TERM comes from its own variable when bound, else from its documented
default — the package can be available yet not loaded.  Returns nil
for a backend this map does not know; callers treat that as \"assume
the host cannot render it\" and force the fallback."
  (let* ((class (beads-terminal-tmux--backend-class))
         (terminal
          (if (eq class 'beads-terminal-auto)
              (cl-find-if (lambda (term)
                            (and (not (cl-typep term 'beads-terminal-auto))
                                 (beads-terminal-available-p term)))
                          (beads-terminal-list))
            (make-instance class))))
    (pcase (and terminal (oref terminal name))
      ("ghostel" (or (bound-and-true-p ghostel-term) "xterm-ghostty"))
      ("vterm" (or (bound-and-true-p vterm-term-environment-variable)
                   "xterm-256color"))
      ("eat" (or (bound-and-true-p eat-term-name) "xterm-256color"))
      ((or "term" "ansi-term") (or (bound-and-true-p term-term-name)
                                   "eterm-color"))
      (_ nil))))

(defun beads-terminal-tmux--remote-term (&optional dir)
  "Return the TERM to force on DIR's host in a remote attach, or nil.
Nil means keep the TERM ssh forwards natively, because the fallback is
off (`beads-terminal-tmux-remote-term' nil), the backend already
advertises the fallback (forcing would change nothing — no probe is
made), or DIR's host has terminfo for the backend's TERM
\(`beads-remote-terminfo-p').  An unknown backend TERM forces the
fallback without probing — the safe default: a missing entry kills the
attach outright (gce-25q), a needlessly forced fallback merely narrows
the capabilities tmux sees."
  (when-let* ((fallback beads-terminal-tmux-remote-term))
    (let ((client (beads-terminal-tmux--client-term)))
      (unless (or (equal client fallback)
                  (and client (beads-remote-terminfo-p client dir)))
        fallback))))

;;; Running a command in a terminal

(defun beads-terminal-tmux--working-dir (dir)
  "Return a usable working directory from DIR, falling back to \"~/\"."
  (let ((d (or dir default-directory)))
    (if (and d (file-directory-p d)) (file-name-as-directory d) "~/")))

(defun beads-terminal-tmux--live-buffer (buffer-name)
  "Return the buffer named BUFFER-NAME when it hosts a live process, else nil.
A terminal buffer whose process has already exited is treated as absent,
so the caller spawns a fresh one; only a buffer with a running process is
worth reusing."
  (when-let* ((buf (get-buffer buffer-name))
              (proc (get-buffer-process buf)))
    (and (process-live-p proc) buf)))

(defun beads-terminal-tmux--unshadow-keys (buffer)
  "Suppress `beads-terminal-tmux-unshadow-minor-modes' inside BUFFER.
Gives each named minor mode an empty keymap in BUFFER's
`minor-mode-overriding-map-alist', so keys the mode binds globally reach
the terminal instead — the backend forwards them from the buffer's local
map, which a minor-mode map would otherwise outrank.  Buffer-local, so
the modes are untouched everywhere else.

Idempotent, and conservative: an entry another package already made for
the same mode is left as it found it.  A nil option, a dead BUFFER, or a
non-symbol entry is a no-op."
  (when (and (buffer-live-p buffer) beads-terminal-tmux-unshadow-minor-modes)
    (with-current-buffer buffer
      (dolist (mode beads-terminal-tmux-unshadow-minor-modes)
        (when (and mode (symbolp mode)
                   (not (assq mode minor-mode-overriding-map-alist)))
          (setq-local minor-mode-overriding-map-alist
                      (cons (cons mode (make-sparse-keymap))
                            minor-mode-overriding-map-alist))))))
  buffer)

(defun beads-terminal-tmux-run (argv buffer-name &optional dir)
  "Display the terminal buffer named BUFFER-NAME, spawning ARGV if needed.
If a buffer named BUFFER-NAME already hosts a live process, reuse it: pop
to it and raise its window without launching a second process.  This is
what keeps the t key on an agent whose terminal is already open from erroring
\(e.g. ghostel's \"already has a running ghostel process\").  The check is
on the Emacs buffer and its process, so it behaves identically across the
vterm / eat / term / ghostel backends.

Otherwise spawn ARGV in a fresh terminal buffer.  ARGV is a (PROGRAM .
ARGS) list run with no intervening shell via beads.el's
`beads-terminal-spawn', using the backend from `beads-terminal-tmux-backend'.
beads sets the working directory from DIR (its WORKING-DIR contract); nil
or a missing DIR falls back to the home directory.  Returns the buffer and
pops to it.

Either way the buffer is unshadowed (`beads-terminal-tmux--unshadow-keys')
so keys the terminal should own are not swallowed by a global minor
mode."
  (let ((existing (beads-terminal-tmux--live-buffer buffer-name)))
    (if existing
        ;; Reuse the live terminal — raise its window, don't re-exec.
        (progn (beads-terminal-tmux--unshadow-keys existing)
               (pop-to-buffer existing)
               existing)
      ;; No live terminal: spawn a fresh one.
      (let* ((default-dir (beads-terminal-tmux--working-dir dir))
             (terminal (make-instance (beads-terminal-tmux--backend-class)))
             (buf (beads-terminal-spawn terminal buffer-name argv default-dir
                                        '(("CLICOLOR_FORCE" . "1")))))
        (when (and buf (buffer-live-p buf))
          (beads-terminal-tmux--unshadow-keys buf)
          (pop-to-buffer buf))
        buf))))

;;; tmux

(defun beads-terminal-tmux--socket-args (socket)
  "Return a `(\"-L\" SOCKET)' list for a real SOCKET, else nil.
nil, the empty string, and the literal \"default\" all mean \"use the
default tmux server\" — no -L flag."
  (when (and socket (stringp socket)
             (not (string-empty-p socket))
             (not (string= socket "default")))
    (list "-L" socket)))

(defun beads-terminal-tmux-session-exists-p (session &optional socket)
  "Return non-nil when tmux SESSION exists (on optional SOCKET).
Probes via `process-file', so on a remote `default-directory' the
city's own tmux server is asked, on its host — tmux resolved there by
`beads-remote-find-executable'.  Signals a `file-error' when tmux
itself cannot be run there (callers that need a clean message wrap
this — see `beads-terminal-tmux-attach').  On a remote directory the
probe is bounded by `beads-remote-sync-timeout'
\(`beads-remote-with-timeout'): a wedged channel answers nil —
uniformly with the other failure modes — instead of hanging forever;
the local probe never needs the bound (no network, blocking C code that
runs no timers)."
  (and session (stringp session) (not (string-empty-p session))
       (condition-case nil
           (beads-remote-with-timeout beads-remote-sync-timeout
             (eq 0 (apply #'beads-remote-call-unshared #'process-file (beads-remote-find-executable "tmux")
                          nil nil nil
                          (append (beads-terminal-tmux--socket-args socket)
                                  (list "has-session" "-t" session)))))
         ;; A wedged channel degrades to "does not exist" — uniformly
         ;; with the non-zero-exit and spawn-failure answers — instead of
         ;; signalling into attach/peek call sites that expect a boolean.
         (beads-remote-timeout nil))))

(defun beads-terminal-tmux-pane-cwd (session &optional socket)
  "Return the working directory of tmux SESSION's active pane, or nil.
Runs `tmux [-L SOCKET] display-message -t SESSION -p #{pane_current_path}'
— via `process-file', so a remote city's tmux answers on its own host,
reporting a host-local path (the caller localizes it) — and returns the
trimmed path.  Returns nil when SESSION is empty, tmux is unavailable,
the session is gone, or the pane reports no path; the caller validates
that the path exists on disk.  This lets `dired' open an
agent's live working directory even when its session bead recorded no
`work_dir' (mirroring gastown).  A remote probe is bounded by
`beads-remote-sync-timeout': a timeout answers nil, uniformly with
the other failure modes, instead of hanging on a dead channel."
  (when (and session (stringp session) (not (string-empty-p session)))
    (with-temp-buffer
      (when (eq 0 (condition-case nil
                      (beads-remote-with-timeout
                          beads-remote-sync-timeout
                        (apply #'beads-remote-call-unshared #'process-file
                               (beads-remote-find-executable "tmux")
                               nil t nil
                               (append (beads-terminal-tmux--socket-args socket)
                                       (list "display-message" "-t" session
                                             "-p" "#{pane_current_path}"))))
                    (file-error nil)
                    (beads-remote-timeout nil)))
        (let ((path (string-trim (buffer-string))))
          (unless (string-empty-p path) path))))))

;;; Host commands without TRAMP (dashboard-v3 D9, §8.3 R2/R4)

;; Every tmux call of the attach pre-step and the status mirror runs as
;; ONE asynchronous local process: `sh -c SCRIPT' for a local city, and
;; for a remote (ssh-family) city a local `ssh -T' pipe to the host
;; (`beads-remote-ssh-pipe-argv', beads' own ControlMaster, the pure
;; PATH fragment so a Guix profile tmux is found) — never `process-file'
;; over TRAMP, which blocks the command loop (and, from a timer, can
;; wedge it).  The callback runs from `run-at-time' 0, never inside the
;; sentinel.

(defun beads-terminal-tmux--sh (&rest words)
  "Return WORDS joined into one sh command line, each shell-quoted.
A word that is a cons (:raw . STRING) is spliced unquoted."
  (mapconcat (lambda (w) (if (and (consp w) (eq (car w) :raw))
                             (cdr w)
                           (shell-quote-argument w)))
             words " "))

(defun beads-terminal-tmux--tmux-sh (socket &rest args)
  "Return an sh command line running tmux [-L SOCKET] ARGS (quoted)."
  (apply #'beads-terminal-tmux--sh
         (append (list "tmux") (beads-terminal-tmux--socket-args socket) args)))

(defun beads-terminal-tmux--host-argv (dir script)
  "Return the local argv running sh SCRIPT on DIR's host.
Locally `sh -c SCRIPT'; for a remote DIR a no-pty ssh to the host
\(no host resolution: nothing here touches TRAMP)."
  (let ((argv (list "sh" "-c" script)))
    (if (beads-remote-prefix dir)
        (beads-remote-ssh-pipe-argv dir argv (beads-remote-pure-path-assignment))
      argv)))

(defun beads-terminal-tmux--run-async (dir script callback)
  "Run sh SCRIPT on DIR's host asynchronously; call CALLBACK when done.
CALLBACK receives (EXIT . STDOUT): EXIT the exit status, or nil when the
process could not start or was killed at its deadline
\(`beads-remote-async-timeout').  The process is local (a local shell,
or a local ssh for a remote DIR) and is started from a local directory,
so starting it does no remote I/O.  Returns the process, or nil."
  (let* ((out "")
         (done nil)
         (timer nil)
         (finish (lambda (exit)
                   (unless done
                     (setq done t)
                     (cancel-timer timer)
                     (let ((result (cons exit out)))
                       ;; From the sentinel: a plain timer here can be
                       ;; lost to TRAMP's timer suspension (B1).
                       (run-at-time 0 nil callback result)))))
         (proc (condition-case err
                   (let ((default-directory
                          (if (beads-remote-prefix dir)
                              (file-name-as-directory temporary-file-directory)
                            dir)))
                     (make-process
                      :name "beads-tmux"
                      :command (beads-terminal-tmux--host-argv dir script)
                      :connection-type 'pipe
                      :noquery t
                      :file-handler nil
                      :stderr (get-buffer-create " *beads-tmux-stderr*")
                      :filter (lambda (_p chunk) (setq out (concat out chunk)))
                      :sentinel (lambda (p _event)
                                  (unless (process-live-p p)
                                    (funcall finish (process-exit-status p))))))
                 (error
                  (setq out (error-message-string err))
                  (funcall finish nil)
                  nil))))
    (when (and proc (not done)
               (numberp beads-remote-async-timeout)
               (> beads-remote-async-timeout 0))
      (setq timer (run-at-time beads-remote-async-timeout nil
                               (lambda ()
                                 (when (process-live-p proc)
                                   (delete-process proc))
                                 (funcall finish nil)))))
    proc))

(defun beads-terminal-tmux--lines (out)
  "Return OUT split into its non-empty lines."
  (split-string (or out "") "\n" t))

(defun beads-terminal-tmux--tagged (lines tag)
  "Return the rest of the first of LINES that starts with TAG, or nil."
  (seq-some (lambda (l) (and (string-prefix-p tag l) (substring l (length tag))))
            lines))

;;; tmux status in the mode line

;; Forward declaration: the scroll section below defines the minor mode;
;; its buffer-local variable is read here (the segment's marker).
(defvar beads-terminal-tmux-scroll-mode)

(defvar-local beads-terminal-tmux--status-session nil
  "Tmux session name mirrored in this buffer's mode line, or nil.")

(defvar-local beads-terminal-tmux--status-socket nil
  "Tmux -L socket for `beads-terminal-tmux--status-session', or nil.")

(defvar-local beads-terminal-tmux--status-directory nil
  "Directory whose host this buffer's tmux status probes run on, or nil.
For a remote city this is the remote (TRAMP) directory the attach was
invoked from; the status refresh and teardown bind `default-directory'
to it so their tmux calls reach the city's host — the terminal buffer
itself has a LOCAL `default-directory', since for a remote attach it
hosts a local ssh.  Nil means probe wherever `default-directory' points
\(a local city).")

(defvar-local beads-terminal-tmux--status-string nil
  "Cached mode-line status string for this buffer, or nil.
Recomputed by `beads-terminal-tmux--status-refresh' and read by the
`beads-terminal-tmux--status-segment' mode-line construct.")

(defvar-local beads-terminal-tmux--status-timer nil
  "Repeating timer refreshing this buffer's tmux status, or nil.")

(defvar-local beads-terminal-tmux--status-mirrored nil
  "Non-nil when this buffer actually mirrors the tmux status.
The mirror is optional (`beads-terminal-tmux-mode-line-status'); the
tmux `status' override is only restored by the teardown when the
mirror was installed.  The mouse ensure has its own switch
 (`beads-terminal-tmux-ensure-mouse') and is restored regardless.")

(defconst beads-terminal-tmux--status-mode-line-segment
  '(:eval (beads-terminal-tmux--status-segment))
  "Mode-line construct that renders the buffer's tmux status string.")

(defun beads-terminal-tmux--tmux (socket &rest args)
  "Run \"tmux [-L SOCKET] ARGS\" and return trimmed stdout, or nil.
Runs via `process-file' where `default-directory' points, so a remote
city's tmux server is probed on its own host — tmux resolved there by
`beads-remote-find-executable'.  Returns nil when tmux is
unavailable, the remote connection fails, or tmux exits non-zero (e.g.
the session is gone), so callers treat a missing session uniformly.
A remote call is bounded by `beads-remote-sync-timeout': a wedged
channel answers nil — uniformly with the other failure modes — instead
of hanging, which is what lets the status-mirror tick keep degrading
gracefully after the link dies (the timer path itself adds the
connection-lock and `non-essential' guards on top)."
  (with-temp-buffer
    (when (eq 0 (condition-case nil
                    (beads-remote-with-timeout
                        beads-remote-sync-timeout
                      (apply #'beads-remote-call-unshared #'process-file
                             (beads-remote-find-executable "tmux")
                             nil t nil
                             (append (beads-terminal-tmux--socket-args socket) args)))
                  (file-error nil)
                  (beads-remote-timeout nil)))
      (string-trim (buffer-string)))))

(defun beads-terminal-tmux--window-list (session socket)
  "Return tmux SESSION's windows on SOCKET as a list of plists, or nil.
Each plist has `:active' (t for the current window) and `:label'
\(\"index:name\" plus tmux's window-flags, e.g. \"1:claude*\").  Returns
nil when the session is gone or lists no windows; this doubles as the
session-existence probe, since `display-message' exits 0 even for a
missing target."
  (let ((out (beads-terminal-tmux--tmux
              socket "list-windows" "-t" session "-F"
"#{window_active}\t#{window_index}:#{window_name}#{window_flags}")))
    (when (and out (not (string-empty-p out)))
      (mapcar (lambda (line)
                (let ((parts (split-string line "\t")))
                  (list :active (equal (car parts) "1")
                        :label (string-join (cdr parts) "\t"))))
              (split-string out "\n" t)))))

(defun beads-terminal-tmux--status-string (session socket)
  "Return the mode-line status string for tmux SESSION on SOCKET, or nil.
Mirrors tmux's status bar without its chrome: the friendly name from the
session's `status-left' (falling back to a truncated SESSION) followed by
the window list, the current window emphasised.  Returns nil when the
session no longer exists.  The session's `status-right' is deliberately
omitted: it is the agent's own `#()' status script, which does not run
while the bar is off, and its residual clock duplicates `display-time'."
  (let ((windows (beads-terminal-tmux--window-list session socket)))
    (when windows
      (let* ((left (beads-terminal-tmux--tmux
                    socket "display-message" "-p" "-t" session
                    "#{E:status-left}"))
             (name (if (and left (not (string-empty-p left)))
                       left
                     (truncate-string-to-width session 24 nil nil "…"))))
        (concat
         (propertize name 'face 'beads-terminal-tmux-session-face)
         "  "
         (mapconcat
          (lambda (w)
            (propertize (plist-get w :label)
                        'face (if (plist-get w :active)
                                  'beads-terminal-tmux-active-window-face 'default)))
          windows " "))))))

(defun beads-terminal-tmux--status-segment ()
  "Mode-line segment for this buffer's cached tmux status, or \"\".
Read on every redisplay; the value is refreshed out-of-band by
`beads-terminal-tmux--status-refresh', not recomputed here.  While
`beads-terminal-tmux-scroll-mode' is active the segment carries a
`[scroll]' marker (REQ-010) — the same status-mirror segment, not a
new one, evaluated live so the marker never lags a refresh tick."
  (if (and beads-terminal-tmux--status-string
           (not (string-empty-p beads-terminal-tmux--status-string)))
      (concat " " beads-terminal-tmux--status-string
              (and beads-terminal-tmux-scroll-mode " [scroll]"))
    ""))

;;; The status mirror, asynchronous

(defvar-local beads-terminal-tmux--status-process nil
  "The status query in flight for this buffer, or nil.")

(defconst beads-terminal-tmux--status-sep "beads-status-left"
  "Line separating the window list from `status-left' in a status query.")

(defun beads-terminal-tmux--status-script (session socket)
  "Return the sh script querying tmux SESSION's windows and status-left.
SOCKET is the tmux server socket.  It exits 3 when the session is gone
\(`list-windows' fails)."
  (concat (beads-terminal-tmux--tmux-sh
           socket "list-windows" "-t" session "-F"
           "#{window_active}\t#{window_index}:#{window_name}#{window_flags}")
          " 2>/dev/null || exit 3; echo "
          beads-terminal-tmux--status-sep "; "
          (beads-terminal-tmux--tmux-sh socket "display-message" "-p" "-t" session
                                     "#{E:status-left}")
          " 2>/dev/null; exit 0"))

(defun beads-terminal-tmux--status-format (session out)
  "Return the mode-line status string for SESSION from query OUT, or nil.
OUT is the stdout of `beads-terminal-tmux--status-script': window lines
\(\"ACTIVE\tINDEX:NAMEFLAGS\"), the separator, then `status-left'."
  (let* ((lines (split-string (or out "") "\n"))
         (sep (seq-position lines beads-terminal-tmux--status-sep))
         (wlines (seq-remove #'string-empty-p (seq-take lines (or sep (length lines)))))
         (left (and sep (string-trim (string-join (nthcdr (1+ sep) lines) "\n"))))
         (windows (mapcar (lambda (line)
                            (let ((parts (split-string line "\t")))
                              (list :active (equal (car parts) "1")
                                    :label (string-join (cdr parts) "\t"))))
                          wlines)))
    (when windows
      (concat
       (propertize (if (and left (not (string-empty-p left)))
                       left
                     (truncate-string-to-width session 24 nil nil "…"))
                   'face 'beads-terminal-tmux-session-face)
       "  "
       (mapconcat (lambda (w)
                    (propertize (plist-get w :label)
                                'face (if (plist-get w :active)
                                          'beads-terminal-tmux-active-window-face 'default)))
                  windows " ")))))

(defun beads-terminal-tmux--status-stop ()
  "Stop this buffer's status refresh timer."
  (when (timerp beads-terminal-tmux--status-timer)
    (cancel-timer beads-terminal-tmux--status-timer))
  (setq beads-terminal-tmux--status-timer nil))

(defun beads-terminal-tmux--status-refresh ()
  "Start one asynchronous status query for this buffer's tmux session.
The mode line keeps showing the last result until the answer comes;
a query still in flight makes this a no-op (a slow link is never
stacked up).  When the session has gone, the mirror clears and its
timer stops.  Runs no TRAMP and no synchronous process: the query is
one local process (`beads-terminal-tmux--run-async') to the host."
  (when (and beads-terminal-tmux--status-session
             (not (process-live-p beads-terminal-tmux--status-process)))
    (let ((buffer (current-buffer))
          (session beads-terminal-tmux--status-session)
          (dir (or beads-terminal-tmux--status-directory default-directory)))
      (setq beads-terminal-tmux--status-process
            (beads-terminal-tmux--run-async
             dir
             (beads-terminal-tmux--status-script session beads-terminal-tmux--status-socket)
             (lambda (result)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq beads-terminal-tmux--status-process nil)
                   (cond
                    ((eql (car result) 0)
                     (setq beads-terminal-tmux--status-string
                           (beads-terminal-tmux--status-format session (cdr result))))
                    ((eql (car result) 3)
                     ;; The session is gone: nothing left to mirror.
                     (setq beads-terminal-tmux--status-string nil)
                     (beads-terminal-tmux--status-stop)))
                   ;; Any other failure (timeout, dropped link) keeps the
                   ;; last result; the next tick retries.
                   (force-mode-line-update)))))))))

(defun beads-terminal-tmux--status-tick (buffer)
  "Timer callback: refresh BUFFER's tmux status while it is live.
Asynchronous and skipped while the previous query is in flight
\(`beads-terminal-tmux--status-refresh'); no TRAMP, so no connection lock
to respect."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (beads-terminal-tmux--status-refresh))))

(defconst beads-terminal-tmux--mouse-wheel-command
  (concat "if -F '#{==:#{scroll_position},0}' 'send -X cancel'"
          " { select-pane; send -X -N 5 scroll-down }")
  "Copy-mode command bound to `WheelDownPane' by the mouse ensure.
At the bottom of the history
`beads-terminal-tmux--mouse-ensure-script': at the bottom of the history
\(`scroll-position' 0) leave copy mode for the live tail, otherwise
select the pane under the mouse and scroll.  The string must reach
tmux as ONE argv word, and exactly in this shape (verified live,
tmux 3.7c): a word ending in a plain `;' is tmux's command separator
\(the `if' then runs at bind time and the key is bound to bare
`select-pane'), an embedded `\\;' is a LITERAL `;' once the binding
fires (\"too many arguments\"), and the brace group must be unquoted
or it stays a string and fails to parse at fire time.")

(defun beads-terminal-tmux--mouse-ensure-script (session socket)
  "Return the sh fragment turning tmux mouse scrolling on for SESSION.
SOCKET is the tmux server socket.  Two tmux commands: `set-option -t
SESSION mouse on' (session-scoped) and one copy-mode `WheelDownPane'
binding that leaves copy mode when it is already at the bottom —
wheeling to the bottom returns to the live tail (DESIGN-agent-scrolling.md
D3).  tmux key tables are server-global, so the binding is not
targeted at SESSION; the compound command is
`beads-terminal-tmux--mouse-wheel-command' and must reach tmux as one
argv word (see its docstring).
The teardown mirror is `beads-terminal-tmux--mouse-teardown-script'."
  (concat
   (beads-terminal-tmux--tmux-sh socket "set-option" "-t" session "mouse" "on")
   " >/dev/null 2>&1; "
   (beads-terminal-tmux--tmux-sh
    socket "bind" "-T" "copy-mode" "WheelDownPane"
    beads-terminal-tmux--mouse-wheel-command)
   " >/dev/null 2>&1; "))

(defun beads-terminal-tmux--mouse-teardown-script (session socket)
  "Return the sh fragment restoring SESSION's tmux mouse defaults on SOCKET.
The mirror of `beads-terminal-tmux--mouse-ensure-script': unsets the
session-scoped `mouse' option and removes the copy-mode
`WheelDownPane' binding, so a later `tmux attach' sees tmux's defaults
 (REQ-013)."
  (concat
   (beads-terminal-tmux--tmux-sh socket "set-option" "-t" session "-u" "mouse")
   " >/dev/null 2>&1; "
   (beads-terminal-tmux--tmux-sh socket "unbind" "-T" "copy-mode" "WheelDownPane")
   " >/dev/null 2>&1; "))

(defun beads-terminal-tmux--teardown-script (session socket)
  "Return the sh script restoring SESSION's attach overrides on SOCKET.
A fragment per installed override: the `status' option only when the
mode-line mirror was installed (`beads-terminal-tmux--status-mirrored'),
and the mouse ensure (`beads-terminal-tmux--mouse-teardown-script') when
`beads-terminal-tmux-ensure-mouse' is non-nil.  Empty when nothing was
installed."
  (concat
   (if beads-terminal-tmux--status-mirrored
       (concat (beads-terminal-tmux--tmux-sh socket "set-option" "-t" session
                                          "-u" "status")
               " >/dev/null 2>&1; ")
     "")
   (if beads-terminal-tmux-ensure-mouse
       (beads-terminal-tmux--mouse-teardown-script session socket)
     "")))

(defun beads-terminal-tmux--status-teardown ()
  "Tear down this buffer's tmux session overrides in the background.
Cancels the refresh timer and, in the background, restores what the
attach changed for the session: the `status' override (when the mode
line mirror was installed) and, when `beads-terminal-tmux-ensure-mouse' is
non-nil, the session's `mouse' option and the copy-mode `WheelDownPane'
binding, so a later `tmux attach' sees tmux's own status bar and mouse
defaults again.  Run from `kill-buffer-hook'; never blocks."
  (beads-terminal-tmux--status-stop)
  (when (process-live-p beads-terminal-tmux--status-process)
    (delete-process beads-terminal-tmux--status-process))
  (when (and beads-terminal-tmux--status-session
             (stringp beads-terminal-tmux--status-session)
             (not (string-empty-p beads-terminal-tmux--status-session)))
    (let ((script (beads-terminal-tmux--teardown-script
                   beads-terminal-tmux--status-session
                   beads-terminal-tmux--status-socket)))
      (unless (string-empty-p script)
        (ignore-errors
          (beads-terminal-tmux--run-async
           (or beads-terminal-tmux--status-directory default-directory)
           script
           #'ignore))))))

(defun beads-terminal-tmux--status-install (buffer session socket &optional dir status-off)
  "Install BUFFER's attach overrides on SESSION/SOCKET.
When enabled, the status mirror is installed too.
  SESSION, SOCKET and the remote DIR are recorded buffer-locally in every
  case (the teardown needs them to restore the overrides), and the
  teardown is added to BUFFER's `kill-buffer-hook' — it reverts the
  `status' override and, under `beads-terminal-tmux-ensure-mouse', the mouse
  ensure (both installed for the session by the attach pre-step).

When `beads-terminal-tmux-mode-line-status' is non-nil, the session's tmux
status bar is additionally mirrored in the buffer's mode line instead:
a segment showing the friendly session name and window list is spliced
in before the trailing fill, a refresh timer is started, and — unless
STATUS-OFF says the caller already did (the attach pre-step does, in
its one host round trip) — the tmux change is made in the background.
DIR, when a remote TRAMP directory, is the city context the tmux
queries run on; it is stored buffer-locally so the refresh and teardown
reach the city's host from this otherwise-local buffer.  Nothing here
blocks: every tmux call is an asynchronous local process.  Idempotent:
safe to re-run when reattaching to a live terminal."
  (when (and (buffer-live-p buffer)
             session (stringp session) (not (string-empty-p session)))
    (with-current-buffer buffer
      (setq beads-terminal-tmux--status-session session
            beads-terminal-tmux--status-socket socket
            beads-terminal-tmux--status-directory (and dir (file-remote-p dir)
                                                    dir))
      (when beads-terminal-tmux-mode-line-status
        (setq beads-terminal-tmux--status-mirrored t)
        (unless status-off
          (beads-terminal-tmux--run-async
           (or beads-terminal-tmux--status-directory default-directory)
           (concat (beads-terminal-tmux--tmux-sh socket "set-option" "-t" session
                                              "status" "off")
                   " >/dev/null 2>&1")
           #'ignore))
        ;; Splice our segment into the mode line exactly once, just before
        ;; the trailing fill (`mode-line-end-spaces') so it stays visible.
        ;; Appending after the fill (`%-') renders it off-screen; prepend as
        ;; a fallback when that anchor is absent.
        (let ((mlf (if (listp mode-line-format)
                       mode-line-format
                     (list mode-line-format))))
          (unless (member beads-terminal-tmux--status-mode-line-segment mlf)
            (let ((tail (member 'mode-line-end-spaces mlf)))
              (setq-local mode-line-format
                          (if tail
                              (append (butlast mlf (length tail))
                                      (cons beads-terminal-tmux--status-mode-line-segment
                                            tail))
                            (cons beads-terminal-tmux--status-mode-line-segment mlf))))))
        ;; (Re)start the refresh timer; query once now, in the background.
        (beads-terminal-tmux--status-stop)
        (let ((interval (if (and (numberp beads-terminal-tmux-status-interval)
                                 (> beads-terminal-tmux-status-interval 0))
                            beads-terminal-tmux-status-interval
                          5)))
          (setq beads-terminal-tmux--status-timer
                (run-with-timer interval interval
                                #'beads-terminal-tmux--status-tick buffer)))
        (beads-terminal-tmux--status-refresh))
      ;; Tear down when the terminal buffer is killed — the mirror and the
      ;; mouse ensure both restore through it.
      (add-hook 'kill-buffer-hook #'beads-terminal-tmux--status-teardown nil t))))

(defun beads-terminal-tmux--attach-argv (session socket &optional remote
                                              program term)
  "Return the local argv that attaches tmux SESSION on SOCKET.
A pure function of its inputs.  The local shape is `env -u TMUX tmux
[-L SOCKET] attach-session -t SESSION' — a clean argv, no shell: `env -u
TMUX' lets the attach nest when Emacs runs inside tmux, and an argv (vs
a format-built shell string) means every backend behaves identically
with no quoting/injection surface.  With REMOTE (a TRAMP name for the
city's host) the same command is wrapped into a local `ssh -t' argv via
`beads-remote-ssh-argv' — the city's tmux server runs on the city's
host, while the terminal backend only spawns local processes — which
signals a `user-error' for non-ssh methods and multi-hop names.
PROGRAM overrides the tmux program name; the remote attach passes the
resolved host path so the ssh side runs the same tmux the probes did.
TERM, when non-nil, is spliced into the env prefix as `TERM=TERM' —
the remote attach passes `beads-terminal-tmux--remote-term' so a host
without terminfo for the client's TERM gets a name it can render
instead of killing the attach (gce-25q).  The assignment runs
host-side, after ssh, overriding the forwarded value; the local
terminal's own TERM (owned by the backend) is never touched, and a
local attach passes no TERM at all."
  (let ((argv (append (list "env" "-u" "TMUX")
                      (and term (list (concat "TERM=" term)))
                      (list (or program "tmux"))
                      (beads-terminal-tmux--socket-args socket)
                      (list "attach-session" "-t" session))))
    (if remote (beads-remote-ssh-argv remote argv) argv)))

(defvar-keymap beads-terminal-tmux-attach-map
  :doc "Keys beads adds to its tmux attach buffers.
Active wherever `beads-terminal-tmux--attach-keys' is set (see
`beads-terminal-tmux--install-keys').  Only `C-c'-prefixed keys belong
here: vterm forwards everything else to the pty
\(`vterm-keymap-exceptions'), and the backends' copy modes own the
plain keys."
  "C-c b" #'beads-show-at-point
  "C-c s" #'beads-terminal-tmux-scroll-toggle)

;;; The Emacs-keys scroll sub-mode (DESIGN-agent-scrolling.md D1)

;; `C-c s' toggles `beads-terminal-tmux-scroll-mode' in an attach buffer.
;; While it is active, the Emacs scroll keys of the mode map are
;; translated to tmux copy-mode byte sequences (the pure table of
;; `beads-terminal-tmux--scroll-sequence', evidence E6/E7) and sent through
;; the per-backend raw-key adapter `beads-terminal-tmux--send-raw'; the
;; agent's pty receives no other keys (E9).  State is optimistic and
;; self-healing: activation sends the copy-mode entry bytes `C-b [' and
;; assumes it took, a re-toggle sends `q' first to recover from an
;; out-of-band copy-mode exit, and the `q'/Esc translations leave both.

(defconst beads-terminal-tmux--copy-mode-entry "\C-b["
  "Bytes entering tmux copy mode from the attach pty: `C-b' `['.")

;; The bottom/top jump is tmux's own: the copy-mode emacs table binds
;; `M-<' to `history-top' and `M->' to `history-bottom', so the table
;; sends those single modified-key bytes (`\\e<' / `\\e>') and lets
;; tmux do the jump.  Locked empirically in the live pass (requirements
;; Open Question 1): the goto-prompt burst `g 0 RET' left the modal
;; prompt stuck open (every later byte typed into it, silently), and a
;; run of C-Downs cannot settle a deep scrollback cheaply.

(defun beads-terminal-tmux--scroll-sequence (event)
  "Return the tmux copy-mode byte sequence for Emacs scroll key EVENT.
EVENT is the single event that invoked a scroll command (as
`last-command-event'): the line-scroll bindings scroll a line — sent
as C-Up/C-Down bytes, since those same keys in tmux copy mode are
cursor moves, not scrolls (E7); the page bindings and `next'/`prior' page;
?\\M-< and
?\\M-> jump to the top/bottom through tmux's own copy-mode
`history-top'/`history-bottom' bindings (locked empirically — see the
note above); ?q and `escape' leave copy mode.  Nil for any other
event — those keys keep the backend's own behaviour.  Pure: this is
the whole D1 table, so tests are table-driven.
 (DESIGN-agent-scrolling.md D1, evidence E6/E7.)"
  (pcase event
    (?\C-p "\e[1;5A")
    (?\C-n "\e[1;5B")
    ((or ?\C-v 'next) "\e[6~")
    ((or ?\M-v 'prior) "\e[5~")
    (?\M-< "\e<")
    (?\M-> "\e>")
    (?q "q")
    ('escape "\e")
    (_ nil)))

(defun beads-terminal-tmux--scroll-backend ()
  "Return the raw-key backend symbol of the current terminal buffer, or nil.
vterm → `vterm', term/`ansi-term' → `term', eat → `eat', ghostel →
`ghostel' (DESIGN-agent-scrolling.md D1); any other major mode has no
adapter (REQ-007)."
  (pcase major-mode
    ('vterm-mode 'vterm)
    ((or 'term-mode 'ansi-term-mode) 'term)
    ('eat-mode 'eat)
    ('ghostel-mode 'ghostel)))

(defun beads-terminal-tmux--ghostel-control-key (char)
  "Return (KEY-NAME . MODS) for the control byte CHAR, or nil.
E6: ghostel's semi-char mode drops control bytes sent as a string —
`C-b' had to go through `ghostel-send-key' — so the adapter encodes
them.  The Latin letters become their `ctrl' keys, RET is `return',
TAB is `tab', DEL is `backspace'; escape sequences (which DO pass as
a string, E6) and printable characters are not control bytes."
  (cond
   ((eq char ?\r) '("return" . nil))
   ((eq char ?\t) '("tab" . nil))
   ((eq char 127) '("backspace" . nil))
   ((and (>= char 1) (<= char 26))
    (cons (char-to-string (+ ?a (1- char))) "ctrl"))
   (t nil)))

(defun beads-terminal-tmux--ghostel-send (seq)
  "Send byte sequence SEQ through ghostel's key/string split (E6).
Runs in the ghostel buffer.  Control bytes go through
`ghostel-send-key' (a bare control byte sent as a string does not
arrive in semi-char mode), everything else — printable runs and
escape sequences alike — through `ghostel-send-string'."
  (let ((chunk ""))
    (dolist (ch (append seq nil))
      (if-let* ((key (beads-terminal-tmux--ghostel-control-key ch)))
          (progn
            (unless (string-empty-p chunk)
              (ghostel-send-string chunk)
              (setq chunk ""))
            (ghostel-send-key (car key) (cdr key)))
        (setq chunk (concat chunk (char-to-string ch)))))
    (unless (string-empty-p chunk)
      (ghostel-send-string chunk))))

(defun beads-terminal-tmux--send-raw (buffer seq)
  "Send the raw byte sequence SEQ to terminal BUFFER's pty.
The sender is the backend's raw-key API, selected from BUFFER's major
mode (E6): vterm → `vterm-send-string', term/`ansi-term' →
`term-send-raw-string', eat → `eat-self-input' (one character event
per byte — eat's encoder passes plain characters through), ghostel →
`beads-terminal-tmux--ghostel-send'.  Returns non-nil when sent.  A
backend without an adapter never errors: it deactivates
`beads-terminal-tmux-scroll-mode' with an echo-area message and returns
nil (REQ-007)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (pcase (beads-terminal-tmux--scroll-backend)
        ('vterm (vterm-send-string seq) t)
        ('term (term-send-raw-string seq) t)
        ('eat (dolist (ch (append seq nil))
                (eat-self-input 1 ch))
               t)
        ('ghostel (beads-terminal-tmux--ghostel-send seq) t)
        (_
         (when beads-terminal-tmux-scroll-mode
           (beads-terminal-tmux-scroll-mode -1)
           (message "scroll mode off: no raw-key adapter for %s" major-mode))
         nil)))))

(defun beads-terminal-tmux-scroll-key ()
  "Send the translation of the scroll key that invoked this command.
The event (`last-command-event') is looked up in the pure table
\(`beads-terminal-tmux--scroll-sequence') and its bytes go to the pty via
`beads-terminal-tmux--send-raw'.  The `q' and Esc entries also deactivate
`beads-terminal-tmux-scroll-mode': leaving copy mode hands the keys back
to the agent (REQ-009).  A key with no table entry sends nothing and
keeps the mode."
  (interactive)
  (let ((event last-command-event))
    (when-let* ((seq (beads-terminal-tmux--scroll-sequence event)))
      (beads-terminal-tmux--send-raw (current-buffer) seq)
      (when (memq event '(?q escape))
        (beads-terminal-tmux-scroll-mode -1)
        (message "scroll mode off")))))

(defvar-local beads-terminal-tmux--scroll-wheel-first nil
  "Non-nil while the next wheel notch is the first after (re-)entry.
The first notch re-sends the copy-mode entry bytes before its
C-Up/Down run — self-healing for the wheel, uniformly with the toggle
\(REQ-011); the toggle arms it, the first notch clears it.")


(defun beads-terminal-tmux-scroll-toggle ()
  "Toggle `beads-terminal-tmux-scroll-mode' in this attach buffer (`C-c s').
Activation sends the copy-mode entry bytes `C-b [' — the only bytes
the agent can see on entry — and arms the map.  Re-toggling while the
mode thinks it is active sends `q' first, recovering from an
out-of-band copy-mode exit, then re-enters: optimistic +
self-healing, with no async `pane_in_mode' resync in v1 (REQ-008).
The way out of the mode is its own `q'/Esc translations.  A backend
without a raw-key adapter deactivates the mode with an echo-area
message instead of erroring (REQ-007)."
  (interactive)
  (if (null (beads-terminal-tmux--scroll-backend))
      (progn
        (when beads-terminal-tmux-scroll-mode
          (beads-terminal-tmux-scroll-mode -1))
        (message "scroll mode: no raw-key adapter for %s" major-mode))
    (when beads-terminal-tmux-scroll-mode
      ;; Self-healing: whatever tmux's pane state actually is, leave
      ;; copy mode first — then the entry below always enters it.
      (beads-terminal-tmux--send-raw (current-buffer) "q"))
    (beads-terminal-tmux--send-raw (current-buffer) beads-terminal-tmux--copy-mode-entry)
    (beads-terminal-tmux-scroll-mode 1)
    (setq beads-terminal-tmux--scroll-wheel-first t)
    (message "scroll mode on")))

;;; Wheel translation for non-reporting backends (DESIGN-agent-scrolling.md D2)

;; On ghostel/eat the wheel is tmux's own: the backends report the mouse
;; natively to tmux, not to Emacs (E4/E5), so their effective scroll-mode
;; map keeps no wheel bindings at all — the native passthrough wins with
;; the mode on or off (REQ-004).  On vterm/term, which report nothing,
;; enabling the mode swaps in the wheel map (buffer-locally, through
;; `minor-mode-overriding-map-alist') and one notch scrolls ≈10 lines
;; through copy mode (REQ-003, REQ-011).

(defun beads-terminal-tmux--backend-reports-mouse-p (backend)
  "Return non-nil when BACKEND reports mouse events natively.
ghostel and eat feed the wheel to tmux themselves (E4/E5); vterm and
term/`ansi-term' do not, and an unknown backend is assumed not to.
Pure: the D2 table."
  (memq backend '(ghostel eat)))

(defconst beads-terminal-tmux--scroll-wheel-notch 3
  "C-Up/Down presses per wheel notch (≈10 lines, tmux's `-N 5' feel).
Adjustable per requirements Open Question 2; the live pass locks it.")

(defun beads-terminal-tmux--wheel-mouse-sequence (up)
  "Return the SGR mouse sequence for one wheel notch, wheel-up when UP.
tmux receives exactly the bytes a mouse-reporting terminal would send
after `mouse on': SGR button 64 is wheel-up, 65 wheel-down, at the
pane's top-left (agent sessions are single-pane).  tmux's own
WheelUpPane/WheelDownPane handling then runs unchanged — entering copy
mode on wheel-up, scrolling, and leaving at the bottom on wheel-down,
the D3 behaviour, so beads keeps no Emacs-side copy-mode state for
the wheel and a mouse-claiming agent still gets the event (E5)."
  (format "\e[<%d;%d;%dM" (if up 64 65) 1 1))

(defun beads-terminal-tmux-scroll-wheel ()
  "Send an intercepted wheel notch to the tmux session.
A no-op on a backend that reports the mouse natively — its wheel goes
to tmux without beads (E5, REQ-004).  On vterm/term the wheel never
reaches tmux (E3), so beads acts as the reporting terminal:

- with no scroll mode active it injects the SGR wheel event tmux would
  have received (`beads-terminal-tmux--wheel-mouse-sequence'), letting
  tmux's own copy-mode handling scroll the transcript; only one wheel
  up enters copy mode, and wheeling back to the bottom returns to the
  live tail (D2, D3, ga-eqpxs);
- with `beads-terminal-tmux-scroll-mode' active it keeps the D2 key
  translation: the first notch after (re-)entry re-sends the copy-mode
  entry bytes before a full run of `beads-terminal-tmux--scroll-wheel-notch'
  C-Ups or C-Downs (REQ-011), later notches the run only — an explicit
  scroll request is not stolen by a mouse-claiming agent."
  (interactive)
  (unless (beads-terminal-tmux--backend-reports-mouse-p
           (beads-terminal-tmux--scroll-backend))
    (if beads-terminal-tmux-scroll-mode
        (let* ((up (memq (event-basic-type last-command-event)
                         '(mouse-4 wheel-up)))
               (run (mapconcat #'identity
                               (make-list beads-terminal-tmux--scroll-wheel-notch
                                          (if up "\e[1;5A" "\e[1;5B"))))
               (first beads-terminal-tmux--scroll-wheel-first))
          (setq beads-terminal-tmux--scroll-wheel-first nil)
          (beads-terminal-tmux--send-raw
           (current-buffer)
           (if first
               (concat beads-terminal-tmux--copy-mode-entry run)
             run)))
      (beads-terminal-tmux--send-raw
       (current-buffer)
       (beads-terminal-tmux--wheel-mouse-sequence
        (memq (event-basic-type last-command-event) '(mouse-4 wheel-up)))))))

(defvar-keymap beads-terminal-tmux-scroll-mode-map
  :doc "Keymap of `beads-terminal-tmux-scroll-mode' (DESIGN-agent-scrolling.md D1).
Every binding translates through `beads-terminal-tmux--scroll-sequence'
and sends via `beads-terminal-tmux--send-raw'; the agent's pty receives
nothing else while the mode is active (E9)."
  "C-p" #'beads-terminal-tmux-scroll-key
  "C-n" #'beads-terminal-tmux-scroll-key
  "C-v" #'beads-terminal-tmux-scroll-key
  "M-v" #'beads-terminal-tmux-scroll-key
  "<next>" #'beads-terminal-tmux-scroll-key
  "<prior>" #'beads-terminal-tmux-scroll-key
  "M-<" #'beads-terminal-tmux-scroll-key
  "M->" #'beads-terminal-tmux-scroll-key
  "q" #'beads-terminal-tmux-scroll-key
  "<escape>" #'beads-terminal-tmux-scroll-key)

(defvar-keymap beads-terminal-tmux-scroll-wheel-map
  :doc "Wheel extension of `beads-terminal-tmux-scroll-mode-map' (D2).
The buffer-local effective map of `beads-terminal-tmux-scroll-mode' on
backends that do NOT report the mouse: the base D1 keys plus the wheel
notches.  Never installed on ghostel/eat — their native tmux
passthrough must not be double-driven (E5, REQ-004)."
  :parent beads-terminal-tmux-scroll-mode-map
  "<mouse-4>" #'beads-terminal-tmux-scroll-wheel
  "<mouse-5>" #'beads-terminal-tmux-scroll-wheel
  "<wheel-up>" #'beads-terminal-tmux-scroll-wheel
  "<wheel-down>" #'beads-terminal-tmux-scroll-wheel)

;;; Wheel-only mode, armed on attach (DESIGN-agent-scrolling.md D2, ga-eqpxs)

;; The scroll mode's wheel extension is only active after `C-c s'; that
;; made the first attach's wheel a no-op on vterm/term.  This separate
;; wheel-only mode is enabled by `beads-terminal-tmux--arm-wheel' in every
;; attach buffer whose backend does not report the mouse, so a notch
;; scrolls with no toggle while every non-wheel key still reaches the
;; agent.  It carries no D1 keyboard bindings; ghostel/eat never get
;; it (their native passthrough must not be double-driven).

(defvar-keymap beads-terminal-tmux-wheel-map
  :doc "Wheel bindings armed on attach out of the box (D2).
Carried by `beads-terminal-tmux-wheel-mode' on backends that do not
report the mouse (vterm, term); never installed on ghostel/eat.  No
D1 keyboard bindings — the agent keeps every key (E9, REQ-004)."
  "<mouse-4>" #'beads-terminal-tmux-scroll-wheel
  "<mouse-5>" #'beads-terminal-tmux-scroll-wheel
  "<wheel-up>" #'beads-terminal-tmux-scroll-wheel
  "<wheel-down>" #'beads-terminal-tmux-scroll-wheel)

(define-minor-mode beads-terminal-tmux-wheel-mode
  "Out-of-the-box wheel support for a non-reporting attach buffer.
Active on vterm/term attach buffers (`beads-terminal-tmux--arm-wheel');
carries only the wheel bindings, so the agent's keys are untouched.
Never enabled on ghostel/eat — the backend reports the mouse to tmux
itself and must not be double-driven (REQ-004)."
  :init-value nil
  :lighter nil
  :keymap beads-terminal-tmux-wheel-map
  ;; As with the scroll mode, install through
  ;; `minor-mode-overriding-map-alist': vterm's copy mode swaps the
  ;; buffer's local map, and this must survive that.
  (setq-local minor-mode-overriding-map-alist
              (assq-delete-all 'beads-terminal-tmux-wheel-mode
                               minor-mode-overriding-map-alist))
  (when beads-terminal-tmux-wheel-mode
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'beads-terminal-tmux-wheel-mode
                            beads-terminal-tmux-wheel-map)
                      minor-mode-overriding-map-alist))))

(define-minor-mode beads-terminal-tmux-scroll-mode
  "Emacs-keys scroll sub-mode for a beads tmux attach buffer (D1).
Toggled with `C-c s' (`beads-terminal-tmux-scroll-toggle').  While
active, the Emacs scroll keys of `beads-terminal-tmux-scroll-mode-map'
are translated to tmux copy-mode byte sequences and sent to the pty;
the agent receives no keys (E9).  `q' and Esc leave copy mode and
deactivate the mode.  On backends that do not report the mouse, the
effective map additionally carries the wheel translation (D2); the
status mirror's segment gains a `[scroll]' marker while the mode is
active (REQ-010)."
  :init-value nil
  :lighter nil
  :keymap beads-terminal-tmux-scroll-mode-map
  ;; The wheel extension is buffer-local and per-backend: install the
  ;; overriding-map entry only when this buffer's backend does not
  ;; report the mouse, and always on deactivate.  Ghostel/eat keep the
  ;; base map — no wheel bindings to interfere with the passthrough.
  (setq-local minor-mode-overriding-map-alist
              (assq-delete-all 'beads-terminal-tmux-scroll-mode
                               minor-mode-overriding-map-alist))
  (when (and beads-terminal-tmux-scroll-mode
             (not (beads-terminal-tmux--backend-reports-mouse-p
                   (beads-terminal-tmux--scroll-backend))))
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'beads-terminal-tmux-scroll-mode
                            beads-terminal-tmux-scroll-wheel-map)
                      minor-mode-overriding-map-alist))))

(defvar-local beads-terminal-tmux--attach-keys nil
"Non-nil in a beads attach buffer: activates `beads-terminal-tmux-attach-map'.")

;; An emulation map rather than a layer over the local map: vterm's
;; copy mode swaps the buffer's local map for its own (and back), which
;; would drop a local layer exactly when point can reach an id.
;; `emulation-mode-map-alists' outranks local and minor-mode maps in
;; every state, and the map binds so little that nothing is shadowed.
(add-to-list 'emulation-mode-map-alists
             `((beads-terminal-tmux--attach-keys . ,beads-terminal-tmux-attach-map)))

(defun beads-terminal-tmux--arm-wheel (buffer)
  "Enable `beads-terminal-tmux-wheel-mode' in attach BUFFER when it helps.
Only on a backend that does not report the mouse natively and only
when `beads-terminal-tmux-ensure-mouse' is on — the injected SGR event is
meaningless without tmux's `mouse on'.  A reporting backend keeps its
passthrough (no double-driving).  Returns BUFFER."
  (when (and (buffer-live-p buffer) beads-terminal-tmux-ensure-mouse)
    (with-current-buffer buffer
      (unless (beads-terminal-tmux--backend-reports-mouse-p
               (beads-terminal-tmux--scroll-backend))
        (beads-terminal-tmux-wheel-mode 1))))
  buffer)

(defun beads-terminal-tmux--install-keys (buffer)
  "Wire BUFFER as an attach buffer.  Returns BUFFER.
Activates `beads-terminal-tmux-attach-map' and arms the out-of-the-box
wheel mode when the backend needs it (`beads-terminal-tmux--arm-wheel')."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq beads-terminal-tmux--attach-keys t)
      (beads-terminal-tmux--arm-wheel buffer)))
  buffer)

;;; The attach pre-step and status helpers
(defun beads-terminal-tmux--beads-integrate (buffer store)
  "Wire beads.el's eldoc in the attach BUFFER to STORE.
STORE is the agent's store directory (nil when unknown).  Sets,
buffer-locally, `beads-eldoc-directory' to STORE when known, so
`bd show' runs against the store owning an id rather than the buffer's
own directory.  A no-op when beads-eldoc is absent or predates the
variable (soft `require', `boundp' guard)."
  (when (buffer-live-p buffer)
    (require 'beads-eldoc nil t)
    (with-current-buffer buffer
      (when (and store (boundp 'beads-eldoc-directory))
        (setq-local beads-eldoc-directory store)))))

(defun beads-terminal-tmux--term-probe-sh (term)
  "Return an sh test that succeeds when the host has terminfo for TERM.
`infocmp TERM', then an existence sweep of the standard compiled-entry
locations — the host-side twin of `beads-remote-terminfo-p'."
  (let ((leaf (format "%s/%s" (substring term 0 1) term)))
    (concat "{ infocmp " (shell-quote-argument term) " >/dev/null 2>&1"
            " || [ -e \"$HOME\"/.terminfo/" (shell-quote-argument leaf) " ]"
            (mapconcat (lambda (d) (concat " || [ -e " (shell-quote-argument
                                                       (concat d "/" leaf))
                                           " ]"))
                       '("/usr/share/terminfo" "/lib/terminfo" "/etc/terminfo"
                         "/usr/local/share/terminfo")
                       "")
            "; }")))

(defun beads-terminal-tmux--attach-script (session socket &rest opts)
  "Return the attach pre-step: one sh script, one host round trip.
It prints `beads-no-session' and stops when SESSION is missing on
SOCKET; else `beads-tmux:PATH' (tmux as the host resolves it), and
per OPTS: :term TERM → `beads-term-ok'/`beads-term-missing';
:dir DIR (host-local) → `beads-dir-ok' when it exists; :status-off →
turns the session's tmux status bar off; and, when
`beads-terminal-tmux-ensure-mouse' is non-nil, turns the session's `mouse'
option on and installs the wheel-to-bottom copy-mode binding
 (`beads-terminal-tmux--mouse-ensure-script')."
  (let ((term (plist-get opts :term))
        (dir (plist-get opts :dir)))
    (concat
     (beads-terminal-tmux--tmux-sh socket "has-session" "-t" session)
     " >/dev/null 2>&1 || { echo beads-no-session; exit 0; }; "
     "echo beads-tmux:$(command -v tmux); "
     (if term
         (concat "if " (beads-terminal-tmux--term-probe-sh term)
"; then echo beads-term-ok; else echo beads-term-missing; fi; ")
       "")
     (if dir
         (concat "[ -d " (shell-quote-argument dir) " ] && echo beads-dir-ok; ")
       "")
     (if (plist-get opts :status-off)
         (concat (beads-terminal-tmux--tmux-sh socket "set-option" "-t" session
                                            "status" "off")
                 " >/dev/null 2>&1; ")
       "")
     (if beads-terminal-tmux-ensure-mouse
         (beads-terminal-tmux--mouse-ensure-script session socket)
       "")
     "echo beads-ok")))

(defun beads-terminal-tmux--term-to-probe (remote)
  "Return (FORCE . PROBE) deciding the TERM of a REMOTE attach.
FORCE is the fallback TERM to force without probing (the backend's TERM
is unknown), PROBE the backend TERM whose terminfo the host must be
asked about; both nil when nothing needs forcing — the fallback is off,
the backend already advertises it, or a previous probe found the entry
\(cached per connection in `beads-remote--cache')."
  (when-let* ((fallback beads-terminal-tmux-remote-term))
    (let ((client
           ;; Backend detection is local, and may load the backend
           ;; package: never with a remote `default-directory' (its
           ;; defcustoms expand `~' through the TRAMP handler).
           (let ((default-directory (file-name-as-directory
                                     temporary-file-directory)))
             (beads-terminal-tmux--client-term))))
      (cond ((null client) (cons fallback nil))
            ((equal client fallback) nil)
            ((gethash (cons (beads-remote-prefix remote) (cons :terminfo client))
                      beads-remote--cache)
             nil)
            (t (cons nil client))))))

(defun beads-terminal-tmux-attach (session &optional socket dir store)
  "Attach to tmux SESSION in a terminal buffer, without blocking Emacs.
SOCKET selects a non-default tmux server when set.  DIR is the working
directory for the spawned terminal.  STORE is the agent's rig store
directory when the caller knows it; it scopes the buffer's beads eldoc.
Signals a `user-error' when SESSION is empty, or at once for a remote
method plain ssh cannot reach.

A live terminal for SESSION is raised at once.  Otherwise ONE
asynchronous pre-step on the city's host (`beads-terminal-tmux--run-async':
a local shell, or a local `ssh -T' pipe for a remote city — never TRAMP)
checks that the session exists, resolves tmux there, asks for terminfo
of the TERM the local backend advertises when it may be missing
\(`beads-terminal-tmux--term-to-probe'), checks DIR, and turns the
session's tmux status bar off; when it answers, the terminal opens.  A
missing session is echoed (\"Can't find tmux session …\"), never
signalled from the callback.  The command returns at once (D9).

When `default-directory' is remote (a view of a remote city), the
terminal is a LOCAL ssh running tmux there (`beads-terminal-tmux--attach-argv'
— ssh methods only) with the tmux path the pre-step found (`ssh HOST
cmd' runs no login shell, so a Guix profile tmux may be off its PATH),
and forcing `beads-terminal-tmux-remote-term' when the host lacks terminfo
for the backend's TERM (gce-25q).  The buffer name is city-qualified so
local and remote attaches — and two same-host cities' attaches —
coexist; its `default-directory' is pinned to DIR on the host when the
pre-step found it there, else to the city directory the attach came
from.

Pinned local or remote, the buffer then gets beads eldoc wired to the
agent's store (`beads-terminal-tmux--beads-integrate') and, when
`beads-terminal-tmux-mode-line-status' is non-nil, the asynchronous tmux
status mirror (`beads-terminal-tmux--status-install').  Returns the live
terminal buffer when one was raised, else nil."
  (unless (and session (stringp session) (not (string-empty-p session)))
    (user-error "No tmux session for this agent"))
  (let* ((origin default-directory)
         (remote (and (file-remote-p origin) origin))
         ;; Built first: an unsupported TRAMP method fails here with its
         ;; clear `user-error', before anything runs.
         (_ (beads-terminal-tmux--attach-argv session socket remote))
         (buf-name (beads-remote-buffer-name
                    (format "*beads-agent-%s*" session) origin))
         (existing (beads-terminal-tmux--live-buffer buf-name)))
    (if existing
        (beads-terminal-tmux-run nil buf-name)
      (let* ((term (and remote (beads-terminal-tmux--term-to-probe remote)))
             (host-dir (and remote dir (stringp dir) (not (string-empty-p dir))
                            (file-local-name
                             (or (beads-remote-localize-path dir remote) dir))))
             (status-off beads-terminal-tmux-mode-line-status)
             (script (beads-terminal-tmux--attach-script
                      session socket :term (cdr term) :dir host-dir
                      :status-off status-off)))
        (message "Attaching %s…" session)
        (beads-terminal-tmux--run-async
         origin script
         (lambda (result)
           (let ((default-directory origin))
             (beads-terminal-tmux--attach-finish
              result session socket dir store remote buf-name term host-dir
              status-off))))
        nil))))

(defun beads-terminal-tmux--attach-finish (result session socket dir store remote
buf-name term host-dir status-off)
  "Open the attach terminal after the pre-step answered RESULT.
RESULT is (EXIT . STDOUT) of `beads-terminal-tmux--attach-script'.
SESSION, SOCKET, DIR, STORE, REMOTE, BUF-NAME, TERM, HOST-DIR and
STATUS-OFF are the attach's, see `beads-terminal-tmux-attach'.  Runs
from a timer: reports problems in the echo area, never signals."
  (let ((lines (beads-terminal-tmux--lines (cdr result))))
    (cond
     ((member "beads-no-session" lines)
      (message "Can't find tmux session: %s (agent may have stopped)" session))
     ((not (member "beads-ok" lines))
      (message "tmux attach %s failed: %s" session
               (if (car result)
                   (or (car (last lines)) (format "exit %s" (car result)))
                 "no answer from the host (timed out)")))
     (t
      (let* ((tmux (beads-terminal-tmux--tagged lines "beads-tmux:"))
             (tmux (and tmux (not (string-empty-p tmux)) tmux))
             (term-ok (member "beads-term-ok" lines))
             (forced (cond ((car term) (car term))
                           ((and (cdr term) (not term-ok)) beads-terminal-tmux-remote-term))))
        (when (and remote (cdr term) term-ok)
          (puthash (cons (beads-remote-prefix remote) (cons :terminfo (cdr term)))
                   t beads-remote--cache))
        (let* ((argv (beads-terminal-tmux--attach-argv
                      session socket remote (and remote tmux) forced))
               (buf (beads-terminal-tmux-run argv buf-name (if remote "~/" dir))))
          (when (and remote (buffer-live-p buf))
            ;; The local ssh spawned from a local directory, but the
            ;; buffer belongs to the remote store: pin the agent's
            ;; directory on the host when the pre-step found it, else the
            ;; remote directory the attach came from.
            (with-current-buffer buf
              (setq default-directory
                    (if (and host-dir (member "beads-dir-ok" lines))
                        (file-name-as-directory
                         (concat (beads-remote-prefix remote) host-dir))
                      remote))))
          (when (buffer-live-p buf)
            (beads-terminal-tmux--install-keys buf)
            (beads-terminal-tmux--beads-integrate buf store)
            (beads-terminal-tmux--status-install buf session socket remote
                                              status-off))
          buf))))))

;;; Backend preload

;; The first attach of a session loads the terminal backend's library
;; (vterm, eat, term …) while deciding the TERM — ~240 ms of blocked
;; command loop on a cold Emacs.  A caller may schedule that load ahead
;; of time, once per session, by adding
;; `beads-terminal-tmux--schedule-preload' to a view-creation hook.
;;
;; Decision (2026-09-25): a `require' is one indivisible load (vterm is
;; one file plus its native module), so it cannot be chunked below the
;; 100 ms budget.  It is accepted as a one-off cost, but only on genuine
;; idle: after `beads-terminal-tmux-preload-idle' seconds (10) without
;; input, and re-armed instead of run when input is pending — a user
;; who is typing never meets it.  Tying it to views that show agents
;; would couple every view to the terminal for no gain: nearly every
;; view does.  (A 2 s idle, the first version, landed inside the
;; comms agent's first-cockpit profile: 240 ms locally, 380 ms remote.)

(defvar beads-terminal-tmux--preload-state nil
  "The backend preload's state: nil (not scheduled), `scheduled', `done'.")

(defun beads-terminal-tmux--backend-loaded-p ()
  "Return non-nil when the configured backend's library is already loaded.
The `auto' backend counts as loaded once any known backend is."
  (pcase beads-terminal-tmux-backend
    ('vterm (featurep 'vterm))
    ('eat (featurep 'eat))
    ('term (featurep 'term))
    (_ (seq-some #'featurep '(vterm eat ghostel term)))))

(defun beads-terminal-tmux-preload-backend ()
  "Load the terminal backend's library now, unless it already is.
Resolves the backend exactly as an attach would (loading its package),
from a local `default-directory'.  Errors are swallowed: a preload must
never disturb the user; the attach reports any real problem later."
  (setq beads-terminal-tmux--preload-state 'done)
  (unless (beads-terminal-tmux--backend-loaded-p)
    (let ((default-directory (file-name-as-directory temporary-file-directory))
          (inhibit-message t))
      (ignore-errors (beads-terminal-tmux--client-term)))))

(defun beads-terminal-tmux--preload-when-idle ()
  "Idle-timer body: preload now, or wait for the next idle if input is pending."
  (if (input-pending-p)
      (beads-terminal-tmux--arm-preload)
    (beads-terminal-tmux-preload-backend)))

(defun beads-terminal-tmux--arm-preload ()
  "Arm the idle timer of the backend preload."
  (run-with-idle-timer beads-terminal-tmux-preload-idle nil
                       #'beads-terminal-tmux--preload-when-idle))

(defun beads-terminal-tmux--schedule-preload (&rest _)
  "Schedule the one-shot backend preload for genuine idle time.
Add this to a view-creation hook: the first view arms the preload
\(after `beads-terminal-tmux-preload-idle' idle seconds); later views do
nothing.  Nil option: never."
  (unless (or beads-terminal-tmux--preload-state noninteractive
              (not (numberp beads-terminal-tmux-preload-idle))
              (beads-terminal-tmux--backend-loaded-p))
    (setq beads-terminal-tmux--preload-state 'scheduled)
    (beads-terminal-tmux--arm-preload)))

(provide 'beads-terminal-tmux)
;;; beads-terminal-tmux.el ends here
