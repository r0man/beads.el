;;; beads-remote.el --- Remote (TRAMP) executable resolution and ssh -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of beads.el

;;; Commentary:

;; One remote exec layer shared by beads.el (for bd) and packages built
;; on it (gascity.el, for gc and tmux):
;;
;; - `beads-remote-find-executable' resolves a bare program name to an
;;   absolute host-local path on a TRAMP `default-directory':
;;   `tramp-remote-path' first, then the profile directories in
;;   `beads-remote-search-path'.  Resolutions are cached per
;;   (connection x name); a definite miss is remembered for
;;   `beads-remote-miss-ttl' seconds, a probe error never.
;;
;; - `beads-remote-path-assignment' returns the "PATH=DIRS:$PATH" sh
;;   fragment that prepends the same directories to the PATH a remote
;;   command runs with, so the program's own children (git, dolt)
;;   resolve too.  Cached per connection.
;;
;; - `beads-remote-ssh-argv' (interactive, with a tty) and
;;   `beads-remote-ssh-pipe-argv' (no pty, for byte-exact long-lived
;;   streams) turn a TRAMP name into a LOCAL ssh argv, bypassing the
;;   TRAMP channel.  The pipe variant hands ssh one shell-quoted
;;   command string (`beads-remote-shell-command'), every element
;;   quoted exactly once.
;;
;; `beads-remote-forget' clears the cache.  Resolution and the PATH
;; fragment do synchronous TRAMP I/O on first contact with a host; call
;; them from commands, never from redisplay or timers.

;;; Code:

(require 'cl-lib)
(require 'beads-custom)

(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-method "tramp")
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function tramp-file-name-port "tramp")
(declare-function tramp-file-name-hop "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-tramp-file-p "tramp")

;;; Cache

(defvar beads-remote--cache (make-hash-table :test 'equal)
  "Per-connection cache of remote resolutions.
Keys are (REMOTE-PREFIX . NAME) for executables, holding the resolved
host-local path, or (:miss . TIME) for a definite miss honoured for
`beads-remote-miss-ttl' seconds; and (REMOTE-PREFIX . :path) for the
PATH fragment of `beads-remote-path-assignment'.  Consumers may store
their own per-connection facts here under keys that cannot collide
with a program name (a cons or keyword cdr).  Cleared by
`beads-remote-forget'.")

(defun beads-remote-forget ()
  "Forget every cached remote resolution and PATH fragment.
Call after a program moved on a host or `beads-remote-search-path'
changed."
  (interactive)
  (clrhash beads-remote--cache))

(defun beads-remote--miss-fresh-p (entry)
  "Return non-nil when cache ENTRY is a miss still inside its TTL."
  (and (consp entry)
       (eq (car entry) :miss)
       (numberp (cdr entry))
       (numberp beads-remote-miss-ttl)
       (> beads-remote-miss-ttl 0)
       (< (- (float-time) (cdr entry)) beads-remote-miss-ttl)))

;;; Executables

(defun beads-remote-find-executable (name &optional dir)
  "Return program NAME resolved for DIR's host (default `default-directory').
For a local DIR, or when NAME already carries a directory (absolute
or `~/...'), return NAME unchanged.  For a remote DIR resolve a bare
NAME to an absolute host-local path (`file-local-name' form, valid in
`process-file', `make-process' and remote shell command lines):

1. `executable-find' on the host, which searches `tramp-remote-path'
   and so honours user setup such as `tramp-own-remote-path';
2. each `beads-remote-search-path' entry in order (`~' expanded on
   the host), first executable hit wins.

Hits are cached per (connection x NAME) until `beads-remote-forget'.
An unresolvable NAME returns NAME unchanged, so the launch fails where
it always did (exit 127); that miss is remembered for
`beads-remote-miss-ttl' seconds.  A probe error (dropped connection)
also returns NAME but is never cached."
  (let ((remote (file-remote-p (or dir default-directory))))
    (if (or (not remote) (file-name-absolute-p name))
        name
      (let* ((key (cons remote name))
             (cached (gethash key beads-remote--cache)))
        (cond
         ((stringp cached) cached)
         ((beads-remote--miss-fresh-p cached) name)
         (t
          (let* ((default-directory (or dir default-directory))
                 (errored nil)
                 (found
                  (condition-case nil
                      (or (executable-find name t)
                          (cl-some
                           (lambda (entry)
                             (let ((candidate
                                    (expand-file-name
                                     name (expand-file-name
                                           (concat remote entry)))))
                               (and (file-executable-p candidate)
                                    (file-local-name candidate))))
                           beads-remote-search-path))
                    (error (setq errored t) nil))))
            (cond (found (puthash key found beads-remote--cache))
                  ((not errored)
                   (puthash key (cons :miss (float-time))
                            beads-remote--cache))
                  (t (remhash key beads-remote--cache)))
            (or found name))))))))

;;; PATH for the program's children

(defun beads-remote-path-assignment (&optional dir)
  "Return a \"PATH=DIRS:$PATH\" sh fragment for DIR's host, or nil.
DIRS are the `beads-remote-search-path' entries expanded on DIR's host
\(default `default-directory'; `~' becomes the remote home),
shell-quoted and colon-joined.  Nil for a local DIR, an empty search
path, or when host-side expansion fails (dropped connection).

Splice it before a command handed to a remote shell: resolving the
program itself (`beads-remote-find-executable') does not help its
children (git, dolt), which inherit the process's bare PATH.  The
remote shell must evaluate it, so $PATH expands to what the process
actually inherited; `process-environment' cannot express that, since
TRAMP forwards env entries quoted.  Nonexistent directories are kept
\(harmless).  Cached per connection until `beads-remote-forget'."
  (let ((remote (file-remote-p (or dir default-directory))))
    (when (and remote beads-remote-search-path)
      (let ((key (cons remote :path)))
        (or (gethash key beads-remote--cache)
            (when-let* ((dirs
                         (condition-case nil
                             (mapcar (lambda (entry)
                                       (shell-quote-argument
                                        (file-local-name
                                         (expand-file-name
                                          (concat remote entry)))))
                                     beads-remote-search-path)
                           (error nil))))
              (puthash key
                       (format "PATH=%s:$PATH"
                               (mapconcat #'identity dirs ":"))
                       beads-remote--cache)))))))

(defun beads-remote-shell-command (argv &optional path-assignment)
  "Return one POSIX shell command string that execs ARGV.
ARGV is (PROGRAM . ARGS); every element is `shell-quote-argument'ed
exactly once, so the string survives one shell evaluation (ssh's
remote login shell) with the argv intact, spaces and quotes included.
PATH-ASSIGNMENT, a fragment from `beads-remote-path-assignment', is
prepended as an assignment prefix:
  PATH=DIRS:$PATH exec PROGRAM ARGS..."
  (concat (if path-assignment (concat path-assignment " ") "")
          "exec "
          (mapconcat #'shell-quote-argument argv " ")))

;;; Transport

(defconst beads-remote-ssh-methods '("ssh" "sshx" "scp" "scpx")
  "TRAMP methods whose host a plain local `ssh' can reach.")

(defcustom beads-remote-transport 'ssh
  "How asynchronous bd processes reach a remote store.
`ssh' (the default): for a single-hop ssh-family TRAMP directory
\(`beads-remote-ssh-methods'), bd runs as a LOCAL `ssh -T' pipe process
\(`beads-remote-ssh-command'): starting it never blocks Emacs, and it
never shares a pty-backed TRAMP ControlMaster, whose mux clients can
deadlock against TRAMP's own waits.  `tramp': use TRAMP's
`make-process' (other methods always do)."
  :type '(choice (const :tag "Local ssh pipe" ssh)
                 (const :tag "TRAMP make-process" tramp))
  :group 'beads)

(defcustom beads-remote-ssh-options
  '("-o" "ControlMaster=auto" "-o" "ControlPersist=60")
  "Extra ssh options of the pipe processes `beads-remote-ssh-command' builds.
Unless a ControlPath is given here, `beads-remote-ssh-control-path' is
added, so concurrent processes to one host share one ssh master."
  :type '(repeat string)
  :group 'beads)

(defcustom beads-remote-ssh-control-path
  (expand-file-name "beads-ssh-%C" temporary-file-directory)
  "ControlPath of the ssh masters of `beads-remote-ssh-command'.
Distinct from TRAMP's \"tramp.%C\": TRAMP's masters relay pty mux
clients, and a pipe process must never queue behind them.  Packages
built on beads.el (gascity.el) use the same value, so all their pipe
processes to a host share one master."
  :type 'string
  :group 'beads)

(defun beads-remote-ssh-pipe-p (&optional dir)
  "Return non-nil when async processes for DIR run over a local ssh pipe.
DIR defaults to `default-directory'.  Pure: parses the name only."
  (let ((dir (or dir default-directory)))
    (and (eq beads-remote-transport 'ssh)
         (file-remote-p dir)
         (member (file-remote-p dir 'method) beads-remote-ssh-methods)
         (progn (require 'tramp)
                (not (tramp-file-name-hop (tramp-dissect-file-name dir)))))))

(defun beads-remote-pure-path-assignment ()
  "Return a \"PATH=DIRS:$PATH\" fragment built WITHOUT touching the host.
Like `beads-remote-path-assignment', but a `~/'-relative
`beads-remote-search-path' entry becomes \"$HOME\"/... for the remote
shell to expand: pure string operations, so a pipe process can be
built with no TRAMP connection at all.  Nil for an empty search path."
  (when beads-remote-search-path
    (format "PATH=%s:\"$PATH\""
            (mapconcat
             (lambda (entry)
               (cond ((string-match "\\`~/\\(.*\\)\\'" entry)
                      (concat "\"$HOME\"/"
                              (shell-quote-argument (match-string 1 entry))))
                     ((equal entry "~") "\"$HOME\"")
                     (t (shell-quote-argument entry))))
             beads-remote-search-path ":"))))

(defun beads-remote-ssh-mux-options ()
  "Return the ssh options every pipe process of `beads-remote-ssh-command' gets.
\"-n\" (stdin from /dev/null), \"-o ForwardX11=no\" (a `ForwardX11 yes'
in ~/.ssh/config makes the master print xauth warnings), then
`beads-remote-ssh-options' and the ControlPath."
  (append (list "-n" "-o" "ForwardX11=no")
          beads-remote-ssh-options
          (unless (cl-some (lambda (o) (string-prefix-p "ControlPath" o))
                           beads-remote-ssh-options)
            (list "-o" (concat "ControlPath=" beads-remote-ssh-control-path)))))

(cl-defun beads-remote-ssh-command (dir argv &key cd env)
  "Return a local ssh argv running ARGV on the host of DIR, with no TRAMP I/O.
The remote command `cd's to CD (a TRAMP or host-local directory; t
means DIR), sets the ENV assignments (an alist of (VAR . VALUE)),
prepends `beads-remote-pure-path-assignment' to PATH, then execs ARGV;
ARGV's program is used as given (a bare name is found on that PATH).
The ssh options are `beads-remote-ssh-mux-options'.  Signals a
`user-error' for a non-ssh method or a multi-hop DIR."
  (let* ((cd (if (eq cd t) dir cd))
         (prefix (mapconcat
                  #'identity
                  (delq nil
                        (list (and cd (concat "cd " (shell-quote-argument
                                                     (file-local-name cd))
                                              " &&"))
                              (and env
                                   (mapconcat (lambda (pair)
                                                (concat (car pair) "="
                                                        (shell-quote-argument
                                                         (cdr pair))))
                                              env " "))
                              (beads-remote-pure-path-assignment)))
                  " "))
         (argv (beads-remote-ssh-pipe-argv
                dir argv (and (not (string-empty-p prefix)) prefix))))
    (append (list (car argv)) (beads-remote-ssh-mux-options) (cdr argv))))

(defcustom beads-remote-sync-timeout 30
  "Seconds a synchronous command over the ssh pipe may run."
  :type 'natnum
  :group 'beads)

(cl-defun beads-remote-ssh-call (dir argv &key cd env out-buffer)
  "Run ARGV on DIR's host over a local ssh pipe and wait for it.
The synchronous twin of `beads-remote-ssh-command' (CD and ENV as
there).  Returns (EXIT STDOUT STDERR), EXIT nil on timeout
\(`beads-remote-sync-timeout'; the process is killed).  With
OUT-BUFFER, stdout goes there and STDOUT is nil.  The wait polls the
process's liveness, never waits on a dead process (whose sentinel a
process-bound wait may not run), and does no TRAMP I/O."
  (let* ((command (beads-remote-ssh-command dir argv :cd cd :env env))
         (out (or out-buffer (generate-new-buffer " *beads-ssh-out*")))
         (err (generate-new-buffer " *beads-ssh-err*"))
         (default-directory temporary-file-directory)
         (deadline (+ (float-time) beads-remote-sync-timeout))
         (proc (make-process :name "beads-ssh" :buffer out :stderr err
                             :command command :connection-type 'pipe
                             :noquery t :file-handler nil :sentinel #'ignore)))
    (unwind-protect
        (progn
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 0.05))
          (if (process-live-p proc)
              (progn (delete-process proc) (list nil nil nil))
            ;; Collect what is still in flight on the pipes.
            (accept-process-output nil 0)
            (when-let* ((errp (get-buffer-process err)))
              (while (and (process-live-p errp)
                          (accept-process-output errp 0.01 nil t))))
            (list (process-exit-status proc)
                  (unless out-buffer
                    (with-current-buffer out (buffer-string)))
                  (with-current-buffer err (buffer-string)))))
      (let ((kill-buffer-query-functions nil))
        (unless out-buffer (kill-buffer out))
        (kill-buffer err)))))

(defconst beads-remote--find-up-script
  (concat "d=$1; shift; "
          "case \"$d\" in \"~\"|\"~/\"*) d=\"$HOME${d#\\~}\";; esac; "
          "while :; do for m in \"$@\"; do "
          "if [ -e \"$d/$m\" ]; then printf '%s\\n' \"$d\"; exit 0; fi; done; "
          "[ \"$d\" = / ] && exit 1; d=$(dirname \"$d\"); done")
  "Shell script: print the nearest directory at or above $1 holding one of $2...
A leading `~' or `~/' in $1 is expanded against the remote `$HOME'
before the walk, so a tilde-relative localname (e.g. the localname of
`/ssh:host:~/store') resolves on the host instead of being treated as
a literal directory named `~'.")

(defun beads-remote-ssh-find-up (dir markers)
  "Return the nearest directory at or above DIR holding one of MARKERS, or nil.
DIR is a TRAMP name on an ssh-transport host; the walk runs there in
one command over the ssh pipe (`beads-remote-ssh-call'), with no TRAMP
I/O.  The result is a TRAMP directory name.  Signals an error when the
host does not answer within `beads-remote-sync-timeout'."
  (require 'tramp)
  (let* ((vec (tramp-dissect-file-name dir))
         (local (directory-file-name (tramp-file-name-localname vec)))
         (result (beads-remote-ssh-call
                  dir (append (list "sh" "-c" beads-remote--find-up-script "sh"
                                    (if (string-empty-p local) "/" local))
                              markers))))
    (pcase result
      (`(nil . ,_) (error "No answer from %s within %ss"
                          (file-remote-p dir) beads-remote-sync-timeout))
      (`(0 ,out . ,_)
       (let ((found (string-trim-right out "\n")))
         (and (not (string-empty-p found))
              (file-name-as-directory (concat (file-remote-p dir) found)))))
      (_ nil))))

;;; Local ssh argv for a remote host


(defun beads-remote--ssh-target (name)
  "Return ([\"-l\" USER] [\"-p\" PORT] HOST) for TRAMP name NAME.
Signal a `user-error' for a local name, a non-ssh method or a
multi-hop name (neither maps onto one plain ssh invocation)."
  (require 'tramp)
  (unless (tramp-tramp-file-p name)
    (user-error "Not a remote TRAMP name: %s" name))
  (let* ((vec (tramp-dissect-file-name name))
         (method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (port (tramp-file-name-port vec)))
    (when (tramp-file-name-hop vec)
      (user-error "Multi-hop TRAMP name not supported for a direct ssh: %s"
                  name))
    (unless (member method beads-remote-ssh-methods)
      (user-error "TRAMP method %s cannot be reached with plain ssh (need %s)"
                  method (mapconcat #'identity beads-remote-ssh-methods "/")))
    (append (and user (list "-l" user))
            (and port (list "-p" (format "%s" port)))
            (list (tramp-file-name-host vec)))))

(defun beads-remote-ssh-argv (name argv)
  "Return a local ssh argv running ARGV with a tty on the host of NAME.
NAME is a TRAMP file name (or prefix) with an ssh-family method
\(`beads-remote-ssh-methods'); ARGV is (PROGRAM . ARGS).  The result is
\(\"ssh\" \"-t\" [\"-l\" USER] [\"-p\" PORT] HOST TOKENS...), each token
shell-quoted for the remote shell (ssh joins them with spaces).  For
interactive programs such as a tmux attach.  Signals a `user-error'
for a non-ssh method or a multi-hop name."
  (append (list "ssh" "-t")
          (beads-remote--ssh-target name)
          (mapcar #'shell-quote-argument argv)))

(defun beads-remote-ssh-pipe-argv (name argv &optional path-assignment)
  "Return a local no-pty ssh argv running ARGV on the host of NAME.
For long-lived, byte-exact streams (JSONL followers) that must not hold
a TRAMP channel nor go through a pty.  The result is

  (\"ssh\" \"-T\" \"-o\" \"BatchMode=yes\" \"-o\" \"ServerAliveInterval=15\"
   \"-o\" \"ServerAliveCountMax=3\" [\"-l\" USER] [\"-p\" PORT] HOST \"--\"
   COMMAND)

COMMAND is one string from `beads-remote-shell-command': ssh hands
everything after HOST to the remote login shell as a single line, so
ARGV is quoted exactly once.  ARGV's program should already be a
host-local path (`beads-remote-find-executable').  PATH-ASSIGNMENT,
when non-nil, is spliced in front (see `beads-remote-path-assignment');
this function does no I/O itself.  BatchMode means ssh never prompts
\(a password prompt would hang invisibly); the user's ~/.ssh/config
\(ControlMaster, ProxyJump) applies as for any ssh.  Signals a
`user-error' for a non-ssh method or a multi-hop name."
  (append (list "ssh" "-T"
                "-o" "BatchMode=yes"
                "-o" "ServerAliveInterval=15"
                "-o" "ServerAliveCountMax=3")
          (beads-remote--ssh-target name)
          (list "--" (beads-remote-shell-command argv path-assignment))))

;;; Pure name operations

(declare-function tramp-make-tramp-file-name "tramp" (vec &optional localname))

(defun beads-remote-prefix (name)
  "Return NAME's TRAMP prefix (\"/method:user@host:\"), or nil when local.
Pure: NAME is only dissected (`tramp-dissect-file-name'), never
expanded.  Use this instead of `file-remote-p' wherever NAME may be a
host-only name such as \"/ssh:host:\" — `file-remote-p' expands its
argument, and expanding an EMPTY localname asks the host for its home
directory, a synchronous round trip."
  (when (and (stringp name) (tramp-tramp-file-p name))
    (when-let* ((vec (ignore-errors (tramp-dissect-file-name name))))
      (tramp-make-tramp-file-name vec 'noloc))))

(defun beads-remote-localize-path (path &optional dir)
  "Return PATH openable from DIR's host (default `default-directory').
When DIR is a remote TRAMP name, re-prefix PATH (a host-local absolute
path) with DIR's remote prefix so `find-file'/`dired' open it on that
host.  A local DIR — or a nil, empty, or already-remote PATH — returns
PATH unchanged."
  (let ((remote (and (stringp path)
                     (not (string-empty-p path))
                     (not (file-remote-p path))
                     (file-remote-p (or dir default-directory)))))
    (if remote (concat remote path) path)))

(defun beads-remote-buffer-name (base &optional dir qualifier)
  "Return BASE qualified by QUALIFIER, or by DIR's remote prefix.
QUALIFIER, when non-nil, is spliced in verbatim before the trailing
`*'; otherwise DIR's TRAMP prefix is spliced before the trailing `*'
so local and remote buffers never collide.  A local DIR (default
`default-directory') returns BASE unchanged."
  (let ((qualifier (or qualifier (file-remote-p (or dir default-directory)))))
    (cond ((not qualifier) base)
          ((string-suffix-p "*" base)
           (format "%s@%s*" (substring base 0 -1) qualifier))
          (t (format "%s@%s" base qualifier)))))

;;; Synchronous-call timeout

(define-error 'beads-remote-timeout
  "Beads synchronous remote call timed out"
  'error)

(declare-function tramp-get-connection-process "tramp" (vec))
(declare-function tramp-get-connection-property "tramp" (key property &optional default))

(defun beads-remote--kill-connection (remote)
  "Delete the TRAMP connection process of REMOTE (a TRAMP prefix), if any.
A TRAMP wait returns — with an error — once its process is gone; this
is what bounds a synchronous TRAMP call.  No remote I/O."
  (when-let* ((vec (ignore-errors (tramp-dissect-file-name remote)))
              (proc (ignore-errors (tramp-get-connection-process vec))))
    (when (process-live-p proc)
      (delete-process proc))))

(defun beads-remote--timeout-signal (secs)
  "Signal `beads-remote-timeout' for a bound of SECS."
  (signal 'beads-remote-timeout
          (list (format "synchronous remote call timed out after %s seconds\
 (connection wedged?); raise or disable `beads-remote-sync-timeout' to suit"
                        secs))))

(defun beads-remote-drain-connection (&optional dir)
  "Best-effort: consume pending stale output on DIR's TRAMP channel.
DIR defaults to `default-directory'; a local DIR is a no-op.  When a
channel command is abandoned mid-flight its output can arrive later and
the next channel command harvests it as its own stdout; draining reads
the connection until it has been quiet for a moment.  Errors are
swallowed; draining is advisory."
  (when-let* ((name (or dir default-directory))
              ((tramp-tramp-file-p name))
              (vec (ignore-errors (tramp-dissect-file-name name)))
              (proc (ignore-errors (tramp-get-connection-process vec))))
    (when (process-live-p proc)
      (ignore-error error
        (with-local-quit
          (let ((rounds 50))
            (while (and (> rounds 0)
                        (accept-process-output proc 0.1 nil t))
              (setq rounds (1- rounds)))))))))

(defun beads-remote-call-with-timeout (secs fn)
  "Call FN, abandoning it after SECS on a remote `default-directory'.
Two bounds run: `with-timeout' and a plain timer that deletes the
directory's TRAMP connection process (the one that holds inside TRAMP,
which suspends `with-timeout' timers).  Either way
`beads-remote-timeout' is signalled.  A nil, zero or negative SECS, or
a local directory, calls FN unbounded."
  (if (not (and (numberp secs) (> secs 0) (file-remote-p default-directory)))
      (funcall fn)
    (let* ((remote (file-remote-p default-directory))
           (fired nil)
           (killer (run-at-time secs nil
                                (lambda ()
                                  (setq fired t)
                                  (beads-remote--kill-connection remote)))))
      (unwind-protect
          (condition-case err
              (with-timeout (secs (setq fired t)
                                  (beads-remote--timeout-signal secs))
                (funcall fn))
            (beads-remote-timeout
             (ignore-error error (beads-remote-drain-connection))
             (signal (car err) (cdr err)))
            (error
             (if fired
                 (beads-remote--timeout-signal secs)
               (signal (car err) (cdr err)))))
        (cancel-timer killer)))))

(defmacro beads-remote-with-timeout (seconds &rest body)
  "Run BODY, abandoning it after SECONDS on a remote directory.
SECONDS is evaluated (typically `beads-remote-sync-timeout'); a nil,
zero, or negative value — or a LOCAL `default-directory' — runs BODY
unbounded.  Expiry signals `beads-remote-timeout'."
  (declare (indent 1))
  `(beads-remote-call-with-timeout ,seconds (lambda () ,@body)))

;;; Asynchronous deadline

(defcustom beads-remote-async-timeout 30
  "Seconds an asynchronous host command may run before it is killed."
  :type 'natnum
  :group 'beads)

;;; Connection sharing (TRAMP make-process / process-file)

(declare-function tramp-direct-async-process-p "tramp" (&optional vec))

(defun beads-remote--share-variable ()
  "Return the TRAMP option controlling ssh connection sharing.
`tramp-use-connection-share' from Emacs 30 on; in Emacs 29 it was
`tramp-use-ssh-controlmaster-options' (same values)."
  (require 'tramp-sh)
  (if (boundp 'tramp-use-connection-share)
      'tramp-use-connection-share
    'tramp-use-ssh-controlmaster-options))

(defun beads-remote-connection-share (&optional dir)
  "Return the connection-share value to spawn with in DIR.
`suppress' for an ssh-family TRAMP DIR that is not in direct-async
mode (a ControlMaster mux session writes into a pty and can block);
otherwise the user's value, unchanged."
  (let ((dir (or dir default-directory)))
    (if (and (file-remote-p dir)
             (member (file-remote-p dir 'method) beads-remote-ssh-methods)
             (not (let ((default-directory dir))
                    (ignore-errors (tramp-direct-async-process-p)))))
        'suppress
      (symbol-value (beads-remote--share-variable)))))

(defun beads-remote-call-unshared (fn &rest args)
  "Call FN with ARGS, TRAMP connection sharing suppressed where needed."
  (cl-progv (list (beads-remote--share-variable))
      (list (beads-remote-connection-share))
    (apply fn args)))

;;; Terminfo on the host

(defun beads-remote--terminfo-candidates (term remote)
  "Return TRAMP file names where TERM's terminfo entry may live on REMOTE.
The compiled-entry locations ncurses consults, each keyed by TERM's
first character (the Linux layout)."
  (let ((leaf (format "%s/%s" (substring term 0 1) term)))
    (mapcar (lambda (dir) (format "%s%s/%s" remote dir leaf))
            '("~/.terminfo" "/usr/share/terminfo" "/lib/terminfo"
              "/etc/terminfo" "/usr/local/share/terminfo"))))

(defun beads-remote-terminfo-p (term &optional dir)
  "Return non-nil when DIR's host likely has a terminfo entry for TERM.
For a local DIR (default `default-directory') this is trivially t.
For a remote DIR the probe is best-effort, on the host: `infocmp TERM'
there first (exit 0 is authoritative), then an existence sweep of the
standard compiled-entry locations.  Positive results are cached per
\(connection x TERM) in `beads-remote--cache'; a miss is re-probed."
  (let ((remote (file-remote-p (or dir default-directory))))
    (if (not remote)
        t
      (let ((key (cons remote (cons :terminfo term))))
        (or (gethash key beads-remote--cache)
            (let* ((default-directory (or dir default-directory))
                   (found
                    (condition-case nil
                        (or (eq 0 (beads-remote-call-unshared
                                   #'process-file
                                   (beads-remote-find-executable "infocmp")
                                   nil nil nil term))
                            (and (cl-some
                                  #'file-exists-p
                                  (beads-remote--terminfo-candidates
                                   term remote))
                                 t))
                      (error nil))))
              (when found
                (puthash key t beads-remote--cache))
              found))))))

;;; Host prewarm

(defcustom beads-remote-prewarm-programs '("bd" "tmux" "infocmp")
  "Programs `beads-remote-prewarm' resolves on an ssh-transport host.
A package built on beads.el (gascity.el) may append its own programs,
e.g. its `gc' executable, so one prewarm covers every remote spawn."
  :type '(repeat string)
  :group 'beads)

(defvar beads-remote--prewarming (make-hash-table :test 'equal)
  "Hosts (TRAMP prefixes) with a prewarm in flight or done.")

(defun beads-remote-prewarm (&optional dir)
  "Resolve `beads-remote-prewarm-programs' on DIR's host in the background.
For an ssh-transport store (`beads-remote-ssh-pipe-p'): one local ssh
pipe process (`beads-remote-ssh-command', no TRAMP I/O) runs
`command -v' for the programs (and a relative `beads-executable')
under the extended PATH and stores each absolute answer in the
per-connection executable cache (`beads-remote--cache'), keyed
\(REMOTE-PREFIX . NAME) exactly as `beads-remote-find-executable'
reads it.  Once per host; a failed prewarm (non-zero exit, or the
`beads-remote-sync-timeout' deadline) may run again later.  Returns
nil at once — the resolution happens in the sentinel."
  (let* ((dir (or dir default-directory))
         (remote (file-remote-p dir)))
    (when (and remote
               (beads-remote-ssh-pipe-p dir)
               (not (gethash remote beads-remote--prewarming)))
      (puthash remote t beads-remote--prewarming)
      (let* ((names (delete-dups
                     (append beads-remote-prewarm-programs
                             (and (stringp beads-executable)
                                  (not (file-name-absolute-p beads-executable))
                                  (list beads-executable)))))
             (script (concat "for n in "
                             (mapconcat #'shell-quote-argument names " ")
                             "; do printf '%s %s\\n' \"$n\" "
                             "\"$(command -v \"$n\" 2>/dev/null)\"; done"))
             (chunks nil)
             (default-directory temporary-file-directory)
             proc)
        (condition-case nil
            (progn
              (setq proc
                    (make-process
                     :name "beads-prewarm" :noquery t
                     :command (beads-remote-ssh-command
                               dir (list "sh" "-c" script))
                     :connection-type 'pipe :file-handler nil :stderr nil
                     :filter (lambda (_p chunk) (push chunk chunks))
                     :sentinel
                     (lambda (p _e)
                       (when (memq (process-status p) '(exit signal))
                         (if (not (eql (process-exit-status p) 0))
                             (remhash remote beads-remote--prewarming)
                           (dolist (line (split-string
                                          (apply #'concat (nreverse chunks))
                                          "\n" t))
                             (let ((pair (split-string line " " t)))
                               (when (and (= (length pair) 2)
                                          (file-name-absolute-p (cadr pair)))
                                 (puthash (cons remote (car pair)) (cadr pair)
                                          beads-remote--cache)))))))))
              (run-at-time (or beads-remote-sync-timeout 30) nil
                           (lambda ()
                             (when (process-live-p proc)
                               (delete-process proc)))))
          (error (remhash remote beads-remote--prewarming)))
        nil))))

(provide 'beads-remote)
;;; beads-remote.el ends here
