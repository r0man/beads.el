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

(provide 'beads-remote)
;;; beads-remote.el ends here
