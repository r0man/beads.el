;;; beads-remote-exec-test.el --- Tests for the beads-remote exec layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Unit tests for the shared remote exec layer: executable resolution
;; and its cache, the PATH fragment, and the ssh argv builders.  The
;; pipe argv's command string is round-tripped through a real `sh -c'
;; (the one shell evaluation ssh's remote login shell performs) to
;; prove the argv survives, paths with spaces included.  No test opens
;; a remote connection.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-remote)

(defconst beads-remote-exec-test--prefix "/ssh:user@example.com:"
  "TRAMP prefix `file-remote-p' parses without a connection.")

(defmacro beads-remote-exec-test--with-clean-cache (&rest body)
  "Run BODY with an empty private `beads-remote--cache'."
  (declare (indent 0))
  `(let ((beads-remote--cache (make-hash-table :test 'equal)))
     ,@body))

;;; Executable resolution

(ert-deftest beads-remote-exec-test-find-executable-local-identity ()
  "Local directories and names with a directory pass through."
  :tags '(:unit)
  (beads-remote-exec-test--with-clean-cache
    (let ((default-directory temporary-file-directory))
      (should (equal (beads-remote-find-executable "gc") "gc")))
    (let ((default-directory (concat beads-remote-exec-test--prefix "/home/u/")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (&rest _) (error "Must not probe"))))
        (should (equal (beads-remote-find-executable "/opt/bin/gc")
                       "/opt/bin/gc"))))))

(ert-deftest beads-remote-exec-test-find-executable-caches-per-name ()
  "Hits are cached per (connection . name)."
  :tags '(:unit)
  (beads-remote-exec-test--with-clean-cache
    (let ((default-directory (concat beads-remote-exec-test--prefix "/home/u/")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name &optional _) (concat "/usr/bin/" name))))
        (should (equal (beads-remote-find-executable "gc") "/usr/bin/gc"))
        (should (equal (beads-remote-find-executable "tmux") "/usr/bin/tmux")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (&rest _) (error "Must not probe twice"))))
        (should (equal (beads-remote-find-executable "gc") "/usr/bin/gc")))
      (should (equal (gethash (cons beads-remote-exec-test--prefix "tmux")
                              beads-remote--cache)
                     "/usr/bin/tmux")))))

(ert-deftest beads-remote-exec-test-find-executable-probe-error-not-cached ()
  "A probe error returns the bare name and caches nothing."
  :tags '(:unit)
  (beads-remote-exec-test--with-clean-cache
    (let ((default-directory (concat beads-remote-exec-test--prefix "/home/u/")))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (&rest _) (error "Connection dropped"))))
        (should (equal (beads-remote-find-executable "gc") "gc")))
      (should (zerop (hash-table-count beads-remote--cache))))))

;;; PATH fragment

(ert-deftest beads-remote-exec-test-path-assignment ()
  "Local: nil.  Remote: quoted dirs joined, cached per connection."
  :tags '(:unit)
  (beads-remote-exec-test--with-clean-cache
    (let ((default-directory temporary-file-directory))
      (should-not (beads-remote-path-assignment)))
    (let ((default-directory (concat beads-remote-exec-test--prefix "/home/u/"))
          (beads-remote-search-path '("/opt/a b/bin" "/opt/c/bin")))
      (should (equal (beads-remote-path-assignment)
                     "PATH=/opt/a\\ b/bin:/opt/c/bin:$PATH"))
      (let ((beads-remote-search-path nil))
        ;; An empty search path yields nothing, not the cached value.
        (should-not (beads-remote-path-assignment))))))

;;; Shell command string

(defun beads-remote-exec-test--sh-argv (command-string)
  "Evaluate COMMAND-STRING with `sh -c'; return the argv it executed.
The program must be the printer script, which prints each argument
followed by a NUL, then the PATH it saw."
  (with-temp-buffer
    (should (zerop (call-process "sh" nil t nil "-c" command-string)))
    (split-string (buffer-string) "\0")))

(ert-deftest beads-remote-exec-test-shell-command-roundtrip ()
  "The command string survives one `sh -c' evaluation with argv intact.
Covers a program and a PATH directory with spaces, quotes, $ and
glob characters."
  :tags '(:unit)
  (let* ((root (make-temp-file "beads remote test " t))
         (bindir (expand-file-name "prof bin" root))
         (prog (expand-file-name "print args" bindir)))
    (unwind-protect
        (progn
          (make-directory bindir t)
          (with-temp-file prog
            (insert "#!/bin/sh\n"
                    "for a in \"$@\"; do printf '%s\\0' \"$a\"; done\n"
                    "printf '%s' \"$PATH\"\n"))
          (set-file-modes prog #o755)
          (let* ((args '("events" "--follow" "--after" "42"
                         "a b" "it's" "$HOME" "*" "" "semi;colon"))
                 (assignment (format "PATH=%s:$PATH"
                                     (shell-quote-argument bindir)))
                 (cmd (beads-remote-shell-command (cons prog args) assignment))
                 (out (beads-remote-exec-test--sh-argv cmd)))
            (should (string-prefix-p "PATH=" cmd))
            (should (equal (butlast out) args))
            (should (string-prefix-p (concat bindir ":") (car (last out))))))
      (delete-directory root t))))

;;; ssh argv

(ert-deftest beads-remote-exec-test-ssh-argv-tty ()
  "The tty variant: -t, user/port, per-token quoting."
  :tags '(:unit)
  (should (equal (beads-remote-ssh-argv "/ssh:u@h#2222:/x" '("echo" "a b"))
                 '("ssh" "-t" "-l" "u" "-p" "2222" "h" "echo" "a\\ b")))
  (should (equal (beads-remote-ssh-argv "/sshx:h:/x" '("true"))
                 '("ssh" "-t" "h" "true")))
  (should-error (beads-remote-ssh-argv "/sudo:root@localhost:/etc" '("true"))
                :type 'user-error)
  (should-error (beads-remote-ssh-argv "/ssh:a@b|ssh:c@d:/x" '("true"))
                :type 'user-error)
  (should-error (beads-remote-ssh-argv "/home/x" '("true"))
                :type 'user-error))

(ert-deftest beads-remote-exec-test-ssh-pipe-argv ()
  "The pipe variant: -T, BatchMode, keepalives, --, ONE command string."
  :tags '(:unit)
  (let ((argv (beads-remote-ssh-pipe-argv
               "/ssh:u@h#2222:/home/u/city/"
               '("/home/u/.guix-home/profile/bin/gc" "events" "--follow"
                 "--after" "7")
               "PATH=/home/u/bin:$PATH")))
    (should (equal argv
                   '("ssh" "-T" "-o" "BatchMode=yes"
                     "-o" "ServerAliveInterval=15"
                     "-o" "ServerAliveCountMax=3"
                     "-l" "u" "-p" "2222" "h" "--"
                     "PATH=/home/u/bin:$PATH exec /home/u/.guix-home/profile/bin/gc events --follow --after 7")))
    (should (equal (car (last (beads-remote-ssh-pipe-argv "/ssh:h:/" '("gc"))))
                   "exec gc")))
  (should-error (beads-remote-ssh-pipe-argv "/docker:c:/" '("gc"))
                :type 'user-error))

(ert-deftest beads-remote-exec-test-ssh-pipe-argv-roundtrip ()
  "The pipe argv's command string, evaluated by sh, execs the argv."
  :tags '(:unit)
  (let* ((root (make-temp-file "beads pipe test " t))
         (prog (expand-file-name "print args" root)))
    (unwind-protect
        (progn
          (with-temp-file prog
            (insert "#!/bin/sh\n"
                    "for a in \"$@\"; do printf '%s\\0' \"$a\"; done\n"
                    "printf '%s' \"$PATH\"\n"))
          (set-file-modes prog #o755)
          (let* ((args '("--after" "12" "x y" "'q'"))
                 (argv (beads-remote-ssh-pipe-argv
                        "/ssh:h:/" (cons prog args)
                        (format "PATH=%s:$PATH"
                                (shell-quote-argument root))))
                 (out (beads-remote-exec-test--sh-argv (car (last argv)))))
            (should (equal (butlast out) args))))
      (delete-directory root t))))

(provide 'beads-remote-exec-test)
;;; beads-remote-exec-test.el ends here
