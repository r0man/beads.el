;;; beads-remote-pipe-test.el --- bd over a local ssh pipe -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; The ssh pipe transport for remote stores (QA F8/B-6): async bd runs
;; as a LOCAL `ssh -T' pipe process, never a tramp-sh `make-process',
;; whose pty mux clients can deadlock the shared ControlMaster against
;; TRAMP's own waits.  Plus the other fixes of that round: bead id
;; prefixes derived from the shown bead (B-3) and the `… more' line
;; as a thing (B-4).  No test opens a connection.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-remote)
(require 'beads-command)
(require 'beads-command-ready)
(require 'beads-command-show)
(require 'beads-dashboard)

;;; Builder

(ert-deftest beads-remote-pipe-test-ssh-pipe-p ()
  "Single-hop ssh-family names use the pipe; others and `tramp' do not."
  :tags '(:unit)
  (let ((beads-remote-transport 'ssh))
    (should (beads-remote-ssh-pipe-p "/ssh:u@h:/srv/rig/"))
    (should-not (beads-remote-ssh-pipe-p "/srv/rig/"))
    (should-not (beads-remote-ssh-pipe-p "/sudo:root@localhost:/srv/"))
    (should-not (beads-remote-ssh-pipe-p "/ssh:a@b|ssh:c@d:/srv/")))
  (let ((beads-remote-transport 'tramp))
    (should-not (beads-remote-ssh-pipe-p "/ssh:u@h:/srv/rig/"))))

(ert-deftest beads-remote-pipe-test-ssh-command-argv ()
  "The argv carries -n, ForwardX11=no, the mux options, cd and env."
  :tags '(:unit)
  (let* ((beads-remote-search-path '("~/.guix-home/profile/bin" "/opt/bin"))
         (beads-remote-ssh-options '("-o" "ControlMaster=auto"))
         (beads-remote-ssh-control-path "/tmp/cp-%C")
         (argv (beads-remote-ssh-command
                "/ssh:u@h:/srv/my rig/" '("bd" "ready" "--json")
                :cd t :env '(("BEADS_DOLT_PORT" . "3307")))))
    (should (equal (seq-take argv 9)
                   '("ssh" "-n" "-o" "ForwardX11=no" "-o" "ControlMaster=auto"
                     "-o" "ControlPath=/tmp/cp-%C" "-T")))
    (should (member "BatchMode=yes" argv))
    (should (equal (car (last argv))
                   (concat "cd /srv/my\\ rig/ && BEADS_DOLT_PORT=3307 "
                           "PATH=\"$HOME\"/.guix-home/profile/bin:/opt/bin:\"$PATH\" "
                           "exec bd ready --json")))))

(ert-deftest beads-remote-pipe-test-no-tramp-io ()
  "Building the command does no file I/O at all."
  :tags '(:unit)
  (cl-letf (((symbol-function 'file-executable-p)
             (lambda (&rest _) (error "No file I/O")))
            ((symbol-function 'executable-find)
             (lambda (&rest _) (error "No file I/O"))))
    (should (beads-remote-ssh-command "/ssh:h:/srv/" '("bd" "list") :cd t))))

(ert-deftest beads-remote-pipe-test-command-roundtrip ()
  "The remote command string, run by sh, cds and execs the argv."
  :tags '(:unit)
  (let* ((dir (make-temp-file "beads pipe " t))
         (bin (expand-file-name "b in" dir))
         (store (expand-file-name "my store" dir)))
    (unwind-protect
        (progn
          (make-directory bin) (make-directory store)
          (with-temp-file (expand-file-name "bd" bin)
            (insert "#!/bin/sh\npwd; for a in \"$@\"; do echo \"[$a]\"; done\n"))
          (set-file-modes (expand-file-name "bd" bin) #o755)
          (let* ((beads-remote-search-path (list bin))
                 (cmd (car (last (beads-remote-ssh-command
                                  (concat "/ssh:h:" store) '("bd" "a b" "c")
                                  :cd t)))))
            (with-temp-buffer
              (should (zerop (call-process "sh" nil t nil "-c" cmd)))
              (should (equal (buffer-string)
                             (concat store "\n[a b]\n[c]\n"))))))
      (delete-directory dir t))))

;;; Spawn

(ert-deftest beads-remote-pipe-test-spawn-uses-local-ssh ()
  "An async bd read on an ssh store spawns a local ssh pipe, no TRAMP."
  :tags '(:unit)
  (let ((beads-remote-transport 'ssh)
        (beads-executable "bd")
        (default-directory "/ssh:u@h:/srv/rig/")
        (real-make-process (symbol-function 'make-process))
        captured)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured (append args (list :cwd default-directory)))
                 (let ((default-directory temporary-file-directory))
                   (funcall real-make-process
                            :name "beads-pipe-test" :command '("true")
                            :buffer (plist-get args :buffer)
                            :sentinel #'ignore))))
              ((symbol-function 'file-directory-p)
               (lambda (&rest _) (error "No TRAMP probe")))
              ((symbol-function 'beads-remote-find-executable)
               (lambda (&rest _) (error "No host resolution"))))
      (beads-command-execute-async (beads-command-ready :json t) #'ignore #'ignore))
    (should captured)
    (should (equal (car (plist-get captured :command)) "ssh"))
    (should (member "-n" (plist-get captured :command)))
    (should-not (plist-get captured :file-handler))
    (should-not (file-remote-p (plist-get captured :cwd)))
    (should (string-match-p "\\`cd /srv/rig/ && .*exec bd ready "
                            (car (last (plist-get captured :command)))))))

(ert-deftest beads-remote-pipe-test-tramp-transport-keeps-tramp ()
  "With `beads-remote-transport' `tramp' the spawn stays on TRAMP."
  :tags '(:unit)
  (let ((beads-remote-transport 'tramp)
        (default-directory "/ssh:u@h:/srv/rig/")
        captured)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args) (setq captured args) nil))
              ((symbol-function 'file-directory-p) (lambda (&rest _) t))
              ((symbol-function 'beads-remote-find-executable)
               (lambda (name &rest _) name)))
      (ignore-errors
        (beads-command-execute-async (beads-command-ready :json t) #'ignore #'ignore)))
    (should (plist-get captured :file-handler))))

;;; B-3: id prefixes from the shown bead

(ert-deftest beads-remote-pipe-test-show-derives-prefixes ()
  "Without configured prefixes, only the bead's own and its dependency
prefixes are ids: hyphenated words are not links."
  :tags '(:unit)
  (let ((issue (beads-issue-from-json
                '((id . "bl-bdj") (title . "Deploy bright-lights")
                  (status . "open") (priority . 1) (issue_type . "task")
                  (description . "see hw-aij and build-basic, bright-lights")
                  (dependencies . [((id . "hw-aij") (title . "x") (status . "open")
                                    (dependency_type . "blocks"))])))))
    (should (equal (sort (beads-show--issue-prefixes issue) #'string<)
                   '("bl" "hw")))
    (with-temp-buffer
      (beads-show-mode)
      (let ((beads-issue-id-prefixes nil))
        (beads-show--render-issue issue)
        (let (links)
          (goto-char (point-min))
          (let (b)
            (while (setq b (next-button (point)))
              (push (button-get b 'issue-id) links)
              (goto-char (button-end b))))
          (should (member "hw-aij" links))
          (should-not (member "build-basic" links))
          (should-not (member "bright-lights" links)))))))

(ert-deftest beads-remote-pipe-test-configured-prefixes-win ()
  "A configured `beads-issue-id-prefixes' overrides the derived ones."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-show--derived-prefixes '("bl"))
    (let ((beads-issue-id-prefixes '("zz")))
      (should (equal (beads-show--prefixes) '("zz"))))
    (let ((beads-issue-id-prefixes nil))
      (should (equal (beads-show--prefixes) '("bl"))))))

;;; B-4: the more line is a thing

(ert-deftest beads-remote-pipe-test-more-line-is-a-thing ()
  "`… and N more (+)' is a thing whose toggle loads more rows."
  :tags '(:unit)
  (let* ((vnode (beads-dashboard--more-line 'ready 15))
         (label (vui-vnode-button-label vnode))
         (thing (get-text-property 2 'beads-thing label))
         bumped)
    (should (eq (plist-get thing :kind) 'more))
    (cl-letf (((symbol-function 'beads-dashboard--bump-extra)
               (lambda (key n) (setq bumped (list key n)))))
      (funcall (plist-get thing :toggle)))
    (should (equal bumped (list 'ready beads-dashboard-section-batch)))))

(provide 'beads-remote-pipe-test)
;;; beads-remote-pipe-test.el ends here
