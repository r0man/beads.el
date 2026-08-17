;;; beads-remote-test.el --- Tests for remote (TRAMP) stores -*- lexical-binding: t; -*-

;;; Commentary:

;; Offline coverage of beads.el against a remote (TRAMP) bead store
;; (bde-6qe):
;;
;; - `beads-command--spawn-async' dispatches through the TRAMP file
;;   handler (`make-process' `:file-handler'), separates stderr on the
;;   host, and never hands `:stderr' a string — under both tramp-sh
;;   and the direct-async handler.
;; - A missing remote directory is rejected up front instead of
;;   wedging a tramp-sh channel forever.
;; - The concurrency policy probe takes the same remote spawn path.
;; - Buffer names (dashboard, show, list, agent, utility) are
;;   qualified with the remote prefix so a local and a remote project
;;   with the same name never collide, and the parse functions
;;   round-trip the qualifier.
;;
;; All tests are offline.  Spawn-path tests use the standard TRAMP
;; "mock" method (the tramp-tests.el pattern): a real local `sh'
;; behind the full TRAMP file-name machinery, so the same tramp-sh /
;; direct-async handlers a real ssh store would use are exercised with
;; no network.  Name-only tests use syntactic TRAMP names with
;; placeholder hosts (user@example.com); `file-remote-p' never
;; connects for those.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tramp)
(require 'beads-custom)
(require 'beads-command)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-buffer)
(require 'beads-dashboard)

;;; Mock method infrastructure

(defconst beads-remote-test--mock-directory
  (format "/mock::%s" temporary-file-directory)
  "TRAMP name of the local temp directory behind the mock method.")

(defun beads-remote-test--ensure-mock-method ()
  "Register the tramp-tests.el \"mock\" method (idempotent).
A real local `sh' behind the full TRAMP machinery — remote-flavored
code paths, no network."
  (unless (assoc "mock" tramp-methods)
    (add-to-list 'tramp-methods
                 '("mock"
                   (tramp-login-program "sh")
                   (tramp-login-args (("-i")))
                   (tramp-direct-async ("-c"))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))
                   (tramp-connection-timeout 10)))
    (add-to-list 'tramp-default-host-alist
                 `("\\`mock\\'" nil ,(system-name)))))

(defmacro beads-remote-test--with-mock-remote (&rest body)
  "Run BODY with a remote `default-directory' via the TRAMP mock method.
Skips the calling test when the mock connection cannot be
established (e.g. no local sh)."
  (declare (indent 0) (debug t))
  `(progn
     (beads-remote-test--ensure-mock-method)
     (let ((tramp-verbose 0)
           (default-directory beads-remote-test--mock-directory))
       (skip-unless (ignore-errors (file-directory-p default-directory)))
       ,@body)))

(defun beads-remote-test--wait-for (box &optional timeout)
  "Pump the event loop until BOX's car is non-nil or TIMEOUT (10s) lapses.
Return BOX's car."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (null (car box)) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (car box)))

(defun beads-remote-test--remove-direct-async-profile (profile)
  "Deregister connection-local PROFILE installed by a direct-async test.
`connection-local-set-profiles' persists beyond a `let'; later mock
tests must stay non-direct."
  (setq connection-local-criteria-alist
        (delq nil
              (mapcar (lambda (entry)
                        (let ((profiles (remq profile (cdr entry))))
                          (and profiles (cons (car entry) profiles))))
                      connection-local-criteria-alist)))
  (setq connection-local-profile-alist
        (assq-delete-all profile connection-local-profile-alist)))

(defun beads-remote-test--spawn (command)
  "Spawn a fake bd running COMMAND via `beads-command--spawn-async'.
COMMAND is the raw argv the spawn should execute in place of a real
bd invocation.  Returns a box whose car becomes (:ok . RESULT) or
\(:error . ERR) when the async callbacks fire."
  (let ((box (list nil)))
    (cl-letf (((symbol-function 'beads-command-line)
               (lambda (_command) command)))
      (beads-command--spawn-async
       (beads-command-list :json t)
       (lambda (result) (setcar box (cons :ok result)))
       (lambda (err) (setcar box (cons :error err)))))
    box))

;;; Remote command wrapping (unit, no TRAMP)

(ert-deftest beads-remote-test-remote-async-command-wrap ()
  "The remote wrapper runs CMD under /bin/sh with stderr host-discarded."
  (should (equal (beads-command--remote-async-command '("bd" "list" "--json"))
                 '("/bin/sh" "-c" "exec \"$0\" \"$@\" 2>/dev/null"
                   "bd" "list" "--json"))))

(ert-deftest beads-remote-test-stderr-buffer-policy-local ()
  "A local spawn always gets a stderr buffer."
  (should (beads-command--async-stderr-buffer-p nil)))

;;; Async spawn through the TRAMP file handler (mock method)

(ert-deftest beads-remote-test-spawn-async-file-handler ()
  "Async bd spawns through the TRAMP file handler on a remote store.
The spawn succeeds with a remote `default-directory' (without
`:file-handler' the local `make-process' cannot even chdir there),
stderr garbage never corrupts the parsed JSON (separation happens on
the host via the /bin/sh wrapper), and over tramp-sh `make-process'
sees neither a string nor a buffer `:stderr'."
  (beads-remote-test--with-mock-remote
    (let* ((spawn-args nil)
           (real-make-process (symbol-function 'make-process))
           box)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (push args spawn-args)
                   (apply real-make-process args))))
        (setq box (beads-remote-test--spawn
                   '("/bin/sh" "-c"
                     "echo GARBAGE-ON-STDERR >&2; printf '[]'")))
        (should (beads-remote-test--wait-for box)))
      ;; Parse success proves stdout arrived clean: merged stderr
      ;; garbage would make the JSON unreadable and reject instead.
      (should (eq (car (car box)) :ok))
      ;; No make-process call anywhere in the chain saw a string :stderr.
      (should (cl-notany (lambda (args) (stringp (plist-get args :stderr)))
                         spawn-args))
      ;; The beads-async spawn itself — the FIRST call pushed (TRAMP's
      ;; handler re-enters `make-process' internally for the channel,
      ;; reusing our :name, so selecting by name would pick the wrong
      ;; call): file handler on, command wrapped for host-side stderr
      ;; separation, no local stderr buffer on the (non-direct)
      ;; tramp-sh path.
      (let ((beads-spawn (car (last spawn-args))))
        (should (equal (plist-get beads-spawn :name) "beads-async"))
        (should (eq (plist-get beads-spawn :file-handler) t))
        (should (null (plist-get beads-spawn :stderr)))
        (let ((cmd (plist-get beads-spawn :command)))
          (should (equal (car cmd) "/bin/sh"))
          (should (member "exec \"$0\" \"$@\" 2>/dev/null" cmd)))))))

(ert-deftest beads-remote-test-spawn-async-direct-async ()
  "Async bd spawns survive TRAMP's direct-async handler.
With the connection-local `tramp-direct-async-process' enabled, TRAMP
dispatches to `tramp-handle-make-process' (spied, to prove the
dispatch took that path), which accepts only nil or a buffer as
`:stderr' — a string signals `wrong-type-argument bufferp'.  There a
LOCAL stderr buffer is wanted after all: the spawned process is a
fresh local login program whose own chatter (the mock method's
`sh -i' reliably emits job-control noise) would otherwise merge into
stdout and corrupt the JSON."
  (skip-unless (fboundp 'tramp-direct-async-process-p))
  (beads-remote-test--with-mock-remote
    (unwind-protect
        (progn
          (connection-local-set-profile-variables
           'beads-remote-test-direct-async
           '((tramp-direct-async-process . t)))
          (connection-local-set-profiles
           '(:application tramp :protocol "mock")
           'beads-remote-test-direct-async)
          (let* ((direct-calls 0)
                 (spawn-args nil)
                 (real-direct (symbol-function 'tramp-handle-make-process))
                 (real-make-process (symbol-function 'make-process))
                 box)
            (cl-letf (((symbol-function 'tramp-handle-make-process)
                       (lambda (&rest args)
                         (cl-incf direct-calls)
                         (apply real-direct args)))
                      ((symbol-function 'make-process)
                       (lambda (&rest args)
                         (push args spawn-args)
                         (apply real-make-process args))))
              (setq box (beads-remote-test--spawn
                         '("/bin/sh" "-c"
                           "echo GARBAGE-ON-STDERR >&2; printf '[]'")))
              (should (beads-remote-test--wait-for box)))
            (should (> direct-calls 0))
            ;; Parse success = stdout clean of both the command's
            ;; stderr garbage and the login program's chatter.
            (should (eq (car (car box)) :ok))
            (should (cl-notany (lambda (args)
                                 (stringp (plist-get args :stderr)))
                               spawn-args))
            ;; The direct path runs with a local stderr buffer capturing
            ;; the login program's chatter.  First call pushed = ours
            ;; (the handler re-enters `make-process' with our :name).
            (let ((beads-spawn (car (last spawn-args))))
              (should (equal (plist-get beads-spawn :name) "beads-async"))
              (should (bufferp (plist-get beads-spawn :stderr))))))
      (beads-remote-test--remove-direct-async-profile
       'beads-remote-test-direct-async))))

(ert-deftest beads-remote-test-spawn-async-missing-remote-directory ()
  "A missing remote directory is rejected up front, not wedged forever.
tramp-sh's \"cd DIR && exec bd ...\" on a missing DIR idles at the
channel prompt with no sentinel, so the spawn must not happen at all."
  (beads-remote-test--with-mock-remote
    (let ((default-directory
           (concat beads-remote-test--mock-directory
                   "beads-remote-test-no-such-dir/"))
          box)
      (setq box (beads-remote-test--spawn '("/bin/sh" "-c" "printf '[]'")))
      (let ((outcome (beads-remote-test--wait-for box)))
        (should (eq (car outcome) :error))
        (should (string-match-p "no such directory" (car (cdr outcome))))))))

(ert-deftest beads-remote-test-policy-probe-remote ()
  "The concurrency policy probe spawns bd on the remote host too.
Same seam as the dashboard's async sections: a probe running locally
against a remote store would read the wrong backend."
  (beads-remote-test--with-mock-remote
    (let* ((dir (make-temp-file "beads-remote-test-bd" t))
           (stub (expand-file-name "beads-remote-test-bd" dir))
           (box (list nil)))
      (unwind-protect
          (progn
            (write-region "#!/bin/sh\nprintf '{\"mode\":\"server\"}'\n"
                          nil stub)
            (set-file-modes stub #o755)
            (let ((beads-executable stub))
              (beads-command-policy--from-dolt-status
               (lambda (plist) (setcar box (or plist :nil)))))
            (let ((outcome (beads-remote-test--wait-for box)))
              (should (consp outcome))
              (should (eq (plist-get outcome :backend) 'server))
              (should (= (plist-get outcome :max-concurrent) 8))))
        (delete-directory dir t)))))

;;; Remote-qualified buffer names (syntactic, no connection)

(ert-deftest beads-remote-test-project-context-qualified ()
  "The bracket context gains the remote prefix; locally it is unchanged."
  (let ((default-directory "/tmp/"))
    (should (equal (beads-buffer-project-context "proj" nil) "proj"))
    (should (equal (beads-buffer-project-context "proj" "feat") "proj@feat")))
  (let ((default-directory "/ssh:user@example.com:/home/user/proj/"))
    (should (equal (beads-buffer-project-context "proj" nil)
                   "/ssh:user@example.com:|proj"))
    (should (equal (beads-buffer-project-context "proj" "feat")
                   "/ssh:user@example.com:|proj@feat"))))

(ert-deftest beads-remote-test-buffer-names-no-collision ()
  "A local and a remote project with the same name get distinct buffers."
  (let* ((local (let ((default-directory "/tmp/"))
                  (beads-buffer-show "bd-1" "Fix" "rig")))
         (remote (let ((default-directory
                        "/ssh:user@example.com:/home/user/rig/"))
                   (beads-buffer-show "bd-1" "Fix" "rig"))))
    (should-not (equal local remote))
    (should (equal local "*beads-show[rig]/bd-1 Fix*"))
    (should (equal remote
                   "*beads-show[/ssh:user@example.com:|rig]/bd-1 Fix*"))))

(ert-deftest beads-remote-test-parse-round-trip ()
  "Every parse function round-trips the remote qualifier cleanly.
Load-bearing: show context commands recover :issue-id from the
buffer name, so the qualifier must not bleed into other fields."
  (let ((default-directory "/ssh:user@example.com:/home/user/rig/"))
    (let ((parsed (beads-buffer-parse-show (beads-buffer-show "bd-42" "Fix it" "rig"))))
      (should (equal (plist-get parsed :remote) "/ssh:user@example.com:"))
      (should (equal (plist-get parsed :project) "rig"))
      (should (null (plist-get parsed :branch)))
      (should (equal (plist-get parsed :issue-id) "bd-42"))
      (should (equal (plist-get parsed :title) "Fix it")))
    (let ((parsed (beads-buffer-parse-list (beads-buffer-list "ready" nil "rig" "feat"))))
      (should (equal (plist-get parsed :type) "ready"))
      (should (equal (plist-get parsed :remote) "/ssh:user@example.com:"))
      (should (equal (plist-get parsed :project) "rig"))
      (should (equal (plist-get parsed :branch) "feat"))
      (should (null (plist-get parsed :filter))))
    (let ((parsed (beads-buffer-parse-agent
                   (beads-buffer-agent "Task" 2 "bd-7" "Do thing" "rig" nil))))
      (should (equal (plist-get parsed :remote) "/ssh:user@example.com:"))
      (should (equal (plist-get parsed :project) "rig"))
      (should (equal (plist-get parsed :type) "Task"))
      (should (= (plist-get parsed :instance) 2))
      (should (equal (plist-get parsed :issue-id) "bd-7"))
      (should (equal (plist-get parsed :title) "Do thing")))
    (let ((parsed (beads-buffer-parse-utility
                   (beads-buffer-utility "dep-tree" "bd-9" "rig" nil))))
      (should (equal (plist-get parsed :type) "dep-tree"))
      (should (equal (plist-get parsed :remote) "/ssh:user@example.com:"))
      (should (equal (plist-get parsed :project) "rig"))
      (should (equal (plist-get parsed :suffix) "bd-9")))))

(ert-deftest beads-remote-test-parse-local-unqualified ()
  "Local names parse exactly as before, with :remote nil."
  (let ((parsed (beads-buffer-parse-show "*beads-show[proj@feat]/bd-42 Fix*")))
    (should (null (plist-get parsed :remote)))
    (should (equal (plist-get parsed :project) "proj"))
    (should (equal (plist-get parsed :branch) "feat"))
    (should (equal (plist-get parsed :issue-id) "bd-42"))
    (should (equal (plist-get parsed :title) "Fix"))))

(ert-deftest beads-remote-test-predicates-accept-remote-names ()
  "Buffer-type predicates recognize remote-qualified names."
  (should (beads-buffer-show-p
           "*beads-show[/ssh:user@example.com:|rig]/bd-1*"))
  (should (beads-buffer-list-p
           "*beads-list[/ssh:user@example.com:|rig]*"))
  (should (beads-buffer-utility-p
           "*beads-stats[/ssh:user@example.com:|rig]*")))

(ert-deftest beads-remote-test-dashboard-buffer-name ()
  "Dashboard names are remote-qualified; a nil root keeps the default."
  (should (equal (beads-dashboard--buffer-name-for "/home/user/rig")
                 "*beads-dashboard<rig>*"))
  (should (equal (beads-dashboard--buffer-name-for
                  "/ssh:user@example.com:/home/user/rig")
                 "*beads-dashboard</ssh:user@example.com:|rig>*"))
  (should-not (equal (beads-dashboard--buffer-name-for "/home/user/rig")
                     (beads-dashboard--buffer-name-for
                      "/ssh:user@example.com:/home/user/rig")))
  (should (equal (beads-dashboard--buffer-name-for nil)
                 beads-dashboard--buffer-name)))

;;; Show buffers anchor to their store

(ert-deftest beads-remote-test-show-buffer-anchored-to-store ()
  "A show buffer pins `default-directory' to its (remote) project root.
Refreshes and renames run with the buffer current; without the
anchor they would re-resolve against whatever buffer happened to be
current at creation."
  (beads-remote-test--ensure-mock-method)
  (let* ((project-dir (concat beads-remote-test--mock-directory
                              "beads-remote-test-rig"))
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads--project-root)
                   (lambda () project-dir))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main")))
          (let ((default-directory beads-remote-test--mock-directory))
            (setq buffer (beads-show--get-or-create-buffer "bd-1" "Fix")))
          (with-current-buffer buffer
            (should (equal default-directory
                           (file-name-as-directory project-dir)))
            ;; `file-remote-p' canonicalizes the prefix (fills in the
            ;; default host), so derive the expected qualifier from it.
            (should (string-match-p
                     (regexp-quote (concat (file-remote-p project-dir) "|"))
                     (buffer-name)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'beads-remote-test)
;;; beads-remote-test.el ends here
