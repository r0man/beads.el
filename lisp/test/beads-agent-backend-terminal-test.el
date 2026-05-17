;;; beads-agent-backend-terminal-test.el --- Tests for the terminal backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Phase 1b (bde-xle9.3) acceptance tests for
;; `beads-agent-backend-terminal' / `beads-agent-backend-claude'.
;; The gate that catches the v1 under-specification: instantiate
;; `beads-agent-backend-claude' and confirm every `beads-agent-backend'
;; generic has an applicable method (no `cl-no-applicable-method').
;; No test spawns a real terminal, touches bd, or uses sleep/sit-for.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-backend)
(require 'beads-agent-backend-terminal)
(require 'beads-terminal)
(require 'beads-test-helpers)

(defun beads-agent-backend-terminal-test--session (buffer)
  "Return a minimal `beads-agent-session' bound to BUFFER."
  (let ((s (beads-agent-session :id "proj#1" :project-dir "/tmp")))
    (beads-agent-session-set-buffer s buffer)
    s))

(defmacro beads-abt-test--no-no-applicable (&rest body)
  "Eval BODY; fail only if it signals `cl-no-applicable-method'.
Other errors are tolerated (the point is dispatch, not behaviour)."
  `(condition-case err
       (progn ,@body)
     (cl-no-applicable-method
      (ert-fail (format "no-applicable-method: %S" err)))
     (error nil)))

;;; The introspection gate: every generic is answered

(ert-deftest beads-agent-backend-terminal-test-answers-every-generic ()
  "`beads-agent-backend-claude' answers every backend generic.
None of available-p / start / stop / stop-async / session-active-p
/ switch-to-buffer / send-prompt / session-name / get-buffer raises
`cl-no-applicable-method'."
  (let* ((backend (beads-agent-backend-claude))
         (buf (generate-new-buffer " *bde-abt*"))
         (session (beads-agent-backend-terminal-test--session buf)))
    (unwind-protect
        (progn
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-available-p backend))
          (beads-abt-test--no-no-applicable
           (let ((default-directory "/tmp"))
             (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/claude"))
                       ((symbol-function 'beads-terminal-spawn)
                        (lambda (&rest _) buf)))
               (beads-agent-backend-start backend nil nil "u"))))
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-session-active-p backend session))
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-switch-to-buffer backend session))
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-send-prompt backend session "x"))
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-session-name backend session))
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-get-buffer backend session))
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-stop-async backend session #'ignore))
          ;; stop last (it kills the buffer)
          (beads-abt-test--no-no-applicable
           (beads-agent-backend-stop backend session)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; build-argv — six cases

(ert-deftest beads-agent-backend-terminal-test-build-argv ()
  "`beads-agent-backend-terminal-build-argv' covers the six cases."
  ;; 1. no flags, no sys -> (command user)
  (let ((b (beads-agent-backend-claude)))
    (should (equal (beads-agent-backend-terminal-build-argv b nil "u" nil)
                   '("claude" "u"))))
  ;; 2. non-empty sys under `flag' -> flag pair then user
  (let ((b (beads-agent-backend-claude)))
    (should (equal (beads-agent-backend-terminal-build-argv b "S" "u" nil)
                   '("claude" "--append-system-prompt" "S" "u"))))
  ;; 3. cli-agent-name set but agent-flag nil -> no pair
  (let ((b (beads-agent-backend-claude :cli-agent-name "bot")))
    (should (equal (beads-agent-backend-terminal-build-argv b nil "u" nil)
                   '("claude" "u"))))
  ;; 4. ordering: agent -> sys -> extra -> user
  (let ((b (beads-agent-backend-claude :agent-flag "--agent"
                                       :cli-agent-name "bot")))
    (should (equal (beads-agent-backend-terminal-build-argv
                    b "S" "u" '("-x" "1"))
                   '("claude" "--agent" "bot"
                     "--append-system-prompt" "S" "-x" "1" "u"))))
  ;; 5. sys = "" -> no pair (unified empty predicate)
  (let ((b (beads-agent-backend-claude)))
    (should (equal (beads-agent-backend-terminal-build-argv b "" "u" nil)
                   '("claude" "u"))))
  ;; 6. system-prompt-position 'env -> no argv pair
  (let ((b (beads-agent-backend-claude :system-prompt-position 'env
                                       :system-prompt-env "CLAUDE_SYS")))
    (should (equal (beads-agent-backend-terminal-build-argv b "S" "u" nil)
                   '("claude" "u")))))

;;; stdin rejected before spawn; send-prompt deferred

(ert-deftest beads-agent-backend-terminal-test-stdin-rejected-pre-spawn ()
  "`stdin' user-prompt position errors BEFORE any spawn occurs."
  (let ((b (beads-agent-backend-claude :user-prompt-position 'stdin))
        (spawned nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/x"))
              ((symbol-function 'beads-terminal-spawn)
               (lambda (&rest _) (setq spawned t) (current-buffer))))
      (should-error (beads-agent-backend-start b nil nil "u") :type 'error)
      (should-not spawned))))

(ert-deftest beads-agent-backend-terminal-test-send-prompt-deferred ()
  "`backend-send-prompt' signals the documented deferral."
  (let ((b (beads-agent-backend-claude)))
    (should-error
     (beads-agent-backend-send-prompt b nil "x") :type 'error)))

;;; bde-h93r terminal-side regression

(ert-deftest beads-agent-backend-terminal-test-bde-h93r-no-hijack ()
  "Pre-existing `*claude-code[<unique>]*' is untouched; a fresh
`*beads-agent[…]*' buffer is the spawn target.
Scope: terminal backend only (wrapper-backend hijack is out of
scope here)."
  (let* ((tag (make-temp-name "h93r"))
         (victim-name (format "*claude-code[%s]*" tag))
         (victim (get-buffer-create victim-name))
         (recorded nil)
         (dir (make-temp-file "bde-h93r-" t)))
    (unwind-protect
        (with-current-buffer victim
          (insert "USER CONTENT — must not be hijacked")
          (let ((b (beads-agent-backend-claude))
                (default-directory dir))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/claude"))
                      ((symbol-function 'beads-terminal-spawn)
                       (lambda (_term name _argv _wd _env)
                         (setq recorded name)
                         (get-buffer-create name))))
              (let ((res (beads-agent-backend-start b nil nil "do work")))
                ;; A fresh, distinct beads-agent buffer was the target.
                (should (string-prefix-p "*beads-agent[" recorded))
                (should-not (equal recorded victim-name))
                ;; Victim buffer + its content are untouched.
                (should (buffer-live-p victim))
                (should (equal "USER CONTENT — must not be hijacked"
                               (with-current-buffer victim
                                 (buffer-string))))
                ;; Returned buffer is live after start returns.
                (should (buffer-live-p (cdr res)))
                (when (buffer-live-p (cdr res))
                  (kill-buffer (cdr res)))))))
      (when (buffer-live-p victim) (kill-buffer victim))
      (delete-directory dir t))))

;;; Slot typing / abstract instantiation

(ert-deftest beads-agent-backend-terminal-test-abstract-not-instantiable ()
  "The abstract intermediate cannot be instantiated."
  (should-error (beads-agent-backend-terminal) :type 'error))

(ert-deftest beads-agent-backend-terminal-test-enum-slot-rejects-typo ()
  "Enum slots reject a typo at `make-instance'."
  (should-error
   (beads-agent-backend-claude :user-prompt-position 'bogus)
   :type 'error)
  (should-error
   (beads-agent-backend-claude :system-prompt-position 'nope)
   :type 'error))

(ert-deftest beads-agent-backend-terminal-test-terminal-slot-accepts-nil ()
  "The `terminal' slot is `:type symbol' and accepts nil."
  (let ((b (beads-agent-backend-claude)))
    (should (null (oref b terminal)))
    ;; nil resolves to `beads-agent-default-terminal' at spawn time.
    (should (eq beads-agent-default-terminal 'beads-terminal-auto))))

(ert-deftest beads-agent-backend-terminal-test-resolve-unknown-errors ()
  "An unknown terminal class symbol signals a clear error."
  (let ((b (beads-agent-backend-claude :terminal 'not-a-terminal-class)))
    (should-error (beads-agent-backend-terminal--resolve b) :type 'error)))

(provide 'beads-agent-backend-terminal-test)
;;; beads-agent-backend-terminal-test.el ends here
