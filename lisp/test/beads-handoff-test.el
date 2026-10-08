;;; beads-handoff-test.el --- Tests for beads-handoff -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, testing

;;; Commentary:

;; Unit tests for `beads-handoff.el' (WI-SF-11): the issue-envelope
;; seam, the mock-backend hand-off, and the `beads-context' prime/setup
;; formatting fixtures.  The command executor is mocked so no `bd'
;; process is required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-handoff)
(require 'beads-agent-mock)

;;; Fixtures

(defconst beads-handoff-test--setup-list
  "Available recipes:\n\n  aider         Aider config and instruction files  (built-in)\n  claude        Claude Code hooks (SessionStart)  (built-in)\n  codex         Codex CLI skill guidance   (built-in)\n  cursor        Cursor IDE rules file + agent hooks  (built-in)\n\nUse 'bd setup <recipe>' to install.\n"
  "Fixture `bd setup --list' output.")

(defconst beads-handoff-test--prime
  "## Beads workflow\n\nRun `bd prime' for context.\n"
  "Fixture `bd prime' output.")

(defconst beads-handoff-test--memories
  "## Memories\n\n- be-1: remember the thing\n"
  "Fixture `bd prime --memories-only' output.")

(defun beads-handoff-test--issue ()
  "Return a sample `beads-issue' object."
  (beads-issue :id "be-1" :title "Sample issue"))

(defun beads-handoff-test--mock-execute (cmd)
  "Mock `beads-command-execute' for CMD using the test fixtures."
  (pcase (eieio-object-class cmd)
    ('beads-command-prime
     (if (oref cmd memories-only)
         beads-handoff-test--memories
       beads-handoff-test--prime))
    ('beads-command-setup
     (if (oref cmd list)
         beads-handoff-test--setup-list
       "✓ installed"))
    ('beads-command-config-get "conservative")
    (_ "")))

(defmacro beads-handoff-test--with-mocked-execute (&rest body)
  "Run BODY with `beads-command-execute' mocked to the fixtures."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'beads-command-execute)
              #'beads-handoff-test--mock-execute))
     ,@body))

;;; Hand-off work and envelope

(ert-deftest beads-handoff-test-work-constructors ()
  "The work constructors tag kind, id, title and issue."
  :tags '(:unit)
  (let ((issue (beads-handoff-test--issue)))
    (let ((work (beads-handoff-issue issue)))
      (should (eq (plist-get work :kind) 'issue))
      (should (equal (plist-get work :id) "be-1"))
      (should (equal (plist-get work :title) "Sample issue"))
      (should (eq (plist-get work :issue) issue)))
    (should (eq (plist-get (beads-handoff-molecule "mol-1" "Build") :kind)
                'molecule))
    (should (equal (plist-get (beads-handoff-formula "build-basic") :id)
                   "build-basic"))))

(ert-deftest beads-handoff-test-envelope-molecule ()
  "The default envelope names the molecule and the work-loop commands."
  :tags '(:unit)
  (let* ((work (beads-handoff-molecule "mol-build" "Build lifecycle"))
         (args (beads-handoff-envelope work "SYS" "USER")))
    (should (equal (nth 0 args) nil))
    (should (equal (nth 1 args) "SYS"))
    (should (string-match-p "Molecule mol-build: Build lifecycle" (nth 2 args)))
    (should (string-match-p "bd mol current mol-build" (nth 2 args)))
    (should (string-match-p "USER" (nth 2 args)))))

(ert-deftest beads-handoff-test-envelope-issue-carries-issue ()
  "The default envelope passes the issue object to the backend args."
  :tags '(:unit)
  (let* ((issue (beads-handoff-test--issue))
         (work (beads-handoff-issue issue))
         (args (beads-handoff-envelope work nil "USER")))
    (should (eq (nth 0 args) issue))
    (should (string-match-p "Issue be-1: Sample issue" (nth 2 args)))))

(ert-deftest beads-handoff-test-user-prompt-appends-context ()
  "A dynamic context excerpt is appended after the envelope and user text."
  :tags '(:unit)
  (let ((beads-handoff--context "PRIME-EXCERPT")
        (work (beads-handoff-formula "build-basic")))
    (let ((prompt (beads-handoff--user-prompt work "TASK")))
      (should (string-match-p "Formula build-basic" prompt))
      (should (string-match-p "TASK" prompt))
      (should (string-match-p "Context:\nPRIME-EXCERPT" prompt))
      ;; envelope first
      (should (< (string-match-p "Formula build-basic" prompt)
                 (string-match-p "TASK" prompt))))))

;;; Hand-off start (mock backend)

(ert-deftest beads-handoff-test-start-mock-backend ()
  "`beads-handoff-start' drives the 4-arity mock backend with the envelope."
  :tags '(:unit)
  (beads-agent-mock-reset)
  (beads-agent-mock-register)
  (unwind-protect
      (let* ((issue (beads-handoff-test--issue))
             (work (beads-handoff-issue issue))
             (result (beads-handoff-start work "mock" "Task" "PRIME-CTX"))
             (session (car result))
             (buffer (cdr result))
             (call (car beads-agent-mock--start-calls)))
        (should (beads-agent-mock-session-handle-p session))
        (should (buffer-live-p buffer))
        (should (equal (car call) issue))
        (should (string-match-p "Issue be-1" (cadr call)))
        (should (string-match-p "PRIME-CTX" (cadr call))))
    (beads-agent-mock-unregister)
    (beads-agent-mock-reset)))

(ert-deftest beads-handoff-test-start-unknown-backend-errors ()
  "Handing off to an unregistered backend signals a `user-error'."
  :tags '(:unit)
  (should-error (beads-handoff-start (beads-handoff-formula "f") "nope")
                :type 'user-error))

;;; Context formatting

(ert-deftest beads-handoff-test-command-line-prime-and-setup-are-text ()
  "`bd prime' and `bd setup' must not serialize `--json'."
  :tags '(:unit)
  (should-not (member "--json"
                      (beads-command-line (beads-command-prime :full t))))
  (should-not (member "--json"
                      (beads-command-line (beads-command-setup :list t))))
  (should (member "--full"
                  (beads-command-line (beads-command-prime :full t)))))

(ert-deftest beads-handoff-test-setup-recipe-names ()
  "Recipe names are parsed from `bd setup --list' in order."
  :tags '(:unit)
  (should (equal (beads-context--setup-recipe-names
                  beads-handoff-test--setup-list)
                 '("aider" "claude" "codex" "cursor")))
  (should (null (beads-context--setup-recipe-names ""))))

(ert-deftest beads-handoff-test-setup-status-line ()
  "Check output is classified as installed/stale/missing."
  :tags '(:unit)
  (should (equal (beads-context--setup-status-line "✓ installed") "installed"))
  (should (equal (beads-context--setup-status-line
                  "status: stale (hash mismatch)")
                 "stale"))
  (should (equal (beads-context--setup-status-line
                  "✗ not installed")
                 "missing"))
  (should (equal (beads-context--setup-status-line "something else")
                 "something else")))

(ert-deftest beads-handoff-test-format-setup ()
  "The setup section lists the recipes and their check status."
  :tags '(:unit)
  (beads-handoff-test--with-mocked-execute
    (let* ((data (beads-context--collect))
           (text (beads-context--format-setup data)))
      (should (string-match-p "Available recipes:" text))
      (should (string-match-p "claude: installed" text))
      (should (string-match-p "codex: installed" text)))))

(ert-deftest beads-handoff-test-collect ()
  "Collection returns prime, memories, setup checks and policy."
  :tags '(:unit)
  (let ((process-environment (cons "BD_AGENT_PROFILE=" process-environment)))
    (beads-handoff-test--with-mocked-execute
      (let ((data (beads-context--collect)))
        (should (equal (plist-get data :prime) beads-handoff-test--prime))
        (should (equal (plist-get data :memories) beads-handoff-test--memories))
        (should (equal (plist-get data :policy) "conservative"))
        (should (equal (mapcar #'car (plist-get data :setup-checks))
                       '("aider" "claude" "codex" "cursor")))))))

(ert-deftest beads-handoff-test-policy-env-wins ()
  "`BD_AGENT_PROFILE' overrides the config key."
  :tags '(:unit)
  (let ((process-environment (cons "BD_AGENT_PROFILE=team-maintainer"
                                   process-environment)))
    (should (equal (beads-context--policy) "team-maintainer"))))

(ert-deftest beads-handoff-test-render ()
  "The context buffer renders every section."
  :tags '(:unit)
  (beads-handoff-test--with-mocked-execute
    (let ((buffer (beads-context)))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'beads-context-mode))
            (should (string-match-p "▾ Prime (bd prime)" (buffer-string)))
            (should (string-match-p "beads workflow" (buffer-string)))
            (should (string-match-p "▾ Setup status" (buffer-string)))
            (should (string-match-p "▾ Policy" (buffer-string)))
            (should (string-match-p "agent.profile: conservative"
                                    (buffer-string))))
        (kill-buffer buffer)))))

(ert-deftest beads-handoff-test-context-refresh ()
  "Refreshing re-renders the buffer with freshly collected data."
  :tags '(:unit)
  (beads-handoff-test--with-mocked-execute
    (let ((buffer (beads-context)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer))
            (beads-context-refresh)
            (should (string-match-p "▾ Prime" (buffer-string))))
        (kill-buffer buffer)))))

;;; Live integration

(ert-deftest beads-handoff-test-context-integration ()
  "Live `beads-context' against a temp bd repo renders real prime/setup."
  :tags '(:integration)
  (require 'beads-test)
  (beads-test-with-project ()
    (let ((buffer (beads-context)))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'beads-context-mode))
            (should (string-match-p "▾ Prime (bd prime)" (buffer-string)))
            (should (string-match-p "▾ Setup status" (buffer-string)))
            (should (string-match-p "agent.profile:" (buffer-string)))
            (should (stringp (plist-get beads-context--data :prime)))
            (should (> (length (plist-get beads-context--data :prime)) 0)))
        (kill-buffer buffer)))))

(provide 'beads-handoff-test)
;;; beads-handoff-test.el ends here
