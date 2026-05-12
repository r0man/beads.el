;;; beads-agent-ralph-confirm-test.el --- Tests for Ralph confirm-start dialog -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT coverage for the Ralph confirm-start dialog and dry-run buffer
;; (`beads-agent-ralph-confirm.el', bde-vxgm):
;;
;; - Argv preview agrees with the live stream's argv assembly.
;; - Summary line surfaces all four user-visible bindings.
;; - read-char-choice maps to spawn/cancel/dry-run as documented.
;; - Dry-run buffer renders both panes and the action bar; refresh
;;   from the saved args reproduces the same contents.
;; - launch wrapper honours `beads-agent-ralph-confirm-start' and
;;   `noninteractive', and forwards args correctly to start.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-ralph)
(require 'beads-agent-ralph-stream)
(require 'beads-agent-ralph-confirm)

;;; Argv preview

(ert-deftest beads-agent-ralph-confirm-test-argv-base-flags ()
  "Preview argv carries the documented baseline `claude --print' flags."
  (let ((argv (beads-agent-ralph--preview-argv
               (list :prompt "hi" :project-dir "/tmp"))))
    (should (equal (car argv) "claude"))
    (should (member "--print" argv))
    (should (member "--output-format" argv))
    (should (member "stream-json" argv))
    (should (member "--verbose" argv))
    (should (member "--include-partial-messages" argv))
    (should (member "--no-session-persistence" argv))
    (should (member "--permission-mode" argv))
    (should (member "--add-dir" argv))
    ;; Prompt is terminated by `--' so claude treats it as a positional.
    (should (member "--" argv))))

(ert-deftest beads-agent-ralph-confirm-test-argv-permission-default ()
  "Permission mode defaults to the bypassPermissions defcustom."
  (let* ((beads-agent-ralph-default-permission-mode "bypassPermissions")
         (argv (beads-agent-ralph--preview-argv
                (list :prompt "p" :project-dir "/x"))))
    (should (member "bypassPermissions" argv))))

(ert-deftest beads-agent-ralph-confirm-test-argv-passes-budget-and-turns ()
  "Optional budget/turns/mcp-config tokens are included when set."
  (let ((argv (beads-agent-ralph--preview-argv
               (list :prompt "p" :project-dir "/x"
                     :max-budget-usd 2.5
                     :max-turns 8
                     :mcp-config "/etc/mcp.json"))))
    (should (member "--max-budget-usd" argv))
    (should (member "2.5" argv))
    (should (member "--max-turns" argv))
    (should (member "8" argv))
    (should (member "--mcp-config" argv))
    (should (member "/etc/mcp.json" argv))))

(ert-deftest beads-agent-ralph-confirm-test-argv-extra-args-appended ()
  "Extra-args are inserted between optional flags and the prompt terminator."
  (let* ((argv (beads-agent-ralph--preview-argv
                (list :prompt "p" :project-dir "/x"
                      :extra-args '("--foo" "bar"))))
         (tail (cdr (member "--foo" argv))))
    (should tail)
    (should (equal (car tail) "bar"))
    ;; The prompt terminator comes after extra-args.
    (should (equal (nth 0 (cdr tail)) "--"))))

(ert-deftest beads-agent-ralph-confirm-test-argv-program-override ()
  "Program defaults to `claude' and is overridable."
  (let ((argv (beads-agent-ralph--preview-argv
               (list :prompt "p" :project-dir "/x" :program "/bin/echo"))))
    (should (equal (car argv) "/bin/echo"))))

;;; Shell-quoting renderer

(ert-deftest beads-agent-ralph-confirm-test-argv-shell-string ()
  "Shell-quote renderer quotes tokens with spaces and joins with continuations."
  (let* ((argv '("claude" "--prompt" "hello world"))
         (text (beads-agent-ralph--argv-to-shell-string argv)))
    (should (string-match-p "claude" text))
    (should (string-match-p "hello world\\|hello\\\\ world\\|'hello world'" text))
    (should (string-match-p " \\\\\n" text))))

;;; Summary + header

(ert-deftest beads-agent-ralph-confirm-test-summary-line ()
  "Summary line shows iter count, budget, permission, and dir."
  (let ((line (beads-agent-ralph--summary-line
               (list :max-iterations 20 :max-budget-usd 2.0
                     :permission-mode "bypassPermissions"
                     :worktree-dir "/tmp/wt/bd-42"))))
    (should (string-match-p "20 iters" line))
    (should (string-match-p "\\$2\\.00/iter cap" line))
    (should (string-match-p "bypassPermissions" line))
    (should (string-match-p "/tmp/wt/bd-42" line))))

(ert-deftest beads-agent-ralph-confirm-test-summary-line-defaults ()
  "Summary line falls back to defaults when args omit fields."
  (let* ((beads-agent-ralph-default-max-budget-per-iter 2.0)
         (beads-agent-ralph-default-permission-mode "bypassPermissions")
         (line (beads-agent-ralph--summary-line
                (list :project-dir "/repo"))))
    (should (string-match-p "50 iters" line))
    (should (string-match-p "\\$2\\.00/iter cap" line))
    (should (string-match-p "bypassPermissions" line))))

(ert-deftest beads-agent-ralph-confirm-test-header-issue ()
  "Header for an issue kind omits the children count."
  (should (equal (beads-agent-ralph--header-line "bd-42" 'issue nil)
                 "Start Ralph on bd-42?")))

(ert-deftest beads-agent-ralph-confirm-test-header-epic-with-count ()
  "Header for an epic includes the open-children count."
  (should (equal (beads-agent-ralph--header-line "bd-42" 'epic 7)
                 "Start Ralph on bd-42 (epic, 7 open children)?")))

(ert-deftest beads-agent-ralph-confirm-test-header-epic-no-count ()
  "Header for an epic without a count omits the children annotation."
  (should (equal (beads-agent-ralph--header-line "bd-42" 'epic nil)
                 "Start Ralph on bd-42 (epic)?")))

;;; Choice prompt

(ert-deftest beads-agent-ralph-confirm-test-choice-y-spawns ()
  "`?y' maps to the `spawn' choice symbol."
  (cl-letf (((symbol-function 'read-char-choice)
             (lambda (&rest _) ?y)))
    (should (eq 'spawn
                (beads-agent-ralph--read-confirm-choice "bd-42" 'issue nil
                                                       '(:max-iterations 5))))))

(ert-deftest beads-agent-ralph-confirm-test-choice-n-cancels ()
  "`?n' maps to the `cancel' choice symbol."
  (cl-letf (((symbol-function 'read-char-choice)
             (lambda (&rest _) ?n)))
    (should (eq 'cancel
                (beads-agent-ralph--read-confirm-choice "bd-42" 'issue nil
                                                       '(:max-iterations 5))))))

(ert-deftest beads-agent-ralph-confirm-test-choice-d-dry-run ()
  "`?d' maps to the `dry-run' choice symbol."
  (cl-letf (((symbol-function 'read-char-choice)
             (lambda (&rest _) ?d)))
    (should (eq 'dry-run
                (beads-agent-ralph--read-confirm-choice "bd-42" 'issue nil
                                                       '(:max-iterations 5))))))

;;; Iter-1 prompt preview

(ert-deftest beads-agent-ralph-confirm-test-iter1-prompt-renders ()
  "Iter-1 prompt preview embeds the root id and a placeholder issue title."
  (let ((text (beads-agent-ralph--preview-iter1-prompt
               "bd-42" 'issue nil)))
    (should (string-match-p "bd-42" text))))

(ert-deftest beads-agent-ralph-confirm-test-iter1-prompt-override-wins ()
  "Iter-1 prompt preview honours an explicit prompt override."
  (let ((text (beads-agent-ralph--preview-iter1-prompt
               "bd-42" 'issue "STATIC PROMPT BODY")))
    (should (string-match-p "STATIC PROMPT BODY" text))))

;;; Dry-run buffer

(defmacro beads-agent-ralph-confirm-test--with-dry-run (args &rest body)
  "Render the dry-run buffer for ARGS and run BODY inside it.
Buffer is killed at teardown."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (beads-agent-ralph--show-dry-run-buffer ,args)))
     (unwind-protect
         (with-current-buffer buf ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-agent-ralph-confirm-test-dry-run-buffer-renders ()
  "Dry-run buffer contains the prompt pane, argv pane, and action bar."
  (beads-agent-ralph-confirm-test--with-dry-run
      (list :issue "bd-42" :root-id "bd-42" :kind 'issue
            :max-iterations 5)
    (let ((text (buffer-string)))
      (should (string-match-p "Ralph dry-run for bd-42" text))
      (should (string-match-p "PROMPT (iter 1)" text))
      (should (string-match-p "ARGV (claude command line)" text))
      (should (string-match-p "\\[Spawn now\\]" text))
      (should (string-match-p "\\[Cancel\\]" text))
      (should (string-match-p "claude" text)))))

(ert-deftest beads-agent-ralph-confirm-test-dry-run-refresh ()
  "Refreshing the buffer reproduces its content from the saved args."
  (beads-agent-ralph-confirm-test--with-dry-run
      (list :issue "bd-42" :root-id "bd-42" :kind 'issue
            :max-iterations 5)
    (let ((before (buffer-string)))
      (beads-agent-ralph--dry-run-refresh)
      (should (equal before (buffer-string))))))

(ert-deftest beads-agent-ralph-confirm-test-dry-run-prompt-pane-bounds ()
  "Prompt pane buffer-local bounds bracket the rendered prompt."
  (beads-agent-ralph-confirm-test--with-dry-run
      (list :issue "bd-42" :root-id "bd-42" :kind 'issue
            :prompt "STATIC PROMPT BODY")
    (should beads-agent-ralph--dry-run-prompt-begin)
    (should beads-agent-ralph--dry-run-prompt-end)
    (let ((slice (buffer-substring-no-properties
                  beads-agent-ralph--dry-run-prompt-begin
                  beads-agent-ralph--dry-run-prompt-end)))
      (should (string-match-p "STATIC PROMPT BODY" slice)))))

;;; Argv overrides

(ert-deftest beads-agent-ralph-confirm-test-argv-overrides-store-and-fetch ()
  "Setting overrides for a root makes them retrievable, and clear removes them."
  (let ((beads-agent-ralph-argv-overrides nil))
    (setf (alist-get "bd-42" beads-agent-ralph-argv-overrides
                     nil nil #'equal)
          '("--foo" "bar"))
    (should (equal (beads-agent-ralph--argv-overrides-for "bd-42")
                   '("--foo" "bar")))
    (beads-agent-ralph--clear-argv-overrides "bd-42")
    (should (null (beads-agent-ralph--argv-overrides-for "bd-42")))))

;;; Launch wrapper

(ert-deftest beads-agent-ralph-confirm-test-launch-skips-prompt-when-disabled ()
  "When `beads-agent-ralph-confirm-start' is nil, launch spawns immediately."
  (let ((beads-agent-ralph-confirm-start nil)
        (called nil))
    (cl-letf (((symbol-function 'beads-agent-ralph-start)
               (lambda (&rest args)
                 (setq called args)
                 (cons :ctrl :buf))))
      (let ((result (beads-agent-ralph-launch :issue "bd-42")))
        (should (equal result '(:ctrl . :buf)))
        (should (equal (plist-get called :issue) "bd-42"))))))

(ert-deftest beads-agent-ralph-confirm-test-launch-skips-prompt-noninteractive ()
  "In batch mode launch spawns directly even when confirm-start is t."
  (let ((beads-agent-ralph-confirm-start t)
        (called nil))
    (cl-letf (((symbol-function 'beads-agent-ralph-start)
               (lambda (&rest args)
                 (setq called args)
                 (cons :ctrl :buf))))
      (beads-agent-ralph-launch :issue "bd-42")
      (should (equal (plist-get called :issue) "bd-42")))))

(ert-deftest beads-agent-ralph-confirm-test-launch-strips-child-count ()
  "Launch does not forward `:child-count' to start (it is dialog-only)."
  (let ((beads-agent-ralph-confirm-start nil)
        (called nil))
    (cl-letf (((symbol-function 'beads-agent-ralph-start)
               (lambda (&rest args) (setq called args) nil)))
      (beads-agent-ralph-launch :issue "bd-42" :kind 'epic :child-count 7)
      (should (null (plist-member called :child-count)))
      (should (eq (plist-get called :kind) 'epic)))))

(ert-deftest beads-agent-ralph-confirm-test-launch-requires-issue ()
  "Launch signals when called without `:issue'."
  (should-error (beads-agent-ralph-launch :kind 'issue)))

(provide 'beads-agent-ralph-confirm-test)

;;; beads-agent-ralph-confirm-test.el ends here
