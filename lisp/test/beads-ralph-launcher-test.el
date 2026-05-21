;;; beads-ralph-launcher-test.el --- Tests for the Ralph launcher -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT coverage for `lisp/beads-ralph-launcher.el' (bde-deqx.5):
;;
;;   - pure helpers: `--merge-params', `--default-params',
;;     `--params-for', `--params-equal-p',
;;     `--params-to-start-args' (round-trip into argv preview),
;;     `--save-template' (customize-save-variable persistence),
;;   - launcher buffer setup (entry point seeds buffer-local state),
;;   - already-running guard (refusal),
;;   - cap-reached guard (refusal),
;;   - prompt-override updates remount the launcher.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(require 'beads-agent-ralph)
(require 'beads-ralph-launcher)

;;; Fixtures

(defmacro beads-ralph-launcher-test--with-clean-registry (&rest body)
  "Run BODY with an empty controller registry, restored on exit."
  (declare (indent 0) (debug (body)))
  `(let ((beads-agent-ralph--controllers nil))
     (unwind-protect (progn ,@body)
       (setq beads-agent-ralph--controllers nil))))

(defmacro beads-ralph-launcher-test--with-clean-templates (&rest body)
  "Run BODY with an empty launch-templates alist, restored on exit."
  (declare (indent 0) (debug (body)))
  `(let ((beads-agent-ralph-launch-templates nil))
     (unwind-protect (progn ,@body)
       (setq beads-agent-ralph-launch-templates nil))))

(defmacro beads-ralph-launcher-test--with-launcher-buffer (var root-id &rest body)
  "Open the launcher for ROOT-ID in a fresh singleton buffer, bind it
to VAR, run BODY, and clean up the buffer after."
  (declare (indent 2) (debug (symbolp form body)))
  `(let* ((beads-ralph-launcher-buffer-name
           (generate-new-buffer-name "*beads-ralph-launcher-test*"))
          (,var (beads-ralph-launcher ,root-id)))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,var)
         (kill-buffer ,var)))))

(defun beads-ralph-launcher-test--make-controller (&rest args)
  "Build a minimal `beads-agent-ralph--controller' from ARGS."
  (beads-agent-ralph--controller
   :root-id (or (plist-get args :root-id) "bde-test")
   :root-kind (or (plist-get args :kind) 'issue)
   :status (or (plist-get args :status) 'running)
   :iteration (or (plist-get args :iter) 1)
   :max-iterations (or (plist-get args :max-iter) 20)
   :cumulative-cost-usd (or (plist-get args :cost) 0.0)
   :started-at (or (plist-get args :started-at) (current-time))))

;;; --merge-params

(ert-deftest beads-ralph-launcher-test-merge-params-empty-override ()
  "When OVERRIDE is nil, the result equals BASE."
  (should (equal '(:a 1 :b 2)
                 (beads-ralph-launcher--merge-params '(:a 1 :b 2) nil))))

(ert-deftest beads-ralph-launcher-test-merge-params-replaces-key ()
  "OVERRIDE replaces matching keys in BASE."
  (should (equal '(:a 99 :b 2)
                 (beads-ralph-launcher--merge-params
                  '(:a 1 :b 2) '(:a 99)))))

(ert-deftest beads-ralph-launcher-test-merge-params-nil-wins ()
  "An explicit nil in OVERRIDE wins over a non-nil in BASE."
  (let ((merged
         (beads-ralph-launcher--merge-params '(:a 1 :b 2) '(:a nil))))
    (should (plist-member merged :a))
    (should (null (plist-get merged :a)))
    (should (equal 2 (plist-get merged :b)))))

(ert-deftest beads-ralph-launcher-test-merge-params-adds-new-key ()
  "A key not in BASE is added from OVERRIDE."
  (let ((merged
         (beads-ralph-launcher--merge-params '(:a 1) '(:b 2))))
    (should (equal 1 (plist-get merged :a)))
    (should (equal 2 (plist-get merged :b)))))

;;; --default-params

(ert-deftest beads-ralph-launcher-test-default-params-honors-defcustoms ()
  "The seven launcher keys are populated from the per-key Ralph defcustoms."
  (let ((beads-ralph-launcher-default-params nil)
        (beads-agent-ralph-max-iterations 7)
        (beads-agent-ralph-max-budget-usd-per-iter 0.25)
        (beads-agent-ralph-max-budget-usd 5.0)
        (beads-agent-ralph-max-turns 11)
        (beads-agent-ralph-permission-mode "plan")
        (beads-agent-ralph-sentinel "<promise>BOOM</promise>"))
    (let ((p (beads-ralph-launcher--default-params)))
      (should (equal 7 (plist-get p :max-iterations)))
      (should (equal 0.25 (plist-get p :max-budget-usd-per-iter)))
      (should (equal 5.0 (plist-get p :max-budget-usd)))
      (should (equal 11 (plist-get p :max-turns)))
      (should (equal "plan" (plist-get p :permission-mode)))
      (should (equal "<promise>BOOM</promise>" (plist-get p :sentinel)))
      (should (eq t (plist-get p :worktree))))))

(ert-deftest beads-ralph-launcher-test-default-params-override-wins ()
  "`beads-ralph-launcher-default-params' overrides the per-key defcustom."
  (let ((beads-ralph-launcher-default-params '(:max-iterations 99))
        (beads-agent-ralph-max-iterations 7))
    (should (equal 99 (plist-get (beads-ralph-launcher--default-params)
                                 :max-iterations)))))

;;; --params-equal-p

(ert-deftest beads-ralph-launcher-test-params-equal-p-true-for-same ()
  "Identical plists compare equal."
  (let ((p '(:max-iterations 10 :max-budget-usd-per-iter 0.5
             :max-budget-usd nil :max-turns nil
             :permission-mode "plan" :worktree t :sentinel "S")))
    (should (beads-ralph-launcher--params-equal-p p (copy-sequence p)))))

(ert-deftest beads-ralph-launcher-test-params-equal-p-false-on-diff ()
  "A differing key flips the predicate to nil."
  (let ((a '(:max-iterations 10 :max-budget-usd-per-iter 0.5
             :max-budget-usd nil :max-turns nil
             :permission-mode "plan" :worktree t :sentinel "S"))
        (b '(:max-iterations 11 :max-budget-usd-per-iter 0.5
             :max-budget-usd nil :max-turns nil
             :permission-mode "plan" :worktree t :sentinel "S")))
    (should-not (beads-ralph-launcher--params-equal-p a b))))

(ert-deftest beads-ralph-launcher-test-params-equal-p-ignores-extra-keys ()
  "Extra keys not in the canonical set are ignored."
  (let ((a '(:max-iterations 10 :max-budget-usd-per-iter 0.5
             :max-budget-usd nil :max-turns nil
             :permission-mode "plan" :worktree t :sentinel "S"
             :extra "ignored"))
        (b '(:max-iterations 10 :max-budget-usd-per-iter 0.5
             :max-budget-usd nil :max-turns nil
             :permission-mode "plan" :worktree t :sentinel "S"
             :extra "differs")))
    (should (beads-ralph-launcher--params-equal-p a b))))

;;; --params-to-start-args

(ert-deftest beads-ralph-launcher-test-start-args-issue-and-kind ()
  "`:issue' and `:kind' are always present."
  (let ((args (beads-ralph-launcher--params-to-start-args
               "bde-x" 'epic '(:max-iterations 5) nil)))
    (should (equal "bde-x" (plist-get args :issue)))
    (should (eq 'epic (plist-get args :kind)))
    (should (equal 5 (plist-get args :max-iterations)))))

(ert-deftest beads-ralph-launcher-test-start-args-drops-nil-numerics ()
  "Nil-valued numeric keys are NOT serialized so `-start' picks defcustoms."
  (let ((args (beads-ralph-launcher--params-to-start-args
               "bde-x" 'issue
               '(:max-iterations nil :max-budget-usd-per-iter nil
                 :max-budget-usd nil :max-turns nil
                 :permission-mode nil)
               nil)))
    (should-not (plist-member args :max-iterations))
    (should-not (plist-member args :max-budget-usd-per-iter))
    (should-not (plist-member args :max-budget-usd))
    (should-not (plist-member args :max-turns))
    (should-not (plist-member args :permission-mode))))

(ert-deftest beads-ralph-launcher-test-start-args-prompt-override ()
  "PROMPT-OVERRIDE (when non-nil) lands in `:prompt'."
  (let ((args (beads-ralph-launcher--params-to-start-args
               "bde-x" 'issue '() "Hello, Ralph")))
    (should (equal "Hello, Ralph" (plist-get args :prompt))))
  (let ((args (beads-ralph-launcher--params-to-start-args
               "bde-x" 'issue '() nil)))
    (should-not (plist-member args :prompt))))

;;; --params-to-preview-args + argv parity

(ert-deftest beads-ralph-launcher-test-preview-args-prefers-per-iter-budget ()
  "When both per-iter and total budgets are set, per-iter wins for the
argv `--max-budget-usd' flag (which claude reads per-spawn)."
  (let* ((params '(:max-budget-usd-per-iter 0.50
                   :max-budget-usd 5.0
                   :max-turns nil
                   :permission-mode "plan"))
         (args (beads-ralph-launcher--params-to-preview-args
                "bde-x" 'issue params "P")))
    (should (equal 0.50 (plist-get args :max-budget-usd)))))

(ert-deftest beads-ralph-launcher-test-preview-args-falls-back-to-total ()
  "When per-iter is nil, the preview falls back to the total budget."
  (let* ((params '(:max-budget-usd-per-iter nil
                   :max-budget-usd 5.0
                   :max-turns nil
                   :permission-mode "plan"))
         (args (beads-ralph-launcher--params-to-preview-args
                "bde-x" 'issue params "P")))
    (should (equal 5.0 (plist-get args :max-budget-usd)))))

(ert-deftest beads-ralph-launcher-test-argv-round-trip ()
  "Launcher params → preview args → `--preview-argv' produces the
expected token list (permission-mode + add-dir + budget honored)."
  (let* ((default-directory "/tmp/")
         (params '(:max-iterations 7
                   :max-budget-usd-per-iter 0.5
                   :max-budget-usd nil
                   :max-turns 6
                   :permission-mode "acceptEdits"))
         (preview (beads-ralph-launcher--params-to-preview-args
                   "bde-x" 'issue params "PROMPT-TEXT"))
         (argv (beads-agent-ralph--preview-argv preview)))
    (should (member "claude" argv))
    (should (member "--permission-mode" argv))
    (should (member "acceptEdits" argv))
    (should (member "--max-budget-usd" argv))
    (should (member "0.5" argv))
    (should (member "--max-turns" argv))
    (should (member "6" argv))
    (should (equal "PROMPT-TEXT" (car (last argv))))))

;;; --save-template / persistence

(ert-deftest beads-ralph-launcher-test-save-template-replaces-existing ()
  "Saving a template for an existing ROOT-ID replaces the prior entry."
  (beads-ralph-launcher-test--with-clean-templates
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (_sym _val) nil)))
      (beads-ralph-launcher--save-template "bde-x" '(:max-iterations 1))
      (beads-ralph-launcher--save-template "bde-x" '(:max-iterations 99))
      (should (equal '(:max-iterations 99)
                     (cdr (assoc "bde-x" beads-agent-ralph-launch-templates))))
      (should (= 1 (cl-count "bde-x" beads-agent-ralph-launch-templates
                             :key #'car :test #'equal))))))

(ert-deftest beads-ralph-launcher-test-save-template-most-recent-first ()
  "Newly-saved templates are inserted at the head of the alist."
  (beads-ralph-launcher-test--with-clean-templates
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (_sym _val) nil)))
      (beads-ralph-launcher--save-template "bde-a" '(:max-iterations 1))
      (beads-ralph-launcher--save-template "bde-b" '(:max-iterations 2))
      (should (equal "bde-b" (caar beads-agent-ralph-launch-templates))))))

(ert-deftest beads-ralph-launcher-test-save-template-calls-customize-save ()
  "`customize-save-variable' is invoked with the right symbol/value."
  (beads-ralph-launcher-test--with-clean-templates
    (let (saved-call)
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (sym val) (setq saved-call (list sym val)))))
        (beads-ralph-launcher--save-template "bde-x" '(:max-iterations 5))
        (should (eq 'beads-agent-ralph-launch-templates (car saved-call)))
        (should (equal beads-agent-ralph-launch-templates
                       (cadr saved-call)))))))

(ert-deftest beads-ralph-launcher-test-save-template-deep-copy ()
  "Mutating the source params after save does not mutate the stored template."
  (beads-ralph-launcher-test--with-clean-templates
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (_sym _val) nil)))
      (let ((params (list :max-iterations 10 :max-budget-usd-per-iter 0.5)))
        (beads-ralph-launcher--save-template "bde-x" params)
        ;; Mutate the caller's plist; the stored copy must not change.
        (plist-put params :max-iterations 999)
        (should (equal 10
                       (plist-get (cdr (assoc "bde-x"
                                              beads-agent-ralph-launch-templates))
                                  :max-iterations)))))))

;;; --params-for

(ert-deftest beads-ralph-launcher-test-params-for-prefers-saved-template ()
  "A saved template for ROOT-ID overrides defaults."
  (beads-ralph-launcher-test--with-clean-templates
    (setq beads-agent-ralph-launch-templates
          '(("bde-x" :max-iterations 42)))
    (should (equal 42
                   (plist-get (beads-ralph-launcher--params-for "bde-x")
                              :max-iterations)))))

(ert-deftest beads-ralph-launcher-test-params-for-falls-back-to-defaults ()
  "Without a saved template, `--params-for' returns the default params."
  (beads-ralph-launcher-test--with-clean-templates
    (let ((beads-agent-ralph-max-iterations 13)
          (beads-ralph-launcher-default-params nil))
      (should (equal 13
                     (plist-get (beads-ralph-launcher--params-for "bde-x")
                                :max-iterations))))))

;;; --incumbent

(ert-deftest beads-ralph-launcher-test-incumbent-returns-live-controller ()
  "A registered non-terminal controller is returned by `--incumbent'."
  (beads-ralph-launcher-test--with-clean-registry
    (let ((c (beads-ralph-launcher-test--make-controller
              :root-id "bde-x" :status 'running)))
      (beads-agent-ralph--register-controller c)
      (should (eq c (beads-ralph-launcher--incumbent "bde-x"))))))

(ert-deftest beads-ralph-launcher-test-incumbent-nil-on-terminal ()
  "Terminal controllers are not treated as incumbents."
  (beads-ralph-launcher-test--with-clean-registry
    (let ((c (beads-ralph-launcher-test--make-controller
              :root-id "bde-x" :status 'done)))
      (beads-agent-ralph--register-controller c)
      (should-not (beads-ralph-launcher--incumbent "bde-x")))))

(ert-deftest beads-ralph-launcher-test-incumbent-nil-when-absent ()
  "No registered controller → nil."
  (beads-ralph-launcher-test--with-clean-registry
    (should-not (beads-ralph-launcher--incumbent "bde-x"))))

;;; Buffer setup + entry point

(ert-deftest beads-ralph-launcher-test-buffer-seeds-state ()
  "Opening the launcher sets root-id / kind / params / baseline correctly."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (beads-ralph-launcher-test--with-launcher-buffer buf "bde-x"
        (with-current-buffer buf
          (should (eq major-mode 'beads-ralph-launcher-mode))
          (should (equal "bde-x" beads-ralph-launcher--root-id))
          (should (eq 'issue beads-ralph-launcher--kind))
          (should beads-ralph-launcher--params)
          (should (beads-ralph-launcher--params-equal-p
                   beads-ralph-launcher--params
                   beads-ralph-launcher--baseline-params))
          (should-not beads-ralph-launcher--prompt-override)
          (should-not beads-ralph-launcher--shell-expanded))))))

(ert-deftest beads-ralph-launcher-test-buffer-renders-header ()
  "The buffer contains a rendered header line with the root id."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (beads-ralph-launcher-test--with-launcher-buffer buf "bde-renders"
        (with-current-buffer buf
          (should (string-match-p "Launch Ralph on bde-renders"
                                  (buffer-string))))))))

(ert-deftest beads-ralph-launcher-test-buffer-renders-already-running-banner ()
  "An incumbent controller for the root surfaces the warning banner."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (let ((c (beads-ralph-launcher-test--make-controller
                :root-id "bde-busy" :status 'running :iter 3 :max-iter 20)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-launcher-test--with-launcher-buffer buf "bde-busy"
          (with-current-buffer buf
            (should (string-match-p "Already running for this root"
                                    (buffer-string)))))))))

(ert-deftest beads-ralph-launcher-test-buffer-renders-cap-banner ()
  "When the concurrent-loops cap is reached, the cap banner is shown."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (let ((beads-agent-ralph-max-concurrent-loops 1)
            (c (beads-ralph-launcher-test--make-controller
                :root-id "bde-other" :status 'running)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-launcher-test--with-launcher-buffer buf "bde-new"
          (with-current-buffer buf
            (should (string-match-p "Max concurrent Ralph loops"
                                    (buffer-string)))))))))

;;; Launch refusal paths

(ert-deftest beads-ralph-launcher-test-launch-refuses-on-incumbent ()
  "Calling `-launch' while an incumbent exists signals a `user-error'."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (let ((c (beads-ralph-launcher-test--make-controller
                :root-id "bde-busy" :status 'running)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-launcher-test--with-launcher-buffer buf "bde-busy"
          (with-current-buffer buf
            (should-error (beads-ralph-launcher-launch)
                          :type 'user-error)))))))

(ert-deftest beads-ralph-launcher-test-launch-refuses-on-cap ()
  "Calling `-launch' when the cap is reached signals a `user-error'."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (let ((beads-agent-ralph-max-concurrent-loops 1)
            (c (beads-ralph-launcher-test--make-controller
                :root-id "bde-other" :status 'running)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-launcher-test--with-launcher-buffer buf "bde-new"
          (with-current-buffer buf
            (should-error (beads-ralph-launcher-launch)
                          :type 'user-error)))))))

;;; --param-keys

(ert-deftest beads-ralph-launcher-test-param-keys-has-seven-keys ()
  "The canonical list holds the seven documented launcher keys."
  (should (= 7 (length beads-ralph-launcher--param-keys)))
  (dolist (k '(:max-iterations :max-budget-usd-per-iter :max-budget-usd
               :max-turns :permission-mode :worktree :sentinel))
    (should (memq k beads-ralph-launcher--param-keys))))

(ert-deftest beads-ralph-launcher-test-default-params-uses-all-canonical-keys ()
  "`--default-params' returns a plist containing every canonical key."
  (let ((p (beads-ralph-launcher--default-params)))
    (dolist (k beads-ralph-launcher--param-keys)
      (should (plist-member p k)))))

;;; Prompt-override roundtrip

(ert-deftest beads-ralph-launcher-test-prompt-override-renders ()
  "After `--prompt-override' is set, the preview contains its text."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (beads-ralph-launcher-test--with-launcher-buffer buf "bde-px"
        (with-current-buffer buf
          (setq-local beads-ralph-launcher--prompt-override
                      "OVERRIDDEN_PROMPT_TOKEN")
          (beads-ralph-launcher--render buf)
          (should (string-match-p "OVERRIDDEN_PROMPT_TOKEN"
                                  (buffer-string))))))))

;;; Entry-point validation

(ert-deftest beads-ralph-launcher-test-entry-rejects-empty-root-id ()
  "`beads-ralph-launcher' refuses an empty ROOT-ID."
  (should-error (beads-ralph-launcher "") :type 'user-error))

(ert-deftest beads-ralph-launcher-test-entry-rejects-non-string-root-id ()
  "`beads-ralph-launcher' refuses a non-string ROOT-ID."
  (should-error (beads-ralph-launcher nil) :type 'user-error)
  (should-error (beads-ralph-launcher 42) :type 'user-error))

;;; Mode guard

(ert-deftest beads-ralph-launcher-test-interactive-commands-refuse-outside-mode ()
  "Interactive commands signal `user-error' when called outside a launcher buffer."
  (with-temp-buffer
    (should-error (beads-ralph-launcher-launch) :type 'user-error)
    (should-error (beads-ralph-launcher-edit-prompt) :type 'user-error)
    (should-error (beads-ralph-launcher-save-template) :type 'user-error)
    (should-error (beads-ralph-launcher-quit) :type 'user-error)
    (should-error (beads-ralph-launcher-refresh) :type 'user-error)
    (should-error (beads-ralph-launcher-switch-to-incumbent) :type 'user-error)
    (should-error (beads-ralph-launcher-force-relaunch) :type 'user-error)))

;;; Shell toggle

(ert-deftest beads-ralph-launcher-test-shell-toggle-expands-argv ()
  "After `--shell-expanded' is set, the argv preview text appears."
  (beads-ralph-launcher-test--with-clean-registry
    (beads-ralph-launcher-test--with-clean-templates
      (beads-ralph-launcher-test--with-launcher-buffer buf "bde-shell"
        (with-current-buffer buf
          (setq-local beads-ralph-launcher--shell-expanded t)
          (beads-ralph-launcher--render buf)
          (should (string-match-p "claude" (buffer-string)))
          (should (string-match-p "--permission-mode" (buffer-string))))))))

(provide 'beads-ralph-launcher-test)

;;; beads-ralph-launcher-test.el ends here
