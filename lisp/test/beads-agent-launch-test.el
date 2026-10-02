;;; beads-agent-launch-test.el --- WI-12 agent-launch redesign tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; Unit tests for the WI-12 agent-launch redesign (mockup §8):
;;
;;   - the role → target → backend → prompt launch flow and live footer
;;   - Review QA mode (QA is Review + QA prompt, not a separate role)
;;   - the prompt preview buffer (system + user envelopes)
;;   - session attach/jump/stop dispatch
;;   - the lifecycle hook the session list refreshes from
;;
;; All tests are :unit — `beads-command-execute' is mocked, so no `bd'
;; binary is required.

;;; Code:

(require 'ert)
(require 'beads-agent)
(require 'beads-agent-types)
(require 'beads-agent-list)
(require 'beads-types)

;;; Helpers

(defmacro beads-agent-launch-test--with-state (&rest body)
  "Run BODY with the launch state bound to fresh defaults."
  (declare (indent 0))
  `(let ((beads-agent-launch--issue-id "be-abcd")
         (beads-agent-launch--role "Task")
         (beads-agent-launch--qa-mode nil)
         (beads-agent-launch--backend nil)
         (beads-agent-launch--worktree-name "be-abcd")
         (beads-agent-launch--worktree-path nil)
         (beads-agent-launch--prompts nil))
     (cl-letf (((symbol-function 'transient--redisplay) #'ignore))
       ,@body)))

(defclass beads-agent-launch-test--backend (beads-agent-backend) ()
  "Concrete backend used only by the launch tests.")

(defun beads-agent-launch-test--make-backend (name &optional description)
  "Build a fake backend object NAMEd NAME for registry tests."
  (beads-agent-launch-test--backend
   :name name :priority 1 :description (or description name)))

;;; Launch-flow state and footer (mockup §8a/§8b)

(ert-deftest beads-agent-launch-test-default-roster ()
  "The launch roster resolves Task / Review / Plan by name."
  (beads-agent-launch-test--with-state
    (should (object-of-class-p (beads-agent-launch--effective-type)
                               'beads-agent-type-task))
    (setq beads-agent-launch--role "Review")
    (should (object-of-class-p (beads-agent-launch--effective-type)
                               'beads-agent-type-review))
    (setq beads-agent-launch--role "Plan")
    (should (object-of-class-p (beads-agent-launch--effective-type)
                               'beads-agent-type-plan))))

(ert-deftest beads-agent-launch-test-role-selection-sets-state ()
  "Role suffixes update the launch state and clear cached prompts."
  (beads-agent-launch-test--with-state
    (setq beads-agent-launch--prompts (cons "s" "u"))
    (beads-agent-start--role-review)
    (should (equal beads-agent-launch--role "Review"))
    (should-not beads-agent-launch--qa-mode)
    (should-not beads-agent-launch--prompts)
    (beads-agent-start--role-plan)
    (should (equal beads-agent-launch--role "Plan"))
    (beads-agent-start--role-task)
    (should (equal beads-agent-launch--role "Task"))))

(ert-deftest beads-agent-launch-test-footer-shows-selection ()
  "The footer mirrors sling: role · target · backend (mockup §8a)."
  (beads-agent-launch-test--with-state
    (setq beads-agent-launch--backend "claude-code")
    (let ((footer (beads-agent-launch--footer)))
      (should (string-match-p "Task" footer))
      (should (string-match-p "worktree be-abcd" footer))
      (should (string-match-p "claude-code" footer)))))

;;; Review QA mode (mockup §8c)

(ert-deftest beads-agent-launch-test-review-qa-toggle ()
  "Toggling QA mode turns Review into the Review+QA prompt instance."
  (beads-agent-launch-test--with-state
    (setq beads-agent-launch--role "Review")
    (beads-agent-start--toggle-qa)
    (should beads-agent-launch--qa-mode)
    (let ((type (beads-agent-launch--effective-type)))
      (should (object-of-class-p type 'beads-agent-type-review))
      (should (oref type qa-mode)))
    ;; The label distinguishes the mode; the session stays Review.
    (should (string-match-p "QA mode" (beads-agent-launch--role-label)))
    (beads-agent-start--toggle-qa)
    (should-not beads-agent-launch--qa-mode)))

(ert-deftest beads-agent-launch-test-qa-mode-prompt-is-qa ()
  "Review in QA mode uses the relocated QA prompt, not the Review prompt."
  (let* ((type (beads-agent-type-review-qa))
         (issue (beads-issue :id "be-1" :title "T" :description "D"))
         (system (beads-agent-type-system-prompt type issue))
         (user (beads-agent-type-build-user-prompt type issue)))
    (should (string-match-p "QA agent" system))
    (should (string-match-p "QA Summary" user))
    ;; The plain Review role keeps the review prompt.
    (let* ((plain (beads-agent-type-get "Review"))
           (plain-system (beads-agent-type-system-prompt plain issue)))
      (should (string-match-p "code review agent" plain-system)))))

;;; Target selection (mockup §8a/§8b)

(ert-deftest beads-agent-launch-test-derived-target ()
  "The derived target seeds the worktree name from the issue."
  (beads-agent-launch-test--with-state
    (setq beads-agent-launch--worktree-name nil)
    (beads-agent-start--target-derived)
    (should (equal beads-agent-launch--worktree-name "be-abcd"))
    (should-not beads-agent-launch--worktree-path)))

;;; Backend curation (slimming.md §3.3)

(ert-deftest beads-agent-launch-test-read-backend-curated ()
  "`b' offers curated backends first and resolves a curated choice."
  (cl-letf (((symbol-function 'beads-agent--curated-backends)
             (lambda () (list (beads-agent-launch-test--make-backend "claude-code")
                              (beads-agent-launch-test--make-backend "agent-shell"))))
            ((symbol-function 'beads-agent--other-backends)
             (lambda () (list (beads-agent-launch-test--make-backend "eca"))))
            ((symbol-function 'completing-read)
             (lambda (_prompt choices &rest _)
               (should (member "claude-code" choices))
               (should (member "… other" choices))
               "claude-code")))
    (should (equal "claude-code" (beads-agent-launch--read-backend)))))

(ert-deftest beads-agent-launch-test-read-backend-other-overflow ()
  "Choosing `… other' reaches a demoted backend via the second read."
  (let ((calls 0))
    (cl-letf (((symbol-function 'beads-agent--curated-backends)
               (lambda () (list (beads-agent-launch-test--make-backend "claude-code"))))
              ((symbol-function 'beads-agent--other-backends)
               (lambda () (list (beads-agent-launch-test--make-backend "eca"))))
              ((symbol-function 'completing-read)
               (lambda (prompt choices &rest _)
                 (setq calls (1+ calls))
                 (if (= calls 1)
                     "… other"
                   (should (member "eca" choices))
                   "eca"))))
      (should (equal "eca" (beads-agent-launch--read-backend)))
      (should (= 2 calls)))))

(ert-deftest beads-agent-launch-test-read-backend-auto ()
  "Choosing `auto' returns nil so the preferred backend is selected."
  (cl-letf (((symbol-function 'beads-agent--curated-backends)
             (lambda () (list (beads-agent-launch-test--make-backend "claude-code"))))
            ((symbol-function 'beads-agent--other-backends) (lambda () nil))
            ((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _) "auto")))
    (should-not (beads-agent-launch--read-backend))))

;;; Prompt preview (mockup §8d)

(ert-deftest beads-agent-launch-test-prompt-preview-buffer ()
  "Preview renders the system role prompt and the user envelope."
  (let* ((issue (beads-issue :id "be-abcd" :title "Add formula browser"
                             :description "Some description"))
         (buf (beads-buffer-utility "prompt-preview" "be-abcd")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (_cmd &rest _args) (list issue)))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (beads-agent-prompt-preview "be-abcd" "Task")
          (with-current-buffer buf
            (let ((body (buffer-string)))
              (should (string-match-p "System (role)" body))
              (should (string-match-p "User (issue envelope)" body))
              (should (string-match-p "be-abcd" body))
              (should (derived-mode-p 'beads-agent-prompt-preview-mode)))))
      (when (buffer-live-p (get-buffer buf))
        (kill-buffer buf)))))

(ert-deftest beads-agent-launch-test-prompt-preview-requires-issue ()
  "Preview fails clearly when the issue cannot be fetched."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd &rest _args) nil)))
    (should-error (beads-agent-prompt-preview "be-nope" "Task")
                  :type 'user-error)))

;;; Attach / jump / stop dispatch (mockup §9)

(ert-deftest beads-agent-launch-test-attach-uses-terminal ()
  "With `beads-terminal-attach' available, attach routes the session to it."
  (let ((attached nil)
        (session (beads-agent-session
                  :id "be-abcd" :issue-id "be-abcd"
                  :backend-name "mock" :project-dir "/tmp"
                  :started-at "2026-01-01T00:00:00Z")))
    (cl-letf (((symbol-function 'beads-terminal-attach)
               (lambda (s) (setq attached s)))
              ((symbol-function 'beads-agent--get-session)
               (lambda (id) (when (equal id "be-abcd") session))))
      (beads-agent-attach "be-abcd")
      (should (eq attached session)))))

(ert-deftest beads-agent-launch-test-list-attach-uses-agent-attach ()
  "`beads-agent-list-attach' routes the session through attach."
  (let ((attached nil))
    (cl-letf (((symbol-function 'beads-agent-list--current-session-id)
               (lambda () "sess-1"))
              ((symbol-function 'beads-agent-attach)
               (lambda (id) (setq attached id))))
      (beads-agent-list-attach)
      (should (equal "sess-1" attached)))))

(ert-deftest beads-agent-launch-test-list-stop-and-jump-delegate ()
  "The list stop/jump commands delegate to the agent backend layer."
  (let ((stopped nil))
    (cl-letf (((symbol-function 'beads-agent-list--current-session-id)
               (lambda () "sess-1"))
              ((symbol-function 'beads-agent-list-refresh) #'ignore)
              ((symbol-function 'beads-agent-stop)
               (lambda (id) (setq stopped id))))
      (beads-agent-list-stop)
      (should (equal "sess-1" stopped))))
  (let ((jumped nil))
    (cl-letf (((symbol-function 'beads-agent-list--current-session-id)
               (lambda () "sess-1"))
              ((symbol-function 'beads-agent-jump)
               (lambda (id) (setq jumped id))))
      (beads-agent-list-jump)
      (should (equal "sess-1" jumped)))))

;;; Lifecycle hook (mockup §8e)

(ert-deftest beads-agent-launch-test-lifecycle-hook-fires ()
  "A state transition fans the lifecycle hook out with action + session."
  (let* ((seen nil)
         (session (beads-agent-session
                   :id "sess-1" :issue-id "be-abcd"
                   :backend-name "claude-code" :project-dir "/tmp"
                   :agent-type-name "Task" :instance-number 1
                   :started-at "2026-01-01T00:00:00Z"))
         (fn (lambda (action sess) (push (cons action sess) seen))))
    ;; Isolate the hook list: the real `beads-sesman' handler registers
    ;; the session with sesman, which would leak across test files.
    (let ((beads-agent-state-change-hook (list fn)))
      (beads-agent--run-state-change-hook 'started session)
      (should (equal (car seen) (cons 'started session))))))

(provide 'beads-agent-launch-test)

;;; beads-agent-launch-test.el ends here
