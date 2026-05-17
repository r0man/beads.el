;;; beads-agent-phase-1a-i-test.el --- Phase 1a-i acceptance gates -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Acceptance gates for Phase 1a-i of the terminal-spawned agent
;; backend epic (bde-xle9.1): the mechanical 4-arity
;; `beads-agent-backend-start' migration, the
;; `beads-agent-type-build-prompt' -> `-build-user-prompt' rename, the
;; new `beads-agent-type-system-prompt' generic, and the prompt-edit
;; (nil nil) cancel sentinel.
;;
;; The defining property of this phase is "zero output change": the
;; rendered prompt strings must be byte-identical to the parent
;; commit.  The golden fixture lisp/test/golden/prompts-pre.eld was
;; captured on the parent commit; this file re-renders post-change and
;; asserts byte-identity.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent)
(require 'beads-agent-backend)
(require 'beads-agent-type)
(require 'beads-agent-types)
(require 'beads-agent-mock)
(require 'beads-types)

(defun beads-agent-phase-1a-i-test--golden-path ()
  "Return the absolute path to the committed golden prompt fixture."
  (expand-file-name
   "golden/prompts-pre.eld"
   (file-name-directory
    (or load-file-name buffer-file-name
        (locate-library "beads-agent-phase-1a-i-test")))))

(defun beads-agent-phase-1a-i-test--read-golden ()
  "Read and return the golden prompt fixture alist."
  (with-temp-buffer
    (insert-file-contents (beads-agent-phase-1a-i-test--golden-path))
    (goto-char (point-min))
    (read (current-buffer))))

;;; Golden-fixture byte-identical gate

(ert-deftest beads-agent-phase-1a-i-test-golden-byte-identical ()
  "Re-rendered prompts are byte-identical to the parent-commit fixture.
SYSTEM must be nil for every built-in type and the fallback in
Phase 1a-i (defaults frozen, slots still nil); USER must equal the
old `beads-agent-type-build-prompt' output verbatim."
  (beads-agent-types-register-builtin)
  (let* ((golden (beads-agent-phase-1a-i-test--read-golden))
         (issue (beads-issue :id "bde-TEST"
                             :title "Synthetic golden issue"
                             :description "Golden description body."
                             :acceptance-criteria "Golden acceptance criteria.")))
    (should golden)
    (dolist (entry golden)
      (let* ((name (car entry))
             (want-sys (cadr entry))
             (want-user (cddr entry)))
        (if (equal name "__fallback__")
            (progn
              (should (null want-sys))
              (should (equal (beads-agent--build-prompt issue) want-user)))
          (let ((type (beads-agent-type-get name)))
            (should type)
            ;; Frozen: every type's system prompt is nil in 1a-i.
            (should (null (beads-agent-type-system-prompt type issue)))
            (should (equal want-sys nil))
            (should (equal (beads-agent-type-build-user-prompt type issue)
                           want-user))))))))

;;; 4-arity protocol introspection

(ert-deftest beads-agent-phase-1a-i-test-every-start-method-is-4-arity ()
  "Every `beads-agent-backend-start' method specializes on 4 args.
Uses cl-generic method introspection (not a grep) so a multi-line
`cl-defmethod' arglist cannot hide a stale 3-arity implementer."
  (require 'cl-generic)
  (let* ((gen (cl--generic 'beads-agent-backend-start))
         (methods (cl--generic-method-table gen)))
    (should methods)
    (dolist (m methods)
      (let ((specializers (cl--generic-method-specializers m)))
        ;; Specializer count == number of dispatched args; the
        ;; signature is (backend issue system-prompt user-prompt) so
        ;; the method must accept 4 args.  cl-defmethod records one
        ;; specializer per declared arg (t for the non-typed ones).
        (should (= 4 (length specializers)))))))

;; A synthetic out-of-tree subclass that did NOT migrate: it provides
;; no method of its own, so dispatch falls through to the abstract
;; base catch-all.  (The catch-all itself is the thing under test.)
(defclass beads-agent-backend-stale-3arity (beads-agent-backend)
  ((name :initform "stale"))
  :documentation "Synthetic un-migrated backend for the signaling-default gate.")

(cl-defmethod beads-agent-backend-available-p
  ((_b beads-agent-backend-stale-3arity))
  "Always available; only `backend-start' dispatch is under test."
  t)

(ert-deftest beads-agent-phase-1a-i-test-signaling-default-plain-error ()
  "An out-of-tree 3-arity subclass hits the plain-error catch-all.
Asserts the message names the offending class and carries the
migration recipe, and that the raised condition is a plain `error'
\(not `cl-no-applicable-method')."
  (let* ((backend (beads-agent-backend-stale-3arity))
         (issue (beads-issue :id "bde-TEST" :title "t" :description "d")))
    (let ((err (should-error
                (beads-agent-backend-start backend issue nil "user")
                :type 'error)))
      ;; Not the cryptic dispatch failure.
      (should-not (eq (car err) 'cl-no-applicable-method))
      (let ((msg (error-message-string err)))
        (should (string-match-p "4-arity beads-agent-backend-start" msg))
        (should (string-match-p "beads-agent-backend-stale-3arity" msg))
        (should (string-match-p "See NEWS" msg))))))

;;; combine-prompt helper (empty-string unification)

(ert-deftest beads-agent-phase-1a-i-test-combine-prompt ()
  "`beads-agent-backend--combine-prompt' treats empty/nil sys as absent."
  ;; No system prompt -> user prompt unchanged (byte-identical path).
  (should (equal (beads-agent-backend--combine-prompt nil "u") "u"))
  (should (equal (beads-agent-backend--combine-prompt "" "u") "u"))
  ;; Non-empty system prompt -> prepended with a single blank line.
  (should (equal (beads-agent-backend--combine-prompt "S" "u") "S\n\nu"))
  ;; Empty user prompt with a system prompt -> no stray blank line.
  (should (equal (beads-agent-backend--combine-prompt "S" "") "S"))
  (should (equal (beads-agent-backend--combine-prompt "S" nil) "S"))
  ;; Both absent -> nil (no "\n\n").
  (should (null (beads-agent-backend--combine-prompt nil nil))))

;;; Cancel-sentinel regression lock

(ert-deftest beads-agent-phase-1a-i-test-cancel-sentinel-aborts ()
  "(callback nil nil) is the cancel sentinel: the start flow aborts.
Drives the real orchestrator closure via a stubbed
`beads-agent-prompt-edit-show' and asserts `beads-agent--continue-start'
is never reached."
  (let ((continue-called nil))
    (cl-letf (((symbol-function 'beads-agent--fetch-issue-async)
               (lambda (id callback)
                 (funcall callback (beads-issue :id id :title "Test"))))
              ((symbol-function 'beads-agent-type-get)
               (lambda (_name) (beads-agent-type-task)))
              ((symbol-function 'beads-agent-type-build-user-prompt)
               (lambda (_type _issue) "the user prompt"))
              ((symbol-function 'beads-agent-prompt-edit-show)
               (lambda (_issue-id _prompt _type callback)
                 ;; Cancel sentinel.
                 (funcall callback nil nil)))
              ((symbol-function 'beads-agent--continue-start)
               (lambda (&rest _) (setq continue-called t))))
      (let ((backend (beads-agent-backend-mock)))
        (beads-agent--start-with-worktree "bde-TEST" backend
                                          "/project" "/worktree")
        (should-not continue-called)))))

(ert-deftest beads-agent-phase-1a-i-test-cancel-sentinel-proceeds ()
  "(callback nil USER) is NOT the cancel sentinel: the start proceeds.
USER non-nil with SYSTEM nil means \"default system, real user\" and
must reach `beads-agent--continue-start'."
  (let ((continue-args nil))
    (cl-letf (((symbol-function 'beads-agent--fetch-issue-async)
               (lambda (id callback)
                 (funcall callback (beads-issue :id id :title "Test"))))
              ((symbol-function 'beads-agent-type-get)
               (lambda (_name) (beads-agent-type-task)))
              ((symbol-function 'beads-agent-type-build-user-prompt)
               (lambda (_type _issue) "the user prompt"))
              ((symbol-function 'beads-agent-prompt-edit-show)
               (lambda (_issue-id _prompt _type callback)
                 (funcall callback nil "the user prompt")))
              ((symbol-function 'beads-agent--continue-start)
               (lambda (&rest args) (setq continue-args args))))
      (let ((backend (beads-agent-backend-mock)))
        (beads-agent--start-with-worktree "bde-TEST" backend
                                          "/project" "/worktree")
        (should continue-args)
        ;; The user prompt is threaded through as the prompt argument.
        (should (member "the user prompt" continue-args))))))

(provide 'beads-agent-phase-1a-i-test)
;;; beads-agent-phase-1a-i-test.el ends here
