;;; beads-sling-test.el --- Tests for the standalone sling abstraction -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, agents

;;; Commentary:

;; Unit tests for the standalone sling abstraction (WI-10, design.md
;; §4.5 and §8, REQ-008, REQ-021).  Covers default target collection,
;; table-driven `beads-sling-shape' inference, the named-backend
;; registry, validator collection and the default dispatch method that
;; launches a local agent through a stubbed boundary.  The launch
;; boundary (`beads-agent-start' / `beads-agent--start-with-worktree')
;; is always stubbed, so no `bd' invocation or real agent is required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-sling)

;;; Test fixtures

(defclass beads-sling-test-backend (beads-agent-backend)
  ((name :initform "test-backend")
   (description :initform "Stub backend for sling tests"))
  "Minimal concrete agent backend used to exercise target collection.")

(ert-deftest beads-sling-test-empty-hook-yields-nil ()
  "An empty `beads-sling-target-functions' is the standalone no-op."
  :tags '(:unit)
  (let ((beads-sling-target-functions nil))
    (should-not (beads-sling-targets))))

(ert-deftest beads-sling-test-default-targets ()
  "Default collection yields role, backend and worktree targets."
  :tags '(:unit)
  (let ((beads-sling-target-functions (list #'beads-sling--default-targets)))
    (cl-letf (((symbol-function 'beads-git-get-project-name)
               (lambda () "beads.el"))
              ((symbol-function 'beads-agent-type-list)
               (lambda ()
                 (list (beads-agent-type-task)
                       (beads-agent-type-review))))
              ((symbol-function 'beads-agent--get-available-backends)
               (lambda () (list (beads-sling-test-backend))))
              ((symbol-function 'beads-git-list-worktrees)
               (lambda ()
                 '(("/proj/worktrees/be-abc" "be-abc")
                   ("/proj/worktrees/be-def" nil)))))
      (let* ((targets (beads-sling-targets))
             (names (mapcar (lambda (target) (oref target name)) targets))
             (by-name (lambda (name)
                        (cl-find name targets
                                 :key (lambda (target) (oref target name))
                                 :test #'equal))))
        (should (= 5 (length targets)))
        ;; Roles lead; names are namespaced under the project label.
        (should (member "beads.el/task" names))
        (should (member "beads.el/review" names))
        ;; One agent backend target carries the backend slot verbatim.
        (should (member "beads.el/test-backend" names))
        (should (equal (oref (funcall by-name "beads.el/test-backend")
                             backend)
                       "test-backend"))
        ;; One worktree target per existing worktree.
        (should (member "worktree: be-abc" names))
        (should (member "worktree: be-def" names))
        (should (equal (oref (funcall by-name "worktree: be-abc") kind)
                       'worktree))
        (should (equal (plist-get (oref (funcall by-name "worktree: be-abc")
                                        metadata)
                                  :path)
                       "/proj/worktrees/be-abc"))))))

(ert-deftest beads-sling-test-targets-dedupe-first-wins ()
  "A later provider cannot shadow a target name already collected.
The default provider runs first, so its target wins over the extension."
  :tags '(:unit)
  (let* ((default (beads-sling-target :name "shared" :kind 'role
                                      :description "default"))
         (extension (beads-sling-target :name "shared" :kind 'city
                                        :description "extension"))
         (beads-sling-target-functions
          (list (lambda () (list default))
                (lambda () (list extension)))))
    (let ((targets (beads-sling-targets)))
      (should (= 1 (length targets)))
      (should (eq (car targets) default))
      (should (equal (oref (car targets) description) "default")))))

(ert-deftest beads-sling-test-targets-call-providers-without-args ()
  "Providers are called with no arguments (design.md §4.5 contract)."
  :tags '(:unit)
  (let* ((called nil)
         (beads-sling-target-functions
          (list (lambda () (setq called t) nil))))
    (should-not (beads-sling-targets "be-abcd"))
    (should called)))

(ert-deftest beads-sling-test-shape ()
  "Shape inference is the pure table from design.md §8."
  :tags '(:unit)
  (dolist (case '((nil nil nil)
                  ("be-abcd" nil plain)
                  (nil "build-basic" formula)
                  ("be-abcd" "build-basic" on)
                  ("" nil nil)
                  ("" "build-basic" formula)))
    (pcase-let ((`(,work ,formula ,expected) case))
      (should (eq (beads-sling-shape work formula) expected)))))

(ert-deftest beads-sling-test-backend-register-get-list ()
  "Named backends round-trip through the registry."
  :tags '(:unit)
  (unwind-protect
      (progn
        (should-not (beads-sling-backend-get "test"))
        (let ((backend (beads-sling-backend
                        :name "test"
                        :description "Test backend"
                        :dispatch (lambda (_t _b _p) 'ignored))))
          (should (eq (beads-sling-backend-register backend) backend))
          (should (eq (beads-sling-backend-get "test") backend))
          (should (eq (beads-sling-backend-get 'test) backend))
          (should (member backend (beads-sling-backend-list)))))
    (remhash "test" beads-sling--backends)))

(ert-deftest beads-sling-test-backend-register-rejects-non-backend ()
  "Registering a non-backend signals an error."
  :tags '(:unit)
  (should-error (beads-sling-backend-register "not-a-backend")
                :type 'error))

(ert-deftest beads-sling-test-dispatch-local ()
  "Default dispatch for a local target starts a local agent."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-agent-start)
               (lambda (&rest args) (setq captured args) 'session)))
      (let ((target (beads-sling-target :name "beads.el/task"
                                        :kind 'role
                                        :backend "local")))
        (should (eq (beads-sling-dispatch target "be-abcd" "do the work")
                    'session))
        (should (equal captured '("be-abcd" nil "do the work" nil)))))))

(ert-deftest beads-sling-test-dispatch-non-local-agent-backend ()
  "A non-local backend slot selects that agent backend."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-agent-start)
               (lambda (&rest args) (setq captured args) 'session)))
      (beads-sling-dispatch
       (beads-sling-target :name "beads.el/claude-code"
                           :kind 'agent
                           :backend "claude-code")
       "be-abcd" nil)
      (should (equal captured '("be-abcd" "claude-code" nil nil))))))

(ert-deftest beads-sling-test-dispatch-registered-backend ()
  "A registered sling backend takes precedence over the local path."
  :tags '(:unit)
  (unwind-protect
      (let ((called nil))
        (beads-sling-backend-register
         (beads-sling-backend
          :name "gc"
          :description "Gas City"
          :dispatch (lambda (target bead prompt)
                      (setq called (list target bead prompt))
                      'backend-session)))
        (let ((target (beads-sling-target :name "city/worker"
                                          :kind 'agent
                                          :backend "gc")))
          (should (eq (beads-sling-dispatch target "be-1" "p")
                      'backend-session))
          (should (equal called (list target "be-1" "p")))))
    (remhash "gc" beads-sling--backends)))

(ert-deftest beads-sling-test-dispatch-worktree ()
  "A worktree target starts a local agent in the target's worktree."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-agent--start-with-worktree)
               (lambda (&rest args) (setq captured args) 'worktree-session))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () "/proj/")))
      (let ((target (beads-sling-worktree-target
                     "be-abc" "/proj/worktrees/be-abc" "be-abc")))
        (should (eq (beads-sling-dispatch target "be-abc" nil)
                    'worktree-session))
        (should (equal captured
                       '("be-abc" nil "/proj/" "/proj/worktrees/be-abc"
                         "Task")))))))

(ert-deftest beads-sling-test-validate ()
  "Validators are consulted; an empty hook warns about nothing."
  :tags '(:unit)
  (should-not (beads-sling-validate nil))
  (let ((beads-sling-validators
         (list (lambda (_context) "cross-store route")
               (lambda (_context) nil))))
    (should (equal (beads-sling-validate '(:work "be-abcd"))
                   '("cross-store route"))))
  (let ((beads-sling-validators
         (list (lambda (_context) (error "validator blew up")))))
    (should-not (beads-sling-validate '(:work "be-abcd")))))

(ert-deftest beads-sling-test-target-p ()
  "`beads-sling-target-p' recognizes only target instances."
  :tags '(:unit)
  (should (beads-sling-target-p
           (beads-sling-target :name "x" :kind 'role)))
  (should-not (beads-sling-target-p nil))
  (should-not (beads-sling-target-p "x")))

(provide 'beads-sling-test)
;;; beads-sling-test.el ends here
