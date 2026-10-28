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

(defclass beads-sling-test-launch-target (beads-sling-target)
  ()
  "Target subclass used to capture the launch context it is dispatched.")

(defvar beads-sling-test--dispatched nil
  "Last (BEAD PROMPT CONTEXT) captured by the test dispatch method.")

(cl-defmethod beads-sling-dispatch ((target beads-sling-test-launch-target)
                                    bead prompt &optional context)
  "Record BEAD, PROMPT and CONTEXT instead of starting an agent.
The method is on a target subclass, so the default
`beads-sling-target' method stays untouched."
  (ignore target)
  (setq beads-sling-test--dispatched (list bead prompt context))
  'captured-session)

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
          :dispatch (lambda (target bead prompt &optional context)
                      (setq called (list target bead prompt context))
                      'backend-session)))
        (let ((target (beads-sling-target :name "city/worker"
                                          :kind 'agent
                                          :backend "gc")))
          (should (eq (beads-sling-dispatch target "be-1" "p")
                      'backend-session))
          (should (equal called (list target "be-1" "p" nil)))))
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

;;; Dispatch context passthrough and launch seam (REQ-004, AC-9)

(ert-deftest beads-sling-test-launch-on-shape-delivers-context ()
  "`beads-sling--launch' delivers work, formula and resolved vars.
A 4-argument `beads-sling-dispatch' method receives the whole
`beads-sling--scope-context' plist, not only the rendered prompt."
  :tags '(:unit)
  (let* ((recipe (beads-formula
                  :name "build-basic"
                  :vars (list (beads-formula-var :name "artifact_root" :required t)
                              (beads-formula-var :name "max_iterations"))))
         (target (beads-sling-test-launch-target
                  :name "beads.el/implementation-worker" :kind 'agent))
         (context (list :work "be-abcd"
                        :work-title "Fix the blocker"
                        :formula "build-basic"
                        :recipe recipe
                        :target "beads.el/implementation-worker"
                        :values '(("artifact_root" . "plans/x/")
                                  ("max_iterations" . "10"))
                        :shape 'on))
         (beads-sling-test--dispatched nil))
    (cl-letf (((symbol-function 'beads-sling--target-get)
               (lambda (name)
                 (should (equal name "beads.el/implementation-worker"))
                 target)))
      (should (eq (beads-sling--launch context) 'captured-session))
      (pcase-let ((`(,bead ,prompt ,delivered) beads-sling-test--dispatched))
        (should (equal bead "be-abcd"))
        (should (equal prompt
                       (concat "Run formula build-basic.\n"
                               "- artifact_root=plans/x/\n"
                               "- max_iterations=10")))
        (should (equal delivered context))
        (should (equal (plist-get delivered :formula) "build-basic"))
        (should (equal (plist-get delivered :values)
                       '(("artifact_root" . "plans/x/")
                         ("max_iterations" . "10"))))))))

(ert-deftest beads-sling-test-launch-registered-backend-receives-context ()
  "A registered backend receives the context through the default method.
F1 regression guard: the seam must forward the fourth argument to the
registered `beads-sling-backend' dispatch function."
  :tags '(:unit)
  (unwind-protect
      (let ((captured nil)
            (target (beads-sling-target :name "city/worker" :kind 'agent
                                        :backend "test-gc"))
            (context (list :work "be-abcd"
                           :formula "build-basic"
                           :target "city/worker"
                           :values '(("artifact_root" . "plans/x/"))
                           :shape 'on)))
        (beads-sling-backend-register
         (beads-sling-backend
          :name "test-gc"
          :description "Capture backend"
          :dispatch (lambda (target bead prompt &optional context)
                      (setq captured (list target bead prompt context))
                      'backend-session)))
        (cl-letf (((symbol-function 'beads-sling--target-get)
                   (lambda (_name) target)))
          (should (eq (beads-sling--launch context) 'backend-session))
          (should (eq (nth 0 captured) target))
          (should (equal (nth 1 captured) "be-abcd"))
          (should (stringp (nth 2 captured)))
          (should (equal (nth 3 captured) context))))
    (remhash "test-gc" beads-sling--backends)))

(ert-deftest beads-sling-test-launch-formula-shape-reaches-backend ()
  "A formula-only launch reaches a registered backend with the context.
F2 regression guard: the formula shape must dispatch through the seam
instead of short-circuiting to a local project agent."
  :tags '(:unit)
  (unwind-protect
      (let ((captured nil)
            (target (beads-sling-target :name "city/worker" :kind 'agent
                                        :backend "test-gc"))
            (context (list :work nil
                           :formula "build-basic"
                           :recipe (beads-formula :name "build-basic")
                           :target "city/worker"
                           :values '(("max_iterations" . "5"))
                           :shape 'formula)))
        (beads-sling-backend-register
         (beads-sling-backend
          :name "test-gc"
          :description "Capture backend"
          :dispatch (lambda (target bead prompt &optional context)
                      (setq captured (list target bead prompt context))
                      'backend-session)))
        (cl-letf (((symbol-function 'beads-sling--target-get)
                   (lambda (_name) target))
                  ((symbol-function 'beads-agent--start-project-agent)
                   (lambda (&rest _)
                     (ert-fail "formula launch bypassed the registered backend"))))
          (should (eq (beads-sling--launch context) 'backend-session))
          (should-not (nth 1 captured))
          (should (equal (nth 2 captured)
                         "Run formula build-basic.\n- max_iterations=5"))
          (should (equal (nth 3 captured) context))))
    (remhash "test-gc" beads-sling--backends)))

(ert-deftest beads-sling-test-launch-local-plain-unchanged ()
  "A plain launch with no registered backend still starts the local agent.
The bead, resolved backend and prompt reaching `beads-agent-start' are
unchanged by the context seam."
  :tags '(:unit)
  (dolist (case '(("be-abcd" "local" nil)
                  ("summarise the blockers" "local" "summarise the blockers")
                  ("be-abcd" "claude-code" nil)))
    (pcase-let ((`(,work ,backend ,prompt) case))
      (let ((captured nil)
            (target (beads-sling-target :name "beads.el/task" :kind 'role
                                        :backend backend))
            (context (list :work work :formula nil :recipe nil
                           :target "beads.el/task" :values nil
                           :shape 'plain)))
        (cl-letf (((symbol-function 'beads-sling--target-get)
                   (lambda (_name) target))
                  ((symbol-function 'beads-agent-start)
                   (lambda (&rest args) (setq captured args) 'session)))
          (should (eq (beads-sling--launch context) 'session))
          (should (equal captured
                         (list work
                               (unless (equal backend "local") backend)
                               prompt
                               nil))))))))

(ert-deftest beads-sling-test-launch-local-on-unchanged ()
  "An `on' launch with no registered backend still starts the local agent.
The formula and vars are rendered into the prompt exactly as before."
  :tags '(:unit)
  (let ((captured nil)
        (target (beads-sling-target :name "beads.el/task" :kind 'role
                                    :backend "local"))
        (context (list :work "be-abcd"
                       :formula "build-basic"
                       :recipe (beads-formula :name "build-basic")
                       :target "beads.el/task"
                       :values '(("artifact_root" . "plans/x/"))
                       :shape 'on)))
    (cl-letf (((symbol-function 'beads-sling--target-get)
               (lambda (_name) target))
              ((symbol-function 'beads-agent-start)
               (lambda (&rest args) (setq captured args) 'session)))
      (should (eq (beads-sling--launch context) 'session))
      (should (equal captured
                     '("be-abcd" nil
                       "Run formula build-basic.\n- artifact_root=plans/x/"
                       nil))))))

(ert-deftest beads-sling-test-launch-formula-shape-starts-local-project-agent ()
  "A formula-only launch with no registered backend starts the local agent.
The project agent keeps the same selected backend, project root and
target directory as before."
  :tags '(:unit)
  (let ((captured nil)
        (target (beads-sling-target :name "beads.el/task" :kind 'role
                                    :backend "local"))
        (context (list :work nil
                       :formula "build-basic"
                       :recipe (beads-formula :name "build-basic")
                       :target "beads.el/task"
                       :values nil
                       :shape 'formula)))
    (cl-letf (((symbol-function 'beads-sling--target-get)
               (lambda (_name) target))
              ((symbol-function 'beads-agent--select-backend)
               (lambda (_type) 'task-backend))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () "/proj/"))
              ((symbol-function 'beads-agent--start-project-agent)
               (lambda (&rest args) (setq captured args) 'project-session)))
      (should (eq (beads-sling--launch context) 'project-session))
      (should (equal captured '(task-backend "/proj/" "/proj/"))))))

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

;;; WI-11 — adaptive transient and preview (REQ-009)

(ert-deftest beads-sling-test-header-sentence-shapes ()
  "The header sentence follows the inferred shape (mockup §6).
Shape inference is wired to the header: the same function renders the
plain, cold, formula and on sentences."
  :tags '(:unit)
  (should (equal (beads-sling--header-sentence "be-abcd" nil "beads.el/task")
                 "Sling bead be-abcd to beads.el/task"))
  (should (equal (beads-sling--header-sentence nil nil nil)
                 "Sling (no work — A or point at a bead) to (no target — T or default)"))
  (should (equal (beads-sling--header-sentence nil "pancakes" nil)
                 "Run pancakes (formula) locally"))
  (should (equal (beads-sling--header-sentence
                  "be-abcd" "build-basic" "beads.el/task")
                 "Run build-basic against bead be-abcd, drained by beads.el/task"))
  (should (equal (beads-sling--header-sentence
                  "summarise the blockers" nil "beads.el/task")
                 "Sling summarise the blockers to beads.el/task")))

(ert-deftest beads-sling-test-footer-ready-and-warnings ()
  "The live footer renders ready and warning states (mockup §6f)."
  :tags '(:unit)
  (let ((beads-sling-validators nil))
    (should (equal (beads-sling--footer
                    (list :shape 'plain :work "be-abcd"
                          :target "beads.el/task" :recipe nil :values nil))
                   "✓ Ready — local route · target beads.el/task · no vars"))
    (should (equal (beads-sling--footer
                    (list :shape 'plain :work "freeform text"
                          :target "beads.el/task" :recipe nil :values nil))
                   "✓ Ready — local route · target beads.el/task · freeform work"))
    (should (string-prefix-p
             "⚠ No target chosen"
             (beads-sling--footer
              (list :shape 'plain :work "be-abcd"
                    :target nil :recipe nil :values nil))))
    (let ((recipe (beads-formula
                   :name "build-basic"
                   :vars (list (beads-formula-var :name "artifact_root" :required t)
                               (beads-formula-var :name "max_iterations")))))
      (should (string-match-p
               "Missing required vars: artifact_root"
               (beads-sling--footer
                (list :shape 'on :work "be-abcd" :formula "build-basic"
                      :target "beads.el/task" :recipe recipe :values nil))))
      (should (equal (beads-sling--footer
                      (list :shape 'on :work "be-abcd" :formula "build-basic"
                            :target "beads.el/task" :recipe recipe
                            :values '(("artifact_root" . "plans/x/"))))
                     "✓ Ready — on run · target beads.el/task · 1 of 2 vars set")))))

(ert-deftest beads-sling-test-var-class-heuristic ()
  "Typed How readers are chosen from the var's declared shape."
  :tags '(:unit)
  (should (eq (beads-sling--var-class
               (beads-formula-var :name "x" :enum '("a" "b")))
              'beads-sling--enum-option))
  (should (eq (beads-sling--var-class
               (beads-formula-var :name "x" :var-type "bool"))
              'beads-sling--bool-option))
  (should (eq (beads-sling--var-class
               (beads-formula-var :name "x" :var-type "int"))
              'beads-sling--numeric-option))
  (should (eq (beads-sling--var-class
               (beads-formula-var :name "context_path"))
              'beads-sling--file-option))
  (should (eq (beads-sling--var-class
               (beads-formula-var :name "artifact_root"))
              'beads-sling--directory-option))
  (should (eq (beads-sling--var-class
               (beads-formula-var :name "implementation_target"))
              'beads-sling--agent-option))
  (should (eq (beads-sling--var-class
               (beads-formula-var :name "max_iterations" :default "10"))
              'beads-sling--numeric-option))
  (should (eq (beads-sling--var-class (beads-formula-var :name "title"))
              'beads-sling--string-option))
  (should (equal (beads-sling--class-tag 'beads-sling--file-option) "[file]"))
  (should-not (beads-sling--class-tag 'beads-sling--string-option)))

(ert-deftest beads-sling-test-var-keys-deterministic-and-reserved ()
  "Generated var keys are unique, deterministic and avoid reserved keys."
  :tags '(:unit)
  (let* ((vars (list (beads-formula-var :name "artifact_root")
                     (beads-formula-var :name "context_path")
                     (beads-formula-var :name "max_iterations")
                     (beads-formula-var :name "implementation_target")))
         (first (mapcar #'car
                        (beads-sling--assign-var-keys
                         vars beads-sling--reserved-keys)))
         (second (mapcar #'car
                         (beads-sling--assign-var-keys
                          vars beads-sling--reserved-keys))))
    (should (equal first second))
    (should (= (length first) (length (delete-dups (copy-sequence first)))))
    (dolist (key first)
      (should-not (member (substring key 0 1) beads-sling--reserved-keys)))))

(ert-deftest beads-sling-test-var-children ()
  "The How group renders one typed infix per var, or nothing."
  :tags '(:unit)
  (let* ((formula (beads-formula
                   :name "build-basic"
                   :vars (list (beads-formula-var :name "artifact_root" :required t)
                               (beads-formula-var :name "max_iterations" :var-type "int"))))
         (group (beads-sling--var-children formula beads-sling--reserved-keys)))
    (should group)
    (should (equal (aref group 0) "How — build-basic vars"))
    (should (= (length group) 3))
    (let ((specs (cdr (append group nil))))
      (dolist (spec specs)
        (should (eq (nth 2 spec) 'beads-sling--set-var))
        (should (memq :class spec))))
    (should-not (beads-sling--var-children
                 (beads-formula :name "pancakes") beads-sling--reserved-keys))))

(ert-deftest beads-sling-test-current-values ()
  "`beads-sling--current-values' parses only the picked formula's vars."
  :tags '(:unit)
  (let ((recipe (beads-formula
                 :name "build-basic"
                 :vars (list (beads-formula-var :name "artifact_root")
                             (beads-formula-var :name "max_iterations")))))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--var artifact_root=plans/x/"
                   "--var max_iterations=10"
                   "--var stale_var=ignored"
                   "--var max_iterations="))))
      (should (equal (beads-sling--current-values recipe)
                     '(("artifact_root" . "plans/x/")
                       ("max_iterations" . "10")))))))

(ert-deftest beads-sling-test-stage-collapse-and-shape ()
  "Stages collapse to one answered line; How/Routing track the shape."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-sling--project-label)
             (lambda () "beads.el"))
            ((symbol-function 'beads-sling--derived-target-name)
             (lambda () "beads.el/task")))
    (let* ((recipe (beads-formula
                    :name "build-basic"
                    :vars (list (beads-formula-var :name "artifact_root"))))
           (plain (beads-sling--children-specs
                   '(:work "be-abcd" :formula nil :recipe nil :target nil)))
           (formula (beads-sling--children-specs
                     (list :work "be-abcd" :formula "build-basic"
                           :recipe recipe :target nil)))
           (plain-titles (mapcar (lambda (group) (aref group 0)) plain))
           (formula-titles (mapcar (lambda (group) (aref group 0)) formula)))
      (should (member "What" plain-titles))
      (should (member "Who" plain-titles))
      (should (member "Actions" plain-titles))
      (should (member "Routing flags" plain-titles))
      (should-not (member "How — build-basic vars" plain-titles))
      (should (member "How — build-basic vars" formula-titles))
      (should-not (member "Routing flags" formula-titles))
      ;; The What line carries its answer through the pick command's
      ;; own dynamic description (stage collapse).
      (let* ((what (cl-find "What" plain :key (lambda (g) (aref g 0))
                            :test #'equal))
             (work-line (aref what 1)))
        (should (eq (nth 1 work-line) 'beads-sling--pick-work))
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda (&rest _) (list :work "be-abcd"))))
          (should (equal (funcall
                          (oref (transient--suffix-prototype
                                 'beads-sling--pick-work)
                                description)
                          nil)
                         "Work: be-abcd")))))))

(ert-deftest beads-sling-test-transient-setup-parses ()
  "Both cold and seeded sling entry parse the adaptive menu.
Regression for the transient 0.13.8 crash (be-d9ht): a function
object in a suffix-description slot was treated as the suffix command,
so `transient-setup' signalled and no menu ever rendered.  The layout
is parsed here by `transient-setup' itself, once cold (mockup §6b) and
once with a seeded scope, and the rendered menu is checked for the
What/Who groups."
  :tags '(:unit :transient)
  (cl-letf (((symbol-function 'beads-sling--project-label)
             (lambda () "beads.el"))
            ((symbol-function 'beads-sling--derived-target-name)
             (lambda () "beads.el/task")))
    (unwind-protect
        (progn
          ;; Cold entry: no work, no formula, no target.
          (transient-setup 'beads-sling--transient nil nil
                           :scope (beads-sling--initial-scope))
          (with-current-buffer " *transient*"
            (should (string-match-p "What" (buffer-string)))
            (should (string-match-p "Who" (buffer-string)))
            (should (string-match-p "Work: (none" (buffer-string))))
          (ignore-errors (transient-quit-all))
          ;; Seeded entry: work, a formula with vars and a target.
          (transient-setup
           'beads-sling--transient nil nil
           :scope (list :work "be-abcd" :work-title nil
                        :formula "build-basic"
                        :recipe (beads-formula
                                 :name "build-basic"
                                 :vars (list (beads-formula-var
                                              :name "artifact_root")))
                        :target "beads.el/task"))
          (with-current-buffer " *transient*"
            (should (string-match-p "How — build-basic vars" (buffer-string)))
            (should (string-match-p "Work: be-abcd" (buffer-string)))))
      (ignore-errors (transient-quit-all)))))

(ert-deftest beads-sling-test-info-descriptions-are-quoted-lambdas ()
  "The header/footer `:info' values are quoted lambda forms.
On Emacs 29.4 a lexical closure is a list `(closure ...)'.  transient
0.13.8 embeds an `:info' value unquoted into the form it `eval's, so a
closure object is called as a function and the menu dies with
`void-function closure'; Emacs 31 hides this because its closures are
self-evaluating `interpreted-function' objects (be-ylw4).  A quoted
lambda survives the `eval' on both, so this pins the
version-independent invariant while CI's Emacs 29.4 job is the
end-to-end gate."
  :tags '(:unit :transient)
  (cl-letf (((symbol-function 'beads-sling--project-label)
             (lambda () "beads.el")))
    (let* ((specs (beads-sling--children-specs
                   '(:work "be-abcd" :formula nil :recipe nil :target nil)))
           (header (aref (car specs) 0))
           (children (append (car specs) nil))
           (infos (cl-remove-if-not (lambda (child) (eq (car-safe child) :info))
                                    children)))
      (should (equal header "Sling — beads.el"))
      (should (= 2 (length infos)))
      (dolist (info infos)
        ;; A source-level lambda form, never a `(closure ...)' object.
        (should (eq (car-safe (cadr info)) 'lambda))
        ;; And it evaluates to the callable transient will invoke.
        (should (functionp (eval (cadr info) t)))))))

(ert-deftest beads-sling-test-preview-paints-sections ()
  "The `P' preview renders header, Validation, Recipe and plan."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-sling--project-label)
             (lambda () "beads.el"))
            ((symbol-function 'beads-sling-validate)
             (lambda (_context) nil)))
    (let* ((recipe (beads-formula
                    :name "build-basic"
                    :vars (list (beads-formula-var :name "artifact_root" :required t))
                    :steps (list (beads-formula-step :id "prepare" :title "prepare"
                                                     :needs '("artifact_root")))))
           (context (list :shape 'on :work "be-abcd" :formula "build-basic"
                          :target "beads.el/task" :recipe recipe
                          :values '(("artifact_root" . "plans/x/"))))
           (buffer (generate-new-buffer " *beads-sling-preview-test*")))
      (unwind-protect
          (progn
            (beads-sling--preview-paint buffer context)
            (with-current-buffer buffer
              (should (string-match-p
                       "Run build-basic against bead be-abcd"
                       (buffer-string)))
              (should (string-match-p "Validation" (buffer-string)))
              (should (string-match-p "✓ target beads.el/task" (buffer-string)))
              (should (string-match-p "Recipe — build-basic" (buffer-string)))
              (should (string-match-p "prepare" (buffer-string)))
              (should (string-match-p "Routing plan" (buffer-string)))))
        (kill-buffer buffer)))))

(ert-deftest beads-sling-test-preview-launch-never-gated ()
  "`s' in the preview launches exactly the stored context."
  :tags '(:unit)
  (let ((captured nil)
        (buffer (generate-new-buffer " *beads-sling-preview-launch-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-sling--launch)
                   (lambda (context) (setq captured context) 'session)))
          (with-current-buffer buffer
            (beads-sling-preview-mode)
            (setq beads-sling-preview--context '(:shape plain :work "be-abcd"))
            (beads-sling-preview-launch))
          (should (equal captured '(:shape plain :work "be-abcd"))))
      (kill-buffer buffer))))

;;; `S' entry point, keymaps and action bar (REQ-001, AC-1/2/3)

(ert-deftest beads-sling-test-s-seeds-transient-from-show-buffer ()
  "`S' in a `beads-show-mode' buffer seeds `beads-sling' with the bead.
The cached `beads-show--issue-data' object is used, so no command runs,
and the transient is opened with work, title and directory answered."
  :tags '(:unit)
  (require 'beads-command-show)
  (let ((issue (beads-issue :id "be-abcd" :title "Fix the blocker"))
        (captured nil)
        (buffer (generate-new-buffer " *beads-sling-show-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (beads-show-mode)
            (setq beads-show--issue-data issue)
            (let ((binding (key-binding (kbd "S"))))
              (should (eq binding #'beads-sling-for-issue))
              (cl-letf (((symbol-function 'transient-setup)
                         (lambda (&rest args) (setq captured args) nil)))
                (call-interactively binding))))
          (let ((scope (plist-get (nthcdr 3 captured) :scope)))
            (should (eq (car captured) 'beads-sling--transient))
            (should (equal (plist-get scope :work) "be-abcd"))
            (should (equal (plist-get scope :work-title) "Fix the blocker"))
            (should (equal (plist-get scope :directory)
                           (buffer-local-value 'default-directory buffer)))
            (should-not (plist-get scope :formula))
            (should-not (plist-get scope :recipe))
            (should-not (plist-get scope :target))))
      (kill-buffer buffer))))

(ert-deftest beads-sling-test-s-without-bead-signals-user-error ()
  "`S' without a bead at point refuses to open the transient."
  :tags '(:unit)
  (require 'beads-command-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq beads-show--issue-data nil)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (&rest _)
                 (ert-fail "the transient opened without a bead")))
              ((symbol-function 'beads-issue-at-point)
               (lambda () nil))
              ((symbol-function 'beads-agent--detect-issue-id)
               (lambda () nil)))
      (should-error (beads-sling-for-issue) :type 'user-error))))

(ert-deftest beads-sling-test-s-keymap-bindings ()
  "`S' is the issue-seeded sling in the four bead views.
Existing `S' meanings (`beads-list-sort' and
`beads-agent-list-stop-all') must not be overwritten."
  :tags '(:unit)
  (require 'beads-command-show)
  (require 'beads-command-epic)
  (require 'beads-command-dep)
  (require 'beads-wisp)
  (require 'beads-command-list)
  (require 'beads-agent-list)
  (dolist (map '(beads-show-mode-map
                 beads-epic-status-mode-map
                 beads-dep-tree-mode-map
                 beads-wisp-list-mode-map))
    (should (eq (lookup-key (symbol-value map) (kbd "S"))
                #'beads-sling-for-issue)))
  (should (eq (lookup-key beads-list-mode-map (kbd "S"))
              #'beads-list-sort))
  (should (eq (lookup-key beads-agent-list-mode-map (kbd "S"))
              #'beads-agent-list-stop-all)))

(ert-deftest beads-sling-test-show-action-bar-names-sling-for-issue ()
  "The show action bar's `S' entry resolves to `beads-sling-for-issue'."
  :tags '(:unit)
  (require 'beads-command-show)
  (should (eq (nth 2 (assq ?S beads-show--action-bar-builtins))
              #'beads-sling-for-issue))
  (should (member (cons "S" "sling") (beads-show--action-bar-entries))))

(provide 'beads-sling-test)
;;; beads-sling-test.el ends here

(ert-deftest beads-sling-test-fetch-formula-inherits-vars ()
  "The sling recipe carries variables declared on an extends parent."
  :tags '(:unit)
  (let ((parent (beads-formula
                 :name "build-base"
                 :vars (list (beads-formula-var :name "artifact_root"
                                                :required t)
                             (beads-formula-var :name "interaction_mode"
                                                :default "interactive"))))
        (child (beads-formula :name "build-basic"
                              :extends '("build-base"))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (pcase (oref command formula-name)
                   ("build-basic" child)
                   ("build-base" parent)))))
      (let ((recipe (beads-sling--fetch-formula "build-basic")))
        (should recipe)
        (should (equal (mapcar (lambda (var) (oref var name))
                               (oref recipe vars))
                       '("artifact_root" "interaction_mode")))
        (should (equal (beads-sling--missing-required-vars
                        recipe nil)
                       '("artifact_root")))))))
