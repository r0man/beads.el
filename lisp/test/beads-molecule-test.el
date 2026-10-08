;;; beads-molecule-test.el --- Tests for beads-molecule -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the molecule execution view (`beads-molecule.el').
;;
;; `:unit' tests exercise the pure normalisation, state-transition,
;; face/glyph, rendering and aggregate-loader logic (the loader is
;; driven with a stubbed `beads-command-execute-async').  The
;; `:integration' test pours a self-contained formula into a temporary
;; `bd init' repo, then verifies the model, the step states and the
;; gate attachment against the real CLI.

;;; Code:

(require 'ert)
(require 'beads-molecule)
(require 'beads-command-mol)
(require 'beads-command-gate)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-test)
(require 'beads-integration-test)

;;; ============================================================
;;; Fixtures
;;; ============================================================

(defvar beads-molecule-test--current-json
  (vector
   (list (cons 'molecule_id "mol-1")
         (cons 'molecule_title "Test Molecule")
         (cons 'completed 1)
         (cons 'total 4)
         (cons 'steps
               (vector
                (list (cons 'issue (list (cons 'id "mol-1.a")
                                         (cons 'title "Prepare")
                                         (cons 'status "closed")
                                         (cons 'issue_type "task")))
                      (cons 'status "done")
                      (cons 'is_current :json-false))
                (list (cons 'issue (list (cons 'id "mol-1.b")
                                         (cons 'title "Build")
                                         (cons 'status "open")
                                         (cons 'issue_type "task")))
                      (cons 'status "ready")
                      (cons 'is_current :json-false))
                (list (cons 'issue (list (cons 'id "mol-1.c")
                                         (cons 'title "Review")
                                         (cons 'status "in_progress")
                                         (cons 'issue_type "task")
                                         (cons 'assignee "worker")))
                      (cons 'status "pending")
                      (cons 'is_current t))
                (list (cons 'issue (list (cons 'id "mol-1.d")
                                         (cons 'title "Deploy")
                                         (cons 'status "open")
                                         (cons 'issue_type "task")))
                      (cons 'status "pending")
                      (cons 'is_current :json-false))))))
  "Synthetic `bd mol current --json' payload for unit tests.")

(defvar beads-molecule-test--progress-json
  '((completed . 1)
    (total . 4)
    (percent . 25.0)
    (current_step_id . "mol-1.c")
    (in_progress . 1)
    (molecule_id . "mol-1")
    (molecule_title . "Test Molecule"))
  "Synthetic `bd mol progress --json' payload for unit tests.")

(defvar beads-molecule-test--show-json
  (list (cons 'root (list (cons 'id "mol-1") (cons 'title "Test Molecule")))
        (cons 'variables (vector "convoy_id"))
        (cons 'dependencies
              (vector
               (list (cons 'issue_id "mol-1.b")
                     (cons 'depends_on_id "mol-1.a")
                     (cons 'type "blocks"))
               (list (cons 'issue_id "mol-1.c")
                     (cons 'depends_on_id "mol-1.b")
                     (cons 'type "blocks"))
               (list (cons 'issue_id "mol-1.d")
                     (cons 'depends_on_id "mol-1.c")
                     (cons 'type "blocks")))))
  "Synthetic `bd mol show --json' payload for unit tests.")

(defun beads-molecule-test--gate-issue (&optional blocked status)
  "Return a gate `beads-issue' blocking BLOCKED, with STATUS open by default."
  (beads-issue
   :id "gate-1"
   :title "Gate: human"
   :status (or status "open")
   :await-type "human"
   :dependents (list (beads-dependency
                      :id (or blocked "mol-1.d")
                      :depends-on-id (or blocked "mol-1.d")
                      :type "blocks"))))

(defun beads-molecule-test--state (&optional gates)
  "Return an aggregate loader state from the fixtures, with GATES."
  (list :current (beads-molecule--normalize-current
                  beads-molecule-test--current-json)
        :progress (beads-molecule--normalize-progress
                   beads-molecule-test--progress-json)
        :show (beads-molecule--normalize-show beads-molecule-test--show-json)
        :gates gates))

;;; ============================================================
;;; Step state transition table
;;; ============================================================

(ert-deftest beads-molecule-test-step-state-done ()
  "A closed step or a `done' status letter is `done'."
  :tags '(:unit)
  (should (eq 'done (beads-molecule-step-state
                     '(:status "done" :issue-status "closed"))))
  (should (eq 'done (beads-molecule-step-state
                     '(:status "pending" :issue-status "closed")))))

(ert-deftest beads-molecule-test-step-state-current ()
  "An in-progress step is `current' even when bd reports another letter."
  :tags '(:unit)
  (should (eq 'current (beads-molecule-step-state
                        '(:status "ready" :issue-status "in_progress"))))
  (should (eq 'current (beads-molecule-step-state
                        '(:status "pending" :is-current t)))))

(ert-deftest beads-molecule-test-step-state-gate-beats-blocked ()
  "A gate annotation wins over a plain dependency block."
  :tags '(:unit)
  (should (eq 'blocked-gate (beads-molecule-step-state
                             '(:status "pending" :blocked t
                               :gate (:id "gate-1"))))))

(ert-deftest beads-molecule-test-step-state-blocked-and-ready ()
  "Dependency blocks render as `blocked'; bd-ready steps render as `ready'."
  :tags '(:unit)
  (should (eq 'blocked (beads-molecule-step-state '(:status "pending" :blocked t))))
  (should (eq 'blocked (beads-molecule-step-state '(:status "blocked"))))
  (should (eq 'ready (beads-molecule-step-state '(:status "ready"))))
  (should (eq 'pending (beads-molecule-step-state '(:status "pending")))))

(ert-deftest beads-molecule-test-step-state-computed-field-wins ()
  "A pre-computed `:state' field short-circuits the derivation."
  :tags '(:unit)
  (should (eq 'done (beads-molecule-step-state '(:state done :status "ready")))))

;;; ============================================================
;;; Faces and glyphs
;;; ============================================================

(ert-deftest beads-molecule-test-step-face ()
  "Each state maps to its documented face."
  :tags '(:unit)
  (should (eq 'beads-molecule-step-done (beads-molecule-step-face 'done)))
  (should (eq 'beads-molecule-step-current (beads-molecule-step-face 'current)))
  (should (eq 'beads-molecule-step-ready (beads-molecule-step-face 'ready)))
  (should (eq 'beads-molecule-step-blocked (beads-molecule-step-face 'blocked)))
  (should (eq 'beads-molecule-step-blocked-gate
              (beads-molecule-step-face 'blocked-gate)))
  (should (eq 'beads-molecule-step-pending
              (beads-molecule-step-face 'something-else))))

(ert-deftest beads-molecule-test-step-glyph ()
  "Each state maps to a distinct-enough glyph."
  :tags '(:unit)
  (should (equal "✓" (beads-molecule-step-glyph 'done)))
  (should (equal "◐" (beads-molecule-step-glyph 'current)))
  (should (equal "○" (beads-molecule-step-glyph 'ready)))
  (should (equal "⊘" (beads-molecule-step-glyph 'blocked-gate)))
  (should (equal "·" (beads-molecule-step-glyph 'unknown))))

;;; ============================================================
;;; Normalisation
;;; ============================================================

(ert-deftest beads-molecule-test-json-bool ()
  "JSON false and nil are nil; everything else is non-nil."
  :tags '(:unit)
  (should-not (beads-molecule--json-bool :json-false))
  (should-not (beads-molecule--json-bool nil))
  (should (beads-molecule--json-bool t))
  (should (beads-molecule--json-bool "x")))

(ert-deftest beads-molecule-test-normalize-current ()
  "`bd mol current' normalises to the canonical molecule plist."
  :tags '(:unit)
  (let ((mol (beads-molecule--normalize-current
              beads-molecule-test--current-json)))
    (should (equal "mol-1" (plist-get mol :id)))
    (should (equal "Test Molecule" (plist-get mol :title)))
    (should (= 4 (length (plist-get mol :steps))))
    (should (= 1 (plist-get mol :completed)))
    (should (= 4 (plist-get mol :total)))
    (let ((current (seq-find (lambda (step) (plist-get step :is-current))
                             (plist-get mol :steps))))
      (should (equal "mol-1.c" (plist-get current :id)))
      (should (equal "worker" (plist-get current :assignee))))))

(ert-deftest beads-molecule-test-normalize-current-nil ()
  "An absent molecule normalises to nil."
  :tags '(:unit)
  (should-not (beads-molecule--normalize-current nil))
  (should-not (beads-molecule--normalize-current [])))

(ert-deftest beads-molecule-test-normalize-progress ()
  "`bd mol progress' normalises percent and current-step id."
  :tags '(:unit)
  (let ((progress (beads-molecule--normalize-progress
                   beads-molecule-test--progress-json)))
    (should (= 1 (plist-get progress :completed)))
    (should (= 4 (plist-get progress :total)))
    (should (equal "mol-1.c" (plist-get progress :current-step-id)))
    (should (equal 25.0 (plist-get progress :percent)))
    (should-not (beads-molecule--normalize-progress nil))))

(ert-deftest beads-molecule-test-normalize-show ()
  "`bd mol show' normalises dependencies and variables."
  :tags '(:unit)
  (let ((show (beads-molecule--normalize-show beads-molecule-test--show-json)))
    (should (= 3 (length (plist-get show :dependencies))))
    (should (equal '("convoy_id") (plist-get show :variables)))
    (should-not (beads-molecule--normalize-show nil))))

(ert-deftest beads-molecule-test-normalize-gates ()
  "A gate issue's `blocks' dependents become the `:blocked' list."
  :tags '(:unit)
  (let ((gates (beads-molecule--normalize-gates
                (list (beads-molecule-test--gate-issue "mol-1.d")))))
    (should (= 1 (length gates)))
    (should (equal "gate-1" (plist-get (car gates) :id)))
    (should (equal "human" (plist-get (car gates) :await-type)))
    (should (equal '("mol-1.d") (plist-get (car gates) :blocked)))))

(ert-deftest beads-molecule-test-normalize-gates-single-object ()
  "`bd show' may return a single issue object; normalisation accepts it."
  :tags '(:unit)
  (let ((gates (beads-molecule--normalize-gates
                (beads-molecule-test--gate-issue "mol-1.d"))))
    (should (= 1 (length gates)))
    (should (equal '("mol-1.d") (plist-get (car gates) :blocked)))))


;;; ============================================================
;;; Model construction
;;; ============================================================

(ert-deftest beads-molecule-test-build-model-no-gates ()
  "The model carries the raw dependency block for a not-yet-done step."
  :tags '(:unit)
  (let ((model (beads-molecule--build-model
                (beads-molecule-test--state))))
    (should model)
    (should (equal "mol-1" (plist-get model :id)))
    (should (equal "persistent" (plist-get model :phase)))
    (should (= 1 (plist-get model :ready-count)))
    (should (= 1 (plist-get model :blocked-count)))
    (let ((build (seq-find (lambda (step) (equal (plist-get step :title) "Build"))
                           (plist-get model :steps)))
          (review (seq-find (lambda (step) (equal (plist-get step :title) "Review"))
                            (plist-get model :steps)))
          (deploy (seq-find (lambda (step) (equal (plist-get step :title) "Deploy"))
                            (plist-get model :steps))))
      (should (eq 'ready (beads-molecule-step-state build)))
      (should (equal '("mol-1.a") (plist-get build :needs)))
      (should (eq 'current (beads-molecule-step-state review)))
      (should (eq 'blocked (beads-molecule-step-state deploy)))
      (should (equal '("mol-1.c") (plist-get deploy :needs))))))

(ert-deftest beads-molecule-test-build-model-with-gate ()
  "An open gate annotates its step as `blocked-gate' and fills the gate section."
  :tags '(:unit)
  (let* ((gates (beads-molecule--normalize-gates
                 (list (beads-molecule-test--gate-issue "mol-1.d"))))
         (model (beads-molecule--build-model
                 (beads-molecule-test--state gates)))
         (deploy (seq-find (lambda (step) (equal (plist-get step :title) "Deploy"))
                           (plist-get model :steps))))
    (should (= 1 (plist-get model :gated-count)))
    (should (= 1 (length (plist-get model :gates))))
    (should (eq 'blocked-gate (beads-molecule-step-state deploy)))
    (should (equal "gate-1" (plist-get (plist-get deploy :gate) :id)))))

(ert-deftest beads-molecule-test-build-model-closed-gate-does-not-block ()
  "A closed gate no longer blocks its step."
  :tags '(:unit)
  (let* ((gates (beads-molecule--normalize-gates
                 (list (beads-molecule-test--gate-issue "mol-1.d" "closed"))))
         (model (beads-molecule--build-model
                 (beads-molecule-test--state gates)))
         (deploy (seq-find (lambda (step) (equal (plist-get step :title) "Deploy"))
                           (plist-get model :steps))))
    (should-not (plist-get deploy :gate))
    (should (eq 'blocked (beads-molecule-step-state deploy)))))

(ert-deftest beads-molecule-test-build-model-output-and-percent ()
  "Closed steps populate the output section and the header percentage."
  :tags '(:unit)
  (let ((model (beads-molecule--build-model
                (beads-molecule-test--state))))
    (should (= 1 (length (plist-get model :output))))
    (should (equal "Prepare" (plist-get (car (plist-get model :output)) :title)))
    (should (= 25.0 (beads-molecule--percent model)))))

(ert-deftest beads-molecule-test-build-model-vapor-phase ()
  "An ephemeral step marks the molecule as vapor."
  :tags '(:unit)
  (let* ((raw (beads-molecule--normalize-current
               (vector (list (cons 'molecule_id "w-1")
                             (cons 'molecule_title "Wisp")
                             (cons 'total 1)
                             (cons 'steps
                                   (vector (list (cons 'issue
                                                       (list (cons 'id "w-1.a")
                                                             (cons 'title "A")
                                                             (cons 'status "open")
                                                             (cons 'ephemeral t)))
                                                 (cons 'status "ready")
                                                 (cons 'is_current :json-false))))))))
         (model (beads-molecule--build-model (list :current raw))))
    (should (equal "vapor" (plist-get model :phase)))))

;;; ============================================================
;;; Rendering
;;; ============================================================

(ert-deftest beads-molecule-test-step-line-is-a-thing ()
  "A rendered step line carries the step id as a `beads-thing'."
  :tags '(:unit)
  (let* ((model (beads-molecule--build-model (beads-molecule-test--state)))
         (step (car (plist-get model :steps)))
         (line (beads-molecule--step-line step 1))
         (thing (get-text-property 0 'beads-thing line)))
    (should (string-match-p (regexp-quote (plist-get step :id)) line))
    (should (equal 'step (plist-get thing :kind)))
    (should (equal (plist-get step :id) (plist-get thing :id)))))

(ert-deftest beads-molecule-test-gate-line ()
  "A gate line names the await type, id and blocked steps."
  :tags '(:unit)
  (let ((line (beads-molecule--gate-line
               '(:id "gate-1" :await-type "gh:run" :status "open"
                 :blocked ("mol-1.c")))))
    (should (string-match-p "gh:run" line))
    (should (string-match-p "gate-1" line))
    (should (string-match-p "mol-1.c" line))))

(ert-deftest beads-molecule-test-shift-range ()
  "Ranges shift by whole pages and floor at one."
  :tags '(:unit)
  (let ((beads-molecule-range-size 10))
    (should (equal '(1 . 10) (beads-molecule--shift-range nil 1)))
    (should (equal '(1 . 10) (beads-molecule--shift-range '(1 . 10) -1)))
    (should (equal '(11 . 20) (beads-molecule--shift-range '(1 . 10) 1)))
    (should (equal '(21 . 30) (beads-molecule--shift-range '(11 . 20) 1)))))

;;; ============================================================
;;; Aggregate loader
;;; ============================================================

(defun beads-molecule-test--stub-async (&optional fail-current)
  "Return a `beads-command-execute-async' stub serving the fixtures.
When FAIL-CURRENT is non-nil, the `mol current' branch calls its error
callback instead of the success callback."
  (lambda (cmd on-success &optional on-error &rest _kwargs)
    (cond
     ((object-of-class-p cmd 'beads-command-mol-current)
      (if fail-current
          (funcall on-error "boom")
        (funcall on-success beads-molecule-test--current-json)))
     ((object-of-class-p cmd 'beads-command-mol-progress)
      (funcall on-success beads-molecule-test--progress-json))
     ((object-of-class-p cmd 'beads-command-mol-show)
      (funcall on-success beads-molecule-test--show-json))
     ((object-of-class-p cmd 'beads-command-list)
      (funcall on-success (list (beads-molecule-test--gate-issue "mol-1.d"))))
     ((object-of-class-p cmd 'beads-command-show)
      (funcall on-success (list (beads-molecule-test--gate-issue "mol-1.d"))))
     (t (funcall on-success nil)))
    nil))

(ert-deftest beads-molecule-test-load-resolves-aggregate ()
  "The loader settles with all four payloads after the fan-out."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute-async)
             (beads-molecule-test--stub-async)))
    (let (result err)
      (beads-molecule--load "mol-1" nil
                            (lambda (value) (setq result value))
                            (lambda (error) (setq err error)))
      (should-not err)
      (should result)
      (should (equal "mol-1" (plist-get (plist-get result :current) :id)))
      (should (= 4 (plist-get (plist-get result :progress) :total)))
      (should (plist-get result :show))
      (should (= 1 (length (plist-get result :gates)))))))

(ert-deftest beads-molecule-test-load-rejects-on-current-error ()
  "A failure of `mol current' rejects the aggregate."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute-async)
             (beads-molecule-test--stub-async t)))
    (let (result err)
      (beads-molecule--load "mol-1" nil
                            (lambda (value) (setq result value))
                            (lambda (error) (setq err error)))
      (should-not result)
      (should (equal "boom" err)))))

(ert-deftest beads-molecule-test-load-soft-failure-degrades ()
  "A failure of a secondary command degrades rather than rejects."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute-async)
             (lambda (cmd on-success &optional on-error &rest _kwargs)
               (if (object-of-class-p cmd 'beads-command-mol-progress)
                   (funcall on-error "no progress")
                 (funcall on-success
                          (cond
                           ((object-of-class-p cmd 'beads-command-mol-current)
                            beads-molecule-test--current-json)
                           ((object-of-class-p cmd 'beads-command-mol-show)
                            beads-molecule-test--show-json)
                           (t nil))))
               nil)))
    (let (result err)
      (beads-molecule--load "mol-1" nil
                            (lambda (value) (setq result value))
                            (lambda (error) (setq err error)))
      (should-not err)
      (should result)
      (should (plist-get result :current))
      (should-not (plist-get result :progress)))))

;;; ============================================================
;;; Integration
;;; ============================================================

(defconst beads-molecule-test--formula
  (concat "formula = \"miniflow\"\n"
          "version = 1\n"
          "description = \"Minimal test molecule\"\n"
          "\n"
          "[[steps]]\n"
          "id = \"prepare\"\n"
          "title = \"Prepare\"\n"
          "\n"
          "[[steps]]\n"
          "id = \"build\"\n"
          "title = \"Build\"\n"
          "needs = [\"prepare\"]\n")
  "Self-contained formula poured by the integration test.")

(ert-deftest beads-molecule-test-integration-model ()
  "Pour a real molecule and derive the model, block state and gate."
  :tags '(:integration)
  (beads-test-skip-unless-bd)
  (beads-test-with-temp-repo (:init-beads t :prefix "mltest")
    (let* ((formula-dir (expand-file-name ".beads/formulas" default-directory)))
      (make-directory formula-dir t)
      (with-temp-file (expand-file-name "miniflow.formula.toml" formula-dir)
        (insert beads-molecule-test--formula))
      (let* ((pour (beads-command-execute
                    (beads-command-mol-pour :proto-id "miniflow" :json t)))
             (root (beads-molecule--aget pour 'new_epic_id))
             (current (beads-molecule--normalize-current
                       (beads-command-execute
                        (beads-command-mol-current :mol-id root :json t))))
             (progress (beads-molecule--normalize-progress
                        (beads-command-execute
                         (beads-command-mol-progress :mol-id root :json t))))
             (show (beads-molecule--normalize-show
                    (beads-command-execute
                     (beads-command-mol-show :mol-id root :json t))))
             (model (beads-molecule--build-model
                     (list :current current :progress progress :show show
                           :gates nil))))
        (should root)
        (should (= 2 (length (plist-get model :steps))))
        (should (equal "persistent" (plist-get model :phase)))
        (should (= 1 (plist-get model :ready-count)))
        (let* ((build (seq-find (lambda (step)
                                  (equal (plist-get step :title) "Build"))
                                (plist-get model :steps)))
               (build-id (plist-get build :id))
               (line (beads-molecule--step-line build 2)))
          (should (eq 'blocked (beads-molecule-step-state build)))
          (should (= 1 (length (plist-get build :needs))))
          (should (equal 'step (plist-get (get-text-property 0 'beads-thing line)
                                          :kind)))
          ;; Attach an open gate and re-derive: the step becomes gated.
          (beads-command-execute
           (beads-command-gate-create :blocks build-id :gate-type "human"
                                      :reason "integration gate" :json t))
          (let* ((gate-issues (beads-command-execute
                               (beads-command-list :issue-type "gate" :json t)))
                 (gate-ids (mapcar (lambda (gate) (oref gate id)) gate-issues))
                 (shown (beads-command-execute
                         (beads-command-show :issue-ids gate-ids
                                             :include-dependents t :json t)))
                 (gates (beads-molecule--normalize-gates shown))
                 (gated-model (beads-molecule--build-model
                               (list :current current :progress progress
                                     :show show :gates gates)))
                 (gated-build (seq-find (lambda (step)
                                          (equal (plist-get step :id) build-id))
                                        (plist-get gated-model :steps))))
            (should gates)
            (should (equal (list build-id)
                           (plist-get (car gates) :blocked)))
            (should (eq 'blocked-gate (beads-molecule-step-state gated-build)))
            (should (equal (oref (car gate-issues) id)
                           (plist-get (plist-get gated-build :gate) :id)))))))))

;;; ============================================================
;;; Work loop: actions and navigation (WI-SF-02)
;;; ============================================================

(defun beads-molecule-test--work-loop-buffer ()
  "Return a buffer with two ready steps and a pending one, model bound."
  (let ((buf (generate-new-buffer "*beads-molecule-work-loop*")))
    (with-current-buffer buf
      (setq-local beads-molecule--root "m")
      (setq-local beads-molecule--collapsed nil)
      (setq-local beads-molecule--model
                  (list :steps (list (list :id "m.a" :title "A" :status "ready")
                                     (list :id "m.b" :title "B" :status "ready")
                                     (list :id "m.c" :title "C" :status "pending"))))
      (insert (mapconcat #'identity
                         (cl-loop for step in (plist-get beads-molecule--model :steps)
                                  for i from 1
                                  collect (beads-molecule--step-line step i))
                         "\n"))
      (goto-char (point-min)))
    buf))

(ert-deftest beads-molecule-test-action-claim-builds-update ()
  "`claim' assembles a `beads-command-update --claim' for the step."
  :tags '(:unit)
  (let (captured)
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd) cmd)))
      (beads-molecule-action 'claim (list :id "m.a")))
    (should (eq 'beads-command-update (eieio-object-class captured)))
    (should (equal '("m.a") (oref captured issue-ids)))
    (should (oref captured claim))))

(ert-deftest beads-molecule-test-action-close-builds-close ()
  "`close' assembles a `beads-command-close' carrying the reason."
  :tags '(:unit)
  (let (captured)
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd) (setq captured cmd) cmd)))
      (beads-molecule-action 'close (list :id "m.a") "done"))
    (should (eq 'beads-command-close (eieio-object-class captured)))
    (should (equal '("m.a") (oref captured issue-ids)))
    (should (equal "done" (oref captured reason)))))

(ert-deftest beads-molecule-test-next-moves-to-ready-steps ()
  "`n' walks the ready frontier and wraps."
  :tags '(:unit)
  (let ((buf (beads-molecule-test--work-loop-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          ;; Point starts on the first ready step, so `n' advances to the
          ;; second, then wraps back to the first.
          (beads-molecule-next)
          (should (equal "m.b" (plist-get (beads-thing-at) :id)))
          (beads-molecule-next)
          (should (equal "m.a" (plist-get (beads-thing-at) :id)))
          (beads-molecule-next)
          (should (equal "m.b" (plist-get (beads-thing-at) :id))))
      (kill-buffer buf))))

(ert-deftest beads-molecule-test-close-requires-reason ()
  "`x' refuses an empty reason before any command runs."
  :tags '(:unit)
  (let ((buf (beads-molecule-test--work-loop-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (beads-molecule--goto-step "m.a")
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
            (should-error (beads-molecule-close) :type 'user-error)))
      (kill-buffer buf))))

(ert-deftest beads-molecule-test-claim-refreshes-and-advances ()
  "`c' claims, refreshes and advances."
  :tags '(:unit)
  (let ((buf (beads-molecule-test--work-loop-buffer))
        (calls nil))
    (unwind-protect
        (with-current-buffer buf
          (beads-molecule--goto-step "m.a")
          (cl-letf (((symbol-function 'beads-command-execute)
                     (lambda (cmd) (push (eieio-object-class cmd) calls) cmd))
                    ((symbol-function 'beads-molecule-refresh)
                     (lambda () (push 'refresh calls)))
                    ((symbol-function 'beads-molecule-next)
                     (lambda () (push 'next calls))))
            (beads-molecule-claim))
          (should (memq 'beads-command-update calls))
          (should (memq 'refresh calls))
          (should (memq 'next calls)))
      (kill-buffer buf))))

(provide 'beads-molecule-test)
;;; beads-molecule-test.el ends here
