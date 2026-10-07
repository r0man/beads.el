;;; beads-cook-test.el --- Tests for beads-cook -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Unit tests for the `bd cook' porcelain in `beads-cook.el' (REQ-SF-012)
;; plus the relocated `beads-command-cook' class: the dry-run step-tree
;; parser, the preview buffer and the transient suffix guards.
;; `beads-command-execute' is mocked, so no `bd' is required.  The
;; `:integration' test runs the real `bd' and checks `bd cook --json'
;; against `bd formula show --json'.

;;; Code:

(require 'ert)
(require 'beads-cook)
(require 'beads-command-cook)
(require 'beads-command-formula)
(require 'beads-test)
(require 'beads-integration-test)

;;; ========================================
;;; Fixtures
;;; ========================================

(defconst beads-cook-test--compile-output
  "\nDry run: would cook formula build-basic as proto build-basic (compile-time mode)\n\nSteps (4) [{{variables}} shown as placeholders]:\n  ├── prepare: Implement {{context_path}} [from: build-basic@steps[0]]\n  ├── requirements: Requirements for {{context_path}} [needs: prepare] [from: build-basic@steps[1]]\n  ├── plan: Plan {{context_path}} [depends: wait-for-ci, needs: requirements] [from: build-basic@steps[2]]\n  └── wait-for-ci: Wait for CI [needs: plan] [from: build-basic@steps[3]]\n\nVariables used: context_path\n\nVariable definitions:\n  {{context_path}}: Path to context (required)\n  {{implementation_target}}: Agent target (default=beads.el/gc.implementation-worker)\n"
  "A captured `bd cook --dry-run' output in compile-time mode.")

(defconst beads-cook-test--runtime-output
  "\nDry run: would cook formula build-basic as proto build-basic (runtime mode)\n\nSteps (2) [variables substituted]:\n  ├── prepare: Implement plans/x [from: build-basic@steps[0]]\n  └── requirements: Requirements for plans/x [needs: prepare] [from: build-basic@steps[1]]\n\nVariables used: context_path\n"
  "A captured `bd cook --dry-run --var context_path=...' output.")

(defconst beads-cook-test--formula
  "formula = \"beads-cook-fixture\"
description = \"Cook fixture for tests\"
type = \"workflow\"
version = 1

[vars.context_path]
description = \"Path to context\"
required = true

[[steps]]
id = \"prepare\"
title = \"Implement {{context_path}}\"
description = \"Prepare the work\"

[[steps]]
id = \"requirements\"
title = \"Requirements for {{context_path}}\"
needs = [\"prepare\"]
"
  "A minimal formula TOML used by the integration parity test.")

;;; ========================================
;;; Command line
;;; ========================================

(ert-deftest beads-cook-test-command-line-basic ()
  "Cook builds the subcommand and its positional formula."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-cook :formula-id "formula-1"))))
    (should (member "cook" args))
    (should (member "formula-1" args))))

(ert-deftest beads-cook-test-command-line-options ()
  "Cook serialises mode, persist, force, prefix, dry-run and vars."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-cook
                :formula-id "build-basic"
                :mode "runtime"
                :persist t
                :force t
                :prefix "gt-"
                :var '("context_path=plans/x" "max_iterations=3")))))
    (should (member "--mode" args))
    (should (member "runtime" args))
    (should (member "--persist" args))
    (should (member "--force" args))
    (should (member "--prefix" args))
    (should (member "gt-" args))
    (should (member "--var" args))
    (should (seq-some (lambda (arg)
                        (string-match-p "context_path=plans/x" arg))
                      args))
    (should (seq-some (lambda (arg)
                        (string-match-p "max_iterations=3" arg))
                      args))))

(ert-deftest beads-cook-test-command-line-dry-run ()
  "Cook serialises --dry-run."
  :tags '(:unit)
  (should (member "--dry-run"
                  (beads-command-line
                   (beads-command-cook :formula-id "x" :dry-run t)))))

;;; ========================================
;;; Dry-run tree parsing
;;; ========================================

(ert-deftest beads-cook-test-parse-tree-compile ()
  "Compile-mode dry-run parses the header and the step tree."
  :tags '(:unit)
  (let ((tree (beads-cook-parse-tree beads-cook-test--compile-output)))
    (should (equal (plist-get tree :formula) "build-basic"))
    (should (equal (plist-get tree :proto) "build-basic"))
    (should (equal (plist-get tree :mode) "compile"))
    (should (= (plist-get tree :step-count) 4))
    (should (equal (plist-get tree :variables-used) '("context_path")))
    (let ((steps (plist-get tree :steps)))
      (should (= 4 (length steps)))
      ;; First step: no needs/depends, placeholder title preserved.
      (should (equal (plist-get (nth 0 steps) :id) "prepare"))
      (should (equal (plist-get (nth 0 steps) :title)
                     "Implement {{context_path}}"))
      (should-not (plist-get (nth 0 steps) :needs))
      (should (equal (plist-get (nth 0 steps) :from) "build-basic@steps[0]"))
      ;; Third step: both needs and depends annotations are parsed.
      (should (equal (plist-get (nth 2 steps) :id) "plan"))
      (should (equal (plist-get (nth 2 steps) :needs) '("requirements")))
      (should (equal (plist-get (nth 2 steps) :depends-on) '("wait-for-ci")))
      ;; Fourth step is the last branch; needs survives.
      (should (equal (plist-get (nth 3 steps) :needs) '("plan"))))))

(ert-deftest beads-cook-test-parse-tree-runtime ()
  "Runtime-mode dry-run records substituted titles and mode."
  :tags '(:unit)
  (let ((tree (beads-cook-parse-tree beads-cook-test--runtime-output)))
    (should (equal (plist-get tree :mode) "runtime"))
    (should (= (plist-get tree :step-count) 2))
    (should (equal (plist-get (nth 0 (plist-get tree :steps)) :title)
                   "Implement plans/x"))
    (should (equal (plist-get (nth 1 (plist-get tree :steps)) :needs)
                   '("prepare")))))

(ert-deftest beads-cook-test-parse-tree-empty ()
  "Empty or nil output parses to nil."
  :tags '(:unit)
  (should-not (beads-cook-parse-tree nil))
  (should-not (beads-cook-parse-tree "")))

(ert-deftest beads-cook-test-parse-tree-ignores-unknown-lines ()
  "Unrecognised lines do not break parsing."
  :tags '(:unit)
  (let ((tree (beads-cook-parse-tree
               (concat beads-cook-test--compile-output
                       "warning: something changed\n"))))
    (should (= 4 (length (plist-get tree :steps))))))

;;; ========================================
;;; Transient arg helpers
;;; ========================================

(ert-deftest beads-cook-test-vars-from-args ()
  "Every repeated --var is collected as key=value."
  :tags '(:unit)
  (should (equal (beads-cook--vars-from-args
                  '("--persist" "--var=a=1" "--var=b=2"))
                 '("a=1" "b=2")))
  (should-not (beads-cook--vars-from-args '("--persist"))))

;;; ========================================
;;; Preview
;;; ========================================

(ert-deftest beads-cook-test-preview-renders-buffer ()
  "Preview runs a non-JSON dry run and renders the parsed tree."
  :tags '(:unit)
  (let (captured-command)
    (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
              ((symbol-function 'beads-buffer-display-same-or-reuse) #'ignore)
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq captured-command cmd)
                 beads-cook-test--compile-output)))
      (unwind-protect
          (let* ((tree (beads-cook-preview "build-basic" "compile"))
                 (buffer (get-buffer
                          (beads-cook--preview-buffer-name "build-basic"))))
            (should tree)
            ;; The command is a dry run and does not request JSON, so
            ;; bd returns the human-readable step tree.
            (should (oref captured-command dry-run))
            (should-not (oref captured-command json))
            (should (buffer-live-p buffer))
            (with-current-buffer buffer
              (should (derived-mode-p 'beads-cook-preview-mode))
              (should (string-match-p "Cook preview" (buffer-string)))
              (should (string-match-p "Would create proto build-basic"
                                      (buffer-string)))
              (should (string-match-p "wait-for-ci" (buffer-string)))))
        (when-let* ((buffer (get-buffer
                             (beads-cook--preview-buffer-name "build-basic"))))
          (kill-buffer buffer))))))

;;; ========================================
;;; Suffix guards
;;; ========================================

(ert-deftest beads-cook-test-execute-force-requires-persist ()
  "Force without Persist is rejected before anything runs."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-cook--formula-at-point)
             (lambda () "build-basic"))
            ((symbol-function 'beads-command-execute-interactive)
             (lambda (&rest _) (ert-fail "must not execute"))))
    (beads-test-with-transient-args 'beads-cook '("--force")
      (should-error (beads-cook--execute) :type 'user-error))))

;;; ========================================
;;; Integration: dry-run fixture and formula-show parity
;;; ========================================

(ert-deftest beads-cook-test-integration-dry-run-and-parity ()
  "Cook parses a live dry run and agrees with `bd formula show --json'."
  :tags '(:integration)
  (beads-test-skip-unless-bd)
  (beads-test-with-temp-repo (:init-beads t)
    (let ((formulas-dir (expand-file-name ".beads/formulas"
                                          default-directory))
          (formula "beads-cook-fixture"))
      (make-directory formulas-dir t)
      (write-region beads-cook-test--formula nil
                    (expand-file-name (concat formula ".formula.toml")
                                      formulas-dir)
                    nil 'silent)
      ;; Dry-run parse.
      (let* ((output (beads-command-execute
                      (beads-command-cook :formula-id formula
                                          :dry-run t :json nil)))
             (tree (beads-cook-parse-tree output)))
        (should (equal (plist-get tree :formula) formula))
        (should (= 2 (length (plist-get tree :steps))))
        (should (equal (plist-get (nth 1 (plist-get tree :steps)) :needs)
                       '("prepare"))))
      ;; `bd cook --json' and `bd formula show --json' describe the
      ;; same resolved formula.
      (let* ((show-json (beads-command-execute
                         (beads-command-formula-show
                          :formula-name formula :json t)))
             (cook-json (beads-command-execute
                         (beads-command-cook
                          :formula-id formula :json t))))
        (should (equal (oref show-json name) (alist-get 'formula cook-json)))
        (should (equal (mapcar (lambda (step) (oref step id))
                               (oref show-json steps))
                       (mapcar (lambda (step) (alist-get 'id step))
                               (append (alist-get 'steps cook-json) nil))))))))

(provide 'beads-cook-test)
;;; beads-cook-test.el ends here
