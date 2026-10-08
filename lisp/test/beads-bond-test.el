;;; beads-bond-test.el --- Tests for the bond flow -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the bonding flow (WI-SF-08, design.md §5.3/§9,
;; REQ-SF-050, REQ-SF-051, REQ-SF-052).  Covers command assembly, the
;; phase override, dynamic ref/vars, validation, the operand
;; completion hook, bond-point attachment and the dry-run preview.
;; `beads-command-execute' is stubbed in every unit test, so no `bd'
;; invocation is required.  The `:integration' test drives the real
;; `bd mol bond' against a temporary repo: a dry run, then apply.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-bond)
(require 'beads-command-mol)
(require 'beads-types)
(require 'beads-integration-test)

;;; Test fixtures

(defvar beads-molecule--root nil
  "Buffer-local molecule root, declared so the entry test can bind it.")
(defvar beads-formula-show--formula-name nil
  "Buffer-local formula name, declared so the entry test can bind it.")

(defclass beads-bond-test-formula (beads-formula)
  ()
  "A `beads-formula' for the bond tests.
WI-SF-05 added the typed `bond-points' slot to the real class, so the
subclass no longer redeclares it.")

(defclass beads-bond-test-point (beads-formula-bond-point)
  ()
  "Test alias for the typed `beads-formula-bond-point' (WI-SF-05).")

(defun beads-bond-test--stub-execute (result)
  "Return a `beads-command-execute' stub that always returns RESULT."
  (lambda (&rest _) result))

;;; Command assembly

(ert-deftest beads-bond-test-command-default ()
  "The default command carries both operands and the default type."
  :tags '(:unit)
  (let ((line (beads-command-line
               (beads-bond-command (list :first "alpha" :second "beta")))))
    (should (member "mol" line))
    (should (member "bond" line))
    (should (member "alpha" line))
    (should (member "beta" line))
    (should (equal '("--type" "sequential")
                   (cl-subseq line (cl-position "--type" line :test #'equal)
                              (+ 2 (cl-position "--type" line :test #'equal)))))))

(ert-deftest beads-bond-test-command-default-type-passes-validation ()
  "The real bd values satisfy the command's own choice validator.
Regression: the class metadata advertised seq/par/gate, which `bd'
rejects, so the flow could never build a valid command."
  :tags '(:unit)
  (should-not (beads-command-validate
               (beads-bond-command (list :first "a" :second "b"
                                         :type "sequential"))))
  (should-not (beads-command-validate
               (beads-bond-command (list :first "a" :second "b"
                                         :type "parallel"))))
  (should-not (beads-command-validate
               (beads-bond-command (list :first "a" :second "b"
                                         :type "conditional")))))

(ert-deftest beads-bond-test-command-phase-override ()
  "`--pour' and `--ephemeral' follow the phase scope key."
  :tags '(:unit)
  (let ((pour (beads-command-line
               (beads-bond-command (list :first "a" :second "b" :phase "pour")))))
    (should (member "--pour" pour))
    (should-not (member "--ephemeral" pour)))
  (let ((ephemeral (beads-command-line
                    (beads-bond-command (list :first "a" :second "b"
                                              :phase "ephemeral")))))
    (should (member "--ephemeral" ephemeral))
    (should-not (member "--pour" ephemeral)))
  (let ((follow (beads-command-line
                 (beads-bond-command (list :first "a" :second "b")))))
    (should-not (member "--pour" follow))
    (should-not (member "--ephemeral" follow))))

(ert-deftest beads-bond-test-command-as-ref-vars ()
  "`--as', `--ref' and repeated `--var' serialize from the scope."
  :tags '(:unit)
  (let ((line (beads-command-line
               (beads-bond-command
                (list :first "a" :second "b" :type "sequential"
                      :as "compound" :ref "arm-{{name}}"
                      :vars '(("name" . "ace") ("env" . "prod")))))))
    (should (member "--as" line))
    (should (member "compound" line))
    (should (member "--ref" line))
    (should (member "arm-{{name}}" line))
    (should (member "name=ace" line))
    (should (member "env=prod" line))))

(ert-deftest beads-bond-test-command-dry-run-and-json ()
  "DRY-RUN and JSON control the `--dry-run' and `--json' flags."
  :tags '(:unit)
  (let ((dry (beads-command-line (beads-bond-command (list :first "a" :second "b")
                                                     t nil)))
        (json (beads-command-line (beads-bond-command (list :first "a" :second "b")
                                                      nil t))))
    (should (member "--dry-run" dry))
    (should-not (member "--json" dry))
    (should (member "--json" json))
    (should-not (member "--dry-run" json))))

;;; Validation

(ert-deftest beads-bond-test-validate-missing-operands ()
  "Both operands are required; each miss is reported once."
  :tags '(:unit)
  (let ((warnings (beads-bond-validate nil)))
    (should (= 2 (length warnings)))
    (should (cl-some (lambda (w) (string-match-p "A (source)" w)) warnings))
    (should (cl-some (lambda (w) (string-match-p "B (target)" w)) warnings)))
  (should-not (beads-bond-validate (list :first "a" :second "b")))
  (should (beads-bond--ready-p (list :first "a" :second "b")))
  (should-not (beads-bond--ready-p (list :first "a"))))

(ert-deftest beads-bond-test-validate-as-only-for-proto-pair ()
  "A result name is flagged for a known non-proto pair."
  :tags '(:unit)
  (should (beads-bond-validate
           (list :first "mol-1" :second "mol-2" :as "compound"
                 :first-kind 'molecule :second-kind 'molecule)))
  (should-not (beads-bond-validate
               (list :first "alpha" :second "beta" :as "compound"
                     :first-kind 'formula :second-kind 'formula))))

(ert-deftest beads-bond-test-validate-unknown-bond-point ()
  "An unknown bond point is reported."
  :tags '(:unit)
  (let ((warnings (beads-bond-validate
                   (list :first "a" :second "b"
                         :bond-point "nope"
                         :bond-points (list '((id . "entry")))))))
    (should (cl-some (lambda (w) (string-match-p "Unknown bond point" w))
                     warnings)))
  (should-not (beads-bond-validate
               (list :first "a" :second "b"
                     :bond-point "entry"
                     :bond-points (list '((id . "entry")))))))

;;; Bond points

(ert-deftest beads-bond-test-bond-points-accessor ()
  "Bond points are read from a typed object, a plist and an alist."
  :tags '(:unit)
  (let ((formula (beads-bond-test-formula
                  :bond-points (list (beads-bond-test-point :id "entry")))))
    (should (equal '("entry")
                   (mapcar #'beads-bond--bond-point-id
                           (beads-bond-bond-points formula)))))
  (should (equal '("p")
                 (mapcar #'beads-bond--bond-point-id
                         (beads-bond-bond-points '(:bond-points (("id" . "p")))))))
  (should (equal '("q")
                 (mapcar #'beads-bond--bond-point-id
                         (beads-bond-bond-points '((bond_points . ((id . "q"))))))))
  ;; The slot is absent on the plain class: nil, never a signal.
  (should-not (beads-bond-bond-points (beads-formula :name "plain"))))

(ert-deftest beads-bond-test-bond-point-fields ()
  "A bond point exposes id, description, before/after and parallel."
  :tags '(:unit)
  (let ((point (beads-bond-test-point :id "entry" :description "Setup"
                                      :before-step "design" :parallel t)))
    (should (equal "entry" (beads-bond--bond-point-id point)))
    (should (equal "Setup" (beads-bond--bond-point-description point)))
    (should (equal "design" (beads-bond--bond-point-before point)))
    (should (beads-bond--bond-point-parallel-p point))
    (should (equal "entry — Setup (before step design) [parallel]"
                   (beads-bond--bond-point-describe point))))
  ;; Alist keys use underscores (\`compose.bond_points' JSON).
  (let ((point '((id . "rel") (after_step . "tag") (parallel . t))))
    (should (equal "tag" (beads-bond--bond-point-after point)))
    (should (equal "rel (after step tag) [parallel]"
                   (beads-bond--bond-point-describe point)))))

(ert-deftest beads-bond-test-bond-point-seeds-parallel-type ()
  "A parallel bond point seeds the bond type; otherwise sequential."
  :tags '(:unit)
  (let ((points (list '((id . "p") (parallel . t)) '((id . "s")))))
    (should (equal "parallel" (beads-bond--bond-point-type "p" points)))
    (should (equal "sequential" (beads-bond--bond-point-type "s" points)))
    (should (equal "sequential" (beads-bond--bond-point-type nil points)))))

;;; Operand discovery

(ert-deftest beads-bond-test-operand-candidates-dedupe ()
  "Provider candidates de-duplicate by name, first provider wins."
  :tags '(:unit)
  (let ((beads-bond-operand-functions
         (list (lambda () '(("alpha" . formula) ("beta" . formula)))
               (lambda () '(("alpha" . molecule) ("mol-1" . molecule))))))
    (should (equal '(("alpha" . formula) ("beta" . formula) ("mol-1" . molecule))
                   (beads-bond-operand-candidates)))
    (should (eq 'formula (beads-bond-operand-kind "alpha")))
    (should (eq 'molecule (beads-bond-operand-kind "mol-1")))))

(ert-deftest beads-bond-test-operand-candidates-empty-hook ()
  "An empty hook yields no candidates (standalone degradation)."
  :tags '(:unit)
  (let ((beads-bond-operand-functions nil))
    (should-not (beads-bond-operand-candidates))))

(ert-deftest beads-bond-test-operand-reader-returns-id ()
  "The reader completes over \"NAME (KIND)\" but returns the id."
  :tags '(:unit)
  (let ((beads-bond-operand-functions
         (list (lambda () '(("alpha" . formula))))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (equal '(("alpha (formula)" . "alpha")) collection))
                 "alpha")))
      (should (equal "alpha" (beads-bond-operand-reader "A: "))))))

(ert-deftest beads-bond-test-operand-reader-free-text ()
  "An unmatched id is accepted verbatim (protos are not listable)."
  :tags '(:unit)
  (let ((beads-bond-operand-functions nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "alpha.a1")))
      (should (equal "alpha.a1" (beads-bond-operand-reader "A: "))))))

;;; Labels, preview and cycling

(ert-deftest beads-bond-test-next-type-and-phase ()
  "The cycle helpers wrap around the declared sequences."
  :tags '(:unit)
  (should (equal "parallel" (beads-bond--next-type "sequential")))
  (should (equal "conditional" (beads-bond--next-type "parallel")))
  (should (equal "sequential" (beads-bond--next-type "conditional")))
  (should (equal "pour" (beads-bond--next-phase nil)))
  (should (equal "ephemeral" (beads-bond--next-phase "pour")))
  (should-not (beads-bond--next-phase "ephemeral")))

(ert-deftest beads-bond-test-relation-sentences ()
  "Each bond type has its own operand-relation sentence."
  :tags '(:unit)
  (should (string-match-p "B depends on A"
                          (beads-bond--relation-sentence '(:type "sequential"))))
  (should (string-match-p "parallel"
                          (beads-bond--relation-sentence '(:type "parallel"))))
  (should (string-match-p "only if A fails"
                          (beads-bond--relation-sentence '(:type "conditional")))))

(ert-deftest beads-bond-test-preview-text ()
  "The preview text carries the operands, type, phase, ref and vars."
  :tags '(:unit)
  (let* ((scope (list :first "alpha" :second "beta" :type "sequential"
                      :phase "pour" :as "compound" :ref "arm-{{name}}"
                      :vars '(("name" . "ace"))
                      :bond-point "entry"
                      :bond-points (list '((id . "entry") (description . "Setup")
                                           (before_step . "design")))))
         (text (beads-bond-preview-text scope)))
    (should (string-match-p "Bond preview — alpha \\+ beta (sequential, dry-run)"
                            (car text)))
    (should (string-match-p "Phase: pour (persistent)"
                            (mapconcat #'identity text "\n")))
    (should (string-match-p "Result name: compound"
                            (mapconcat #'identity text "\n")))
    (should (string-match-p "Ref: arm-{{name}}"
                            (mapconcat #'identity text "\n")))
    (should (string-match-p "Vars: name=ace"
                            (mapconcat #'identity text "\n")))
    (should (string-match-p "Bond point: entry — Setup (before step design)"
                            (mapconcat #'identity text "\n")))))

;;; Entry points and the scope

(ert-deftest beads-bond-test-context-first ()
  "The molecule root and formula name pre-fill the source operand."
  :tags '(:unit)
  (let ((beads-molecule--root "mol-abc")
        (beads-formula-show--formula-name "alpha"))
    (should (equal "mol-abc" (beads-bond--context-first))))
  (let ((beads-molecule--root nil)
        (beads-formula-show--formula-name "alpha"))
    (should (equal "alpha" (beads-bond--context-first))))
  (let ((beads-molecule--root nil)
        (beads-formula-show--formula-name nil))
    (should-not (beads-bond--context-first))))

(ert-deftest beads-bond-test-entry-seeds-scope ()
  "`beads-bond' seeds the transient with the pre-filled operand."
  :tags '(:unit)
  (let (captured)
    (cl-letf (((symbol-function 'beads-command-execute)
               (beads-bond-test--stub-execute nil))
              ((symbol-function 'transient-setup)
               (lambda (_name &rest args)
                 (setq captured (plist-get args :scope)))))
      (beads-bond "alpha" nil)
      (should (equal "alpha" (plist-get captured :first)))
      (should (equal "sequential" (plist-get captured :type)))
      ;; `beads-bond-for' is the same entry for programmatic callers.
      (beads-bond-for "beta" nil)
      (should (equal "beta" (plist-get captured :first))))))

(ert-deftest beads-bond-test-entry-seeds-bond-point-type ()
  "An entry with a parallel bond point defaults the bond type."
  :tags '(:unit)
  (let* ((formula (beads-bond-test-formula
                   :bond-points (list (beads-bond-test-point
                                       :id "entry" :parallel t))))
         (captured nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _) formula))
              ((symbol-function 'transient-setup)
               (lambda (_name &rest args)
                 (setq captured (plist-get args :scope)))))
      (beads-bond "alpha" "entry")
      (should (equal "parallel" (plist-get captured :type)))
      (should (equal "entry" (plist-get captured :bond-point)))
      (should (length (plist-get captured :bond-points))))))

;;; Run and preview

(ert-deftest beads-bond-test-run-requires-operands ()
  "The run refuses to invoke bd without both operands."
  :tags '(:unit)
  (should-error (beads-bond-run (list :first "alpha")) :type 'user-error))

(ert-deftest beads-bond-test-run-applies-and-reports ()
  "The run executes the JSON command and returns the parsed result."
  :tags '(:unit)
  (let (seen)
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (setq seen command)
                 '((result_id . "mol-x") (result_type . "compound_proto")))))
      (let ((result (beads-bond-run (list :first "a" :second "b"
                                          :type "parallel"))))
        (should (equal "mol-x" (alist-get 'result_id result)))
        (should (equal "compound_proto" (alist-get 'result_type result)))
        (should (equal "parallel" (oref seen bond-type)))
        (should (oref seen json))))))

(ert-deftest beads-bond-test-preview-paints-dry-run ()
  "The preview buffer renders the client text and the bd dry run."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute)
             (beads-bond-test--stub-execute "Dry run: bond alpha + beta"))
            ((symbol-function 'pop-to-buffer) (lambda (buffer) buffer)))
    (let ((buffer (generate-new-buffer " *beads-bond-preview-test*")))
      (unwind-protect
          (progn
            (beads-bond-preview (list :first "alpha" :second "beta"
                                      :type "sequential"))
            (with-current-buffer beads-bond-preview-buffer-name
              (should (derived-mode-p 'beads-bond-preview-mode))
              (should (string-match-p "Bond preview — alpha \\+ beta"
                                      (buffer-string)))
              (should (string-match-p "B depends on A" (buffer-string)))
              (should (string-match-p "Dry run: bond alpha \\+ beta"
                                      (buffer-string)))))
        (kill-buffer beads-bond-preview-buffer-name)
        (kill-buffer buffer)))))

(ert-deftest beads-bond-test-preview-run-never-gated ()
  "`s' in the preview applies exactly the stored scope."
  :tags '(:unit)
  (let ((captured nil)
        (buffer (generate-new-buffer " *beads-bond-preview-run-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-bond-run)
                   (lambda (scope) (setq captured scope) 'result)))
          (with-current-buffer buffer
            (beads-bond-preview-mode)
            (setq beads-bond-preview--scope '(:first "a" :second "b"))
            (beads-bond-preview-run))
          (should (equal '(:first "a" :second "b") captured)))
      (kill-buffer buffer))))

;;; Transient layout

(ert-deftest beads-bond-test-transient-parses ()
  "The bond menu parses cold and seeded, with every group rendered."
  :tags '(:unit :transient)
  (unwind-protect
      (progn
        (transient-setup 'beads-bond--transient nil nil
                         :scope (list :first nil :second nil :type "sequential"))
        (with-current-buffer " *transient*"
          (should (string-match-p "Bond (mol bond)" (buffer-string)))
          (should (string-match-p "Operands" (buffer-string)))
          (should (string-match-p "A (source): (pick)" (buffer-string)))
          (should (string-match-p "B (target): (pick)" (buffer-string)))
          (should (string-match-p "Options" (buffer-string)))
          (should (string-match-p "Actions" (buffer-string)))
          (should (string-match-p "Dry-run preview" (buffer-string))))
        (ignore-errors (transient-quit-all))
        (transient-setup 'beads-bond--transient nil nil
                         :scope (list :first "alpha" :second "beta"
                                      :first-kind 'formula :second-kind 'formula
                                      :type "parallel" :phase "pour"
                                      :vars '(("name" . "ace"))
                                      :bond-point "entry"
                                      :bond-points (list '((id . "entry")
                                                           (description . "Setup")))))
        (with-current-buffer " *transient*"
          (should (string-match-p "A alpha \\[formula\\]" (buffer-string)))
          (should (string-match-p "Type: parallel" (buffer-string)))
          (should (string-match-p "Phase: pour (persistent)" (buffer-string)))
          (should (string-match-p "Vars: name=ace" (buffer-string)))))
    (ignore-errors (transient-quit-all))))

(ert-deftest beads-bond-test-info-descriptions-are-quoted-lambdas ()
  "The header/footer `:info' values are quoted lambda forms.
See the beads-sling regression (be-d9ht): transient 0.13.8 `eval's an
`:info' value unquoted, so on Emacs 29.4 a runtime closure is called as
a function and the menu dies with `void-function closure'."
  :tags '(:unit :transient)
  (let* ((specs (beads-bond--children-specs '(:first "a" :second "b")))
         (group (car specs))
         (children (append group nil))
         (infos (cl-remove-if-not (lambda (child) (eq (car-safe child) :info))
                                  children)))
    (should (= 2 (length infos)))
    (dolist (info infos)
      (should (eq (car-safe (cadr info)) 'lambda))
      (should (functionp (eval (cadr info) t))))))

;;; Integration

(defun beads-bond-test--write-formula (name description)
  "Write a minimal NAME formula with DESCRIPTION into the temp repo."
  (let ((path (expand-file-name (format ".beads/formulas/%s.formula.toml" name))))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert (format "formula = \"%s\"\ndescription = \"%s\"\ntype = \"workflow\"\nversion = 1\n\n[[steps]]\nid = \"s1\"\ntitle = \"%s step\"\ntype = \"task\"\n"
                      name description name)))
    path))

(ert-deftest beads-bond-test-integration-dry-run-then-apply ()
  "Bond two formulas: dry-run preview, then apply, against real bd."
  :tags '(:integration)
  (beads-test-skip-unless-bd)
  (skip-unless (beads-test-bd-has-subcommand-p "cook"))
  (beads-test-with-temp-repo (:init-beads t)
    (beads-bond-test--write-formula "alpha" "Alpha workflow")
    (beads-bond-test--write-formula "beta" "Beta workflow")
    ;; Bonding two formula names cooks them inline in newer bd; cook
    ;; the protos first so the apply is version-robust.
    (dolist (name '("alpha" "beta"))
      (should (zerop (call-process (or (bound-and-true-p beads-executable) "bd")
                                   nil nil nil "cook" name "--persist"))))
    (let ((scope (list :first "alpha" :second "beta" :type "sequential")))
      ;; Dry run: the preview shows the real bd plan, nothing written.
      (let ((dry (beads-bond--dry-run-output scope)))
        (should (string-match-p "Dry run: bond alpha \\+ beta" dry))
        (should (string-match-p "sequential" dry)))
      ;; Apply: a compound proto is created and the result id returned.
      (let ((result (beads-bond-run scope)))
        (should (alist-get 'result_id result))
        (should (equal "compound_proto" (alist-get 'result_type result)))))))

(provide 'beads-bond-test)
;;; beads-bond-test.el ends here
