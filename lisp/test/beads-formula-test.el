;;; beads-formula-test.el --- Tests for beads-formula -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Unit tests for the formula browser/detail/launch UI and ABI in
;; `beads-formula.el': typed var readers, type grouping, detail
;; sections, sling seeding, and the `beads-formula-launch' default.
;; `beads-command-execute' is mocked, so no `bd' is required.

;;; Code:

(require 'ert)
(require 'beads-formula)
(require 'beads-command-mol)
(require 'beads-test)

;;; ========================================
;;; Typed variable readers
;;; ========================================

(ert-deftest beads-formula-test-var-reader-kind-table ()
  "The reader kind follows enum, type, then naming conventions."
  :tags '(:unit)
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "x" :enum '("a" "b")))
              'enum))
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "x" :var-type "bool"))
              'bool))
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "x" :var-type "int"))
              'numeric))
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "context_path"))
              'file))
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "artifact_root"))
              'directory))
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "implementation_target"))
              'agent))
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "max_iterations"))
              'numeric))
  (should (eq (beads-formula-var-kind
               (beads-formula-var :name "interaction_mode"))
              'string)))

(ert-deftest beads-formula-test-var-reader-carries-metadata ()
  "The reader spec carries the declared metadata."
  :tags '(:unit)
  (let* ((var (beads-formula-var
               :name "mode"
               :description "How to run"
               :required t
               :default "auto"
               :enum '("auto" "manual")
               :pattern "^a"))
         (spec (beads-formula-var-reader var)))
    (should (eq (plist-get spec :kind) 'enum))
    (should (equal (plist-get spec :name) "mode"))
    (should (equal (plist-get spec :prompt) "How to run"))
    (should (plist-get spec :required))
    (should (equal (plist-get spec :default) "auto"))
    (should (equal (plist-get spec :choices) '("auto" "manual")))
    (should (equal (plist-get spec :pattern) "^a"))))

;;; ========================================
;;; Grouping
;;; ========================================

(ert-deftest beads-formula-test-grouped-entries ()
  "Formulas are grouped by type, headers included, members sorted."
  :tags '(:unit)
  (let* ((formulas (list (beads-formula-summary
                          :name "beta" :formula-type "workflow")
                         (beads-formula-summary
                          :name "alpha" :formula-type "workflow")
                         (beads-formula-summary
                          :name "lint" :formula-type "aspect")))
         (entries (beads-formula-grouped-entries formulas)))
    ;; 2 headers + 3 formula rows.
    (should (= 5 (length entries)))
    ;; workflow header first, then its sorted members.
    (should (beads-formula-group-header-p (car (nth 0 entries))))
    (should (equal (cdr (car (nth 0 entries))) "workflow"))
    (should (equal (nth 0 (nth 1 entries)) "alpha"))
    (should (equal (nth 0 (nth 2 entries)) "beta"))
    ;; aspect header (recognised type order) then its member.
    (should (beads-formula-group-header-p (car (nth 3 entries))))
    (should (equal (cdr (car (nth 3 entries))) "aspect"))
    (should (equal (nth 0 (nth 4 entries)) "lint"))))

(ert-deftest beads-formula-test-grouped-entries-unknown-type ()
  "Recognised types precede unrecognised ones; untyped still groups."
  :tags '(:unit)
  (let* ((formulas (list (beads-formula-summary :name "z" :formula-type "custom")
                         (beads-formula-summary :name "y" :formula-type nil)
                         (beads-formula-summary :name "x" :formula-type "workflow")))
         (entries (beads-formula-grouped-entries formulas)))
    (should (equal (cdr (car (nth 0 entries))) "workflow"))
    ;; Remaining headers are sorted alphabetically: "" (untyped) then custom.
    (should (equal (cdr (car (nth 2 entries))) ""))
    (should (equal (cdr (car (nth 4 entries))) "custom"))))

(ert-deftest beads-formula-test-populate-uses-entry-function ()
  "`beads-formula-list--populate-buffer' honours the buffer-local builder."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-list-mode)
    (setq-local beads-formula-list--entry-function
                #'beads-formula-grouped-entries)
    (beads-formula-list--populate-buffer
     (list (beads-formula-summary :name "a" :formula-type "workflow")
           (beads-formula-summary :name "b" :formula-type "aspect"))
     nil)
    ;; 2 headers + 2 rows.
    (should (= 4 (length tabulated-list-entries)))))

;;; ========================================
;;; Detail sections
;;; ========================================

(ert-deftest beads-formula-test-detail-sections ()
  "Detail sections list Vars, Steps and Source in order."
  :tags '(:unit)
  (let ((formula (beads-formula
                  :name "build-basic"
                  :vars (list (beads-formula-var :name "a")
                              (beads-formula-var :name "b"))
                  :steps (list (beads-formula-step :id "prepare"))
                  :source "/tmp/build-basic.toml")))
    (should (equal (mapcar (lambda (s) (plist-get s :key))
                           (beads-formula-detail-sections formula))
                   '(vars steps source)))
    (should (equal (plist-get (car (beads-formula-detail-sections formula))
                              :title)
                   "Vars (2)"))))

(ert-deftest beads-formula-test-detail-sections-empty ()
  "A formula with no vars/steps/source has no detail sections."
  :tags '(:unit)
  (should (null (beads-formula-detail-sections
                 (beads-formula :name "bare")))))

;;; ========================================
;;; Sling seeding
;;; ========================================

(ert-deftest beads-formula-test-sling-scope-seeds-formula ()
  "The sling scope pre-fills the formula stage and leaves work empty."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (cmd)
               (should (cl-typep cmd 'beads-command-formula-show))
               (beads-formula :name "build-basic"
                              :vars (list (beads-formula-var :name "a"))))))
    (let ((scope (beads-formula-sling-scope "build-basic")))
      (should (equal (plist-get scope :formula) "build-basic"))
      (should (beads-formula-p (plist-get scope :recipe)))
      (should (null (plist-get scope :work)))
      (should (null (plist-get scope :target))))))

;;; ========================================
;;; Launch
;;; ========================================

(ert-deftest beads-formula-test-launch-default-mol-pour ()
  "The default launch irons the formula through `bd mol pour'."
  :tags '(:unit)
  (let ((captured nil)
        (followed nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq captured cmd)
                 '((root_id . "mol-1"))))
              ((symbol-function 'beads-formula-follow)
               (lambda (result context)
                 (setq followed (list result context))
                 context)))
      (let* ((formula (beads-formula :name "pancakes"))
             (context (beads-formula-launch formula nil '((taste . "sweet")))))
        (should (cl-typep captured 'beads-command-mol-pour))
        (should (equal (oref captured proto-id) "pancakes"))
        (should (equal (oref captured var) '("taste=sweet")))
        (should (cl-typep context 'beads-formula-launch-context))
        (should (eq (oref context shape) 'formula))
        (should (equal (oref context vars) '(("taste" . "sweet"))))
        (should (null (oref context warnings)))
        (should followed)))))

(ert-deftest beads-formula-test-launch-against-bead-shape-on ()
  "Launching against a bead infers the `on' shape."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute) (lambda (_cmd) nil))
            ((symbol-function 'beads-formula-follow) (lambda (_r c) c)))
    (let ((context (beads-formula-launch
                    (beads-formula :name "build-basic") "be-1" nil)))
      (should (eq (oref context shape) 'on)))))

(ert-deftest beads-formula-test-launch-warns-required ()
  "A missing required var becomes a launch warning."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-execute) (lambda (_cmd) nil))
            ((symbol-function 'beads-formula-follow) (lambda (_r c) c)))
    (let ((context (beads-formula-launch
                    (beads-formula
                     :name "build-basic"
                     :vars (list (beads-formula-var :name "target" :required t)))
                    nil nil)))
      (should (= 1 (length (oref context warnings)))))))

(ert-deftest beads-formula-test-launch-string-resolves ()
  "The string method resolves the formula, then launches it."
  :tags '(:unit)
  (let ((captured nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (if (cl-typep cmd 'beads-command-formula-show)
                     (beads-formula :name "pancakes")
                   (setq captured cmd)
                   nil)))
              ((symbol-function 'beads-formula-follow) (lambda (_r c) c)))
      (beads-formula-launch "pancakes" nil nil)
      (should (cl-typep captured 'beads-command-mol-pour))
      (should (equal (oref captured proto-id) "pancakes")))))

(ert-deftest beads-formula-test-result-root-id ()
  "The root id is found in the decoded pour output."
  :tags '(:unit)
  (should (equal (beads-formula--result-root-id '((root_id . "mol-1")))
                 "mol-1"))
  (should (equal (beads-formula--result-root-id
                  '((foo . 1) (bar . ((id . "nested")))))
                 "nested"))
  (should (null (beads-formula--result-root-id nil))))

;;; ========================================
;;; Standalone launch
;;; ========================================

(ert-deftest beads-formula-test-read-vars-required-without-default ()
  "Only required vars without a default are prompted for."
  :tags '(:unit)
  (let* ((formula (beads-formula
                   :name "f"
                   :vars (list (beads-formula-var :name "req" :required t)
                               (beads-formula-var :name "opt")
                               (beads-formula-var :name "def"
                                                  :required t
                                                  :default "d"))))
         (asked nil))
    (cl-letf (((symbol-function 'beads-formula-read-var)
               (lambda (var)
                 (push (oref var name) asked)
                 "value")))
      (let ((vars (beads-formula-read-vars formula)))
        (should (equal asked '("req")))
        (should (equal vars '(("req" . "value"))))))))

(ert-deftest beads-formula-test-launch-standalone-reads-then-launches ()
  "Standalone launch reads required vars and calls the launch generic."
  :tags '(:unit)
  (let ((launched nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) (beads-formula :name "build-basic")))
              ((symbol-function 'beads-formula-read-vars)
               (lambda (_formula) '(("target" . "beads.el"))))
              ((symbol-function 'beads-formula-launch)
               (lambda (formula bead vars)
                 (setq launched (list formula bead vars))
                 'context)))
      (beads-formula-launch-standalone "build-basic")
      (should (equal (nth 1 launched) nil))
      (should (equal (nth 2 launched) '(("target" . "beads.el"))))
      (should (beads-formula-p (car launched))))))

;;; ========================================
;;; Provenance types (REQ-SF-010/011/052)
;;; ========================================

(defconst beads-formula-test--rich-json
  '((formula . "rich")
    (description . "Rich formula")
    (version . 2)
    (type . "workflow")
    (phase . "vapor")
    (extends . ["base-one" "base-two"])
    (source . "/tmp/rich.formula.toml")
    (vars . ((mode . ((description . "Mode")
                      (type . "string")
                      (enum . ["a" "b"])
                      (pattern . "^[ab]$")
                      (default . "a")
                      (required . t)))))
    (steps . [((id . "prepare") (title . "Prepare"))
              ((id . "build")
               (title . "Build")
               (needs . ["prepare"])
               (waits_for . "prep-gate")
               (gate . ((id . "approve") (type . "human"))))])
    (compose . ((aspects . ["security-audit" "logging"])
                (bond_points . [((id . "entry")
                                 (description . "Attach setup work here")
                                 (before_step . "build"))
                                ((id . "release-after")
                                 (after_step . "verify")
                                 (parallel . t))])
                (expand . [((target . "build")
                            (with . "expansion-formula"))])
                (map . [((select . "*.verify")
                         (with . "map-formula"))]))))
  "Fixture mirroring `bd formula show rich --json'.")

(ert-deftest beads-formula-test-from-json-provenance ()
  "`beads-formula-from-json' reads phase, extends and composition."
  :tags '(:unit)
  (let ((formula (beads-formula-from-json beads-formula-test--rich-json)))
    (should (equal (oref formula phase) "vapor"))
    (should (equal (oref formula extends) '("base-one" "base-two")))
    (should (equal (oref formula aspects) '("security-audit" "logging")))
    (should (equal (oref formula expansions)
                   '("expansion-formula" "map-formula")))
    (should (= 2 (length (oref formula bond-points))))
    (let ((entry (car (oref formula bond-points))))
      (should (equal (oref entry id) "entry"))
      (should (equal (oref entry before-step) "build"))
      (should (equal (oref entry description) "Attach setup work here")))
    (let ((release (nth 1 (oref formula bond-points))))
      (should (equal (oref release after-step) "verify"))
      (should (oref release parallel)))))

(ert-deftest beads-formula-test-from-json-step-gate-waits-for ()
  "Steps carry their declared gate and waits_for."
  :tags '(:unit)
  (let* ((formula (beads-formula-from-json beads-formula-test--rich-json))
         (build (seq-find (lambda (step) (equal (oref step id) "build"))
                          (oref formula steps))))
    (should (equal (oref build waits-for) "prep-gate"))
    (should (beads-formula-gate-p (oref build gate)))
    (should (equal (oref (oref build gate) type) "human"))
    (should (equal (oref (oref build gate) id) "approve"))))

(ert-deftest beads-formula-test-from-json-var-constraints ()
  "Variables keep their declared type, enum and pattern (REQ-SF-011)."
  :tags '(:unit)
  (let* ((formula (beads-formula-from-json beads-formula-test--rich-json))
         (var (car (oref formula vars))))
    (should (equal (oref var name) "mode"))
    (should (equal (oref var var-type) "string"))
    (should (equal (oref var enum) '("a" "b")))
    (should (equal (oref var pattern) "^[ab]$"))
    (should (oref var required))))

(ert-deftest beads-formula-test-detail-sections-composition ()
  "Detail sections list bond points and composition before source."
  :tags '(:unit)
  (let ((formula (beads-formula-from-json beads-formula-test--rich-json)))
    (should (equal (mapcar (lambda (section) (plist-get section :key))
                           (beads-formula-detail-sections formula))
                   '(vars steps bond-points composition source)))
    (should (equal (plist-get (nth 2 (beads-formula-detail-sections formula))
                              :title)
                   "Bond points (2)"))))

(ert-deftest beads-formula-test-list-entry-columns ()
  "The list row carries phase and an abbreviated source path."
  :tags '(:unit)
  (let* ((formula (beads-formula-summary
                   :name "release"
                   :formula-type "workflow"
                   :phase "vapor"
                   :source "/tmp/release.formula.toml"
                   :steps 6
                   :vars 1))
         (entry (beads-formula-list--formula-to-entry formula))
         (row (nth 1 entry)))
    (should (equal (nth 0 entry) "release"))
    (should (equal (aref row 4) "vapor"))
    (should (equal (aref row 5) "/tmp/release.formula.toml"))
    (should (= 6 (length row)))))

(ert-deftest beads-formula-test-list-mode-has-provenance-columns ()
  "The browser format exposes Name/Type/Steps/Vars/Phase/Source."
  :tags '(:unit)
  (with-temp-buffer
    (beads-formula-list-mode)
    (should (equal (mapcar #'car tabulated-list-format)
                   '("Name" "Type" "Steps" "Vars" "Phase" "Source")))))

;;; ========================================
;;; Scope filter and shadowing
;;; ========================================

(ert-deftest beads-formula-test-scope-of ()
  "`beads-formula-scope-of' classifies sources by search path."
  :tags '(:unit)
  (let* ((root (make-temp-file "beads-scope" t))
         (project (expand-file-name ".beads/formulas" root)))
    (unwind-protect
        (progn
          (should (eq (beads-formula-scope-of
                       (expand-file-name "a.formula.toml" project) root)
                      'project))
          (should (eq (beads-formula-scope-of
                       (expand-file-name "~/.beads/formulas/a.formula.toml")
                       root)
                      'user))
          (should (eq (beads-formula-scope-of "/tmp/elsewhere/a.formula.toml"
                                              root)
                      'other))
          (should (eq (beads-formula-scope-of nil root) 'other)))
      (delete-directory root t))))

(ert-deftest beads-formula-test-filter-by-scope ()
  "`beads-formula-filter-by-scope' keeps only matching sources."
  :tags '(:unit)
  (let* ((root (make-temp-file "beads-scope" t))
         (project (expand-file-name ".beads/formulas" root))
         (project-formula (beads-formula-summary
                           :name "proj"
                           :source (expand-file-name "p.formula.toml" project)))
         (user-formula (beads-formula-summary
                        :name "user"
                        :source (expand-file-name
                                 "~/.beads/formulas/u.formula.toml")))
         (formulas (list project-formula user-formula)))
    (unwind-protect
        (progn
          (should (equal (beads-formula-filter-by-scope formulas 'all root)
                         formulas))
          (should (equal (beads-formula-filter-by-scope formulas 'project root)
                         (list project-formula)))
          (should (equal (beads-formula-filter-by-scope formulas 'user root)
                         (list user-formula))))
      (delete-directory root t))))

(ert-deftest beads-formula-test-shadow-index-and-shadowed-by ()
  "A same-name file lower on the search path is reported as shadowed."
  :tags '(:unit)
  (let* ((project (make-temp-file "beads-shadow-proj" t))
         (gt (make-temp-file "beads-shadow-gt" t))
         (project-dir (expand-file-name ".beads/formulas" project))
         (gt-dir (expand-file-name ".beads/formulas" gt)))
    (unwind-protect
        (progn
          (make-directory project-dir t)
          (make-directory gt-dir t)
          (with-temp-file (expand-file-name "build.formula.toml" project-dir)
            (insert "formula = \"build\"\n"))
          (with-temp-file (expand-file-name "build.formula.toml" gt-dir)
            (insert "formula = \"build\"\n"))
          (with-temp-file (expand-file-name "only.formula.toml" project-dir)
            (insert "formula = \"only\"\n"))
          (cl-letf (((symbol-function 'getenv)
                     (lambda (name)
                       (if (equal name "GT_ROOT") gt nil))))
            (let* ((index (beads-formula-shadow-index project))
                   (winner (beads-formula-summary
                            :name "build"
                            :source (expand-file-name "build.formula.toml"
                                                      project-dir)))
                   (solo (beads-formula-summary
                          :name "only"
                          :source (expand-file-name "only.formula.toml"
                                                    project-dir))))
              (should (equal (beads-formula-shadowed-by winner index)
                             (list (expand-file-name "build.formula.toml"
                                                     gt-dir))))
              (should (null (beads-formula-shadowed-by solo index))))))
      (delete-directory project t)
      (delete-directory gt t))))

(ert-deftest beads-formula-test-browser-entry-marks-shadowed ()
  "A shadowing formula's Name cell carries the shadow marker."
  :tags '(:unit)
  (let* ((index (make-hash-table :test #'equal))
         (formula (beads-formula-summary
                   :name "build"
                   :formula-type "workflow"
                   :source "/proj/build.formula.toml")))
    (puthash "build" (list "/proj/build.formula.toml" "/user/build.formula.toml")
             index)
    (with-temp-buffer
      (setq-local beads-formula-browser-shadow-index index)
      (let* ((entry (beads-formula-browser--entry formula))
             (row (nth 1 entry)))
        (should (equal (nth 0 entry) "build"))
        (should (string-match-p "⧉shad" (aref row 0)))
        (should (= 6 (length row)))))))

(provide 'beads-formula-test)
;;; beads-formula-test.el ends here
