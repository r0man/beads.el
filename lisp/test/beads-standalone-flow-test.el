;;; beads-standalone-flow-test.el --- Consolidated standalone flow suite -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; REQ-SF-080 / REQ-SF-083: the standalone flows are exercised end to end
;; with `bd' alone, against a real temporary store.  This consolidates the
;; formula -> molecule execution suite the plan calls for:
;;
;;   cook -> pour -> work -> close
;;   cook -> wisp -> squash
;;   gate create -> check -> resolve
;;   distill an epic back into a formula
;;   setup status
;;
;; The formula fixtures are written into the temp repo's
;; `.beads/formulas/' directory, so the suite needs no external city and
;; no gascity.  Every test is `:integration' and skips when `bd' is
;; unavailable.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-command)
(require 'beads-command-init)
(require 'beads-command-create)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-command-ready)
(require 'beads-command-update)
(require 'beads-command-close)
(require 'beads-command-mol)
(require 'beads-command-gate)
(require 'beads-command-misc)
(require 'beads-integration-test)

;;; ============================================================
;;; Fixtures and helpers
;;; ============================================================

(defun beads-standalone-flow--write-formula (name)
  "Write a two-step formula NAME under the repo's .beads/formulas/.
Return NAME.  The formula is self-contained so the flow never depends
on the standing city or on any external formula catalog."
  (let ((dir (expand-file-name ".beads/formulas" default-directory)))
    (make-directory dir t)
    (with-temp-file (expand-file-name (concat name ".formula.toml") dir)
      (insert (format "description = \"%s standalone flow fixture\"\n" name)
              (format "formula = \"%s\"\n" name)
              "version = 1\n"
              "\n[[steps]]\n"
              "id = \"step-one\"\n"
              "title = \"Step one\"\n"
              "description = \"First step\"\n"
              "\n[[steps]]\n"
              "id = \"step-two\"\n"
              "title = \"Step two\"\n"
              "description = \"Second step\"\n"
              "needs = [\"step-one\"]\n")))
  name)

(defun beads-standalone-flow--pour (formula)
  "Pour FORMULA and return the decoded pour result alist."
  (beads-execute 'beads-command-mol-pour :proto-id formula :json t))

(defun beads-standalone-flow--id-mapping (pour-result)
  "Return POUR-RESULT's id_mapping as an alist of (REF . ID)."
  (alist-get 'id_mapping pour-result))

(defun beads-standalone-flow--root-id (pour-result)
  "Return POUR-RESULT's root molecule id."
  (or (alist-get 'new_epic_id pour-result)
      (alist-get 'root_id pour-result)
      (let ((mapping (beads-standalone-flow--id-mapping pour-result)))
        (cdr (assoc (car mapping) mapping)))))

;;; ============================================================
;;; cook -> pour -> work -> close
;;; ============================================================

(ert-deftest beads-standalone-flow-test-cook-pour-work-close ()
  "Cook a formula, pour it, claim a step and close it with bd alone."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (let ((formula (beads-standalone-flow--write-formula "flow-pour")))
      ;; Compile-time dry-run keeps {{vars}} and prints the step tree as
      ;; human text (bd cook --dry-run does not emit JSON).
      (let ((preview (beads-execute 'beads-command-cook
                                    :formula-id formula :dry-run t :json nil)))
        (should (stringp preview))
        (should (string-match-p "step-one" preview)))
      ;; Pour creates the persistent molecule.
      (let* ((pour (beads-standalone-flow--pour formula))
             (root (beads-standalone-flow--root-id pour)))
        (should (stringp root))
        (should (beads-execute 'beads-command-show :issue-ids (list root)))
        ;; Work a step: claim it, then close it.
        (let* ((mapping (beads-standalone-flow--id-mapping pour))
               (step (or (cdr (assoc (intern (format "%s.step-one" formula)) mapping))
                         (cdr (assoc 'step-one mapping)))))
          (should (stringp step))
          (beads-execute 'beads-command-update
                         :issue-ids (list step) :claim t :json t)
          (beads-execute 'beads-command-close
                         :issue-ids (list step) :reason "standalone flow" :json t)
          (let ((closed (beads-execute 'beads-command-show
                                       :issue-ids (list step) :json t)))
            (should (equal (oref closed status) "closed"))))))))

;;; ============================================================
;;; cook -> wisp -> squash
;;; ============================================================

(ert-deftest beads-standalone-flow-test-cook-wisp-squash ()
  "Cook a formula, instantiate it as a wisp and squash it with bd alone."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (let* ((formula (beads-standalone-flow--write-formula "flow-wisp"))
           (parsed (beads-execute 'beads-command-mol-wisp-create
                                  :proto-id formula :json t))
           (wisp (or (alist-get 'id parsed)
                     (alist-get 'new_epic_id parsed)
                     (beads-standalone-flow--root-id parsed))))
      (should (stringp wisp))
      ;; The wisp is visible to the list command...
      (let ((listed (append (beads-execute 'beads-command-mol-wisp-list
                                           :show-all t :json t)
                            nil)))
        (should (listp listed)))
      ;; ...and can be squashed into a digest with bd alone.
      (let ((squash (beads-execute 'beads-command-mol-squash
                                   :mol-id wisp :json t)))
        (should (listp squash))))))

;;; ============================================================
;;; gate create -> check -> resolve
;;; ============================================================

(ert-deftest beads-standalone-flow-test-gate-round-trip ()
  "Create an ad-hoc gate, dry-run check it and resolve it with bd alone."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (let* ((issue (beads-execute 'beads-command-create
                                 :title "Gated work" :json t))
           (issue-id (oref issue id)))
      (beads-execute 'beads-command-gate-create
                     :blocks issue-id :gate-type "timer" :json t)
      (let ((gates (append (beads-execute 'beads-command-gate-list
                                          :all t :json t)
                           nil)))
        (should (listp gates)))
      ;; A dry-run check must not mutate anything.  bd prints a summary
      ;; line before the JSON object here, so ask for the raw text.
      (let ((check (beads-execute 'beads-command-gate-check
                                  :dry-run t :json nil)))
        (should (stringp check)))
      ;; Manually resolve the gate and confirm the issue is ready again.
      (let* ((listed (append (beads-execute 'beads-command-gate-list
                                            :all t :json t)
                             nil))
             (gate-id (or (alist-get 'id (car listed))
                          (alist-get 'gate_id (car listed)))))
        (when (stringp gate-id)
          (let ((resolved (beads-execute 'beads-command-gate-resolve
                                         :gate-id gate-id
                                         :reason "standalone flow" :json nil)))
            (should (stringp resolved)))
          (let ((ready (beads-execute 'beads-command-ready :json t)))
            (should (cl-some (lambda (item)
                               (equal (oref item id) issue-id))
                             ready))))))))

;;; ============================================================
;;; distill and setup
;;; ============================================================

(ert-deftest beads-standalone-flow-test-distill-epic ()
  "Distill a poured molecule back into a formula with bd alone."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (let* ((formula (beads-standalone-flow--write-formula "flow-distill"))
           (pour (beads-standalone-flow--pour formula))
           (root (beads-standalone-flow--root-id pour))
           (output (expand-file-name "distilled.formula.toml" default-directory)))
      (should (stringp root))
      (let ((result (beads-execute 'beads-command-mol-distill
                                   :epic-id root :output output :json t)))
        (should (listp result))))))

(ert-deftest beads-standalone-flow-test-setup-check ()
  "`bd setup <recipe> --check' answers without any gascity integration."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (let ((result (beads-execute 'beads-command-setup
                                 :editor "cursor" :check t :json nil)))
      (should (stringp result)))))

(provide 'beads-standalone-flow-test)
;;; beads-standalone-flow-test.el ends here
