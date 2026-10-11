;;; beads-molecule-coverage-test.el --- Extra coverage for molecule view -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Additional `:unit' coverage for `beads-molecule.el': the progress
;; formatting helpers, the section/header/step/output vnode builders,
;; the async aggregate loaders, buffer naming, and the work-loop
;; commands (store mocked).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-molecule)

;;; Fixtures

(defconst beads-molecule-cov--model
  (list :id "mol-1"
        :title "Test Molecule"
        :phase "apply"
        :assignee "worker"
        :percent 25.0
        :ready-count 1
        :blocked-count 1
        :gated-count 0
        :progress (list :completed 1 :total 4 :percent 25.0
                        :rate 2.0 :eta 7200.0)
        :steps (list (list :id "mol-1.a" :issue-type "task" :title "A"
                           :status "done" :issue-status "closed")
                     (list :id "mol-1.b" :issue-type "task" :title "B"
                           :status "ready")
                     (list :id "mol-1.c" :issue-type "task" :title "C"
                           :status "pending" :blocked t :needs '("mol-1.b"))
                     (list :id "mol-1.d" :issue-type "task" :title "D"
                           :status "pending" :gate (list :id "gate-1")))
        :gates (list (list :id "gate-1" :await-type "human" :status "open"
                           :blocked '("mol-1.d")))
        :output (list (list :id "mol-1.a" :title "A"
                            :closed-at "2026-10-07T10:00:00Z"
                            :close-reason "done")
                      (list :id "mol-1.b" :title "B"
                            :closed-at "2026-10-07T11:00:00Z"))))

;;; Progress formatting

(ert-deftest beads-molecule-cov-percent-test ()
  "Unit test: percent is taken or derived from completed/total."
  :tags '(:unit)
  (should (= (beads-molecule--percent '(:progress (:percent 42.5))) 42.5))
  (should (= (beads-molecule--percent '(:progress (:completed 1 :total 4))) 25.0))
  (should-not (beads-molecule--percent '(:progress (:completed 0 :total 0))))
  (should-not (beads-molecule--percent '(:progress nil))))

(ert-deftest beads-molecule-cov-rate-eta-test ()
  "Unit test: rate and ETA formatting handle each input shape."
  :tags '(:unit)
  (should (equal (beads-molecule--rate '(:progress (:rate 2.0))) "2.0/h"))
  (should (equal (beads-molecule--rate '(:progress nil)) "—"))
  (should (equal (beads-molecule--eta '(:progress (:eta 3600))) "1h"))
  (should (equal (beads-molecule--eta '(:progress (:eta "2h"))) "2h"))
  (should (equal (beads-molecule--eta '(:progress nil)) "—")))

;;; Section chrome

(ert-deftest beads-molecule-cov-section-header-vnode-test ()
  "Unit test: section headers fold and carry a click action."
  :tags '(:unit)
  (let ((beads-molecule--collapsed nil))
    (should (beads-molecule--section-header-vnode 'steps "Steps" 3)))
  (let ((beads-molecule--collapsed '((steps . t))))
    (should (beads-molecule--section-header-vnode 'steps "Steps" 3)))
  (should (beads-molecule--section-header-vnode 'output "Output" nil)))

(ert-deftest beads-molecule-cov-step-and-output-line-test ()
  "Unit test: step and output lines cover needs, gate, and reason."
  :tags '(:unit)
  (let* ((blocked (nth 2 (plist-get beads-molecule-cov--model :steps)))
         (gated (nth 3 (plist-get beads-molecule-cov--model :steps)))
         (done (nth 0 (plist-get beads-molecule-cov--model :steps))))
    (should (stringp (beads-molecule--step-line blocked 3)))
    (should (string-match-p "needs mol-1.b" (beads-molecule--step-line blocked 3)))
    (should (string-match-p "gate-1" (beads-molecule--step-line gated 4))))
  (should (string-match-p "reason" (beads-molecule--output-line
                                    (car (plist-get beads-molecule-cov--model :output)))))
  (should (string-match-p "mol-1.b" (beads-molecule--output-line
                                     (nth 1 (plist-get beads-molecule-cov--model :output)))))
  (should (equal (beads-molecule--short-time "short") "short"))
  (should (equal (beads-molecule--short-time nil) "—"))
  (should (stringp (beads-molecule--join-lines '("a" "b")))))

;;; Header and section vnodes

(ert-deftest beads-molecule-cov-header-vnodes-test ()
  "Unit test: the header vnodes render id, progress, and frontier."
  :tags '(:unit)
  (should (= 3 (length (beads-molecule--header-vnodes beads-molecule-cov--model))))
  ;; Missing assignee falls back to "(none)".
  (should (beads-molecule--header-vnodes
           (plist-put (copy-sequence beads-molecule-cov--model) :assignee nil))))

(ert-deftest beads-molecule-cov-section-vnodes-test ()
  "Unit test: each section builder returns a vnode, empty or filled."
  :tags '(:unit)
  (let ((beads-molecule--collapsed nil))
    (should (beads-molecule--steps-vnode beads-molecule-cov--model nil))
    (should (beads-molecule--steps-vnode beads-molecule-cov--model t))
    (should (beads-molecule--gates-vnode beads-molecule-cov--model))
    (should (beads-molecule--output-vnode beads-molecule-cov--model))
    (should (beads-molecule--steps-vnode (list :steps nil) nil))
    (should (beads-molecule--steps-vnode (list :steps nil) t))
    (should (beads-molecule--gates-vnode (list :gates nil)))
    (should (beads-molecule--output-vnode (list :output nil))))
    (should (beads-molecule--section-vnode 'steps beads-molecule-cov--model nil))
    (should (beads-molecule--section-vnode 'gates beads-molecule-cov--model nil))
  (should (beads-molecule--section-vnode 'output beads-molecule-cov--model nil))
  (should-not (beads-molecule--section-vnode 'bogus beads-molecule-cov--model nil)))

(ert-deftest beads-molecule-cov-extra-and-error-test ()
  "Unit test: section hooks are isolated and errors render inline."
  :tags '(:unit)
  (let ((beads-molecule-section-functions
         (list (lambda (_) nil)
               (lambda (_) (vui-text "extra"))
               (lambda (_) (error "bad section")))))
    (let ((vnodes (beads-molecule--extra-vnodes beads-molecule-cov--model)))
      (should (= 2 (length vnodes)))))
  (should (equal (beads-molecule--error-string "plain") "plain"))
  (should (equal (beads-molecule--error-string '("first" "second")) "first"))
  (should (stringp (beads-molecule--error-string '(1 2 3)))))

;;; Async loaders

(ert-deftest beads-molecule-cov-load-gates-test ()
  "Unit test: gate loading short-circuits an empty gate list."
  :tags '(:unit)
  (let ((resolved 'unset))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (_cmd ok _err &rest _) (funcall ok nil))))
      (beads-molecule--load-gates
       "mol-1"
       (lambda (gates) (setq resolved gates))
       (lambda (_) (setq resolved 'rejected))))
    (should-not resolved)))

(ert-deftest beads-molecule-cov-load-test ()
  "Unit test: the aggregate loader resolves once all four settle."
  :tags '(:unit)
  (let ((result 'unset))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (_cmd ok _err &rest _) (funcall ok nil)))
              ((symbol-function 'beads-molecule--load-gates)
               (lambda (_root ok _err) (funcall ok nil))))
      (beads-molecule--load "mol-1" nil
                            (lambda (state) (setq result state))
                            (lambda (err) (setq result err))))
    (should (listp result))
    (should (plist-member result :current))))

(ert-deftest beads-molecule-cov-load-rejects-on-current-test ()
  "Unit test: a mol-current failure rejects the whole load."
  :tags '(:unit)
  (let ((result 'unset))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd ok err &rest _)
                 (if (beads-command-mol-current-p cmd)
                     (funcall err 'boom)
                   (funcall ok nil))))
              ((symbol-function 'beads-molecule--load-gates)
               (lambda (_root ok _err) (funcall ok nil))))
      (beads-molecule--load "mol-1" nil
                            (lambda (state) (setq result state))
                            (lambda (err) (setq result err))))
    (should (eq result 'boom))))

;;; Commands

(ert-deftest beads-molecule-cov-action-test ()
  "Unit test: the default action dispatches inspect or errors."
  :tags '(:unit)
  (let ((shown nil))
    (cl-letf (((symbol-function 'beads-show) (lambda (id) (setq shown id))))
      (beads-molecule-action 'inspect '(:id "mol-1.a"))
      (should (equal shown "mol-1.a"))
      (should-error (beads-molecule-action 'bogus '(:id "x")) :type 'user-error))))

(ert-deftest beads-molecule-cov-inspect-test ()
  "Unit test: inspect requires a step at point."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-molecule--step-at-point) (lambda () nil)))
    (should-error (beads-molecule-inspect) :type 'user-error))
  (let ((acted nil))
    (cl-letf (((symbol-function 'beads-molecule--step-at-point)
               (lambda () '(:id "mol-1.a")))
              ((symbol-function 'beads-molecule-action)
               (lambda (action step) (setq acted (list action step)))))
      (beads-molecule-inspect)
      (should (equal (car acted) 'inspect)))))

(ert-deftest beads-molecule-cov-bump-commands-test ()
  "Unit test: refresh/toggle/range commands bump the right root state."
  :tags '(:unit)
  (let (calls)
    (cl-letf (((symbol-function 'beads-molecule--root-state)
               (lambda (key) (pcase key (:generation 1) (:ready-only nil)
                                    (:range '(1 . 10)))))
              ((symbol-function 'beads-molecule--bump)
               (lambda (key value) (push (cons key value) calls))))
      (beads-molecule-refresh)
      (beads-molecule-toggle-ready-only)
      (beads-molecule-next-range)
      (beads-molecule-previous-range)
      (should (equal (assq :generation calls) '(:generation . 2)))
      (should (equal (assq :ready-only calls) '(:ready-only . t)))
      (should (assq :range calls)))))

(ert-deftest beads-molecule-cov-next-claim-test ()
  "Unit test: next/claim validate the frontier and act."
  :tags '(:unit)
  (with-temp-buffer
    (cl-letf (((symbol-function 'beads-molecule--ready-steps) (lambda () nil)))
      (should-error (beads-molecule-next) :type 'user-error)))
  (with-temp-buffer
    (cl-letf (((symbol-function 'beads-molecule--ready-steps)
               (lambda () '((:id "mol-1.b"))))
              ((symbol-function 'beads-thing-at) (lambda () nil))
              ((symbol-function 'beads-molecule--goto-step) (lambda (_) 'moved)))
      (should (eq (beads-molecule-next) 'moved))))
  (cl-letf (((symbol-function 'beads-molecule--step-at-point) (lambda () nil)))
    (should-error (beads-molecule-claim) :type 'user-error))
  (let ((acted nil))
    (cl-letf (((symbol-function 'beads-molecule--step-at-point)
               (lambda () '(:id "mol-1.b")))
              ((symbol-function 'beads-molecule-action)
               (lambda (action step) (setq acted (list action step))))
              ((symbol-function 'beads-molecule-refresh) #'ignore)
              ((symbol-function 'beads-molecule-next) #'ignore))
      (beads-molecule-claim)
      (should (equal (car acted) 'claim)))))

(ert-deftest beads-molecule-cov-close-test ()
  "Unit test: close requires a step and a non-empty reason."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-molecule--step-at-point) (lambda () nil)))
    (should-error (beads-molecule-close) :type 'user-error))
  (cl-letf (((symbol-function 'beads-molecule--step-at-point)
             (lambda () '(:id "mol-1.b")))
            ((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (should-error (beads-molecule-close) :type 'user-error))
  (let ((acted nil))
    (cl-letf (((symbol-function 'beads-molecule--step-at-point)
               (lambda () '(:id "mol-1.b")))
              ((symbol-function 'read-string) (lambda (&rest _) "done"))
              ((symbol-function 'beads-molecule-action)
               (lambda (action step reason) (setq acted (list action step reason))))
              ((symbol-function 'beads-molecule-refresh) #'ignore)
              ((symbol-function 'beads-molecule-next) #'ignore))
      (beads-molecule-close)
      (should (equal (car acted) 'close))
      (should (equal (nth 2 acted) "done")))))

(ert-deftest beads-molecule-cov-close-eligible-test ()
  "Unit test: the epic sweep previews then applies on confirmation."
  :tags '(:unit)
  (let ((executed nil) (refreshed 0))
    ;; Empty preview: nothing happens.
    (cl-letf (((symbol-function 'beads-execute) (lambda (&rest _) nil))
              ((symbol-function 'beads-molecule-refresh)
               (lambda () (setq refreshed (1+ refreshed)))))
      (beads-molecule-close-eligible)
      (should (= refreshed 0)))
    ;; Non-empty preview, declined: nothing applied.
    (cl-letf (((symbol-function 'beads-execute)
               (lambda (cmd &rest _) (push cmd executed) "pending"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'beads-molecule-refresh)
               (lambda () (setq refreshed (1+ refreshed)))))
      (beads-molecule-close-eligible)
      (should (= refreshed 0)))
    ;; Non-empty preview, confirmed: applies and refreshes.
    (cl-letf (((symbol-function 'beads-execute)
               (lambda (cmd &rest _) (push cmd executed) "pending"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'beads-molecule-refresh)
               (lambda () (setq refreshed (1+ refreshed)))))
      (beads-molecule-close-eligible)
      (should (= refreshed 1)))))

(ert-deftest beads-molecule-cov-tool-commands-test ()
  "Unit test: handoff/bond/gate delegate when available, else error."
  :tags '(:unit)
  (let ((called nil))
    (cl-letf (((symbol-function 'beads-handoff-agent)
               (lambda () (interactive) (setq called 'handoff)))
              ((symbol-function 'call-interactively)
               (lambda (fn &rest _) (funcall fn))))
      (beads-molecule-handoff)
      (should (eq called 'handoff))))
  (let ((called nil))
    (cl-letf (((symbol-function 'beads-bond)
               (lambda () (interactive) (setq called 'bond)))
              ((symbol-function 'call-interactively)
               (lambda (fn &rest _) (funcall fn))))
      (beads-molecule-bond)
      (should (eq called 'bond))))
  (let ((called nil))
    (cl-letf (((symbol-function 'beads-gate-list)
               (lambda () (interactive) (setq called 'gate)))
              ((symbol-function 'beads-gate) nil)
              ((symbol-function 'call-interactively)
               (lambda (fn &rest _) (funcall fn))))
      (beads-molecule-gate)
      (should (eq called 'gate)))))

;;; Naming and entry point

(ert-deftest beads-molecule-cov-buffer-name-for-test ()
  "Unit test: buffer names carry the molecule id and store/project."
  :tags '(:unit)
  (let ((name (beads-molecule--buffer-name-for "mol-1" "/tmp/covstore")))
    (should (string-match-p "mol-1" name)))
  (cl-letf (((symbol-function 'beads--project-name) (lambda () "covproj")))
    (let ((name (beads-molecule--buffer-name-for "mol-1" nil)))
      (should (string-match-p "mol-1" name)))))

(ert-deftest beads-molecule-cov-entry-point-test ()
  "Unit test: `beads-molecule' delegates to `beads-molecule-open'."
  :tags '(:unit)
  (let ((called nil))
    (cl-letf (((symbol-function 'beads-molecule-open)
               (lambda (root dir) (setq called (list root dir)) 'buffer)))
      (should (eq (beads-molecule "mol-1" "/tmp/x") 'buffer))
      (should (equal called '("mol-1" "/tmp/x"))))))

(provide 'beads-molecule-coverage-test)
;;; beads-molecule-coverage-test.el ends here
