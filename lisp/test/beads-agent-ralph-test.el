;;; beads-agent-ralph-test.el --- Tests for beads-agent-ralph -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the Ralph loop controller module.  This file is the
;; shared home for both prompt-construction tests (bde-aqu1) and the
;; controller state-machine tests (bde-t9is); the latter task adds
;; coverage of the iteration sequence and detectors.

;;; Code:

(require 'ert)
(require 'beads-agent-ralph)
(require 'beads-types)

;;; Test Fixtures

(defun beads-agent-ralph-test--make-issue (&rest args)
  "Create a `beads-issue' for tests with sensible defaults overridable via ARGS."
  (apply #'beads-issue
         (append args
                 (list :id (or (plist-get args :id) "bde-aaa")
                       :title (or (plist-get args :title) "Sample issue")
                       :description (or (plist-get args :description)
                                        "Sample description")
                       :status (or (plist-get args :status) "in_progress")
                       :acceptance-criteria
                       (or (plist-get args :acceptance-criteria)
                           "- [ ] does X")))))

(defun beads-agent-ralph-test--make-controller (&rest args)
  "Create a `beads-agent-ralph--controller' for tests."
  (apply #'beads-agent-ralph--controller
         (append args
                 (list :root-id (or (plist-get args :root-id) "bde-root")
                       :root-kind (or (plist-get args :root-kind) 'issue)
                       :iteration (or (plist-get args :iteration) 3)
                       :max-iterations (or (plist-get args :max-iterations) 50)
                       :current-issue-id (or (plist-get args :current-issue-id)
                                             "bde-aaa")))))

(defun beads-agent-ralph-test--iteration (issue-id summary)
  "Return a `--iteration' record with ISSUE-ID and SUMMARY."
  (beads-agent-ralph--iteration
   :issue-id issue-id
   :status 'finished
   :summary summary))

;;; Substitution Tests

(ert-deftest beads-agent-ralph-test-apply-substitutions-basic ()
  "Single placeholder is replaced with its alist value."
  (should (equal (beads-agent-ralph--apply-substitutions
                  "Hello <ISSUE-ID>!"
                  '(("ISSUE-ID" . "bde-xyz")))
                 "Hello bde-xyz!")))

(ert-deftest beads-agent-ralph-test-apply-substitutions-literal ()
  "Replacement is literal (no regexp back-reference interpretation)."
  (should (equal (beads-agent-ralph--apply-substitutions
                  "x=<ISSUE-DESCRIPTION>"
                  '(("ISSUE-DESCRIPTION" . "uses \\1 backrefs")))
                 "x=uses \\1 backrefs")))

(ert-deftest beads-agent-ralph-test-apply-substitutions-single-pass ()
  "Substituted values are not re-expanded."
  (should (equal (beads-agent-ralph--apply-substitutions
                  "<ISSUE-DESCRIPTION>"
                  '(("ISSUE-DESCRIPTION" . "see <ISSUE-ID>")
                    ("ISSUE-ID" . "should-not-appear")))
                 "see <ISSUE-ID>")))

(ert-deftest beads-agent-ralph-test-apply-substitutions-unknown-tag ()
  "Tags outside the recognised set survive verbatim."
  (should (equal (beads-agent-ralph--apply-substitutions
                  "no <BOGUS> here"
                  '(("ISSUE-ID" . "bde-1")))
                 "no <BOGUS> here")))

(ert-deftest beads-agent-ralph-test-apply-substitutions-known-but-absent ()
  "Recognised tags absent from the alist collapse to the empty string."
  (should (equal (beads-agent-ralph--apply-substitutions
                  "[<ROOT-ID>]"
                  '(("ISSUE-ID" . "bde-1")))
                 "[]")))

;;; Tail/verify helpers

(ert-deftest beads-agent-ralph-test-tail-text-short ()
  "Short text is returned unchanged."
  (should (equal (beads-agent-ralph--tail-text "abc" 100) "abc")))

(ert-deftest beads-agent-ralph-test-tail-text-truncates ()
  "Long text is truncated and prefixed with the marker."
  (let ((res (beads-agent-ralph--tail-text "0123456789" 4)))
    (should (string-match-p "\\`\\.\\.\\." res))
    (should (string-suffix-p "6789" res))))

(ert-deftest beads-agent-ralph-test-tail-text-nil ()
  "Nil text returns empty string."
  (should (equal (beads-agent-ralph--tail-text nil 10) "")))

(ert-deftest beads-agent-ralph-test-render-verify-tail-nil ()
  "Nil verify result renders empty so first-iter prompt is clean."
  (should (equal (beads-agent-ralph--render-verify-tail nil) "")))

(ert-deftest beads-agent-ralph-test-render-verify-tail-non-nil ()
  "Non-nil verify result includes exit + tails."
  (let ((rendered (beads-agent-ralph--render-verify-tail
                   (list :exit 1 :stdout "ok" :stderr "boom"))))
    (should (string-match-p "exit: 1" rendered))
    (should (string-match-p "stdout:" rendered))
    (should (string-match-p "ok" rendered))
    (should (string-match-p "boom" rendered))))

;;; Prior-claim/stall helpers

(ert-deftest beads-agent-ralph-test-prior-false-claims-zero ()
  "Zero false-claim count renders the empty string."
  (let ((c (beads-agent-ralph-test--make-controller :false-claim-count 0)))
    (should (equal (beads-agent-ralph--render-prior-false-claims c) ""))))

(ert-deftest beads-agent-ralph-test-prior-false-claims-nonzero ()
  "Non-zero false-claim count renders a note that mentions the root."
  (let* ((c (beads-agent-ralph-test--make-controller
             :root-id "bde-42" :false-claim-count 3))
         (out (beads-agent-ralph--render-prior-false-claims c)))
    (should (string-match-p "PRIOR FALSE CLAIMS" out))
    (should (string-match-p "3 times" out))
    (should (string-match-p "bde-42" out))))

(ert-deftest beads-agent-ralph-test-prior-stall-count-zero ()
  "Zero stall count renders the empty string."
  (let ((c (beads-agent-ralph-test--make-controller :consecutive-stalls 0)))
    (should (equal (beads-agent-ralph--render-prior-stall-count c) ""))))

(ert-deftest beads-agent-ralph-test-prior-stall-count-nonzero ()
  "Non-zero stall count surfaces the count."
  (let* ((c (beads-agent-ralph-test--make-controller :consecutive-stalls 2))
         (out (beads-agent-ralph--render-prior-stall-count c)))
    (should (string-match-p "PRIOR STALL COUNT" out))
    (should (string-match-p "2 consecutive" out))))

;;; Iteration-number lookup

(ert-deftest beads-agent-ralph-test-iter-number-for-issue-missing ()
  "An issue absent from history returns nil."
  (let ((c (beads-agent-ralph-test--make-controller :history nil)))
    (should (null (beads-agent-ralph--iteration-number-for-issue c "bde-x")))))

(ert-deftest beads-agent-ralph-test-iter-number-for-issue-newest-wins ()
  "Most recent history entry (head of list) maps to the highest iter number."
  (let* ((history (list
                   (beads-agent-ralph-test--iteration "bde-a" "iter3")
                   (beads-agent-ralph-test--iteration "bde-b" "iter2")
                   (beads-agent-ralph-test--iteration "bde-a" "iter1")))
         (c (beads-agent-ralph-test--make-controller :history history)))
    (should (= (beads-agent-ralph--iteration-number-for-issue c "bde-a") 3))
    (should (= (beads-agent-ralph--iteration-number-for-issue c "bde-b") 2))))

;;; Plan view rendering

(ert-deftest beads-agent-ralph-test-plan-view-empty ()
  "Empty plan-issues renders a placeholder mentioning current target."
  (let ((c (beads-agent-ralph-test--make-controller :current-issue-id "bde-9")))
    (should (string-match-p
             "bde-9"
             (beads-agent-ralph--render-plan-view c nil "bde-9")))))

(ert-deftest beads-agent-ralph-test-plan-view-checkbox-closed ()
  "Closed rows render with [x]."
  (let* ((issue (beads-agent-ralph-test--make-issue
                 :id "bde-1" :title "Foo" :status "closed"))
         (c (beads-agent-ralph-test--make-controller))
         (out (beads-agent-ralph--render-plan-view c (list issue) "bde-2")))
    (should (string-match-p "\\`- \\[x\\] bde-1 Foo (closed)" out))))

(ert-deftest beads-agent-ralph-test-plan-view-checkbox-open ()
  "Open rows render with [ ]."
  (let* ((issue (beads-agent-ralph-test--make-issue
                 :id "bde-1" :title "Foo" :status "open"))
         (c (beads-agent-ralph-test--make-controller))
         (out (beads-agent-ralph--render-plan-view c (list issue) "bde-2")))
    (should (string-match-p "\\`- \\[ \\] bde-1 Foo (open)" out))))

(ert-deftest beads-agent-ralph-test-plan-view-current-marker ()
  "Current target row gets the arrow annotation."
  (let* ((issue (beads-agent-ralph-test--make-issue
                 :id "bde-7" :title "Now" :status "in_progress"))
         (c (beads-agent-ralph-test--make-controller))
         (out (beads-agent-ralph--render-plan-view c (list issue) "bde-7")))
    (should (string-match-p "<- current" out))))

(ert-deftest beads-agent-ralph-test-plan-view-last-touched ()
  "Rows with history get an annotation citing the iter and summary."
  (let* ((issue (beads-agent-ralph-test--make-issue
                 :id "bde-1" :title "Bar" :status "in_progress"))
         (history (list (beads-agent-ralph-test--iteration
                         "bde-1" "did the thing")))
         (c (beads-agent-ralph-test--make-controller :history history))
         (out (beads-agent-ralph--render-plan-view c (list issue) "bde-2")))
    (should (string-match-p "last touched by iter 1" out))
    (should (string-match-p "did the thing" out))))

(ert-deftest beads-agent-ralph-test-plan-view-blocked-by ()
  "Rows whose deps are open in plan-issues get a blocked-by annotation."
  (let* ((blocker (beads-agent-ralph-test--make-issue
                   :id "bde-1" :title "Blocker" :status "open"))
         (dep (beads-dependency :depends-on-id "bde-1" :type "blocks"))
         (blocked (beads-agent-ralph-test--make-issue
                   :id "bde-2" :title "Blocked"
                   :status "open" :dependencies (list dep)))
         (c (beads-agent-ralph-test--make-controller))
         (out (beads-agent-ralph--render-plan-view
               c (list blocker blocked) "bde-9")))
    (should (string-match-p "blocked by bde-1" out))))

(ert-deftest beads-agent-ralph-test-plan-view-blocked-suppressed-when-closed ()
  "Closed-dep rows do NOT add a blocked-by annotation."
  (let* ((blocker (beads-agent-ralph-test--make-issue
                   :id "bde-1" :title "Done" :status "closed"))
         (dep (beads-dependency :depends-on-id "bde-1" :type "blocks"))
         (blocked (beads-agent-ralph-test--make-issue
                   :id "bde-2" :title "Free"
                   :status "open" :dependencies (list dep)))
         (c (beads-agent-ralph-test--make-controller))
         (out (beads-agent-ralph--render-plan-view
               c (list blocker blocked) "bde-9")))
    (should-not (string-match-p "blocked by" out))))

;;; Effective template resolution

(ert-deftest beads-agent-ralph-test-effective-template-slot-wins ()
  "Slot template overrides defcustom and prompt-file."
  (let ((beads-agent-ralph-prompt "FROM DEFCUSTOM")
        (beads-agent-ralph-prompt-file nil)
        (c (beads-agent-ralph-test--make-controller
            :prompt-template "FROM SLOT")))
    (should (equal (beads-agent-ralph--effective-template c) "FROM SLOT"))))

(ert-deftest beads-agent-ralph-test-effective-template-defcustom-default ()
  "Falls back to defcustom when slot and file are nil."
  (let ((beads-agent-ralph-prompt "FROM DEFCUSTOM")
        (beads-agent-ralph-prompt-file nil)
        (c (beads-agent-ralph-test--make-controller :prompt-template nil)))
    (should (equal (beads-agent-ralph--effective-template c)
                   "FROM DEFCUSTOM"))))

(ert-deftest beads-agent-ralph-test-effective-template-from-file ()
  "When prompt-file is readable, its contents win over defcustom."
  (let* ((tmp (make-temp-file "ralph-prompt"))
         (beads-agent-ralph-prompt "FROM DEFCUSTOM")
         (beads-agent-ralph-prompt-file tmp)
         (c (beads-agent-ralph-test--make-controller :prompt-template nil)))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "FROM FILE"))
          (should (equal (beads-agent-ralph--effective-template c)
                         "FROM FILE")))
      (delete-file tmp))))

(ert-deftest beads-agent-ralph-test-effective-template-file-missing ()
  "Configured prompt-file that doesn't exist falls through to defcustom."
  (let ((beads-agent-ralph-prompt "FROM DEFCUSTOM")
        (beads-agent-ralph-prompt-file "/definitely/not/here.md")
        (c (beads-agent-ralph-test--make-controller :prompt-template nil)))
    (should (equal (beads-agent-ralph--effective-template c)
                   "FROM DEFCUSTOM"))))

(ert-deftest beads-agent-ralph-test-effective-template-file-root-substituted ()
  "<ROOT-ID> in prompt-file path is replaced with the controller's root."
  (let* ((dir (make-temp-file "ralph-prompt-dir" t))
         (target (expand-file-name "bde-zzz.prompt.md" dir))
         (beads-agent-ralph-prompt "FROM DEFCUSTOM")
         (beads-agent-ralph-prompt-file
          (expand-file-name "<ROOT-ID>.prompt.md" dir))
         (c (beads-agent-ralph-test--make-controller
             :root-id "bde-zzz" :prompt-template nil)))
    (unwind-protect
        (progn
          (with-temp-file target (insert "PER-ROOT"))
          (should (equal (beads-agent-ralph--effective-template c)
                         "PER-ROOT")))
      (delete-directory dir t))))

;;; End-to-end render-prompt

(ert-deftest beads-agent-ralph-test-render-prompt-substitutes-basics ()
  "End-to-end render injects issue + controller fields."
  (let* ((issue (beads-agent-ralph-test--make-issue
                 :id "bde-x" :title "Tee" :description "Desc"
                 :acceptance-criteria "AC"))
         (c (beads-agent-ralph-test--make-controller
             :prompt-template "[<ISSUE-ID>] <ISSUE-TITLE>
desc:<ISSUE-DESCRIPTION>
ac:<ACCEPTANCE>
root:<ROOT-ID> iter:<ITERATION>/<MAX-ITERATIONS>
sentinel:<SENTINEL>"
             :root-id "bde-root" :iteration 4 :max-iterations 10))
         (out (beads-agent-ralph--render-prompt c issue nil)))
    (should (string-match-p "\\[bde-x\\] Tee" out))
    (should (string-match-p "desc:Desc" out))
    (should (string-match-p "ac:AC" out))
    (should (string-match-p "root:bde-root iter:4/10" out))
    (should (string-match-p "sentinel:<promise>COMPLETE</promise>" out))))

(ert-deftest beads-agent-ralph-test-render-prompt-nil-issue ()
  "Render with a nil issue collapses issue fields without erroring."
  (let* ((c (beads-agent-ralph-test--make-controller
             :prompt-template "<ISSUE-TITLE>|<ACCEPTANCE>"))
         (out (beads-agent-ralph--render-prompt c nil nil)))
    (should (equal out "|"))))

(ert-deftest beads-agent-ralph-test-render-prompt-injects-plan-view ()
  "PLAN-VIEW placeholder is replaced with a rendered child list."
  (let* ((kid (beads-agent-ralph-test--make-issue
               :id "bde-kid" :title "Kid" :status "open"))
         (c (beads-agent-ralph-test--make-controller
             :prompt-template "PLAN:\n<PLAN-VIEW>"))
         (out (beads-agent-ralph--render-prompt c nil (list kid))))
    (should (string-match-p "bde-kid Kid" out))))

(ert-deftest beads-agent-ralph-test-render-prompt-injects-verify-and-diff ()
  "Both VERIFY-TAIL and GIT-DIFF-STAT placeholders are honoured."
  (let* ((c (beads-agent-ralph-test--make-controller
             :prompt-template "v:<VERIFY-TAIL>\nd:<GIT-DIFF-STAT>"
             :last-verify '(:exit 0 :stdout "ok" :stderr "")
             :last-git-diff-stat " lisp/foo.el | 4 +-"))
         (out (beads-agent-ralph--render-prompt c nil nil)))
    (should (string-match-p "exit: 0" out))
    (should (string-match-p "lisp/foo\\.el" out))))

(ert-deftest beads-agent-ralph-test-render-prompt-defcustom-default ()
  "When no slot template is set the rendered prompt comes from the defcustom."
  (let* ((beads-agent-ralph-prompt
          "DEFAULT <ISSUE-ID> <ROOT-ID> <ITERATION>")
         (beads-agent-ralph-prompt-file nil)
         (issue (beads-agent-ralph-test--make-issue :id "bde-x"))
         (c (beads-agent-ralph-test--make-controller
             :prompt-template nil
             :root-id "bde-r" :iteration 2))
         (out (beads-agent-ralph--render-prompt c issue nil)))
    (should (equal out "DEFAULT bde-x bde-r 2"))))

(ert-deftest beads-agent-ralph-test-default-prompt-mentions-key-hooks ()
  "The default prompt template names the bd commands and sentinel keys.
Regression guard: if someone trims the default, the agent loses
the (a)-(g) instructions on the parent issue."
  (let ((tmpl beads-agent-ralph-prompt))
    (should (string-match-p "bd show <ROOT-ID>" tmpl))
    (should (string-match-p "bd update <ISSUE-ID>" tmpl))
    (should (string-match-p "bd close <ISSUE-ID>" tmpl))
    (should (string-match-p "<summary>" tmpl))
    (should (string-match-p "<SENTINEL>" tmpl))
    (should (string-match-p "<PLAN-VIEW>" tmpl))))

(provide 'beads-agent-ralph-test)

;;; beads-agent-ralph-test.el ends here
