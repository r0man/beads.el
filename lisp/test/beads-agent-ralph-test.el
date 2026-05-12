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

;; `beads-agent-use-worktrees' is defined in `beads-agent.el' (not
;; loaded here).  Tell the byte-compiler the symbol is dynamic so the
;; `let' bindings below take effect even when only the forward declaration
;; in `beads-agent-ralph.el' is in scope.
(defvar beads-agent-use-worktrees)

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

;;; Controller state-machine tests (bde-t9is)
;;
;; The tests below cover the detectors, banner ring, derived-iteration
;; builder, async wrappers (via cl-letf stubs), and the controller's
;; status-mutation primitives.  They never spawn a real claude or hit
;; bd: every external surface is rebound under cl-letf for the duration
;; of one test.  Where the controller uses `run-at-time' to break the
;; call stack between iterations, the test pumps the timer queue with
;; `accept-process-output' (with a tiny timeout) so the deferred body
;; runs synchronously enough to assert against.

(defun beads-agent-ralph-test--fake-stream (&rest args)
  "Return a `beads-agent-ralph--stream' populated for tests.
ARGS overrides slots; missing slots take sensible defaults so the
extract-* helpers can run."
  (apply #'beads-agent-ralph--stream
         (append args
                 (list :events (or (plist-get args :events) nil)
                       :stderr-tail (or (plist-get args :stderr-tail) nil)
                       :status (or (plist-get args :status) 'finished)))))

(defun beads-agent-ralph-test--assistant-event (&rest blocks)
  "Build an `assistant' NDJSON event plist with content BLOCKS."
  (list :type "assistant"
        :message (list :content blocks)))

(defun beads-agent-ralph-test--text-block (text)
  "Return an assistant `text' content block with TEXT."
  (list :type "text" :text text))

(defun beads-agent-ralph-test--tool-use-block (name input)
  "Return an assistant `tool_use' content block with NAME and INPUT plist."
  (list :type "tool_use" :name name :input input))

(defun beads-agent-ralph-test--result-event (&rest fields)
  "Build a `result' NDJSON event plist with FIELDS merged in."
  (apply #'list :type "result" fields))

;;; Stalled / lying-agent detectors

(ert-deftest beads-agent-ralph-test-stalled-when-everything-quiet ()
  "Iter with no bd updates, no files, and no sentinel is stalled."
  (let ((iter (beads-agent-ralph--iteration
               :bd-updates-count 0 :files-touched nil)))
    (should (beads-agent-ralph--iteration-stalled-p iter nil))))

(ert-deftest beads-agent-ralph-test-stalled-cleared-by-bd-update ()
  "A bd-mutating tool call defeats stall."
  (let ((iter (beads-agent-ralph--iteration
               :bd-updates-count 1 :files-touched nil)))
    (should-not (beads-agent-ralph--iteration-stalled-p iter nil))))

(ert-deftest beads-agent-ralph-test-stalled-cleared-by-files-touched ()
  "A file edit defeats stall."
  (let ((iter (beads-agent-ralph--iteration
               :bd-updates-count 0 :files-touched '("lisp/x.el"))))
    (should-not (beads-agent-ralph--iteration-stalled-p iter nil))))

(ert-deftest beads-agent-ralph-test-stalled-cleared-by-sentinel ()
  "Sentinel hit defeats stall even with no other activity."
  (let ((iter (beads-agent-ralph--iteration
               :bd-updates-count 0 :files-touched nil)))
    (should-not (beads-agent-ralph--iteration-stalled-p iter t))))

(ert-deftest beads-agent-ralph-test-lying-agent-sentinel-but-open ()
  "Sentinel hit + non-closed root counts as a false claim."
  (should (beads-agent-ralph--lying-agent-p t "in_progress"))
  (should (beads-agent-ralph--lying-agent-p t "open")))

(ert-deftest beads-agent-ralph-test-lying-agent-honest-when-closed ()
  "Sentinel hit + closed root is honest."
  (should-not (beads-agent-ralph--lying-agent-p t "closed")))

(ert-deftest beads-agent-ralph-test-lying-agent-no-claim ()
  "Without a sentinel hit there's no claim to lie about."
  (should-not (beads-agent-ralph--lying-agent-p nil "in_progress"))
  (should-not (beads-agent-ralph--lying-agent-p nil "closed")))

;;; Budget detectors

(ert-deftest beads-agent-ralph-test-budget-iter-cap-hit ()
  "Reaching max-iterations triggers budget-exhausted."
  (let ((c (beads-agent-ralph-test--make-controller
            :iteration 10 :max-iterations 10)))
    (let ((beads-agent-ralph-max-budget-usd nil))
      (should (beads-agent-ralph--budget-exhausted-p c)))))

(ert-deftest beads-agent-ralph-test-budget-cost-cap-hit ()
  "Cumulative cost at-or-above the total cap triggers budget-exhausted."
  (let ((c (beads-agent-ralph-test--make-controller
            :iteration 1 :max-iterations 50
            :cumulative-cost-usd 5.0)))
    (let ((beads-agent-ralph-max-budget-usd 5.0))
      (should (beads-agent-ralph--budget-exhausted-p c)))))

(ert-deftest beads-agent-ralph-test-budget-not-hit ()
  "Below both caps, budget-exhausted is nil."
  (let ((c (beads-agent-ralph-test--make-controller
            :iteration 1 :max-iterations 50
            :cumulative-cost-usd 1.0)))
    (let ((beads-agent-ralph-max-budget-usd 5.0))
      (should-not (beads-agent-ralph--budget-exhausted-p c)))))

(ert-deftest beads-agent-ralph-test-effective-budget-min-of-per-and-remaining ()
  "When both caps are set the next spawn gets min(per-iter, total - spent)."
  (let ((c (beads-agent-ralph-test--make-controller :cumulative-cost-usd 4.2)))
    (let ((beads-agent-ralph-max-budget-usd-per-iter 2.0)
          (beads-agent-ralph-max-budget-usd 5.0))
      (should (< (abs (- (beads-agent-ralph--effective-per-iter-budget c)
                         0.8))
                 0.0001)))))

(ert-deftest beads-agent-ralph-test-effective-budget-per-only ()
  "With only per-iter set the per-iter value is passed through."
  (let ((c (beads-agent-ralph-test--make-controller :cumulative-cost-usd 4.2)))
    (let ((beads-agent-ralph-max-budget-usd-per-iter 1.5)
          (beads-agent-ralph-max-budget-usd nil))
      (should (= (beads-agent-ralph--effective-per-iter-budget c) 1.5)))))

(ert-deftest beads-agent-ralph-test-effective-budget-total-only ()
  "With only total set the per-iter value is the remaining headroom."
  (let ((c (beads-agent-ralph-test--make-controller :cumulative-cost-usd 3.0)))
    (let ((beads-agent-ralph-max-budget-usd-per-iter nil)
          (beads-agent-ralph-max-budget-usd 5.0))
      (should (= (beads-agent-ralph--effective-per-iter-budget c) 2.0)))))

(ert-deftest beads-agent-ralph-test-effective-budget-none ()
  "With no caps the effective per-iter budget is nil."
  (let ((c (beads-agent-ralph-test--make-controller :cumulative-cost-usd 1.0)))
    (let ((beads-agent-ralph-max-budget-usd-per-iter nil)
          (beads-agent-ralph-max-budget-usd nil))
      (should (null (beads-agent-ralph--effective-per-iter-budget c))))))

(ert-deftest beads-agent-ralph-test-effective-budget-clamp-floor ()
  "When cumulative exceeds total cap the effective value clamps to 0, not negative."
  (let ((c (beads-agent-ralph-test--make-controller :cumulative-cost-usd 5.5)))
    (let ((beads-agent-ralph-max-budget-usd-per-iter 2.0)
          (beads-agent-ralph-max-budget-usd 5.0))
      (should (= (beads-agent-ralph--effective-per-iter-budget c) 0.0)))))

;;; Banner ring

(ert-deftest beads-agent-ralph-test-push-banner-prepends ()
  "Newest banner is at the head of `banner-log'."
  (let ((c (beads-agent-ralph-test--make-controller :banner-log nil)))
    (beads-agent-ralph--push-banner c 'info "first")
    (beads-agent-ralph--push-banner c 'warning "second")
    (should (equal (plist-get (car (oref c banner-log)) :text) "second"))
    (should (equal (plist-get (cadr (oref c banner-log)) :text) "first"))))

(ert-deftest beads-agent-ralph-test-push-banner-trims-to-max ()
  "Banner ring is bounded by `beads-agent-ralph-banner-log-max'."
  (let ((c (beads-agent-ralph-test--make-controller :banner-log nil))
        (beads-agent-ralph-banner-log-max 3))
    (dotimes (i 5)
      (beads-agent-ralph--push-banner c 'info (format "b%d" i)))
    (should (= (length (oref c banner-log)) 3))
    (should (equal (plist-get (car (oref c banner-log)) :text) "b4"))))

;;; Extractors

(ert-deftest beads-agent-ralph-test-events-in-order-reverses-stream ()
  "`events' is stored newest-first; the helper reverses to receive order."
  (let ((s (beads-agent-ralph-test--fake-stream
            :events '((:type "result") (:type "assistant") (:type "system_init")))))
    (should (equal (mapcar (lambda (e) (plist-get e :type))
                           (beads-agent-ralph--events-in-order s))
                   '("system_init" "assistant" "result")))))

(ert-deftest beads-agent-ralph-test-tool-uses-extracts-blocks ()
  "Tool-use blocks across assistant events are collected in order."
  (let* ((a1 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--tool-use-block "Bash" '(:command "ls"))))
         (a2 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--tool-use-block "Edit" '(:file_path "x.el"))))
         (s (beads-agent-ralph-test--fake-stream :events (list a2 a1)))
         (tus (beads-agent-ralph--tool-uses s)))
    (should (= (length tus) 2))
    (should (equal (plist-get (car tus) :name) "Bash"))
    (should (equal (plist-get (cadr tus) :name) "Edit"))))

(ert-deftest beads-agent-ralph-test-tool-uses-ignores-non-assistant ()
  "Tool-use extraction skips events whose type is not `assistant'."
  (let* ((sys (list :type "system_init"
                    :message (list :content
                                   (list (beads-agent-ralph-test--tool-use-block
                                          "Bash" '(:command "echo"))))))
         (s (beads-agent-ralph-test--fake-stream :events (list sys))))
    (should (null (beads-agent-ralph--tool-uses s)))))

(ert-deftest beads-agent-ralph-test-files-touched-from-fs-edit-tools ()
  "Files-touched dedupes paths from Edit/Write/MultiEdit calls."
  (let* ((e1 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--tool-use-block
               "Edit" '(:file_path "lisp/a.el"))))
         (e2 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--tool-use-block
               "Write" '(:file_path "lisp/b.el"))))
         (e3 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--tool-use-block
               "Edit" '(:file_path "lisp/a.el"))))
         (s (beads-agent-ralph-test--fake-stream :events (list e3 e2 e1))))
    (should (equal (beads-agent-ralph--extract-files-touched s)
                   '("lisp/a.el" "lisp/b.el")))))

(ert-deftest beads-agent-ralph-test-files-touched-ignores-bash ()
  "Bash tool calls do not contribute to files-touched."
  (let* ((e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--tool-use-block
              "Bash" '(:command "echo hi"))))
         (s (beads-agent-ralph-test--fake-stream :events (list e))))
    (should (null (beads-agent-ralph--extract-files-touched s)))))

(ert-deftest beads-agent-ralph-test-bd-updates-extracts-id-and-sub ()
  "Bash bd-update commands surface as (id . subcommand) pairs."
  (let* ((e1 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--tool-use-block
               "Bash" '(:command "bd update bde-aaa --notes 'hi'"))))
         (e2 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--tool-use-block
               "Bash" '(:command "bd close bde-bbb"))))
         (s (beads-agent-ralph-test--fake-stream :events (list e2 e1)))
         (upd (beads-agent-ralph--extract-bd-updates s)))
    (should (equal upd '(("bde-aaa" . "update") ("bde-bbb" . "close"))))))

(ert-deftest beads-agent-ralph-test-bd-updates-skips-non-mutating ()
  "bd list / bd show are read-only and not classified as bd updates."
  (let* ((e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--tool-use-block
              "Bash" '(:command "bd show bde-aaa --json"))))
         (s (beads-agent-ralph-test--fake-stream :events (list e))))
    (should (null (beads-agent-ralph--extract-bd-updates s)))))

(ert-deftest beads-agent-ralph-test-tool-call-count ()
  "Tool-call count matches the number of tool_use blocks."
  (let* ((e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--tool-use-block "Bash" '(:command "ls"))
             (beads-agent-ralph-test--tool-use-block "Edit" '(:file_path "x")))))
    (should (= (beads-agent-ralph--extract-tool-call-count
                (beads-agent-ralph-test--fake-stream :events (list e)))
               2))))

(ert-deftest beads-agent-ralph-test-summary-tag-extracted ()
  "The last <summary>...</summary> block wins."
  (let* ((e1 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--text-block "<summary>earlier</summary>")))
         (e2 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--text-block "<summary>later one</summary>")))
         (s (beads-agent-ralph-test--fake-stream :events (list e2 e1))))
    (should (equal (beads-agent-ralph--extract-summary-tag s) "later one"))))

(ert-deftest beads-agent-ralph-test-summary-tag-absent ()
  "No <summary> tag yields nil."
  (let* ((e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--text-block "plain prose"))))
    (should (null (beads-agent-ralph--extract-summary-tag
                   (beads-agent-ralph-test--fake-stream
                    :events (list e)))))))

(ert-deftest beads-agent-ralph-test-sentinel-hit-detection ()
  "The completion sentinel is detected from an assistant text block."
  (let* ((e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--text-block
              "done <promise>COMPLETE</promise>"))))
    (should (beads-agent-ralph--extract-sentinel-hit
             (beads-agent-ralph-test--fake-stream :events (list e))))))

(ert-deftest beads-agent-ralph-test-sentinel-hit-absent ()
  "No sentinel token yields nil."
  (let* ((e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--text-block "still working"))))
    (should-not (beads-agent-ralph--extract-sentinel-hit
                 (beads-agent-ralph-test--fake-stream :events (list e))))))

(ert-deftest beads-agent-ralph-test-last-text-newest-wins ()
  "The last non-empty assistant text block surfaces as `last-text'."
  (let* ((e1 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--text-block "first")))
         (e2 (beads-agent-ralph-test--assistant-event
              (beads-agent-ralph-test--text-block "second")))
         (s (beads-agent-ralph-test--fake-stream :events (list e2 e1))))
    (should (equal (beads-agent-ralph--extract-last-text s) "second"))))

(ert-deftest beads-agent-ralph-test-result-fields-extracted ()
  "Result event populates `:cost-usd' and `:duration-ms'."
  (let* ((r (beads-agent-ralph-test--result-event
             :total_cost_usd 0.5 :duration_ms 1234))
         (s (beads-agent-ralph-test--fake-stream :events (list r)))
         (fields (beads-agent-ralph--extract-result-fields s)))
    (should (equal (plist-get fields :cost-usd) 0.5))
    (should (equal (plist-get fields :duration-ms) 1234))))

(ert-deftest beads-agent-ralph-test-result-fields-fallback-keys ()
  "The result extractor accepts the alternative `:cost_usd' key."
  (let* ((r (beads-agent-ralph-test--result-event :cost_usd 0.25))
         (s (beads-agent-ralph-test--fake-stream :events (list r))))
    (should (= (plist-get (beads-agent-ralph--extract-result-fields s)
                          :cost-usd)
               0.25))))

(ert-deftest beads-agent-ralph-test-result-fields-absent ()
  "Without a result event, both fields stay nil."
  (let ((s (beads-agent-ralph-test--fake-stream :events nil)))
    (should (null (plist-get (beads-agent-ralph--extract-result-fields s)
                             :cost-usd)))
    (should (null (plist-get (beads-agent-ralph--extract-result-fields s)
                             :duration-ms)))))

;;; build-iteration composition

(ert-deftest beads-agent-ralph-test-build-iteration-captures-fields ()
  "`build-iteration' assembles cost, files, summary, sentinel-hit, and status."
  (let* ((c (beads-agent-ralph-test--make-controller
             :current-issue-id "bde-aaa"))
         (e-text (beads-agent-ralph-test--assistant-event
                  (beads-agent-ralph-test--text-block
                   "doing things <promise>COMPLETE</promise>")
                  (beads-agent-ralph-test--text-block
                   "<summary>did the thing</summary>")))
         (e-edit (beads-agent-ralph-test--assistant-event
                  (beads-agent-ralph-test--tool-use-block
                   "Edit" '(:file_path "lisp/foo.el"))))
         (e-result (beads-agent-ralph-test--result-event
                    :total_cost_usd 0.42 :duration_ms 9876))
         (s (beads-agent-ralph-test--fake-stream
             :events (list e-result e-edit e-text)
             :stderr-tail '("warn"))) ;; copied verbatim
         (iter (beads-agent-ralph--build-iteration c s)))
    (should (equal (oref iter issue-id) "bde-aaa"))
    (should (eq (oref iter status) 'finished))
    (should (= (oref iter cost-usd) 0.42))
    (should (= (oref iter duration-ms) 9876))
    (should (equal (oref iter files-touched) '("lisp/foo.el")))
    (should (equal (oref iter summary) "did the thing"))
    (should (oref iter sentinel-hit))
    (should (equal (oref iter stderr-tail) '("warn")))))

(ert-deftest beads-agent-ralph-test-build-iteration-summary-truncates ()
  "Summary longer than `beads-agent-ralph-summary-max-len' is ellided."
  (let* ((c (beads-agent-ralph-test--make-controller))
         (long (make-string 200 ?A))
         (e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--text-block
              (format "<summary>%s</summary>" long))))
         (s (beads-agent-ralph-test--fake-stream
             :events (list e) :status 'finished))
         (beads-agent-ralph-summary-max-len 50)
         (iter (beads-agent-ralph--build-iteration c s)))
    (should (= (length (oref iter summary)) 51)) ;; 50 chars + ellipsis
    (should (string-suffix-p "…" (oref iter summary)))))

(ert-deftest beads-agent-ralph-test-build-iteration-summary-falls-back-to-last-text ()
  "When no <summary> tag is present the last assistant text is used."
  (let* ((c (beads-agent-ralph-test--make-controller))
         (e (beads-agent-ralph-test--assistant-event
             (beads-agent-ralph-test--text-block "plain ending")))
         (s (beads-agent-ralph-test--fake-stream
             :events (list e) :status 'finished))
         (iter (beads-agent-ralph--build-iteration c s)))
    (should (equal (oref iter summary) "plain ending"))))

(ert-deftest beads-agent-ralph-test-build-iteration-stream-status-mapping ()
  "Stream-level status maps to the iteration-level status enum."
  (let ((c (beads-agent-ralph-test--make-controller)))
    (dolist (pair '((finished . finished)
                    (failed   . failed)
                    (stopped  . stopped)
                    (running  . failed))) ;; non-terminal coerces to failed
      (let* ((s (beads-agent-ralph-test--fake-stream
                 :events nil :status (car pair)))
             (iter (beads-agent-ralph--build-iteration c s)))
        (should (eq (oref iter status) (cdr pair)))))))

;;; Coercion

(ert-deftest beads-agent-ralph-test-coerce-single-issue-object ()
  "An object passes through unchanged."
  (let ((issue (beads-agent-ralph-test--make-issue :id "bde-x")))
    (should (eq (beads-agent-ralph--coerce-single-issue issue) issue))))

(ert-deftest beads-agent-ralph-test-coerce-single-issue-singleton-list ()
  "A one-element list unwraps to its issue."
  (let* ((issue (beads-agent-ralph-test--make-issue :id "bde-x"))
         (out (beads-agent-ralph--coerce-single-issue (list issue))))
    (should (eq out issue))))

(ert-deftest beads-agent-ralph-test-coerce-single-issue-multi-list ()
  "A multi-element list returns the head (bd convention)."
  (let* ((a (beads-agent-ralph-test--make-issue :id "bde-a"))
         (b (beads-agent-ralph-test--make-issue :id "bde-b"))
         (out (beads-agent-ralph--coerce-single-issue (list a b))))
    (should (eq out a))))

;;; Status mutations

(ert-deftest beads-agent-ralph-test-set-status-fires-hook ()
  "`set-status' invokes `beads-agent-ralph-state-change-functions'."
  (let* ((c (beads-agent-ralph-test--make-controller :status 'idle))
         (calls nil)
         (beads-agent-ralph-state-change-functions
          (list (lambda (ctrl new) (push (cons (oref ctrl root-id) new) calls)))))
    (beads-agent-ralph--set-status c 'running)
    (should (eq (oref c status) 'running))
    (should (equal calls '(("bde-root" . running))))))

(ert-deftest beads-agent-ralph-test-set-status-idempotent ()
  "Same-status transitions don't fire the hook again."
  (let* ((c (beads-agent-ralph-test--make-controller :status 'running))
         (calls nil)
         (beads-agent-ralph-state-change-functions
          (list (lambda (_ctrl _new) (cl-incf calls 1)))))
    (setq calls 0)
    (beads-agent-ralph--set-status c 'running)
    (should (= calls 0))))

(ert-deftest beads-agent-ralph-test-set-status-hook-error-doesnt-leak ()
  "A signalling hook still lets `set-status' complete."
  (let* ((c (beads-agent-ralph-test--make-controller :status 'idle))
         (beads-agent-ralph-state-change-functions
          (list (lambda (_c _s) (error "intentional")))))
    (beads-agent-ralph--set-status c 'running)
    (should (eq (oref c status) 'running))))

(ert-deftest beads-agent-ralph-test-terminate-maps-reasons ()
  "Each terminate reason maps to its terminal status."
  (dolist (pair '((stop . stopped) (failed . failed) (budget . done)
                  (epic-empty . done) (closed . done) (sentinel . done)))
    (let ((c (beads-agent-ralph-test--make-controller :status 'running)))
      (beads-agent-ralph--terminate c (car pair))
      (should (eq (oref c status) (cdr pair)))
      (should (eq (oref c done-reason) (car pair)))
      (should (null (oref c current-stream))))))

(ert-deftest beads-agent-ralph-test-terminate-no-op-after-terminal ()
  "Terminate is a no-op when the controller already reached a terminal state."
  (let ((c (beads-agent-ralph-test--make-controller
            :status 'done :done-reason 'sentinel)))
    (beads-agent-ralph--terminate c 'failed)
    (should (eq (oref c status) 'done))
    (should (eq (oref c done-reason) 'sentinel))))

(ert-deftest beads-agent-ralph-test-pause-sets-auto-paused-and-banner ()
  "`pause' transitions to auto-paused and pushes a warning banner."
  (let ((c (beads-agent-ralph-test--make-controller
            :status 'running :banner-log nil)))
    (beads-agent-ralph--pause c "stalled out")
    (should (eq (oref c status) 'auto-paused))
    (should (null (oref c current-stream)))
    (let ((latest (car (oref c banner-log))))
      (should (eq (plist-get latest :severity) 'warning))
      (should (equal (plist-get latest :text) "stalled out")))))

;;; continue-after-iteration

(ert-deftest beads-agent-ralph-test-continue-honours-budget-cap ()
  "`continue-after-iteration' terminates when iterations are exhausted."
  (let ((c (beads-agent-ralph-test--make-controller
            :status 'running :iteration 5 :max-iterations 5)))
    (let ((beads-agent-ralph-max-budget-usd nil))
      (beads-agent-ralph--continue-after-iteration c)
      (should (eq (oref c status) 'done))
      (should (eq (oref c done-reason) 'budget)))))

(ert-deftest beads-agent-ralph-test-continue-schedules-next-iteration ()
  "`continue-after-iteration' transitions to cooling-down and schedules a run."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :iteration 1 :max-iterations 50
             :iteration-delay 0.0))
         (calls 0))
    (cl-letf (((symbol-function 'beads-agent-ralph--run-iteration)
               (lambda (_c) (cl-incf calls))))
      (let ((beads-agent-ralph-max-budget-usd nil))
        (beads-agent-ralph--continue-after-iteration c))
      ;; The schedule fires asynchronously; pump the timer queue.
      (with-timeout (1.0) (while (zerop calls) (sit-for 0.01)))
      (should (= calls 1))
      ;; Schedule increments the iteration counter before the thunk runs.
      (should (= (oref c iteration) 2)))))

;;; schedule-next-iteration

(ert-deftest beads-agent-ralph-test-schedule-increments-iteration ()
  "The scheduler increments `iteration' before invoking THUNK."
  (let* ((c (beads-agent-ralph-test--make-controller
             :iteration 4 :iteration-delay 0.0))
         (observed nil))
    (beads-agent-ralph--schedule-next-iteration
     c (lambda () (setq observed (oref c iteration))))
    (with-timeout (1.0) (while (null observed) (sit-for 0.01)))
    (should (= observed 5))))

;;; Async chain helper

(ert-deftest beads-agent-ralph-test-then-runs-all-steps-in-order ()
  "Each step receives the accumulated plist; results accumulate left to right."
  (let* ((trace nil))
    (beads-agent-ralph--then
     :acc (list :i 0)
     :steps
     (list
      (lambda (acc k)
        (push (cons 'a (plist-get acc :i)) trace)
        (funcall k nil (list :seen 'a)))
      (lambda (acc k)
        (push (cons 'b (plist-get acc :seen)) trace)
        (funcall k nil nil))))
    (should (equal (reverse trace) '((a . 0) (b . a))))))

(ert-deftest beads-agent-ralph-test-then-short-circuits-on-error ()
  "An error in one step skips remaining steps and runs on-error with the index."
  (let* ((second-called nil)
         (err-args nil))
    (beads-agent-ralph--then
     :steps
     (list
      (lambda (_acc k) (funcall k nil nil))
      (lambda (_acc k) (funcall k 'boom nil))
      (lambda (_acc _k) (setq second-called t)))
     :on-error (lambda (err idx) (setq err-args (list err idx))))
    (should (null second-called))
    (should (equal err-args (list 'boom 1)))))

(ert-deftest beads-agent-ralph-test-then-honours-cancellation ()
  "Cancellation predicate aborts before the next step runs."
  (let* ((calls 0)
         (cancelled t)
         (err-args nil))
    (beads-agent-ralph--then
     :steps
     (list (lambda (_acc _k) (cl-incf calls)))
     :cancelled (lambda () cancelled)
     :on-error (lambda (err idx) (setq err-args (list err idx))))
    (should (= calls 0))
    (should (equal err-args (list 'cancelled 0)))))

;;; Async shell wrapper

(ert-deftest beads-agent-ralph-test-run-shell-async-true ()
  "/bin/true exits 0 with empty stdout/stderr."
  (let ((done nil) (result nil))
    (beads-agent-ralph--run-shell-async
     "true" nil (lambda (r) (setq result r done t)))
    (with-timeout (5.0) (while (not done) (sit-for 0.05)))
    (should (= (plist-get result :exit) 0))
    (should (equal (plist-get result :stdout) ""))
    (should (equal (plist-get result :stderr) ""))))

(ert-deftest beads-agent-ralph-test-run-shell-async-false ()
  "/bin/false exits 1; stderr can be empty."
  (let ((done nil) (result nil))
    (beads-agent-ralph--run-shell-async
     "false" nil (lambda (r) (setq result r done t)))
    (with-timeout (5.0) (while (not done) (sit-for 0.05)))
    (should (= (plist-get result :exit) 1))))

(ert-deftest beads-agent-ralph-test-run-verify-async-nil ()
  "When no verify command is configured the callback receives nil."
  (let ((c (beads-agent-ralph-test--make-controller :verify-command nil))
        (got 'unset))
    (beads-agent-ralph--run-verify-async
     c (lambda (r) (setq got r)))
    (should (null got))))

(ert-deftest beads-agent-ralph-test-run-verify-async-function ()
  "Function-valued verify commands are invoked with the controller."
  (let* ((c (beads-agent-ralph-test--make-controller))
         (got nil))
    (oset c verify-command
          (lambda (_ctrl) (list :exit 0 :stdout "fn-ok" :stderr "")))
    (beads-agent-ralph--run-verify-async
     c (lambda (r) (setq got r)))
    (should (equal (plist-get got :stdout) "fn-ok"))))

(ert-deftest beads-agent-ralph-test-run-verify-async-invalid-value ()
  "Non-string, non-function verify-command returns an :exit -1 plist."
  (let* ((c (beads-agent-ralph-test--make-controller))
         (got nil))
    (oset c verify-command 42)
    (beads-agent-ralph--run-verify-async
     c (lambda (r) (setq got r)))
    (should (= (plist-get got :exit) -1))
    (should (string-match-p "invalid" (plist-get got :stderr)))))

;;; on-stream-finish: detector dispatch

(ert-deftest beads-agent-ralph-test-on-stream-finish-stall-pauses ()
  "Two consecutive stalled iterations transition the controller to auto-paused."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :consecutive-stalls 1
             :verify-command nil :history nil))
         (beads-agent-ralph-stall-threshold 2)
         (stream (beads-agent-ralph-test--fake-stream
                  :events nil :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "open")))))
      (beads-agent-ralph--on-stream-finish c stream))
    (should (eq (oref c status) 'auto-paused))
    (should (= (length (oref c history)) 1))
    (should (eq (oref (car (oref c history)) status) 'finished))
    (should (= (oref c consecutive-stalls) 2))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-closed-terminates ()
  "Single-issue loop terminates `done(closed)' when root closes."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :root-kind 'issue
             :history nil))
         (stream (beads-agent-ralph-test--fake-stream
                  :events (list (beads-agent-ralph-test--assistant-event
                                 (beads-agent-ralph-test--tool-use-block
                                  "Bash" '(:command "bd close bde-root"))))
                  :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "closed")))))
      (beads-agent-ralph--on-stream-finish c stream))
    (should (eq (oref c status) 'done))
    (should (eq (oref c done-reason) 'closed))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-lying-agent-banner ()
  "Sentinel hit + open root increments false-claim-count and banners."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :root-kind 'issue
             :history nil :banner-log nil :false-claim-count 0
             :iteration 1 :max-iterations 10))
         (text-block (beads-agent-ralph-test--text-block
                      "<promise>COMPLETE</promise>"))
         (bd-update-block (beads-agent-ralph-test--tool-use-block
                           "Bash" '(:command "bd update bde-root --notes x")))
         (stream (beads-agent-ralph-test--fake-stream
                  :events (list (beads-agent-ralph-test--assistant-event
                                 text-block bd-update-block))
                  :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               #'ignore))
      (beads-agent-ralph--on-stream-finish c stream))
    (should (= (oref c false-claim-count) 1))
    (should (cl-find-if
             (lambda (entry)
               (string-match-p "Agent claimed complete"
                               (plist-get entry :text)))
             (oref c banner-log)))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-accumulates-cost ()
  "Per-iter cost is added to the cumulative total on completion."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :cumulative-cost-usd 1.0 :history nil))
         (stream (beads-agent-ralph-test--fake-stream
                  :events (list (beads-agent-ralph-test--result-event
                                 :total_cost_usd 0.5))
                  :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               #'ignore))
      (beads-agent-ralph--on-stream-finish c stream))
    (should (= (oref c cumulative-cost-usd) 1.5))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-stall-below-threshold-continues ()
  "A single stall increments the counter but does not auto-pause."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :consecutive-stalls 0 :history nil
             :iteration 1 :max-iterations 50))
         (stream (beads-agent-ralph-test--fake-stream
                  :events nil :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               #'ignore))
      (let ((beads-agent-ralph-stall-threshold 2))
        (beads-agent-ralph--on-stream-finish c stream)))
    (should (= (oref c consecutive-stalls) 1))
    (should (eq (oref c status) 'running))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-resets-stalls-on-activity ()
  "An iter with bd-update activity zeroes `consecutive-stalls'."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :consecutive-stalls 2 :history nil
             :iteration 1 :max-iterations 50))
         (active (beads-agent-ralph-test--assistant-event
                  (beads-agent-ralph-test--tool-use-block
                   "Bash" '(:command "bd update bde-root --notes hi"))))
         (stream (beads-agent-ralph-test--fake-stream
                  :events (list active) :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               #'ignore))
      (beads-agent-ralph--on-stream-finish c stream))
    (should (= (oref c consecutive-stalls) 0))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-failed-no-stop-continues ()
  "stop-on-failed=nil: a failed stream does not terminate the loop."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :history nil
             :iteration 1 :max-iterations 50))
         (stream (beads-agent-ralph-test--fake-stream
                  :events nil :status 'failed))
         (continued nil))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               (lambda (_c) (setq continued t))))
      (let ((beads-agent-ralph-stop-on-failed nil))
        (beads-agent-ralph--on-stream-finish c stream)))
    (should continued)
    (should-not (eq (oref c status) 'failed))
    (should (null (oref c done-reason)))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-failed-stop-terminates ()
  "stop-on-failed=t: a failed stream terminates the loop with reason `failed'."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :history nil
             :iteration 1 :max-iterations 50))
         (stream (beads-agent-ralph-test--fake-stream
                  :events nil :status 'failed)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress")))))
      (let ((beads-agent-ralph-stop-on-failed t))
        (beads-agent-ralph--on-stream-finish c stream)))
    (should (eq (oref c status) 'failed))
    (should (eq (oref c done-reason) 'failed))))

(ert-deftest beads-agent-ralph-test-on-stream-finish-epic-mode-continues ()
  "Epic mode with an open root continues even after a stream finishes."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :root-kind 'epic :history nil
             :iteration 1 :max-iterations 50))
         (stream (beads-agent-ralph-test--fake-stream
                  :events (list
                           (beads-agent-ralph-test--assistant-event
                            (beads-agent-ralph-test--tool-use-block
                             "Bash" '(:command "bd close bde-kid"))))
                  :status 'finished))
         (continued nil))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               (lambda (_c) (setq continued t))))
      (beads-agent-ralph--on-stream-finish c stream))
    (should continued)
    (should (null (oref c done-reason)))))

;;; Cross-iteration prompt threading

(ert-deftest beads-agent-ralph-test-prior-false-claims-flows-into-next-prompt ()
  "After a lying-agent iter the next prompt mentions PRIOR FALSE CLAIMS."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :root-kind 'issue :root-id "bde-root"
             :history nil :banner-log nil :false-claim-count 0
             :iteration 1 :max-iterations 10
             :prompt-template "P:<PRIOR-FALSE-CLAIMS>"))
         (text-block (beads-agent-ralph-test--text-block
                      "<promise>COMPLETE</promise>"))
         (bd-update-block (beads-agent-ralph-test--tool-use-block
                           "Bash" '(:command "bd update bde-root --notes x")))
         (stream (beads-agent-ralph-test--fake-stream
                  :events (list (beads-agent-ralph-test--assistant-event
                                 text-block bd-update-block))
                  :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               #'ignore))
      (beads-agent-ralph--on-stream-finish c stream))
    (let ((rendered (beads-agent-ralph--render-prompt c nil nil)))
      (should (string-match-p "PRIOR FALSE CLAIMS" rendered)))))

(ert-deftest beads-agent-ralph-test-prior-stall-count-flows-into-next-prompt ()
  "After a stalled (but not auto-paused) iter the next prompt mentions stalls."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :consecutive-stalls 0 :history nil
             :iteration 1 :max-iterations 50
             :prompt-template "P:<PRIOR-STALL-COUNT>"))
         (stream (beads-agent-ralph-test--fake-stream
                  :events nil :status 'finished)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (_id k)
                 (funcall k t (beads-agent-ralph-test--make-issue
                               :id "bde-root" :status "in_progress"))))
              ((symbol-function 'beads-agent-ralph--continue-after-iteration)
               #'ignore))
      (let ((beads-agent-ralph-stall-threshold 99))
        (beads-agent-ralph--on-stream-finish c stream)))
    (let ((rendered (beads-agent-ralph--render-prompt c nil nil)))
      (should (string-match-p "PRIOR STALL COUNT" rendered)))))

;;; run-iteration: target resolution

(ert-deftest beads-agent-ralph-test-run-iteration-epic-empty-terminates ()
  "Epic mode with no ready children terminates `done(epic-empty)'."
  (let ((c (beads-agent-ralph-test--make-controller
            :status 'running :root-kind 'epic :history nil)))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-ready-children-async)
               (lambda (_id k) (funcall k t nil))))
      (beads-agent-ralph--run-iteration c))
    (should (eq (oref c status) 'done))
    (should (eq (oref c done-reason) 'epic-empty))))

(ert-deftest beads-agent-ralph-test-run-iteration-issue-mode-uses-root-id ()
  "Issue mode sets `current-issue-id' to the root and spawns a stream."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :root-kind 'issue :root-id "bde-root"
             :prompt-template "X"))
         (spawned nil))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-claim-async)
               (lambda (_id k) (funcall k t nil)))
              ((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (id k)
                 (funcall k t (beads-agent-ralph-test--make-issue :id id))))
              ((symbol-function 'beads-agent-ralph--bd-list-children-async)
               (lambda (_id k) (funcall k t nil)))
              ((symbol-function 'beads-agent-ralph--spawn-stream-for)
               (lambda (_c id _p) (setq spawned id))))
      (beads-agent-ralph--run-iteration c))
    (should (equal spawned "bde-root"))
    (should (equal (oref c current-issue-id) "bde-root"))))

(ert-deftest beads-agent-ralph-test-run-iteration-epic-picks-first-ready-child ()
  "Epic mode resolves `current-issue-id' to the first ready child."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :root-kind 'epic :root-id "bde-epic"
             :prompt-template "X"))
         (kid (beads-agent-ralph-test--make-issue :id "bde-kid1"))
         (kid2 (beads-agent-ralph-test--make-issue :id "bde-kid2"))
         (spawned nil))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-ready-children-async)
               (lambda (_id k) (funcall k t (list kid kid2))))
              ((symbol-function 'beads-agent-ralph--bd-claim-async)
               (lambda (_id k) (funcall k t nil)))
              ((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (id k)
                 (funcall k t (beads-agent-ralph-test--make-issue :id id))))
              ((symbol-function 'beads-agent-ralph--bd-list-children-async)
               (lambda (_id k) (funcall k t nil)))
              ((symbol-function 'beads-agent-ralph--spawn-stream-for)
               (lambda (_c id _p) (setq spawned id))))
      (beads-agent-ralph--run-iteration c))
    (should (equal spawned "bde-kid1"))
    (should (equal (oref c current-issue-id) "bde-kid1"))))

(ert-deftest beads-agent-ralph-test-run-iteration-claim-failure-is-non-fatal ()
  "A failed bd claim is recorded as a banner but does not abort the iter."
  (let* ((c (beads-agent-ralph-test--make-controller
             :status 'running :root-kind 'issue :root-id "bde-root"
             :banner-log nil :prompt-template "X"))
         (spawned nil))
    (cl-letf (((symbol-function 'beads-agent-ralph--bd-claim-async)
               (lambda (_id k) (funcall k nil "claim-failed")))
              ((symbol-function 'beads-agent-ralph--bd-show-async)
               (lambda (id k)
                 (funcall k t (beads-agent-ralph-test--make-issue :id id))))
              ((symbol-function 'beads-agent-ralph--bd-list-children-async)
               (lambda (_id k) (funcall k t nil)))
              ((symbol-function 'beads-agent-ralph--spawn-stream-for)
               (lambda (_c id _p) (setq spawned id))))
      (beads-agent-ralph--run-iteration c))
    (should (equal spawned "bde-root"))
    (should (cl-some (lambda (b)
                       (string-match-p "Claim failed"
                                       (plist-get b :text)))
                     (oref c banner-log)))))

;;; beads-agent-ralph-stop

(ert-deftest beads-agent-ralph-test-stop-without-stream-transitions-to-stopped ()
  "Stop on a controller with no in-flight stream is synchronous."
  (let ((c (beads-agent-ralph-test--make-controller
            :status 'running :current-stream nil)))
    (beads-agent-ralph-stop c)
    (should (eq (oref c status) 'stopped))
    (should (eq (oref c done-reason) 'stop))))

(ert-deftest beads-agent-ralph-test-stop-with-stream-calls-stream-stop ()
  "Stop on an in-flight controller forwards to `stream-stop' without flipping status."
  (let* ((stop-called nil)
         (stream (beads-agent-ralph-test--fake-stream :status 'running))
         (c (beads-agent-ralph-test--make-controller
             :status 'running :current-stream stream)))
    (cl-letf (((symbol-function 'beads-agent-ralph--stream-stop)
               (lambda (s) (should (eq s stream)) (setq stop-called t))))
      (beads-agent-ralph-stop c))
    (should stop-called)
    (should (eq (oref c done-reason) 'stop))))

;;; Protected-paths post-iter check

(defmacro beads-agent-ralph-test--with-git-repo (varname &rest body)
  "Initialize a temp git repo, bind its path to VARNAME, run BODY, cleanup."
  (declare (indent 1) (debug (sexp body)))
  `(let ((,varname (make-temp-file "ralph-protect-" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory ,varname)))
           (call-process "git" nil nil nil "init" "-q")
           (call-process "git" nil nil nil "config" "user.email" "t@e")
           (call-process "git" nil nil nil "config" "user.name" "t")
           (call-process "git" nil nil nil "commit" "--allow-empty"
                         "-q" "-m" "init")
           ,@body)
       (delete-directory ,varname t))))

(ert-deftest beads-agent-ralph-test-protected-paths-empty ()
  "Nothing modified → protected-paths-modified returns nil."
  (beads-agent-ralph-test--with-git-repo dir
    (let ((c (beads-agent-ralph-test--make-controller :project-dir dir))
          (beads-agent-ralph-verify-protect-paths '("test/**")))
      (should (null (beads-agent-ralph--protected-paths-modified c))))))

(ert-deftest beads-agent-ralph-test-protected-paths-detects-touch ()
  "Modifying a protected path surfaces it in the result list."
  (beads-agent-ralph-test--with-git-repo dir
    (let ((c (beads-agent-ralph-test--make-controller :project-dir dir))
          (beads-agent-ralph-verify-protect-paths '("tests/")))
      (make-directory "tests")
      (with-temp-file "tests/example.el" (insert "hi"))
      (call-process "git" nil nil nil "add" "tests/example.el")
      (let ((touched (beads-agent-ralph--protected-paths-modified c)))
        (should touched)
        (should (cl-some (lambda (p) (string-match-p "tests/example\\.el" p))
                         touched))))))

(ert-deftest beads-agent-ralph-test-dirty-state-clean ()
  "Clean tree → git-dirty-state returns nil."
  (beads-agent-ralph-test--with-git-repo dir
    (let ((c (beads-agent-ralph-test--make-controller :project-dir dir)))
      (should (null (beads-agent-ralph--git-dirty-state c))))))

(ert-deftest beads-agent-ralph-test-dirty-state-detects-untracked ()
  "Untracked files surface in the git-dirty-state output."
  (beads-agent-ralph-test--with-git-repo dir
    (let ((c (beads-agent-ralph-test--make-controller :project-dir dir)))
      (with-temp-file (expand-file-name "new.txt" dir) (insert "x"))
      (should (beads-agent-ralph--git-dirty-state c)))))

(ert-deftest beads-agent-ralph-test-dirty-state-banner-notice ()
  "Dirty state pushes a `notice' (not `warning') banner."
  (beads-agent-ralph-test--with-git-repo dir
    (let ((c (beads-agent-ralph-test--make-controller :project-dir dir)))
      (with-temp-file (expand-file-name "new.txt" dir) (insert "x"))
      (beads-agent-ralph--maybe-banner-dirty-state c)
      (let ((banner (car (oref c banner-log))))
        (should banner)
        (should (eq (plist-get banner :severity) 'notice))
        (should (string-match-p "Worktree dirty"
                                (plist-get banner :text)))))))

(ert-deftest beads-agent-ralph-test-protected-paths-banner ()
  "The maybe-banner helper pushes a warning to banner-log on violation."
  (beads-agent-ralph-test--with-git-repo dir
    (let ((c (beads-agent-ralph-test--make-controller :project-dir dir))
          (beads-agent-ralph-verify-protect-paths '("tests/")))
      (make-directory "tests")
      (with-temp-file "tests/example.el" (insert "hi"))
      (call-process "git" nil nil nil "add" "tests/example.el")
      (beads-agent-ralph--maybe-banner-protected-paths c)
      (let ((banner (car (oref c banner-log))))
        (should banner)
        (should (eq (plist-get banner :severity) 'warning))
        (should (string-match-p "Protected paths modified"
                                (plist-get banner :text)))))))

;;; Permission-mode safety guard

(ert-deftest beads-agent-ralph-test-permission-guard-blocks-default-no-worktree ()
  "Default permission-mode in non-worktree dir signals a user-error."
  (let ((beads-agent-ralph-permission-mode "bypassPermissions"))
    (should-error
     (beads-agent-ralph--guard-permission-mode
      "bypassPermissions" nil)
     :type 'user-error)))

(ert-deftest beads-agent-ralph-test-permission-guard-allows-worktree ()
  "Default permission-mode with a worktree-dir is fine (no error)."
  (let ((beads-agent-ralph-permission-mode "bypassPermissions"))
    (should-not
     (condition-case _err
         (progn
           (beads-agent-ralph--guard-permission-mode
            "bypassPermissions" "/tmp/wt/bd-42")
           nil)
       (error t)))))

(ert-deftest beads-agent-ralph-test-permission-guard-allows-custom ()
  "Customized permission-mode bypasses the guard even in non-worktree dir."
  (let ((beads-agent-ralph-permission-mode "ask"))
    (should-not
     (condition-case _err
         (progn
           (beads-agent-ralph--guard-permission-mode "ask" nil)
           nil)
       (error t)))))

(ert-deftest beads-agent-ralph-test-permission-mode-at-default-p ()
  "`--at-default-p' tracks the defcustom standard value."
  (let ((beads-agent-ralph-permission-mode "bypassPermissions"))
    (should (beads-agent-ralph--permission-mode-at-default-p)))
  (let ((beads-agent-ralph-permission-mode "ask"))
    (should-not (beads-agent-ralph--permission-mode-at-default-p))))

;;; Model + extra-args wiring

(ert-deftest beads-agent-ralph-test-effective-extra-args-empty ()
  "No model, no extra-args, no protect-paths produces an empty list."
  (let ((c (beads-agent-ralph-test--make-controller
            :model nil :extra-args nil))
        (beads-agent-ralph-verify-protect-paths nil))
    (should (null (beads-agent-ralph--effective-extra-args c)))))

(ert-deftest beads-agent-ralph-test-effective-extra-args-model-only ()
  "A model slot renders as a `--model VALUE' pair."
  (let ((c (beads-agent-ralph-test--make-controller
            :model "haiku" :extra-args nil))
        (beads-agent-ralph-verify-protect-paths nil))
    (should (equal (beads-agent-ralph--effective-extra-args c)
                   '("--model" "haiku")))))

(ert-deftest beads-agent-ralph-test-effective-extra-args-extra-only ()
  "Bare extra-args pass through unchanged."
  (let ((c (beads-agent-ralph-test--make-controller
            :model nil :extra-args '("--settings" "/p")))
        (beads-agent-ralph-verify-protect-paths nil))
    (should (equal (beads-agent-ralph--effective-extra-args c)
                   '("--settings" "/p")))))

(ert-deftest beads-agent-ralph-test-effective-extra-args-model-first ()
  "Model precedes extra-args so user overrides remain visible at the tail."
  (let ((c (beads-agent-ralph-test--make-controller
            :model "sonnet" :extra-args '("--max-budget-usd" "0.5")))
        (beads-agent-ralph-verify-protect-paths nil))
    (should (equal (beads-agent-ralph--effective-extra-args c)
                   '("--model" "sonnet" "--max-budget-usd" "0.5")))))

(ert-deftest beads-agent-ralph-test-effective-extra-args-injects-settings ()
  "When protect-paths are set, --settings PATH is appended."
  (let* ((c (beads-agent-ralph-test--make-controller
             :model nil :extra-args nil))
         (beads-agent-ralph-verify-protect-paths '("test/**"))
         (args (beads-agent-ralph--effective-extra-args c)))
    (unwind-protect
        (progn
          (should (member "--settings" args))
          (let ((path (cadr (member "--settings" args))))
            (should (stringp path))
            (should (file-readable-p path))))
      (beads-agent-ralph--cleanup-settings-file c))))

;;; Permission-settings file injection (bde-soyy)

(ert-deftest beads-agent-ralph-test-settings-deny-rules-empty ()
  "Empty protect-paths produce zero deny rules."
  (should (null (beads-agent-ralph--settings-deny-rules nil))))

(ert-deftest beads-agent-ralph-test-settings-deny-rules-cross-product ()
  "Each glob produces one rule per tool in tool-denies."
  (let* ((beads-agent-ralph-settings-tool-denies '("Edit" "Write"))
         (rules (beads-agent-ralph--settings-deny-rules
                 '("test/**" "spec/**"))))
    (should (member "Edit(test/**)" rules))
    (should (member "Write(test/**)" rules))
    (should (member "Edit(spec/**)" rules))
    (should (member "Write(spec/**)" rules))
    (should (= 4 (length rules)))))

(ert-deftest beads-agent-ralph-test-settings-payload-shape ()
  "Payload is a JSON object with permissions.deny array."
  (let* ((beads-agent-ralph-verify-protect-paths '("test/**"))
         (json (beads-agent-ralph--settings-payload))
         (parsed (let ((json-object-type 'alist)
                       (json-array-type 'list)
                       (json-key-type 'string))
                   (json-read-from-string json))))
    (should (assoc "permissions" parsed))
    (should (assoc "deny" (cdr (assoc "permissions" parsed))))
    (should (cl-some (lambda (rule) (string-match-p "test/\\*\\*" rule))
                     (cdr (assoc "deny"
                                 (cdr (assoc "permissions" parsed))))))))

(ert-deftest beads-agent-ralph-test-ensure-settings-file-skipped-without-paths ()
  "Empty protect-paths returns nil and writes no file."
  (let ((c (beads-agent-ralph-test--make-controller))
        (beads-agent-ralph-verify-protect-paths nil))
    (should (null (beads-agent-ralph--ensure-settings-file c)))
    (should (null (oref c settings-file)))))

(ert-deftest beads-agent-ralph-test-ensure-settings-file-writes-once ()
  "Calling ensure-settings-file twice reuses the same path."
  (let ((c (beads-agent-ralph-test--make-controller))
        (beads-agent-ralph-verify-protect-paths '("test/**")))
    (unwind-protect
        (let ((p1 (beads-agent-ralph--ensure-settings-file c))
              (p2 (beads-agent-ralph--ensure-settings-file c)))
          (should (equal p1 p2))
          (should (file-readable-p p1)))
      (beads-agent-ralph--cleanup-settings-file c))))

(ert-deftest beads-agent-ralph-test-cleanup-settings-file ()
  "Cleanup deletes the temp file and clears the slot."
  (let ((c (beads-agent-ralph-test--make-controller))
        (beads-agent-ralph-verify-protect-paths '("test/**")))
    (let ((path (beads-agent-ralph--ensure-settings-file c)))
      (should (file-readable-p path))
      (beads-agent-ralph--cleanup-settings-file c)
      (should-not (file-exists-p path))
      (should-not (oref c settings-file)))))

;;; Worktree resolution (bde-soyy)

(ert-deftest beads-agent-ralph-test-worktree-skipped-when-disabled ()
  "Worktree resolution is a no-op when use-worktrees is nil."
  (let ((c (beads-agent-ralph-test--make-controller))
        (beads-agent-use-worktrees nil)
        (called nil))
    (beads-agent-ralph--maybe-resolve-worktree-async
     c (lambda () (setq called t)))
    (should called)
    (should (oref c worktree-resolved))
    (should-not (oref c worktree-dir))))

(ert-deftest beads-agent-ralph-test-worktree-uses-explicit-dir ()
  "Explicit :worktree-dir at start is preserved and marks resolved."
  (let ((c (beads-agent-ralph-test--make-controller
            :worktree-dir "/tmp/explicit"))
        (called nil))
    (beads-agent-ralph--maybe-resolve-worktree-async
     c (lambda () (setq called t)))
    (should called)
    (should (oref c worktree-resolved))
    (should (equal "/tmp/explicit" (oref c worktree-dir)))))

(defmacro beads-agent-ralph-test--with-worktree-resolver (impl &rest body)
  "Bind `beads-agent--ensure-worktree-async' to IMPL for BODY.
Uses an explicit `fset' / `fmakunbound' pair so the stub works
reliably across tests even when the symbol was previously unbound.
`cl-letf' on `symbol-function' loses its restoration target for a
previously-unbound symbol and leaves the binding visible to the next
test, which is exactly what we need to avoid."
  (declare (indent 1) (debug (form body)))
  (let ((prev (make-symbol "prev"))
        (was-bound (make-symbol "was-bound")))
    `(let* ((,was-bound (fboundp 'beads-agent--ensure-worktree-async))
            (,prev (and ,was-bound
                        (symbol-function 'beads-agent--ensure-worktree-async))))
       (unwind-protect
           (progn
             (fset 'beads-agent--ensure-worktree-async ,impl)
             ,@body)
         (if ,was-bound
             (fset 'beads-agent--ensure-worktree-async ,prev)
           (fmakunbound 'beads-agent--ensure-worktree-async))))))

(ert-deftest beads-agent-ralph-test-worktree-resolve-success ()
  "Successful worktree async resolve pins the path on the controller."
  (let ((c (beads-agent-ralph-test--make-controller))
        (beads-agent-use-worktrees t)
        (called nil))
    (beads-agent-ralph-test--with-worktree-resolver
        (lambda (_id cb) (funcall cb t "/tmp/resolved"))
      (beads-agent-ralph--maybe-resolve-worktree-async
       c (lambda () (setq called t))))
    (should called)
    (should (oref c worktree-resolved))
    (should (equal "/tmp/resolved" (oref c worktree-dir)))))

(ert-deftest beads-agent-ralph-test-worktree-resolve-failure-banner ()
  "Failed worktree async still calls DONE and pushes a warning banner."
  (let ((c (beads-agent-ralph-test--make-controller))
        (beads-agent-use-worktrees t)
        (called nil))
    (beads-agent-ralph-test--with-worktree-resolver
        (lambda (_id cb) (funcall cb nil "fake-error"))
      (beads-agent-ralph--maybe-resolve-worktree-async
       c (lambda () (setq called t))))
    (should called)
    (should (oref c worktree-resolved))
    (should-not (oref c worktree-dir))
    (should (cl-some (lambda (b) (eq 'warning (plist-get b :severity)))
                     (oref c banner-log)))))

(ert-deftest beads-agent-ralph-test-worktree-resolve-is-one-shot ()
  "A second call after resolve invokes DONE immediately without resolver."
  (let ((c (beads-agent-ralph-test--make-controller))
        (resolver-calls 0)
        (called nil))
    (oset c worktree-resolved t)
    (beads-agent-ralph-test--with-worktree-resolver
        (lambda (_id cb)
          (cl-incf resolver-calls)
          (funcall cb t "/tmp/r"))
      (beads-agent-ralph--maybe-resolve-worktree-async
       c (lambda () (setq called t))))
    (should called)
    (should (zerop resolver-calls))))

(provide 'beads-agent-ralph-test)

;;; beads-agent-ralph-test.el ends here
