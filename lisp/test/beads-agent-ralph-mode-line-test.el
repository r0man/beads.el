;;; beads-agent-ralph-mode-line-test.el --- Tests for Ralph mode-line + notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT coverage for the Ralph mode-line indicator and desktop
;; notifications module (`beads-agent-ralph-mode-line.el', bde-vjfa):
;;
;; - Width-adaptive cost formatter holds 4-char width.
;; - State-change observer enables / disables the mode-line minor mode.
;; - Sticky window keeps terminal indicators visible briefly.
;; - Notification policy fires for the right statuses only.
;; - Notification body renders the format spec correctly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-ralph)
(require 'beads-agent-ralph-mode-line)

;;; Test Fixtures

(defun beads-agent-ralph-mode-line-test--make-controller (&rest overrides)
  "Build a controller with sensible defaults overridable via OVERRIDES."
  (apply #'beads-agent-ralph--controller
         (append overrides
                 (list :root-id (or (plist-get overrides :root-id) "bde-42")
                       :root-kind (or (plist-get overrides :root-kind) 'issue)
                       :iteration (or (plist-get overrides :iteration) 3)
                       :max-iterations
                       (or (plist-get overrides :max-iterations) 12)
                       :cumulative-cost-usd
                       (or (plist-get overrides :cumulative-cost-usd) 4.17)
                       :started-at (or (plist-get overrides :started-at)
                                       (time-subtract (current-time) 60))
                       :status (or (plist-get overrides :status) 'idle)))))

(defmacro beads-agent-ralph-mode-line-test--with-clean-registry (&rest body)
  "Run BODY with a freshly cleared registry and mode-line state.
Disables the minor mode at teardown so leftover state never leaks."
  (declare (indent 0) (debug (body)))
  `(unwind-protect
       (progn
         (beads-agent-ralph--mode-line-registry-clear)
         (when beads-agent-ralph-mode-line-mode
           (beads-agent-ralph-mode-line-mode -1))
         ,@body)
     (beads-agent-ralph--mode-line-registry-clear)
     (when beads-agent-ralph-mode-line-mode
       (beads-agent-ralph-mode-line-mode -1))))

;;; Cost Formatter — 4-char width invariant

(ert-deftest beads-agent-ralph-mode-line-test-cost-below-ten ()
  "Costs under $10 render as X.XX (4 chars)."
  (should (equal (beads-agent-ralph--format-cost 0) "0.00"))
  (should (equal (beads-agent-ralph--format-cost 4.17) "4.17"))
  (should (equal (beads-agent-ralph--format-cost 9.999) "10.0")))

(ert-deftest beads-agent-ralph-mode-line-test-cost-below-hundred ()
  "Costs $10–$99 render as XX.X (4 chars)."
  (should (equal (beads-agent-ralph--format-cost 10) "10.0"))
  (should (equal (beads-agent-ralph--format-cost 87.3) "87.3"))
  (should (equal (beads-agent-ralph--format-cost 99.94) "99.9")))

(ert-deftest beads-agent-ralph-mode-line-test-cost-below-thousand ()
  "Costs $100–$999 render as XXX-space (4 chars, integer + pad)."
  (should (equal (beads-agent-ralph--format-cost 100) "100 "))
  (should (equal (beads-agent-ralph--format-cost 487) "487 "))
  (should (equal (beads-agent-ralph--format-cost 999.9) "999 ")))

(ert-deftest beads-agent-ralph-mode-line-test-cost-below-tenk ()
  "Costs $1000–$9999 render as X.Xk (4 chars)."
  (should (equal (beads-agent-ralph--format-cost 1000) "1.0k"))
  (should (equal (beads-agent-ralph--format-cost 1200) "1.2k"))
  (should (equal (beads-agent-ralph--format-cost 9876) "9.9k")))

(ert-deftest beads-agent-ralph-mode-line-test-cost-above-tenk ()
  "Costs >= $10000 render as XXk-space (4 chars)."
  (should (equal (beads-agent-ralph--format-cost 10000) "10k "))
  (should (equal (beads-agent-ralph--format-cost 12345) "12k ")))

(ert-deftest beads-agent-ralph-mode-line-test-cost-width-is-always-four ()
  "Whatever bucket the cost lands in, the rendering is exactly 4 chars."
  (dolist (cost '(0 4.17 9.99 10 87.3 99.94 100 487 999.9 1000 1200 9876
                    10000 12345))
    (should (= 4 (length (beads-agent-ralph--format-cost cost))))))

;;; Indicator String

(ert-deftest beads-agent-ralph-mode-line-test-string-nil-when-empty ()
  "With no registered controller the indicator returns nil."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (should (null (beads-agent-ralph--mode-line-string)))))

(ert-deftest beads-agent-ralph-mode-line-test-string-formats-counts ()
  "The indicator includes Ralph N/M and the formatted cost."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-mode-line-test--make-controller
                 :iteration 5 :max-iterations 50
                 :cumulative-cost-usd 4.17 :status 'running)))
      (beads-agent-ralph--mode-line-register ctrl)
      (let ((s (substring-no-properties
                (beads-agent-ralph--mode-line-string))))
        (should (string-match-p "Ralph 5/50" s))
        (should (string-match-p "\\$4\\.17" s))))))

(ert-deftest beads-agent-ralph-mode-line-test-string-face-for-stalled ()
  "Auto-paused controllers render with the stalled face."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-mode-line-test--make-controller
                 :status 'auto-paused)))
      (beads-agent-ralph--mode-line-register ctrl)
      (let* ((s (beads-agent-ralph--mode-line-string))
             (face (get-text-property 0 'face s)))
        (should (eq face 'beads-agent-ralph-mode-line-stalled))))))

(ert-deftest beads-agent-ralph-mode-line-test-string-face-for-failed ()
  "Failed controllers render with the failed face."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-mode-line-test--make-controller
                 :status 'failed)))
      (beads-agent-ralph--mode-line-register ctrl)
      (let* ((s (beads-agent-ralph--mode-line-string))
             (face (get-text-property 0 'face s)))
        (should (eq face 'beads-agent-ralph-mode-line-failed))))))

;;; Registry lifecycle

(ert-deftest beads-agent-ralph-mode-line-test-state-change-enables-mode ()
  "Transitioning to running enables the mode-line minor mode."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-mode-line-test--make-controller)))
      (should-not beads-agent-ralph-mode-line-mode)
      (beads-agent-ralph--mode-line-on-state-change ctrl 'running)
      (should beads-agent-ralph-mode-line-mode)
      (should (eq ctrl (beads-agent-ralph--mode-line-active-controller))))))

(ert-deftest beads-agent-ralph-mode-line-test-state-change-keeps-sticky ()
  "After a terminal transition the indicator stays visible until sweep."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((beads-agent-ralph-mode-line-sticky-seconds 30)
          (ctrl (beads-agent-ralph-mode-line-test--make-controller)))
      (beads-agent-ralph--mode-line-on-state-change ctrl 'running)
      (beads-agent-ralph--mode-line-on-state-change ctrl 'done)
      (should beads-agent-ralph-mode-line-mode)
      (should (eq ctrl (beads-agent-ralph--mode-line-active-controller)))
      (let ((entry (beads-agent-ralph--mode-line-find ctrl)))
        (should entry)
        (should (plist-get entry :sticky-until))))))

(ert-deftest beads-agent-ralph-mode-line-test-zero-sticky-removes-immediately ()
  "Sticky-seconds of 0 removes terminal controllers without delay."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((beads-agent-ralph-mode-line-sticky-seconds 0)
          (ctrl (beads-agent-ralph-mode-line-test--make-controller)))
      (beads-agent-ralph--mode-line-on-state-change ctrl 'running)
      (should beads-agent-ralph-mode-line-mode)
      (beads-agent-ralph--mode-line-on-state-change ctrl 'done)
      (should-not beads-agent-ralph-mode-line-mode)
      (should (null (beads-agent-ralph--mode-line-active-controller))))))

(ert-deftest beads-agent-ralph-mode-line-test-sweep-removes-expired ()
  "The sweeper drops entries whose `:sticky-until' is in the past."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((beads-agent-ralph-mode-line-sticky-seconds 30)
          (ctrl (beads-agent-ralph-mode-line-test--make-controller)))
      (beads-agent-ralph--mode-line-on-state-change ctrl 'running)
      (beads-agent-ralph--mode-line-on-state-change ctrl 'done)
      (let ((entry (beads-agent-ralph--mode-line-find ctrl)))
        (setf (plist-get entry :sticky-until)
              (time-subtract (current-time) 1)))
      (beads-agent-ralph--mode-line-sweep-expired)
      (should (null (beads-agent-ralph--mode-line-active-controller)))
      (should-not beads-agent-ralph-mode-line-mode))))

(ert-deftest beads-agent-ralph-mode-line-test-resume-clears-sticky ()
  "Re-registering a controller (e.g. resume) clears the sticky tail."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((beads-agent-ralph-mode-line-sticky-seconds 30)
          (ctrl (beads-agent-ralph-mode-line-test--make-controller)))
      (beads-agent-ralph--mode-line-on-state-change ctrl 'running)
      (beads-agent-ralph--mode-line-on-state-change ctrl 'auto-paused)
      (beads-agent-ralph--mode-line-on-state-change ctrl 'running)
      (let ((entry (beads-agent-ralph--mode-line-find ctrl)))
        (should entry)
        (should (null (plist-get entry :sticky-until)))))))

(ert-deftest beads-agent-ralph-mode-line-test-mode-toggle-adds-misc-info ()
  "Enabling the mode adds its entry to mode-line-misc-info; disabling removes it."
  (beads-agent-ralph-mode-line-test--with-clean-registry
    (let ((mode-line-misc-info nil))
      (beads-agent-ralph-mode-line-mode 1)
      (should (member beads-agent-ralph--mode-line-misc-info-entry
                      mode-line-misc-info))
      (beads-agent-ralph-mode-line-mode -1)
      (should-not (member beads-agent-ralph--mode-line-misc-info-entry
                          mode-line-misc-info)))))

;;; Notification policy

(ert-deftest beads-agent-ralph-mode-line-test-policy-always ()
  "Policy `always' fires for any status."
  (let ((beads-agent-ralph-notify 'always))
    (dolist (s '(running cooling-down done stopped failed auto-paused))
      (should (beads-agent-ralph--should-notify-p s)))))

(ert-deftest beads-agent-ralph-mode-line-test-policy-never ()
  "Policy `never' never fires."
  (let ((beads-agent-ralph-notify 'never))
    (dolist (s '(running cooling-down done stopped failed auto-paused))
      (should-not (beads-agent-ralph--should-notify-p s)))))

(ert-deftest beads-agent-ralph-mode-line-test-policy-on-stop ()
  "Policy `on-stop' fires for terminal states including success."
  (let ((beads-agent-ralph-notify 'on-stop))
    (should (beads-agent-ralph--should-notify-p 'done))
    (should (beads-agent-ralph--should-notify-p 'stopped))
    (should (beads-agent-ralph--should-notify-p 'failed))
    (should (beads-agent-ralph--should-notify-p 'auto-paused))
    (should-not (beads-agent-ralph--should-notify-p 'running))
    (should-not (beads-agent-ralph--should-notify-p 'cooling-down))))

(ert-deftest beads-agent-ralph-mode-line-test-policy-on-failure ()
  "Policy `on-failure' excludes successful `done'."
  (let ((beads-agent-ralph-notify 'on-failure))
    (should-not (beads-agent-ralph--should-notify-p 'done))
    (should (beads-agent-ralph--should-notify-p 'stopped))
    (should (beads-agent-ralph--should-notify-p 'failed))
    (should (beads-agent-ralph--should-notify-p 'auto-paused))
    (should-not (beads-agent-ralph--should-notify-p 'running))))

;;; Notification body rendering

(ert-deftest beads-agent-ralph-mode-line-test-body-done-format ()
  "Body for `done' uses the closed suffix and includes iteration + cost.
Natural-success (`sentinel') reason is suppressed: a successful body
should not carry a trailing reason segment per the spec example."
  (let* ((ctrl (beads-agent-ralph-mode-line-test--make-controller
                :iteration 12 :cumulative-cost-usd 4.17
                :started-at (time-subtract (current-time) (+ (* 60 60) (* 34 60)))
                :done-reason 'sentinel))
         (body (beads-agent-ralph--notification-body ctrl 'done)))
    (should (string-match-p "done" body))
    (should (string-match-p "bde-42 closed" body))
    (should (string-match-p "12 iters" body))
    (should (string-match-p "\\$4\\.17" body))
    (should (string-match-p "1h" body))
    (should-not (string-match-p "agent" body))))

(ert-deftest beads-agent-ralph-mode-line-test-body-stopped-budget ()
  "Body for budget-stopped includes the budget reason and `open' suffix."
  (let* ((ctrl (beads-agent-ralph-mode-line-test--make-controller
                :iteration 7 :cumulative-cost-usd 2.91
                :started-at (time-subtract (current-time) (* 22 60))
                :done-reason 'budget))
         (body (beads-agent-ralph--notification-body ctrl 'stopped)))
    (should (string-match-p "bde-42 open" body))
    (should (string-match-p "budget" body))))

(ert-deftest beads-agent-ralph-mode-line-test-body-stall-reason ()
  "Body for an auto-paused controller surfaces stall counter."
  (let* ((ctrl (beads-agent-ralph-mode-line-test--make-controller
                :iteration 5 :consecutive-stalls 2
                :cumulative-cost-usd 1.04
                :started-at (time-subtract (current-time) (* 18 60))))
         (body (beads-agent-ralph--notification-body ctrl 'auto-paused)))
    (should (string-match-p "auto-paused" body))
    (should (string-match-p "stalled 2" body))))

(ert-deftest beads-agent-ralph-mode-line-test-title-uses-root-id ()
  "Title is composed from the title format and the controller's root id."
  (let ((ctrl (beads-agent-ralph-mode-line-test--make-controller
               :root-id "bde-zzz")))
    (should (equal (beads-agent-ralph--notification-title ctrl)
                   "Ralph: bde-zzz"))))

;;; Elapsed formatter

(ert-deftest beads-agent-ralph-mode-line-test-elapsed-seconds ()
  "Sub-minute elapses render as Ns."
  (let ((started (time-subtract (current-time) 47)))
    (should (equal (beads-agent-ralph--format-elapsed started) "47s"))))

(ert-deftest beads-agent-ralph-mode-line-test-elapsed-minutes ()
  "Sub-hour elapses render as Nm."
  (let ((started (time-subtract (current-time) (* 22 60))))
    (should (equal (beads-agent-ralph--format-elapsed started) "22m"))))

(ert-deftest beads-agent-ralph-mode-line-test-elapsed-hours ()
  "Hour-plus elapses render as XhYYm."
  (let ((started (time-subtract (current-time)
                                (+ (* 1 3600) (* 34 60)))))
    (should (equal (beads-agent-ralph--format-elapsed started) "1h34m"))))

(ert-deftest beads-agent-ralph-mode-line-test-elapsed-nil ()
  "Missing started-at renders as `?' rather than erroring."
  (should (equal (beads-agent-ralph--format-elapsed nil) "?")))

;;; Hook installation

(ert-deftest beads-agent-ralph-mode-line-test-hook-is-installed ()
  "Loading the module registers the state-change observer."
  (should (memq #'beads-agent-ralph--mode-line-on-state-change
                beads-agent-ralph-state-change-functions)))

(provide 'beads-agent-ralph-mode-line-test)

;;; beads-agent-ralph-mode-line-test.el ends here
