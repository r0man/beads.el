;;; beads-ralph-cockpit-test.el --- Tests for the Ralph multi-loop cockpit -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT coverage for `lisp/beads-ralph-cockpit.el' (bde-deqx.4):
;;
;;   - row data extraction from fixture controllers;
;;   - 5-way badge collapse (6 raw statuses → 5 groups);
;;   - badge annotations (cooling / auto);
;;   - filter logic (badge-group membership);
;;   - contested-claim detection from banner-log;
;;   - state-change subscription + debounce coalesce;
;;   - 1s refresh timer suspends when no loop is active;
;;   - kill-buffer cancels pending timers.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'beads-agent-ralph)
(require 'beads-ralph-cockpit)

;;; Fixtures

(defun beads-ralph-cockpit-test--make-controller
    (&rest args)
  "Build a minimal controller from ARGS.
ARGS keys: :root-id :kind :status :iter :max-iter :cost :started-at
:current :banner-log."
  (beads-agent-ralph--controller
   :root-id (or (plist-get args :root-id) "bde-test")
   :root-kind (or (plist-get args :kind) 'epic)
   :status (or (plist-get args :status) 'running)
   :iteration (or (plist-get args :iter) 0)
   :max-iterations (or (plist-get args :max-iter) 10)
   :cumulative-cost-usd (or (plist-get args :cost) 0.0)
   :started-at (or (plist-get args :started-at) (current-time))
   :current-issue-id (plist-get args :current)
   :banner-log (plist-get args :banner-log)))

(defmacro beads-ralph-cockpit-test--with-clean-registry (&rest body)
  "Run BODY with a fresh empty controller registry."
  (declare (indent 0) (debug (body)))
  `(let ((beads-agent-ralph--controllers nil))
     (unwind-protect (progn ,@body)
       (setq beads-agent-ralph--controllers nil))))

(defmacro beads-ralph-cockpit-test--with-buffer (var &rest body)
  "Bind VAR to a fresh cockpit buffer; tear it down after BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (generate-new-buffer "*beads-ralph-cockpit-test*")))
     (unwind-protect
         (with-current-buffer ,var
           (beads-ralph-cockpit-mode)
           ,@body)
       (when (buffer-live-p ,var)
         (kill-buffer ,var)))))

;;; Badge collapse

(ert-deftest beads-ralph-cockpit-test-badge-running-is-active ()
  "`running' collapses to the `active' badge group."
  (should (eq 'active (beads-ralph-cockpit--badge-group 'running))))

(ert-deftest beads-ralph-cockpit-test-badge-cooling-is-active ()
  "`cooling-down' collapses to the `active' badge group."
  (should (eq 'active (beads-ralph-cockpit--badge-group 'cooling-down))))

(ert-deftest beads-ralph-cockpit-test-badge-idle-is-active ()
  "`idle' (transient pre-first-iter state) is shown under `active'."
  (should (eq 'active (beads-ralph-cockpit--badge-group 'idle))))

(ert-deftest beads-ralph-cockpit-test-badge-auto-paused-is-paused ()
  "`auto-paused' collapses to the `paused' badge group."
  (should (eq 'paused (beads-ralph-cockpit--badge-group 'auto-paused))))

(ert-deftest beads-ralph-cockpit-test-badge-paused-is-paused ()
  "`paused' (if ever surfaced) groups as `paused'."
  (should (eq 'paused (beads-ralph-cockpit--badge-group 'paused))))

(ert-deftest beads-ralph-cockpit-test-badge-done-stopped-failed-pass-through ()
  "Terminal statuses pass through to their own badge group."
  (should (eq 'done    (beads-ralph-cockpit--badge-group 'done)))
  (should (eq 'stopped (beads-ralph-cockpit--badge-group 'stopped)))
  (should (eq 'failed  (beads-ralph-cockpit--badge-group 'failed))))

(ert-deftest beads-ralph-cockpit-test-badge-unknown-is-active ()
  "An unknown status falls back to `active' rather than hiding the row."
  (should (eq 'active (beads-ralph-cockpit--badge-group 'never-heard-of))))

(ert-deftest beads-ralph-cockpit-test-badge-annotation-cooling ()
  "`cooling-down' is annotated with \"cooling\"."
  (should (equal "cooling"
                 (beads-ralph-cockpit--badge-annotation 'cooling-down))))

(ert-deftest beads-ralph-cockpit-test-badge-annotation-auto ()
  "`auto-paused' is annotated with \"auto\"."
  (should (equal "auto"
                 (beads-ralph-cockpit--badge-annotation 'auto-paused))))

(ert-deftest beads-ralph-cockpit-test-badge-annotation-running-is-nil ()
  "`running' has no annotation."
  (should-not (beads-ralph-cockpit--badge-annotation 'running)))

(ert-deftest beads-ralph-cockpit-test-badge-label-cooling ()
  "Badge label for cooling-down reads \"active (cooling)\"."
  (should (equal "active (cooling)"
                 (beads-ralph-cockpit--badge-label 'cooling-down))))

(ert-deftest beads-ralph-cockpit-test-badge-label-running ()
  "Badge label for running reads plain \"active\"."
  (should (equal "active"
                 (beads-ralph-cockpit--badge-label 'running))))

;;; Row data extraction

(ert-deftest beads-ralph-cockpit-test-row-extracts-slots ()
  "`--row' surfaces the slots the renderer needs."
  (let* ((time (current-time))
         (c (beads-ralph-cockpit-test--make-controller
             :root-id "bde-r1" :kind 'epic :status 'running
             :iter 3 :max-iter 20 :cost 0.42 :started-at time
             :current "bde-r1.2")))
    (let ((row (beads-ralph-cockpit--row c)))
      (should (equal "bde-r1" (plist-get row :root-id)))
      (should (eq 'epic (plist-get row :kind)))
      (should (eq 'running (plist-get row :status)))
      (should (= 3 (plist-get row :iter)))
      (should (= 20 (plist-get row :max-iter)))
      (should (= 0.42 (plist-get row :cost)))
      (should (equal time (plist-get row :started-at)))
      (should (equal "bde-r1.2" (plist-get row :current)))
      (should (null (plist-get row :contested))))))

(ert-deftest beads-ralph-cockpit-test-row-defaults-nil-current ()
  "Row data tolerates a nil `current-issue-id'."
  (let* ((c (beads-ralph-cockpit-test--make-controller :current nil))
         (row (beads-ralph-cockpit--row c)))
    (should (null (plist-get row :current)))))

;;; Filters

(ert-deftest beads-ralph-cockpit-test-filter-keeps-matching ()
  "`--filtered' keeps only controllers whose badge group is in FILTERS."
  (let ((a (beads-ralph-cockpit-test--make-controller :status 'running))
        (b (beads-ralph-cockpit-test--make-controller :status 'auto-paused))
        (c (beads-ralph-cockpit-test--make-controller :status 'done)))
    (should (equal (list a)
                   (beads-ralph-cockpit--filtered (list a b c) '(active))))
    (should (equal (list b)
                   (beads-ralph-cockpit--filtered (list a b c) '(paused))))
    (should (equal (list a c)
                   (beads-ralph-cockpit--filtered (list a b c)
                                                  '(active done))))
    (should (null (beads-ralph-cockpit--filtered (list a b c) '(failed))))))

(ert-deftest beads-ralph-cockpit-test-badge-counts-includes-zeros ()
  "Header counts include every group even when count is zero."
  (let* ((c (beads-ralph-cockpit-test--make-controller :status 'running))
         (counts (beads-ralph-cockpit--badge-counts (list c))))
    (should (= 1 (cdr (assq 'active counts))))
    (should (= 0 (cdr (assq 'paused counts))))
    (should (= 0 (cdr (assq 'done counts))))
    (should (= 0 (cdr (assq 'stopped counts))))
    (should (= 0 (cdr (assq 'failed counts))))))

(ert-deftest beads-ralph-cockpit-test-header-line-shape ()
  "Header reports total loops and per-group counts."
  (let* ((a (beads-ralph-cockpit-test--make-controller :status 'running))
         (b (beads-ralph-cockpit-test--make-controller :status 'auto-paused))
         (line (beads-ralph-cockpit--header-line (list a b))))
    (should (string-match-p "2 loops" line))
    (should (string-match-p "1 active" line))
    (should (string-match-p "1 paused" line))
    (should (string-match-p "0 done" line))))

;;; Contested-claim detection

(ert-deftest beads-ralph-cockpit-test-contested-detects-banner ()
  "`--contested-p' reads the most-recent notice banner.
The controller pushes \"Claim failed for ID; continuing\" on a
losing race; the cockpit surfaces it on the row."
  (let* ((banner (list :severity 'notice
                       :text "Claim failed for bde-x; continuing"
                       :time (current-time)))
         (c (beads-ralph-cockpit-test--make-controller
             :banner-log (list banner))))
    (should (beads-ralph-cockpit--contested-p c))))

(ert-deftest beads-ralph-cockpit-test-contested-ignores-unrelated-notice ()
  "An unrelated notice does not mark the row contested."
  (let* ((banner (list :severity 'notice
                       :text "Resumed after pause"
                       :time (current-time)))
         (c (beads-ralph-cockpit-test--make-controller
             :banner-log (list banner))))
    (should-not (beads-ralph-cockpit--contested-p c))))

(ert-deftest beads-ralph-cockpit-test-contested-needs-notice-severity ()
  "A claim-failed banner at warning severity is not surfaced as contested.
The controller only writes the contested-claim banner at notice
severity; anything else is a different event."
  (let* ((banner (list :severity 'warning
                       :text "Claim failed for bde-x; continuing"
                       :time (current-time)))
         (c (beads-ralph-cockpit-test--make-controller
             :banner-log (list banner))))
    (should-not (beads-ralph-cockpit--contested-p c))))

(ert-deftest beads-ralph-cockpit-test-contested-uses-top-of-log ()
  "Only the freshest banner counts (banner-log is newest-first)."
  (let* ((older (list :severity 'notice
                      :text "Claim failed for bde-old; continuing"
                      :time (current-time)))
         (newer (list :severity 'notice
                      :text "Resumed normally"
                      :time (current-time)))
         (c (beads-ralph-cockpit-test--make-controller
             :banner-log (list newer older))))
    (should-not (beads-ralph-cockpit--contested-p c))))

;;; Mount + rendering

(ert-deftest beads-ralph-cockpit-test-mount-renders-id-and-badge ()
  "Mounting writes each controller's id and badge into the buffer."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (let ((c (beads-ralph-cockpit-test--make-controller
                :root-id "bde-mm1" :status 'running :iter 4 :max-iter 20)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-cockpit--render buf)
        (let ((text (buffer-string)))
          (should (string-match-p "bde-mm1" text))
          (should (string-match-p "active" text))
          (should (string-match-p "iter 4/20" text)))))))

(ert-deftest beads-ralph-cockpit-test-mount-shows-cooling-annotation ()
  "Cooling-down renders as the active badge with the `(cooling)' annotation."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (let ((c (beads-ralph-cockpit-test--make-controller
                :root-id "bde-mm2" :status 'cooling-down)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-cockpit--render buf)
        (should (string-match-p "active (cooling)" (buffer-string)))))))

(ert-deftest beads-ralph-cockpit-test-mount-includes-action-bar ()
  "Action-bar legend is rendered at the foot of the buffer."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (beads-ralph-cockpit--render buf)
      (let ((text (buffer-string)))
        (should (string-match-p "\\[SPC\\]switch" text))
        (should (string-match-p "\\[s\\]top" text))
        (should (string-match-p "\\[N\\]ew" text))
        (should (string-match-p "\\[q\\]uit" text))))))

(ert-deftest beads-ralph-cockpit-test-mount-empty-registry ()
  "An empty registry renders cleanly with the \"no matching loops\" hint."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (beads-ralph-cockpit--render buf)
      (let ((text (buffer-string)))
        (should (string-match-p "0 loops" text))
        (should (string-match-p "(no matching loops)" text))))))

(ert-deftest beads-ralph-cockpit-test-mount-shows-contested-marker ()
  "A contested-claim controller surfaces the inline marker on its row."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (let* ((banner (list :severity 'notice
                           :text "Claim failed for bde-c1; continuing"
                           :time (current-time)))
             (c (beads-ralph-cockpit-test--make-controller
                 :root-id "bde-c1" :status 'running
                 :banner-log (list banner))))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-cockpit--render buf)
        (should (string-match-p "claim-contested" (buffer-string)))))))

;;; State-change subscription + debounce

(ert-deftest beads-ralph-cockpit-test-hook-is-subscribed ()
  "The cockpit registers its callback on the state-change hook."
  (should (memq #'beads-ralph-cockpit--on-state-change
                beads-agent-ralph-state-change-functions)))

(ert-deftest beads-ralph-cockpit-test-hook-schedules-rerender ()
  "Firing the hook while the singleton buffer is alive arms a timer.
A second fire within the debounce window does NOT replace the
pending timer — that is the coalescing behaviour."
  (let ((buf (get-buffer-create beads-ralph-cockpit-buffer-name)))
    (unwind-protect
        (with-current-buffer buf
          (beads-ralph-cockpit-mode)
          (setq-local beads-ralph-cockpit--rerender-timer nil)
          (beads-ralph-cockpit--on-state-change nil nil)
          (should (timerp beads-ralph-cockpit--rerender-timer))
          (let ((first beads-ralph-cockpit--rerender-timer))
            (beads-ralph-cockpit--on-state-change nil nil)
            (should (eq first beads-ralph-cockpit--rerender-timer)))
          (when (timerp beads-ralph-cockpit--rerender-timer)
            (cancel-timer beads-ralph-cockpit--rerender-timer)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-ralph-cockpit-test-hook-without-buffer-is-noop ()
  "When no cockpit buffer exists the hook is a no-op."
  (let ((buf (get-buffer beads-ralph-cockpit-buffer-name)))
    (when buf (kill-buffer buf)))
  (should-not (get-buffer beads-ralph-cockpit-buffer-name))
  ;; Should not signal.
  (beads-ralph-cockpit--on-state-change nil nil))

;;; 1s refresh timer

(ert-deftest beads-ralph-cockpit-test-refresh-timer-suspended-when-idle ()
  "With no running controller, the refresh timer is not armed."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      ;; Register only a done controller.
      (let ((c (beads-ralph-cockpit-test--make-controller
                :root-id "bde-tt1" :status 'done)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-cockpit--render buf)
        (should (null beads-ralph-cockpit--refresh-timer))))))

(ert-deftest beads-ralph-cockpit-test-refresh-timer-armed-when-running ()
  "With a running controller the refresh timer is armed."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (let ((c (beads-ralph-cockpit-test--make-controller
                :root-id "bde-tt2" :status 'running)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-cockpit--render buf)
        (should (timerp beads-ralph-cockpit--refresh-timer))
        (cancel-timer beads-ralph-cockpit--refresh-timer)
        (setq-local beads-ralph-cockpit--refresh-timer nil)))))

(ert-deftest beads-ralph-cockpit-test-refresh-timer-cancels-when-going-idle ()
  "A render after all loops left running cancels the existing timer."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (let ((c (beads-ralph-cockpit-test--make-controller
                :root-id "bde-tt3" :status 'running)))
        (beads-agent-ralph--register-controller c)
        (beads-ralph-cockpit--render buf)
        (should (timerp beads-ralph-cockpit--refresh-timer))
        ;; Flip to a terminal state and re-render.
        (oset c status 'done)
        (beads-ralph-cockpit--render buf)
        (should (null beads-ralph-cockpit--refresh-timer))))))

;;; Kill-buffer cleanup

(ert-deftest beads-ralph-cockpit-test-kill-buffer-cancels-timers ()
  "Killing the cockpit cancels both pending timers."
  (beads-ralph-cockpit-test--with-clean-registry
    (let ((buf (get-buffer-create beads-ralph-cockpit-buffer-name))
          rerender refresh)
      (unwind-protect
          (progn
            (with-current-buffer buf
              (beads-ralph-cockpit-mode)
              (let ((c (beads-ralph-cockpit-test--make-controller
                        :root-id "bde-kk1" :status 'running)))
                (beads-agent-ralph--register-controller c)
                (beads-ralph-cockpit--render buf)))
            (beads-ralph-cockpit--on-state-change nil nil)
            (with-current-buffer buf
              (setq rerender beads-ralph-cockpit--rerender-timer
                    refresh beads-ralph-cockpit--refresh-timer)
              (should (memq #'beads-ralph-cockpit--kill-buffer-cleanup
                            kill-buffer-hook)))
            (should (timerp rerender))
            (should (timerp refresh))
            (should (memq rerender timer-idle-list))
            (should (memq refresh timer-list))
            (kill-buffer buf)
            (should-not (memq rerender timer-idle-list))
            (should-not (memq refresh timer-list)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; GC

(ert-deftest beads-ralph-cockpit-test-gc-drops-terminal-controllers ()
  "GC removes done/stopped/failed controllers but keeps live ones."
  (beads-ralph-cockpit-test--with-clean-registry
    (beads-ralph-cockpit-test--with-buffer buf
      (let ((live   (beads-ralph-cockpit-test--make-controller
                     :root-id "bde-live" :status 'running))
            (done   (beads-ralph-cockpit-test--make-controller
                     :root-id "bde-done" :status 'done))
            (failed (beads-ralph-cockpit-test--make-controller
                     :root-id "bde-fail" :status 'failed)))
        (beads-agent-ralph--register-controller live)
        (beads-agent-ralph--register-controller done)
        (beads-agent-ralph--register-controller failed)
        (let ((default-directory temporary-file-directory))
          (beads-ralph-cockpit-gc-done))
        (let ((view (beads-agent-ralph-controllers)))
          (should (= 1 (length view)))
          (should (eq live (car view))))))))

(provide 'beads-ralph-cockpit-test)

;;; beads-ralph-cockpit-test.el ends here
