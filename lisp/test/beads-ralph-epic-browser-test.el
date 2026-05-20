;;; beads-ralph-epic-browser-test.el --- Tests for the minimum vui epic browser -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT coverage for `lisp/beads-ralph-epic-browser.el' (bde-deqx.6).
;;
;; Acceptance criteria from the bead, covered here:
;;   - epic-row rendering with fixture data;
;;   - Ralph badge appears / disappears with the controller registry.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'beads-agent-ralph)
(require 'beads-ralph-epic-browser)
(require 'beads-types)

;;; Fixtures

(defun beads-ralph-epic-browser-test--make-epic-status
    (&rest args)
  "Build a `beads-epic-status' for tests.
ARGS is a plist: :id :title :priority :status :closed :total."
  (let* ((id (or (plist-get args :id) "bde-fake"))
         (title (or (plist-get args :title) "Fake epic"))
         (priority (or (plist-get args :priority) 1))
         (status (or (plist-get args :status) "open"))
         (closed (or (plist-get args :closed) 0))
         (total (or (plist-get args :total) 0))
         (epic (beads-issue :id id :title title
                            :priority priority :status status
                            :issue-type "epic")))
    (beads-epic-status :epic epic
                       :closed-children closed
                       :total-children total
                       :eligible-for-close (and (> total 0)
                                                (= total closed)))))

(defun beads-ralph-epic-browser-test--make-controller
    (root-id &optional iter max-iter)
  "Build a minimal `beads-agent-ralph--controller' pinned to ROOT-ID."
  (beads-agent-ralph--controller
   :root-id root-id
   :root-kind 'epic
   :iteration (or iter 0)
   :max-iterations (or max-iter 10)
   :cumulative-cost-usd 0.0
   :started-at (current-time)
   :status 'idle))

(defmacro beads-ralph-epic-browser-test--with-clean-registry (&rest body)
  "Run BODY with a fresh empty controller registry."
  (declare (indent 0) (debug (body)))
  `(let ((beads-agent-ralph--controllers nil))
     (unwind-protect (progn ,@body)
       (setq beads-agent-ralph--controllers nil))))

(defmacro beads-ralph-epic-browser-test--with-buffer (var &rest body)
  "Bind VAR to a fresh browser buffer; tear it down after BODY.
The buffer is created, the mode is enabled, but no fetch occurs."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (generate-new-buffer
                "*beads-ralph-epic-browser-test*")))
     (unwind-protect
         (with-current-buffer ,var
           (beads-ralph-epic-browser-mode)
           ,@body)
       (when (buffer-live-p ,var)
         (kill-buffer ,var)))))

;;; Pure row helpers

(ert-deftest beads-ralph-epic-browser-test-row-id ()
  "Row helpers project the epic id out of an epic-status."
  (let ((es (beads-ralph-epic-browser-test--make-epic-status :id "bde-x1")))
    (should (string= "bde-x1" (beads-ralph-epic-browser--row-id es)))))

(ert-deftest beads-ralph-epic-browser-test-row-priority-format ()
  "Priority is rendered as PN (P0 .. P4)."
  (let ((es (beads-ralph-epic-browser-test--make-epic-status :priority 2)))
    (should (string= "P2" (beads-ralph-epic-browser--row-priority es)))))

(ert-deftest beads-ralph-epic-browser-test-row-status-in-progress ()
  "The in_progress status renders as the compact \"in_prog\"."
  (let ((es (beads-ralph-epic-browser-test--make-epic-status
             :status "in_progress")))
    (should (string= "in_prog" (beads-ralph-epic-browser--row-status es)))))

(ert-deftest beads-ralph-epic-browser-test-row-progress ()
  "Progress is rendered as closed/total."
  (let ((es (beads-ralph-epic-browser-test--make-epic-status
             :closed 4 :total 9)))
    (should (string= "4/9" (beads-ralph-epic-browser--row-progress es)))))

;;; Ralph badge driven by the registry

(ert-deftest beads-ralph-epic-browser-test-badge-absent-without-controller ()
  "With no registered controller, the badge is nil."
  (beads-ralph-epic-browser-test--with-clean-registry
    (let ((es (beads-ralph-epic-browser-test--make-epic-status :id "bde-x2")))
      (should (null (beads-ralph-epic-browser--row-ralph-badge es))))))

(ert-deftest beads-ralph-epic-browser-test-badge-present-with-controller ()
  "Registering a controller for the epic-id surfaces the badge."
  (beads-ralph-epic-browser-test--with-clean-registry
    (let ((es (beads-ralph-epic-browser-test--make-epic-status :id "bde-x3"))
          (ctrl (beads-ralph-epic-browser-test--make-controller
                 "bde-x3" 3 20)))
      (beads-agent-ralph--register-controller ctrl)
      (let ((badge (beads-ralph-epic-browser--row-ralph-badge es)))
        (should (stringp badge))
        (should (string-match-p "iter 3/20" badge))
        (should (string-match-p "Ralph running" badge))))))

(ert-deftest beads-ralph-epic-browser-test-badge-disappears-on-unregister ()
  "Unregistering the controller removes the badge again."
  (beads-ralph-epic-browser-test--with-clean-registry
    (let ((es (beads-ralph-epic-browser-test--make-epic-status :id "bde-x4"))
          (ctrl (beads-ralph-epic-browser-test--make-controller
                 "bde-x4" 1 10)))
      (beads-agent-ralph--register-controller ctrl)
      (should (beads-ralph-epic-browser--row-ralph-badge es))
      (beads-agent-ralph--unregister-controller ctrl)
      (should (null (beads-ralph-epic-browser--row-ralph-badge es))))))

(ert-deftest beads-ralph-epic-browser-test-running-count ()
  "The header running-count counts only epics with a live controller."
  (beads-ralph-epic-browser-test--with-clean-registry
    (let* ((a (beads-ralph-epic-browser-test--make-epic-status :id "bde-a"))
           (b (beads-ralph-epic-browser-test--make-epic-status :id "bde-b"))
           (c (beads-ralph-epic-browser-test--make-epic-status :id "bde-c"))
           (ca (beads-ralph-epic-browser-test--make-controller "bde-a"))
           (cc (beads-ralph-epic-browser-test--make-controller "bde-c")))
      (beads-agent-ralph--register-controller ca)
      (beads-agent-ralph--register-controller cc)
      (should (= 2 (beads-ralph-epic-browser--ralph-running-count
                    (list a b c)))))))

(ert-deftest beads-ralph-epic-browser-test-header-line-shape ()
  "Header line reports total epics and Ralph-running count."
  (beads-ralph-epic-browser-test--with-clean-registry
    (let* ((a (beads-ralph-epic-browser-test--make-epic-status :id "bde-a"))
           (b (beads-ralph-epic-browser-test--make-epic-status :id "bde-b"))
           (ca (beads-ralph-epic-browser-test--make-controller "bde-a")))
      (beads-agent-ralph--register-controller ca)
      (let ((line (beads-ralph-epic-browser--header-line (list a b))))
        (should (string-match-p "2 total" line))
        (should (string-match-p "1 Ralph running" line))))))

;;; Mount + rendered output

(ert-deftest beads-ralph-epic-browser-test-mount-renders-epic-ids ()
  "Mounting writes each epic id into the buffer."
  (beads-ralph-epic-browser-test--with-clean-registry
    (beads-ralph-epic-browser-test--with-buffer buf
      (let ((epics (list
                    (beads-ralph-epic-browser-test--make-epic-status
                     :id "bde-zz1" :title "Alpha" :priority 1
                     :status "open" :closed 0 :total 4)
                    (beads-ralph-epic-browser-test--make-epic-status
                     :id "bde-zz2" :title "Beta" :priority 2
                     :status "in_progress" :closed 1 :total 3))))
        (beads-ralph-epic-browser--render buf epics)
        (let ((text (buffer-string)))
          (should (string-match-p "bde-zz1" text))
          (should (string-match-p "bde-zz2" text))
          (should (string-match-p "Alpha" text))
          (should (string-match-p "Beta" text))
          (should (string-match-p "0/4" text))
          (should (string-match-p "1/3" text)))))))

(ert-deftest beads-ralph-epic-browser-test-mount-includes-action-bar ()
  "Rendered buffer contains the action-bar legend."
  (beads-ralph-epic-browser-test--with-clean-registry
    (beads-ralph-epic-browser-test--with-buffer buf
      (beads-ralph-epic-browser--render
       buf (list (beads-ralph-epic-browser-test--make-epic-status
                  :id "bde-zz3")))
      (should (string-match-p
               "\\[R\\]un.*\\[d\\]etails.*\\[g\\]refresh.*\\[q\\]uit"
               (buffer-string))))))

(ert-deftest beads-ralph-epic-browser-test-mount-shows-ralph-badge ()
  "A live controller adds an inline Ralph badge to its row."
  (beads-ralph-epic-browser-test--with-clean-registry
    (beads-ralph-epic-browser-test--with-buffer buf
      (let ((ctrl (beads-ralph-epic-browser-test--make-controller
                   "bde-zz4" 7 20)))
        (beads-agent-ralph--register-controller ctrl)
        (beads-ralph-epic-browser--render
         buf (list (beads-ralph-epic-browser-test--make-epic-status
                    :id "bde-zz4" :title "Live")))
        (let ((text (buffer-string)))
          (should (string-match-p "Ralph running" text))
          (should (string-match-p "iter 7/20" text)))))))

(ert-deftest beads-ralph-epic-browser-test-mount-empty-list ()
  "An empty list still renders cleanly without errors."
  (beads-ralph-epic-browser-test--with-clean-registry
    (beads-ralph-epic-browser-test--with-buffer buf
      (beads-ralph-epic-browser--render buf nil)
      (should (string-match-p "0 total" (buffer-string)))
      (should (string-match-p "(no epics)" (buffer-string))))))

;;; State-change subscription

(ert-deftest beads-ralph-epic-browser-test-hook-is-subscribed ()
  "The browser registers its callback on the state-change hook."
  (should (memq #'beads-ralph-epic-browser--on-controller-state-change
                beads-agent-ralph-state-change-functions)))

(ert-deftest beads-ralph-epic-browser-test-hook-schedules-rerender ()
  "Firing the hook with the singleton buffer alive schedules a timer."
  (let ((buf (get-buffer-create
              beads-ralph-epic-browser-buffer-name)))
    (unwind-protect
        (with-current-buffer buf
          (beads-ralph-epic-browser-mode)
          (setq-local beads-ralph-epic-browser--rerender-timer nil)
          (beads-ralph-epic-browser--on-controller-state-change nil nil)
          (should (timerp beads-ralph-epic-browser--rerender-timer))
          ;; A second fire does NOT replace the pending timer.
          (let ((first beads-ralph-epic-browser--rerender-timer))
            (beads-ralph-epic-browser--on-controller-state-change nil nil)
            (should (eq first beads-ralph-epic-browser--rerender-timer)))
          ;; Cleanup.
          (when (timerp beads-ralph-epic-browser--rerender-timer)
            (cancel-timer beads-ralph-epic-browser--rerender-timer)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-ralph-epic-browser-test-hook-without-buffer-is-noop ()
  "When the singleton buffer is gone the hook does not error."
  (let ((buf (get-buffer beads-ralph-epic-browser-buffer-name)))
    (when buf (kill-buffer buf)))
  (should-not (get-buffer beads-ralph-epic-browser-buffer-name))
  ;; Should not signal.
  (beads-ralph-epic-browser--on-controller-state-change nil nil))

;;; Epic-id at point

(ert-deftest beads-ralph-epic-browser-test-epic-id-at-point ()
  "`epic-id-at-point' recovers the id from a rendered row."
  (beads-ralph-epic-browser-test--with-clean-registry
    (beads-ralph-epic-browser-test--with-buffer buf
      (let ((epics (list
                    (beads-ralph-epic-browser-test--make-epic-status
                     :id "bde-pp1" :title "First")
                    (beads-ralph-epic-browser-test--make-epic-status
                     :id "bde-pp2" :title "Second"))))
        (beads-ralph-epic-browser--render buf epics)
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "bde-pp1")
          (beginning-of-line)
          (should (string= "bde-pp1"
                           (beads-ralph-epic-browser--epic-id-at-point)))
          (goto-char (point-min))
          (re-search-forward "bde-pp2")
          (beginning-of-line)
          (should (string= "bde-pp2"
                           (beads-ralph-epic-browser--epic-id-at-point))))))))

(provide 'beads-ralph-epic-browser-test)

;;; beads-ralph-epic-browser-test.el ends here
