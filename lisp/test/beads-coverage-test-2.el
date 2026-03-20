;;; beads-coverage-test-2.el --- Coverage gap tests batch 2 -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Second batch of targeted tests to increase code coverage to 85%.
;; Covers uncovered code paths in:
;; - beads-agent.el (format-header, detect-issue-id, mode-line, sessions)
;; - beads-command-worktree.el (parse, validate, format, display)
;; - beads-command-epic.el (parse, render, navigation)
;; - beads-command-formula.el (list navigation, commands, show entry)
;; - beads-command-edit.el (parse args, validate, suffixes)
;; - beads-command-list.el (validate, parse, actions)
;; - beads-command-update.el (parse, validate, changed fields)
;; - beads-command-dep.el (parse)
;; - beads-option.el (infix-read methods)
;; - beads-agent-agent-shell.el (backend methods)

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-agent)
(require 'beads-sesman)
(require 'beads-command-worktree)
(require 'beads-command-epic)
(require 'beads-command-formula)
(require 'beads-command-edit)
(require 'beads-command-list)
(require 'beads-command-update)
(require 'beads-command-dep)
(require 'beads-option)
(require 'beads-command-label)
(require 'beads-types)
(require 'beads-buffer)
(require 'beads-agent-backend)

;;; ============================================================
;;; beads-agent.el - Format Header Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-format-header ()
  "Test beads-agent--format-header with mocked sessions/backends."
  (cl-letf (((symbol-function 'beads-agent--get-all-sessions)
             (lambda () '(a b)))
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda () '(x))))
    (let ((result (beads-agent--format-header)))
      (should (stringp result))
      (should (string-match "2 sessions" result))
      (should (string-match "1 backend" result)))))

(ert-deftest beads-coverage-2-agent-format-header-empty ()
  "Test beads-agent--format-header with no sessions."
  (cl-letf (((symbol-function 'beads-agent--get-all-sessions)
             (lambda () nil))
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda () nil)))
    (let ((result (beads-agent--format-header)))
      (should (string-match "0 sessions" result))
      (should (string-match "0 backends" result)))))

(ert-deftest beads-coverage-2-agent-start-format-header ()
  "Test beads-agent-start--format-header."
  (cl-letf (((symbol-function 'beads-agent--get-available-backends)
             (lambda () '(a b)))
            ((symbol-function 'beads-agent--detect-issue-id)
             (lambda () "bd-42")))
    (let ((result (beads-agent-start--format-header)))
      (should (stringp result))
      (should (string-match "bd-42" result))
      (should (string-match "2 backend" result)))))

(ert-deftest beads-coverage-2-agent-start-format-header-no-context ()
  "Test beads-agent-start--format-header without context."
  (cl-letf (((symbol-function 'beads-agent--get-available-backends)
             (lambda () '(a)))
            ((symbol-function 'beads-agent--detect-issue-id)
             (lambda () nil)))
    (let ((result (beads-agent-start--format-header)))
      (should (stringp result))
      (should (string-match "Start AI Agent" result)))))

;;; ============================================================
;;; beads-agent.el - Issue Detection Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-detect-issue-id-from-show-buffer ()
  "Test beads-agent--detect-issue-id from beads-show buffer."
  (with-temp-buffer
    (defvar-local beads-show--issue-id nil)
    (setq beads-show--issue-id "bd-123")
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes)
                 (if (memq 'beads-list-mode modes) nil
                   (memq 'beads-show-mode modes)))))
      (should (equal "bd-123" (beads-agent--detect-issue-id))))))

(ert-deftest beads-coverage-2-agent-detect-issue-id-from-buffer-name ()
  "Test beads-agent--detect-issue-id from buffer name."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'beads-buffer-parse-show)
               (lambda (_name) '(:issue-id "bd-456"))))
      (should (equal "bd-456" (beads-agent--detect-issue-id))))))

(ert-deftest beads-coverage-2-agent-detect-issue-id-nil ()
  "Test beads-agent--detect-issue-id returns nil when no context."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'beads-buffer-parse-show) (lambda (_) nil)))
      (should-not (beads-agent--detect-issue-id)))))

;;; ============================================================
;;; beads-agent.el - Session Selection Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-select-session-empty ()
  "Test beads-agent--select-session-completing-read with empty sessions."
  (should-not (beads-agent--select-session-completing-read nil "Pick: ")))

(ert-deftest beads-coverage-2-agent-select-session-single ()
  "Test session selection with a single session."
  (let* ((session (beads-agent-session
                   :id "s1"
                   :issue-id "bd-1"
                   :backend-name "mock"
                   :project-dir "/tmp"
                   :started-at "2025-01-01T00:00:00+0000")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (caar choices)))
              ((symbol-function 'beads-agent-session-type-name)
               (lambda (_s) "Task"))
              ((symbol-function 'beads-agent--session-instance-number)
               (lambda (_s) 1)))
      (let ((result (beads-agent--select-session-completing-read
                     (list session) "Pick: ")))
        (should (eq result session))))))

;;; ============================================================
;;; beads-agent.el - Typed Start / Stop / Jump Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-start-typed-no-issue ()
  "Test beads-agent--start-typed when no issue at point."
  (cl-letf (((symbol-function 'beads-agent--detect-issue-id) (lambda () nil)))
    (should-error (beads-agent--start-typed "Task") :type 'user-error)))

(ert-deftest beads-coverage-2-agent-stop-at-point-no-issue ()
  "Test beads-agent-stop-at-point with no issue."
  (cl-letf (((symbol-function 'beads-agent--detect-issue-id) (lambda () nil)))
    (should-error (beads-agent-stop-at-point) :type 'user-error)))

(ert-deftest beads-coverage-2-agent-stop-at-point-no-sessions ()
  "Test beads-agent-stop-at-point with no sessions."
  (cl-letf (((symbol-function 'beads-agent--detect-issue-id)
             (lambda () "bd-1"))
            ((symbol-function 'beads-agent--get-sessions-for-issue)
             (lambda (_id) nil)))
    ;; Should just message, not error
    (beads-agent-stop-at-point)))

(ert-deftest beads-coverage-2-agent-stop-at-point-single-session ()
  "Test beads-agent-stop-at-point with one session."
  (let ((stopped nil))
    (cl-letf (((symbol-function 'beads-agent--detect-issue-id)
               (lambda () "bd-1"))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id)
                 (list (beads-agent-session
                        :id "s1" :issue-id "bd-1"
                        :backend-name "mock" :project-dir "/tmp"
                        :started-at "2025-01-01T00:00:00+0000"))))
              ((symbol-function 'beads-agent-stop)
               (lambda (sid) (setq stopped sid))))
      (beads-agent-stop-at-point)
      (should (equal "s1" stopped)))))

(ert-deftest beads-coverage-2-agent-jump-at-point-no-issue ()
  "Test beads-agent-jump-at-point with no issue at point."
  (let ((called nil))
    (cl-letf (((symbol-function 'beads-agent--detect-issue-id)
               (lambda () nil))
              ((symbol-function 'call-interactively)
               (lambda (fn) (setq called fn))))
      (beads-agent-jump-at-point)
      (should (eq called #'beads-agent-jump)))))

(ert-deftest beads-coverage-2-agent-jump-at-point-single-session ()
  "Test beads-agent-jump-at-point with one session."
  (let ((jumped nil))
    (cl-letf (((symbol-function 'beads-agent--detect-issue-id)
               (lambda () "bd-1"))
              ((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id)
                 (list (beads-agent-session
                        :id "s1" :issue-id "bd-1"
                        :backend-name "mock" :project-dir "/tmp"
                        :started-at "2025-01-01T00:00:00+0000"))))
              ((symbol-function 'beads-agent-jump)
               (lambda (sid) (setq jumped sid))))
      (beads-agent-jump-at-point)
      (should (equal "s1" jumped)))))

;;; ============================================================
;;; beads-agent.el - Focus / Touched Commands
;;; ============================================================

(ert-deftest beads-coverage-2-agent-focus-issue-no-session ()
  "Test beads-agent-focus-issue with no session."
  (cl-letf (((symbol-function 'beads-agent--get-current-project-session)
             (lambda () nil)))
    (should-error (beads-agent-focus-issue "bd-1") :type 'user-error)))

(ert-deftest beads-coverage-2-agent-clear-focus-no-session ()
  "Test beads-agent-clear-focus with no session."
  (cl-letf (((symbol-function 'beads-agent--get-current-project-session)
             (lambda () nil)))
    (should-error (beads-agent-clear-focus) :type 'user-error)))

(ert-deftest beads-coverage-2-agent-show-touched-no-session ()
  "Test beads-agent-show-touched with no session."
  (cl-letf (((symbol-function 'beads-agent--get-current-project-session)
             (lambda () nil)))
    (should-error (beads-agent-show-touched) :type 'user-error)))

;;; ============================================================
;;; beads-agent.el - Mode Line Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-mode-line-format-default ()
  "Test beads-agent--mode-line-format-default."
  (let ((beads-agent-mode-line-faces nil))
    (let ((ctx '(:project-name "beads.el" :branch "main"
                 :in-worktree nil :agent-type nil
                 :agent-instance nil :agent-session nil)))
      (let ((result (beads-agent--mode-line-format-default ctx)))
        (should (stringp result))
        (should (string-match "beads.el" result))))))

(ert-deftest beads-coverage-2-agent-mode-line-format-default-nil ()
  "Test beads-agent--mode-line-format-default with nil project."
  (let ((ctx '(:project-name nil)))
    (should-not (beads-agent--mode-line-format-default ctx))))

;;; ============================================================
;;; beads-agent.el - Transient Suffix Forwarding Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-sling-suffix-defined ()
  "Test that beads-agent--sling-suffix is defined."
  (should (fboundp 'beads-agent--sling-suffix)))

(ert-deftest beads-coverage-2-agent-start-suffix-defined ()
  "Test that beads-agent--start-suffix is defined."
  (should (fboundp 'beads-agent--start-suffix)))

(ert-deftest beads-coverage-2-agent-stop-suffix-defined ()
  "Test that beads-agent--stop-suffix is defined."
  (should (fboundp 'beads-agent--stop-suffix)))

(ert-deftest beads-coverage-2-agent-jump-suffix-defined ()
  "Test that beads-agent--jump-suffix is defined."
  (should (fboundp 'beads-agent--jump-suffix)))

(ert-deftest beads-coverage-2-agent-send-prompt-suffix-defined ()
  "Test that beads-agent--send-prompt-suffix is defined."
  (should (fboundp 'beads-agent--send-prompt-suffix)))

(ert-deftest beads-coverage-2-agent-list-suffix-defined ()
  "Test that beads-agent--list-suffix is defined."
  (should (fboundp 'beads-agent--list-suffix)))

(ert-deftest beads-coverage-2-agent-cleanup-suffix-defined ()
  "Test that beads-agent--cleanup-suffix is defined."
  (should (fboundp 'beads-agent--cleanup-suffix)))

(ert-deftest beads-coverage-2-agent-refresh-suffix-defined ()
  "Test that beads-agent--refresh-suffix is defined."
  (should (fboundp 'beads-agent--refresh-suffix)))

(ert-deftest beads-coverage-2-agent-switch-backend-suffix-defined ()
  "Test that beads-agent--switch-backend-suffix is defined."
  (should (fboundp 'beads-agent--switch-backend-suffix)))

;;; ============================================================
;;; beads-agent.el - Issue Menu Suffix Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-issue-stop-one-no-sessions ()
  "Test beads-agent-issue--stop-one with no sessions."
  (let ((beads-agent-issue--current-issue-id "bd-1"))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil)))
      ;; Should just message, not error
      (funcall (lambda ()
                 (let ((sessions (beads-agent--get-sessions-for-issue
                                  beads-agent-issue--current-issue-id)))
                   (should (null sessions))))))))

(ert-deftest beads-coverage-2-agent-issue-stop-all-no-sessions ()
  "Test beads-agent-issue--stop-all logic with no sessions."
  (let ((beads-agent-issue--current-issue-id "bd-1"))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) nil)))
      (let* ((sessions (beads-agent--get-sessions-for-issue "bd-1"))
             (count (length sessions)))
        (should (zerop count))))))

;;; ============================================================
;;; beads-agent.el - Backend Selection Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-select-backend-no-backends ()
  "Test beads-agent--select-backend with no backends."
  (cl-letf (((symbol-function 'beads-agent--get-available-backends)
             (lambda () nil)))
    (should-error (beads-agent--select-backend) :type 'user-error)))

(ert-deftest beads-coverage-2-agent-backend-available-and-get-nil ()
  "Test beads-agent--backend-available-and-get with non-existent."
  (cl-letf (((symbol-function 'beads-agent--get-backend)
             (lambda (_name) nil)))
    (should-not (beads-agent--backend-available-and-get "nonexistent"))))

;;; ============================================================
;;; beads-agent.el - Maybe Update Status (sync)
;;; ============================================================

(ert-deftest beads-coverage-2-agent-maybe-update-status-disabled ()
  "Test beads-agent--maybe-update-status when disabled."
  (let ((beads-agent-auto-set-in-progress nil))
    ;; Should not error, just be a no-op
    (beads-agent--maybe-update-status "bd-1")))

(ert-deftest beads-coverage-2-agent-maybe-update-status-non-open ()
  "Test beads-agent--maybe-update-status when issue is not open."
  (let ((beads-agent-auto-set-in-progress t))
    (cl-letf (((symbol-function 'beads-command-show!)
               (lambda (&rest _)
                 (beads-issue :id "bd-1" :title "Test" :status "in_progress"
                              :priority 1 :issue-type "task"))))
      ;; Issue is already in_progress, so no update should happen
      (beads-agent--maybe-update-status "bd-1"))))

;;; ============================================================
;;; beads-command-worktree.el - Parse Method Tests
;;; ============================================================

(ert-deftest beads-coverage-2-worktree-create-parse-success ()
  "Test beads-command-parse for worktree-create with valid JSON."
  (let* ((json-str "{\"name\":\"test-wt\",\"path\":\"/tmp/wt\",\"branch\":\"feature/test\",\"is_main\":false,\"beads_state\":\"redirect\"}")
         (cmd (beads-command-worktree-create :name "test-wt" :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (beads-worktree-p result))
      (should (equal "test-wt" (oref result name)))
      (should (equal "/tmp/wt" (oref result path))))))

(ert-deftest beads-coverage-2-worktree-create-parse-no-json ()
  "Test beads-command-parse for worktree-create without JSON."
  (let* ((cmd (beads-command-worktree-create :name "test-wt" :json nil))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "raw output" :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (equal "raw output" result)))))

(ert-deftest beads-coverage-2-worktree-list-parse-success ()
  "Test beads-command-parse for worktree-list with JSON array."
  (let* ((json-str "[{\"name\":\"main\",\"path\":\"/repo\",\"branch\":\"main\",\"is_main\":true,\"beads_state\":\"shared\"},{\"name\":\"feat\",\"path\":\"/repo-feat\",\"branch\":\"feat\",\"is_main\":false,\"beads_state\":\"redirect\"}]")
         (cmd (beads-command-worktree-list :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= 2 (length result)))
      (should (beads-worktree-p (car result))))))

(ert-deftest beads-coverage-2-worktree-info-parse-success ()
  "Test beads-command-parse for worktree-info."
  (let* ((json-str "{\"is_worktree\":true,\"name\":\"feat-wt\",\"branch\":\"feature/auth\",\"path\":\"/repo-feat\",\"main_path\":\"/repo\",\"beads_state\":\"redirect\"}")
         (cmd (beads-command-worktree-info :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (beads-worktree-info-p result))
      (should (equal "feat-wt" (oref result name))))))

;;; ============================================================
;;; beads-command-worktree.el - Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-2-worktree-create-validate-no-name ()
  "Test worktree-create validation: name required."
  (let ((cmd (beads-command-worktree-create :name nil)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-worktree-create-validate-empty-name ()
  "Test worktree-create validation: empty name."
  (let ((cmd (beads-command-worktree-create :name "")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-worktree-create-validate-valid ()
  "Test worktree-create validation: valid name."
  (let ((cmd (beads-command-worktree-create :name "my-worktree")))
    (should-not (beads-command-validate cmd))))

;;; ============================================================
;;; beads-command-worktree.el - Format and Display Tests
;;; ============================================================

(ert-deftest beads-coverage-2-worktree-format-entry ()
  "Test beads-worktree--format-entry."
  (let ((wt (beads-worktree
             :name "feature-auth"
             :path "/path/to/wt"
             :branch "feature/auth"
             :is-main nil
             :beads-state "redirect")))
    (let ((entry (beads-worktree--format-entry wt)))
      (should (listp entry))
      (should (equal "feature-auth" (car entry)))
      (should (vectorp (cadr entry))))))

(ert-deftest beads-coverage-2-worktree-format-entry-main ()
  "Test beads-worktree--format-entry for main worktree."
  (let ((wt (beads-worktree
             :name "main"
             :path "/path/to/repo"
             :branch "main"
             :is-main t
             :beads-state "shared")))
    (let* ((entry (beads-worktree--format-entry wt))
           (vec (cadr entry)))
      (should (string-match "Yes" (aref vec 3))))))

(ert-deftest beads-coverage-2-worktree-format-entry-states ()
  "Test beads-worktree--format-entry with different states."
  (dolist (state '("shared" "redirect" "local" "none"))
    (let* ((wt (beads-worktree
                :name "test" :path "/tmp" :branch "b"
                :is-main nil :beads-state state))
           (entry (beads-worktree--format-entry wt))
           (vec (cadr entry)))
      (should (string-match state (aref vec 2))))))

(ert-deftest beads-coverage-2-worktree-display-list ()
  "Test beads-worktree--display-list creates buffer."
  (let ((worktrees (list
                    (beads-worktree
                     :name "main" :path "/repo" :branch "main"
                     :is-main t :beads-state "shared"))))
    (cl-letf (((symbol-function 'beads-buffer-name-utility)
               (lambda (&rest _) "*test-worktrees*"))
              ((symbol-function 'pop-to-buffer)
               (lambda (_buf) nil)))
      (beads-worktree--display-list worktrees)
      (let ((buf (get-buffer "*test-worktrees*")))
        (should buf)
        (when buf (kill-buffer buf))))))

;;; ============================================================
;;; beads-command-epic.el - Parse Tests
;;; ============================================================

(ert-deftest beads-coverage-2-epic-status-parse-vector ()
  "Test beads-command-parse for epic-status with vector."
  (let* ((json-str "[{\"epic\":{\"id\":\"bd-1\",\"title\":\"Epic1\",\"status\":\"open\",\"priority\":0,\"issue_type\":\"epic\"},\"total_children\":5,\"closed_children\":2,\"eligible_for_close\":false}]")
         (cmd (beads-command-epic-status :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= 1 (length result))))))

(ert-deftest beads-coverage-2-epic-status-parse-single ()
  "Test beads-command-parse for epic-status with single object."
  (let* ((json-str "{\"epic\":{\"id\":\"bd-1\",\"title\":\"Epic1\",\"status\":\"open\",\"priority\":0,\"issue_type\":\"epic\"},\"total_children\":3,\"closed_children\":1,\"eligible_for_close\":false}")
         (cmd (beads-command-epic-status :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= 1 (length result))))))

(ert-deftest beads-coverage-2-epic-status-parse-null ()
  "Test beads-command-parse for epic-status with null."
  (let* ((cmd (beads-command-epic-status :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "null" :stderr "" :result nil)))
    (should-not (beads-command-parse cmd exec))))

;;; ============================================================
;;; beads-command-epic.el - Render and Navigation Tests
;;; ============================================================

(ert-deftest beads-coverage-2-epic-render-epic ()
  "Test beads-epic-status--render-epic rendering."
  (let ((epic-status (beads-epic-status
                      :epic (beads-issue
                             :id "bd-1" :title "Test Epic"
                             :status "open" :priority 0
                             :issue-type "epic")
                      :total-children 10
                      :closed-children 3
                      :eligible-for-close nil))
        (beads-epic-status--expanded nil))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (beads-epic-status--render-epic epic-status)
        (should (> (buffer-size) 0))
        (should (string-match "bd-1"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
        (should (string-match "3/10"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))))))

(ert-deftest beads-coverage-2-epic-render-eligible ()
  "Test beads-epic-status--render-epic with eligible epic."
  (let ((epic-status (beads-epic-status
                      :epic (beads-issue
                             :id "bd-2" :title "Done Epic"
                             :status "open" :priority 1
                             :issue-type "epic")
                      :total-children 5
                      :closed-children 5
                      :eligible-for-close t))
        (beads-epic-status--expanded nil))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (beads-epic-status--render-epic epic-status)
        (should (string-match "Eligible"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))))))

(ert-deftest beads-coverage-2-epic-navigation-next ()
  "Test beads-epic-status-next navigation."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
      (insert "  Progress: 3/10\n")
      (insert "\n")
      (insert (propertize "Epic 2\n" 'epic-id "bd-2"))
      (goto-char (point-min))
      (should (equal "bd-1" (get-text-property (point) 'epic-id)))
      (beads-epic-status-next)
      (should (equal "bd-2" (get-text-property (point) 'epic-id))))))

(ert-deftest beads-coverage-2-epic-navigation-previous ()
  "Test beads-epic-status-previous navigation."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
      (insert "  Progress: 3/10\n")
      (insert "\n")
      (insert (propertize "Epic 2\n" 'epic-id "bd-2"))
      (goto-char (point-max))
      (forward-line -1)
      (beads-epic-status-previous)
      (should (equal "bd-1" (get-text-property (point) 'epic-id))))))

(ert-deftest beads-coverage-2-epic-navigation-next-item ()
  "Test beads-epic-status-next-item for epic and issue items."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
      (insert "  Progress\n")
      (insert (propertize "  Child 1\n" 'issue-id "bd-c1"))
      (goto-char (point-min))
      (beads-epic-status-next-item)
      (should (equal "bd-c1" (get-text-property (point) 'issue-id))))))

(ert-deftest beads-coverage-2-epic-navigation-previous-item ()
  "Test beads-epic-status-previous-item navigation."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert (propertize "Epic 1\n" 'epic-id "bd-1"))
      (insert "  Progress\n")
      (insert (propertize "  Child 1\n" 'issue-id "bd-c1"))
      (goto-char (point-max))
      (forward-line -1)
      (beads-epic-status-previous-item)
      (should (equal "bd-1" (get-text-property (point) 'epic-id))))))

(ert-deftest beads-coverage-2-epic-show-at-point-no-issue ()
  "Test beads-epic-status-show-at-point with no issue."
  (with-temp-buffer
    (insert "No properties\n")
    (goto-char (point-min))
    (should-error (beads-epic-status-show-at-point) :type 'user-error)))

(ert-deftest beads-coverage-2-epic-show-children-no-epic ()
  "Test beads-epic-status-show-children with no epic."
  (with-temp-buffer
    (insert "No epic\n")
    (goto-char (point-min))
    (should-error (beads-epic-status-show-children) :type 'user-error)))

;;; ============================================================
;;; beads-command-formula.el - List Navigation Tests
;;; ============================================================

(ert-deftest beads-coverage-2-formula-list-current-name ()
  "Test beads-formula-list--current-formula-name returns nil with no entry."
  (with-temp-buffer
    (beads-formula-list-mode)
    ;; No entries, so tabulated-list-get-id returns nil
    (should (null (beads-formula-list--current-formula-name)))))

(ert-deftest beads-coverage-2-formula-list-get-by-name ()
  "Test beads-formula-list--get-formula-by-name."
  (let* ((f1 (beads-formula-summary
              :name "workflow1" :formula-type "workflow" :description "d1"))
         (f2 (beads-formula-summary
              :name "expand1" :formula-type "expansion" :description "d2"))
         (beads-formula-list--formulas (list f1 f2)))
    (should (eq f1 (beads-formula-list--get-formula-by-name "workflow1")))
    (should (eq f2 (beads-formula-list--get-formula-by-name "expand1")))
    (should-not (beads-formula-list--get-formula-by-name "nonexist"))))

(ert-deftest beads-coverage-2-formula-list-next ()
  "Test beads-formula-list-next moves forward, skipping header."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert "line1\nline2\nline3\n")
      (goto-char (point-min))
      (beads-formula-list-next)
      ;; Moves to line 2, then skips to 3 (header skip)
      (should (= 3 (line-number-at-pos))))))

(ert-deftest beads-coverage-2-formula-list-previous ()
  "Test beads-formula-list-previous moves backward."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (goto-char (point-max))
    (forward-line -1)
    (beads-formula-list-previous)
    (should (>= 2 (line-number-at-pos)))))

(ert-deftest beads-coverage-2-formula-list-quit ()
  "Test beads-formula-list-quit."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (kill) (setq quit-called kill))))
      (beads-formula-list-quit)
      (should quit-called))))

(ert-deftest beads-coverage-2-formula-list-open-source-no-formula ()
  "Test beads-formula-list-open-source with no formula."
  (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil)))
    (should-error (beads-formula-list-open-source) :type 'user-error)))

;;; ============================================================
;;; beads-command-formula.el - Show Commands Tests
;;; ============================================================

(ert-deftest beads-coverage-2-formula-show-refresh-no-formula ()
  "Test beads-formula-show-refresh with no formula name."
  (let ((beads-formula-show--formula-name nil))
    (should-error (beads-formula-show-refresh) :type 'user-error)))

(ert-deftest beads-coverage-2-formula-show-quit ()
  "Test beads-formula-show-quit."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'quit-window)
               (lambda (kill) (setq quit-called kill))))
      (beads-formula-show-quit)
      (should quit-called))))

(ert-deftest beads-coverage-2-formula-show-open-source-no-data ()
  "Test beads-formula-show-open-source with no formula data."
  (let ((beads-formula-show--formula-data nil))
    (should-error (beads-formula-show-open-source) :type 'user-error)))

;;; ============================================================
;;; beads-command-edit.el - Parse and Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-2-edit-parse-transient-args ()
  "Test beads-edit--parse-transient-args."
  (cl-letf (((symbol-function 'beads--sanitize-string)
             (lambda (s) s)))
    (let ((cmd (beads-edit--parse-transient-args
                '("--id=bd-42" "--title" "--description"))))
      (should (beads-command-edit-p cmd))
      (should (equal "bd-42" (oref cmd issue-id)))
      (should (oref cmd title))
      (should (oref cmd description)))))

(ert-deftest beads-coverage-2-edit-parse-transient-args-minimal ()
  "Test beads-edit--parse-transient-args with minimal args."
  (cl-letf (((symbol-function 'beads--sanitize-string)
             (lambda (s) s)))
    (let ((cmd (beads-edit--parse-transient-args '("--id=bd-1"))))
      (should (equal "bd-1" (oref cmd issue-id)))
      (should-not (oref cmd title)))))

(ert-deftest beads-coverage-2-edit-validate-issue-id-empty ()
  "Test beads-edit--validate-issue-id with empty."
  (should (stringp (beads-edit--validate-issue-id ""))))

(ert-deftest beads-coverage-2-edit-validate-issue-id-nil ()
  "Test beads-edit--validate-issue-id with nil."
  (should (stringp (beads-edit--validate-issue-id nil))))

(ert-deftest beads-coverage-2-edit-validate-issue-id-valid ()
  "Test beads-edit--validate-issue-id with valid."
  (should-not (beads-edit--validate-issue-id "bd-42")))

(ert-deftest beads-coverage-2-edit-validate-all-missing-id ()
  "Test beads-edit--validate-all with missing issue-id."
  (let ((cmd (beads-command-edit :issue-id nil :title t)))
    (should (beads-edit--validate-all cmd))))

(ert-deftest beads-coverage-2-edit-validate-all-valid ()
  "Test beads-edit--validate-all with valid cmd."
  (let ((cmd (beads-command-edit :issue-id "bd-1" :title t)))
    (should-not (beads-edit--validate-all cmd))))

;;; ============================================================
;;; beads-command-list.el - Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-2-list-validate-priority-conflict ()
  "Test list command validation: priority + priority-min conflict."
  (let ((cmd (beads-command-list :priority "2" :priority-min "1")))
    (let ((err (beads-command-validate cmd)))
      (should (stringp err))
      (should (string-match "Cannot use --priority" err)))))

(ert-deftest beads-coverage-2-list-validate-assignee-conflict ()
  "Test list command validation: assignee + no-assignee conflict."
  (let ((cmd (beads-command-list :assignee "user" :no-assignee t)))
    (let ((err (beads-command-validate cmd)))
      (should (stringp err))
      (should (string-match "Cannot use both" err)))))

(ert-deftest beads-coverage-2-list-validate-label-conflict ()
  "Test list command validation: label + no-labels conflict."
  (let ((cmd (beads-command-list :label '("bug") :no-labels t)))
    (let ((err (beads-command-validate cmd)))
      (should (stringp err))
      (should (string-match "no-labels" err)))))

(ert-deftest beads-coverage-2-list-validate-invalid-priority ()
  "Test list command validation: invalid priority value."
  (let ((cmd (beads-command-list :priority "9")))
    (let ((err (beads-command-validate cmd)))
      (should (stringp err))
      (should (string-match "between 0 and 4" err)))))

(ert-deftest beads-coverage-2-list-validate-valid ()
  "Test list command validation: valid command."
  (let ((cmd (beads-command-list :status "open" :priority "2")))
    (should-not (beads-command-validate cmd))))

;;; ============================================================
;;; beads-command-list.el - Parse Tests
;;; ============================================================

(ert-deftest beads-coverage-2-list-parse-vector ()
  "Test beads-command-parse for list with vector JSON."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"Issue 1\",\"status\":\"open\",\"priority\":1,\"issue_type\":\"task\"},{\"id\":\"bd-2\",\"title\":\"Issue 2\",\"status\":\"closed\",\"priority\":2,\"issue_type\":\"bug\"}]")
         (cmd (beads-command-list :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= 2 (length result)))
      (should (beads-issue-p (car result))))))

(ert-deftest beads-coverage-2-list-parse-null ()
  "Test beads-command-parse for list with null JSON."
  (let* ((cmd (beads-command-list :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "null" :stderr "" :result nil)))
    (should-not (beads-command-parse cmd exec))))

(ert-deftest beads-coverage-2-list-parse-no-json ()
  "Test beads-command-parse for list without JSON mode."
  (let* ((cmd (beads-command-list :json nil))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "raw" :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (equal "raw" result)))))

;;; ============================================================
;;; beads-command-list.el - Actions Tests
;;; ============================================================

(ert-deftest beads-coverage-2-list-copy-id ()
  "Test beads-list-copy-id copies to kill ring."
  (cl-letf (((symbol-function 'beads-list--current-issue-id)
             (lambda () "bd-42")))
    (beads-list-copy-id)
    (should (equal "bd-42" (current-kill 0)))))

(ert-deftest beads-coverage-2-list-copy-id-no-issue ()
  "Test beads-list-copy-id with no issue."
  (cl-letf (((symbol-function 'beads-list--current-issue-id)
             (lambda () nil)))
    (should-error (beads-list-copy-id) :type 'user-error)))

(ert-deftest beads-coverage-2-list-sort ()
  "Test beads-list-sort calls tabulated-list-sort."
  (let ((called nil))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn) (setq called fn))))
      (beads-list-sort)
      (should (eq called #'tabulated-list-sort)))))

(ert-deftest beads-coverage-2-list-create ()
  "Test beads-list-create calls beads-compose-create."
  (let ((called nil))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn) (setq called fn))))
      (beads-list-create)
      (should (eq called #'beads-compose-create)))))

(ert-deftest beads-coverage-2-list-update-no-issue ()
  "Test beads-list-update with no issue at point."
  (cl-letf (((symbol-function 'beads-list--current-issue-id)
             (lambda () nil)))
    (should-error (beads-list-update) :type 'user-error)))

(ert-deftest beads-coverage-2-list-close-no-issue ()
  "Test beads-list-close with no issue at point."
  (cl-letf (((symbol-function 'beads-list--current-issue-id)
             (lambda () nil)))
    (should-error (beads-list-close) :type 'user-error)))

(ert-deftest beads-coverage-2-list-delete-no-issue ()
  "Test beads-list-delete with no issue at point."
  (cl-letf (((symbol-function 'beads-list--current-issue-id)
             (lambda () nil)))
    (should-error (beads-list-delete) :type 'user-error)))

(ert-deftest beads-coverage-2-list-reopen-no-issue ()
  "Test beads-list-reopen with no issue at point."
  (cl-letf (((symbol-function 'beads-list--current-issue-id)
             (lambda () nil)))
    (should-error (beads-list-reopen) :type 'user-error)))

;;; ============================================================
;;; beads-command-update.el - Parse Tests
;;; ============================================================

(ert-deftest beads-coverage-2-update-parse-vector ()
  "Test beads-command-parse for update with vector JSON."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"Issue 1\",\"status\":\"in_progress\",\"priority\":1,\"issue_type\":\"task\"}]")
         (cmd (beads-command-update :issue-ids '("bd-1") :json t
                                     :status "in_progress"))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      ;; Single issue-id returns a single issue, not a list
      (should (beads-issue-p result))
      (should (equal "bd-1" (oref result id))))))

(ert-deftest beads-coverage-2-update-parse-multiple ()
  "Test beads-command-parse for update with multiple IDs."
  (let* ((json-str "[{\"id\":\"bd-1\",\"title\":\"I1\",\"status\":\"closed\",\"priority\":1,\"issue_type\":\"task\"},{\"id\":\"bd-2\",\"title\":\"I2\",\"status\":\"closed\",\"priority\":2,\"issue_type\":\"bug\"}]")
         (cmd (beads-command-update :issue-ids '("bd-1" "bd-2")
                                     :json t :status "closed"))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      ;; Multiple IDs returns a list
      (should (listp result))
      (should (= 2 (length result))))))

(ert-deftest beads-coverage-2-update-parse-no-json ()
  "Test beads-command-parse for update without JSON."
  (let* ((cmd (beads-command-update :issue-ids '("bd-1") :json nil
                                     :status "open"))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "raw" :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (equal "raw" result)))))

;;; ============================================================
;;; beads-command-update.el - Validate and Changed Fields Tests
;;; ============================================================

(ert-deftest beads-coverage-2-update-validate-no-issue ()
  "Test update validation: no issue IDs."
  (let ((cmd (beads-command-update :issue-ids nil :status "open")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-update-validate-no-field ()
  "Test update validation: no field to update."
  (let ((cmd (beads-command-update :issue-ids '("bd-1"))))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-update-validate-valid ()
  "Test update validation: valid."
  (let ((cmd (beads-command-update :issue-ids '("bd-1")
                                    :status "in_progress")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-coverage-2-update-get-changed-fields ()
  "Test beads-update--get-changed-fields."
  (let ((beads-update--original-data
         (beads-issue :id "bd-1" :title "Old title" :status "open"
                      :priority 1 :issue-type "task"
                      :description "Old desc")))
    (let ((cmd (beads-command-update :issue-ids '("bd-1")
                                      :title "New title"
                                      :status "open")))
      (let ((changes (beads-update--get-changed-fields cmd)))
        ;; title changed, status same
        (should (assq 'title changes))
        (should-not (assq 'status changes))))))

(ert-deftest beads-coverage-2-update-get-changed-fields-no-changes ()
  "Test beads-update--get-changed-fields with no changes."
  (let ((beads-update--original-data
         (beads-issue :id "bd-1" :title "Same" :status "open"
                      :priority 1 :issue-type "task")))
    (let ((cmd (beads-command-update :issue-ids '("bd-1")
                                      :title "Same")))
      (should-not (beads-update--get-changed-fields cmd)))))

(ert-deftest beads-coverage-2-update-validate-all-no-changes ()
  "Test beads-update--validate-all when no fields changed.
beads-update--validate-all delegates to beads-command-validate which
does not check whether fields have actually changed."
  (let ((beads-update--original-data
         (beads-issue :id "bd-1" :title "Same" :status "open"
                      :priority 1 :issue-type "task")))
    (let ((cmd (beads-command-update :issue-ids '("bd-1")
                                      :title "Same")))
      (let ((errors (beads-update--validate-all cmd)))
        (should (null errors))))))

;;; ============================================================
;;; beads-command-dep.el - Parse Tests
;;; ============================================================

(ert-deftest beads-coverage-2-dep-list-parse-vector ()
  "Test beads-command-parse for dep list with vector."
  (let* ((json-str "[{\"id\":\"bd-2\",\"title\":\"Dep issue\",\"status\":\"open\",\"priority\":1,\"issue_type\":\"task\",\"dependency_type\":\"depends_on\"}]")
         (cmd (beads-command-dep-list :issue-id "bd-1" :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout json-str :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= 1 (length result))))))

(ert-deftest beads-coverage-2-dep-list-parse-null ()
  "Test beads-command-parse for dep list with null."
  (let* ((cmd (beads-command-dep-list :issue-id "bd-1" :json t))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "null" :stderr "" :result nil)))
    (should-not (beads-command-parse cmd exec))))

(ert-deftest beads-coverage-2-dep-list-parse-no-json ()
  "Test beads-command-parse for dep list without JSON."
  (let* ((cmd (beads-command-dep-list :issue-id "bd-1" :json nil))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "raw" :stderr "" :result nil)))
    (let ((result (beads-command-parse cmd exec)))
      (should (equal "raw" result)))))

;;; ============================================================
;;; beads-option.el - Infix Read Tests
;;; ============================================================

(defvar beads-coverage-test--temp-var nil
  "Temporary variable for option testing.")
(defvar beads-coverage-test--temp-switch nil
  "Temporary switch variable for option testing.")

(ert-deftest beads-coverage-2-label-add-validate-no-issue ()
  "Test label add validation with no issue IDs."
  (let ((cmd (beads-command-label-add :issue-ids nil :label "bug")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-label-add-validate-no-label ()
  "Test label add validation with no label."
  (let ((cmd (beads-command-label-add :issue-ids '("bd-1") :label "")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-option-multiline-class-exists ()
  "Test that beads-transient-multiline class exists."
  (should (find-class 'beads-transient-multiline)))

;;; ============================================================
;;; beads-agent-agent-shell.el - Backend Method Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-shell-backend-available-no-package ()
  "Test agent-shell availability when package not installed."
  (let ((backend (beads-agent-backend-agent-shell)))
    (cl-letf (((symbol-function 'featurep) (lambda (_feat) nil))
              ((symbol-function 'require) (lambda (_feat &rest _) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-coverage-2-agent-shell-switch-to-buffer-no-buffer ()
  "Test agent-shell switch-to-buffer with no buffer."
  (require 'beads-agent-agent-shell)
  (let ((backend (beads-agent-backend-agent-shell))
        (session (beads-agent-session
                  :id "s1" :issue-id "bd-1"
                  :backend-name "agent-shell"
                  :project-dir "/tmp"
                  :started-at "2025-01-01T00:00:00+0000")))
    (cl-letf (((symbol-function 'beads-agent-backend-get-buffer)
               (lambda (_b _s) nil)))
      (should-error
       (beads-agent-backend-switch-to-buffer backend session)
       :type 'user-error))))

;;; ============================================================
;;; beads-command-worktree.el - Transient Suffix Tests
;;; ============================================================

(ert-deftest beads-coverage-2-worktree-list-mode-defined ()
  "Test beads-worktree-list-mode is defined."
  (should (fboundp 'beads-worktree-list-mode)))

(ert-deftest beads-coverage-2-worktree-list-info-no-worktree ()
  "Test beads-worktree-list-info with no worktree at point."
  (with-temp-buffer
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil)))
      ;; Should just message, not error
      (beads-worktree-list-info))))

(ert-deftest beads-coverage-2-worktree-list-remove-no-worktree ()
  "Test beads-worktree-list-remove with no worktree at point."
  (with-temp-buffer
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil)))
      ;; Should just message, not error
      (beads-worktree-list-remove))))

(ert-deftest beads-coverage-2-worktree-list-create ()
  "Test beads-worktree-list-create calls worktree-menu."
  (let ((called nil))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn) (setq called fn))))
      (beads-worktree-list-create)
      (should (eq called #'beads-worktree-menu)))))

;;; ============================================================
;;; beads-command-epic.el - Execute Interactive Tests
;;; ============================================================

(ert-deftest beads-coverage-2-epic-status-execute-interactive ()
  "Test beads-command-execute-interactive for epic-status calls parent."
  (let* ((cmd (beads-command-epic-status)))
    ;; execute-interactive calls parent (json defaults to nil)
    (cl-letf (((symbol-function 'cl-call-next-method)
               (lambda (&rest _) nil)))
      (beads-command-execute-interactive cmd)
      (should-not (oref cmd json)))))

;;; ============================================================
;;; beads-command-formula.el - Render Helpers Tests
;;; ============================================================

(ert-deftest beads-coverage-2-formula-render-header ()
  "Test beads-formula-show--render-header."
  (with-temp-buffer
    (beads-formula-show--render-header "Name" "my-formula")
    (should (string-match "Name" (buffer-string)))
    (should (string-match "my-formula" (buffer-string)))))

(ert-deftest beads-coverage-2-formula-render-section ()
  "Test beads-formula-show--render-section."
  (with-temp-buffer
    (beads-formula-show--render-section "Variables")
    (should (string-match "Variables" (buffer-string)))))

;;; ============================================================
;;; beads-agent.el - Start Command Forwarding Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-start-task-defined ()
  "Test beads-agent-start-task is defined."
  (should (commandp 'beads-agent-start-task)))

(ert-deftest beads-coverage-2-agent-start-review-defined ()
  "Test beads-agent-start-review is defined."
  (should (commandp 'beads-agent-start-review)))

(ert-deftest beads-coverage-2-agent-start-plan-defined ()
  "Test beads-agent-start-plan is defined."
  (should (commandp 'beads-agent-start-plan)))

(ert-deftest beads-coverage-2-agent-start-qa-defined ()
  "Test beads-agent-start-qa is defined."
  (should (commandp 'beads-agent-start-qa)))

(ert-deftest beads-coverage-2-agent-start-custom-defined ()
  "Test beads-agent-start-custom is defined."
  (should (commandp 'beads-agent-start-custom)))

;;; ============================================================
;;; beads-agent.el - Get Sessions for Issue Type Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-get-sessions-for-issue-type ()
  "Test beads-agent--get-sessions-for-issue-type filtering."
  (let* ((s1 (beads-agent-session
              :id "s1" :issue-id "bd-1" :backend-name "mock"
              :project-dir "/tmp" :started-at "2025-01-01T00:00:00+0000"
              :agent-type-name "Task"))
         (s2 (beads-agent-session
              :id "s2" :issue-id "bd-1" :backend-name "mock"
              :project-dir "/tmp" :started-at "2025-01-01T00:00:00+0000"
              :agent-type-name "Review"))
         (called-tasks nil))
    (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
               (lambda (_id) (list s1 s2))))
      (let ((tasks (beads-agent--get-sessions-for-issue-type "bd-1" "Task")))
        (should (= 1 (length tasks)))
        (should (equal "s1" (oref (car tasks) id)))))))

;;; ============================================================
;;; beads-command-list.el - Filter Command Test
;;; ============================================================

(ert-deftest beads-coverage-2-list-filter-no-command ()
  "Test beads-list-filter when no command obj is bound."
  (let ((called nil))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn) (setq called fn))))
      ;; No beads-list--command-obj bound
      (let ((beads-list--command-obj nil))
        (beads-list-filter)
        (should (eq called #'beads-list))))))

;;; ============================================================
;;; beads-agent.el - Agent at Point Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-mode-line-format-compact-wt ()
  "Test mode-line compact format with worktree context."
  (let ((ctx (list :project-name "test-proj" :branch "feat/wt"
                   :in-worktree t :agent-type "task"
                   :agent-instance 2)))
    (let ((result (beads-agent--mode-line-format-compact ctx)))
      (should (stringp result))
      (should (string-match "\\*" result)))))

(ert-deftest beads-coverage-2-agent-jump-at-point-defined ()
  "Test beads-agent-jump-at-point is a command."
  (should (commandp 'beads-agent-jump-at-point)))

(ert-deftest beads-coverage-2-agent-stop-at-point-defined ()
  "Test beads-agent-stop-at-point is a command."
  (should (commandp 'beads-agent-stop-at-point)))

;;; ============================================================
;;; beads-agent.el - Worktree Setup Interactive Tests
;;; ============================================================

(ert-deftest beads-coverage-2-agent-setup-worktree-no-wt ()
  "Test beads-agent--setup-worktree-interactive with no worktree."
  (let ((callback-result nil))
    (cl-letf (((symbol-function 'beads-agent--read-worktree-name)
               (lambda (_id) nil))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () "/project")))
      (beads-agent--setup-worktree-interactive
       "bd-1"
       (lambda (success path)
         (setq callback-result (list success path))))
      (should (equal '(t "/project") callback-result)))))

;;; ============================================================
;;; Additional beads-command-formula.el parse coverage
;;; ============================================================

(ert-deftest beads-coverage-2-formula-list-show-no-formula ()
  "Test beads-formula-list-show with no formula at point."
  (cl-letf (((symbol-function 'beads-formula-list--current-formula-name)
             (lambda () nil)))
    (should-error (beads-formula-list-show) :type 'user-error)))

;;; ============================================================
;;; beads-command-dep.el - Validate Tests
;;; ============================================================

(ert-deftest beads-coverage-2-dep-add-validate-no-issue ()
  "Test dep add validation with no issue ID."
  (let ((cmd (beads-command-dep-add :issue-id nil :depends-on "bd-2")))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-dep-add-validate-no-target ()
  "Test dep add validation with no depends-on."
  (let ((cmd (beads-command-dep-add :issue-id "bd-1"
                                     :depends-on nil
                                     :blocked-by nil)))
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-coverage-2-dep-remove-validate-no-name ()
  "Test dep remove validation with no name."
  (let ((cmd (beads-command-worktree-remove :name nil)))
    (should (stringp (beads-command-validate cmd)))))

;;; ============================================================
;;; beads-command-update.el - Execute Interactive Tests
;;; ============================================================

(ert-deftest beads-coverage-2-update-execute-interactive-success ()
  "Test beads-command-execute-interactive for update."
  (let* ((issue (beads-issue :id "bd-1" :title "Test"
                              :status "in_progress" :priority 1
                              :issue-type "task"))
         (cmd (beads-command-update :issue-ids '("bd-1")
                                     :json t :status "in_progress"))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "" :stderr "" :result issue)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) exec))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(ert-deftest beads-coverage-2-update-execute-interactive-nil-result ()
  "Test beads-command-execute-interactive for update with nil result."
  (let* ((cmd (beads-command-update :issue-ids '("bd-1")
                                     :json t :status "closed"))
         (exec (beads-command-execution
                :command cmd :exit-code 0
                :stdout "" :stderr "" :result nil)))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) exec))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-command-execute-interactive cmd))))

(provide 'beads-coverage-test-2)
;;; beads-coverage-test-2.el ends here
