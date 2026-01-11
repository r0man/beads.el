;;; beads-agent-list-test.el --- Tests for beads-agent-list -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for beads-agent-list.el

;;; Code:

(require 'ert)
(require 'beads-agent-list)
(require 'beads-agent-backend)
(require 'beads-buffer)
(require 'beads-types)

;;; Test Helpers

(defun beads-agent-list-test--get-agents-buffer ()
  "Get the agents buffer for the current project."
  (beads-buffer-name-utility "agents"))

(defun beads-agent-list-test--find-and-kill-agents-buffers ()
  "Kill all agent list buffers for cleanup."
  (dolist (buf (beads-buffer-name-find-utility-buffers nil "agents"))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

;;; Test Data

(defvar beads-agent-list-test--mock-sessions nil
  "Mock sessions for testing.")

(defun beads-agent-list-test--make-session (id issue-id backend-name
                                               &optional worktree-dir)
  "Create a mock beads-agent-session for testing.
ID is the session ID.
ISSUE-ID is the associated issue.
BACKEND-NAME is the backend name.
WORKTREE-DIR is optional worktree directory."
  (beads-agent-session
   :id id
   :issue-id issue-id
   :backend-name backend-name
   :project-dir "/home/test/project"
   :worktree-dir worktree-dir
   :started-at (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                   (time-subtract (current-time)
                                                  (seconds-to-time 3700)))
   :backend-session nil))

;;; Utility Function Tests

(ert-deftest beads-agent-list-test-format-duration-seconds ()
  "Test duration formatting for seconds."
  (let ((now (current-time)))
    (cl-letf (((symbol-function 'current-time)
               (lambda () now)))
      (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                           (time-subtract now (seconds-to-time 45)))))
        (should (equal "45s" (beads-agent-list--format-duration timestamp)))))))

(ert-deftest beads-agent-list-test-format-duration-minutes ()
  "Test duration formatting for minutes."
  (let ((now (current-time)))
    (cl-letf (((symbol-function 'current-time)
               (lambda () now)))
      (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                           (time-subtract now (seconds-to-time 300)))))
        (should (equal "5m" (beads-agent-list--format-duration timestamp)))))))

(ert-deftest beads-agent-list-test-format-duration-hours ()
  "Test duration formatting for hours."
  (let ((now (current-time)))
    (cl-letf (((symbol-function 'current-time)
               (lambda () now)))
      (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                           (time-subtract now (seconds-to-time 7500)))))
        (should (equal "2h 5m" (beads-agent-list--format-duration timestamp)))))))

(ert-deftest beads-agent-list-test-format-duration-days ()
  "Test duration formatting for days."
  (let ((now (current-time)))
    (cl-letf (((symbol-function 'current-time)
               (lambda () now)))
      (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                           (time-subtract now (seconds-to-time 180000)))))
        (should (equal "2d 2h" (beads-agent-list--format-duration timestamp)))))))

(ert-deftest beads-agent-list-test-format-duration-empty ()
  "Test duration formatting with empty or nil input."
  (should (equal "" (beads-agent-list--format-duration nil)))
  (should (equal "" (beads-agent-list--format-duration ""))))

(ert-deftest beads-agent-list-test-format-directory ()
  "Test directory formatting for sessions."
  (let ((session (beads-agent-list-test--make-session
                  "sess-1" "beads.el-42" "claude-code-ide")))
    (should (stringp (beads-agent-list--format-directory session)))))

(ert-deftest beads-agent-list-test-format-directory-worktree ()
  "Test directory formatting prefers worktree when set."
  (let ((session (beads-agent-list-test--make-session
                  "sess-1" "beads.el-42" "claude-code-ide"
                  "/home/test/beads.el-42")))
    (should (string-match-p "beads.el-42"
                            (beads-agent-list--format-directory session)))))

;;; Status Formatting Tests

(ert-deftest beads-agent-list-test-status-face-running ()
  "Test that running status gets correct face."
  (should (eq 'beads-agent-list-running
              (beads-agent-list--status-face 'running))))

(ert-deftest beads-agent-list-test-status-face-stale ()
  "Test that stale status gets correct face."
  (should (eq 'beads-agent-list-stale
              (beads-agent-list--status-face 'stale))))

(ert-deftest beads-agent-list-test-status-face-finished ()
  "Test that finished status gets correct face."
  (should (eq 'beads-agent-list-finished
              (beads-agent-list--status-face 'finished))))

(ert-deftest beads-agent-list-test-status-face-failed ()
  "Test that failed status gets correct face."
  (should (eq 'beads-agent-list-failed
              (beads-agent-list--status-face 'failed))))

;;; Session to Entry Conversion Tests

(ert-deftest beads-agent-list-test-session-to-entry ()
  "Test converting a session to a tabulated-list entry."
  (let ((session (beads-agent-list-test--make-session
                  "sess-1" "beads.el-42" "claude-code-ide"))
        (beads-agent-list--title-cache (make-hash-table :test #'equal)))
    (puthash "beads.el-42" "Test Issue" beads-agent-list--title-cache)
    (cl-letf (((symbol-function 'beads-agent--session-active-p)
               (lambda (_s) t))
              ((symbol-function 'beads-agent--get-issue-outcome)
               (lambda (_id) nil)))
      (let ((entry (beads-agent-list--session-to-entry session)))
        ;; Entry is (id [columns...])
        (should (equal "sess-1" (car entry)))
        (should (vectorp (cadr entry)))
        ;; Check columns
        (let ((vec (cadr entry)))
          (should (equal "beads.el-42" (aref vec 0)))  ; Issue ID
          (should (equal "Test Issue" (aref vec 1)))   ; Title
          (should (equal "claude-code-ide" (aref vec 2))))))))  ; Backend

;;; Buffer Creation Tests

(ert-deftest beads-agent-list-test-mode-setup ()
  "Test that beads-agent-list-mode sets up the buffer correctly."
  (with-temp-buffer
    (beads-agent-list-mode)
    (should (eq major-mode 'beads-agent-list-mode))
    (should tabulated-list-format)
    (should (= 6 (length tabulated-list-format)))))  ; 6 columns

(ert-deftest beads-agent-list-test-keymap ()
  "Test that keymap has expected bindings."
  (let ((map beads-agent-list-mode-map))
    (should (eq 'beads-agent-list-jump (lookup-key map (kbd "RET"))))
    (should (eq 'beads-agent-list-jump (lookup-key map (kbd "j"))))
    (should (eq 'beads-agent-list-show-issue (lookup-key map (kbd "i"))))
    (should (eq 'beads-agent-list-stop (lookup-key map (kbd "s"))))
    (should (eq 'beads-agent-list-stop-all (lookup-key map (kbd "S"))))
    (should (eq 'beads-agent-list-restart (lookup-key map (kbd "r"))))
    (should (eq 'beads-agent-list-refresh (lookup-key map (kbd "g"))))
    (should (eq 'beads-agent-list-quit (lookup-key map (kbd "q"))))
    (should (eq 'beads-agent-list-cleanup (lookup-key map (kbd "c"))))
    (should (eq 'beads-agent-list-next (lookup-key map (kbd "n"))))
    (should (eq 'beads-agent-list-previous (lookup-key map (kbd "p"))))
    (should (eq 'beads-agent-list-copy-session-id (lookup-key map (kbd "w"))))
    (should (eq 'beads-agent-list-copy-issue-id (lookup-key map (kbd "W"))))))

;;; Populate Buffer Tests

(ert-deftest beads-agent-list-test-populate-empty ()
  "Test populating buffer with no sessions."
  (with-temp-buffer
    (beads-agent-list-mode)
    (cl-letf (((symbol-function 'beads-agent--get-all-sessions)
               (lambda () nil))
              ((symbol-function 'beads-command-show!)
               (lambda (&rest _args) nil)))
      (beads-agent-list--populate-buffer)
      (should (null tabulated-list-entries)))))

(ert-deftest beads-agent-list-test-populate-with-sessions ()
  "Test populating buffer with multiple sessions."
  (with-temp-buffer
    (beads-agent-list-mode)
    (let ((sessions (list
                     (beads-agent-list-test--make-session
                      "sess-1" "beads.el-42" "claude-code-ide")
                     (beads-agent-list-test--make-session
                      "sess-2" "beads.el-43" "efrit"))))
      (cl-letf (((symbol-function 'beads-agent--get-all-sessions)
                 (lambda () sessions))
                ((symbol-function 'beads-command-show!)
                 (lambda (&rest _args)
                   (list (beads-issue :id "beads.el-42" :title "Issue 42")
                         (beads-issue :id "beads.el-43" :title "Issue 43"))))
                ((symbol-function 'beads-agent--session-active-p)
                 (lambda (_s) t))
                ((symbol-function 'beads-agent--get-issue-outcome)
                 (lambda (_id) nil)))
        (beads-agent-list--populate-buffer)
        (should (= 2 (length tabulated-list-entries)))))))

;;; Current Session/Issue Tests

(ert-deftest beads-agent-list-test-current-session-id-none ()
  "Test getting current session ID when none at point."
  (with-temp-buffer
    (beads-agent-list-mode)
    (should (null (beads-agent-list--current-session-id)))))

(ert-deftest beads-agent-list-test-current-issue-id-none ()
  "Test getting current issue ID when none at point."
  (with-temp-buffer
    (beads-agent-list-mode)
    (cl-letf (((symbol-function 'beads-agent--get-session)
               (lambda (_id) nil)))
      (should (null (beads-agent-list--current-issue-id))))))

;;; Command Error Handling Tests

(ert-deftest beads-agent-list-test-jump-no-session ()
  "Test that jump errors when no session at point."
  (with-temp-buffer
    (beads-agent-list-mode)
    (should-error (beads-agent-list-jump) :type 'user-error)))

(ert-deftest beads-agent-list-test-show-issue-no-session ()
  "Test that show-issue errors when no session at point."
  (with-temp-buffer
    (beads-agent-list-mode)
    (cl-letf (((symbol-function 'beads-agent--get-session)
               (lambda (_id) nil)))
      (should-error (beads-agent-list-show-issue) :type 'user-error))))

(ert-deftest beads-agent-list-test-stop-no-session ()
  "Test that stop errors when no session at point."
  (with-temp-buffer
    (beads-agent-list-mode)
    (should-error (beads-agent-list-stop) :type 'user-error)))

(ert-deftest beads-agent-list-test-copy-session-id-no-session ()
  "Test that copy-session-id errors when no session at point."
  (with-temp-buffer
    (beads-agent-list-mode)
    (should-error (beads-agent-list-copy-session-id) :type 'user-error)))

(ert-deftest beads-agent-list-test-copy-issue-id-no-session ()
  "Test that copy-issue-id errors when no session at point."
  (with-temp-buffer
    (beads-agent-list-mode)
    (cl-letf (((symbol-function 'beads-agent--get-session)
               (lambda (_id) nil)))
      (should-error (beads-agent-list-copy-issue-id) :type 'user-error))))

;;; Title Cache Tests

(ert-deftest beads-agent-list-test-fetch-titles-empty ()
  "Test fetching titles with empty list."
  (let ((cache (beads-agent-list--fetch-titles nil)))
    (should (hash-table-p cache))
    (should (= 0 (hash-table-count cache)))))

(ert-deftest beads-agent-list-test-fetch-titles-success ()
  "Test fetching titles successfully."
  (cl-letf (((symbol-function 'beads-command-show!)
             (lambda (&rest _args)
               (list (beads-issue :id "beads.el-42" :title "Test Issue")))))
    (let ((cache (beads-agent-list--fetch-titles '("beads.el-42"))))
      (should (hash-table-p cache))
      (should (equal "Test Issue" (gethash "beads.el-42" cache))))))

(ert-deftest beads-agent-list-test-fetch-titles-error ()
  "Test fetching titles handles errors gracefully."
  (cl-letf (((symbol-function 'beads-command-show!)
             (lambda (&rest _args)
               (error "Command failed"))))
    (let ((cache (beads-agent-list--fetch-titles '("beads.el-42"))))
      (should (hash-table-p cache))
      (should (= 0 (hash-table-count cache))))))

(ert-deftest beads-agent-list-test-get-title-from-cache ()
  "Test getting title from cache."
  (let ((beads-agent-list--title-cache (make-hash-table :test #'equal)))
    (puthash "beads.el-42" "Cached Title" beads-agent-list--title-cache)
    (should (equal "Cached Title" (beads-agent-list--get-title "beads.el-42")))))

(ert-deftest beads-agent-list-test-get-title-missing ()
  "Test getting title when not in cache."
  (let ((beads-agent-list--title-cache (make-hash-table :test #'equal)))
    (should (equal "" (beads-agent-list--get-title "beads.el-99")))))

(ert-deftest beads-agent-list-test-get-title-no-cache ()
  "Test getting title when cache is nil."
  (let ((beads-agent-list--title-cache nil))
    (should (equal "" (beads-agent-list--get-title "beads.el-42")))))

;;; Hook Tests

(ert-deftest beads-agent-list-test-hook-registered ()
  "Test that state change hook is registered."
  (should (memq 'beads-agent-list--on-state-change
                beads-agent-state-change-hook)))

(ert-deftest beads-agent-list-test-hook-refreshes-buffer ()
  "Test that hook refreshes the agent list buffer."
  (let ((refresh-called nil))
    (cl-letf (((symbol-function 'beads-agent-list--populate-buffer)
               (lambda () (setq refresh-called t)))
              ((symbol-function 'beads-agent--get-all-sessions)
               (lambda () nil)))
      (with-current-buffer (get-buffer-create (beads-agent-list-test--get-agents-buffer))
        (beads-agent-list-mode)
        (beads-agent-list--on-state-change 'started nil)
        (should refresh-called))
      (beads-agent-list-test--find-and-kill-agents-buffers))))

(ert-deftest beads-agent-list-test-hook-no-buffer ()
  "Test that hook handles missing buffer gracefully."
  (beads-agent-list-test--find-and-kill-agents-buffers)
  ;; Should not error
  (beads-agent-list--on-state-change 'started nil))

;;; Navigation Tests

(ert-deftest beads-agent-list-test-next-command ()
  "Test that next command is defined and callable."
  (with-temp-buffer
    (beads-agent-list-mode)
    ;; Insert some content to have lines to navigate
    (let ((inhibit-read-only t))
      (insert "line1\nline2\nline3\n"))
    (goto-char (point-min))
    ;; Should not error
    (beads-agent-list-next)
    (should t)))

(ert-deftest beads-agent-list-test-previous-command ()
  "Test that previous command is defined and callable."
  (with-temp-buffer
    (beads-agent-list-mode)
    ;; Insert some content to have lines to navigate
    (let ((inhibit-read-only t))
      (insert "line1\nline2\nline3\n"))
    (goto-char (point-max))
    ;; Should not error
    (beads-agent-list-previous)
    (should t)))

;;; Customization Tests

(ert-deftest beads-agent-list-test-customizations-exist ()
  "Test that all customization variables exist."
  (should (boundp 'beads-agent-list-issue-width))
  (should (boundp 'beads-agent-list-title-width))
  (should (boundp 'beads-agent-list-backend-width))
  (should (boundp 'beads-agent-list-status-width))
  (should (boundp 'beads-agent-list-duration-width))
  (should (boundp 'beads-agent-list-directory-width)))

(ert-deftest beads-agent-list-test-faces-exist ()
  "Test that all faces are defined."
  (should (facep 'beads-agent-list-running))
  (should (facep 'beads-agent-list-stale))
  (should (facep 'beads-agent-list-finished))
  (should (facep 'beads-agent-list-failed)))

(ert-deftest beads-agent-list-test-hl-line-mode-enabled ()
  "Test that hl-line-mode is enabled when entering beads-agent-list-mode."
  (with-temp-buffer
    (beads-agent-list-mode)
    (should (bound-and-true-p hl-line-mode))))

(provide 'beads-agent-list-test)

;;; beads-agent-list-test.el ends here
