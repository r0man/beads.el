;;; beads-daemons-test.el --- Tests for beads-daemons -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-daemons.el.
;; Tests cover:
;; - Multi-daemon command classes
;; - JSON parsing functions
;; - List buffer functionality
;; - Transient menu integration

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-daemons)
(require 'beads-test)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar beads-daemons-test--sample-list-json
  (vector
   '((WorkspacePath . "/home/user/project1")
     (DatabasePath . "/home/user/project1/.beads/issues.db")
     (SocketPath . "/home/user/project1/.beads/bd.sock")
     (PID . 12345)
     (Version . "0.28.0")
     (UptimeSeconds . 3600.5)
     (LastActivityTime . "2025-12-08T17:00:00Z")
     (ExclusiveLockActive . :json-false)
     (ExclusiveLockHolder . "")
     (Alive . t)
     (Error . ""))
   '((WorkspacePath . "/home/user/project2")
     (DatabasePath . "/home/user/project2/.beads/issues.db")
     (SocketPath . "/home/user/project2/.beads/bd.sock")
     (PID . 12346)
     (Version . "0.28.0")
     (UptimeSeconds . 7200.0)
     (LastActivityTime . "2025-12-08T16:30:00Z")
     (ExclusiveLockActive . t)
     (ExclusiveLockHolder . "agent1")
     (Alive . t)
     (Error . "")))
  "Sample daemons list JSON for testing.")

(defvar beads-daemons-test--sample-health-json
  '((total . 3)
    (healthy . 2)
    (stale . 1)
    (mismatched . 0)
    (unresponsive . 0)
    (daemons . [((workspace . "/home/user/project1")
                 (socket_path . "/home/user/project1/.beads/bd.sock")
                 (pid . 12345)
                 (version . "0.28.0")
                 (status . "healthy")
                 (issue . "")
                 (version_mismatch . :json-false))
                ((workspace . "/home/user/project2")
                 (socket_path . "/home/user/project2/.beads/bd.sock")
                 (pid . 12346)
                 (version . "0.28.0")
                 (status . "healthy")
                 (issue . "")
                 (version_mismatch . :json-false))
                ((workspace . "/home/user/project3")
                 (socket_path . "/home/user/project3/.beads/bd.sock")
                 (pid . 0)
                 (version . "")
                 (status . "stale")
                 (issue . "process not found")
                 (version_mismatch . :json-false))]))
  "Sample daemons health JSON for testing.")

(defvar beads-daemons-test--sample-stop-json
  '((workspace . "/home/user/project1")
    (pid . 12345)
    (stopped . t))
  "Sample daemons stop JSON for testing.")

(defvar beads-daemons-test--sample-restart-json
  '((workspace . "/home/user/project1")
    (action . "restarted"))
  "Sample daemons restart JSON for testing.")

(defvar beads-daemons-test--sample-logs-json
  '((workspace . "/home/user/project1")
    (log_path . "/home/user/project1/.beads/daemon.log")
    (content . "2025-12-08 Starting daemon...\n2025-12-08 Daemon started"))
  "Sample daemons logs JSON for testing.")

(defvar beads-daemons-test--sample-killall-json
  '((Stopped . 2)
    (Failed . 1)
    (Failures . [((Workspace . "/home/user/project3")
                  (PID . 12347)
                  (Error . "process not found"))]))
  "Sample daemons killall JSON for testing.")

;;; ============================================================
;;; Data Structure Tests
;;; ============================================================

(ert-deftest beads-daemons-test-daemon-info-creation ()
  "Test creating beads-daemon-info object."
  (let ((info (beads-daemon-info
               :workspace-path "/home/user/project"
               :pid 12345
               :version "0.28.0"
               :alive t)))
    (should (beads-daemon-info-p info))
    (should (equal (oref info workspace-path) "/home/user/project"))
    (should (equal (oref info pid) 12345))
    (should (equal (oref info version) "0.28.0"))
    (should (eq (oref info alive) t))))

(ert-deftest beads-daemons-test-health-report-creation ()
  "Test creating beads-daemons-health-report object."
  (let ((report (beads-daemons-health-report
                 :total 3
                 :healthy 2
                 :stale 1
                 :mismatched 0)))
    (should (beads-daemons-health-report-p report))
    (should (equal (oref report total) 3))
    (should (equal (oref report healthy) 2))
    (should (equal (oref report stale) 1))))

(ert-deftest beads-daemons-test-stop-result-creation ()
  "Test creating beads-daemons-stop-result object."
  (let ((result (beads-daemons-stop-result
                 :workspace "/home/user/project"
                 :pid 12345
                 :stopped t)))
    (should (beads-daemons-stop-result-p result))
    (should (equal (oref result workspace) "/home/user/project"))
    (should (eq (oref result stopped) t))))

(ert-deftest beads-daemons-test-killall-result-creation ()
  "Test creating beads-daemons-killall-result object."
  (let ((result (beads-daemons-killall-result
                 :stopped 2
                 :failed 1
                 :failures nil)))
    (should (beads-daemons-killall-result-p result))
    (should (equal (oref result stopped) 2))
    (should (equal (oref result failed) 1))))

;;; ============================================================
;;; JSON Parsing Tests
;;; ============================================================

(ert-deftest beads-daemons-test-parse-info ()
  "Test parsing daemon info from JSON."
  (let* ((json '((WorkspacePath . "/home/user/project")
                 (DatabasePath . "/home/user/project/.beads/issues.db")
                 (SocketPath . "/home/user/project/.beads/bd.sock")
                 (PID . 12345)
                 (Version . "0.28.0")
                 (UptimeSeconds . 3600.5)
                 (LastActivityTime . "2025-12-08T17:00:00Z")
                 (ExclusiveLockActive . t)
                 (ExclusiveLockHolder . "agent1")
                 (Alive . t)
                 (Error . "")))
         (info (beads-daemons--parse-info json)))
    (should (beads-daemon-info-p info))
    (should (equal (oref info workspace-path) "/home/user/project"))
    (should (equal (oref info pid) 12345))
    (should (equal (oref info version) "0.28.0"))
    (should (equal (oref info uptime-seconds) 3600.5))
    (should (eq (oref info exclusive-lock-active) t))
    (should (equal (oref info exclusive-lock-holder) "agent1"))
    (should (eq (oref info alive) t))))

(ert-deftest beads-daemons-test-parse-info-list ()
  "Test parsing list of daemon info from JSON array."
  (let ((infos (beads-daemons--parse-info-list
                beads-daemons-test--sample-list-json)))
    (should (listp infos))
    (should (= (length infos) 2))
    (should (beads-daemon-info-p (car infos)))
    (should (equal (oref (car infos) workspace-path) "/home/user/project1"))
    (should (equal (oref (cadr infos) workspace-path) "/home/user/project2"))))

(ert-deftest beads-daemons-test-parse-info-list-empty ()
  "Test parsing empty daemon list."
  (should (null (beads-daemons--parse-info-list nil)))
  (should (null (beads-daemons--parse-info-list []))))

(ert-deftest beads-daemons-test-parse-health-report ()
  "Test parsing health report from JSON."
  (let ((report (beads-daemons--parse-health-report
                 beads-daemons-test--sample-health-json)))
    (should (beads-daemons-health-report-p report))
    (should (equal (oref report total) 3))
    (should (equal (oref report healthy) 2))
    (should (equal (oref report stale) 1))
    (should (= (length (oref report daemons)) 3))
    ;; Check individual daemon health
    (let ((first-daemon (car (oref report daemons))))
      (should (beads-daemons-health-daemon-p first-daemon))
      (should (equal (oref first-daemon status) "healthy")))))

(ert-deftest beads-daemons-test-parse-stop-result ()
  "Test parsing stop result from JSON."
  (let ((result (beads-daemons--parse-stop-result
                 beads-daemons-test--sample-stop-json)))
    (should (beads-daemons-stop-result-p result))
    (should (equal (oref result workspace) "/home/user/project1"))
    (should (equal (oref result pid) 12345))
    (should (eq (oref result stopped) t))))

(ert-deftest beads-daemons-test-parse-restart-result ()
  "Test parsing restart result from JSON."
  (let ((result (beads-daemons--parse-restart-result
                 beads-daemons-test--sample-restart-json)))
    (should (beads-daemons-restart-result-p result))
    (should (equal (oref result workspace) "/home/user/project1"))
    (should (equal (oref result action) "restarted"))))

(ert-deftest beads-daemons-test-parse-logs-result ()
  "Test parsing logs result from JSON."
  (let ((result (beads-daemons--parse-logs-result
                 beads-daemons-test--sample-logs-json)))
    (should (beads-daemons-logs-result-p result))
    (should (equal (oref result workspace) "/home/user/project1"))
    (should (equal (oref result log-path)
                   "/home/user/project1/.beads/daemon.log"))
    (should (string-match-p "Starting daemon" (oref result content)))))

(ert-deftest beads-daemons-test-parse-killall-result ()
  "Test parsing killall result from JSON."
  (let ((result (beads-daemons--parse-killall-result
                 beads-daemons-test--sample-killall-json)))
    (should (beads-daemons-killall-result-p result))
    (should (equal (oref result stopped) 2))
    (should (equal (oref result failed) 1))
    (should (= (length (oref result failures)) 1))
    (let ((failure (car (oref result failures))))
      (should (beads-daemons-killall-failure-p failure))
      (should (equal (oref failure error) "process not found")))))

;;; ============================================================
;;; Command Class Tests
;;; ============================================================

(ert-deftest beads-daemons-test-command-list-line ()
  "Test daemons list command line generation."
  (let ((cmd (beads-daemons-command-list)))
    (should (member "daemons" (beads-command-line cmd)))
    (should (member "list" (beads-command-line cmd)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-list-no-cleanup ()
  "Test daemons list command with --no-cleanup."
  (let ((cmd (beads-daemons-command-list :no-cleanup t)))
    (should (member "--no-cleanup" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-list-search ()
  "Test daemons list command with --search."
  (let ((cmd (beads-daemons-command-list :search '("/home/user/projects"))))
    (should (member "--search" (beads-command-line cmd)))
    (should (member "/home/user/projects" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-health-line ()
  "Test daemons health command line generation."
  (let ((cmd (beads-daemons-command-health)))
    (should (member "daemons" (beads-command-line cmd)))
    (should (member "health" (beads-command-line cmd)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-stop-line ()
  "Test daemons stop command line generation."
  (let ((cmd (beads-daemons-command-stop :target "/home/user/project")))
    (should (member "daemons" (beads-command-line cmd)))
    (should (member "stop" (beads-command-line cmd)))
    (should (member "/home/user/project" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-stop-validation ()
  "Test daemons stop command validation."
  (let ((cmd-no-target (beads-daemons-command-stop))
        (cmd-with-target (beads-daemons-command-stop :target "/path")))
    (should (beads-command-validate cmd-no-target))
    (should (null (beads-command-validate cmd-with-target)))))

(ert-deftest beads-daemons-test-command-restart-line ()
  "Test daemons restart command line generation."
  (let ((cmd (beads-daemons-command-restart :target "12345")))
    (should (member "daemons" (beads-command-line cmd)))
    (should (member "restart" (beads-command-line cmd)))
    (should (member "12345" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-logs-line ()
  "Test daemons logs command line generation."
  (let ((cmd (beads-daemons-command-logs :target "/path" :lines 100)))
    (should (member "daemons" (beads-command-line cmd)))
    (should (member "logs" (beads-command-line cmd)))
    (should (member "/path" (beads-command-line cmd)))
    (should (member "--lines" (beads-command-line cmd)))
    (should (member "100" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-logs-follow ()
  "Test daemons logs command with --follow."
  (let ((cmd (beads-daemons-command-logs :target "/path" :follow t)))
    (should (member "--follow" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-logs-validation ()
  "Test daemons logs command validation."
  (let ((cmd-no-target (beads-daemons-command-logs))
        (cmd-with-target (beads-daemons-command-logs :target "/path"))
        (cmd-bad-lines (beads-daemons-command-logs :target "/path" :lines -1)))
    (should (beads-command-validate cmd-no-target))
    (should (null (beads-command-validate cmd-with-target)))
    (should (beads-command-validate cmd-bad-lines))))

(ert-deftest beads-daemons-test-command-killall-line ()
  "Test daemons killall command line generation."
  (let ((cmd (beads-daemons-command-killall)))
    (should (member "daemons" (beads-command-line cmd)))
    (should (member "killall" (beads-command-line cmd)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-daemons-test-command-killall-force ()
  "Test daemons killall command with --force."
  (let ((cmd (beads-daemons-command-killall :force t)))
    (should (member "--force" (beads-command-line cmd)))))

;;; ============================================================
;;; Command Execution Tests
;;; ============================================================

(ert-deftest beads-daemons-test-execute-list ()
  "Test executing daemons list command."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-list-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-command-execute (beads-daemons-command-list))))
         (should (listp result))
         (should (= (length result) 2))
         (should (beads-daemon-info-p (car result))))))))

(ert-deftest beads-daemons-test-execute-health ()
  "Test executing daemons health command."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-health-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-command-execute (beads-daemons-command-health))))
         (should (beads-daemons-health-report-p result))
         (should (equal (oref result total) 3)))))))

(ert-deftest beads-daemons-test-execute-stop ()
  "Test executing daemons stop command."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-stop-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-command-execute
                      (beads-daemons-command-stop :target "/home/user/project1"))))
         (should (beads-daemons-stop-result-p result))
         (should (eq (oref result stopped) t)))))))

(ert-deftest beads-daemons-test-execute-restart ()
  "Test executing daemons restart command."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-restart-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-command-execute
                      (beads-daemons-command-restart :target "/path"))))
         (should (beads-daemons-restart-result-p result))
         (should (equal (oref result action) "restarted")))))))

(ert-deftest beads-daemons-test-execute-logs ()
  "Test executing daemons logs command."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-logs-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-command-execute
                      (beads-daemons-command-logs :target "/path"))))
         (should (beads-daemons-logs-result-p result))
         (should (string-match-p "daemon" (oref result content))))))))

(ert-deftest beads-daemons-test-execute-killall ()
  "Test executing daemons killall command."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-killall-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-command-execute (beads-daemons-command-killall))))
         (should (beads-daemons-killall-result-p result))
         (should (equal (oref result stopped) 2))
         (should (equal (oref result failed) 1)))))))

;;; ============================================================
;;; High-Level API Tests
;;; ============================================================

(ert-deftest beads-daemons-test-api-list ()
  "Test beads-daemons--list function."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-list-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-daemons--list)))
         (should (listp result))
         (should (= (length result) 2)))))))

(ert-deftest beads-daemons-test-api-health ()
  "Test beads-daemons--health function."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-health-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-daemons--health)))
         (should (beads-daemons-health-report-p result))
         (should (equal (oref result healthy) 2)))))))

;;; ============================================================
;;; List Buffer Formatting Tests
;;; ============================================================

(ert-deftest beads-daemons-test-format-workspace ()
  "Test workspace path formatting."
  (should (string-match-p "unknown"
                          (beads-daemons-list--format-workspace nil)))
  (let ((formatted (beads-daemons-list--format-workspace "/home/user/project")))
    (should (stringp formatted))))

(ert-deftest beads-daemons-test-format-pid ()
  "Test PID formatting."
  (should (equal (beads-daemons-list--format-pid nil) "-"))
  (let ((formatted (beads-daemons-list--format-pid 12345)))
    (should (string-match-p "12345" formatted))))

(ert-deftest beads-daemons-test-format-version ()
  "Test version formatting."
  (should (equal (beads-daemons-list--format-version nil) "-"))
  (should (equal (beads-daemons-list--format-version "") "-"))
  (let ((formatted (beads-daemons-list--format-version "0.28.0")))
    (should (string-match-p "0.28.0" formatted))))

(ert-deftest beads-daemons-test-format-uptime ()
  "Test uptime formatting."
  (should (equal (beads-daemons-list--format-uptime nil) "-"))
  (should (equal (beads-daemons-list--format-uptime -1) "-"))
  (let ((formatted (beads-daemons-list--format-uptime 3600)))
    (should (string-match-p "1h" formatted))))

(ert-deftest beads-daemons-test-format-lock ()
  "Test lock status formatting."
  (should (equal (beads-daemons-list--format-lock nil nil) "-"))
  (let ((formatted (beads-daemons-list--format-lock t "agent1")))
    (should (string-match-p "locked" formatted))
    (should (string-match-p "agent1" formatted))))

(ert-deftest beads-daemons-test-make-entry ()
  "Test making tabulated list entry from daemon info."
  (let* ((daemon (beads-daemon-info
                  :workspace-path "/home/user/project"
                  :pid 12345
                  :version "0.28.0"
                  :uptime-seconds 3600.5
                  :alive t))
         (entry (beads-daemons-list--make-entry daemon)))
    (should (listp entry))
    (should (eq (car entry) daemon))
    (should (vectorp (cadr entry)))
    (should (= (length (cadr entry)) 6))))

;;; ============================================================
;;; Transient Menu Tests
;;; ============================================================

(ert-deftest beads-daemons-test-transient-defined ()
  "Test that daemons transient is defined."
  (should (fboundp 'beads-daemons))
  (should (commandp 'beads-daemons)))

(ert-deftest beads-daemons-test-list-command-defined ()
  "Test that daemons list command is defined."
  (should (fboundp 'beads-daemons-list))
  (should (commandp 'beads-daemons-list)))

(ert-deftest beads-daemons-test-reset-state ()
  "Test resetting daemons transient state."
  (setq beads-daemons--search '("/path")
        beads-daemons--target "test"
        beads-daemons--lines 100
        beads-daemons--force t)
  (beads-daemons--reset-state)
  (should (null beads-daemons--search))
  (should (null beads-daemons--target))
  (should (null beads-daemons--lines))
  (should (null beads-daemons--force)))

;;; ============================================================
;;; List Buffer Mode Tests
;;; ============================================================

(ert-deftest beads-daemons-test-list-mode-defined ()
  "Test that daemons list mode is defined."
  (should (fboundp 'beads-daemons-list-mode)))

(ert-deftest beads-daemons-test-list-keymap-defined ()
  "Test that list mode keymap is properly defined."
  (should (keymapp beads-daemons-list-mode-map))
  (should (eq (lookup-key beads-daemons-list-mode-map "g")
              'beads-daemons-list-refresh))
  (should (eq (lookup-key beads-daemons-list-mode-map "q")
              'quit-window))
  (should (eq (lookup-key beads-daemons-list-mode-map "s")
              'beads-daemons-list-stop))
  (should (eq (lookup-key beads-daemons-list-mode-map "r")
              'beads-daemons-list-restart))
  (should (eq (lookup-key beads-daemons-list-mode-map "l")
              'beads-daemons-list-view-log))
  (should (eq (lookup-key beads-daemons-list-mode-map "K")
              'beads-daemons-list-killall))
  (should (eq (lookup-key beads-daemons-list-mode-map "H")
              'beads-daemons-list-health)))

(ert-deftest beads-daemons-test-list-revert-buffer-function ()
  "Test that revert-buffer-function is set in daemons list mode."
  (with-temp-buffer
    (beads-daemons-list-mode)
    (should (eq revert-buffer-function
                #'beads-daemons-list--revert-buffer))))

(ert-deftest beads-daemons-test-list-revert-buffer-calls-refresh ()
  "Test that revert-buffer-function calls the refresh function."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-list-json))
         (refresh-called nil))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output))
               ((symbol-function 'beads-daemons-list-refresh)
                (lambda () (setq refresh-called t))))
       (with-temp-buffer
         (beads-daemons-list-mode)
         (beads-daemons-list--revert-buffer nil nil)
         (should refresh-called))))))

;;; ============================================================
;;; List Buffer Integration Tests
;;; ============================================================

(ert-deftest beads-daemons-test-list-buffer-creation ()
  "Test that list buffer can be created with mocked data."
  :tags '(integration)
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemons-test--sample-list-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (beads-daemons-list)
       (unwind-protect
           (progn
             (should (get-buffer "*beads-daemons*"))
             (with-current-buffer "*beads-daemons*"
               (should (derived-mode-p 'beads-daemons-list-mode))
               (should (= (length beads-daemons-list--data) 2))))
         (when (get-buffer "*beads-daemons*")
           (kill-buffer "*beads-daemons*")))))))

(provide 'beads-daemons-test)
;;; beads-daemons-test.el ends here
