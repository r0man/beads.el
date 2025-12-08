;;; beads-daemon-test.el --- Tests for beads-daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-daemon.el.
;; Tests cover:
;; - High-level API functions
;; - Transient state management
;; - Transient menu integration
;;
;; Note: Tests for daemon command classes, data structures, and JSON parsing
;; are in beads-command-test.el.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-daemon)
(require 'beads-test)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar beads-daemon-test--sample-status-json
  '((running . t)
    (pid . 130264)
    (started . "2025-12-08 11:10:36")
    (log_path . "/home/user/.beads/daemon.log"))
  "Sample daemon status JSON for testing.")

(defvar beads-daemon-test--sample-health-json
  '((status . "healthy")
    (version . "0.28.0")
    (client_version . "0.0.0")
    (compatible . t)
    (uptime_seconds . 24081.247)
    (db_response_ms . 4.105)
    (active_connections . 1)
    (max_connections . 100)
    (memory_alloc_mb . 5))
  "Sample daemon health JSON for testing.")

(defvar beads-daemon-test--sample-metrics-json
  `((timestamp . "2025-12-08T17:51:57.308003235+01:00")
    (uptime_seconds . 24083)
    (operations . ,(vector
                    '((operation . "health")
                      (total_count . 99)
                      (success_count . 99)
                      (error_count . 0)
                      (latency . ((min_ms . 2.56)
                                  (p50_ms . 3.58)
                                  (p95_ms . 7.01)
                                  (p99_ms . 8.70)
                                  (max_ms . 8.70)
                                  (avg_ms . 3.89))))
                    '((operation . "list")
                      (total_count . 4)
                      (success_count . 4)
                      (error_count . 0)
                      (latency . ((min_ms . 6.67)
                                  (p50_ms . 14.89)
                                  (p95_ms . 21.40)
                                  (p99_ms . 21.40)
                                  (max_ms . 21.40)
                                  (avg_ms . 14.46))))))
    (total_connections . 51)
    (active_connections . 1)
    (rejected_connections . 0)
    (memory_alloc_mb . 5)
    (memory_sys_mb . 21)
    (goroutine_count . 12))
  "Sample daemon metrics JSON for testing.")

;;; ============================================================
;;; High-Level API Tests
;;; ============================================================

(ert-deftest beads-daemon-test-running-p-true ()
  "Test beads-daemon--running-p when daemon is running."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemon-test--sample-status-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (should (eq (beads-daemon--running-p) t))))))

(ert-deftest beads-daemon-test-running-p-false ()
  "Test beads-daemon--running-p when daemon is stopped."
  (beads-test-with-temp-config
   (let ((json-output (json-encode '((running . :json-false)))))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (should (null (beads-daemon--running-p)))))))

(ert-deftest beads-daemon-test-running-p-error ()
  "Test beads-daemon--running-p when command fails."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 1 "Error: no daemon")))
     (should (null (beads-daemon--running-p))))))

(ert-deftest beads-daemon-test-get-status-success ()
  "Test beads-daemon--get-status success case."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemon-test--sample-status-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((status (beads-daemon--get-status)))
         (should (beads-daemon-status-p status))
         (should (eq (oref status running) t)))))))

(ert-deftest beads-daemon-test-get-status-error ()
  "Test beads-daemon--get-status error case."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 1 "Error")))
     (should (null (beads-daemon--get-status))))))

(ert-deftest beads-daemon-test-get-health-success ()
  "Test beads-daemon--get-health success case."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemon-test--sample-health-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((health (beads-daemon--get-health)))
         (should (beads-daemon-health-p health))
         (should (equal (oref health status) "healthy")))))))

(ert-deftest beads-daemon-test-get-metrics-success ()
  "Test beads-daemon--get-metrics success case."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemon-test--sample-metrics-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((metrics (beads-daemon--get-metrics)))
         (should (beads-daemon-metrics-p metrics))
         (should (= (oref metrics goroutine-count) 12)))))))

(ert-deftest beads-daemon-test-get-log-path ()
  "Test beads-daemon--get-log-path."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemon-test--sample-status-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((log-path (beads-daemon--get-log-path)))
         (should (equal log-path "/home/user/.beads/daemon.log")))))))

(ert-deftest beads-daemon-test-get-log-path-not-running ()
  "Test beads-daemon--get-log-path when daemon not running."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 1 "Error")))
     (should (null (beads-daemon--get-log-path))))))

;;; ============================================================
;;; Transient State Tests
;;; ============================================================

(ert-deftest beads-daemon-test-reset-state ()
  "Test resetting daemon start state variables."
  ;; Set some state
  (setq beads-daemon-start--auto-commit t
        beads-daemon-start--auto-push t
        beads-daemon-start--foreground nil
        beads-daemon-start--local nil
        beads-daemon-start--interval "10s"
        beads-daemon-start--log "/tmp/test.log")
  ;; Reset
  (beads-daemon-start--reset-state)
  ;; Verify all nil
  (should (null beads-daemon-start--auto-commit))
  (should (null beads-daemon-start--auto-push))
  (should (null beads-daemon-start--foreground))
  (should (null beads-daemon-start--local))
  (should (null beads-daemon-start--interval))
  (should (null beads-daemon-start--log)))

(ert-deftest beads-daemon-test-parse-transient-args-empty ()
  "Test parsing empty transient args."
  (let ((cmd (beads-daemon-start--parse-transient-args nil)))
    (should (beads-daemon-command-start-p cmd))
    (should (null (oref cmd auto-commit)))
    (should (null (oref cmd auto-push)))
    (should (null (oref cmd foreground)))
    (should (null (oref cmd local)))
    (should (null (oref cmd interval)))
    (should (null (oref cmd log)))))

(ert-deftest beads-daemon-test-parse-transient-args-full ()
  "Test parsing full transient args."
  (let ((cmd (beads-daemon-start--parse-transient-args
              '("--auto-commit" "--auto-push" "--foreground"
                "--local" "--interval=10s" "--log=/tmp/test.log"))))
    (should (beads-daemon-command-start-p cmd))
    (should (oref cmd auto-commit))
    (should (oref cmd auto-push))
    (should (oref cmd foreground))
    (should (oref cmd local))
    (should (equal (oref cmd interval) "10s"))
    (should (equal (oref cmd log) "/tmp/test.log"))))

;;; ============================================================
;;; Transient Menu Tests
;;; ============================================================

(ert-deftest beads-daemon-test-transient-defined ()
  "Test that main daemon transient is defined."
  (should (fboundp 'beads-daemon))
  (should (commandp 'beads-daemon)))

(ert-deftest beads-daemon-test-transient-start-defined ()
  "Test that daemon start transient is defined."
  (should (fboundp 'beads-daemon-start))
  (should (commandp 'beads-daemon-start)))

(ert-deftest beads-daemon-test-transient-status-defined ()
  "Test that daemon status command is defined."
  (should (fboundp 'beads-daemon-show-status))
  (should (commandp 'beads-daemon-show-status)))

;;; ============================================================
;;; Format Header Tests
;;; ============================================================

(ert-deftest beads-daemon-test-format-header-running ()
  "Test format header when daemon is running."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemon-test--sample-status-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((header (beads-daemon--format-header)))
         (should (string-match-p "Running" header))
         (should (string-match-p "130264" header)))))))

(ert-deftest beads-daemon-test-format-header-stopped ()
  "Test format header when daemon is stopped."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 1 "Error")))
     (let ((header (beads-daemon--format-header)))
       (should (string-match-p "Stopped" header))))))

(provide 'beads-daemon-test)
;;; beads-daemon-test.el ends here
