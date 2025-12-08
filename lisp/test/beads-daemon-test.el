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

;;; ============================================================
;;; Status Buffer Mode Tests
;;; ============================================================

(ert-deftest beads-daemon-test-status-mode-defined ()
  "Test that daemon status mode is defined."
  (should (fboundp 'beads-daemon-status-mode)))

(ert-deftest beads-daemon-test-status-buffer-command-defined ()
  "Test that daemon dashboard command is defined."
  (should (fboundp 'beads-daemon-dashboard))
  (should (commandp 'beads-daemon-dashboard)))

(ert-deftest beads-daemon-test-status-refresh-defined ()
  "Test that status refresh command is defined."
  (should (fboundp 'beads-daemon-status-refresh))
  (should (commandp 'beads-daemon-status-refresh)))

;;; ============================================================
;;; Status Buffer Rendering Tests
;;; ============================================================

(ert-deftest beads-daemon-test-status-format-state-running ()
  "Test formatting state when daemon is running and healthy."
  (let* ((status (beads-daemon-status :running t :pid 12345))
         (health (beads-daemon-health :status "healthy")))
    (let ((result (beads-daemon-status--format-state status health)))
      (should (string-match-p "Running" result))
      (should (string-match-p "healthy" result)))))

(ert-deftest beads-daemon-test-status-format-state-stopped ()
  "Test formatting state when daemon is stopped."
  (let ((result (beads-daemon-status--format-state nil nil)))
    (should (string-match-p "Stopped" result))))

(ert-deftest beads-daemon-test-status-insert-header ()
  "Test inserting header lines."
  (with-temp-buffer
    (beads-daemon-status--insert-header "Label:" "value")
    (should (string-match-p "Label:" (buffer-string)))
    (should (string-match-p "value" (buffer-string)))))

(ert-deftest beads-daemon-test-status-insert-section-header ()
  "Test inserting section headers."
  (with-temp-buffer
    (beads-daemon-status--insert-section-header "Test Section")
    (should (string-match-p "Test Section" (buffer-string)))
    (should (string-match-p "─" (buffer-string)))))

(ert-deftest beads-daemon-test-status-render-status-section ()
  "Test rendering the status section."
  (with-temp-buffer
    (let* ((status (beads-daemon-status :running t :pid 12345
                                        :started "2025-12-08 12:00:00"))
           (health (beads-daemon-health :status "healthy"
                                        :uptime-seconds 3600)))
      (beads-daemon-status--render-status-section status health)
      (should (string-match-p "Status" (buffer-string)))
      (should (string-match-p "Running" (buffer-string)))
      (should (string-match-p "12345" (buffer-string)))
      (should (string-match-p "1h" (buffer-string))))))

(ert-deftest beads-daemon-test-status-render-operations-table ()
  "Test rendering the operations table."
  (with-temp-buffer
    (let* ((op1 (beads-daemon-operation
                 :operation "health"
                 :total-count 100
                 :success-count 100
                 :error-count 0
                 :latency '((avg_ms . 3.5) (p99_ms . 8.0))))
           (op2 (beads-daemon-operation
                 :operation "list"
                 :total-count 10
                 :success-count 8
                 :error-count 2
                 :latency '((avg_ms . 15.0) (p99_ms . 25.0))))
           (metrics (beads-daemon-metrics :operations (list op1 op2))))
      (beads-daemon-status--render-operations-table metrics)
      (should (string-match-p "Operations" (buffer-string)))
      (should (string-match-p "health" (buffer-string)))
      (should (string-match-p "list" (buffer-string)))
      (should (string-match-p "100" (buffer-string)))
      (should (string-match-p "Total:" (buffer-string))))))

(ert-deftest beads-daemon-test-status-render-operations-table-empty ()
  "Test rendering operations table with no data."
  (with-temp-buffer
    (beads-daemon-status--render-operations-table nil)
    (should (string-match-p "No operation data available" (buffer-string)))))

(ert-deftest beads-daemon-test-status-render-health-section ()
  "Test rendering the health section."
  (with-temp-buffer
    (let ((health (beads-daemon-health
                   :db-response-ms 4.5
                   :active-connections 1
                   :max-connections 100
                   :memory-alloc-mb 5)))
      (beads-daemon-status--render-health-section health)
      (should (string-match-p "Health" (buffer-string)))
      (should (string-match-p "DB Response" (buffer-string)))
      (should (string-match-p "4.50" (buffer-string)))
      (should (string-match-p "1 / 100" (buffer-string))))))

(ert-deftest beads-daemon-test-status-render-resources-section ()
  "Test rendering the resources section."
  (with-temp-buffer
    (let ((metrics (beads-daemon-metrics
                    :memory-alloc-mb 5
                    :memory-sys-mb 20
                    :goroutine-count 12
                    :total-connections 50
                    :active-connections 1
                    :rejected-connections 0)))
      (beads-daemon-status--render-resources-section metrics)
      (should (string-match-p "Resources" (buffer-string)))
      (should (string-match-p "5 MB" (buffer-string)))
      (should (string-match-p "20 MB" (buffer-string)))
      (should (string-match-p "12" (buffer-string))))))

(ert-deftest beads-daemon-test-status-render-buffer-running ()
  "Test rendering complete status buffer when daemon is running."
  (with-temp-buffer
    (let* ((status (beads-daemon-status
                    :running t :pid 12345
                    :started "2025-12-08 12:00:00"
                    :log-path "/tmp/daemon.log"))
           (health (beads-daemon-health
                    :status "healthy"
                    :version "0.28.0"
                    :uptime-seconds 3600
                    :db-response-ms 4.5
                    :active-connections 1
                    :max-connections 100
                    :memory-alloc-mb 5))
           (metrics (beads-daemon-metrics
                     :operations nil
                     :memory-alloc-mb 5
                     :memory-sys-mb 20
                     :goroutine-count 12)))
      (beads-daemon-status--render-buffer status health metrics)
      (should (string-match-p "Daemon Status" (buffer-string)))
      (should (string-match-p "Running" (buffer-string)))
      (should (string-match-p "12345" (buffer-string)))
      (should (string-match-p "Configuration" (buffer-string)))
      (should (string-match-p "Health" (buffer-string)))
      (should (string-match-p "Resources" (buffer-string))))))

(ert-deftest beads-daemon-test-status-render-buffer-stopped ()
  "Test rendering status buffer when daemon is stopped."
  (with-temp-buffer
    (beads-daemon-status--render-buffer nil nil nil)
    (should (string-match-p "Daemon Status" (buffer-string)))
    (should (string-match-p "Stopped" (buffer-string)))
    ;; Should not have extended sections
    (should-not (string-match-p "Configuration" (buffer-string)))
    (should-not (string-match-p "Operations" (buffer-string)))))

;;; ============================================================
;;; Status Buffer Keymap Tests
;;; ============================================================

(ert-deftest beads-daemon-test-status-keymap-defined ()
  "Test that status mode keymap is properly defined."
  (should (keymapp beads-daemon-status-mode-map))
  ;; Check key bindings
  (should (eq (lookup-key beads-daemon-status-mode-map "g")
              'beads-daemon-status-refresh))
  (should (eq (lookup-key beads-daemon-status-mode-map "q")
              'quit-window))
  (should (eq (lookup-key beads-daemon-status-mode-map "s")
              'beads-daemon-start))
  (should (eq (lookup-key beads-daemon-status-mode-map "S")
              'beads-daemon-status--stop))
  (should (eq (lookup-key beads-daemon-status-mode-map "r")
              'beads-daemon-status--restart))
  (should (eq (lookup-key beads-daemon-status-mode-map "l")
              'beads-daemon-status--view-log))
  (should (eq (lookup-key beads-daemon-status-mode-map "L")
              'beads-daemon-status--tail-log))
  (should (eq (lookup-key beads-daemon-status-mode-map "n")
              'beads-daemon-status-next-section))
  (should (eq (lookup-key beads-daemon-status-mode-map "p")
              'beads-daemon-status-previous-section))
  (should (eq (lookup-key beads-daemon-status-mode-map "?")
              'beads-daemon-status-help)))

(ert-deftest beads-daemon-test-status-revert-buffer-function ()
  "Test that revert-buffer-function is set in daemon status mode."
  (with-temp-buffer
    (beads-daemon-status-mode)
    (should (eq revert-buffer-function
                #'beads-daemon-status--revert-buffer))))

(ert-deftest beads-daemon-test-status-revert-buffer-calls-refresh ()
  "Test that revert-buffer-function calls the refresh function."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-daemon-test--sample-status-json))
         (refresh-called nil))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output))
               ((symbol-function 'beads-daemon-status-refresh)
                (lambda () (setq refresh-called t))))
       (with-temp-buffer
         (beads-daemon-status-mode)
         (beads-daemon-status--revert-buffer nil nil)
         (should refresh-called))))))

;;; ============================================================
;;; Status Buffer Integration Tests
;;; ============================================================

(ert-deftest beads-daemon-test-status-buffer-creation ()
  "Test that status buffer can be created with mocked data."
  :tags '(integration)
  (beads-test-with-temp-config
   (let ((status-json (json-encode beads-daemon-test--sample-status-json))
         (health-json (json-encode beads-daemon-test--sample-health-json))
         (metrics-json (json-encode beads-daemon-test--sample-metrics-json))
         (call-count 0))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (setq call-count (1+ call-count))
                  ;; Handle destination being a list (buffer stderr-file)
                  (let ((output-buffer (cond
                                        ((bufferp destination) destination)
                                        ((consp destination) (car destination))
                                        (t nil))))
                    (when output-buffer
                      (with-current-buffer output-buffer
                        (cond
                         ((member "--status" args)
                          (insert status-json))
                         ((member "--health" args)
                          (insert health-json))
                         ((member "--metrics" args)
                          (insert metrics-json))))))
                  0)))
       (beads-daemon-dashboard)
       (unwind-protect
           (progn
             (should (get-buffer "*beads-daemon*"))
             (with-current-buffer "*beads-daemon*"
               (should (derived-mode-p 'beads-daemon-status-mode))
               (should (string-match-p "Running" (buffer-string)))
               (should (string-match-p "130264" (buffer-string)))
               (should (string-match-p "healthy" (buffer-string)))))
         (when (get-buffer "*beads-daemon*")
           (kill-buffer "*beads-daemon*")))))))

;;; ============================================================
;;; Cache Invalidation Tests
;;; ============================================================

(ert-deftest beads-daemon-test-start-invalidates-cache ()
  "Test that daemon start invalidates completion cache."
  (beads-test-with-temp-config
   (let ((json-output (json-encode '((running . t) (pid . 12345))))
         (cache-invalidated nil))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () (setq cache-invalidated t))))
       (beads-daemon-start--execute)
       (should cache-invalidated)))))

(ert-deftest beads-daemon-test-stop-invalidates-cache ()
  "Test that daemon stop invalidates completion cache."
  (beads-test-with-temp-config
   (let ((status-json (json-encode '((running . t) (pid . 12345))))
         (stop-json (json-encode '((stopped . t))))
         (cache-invalidated nil)
         (call-count 0))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (setq call-count (1+ call-count))
                  (let ((output-buffer (cond
                                        ((bufferp destination) destination)
                                        ((consp destination) (car destination))
                                        (t nil))))
                    (when output-buffer
                      (with-current-buffer output-buffer
                        (cond
                         ;; First call is status check
                         ((= call-count 1) (insert status-json))
                         ;; Second call is stop
                         (t (insert stop-json))))))
                  0))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () (setq cache-invalidated t)))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt) t)))
       (beads-daemon--stop)
       (should cache-invalidated)))))

(ert-deftest beads-daemon-test-restart-invalidates-cache ()
  "Test that daemon restart invalidates completion cache."
  (beads-test-with-temp-config
   (let ((status-json (json-encode '((running . t) (pid . 12345))))
         (stop-json (json-encode '((stopped . t))))
         (start-json (json-encode '((running . t) (pid . 12346))))
         (cache-invalidated nil)
         (call-count 0))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (setq call-count (1+ call-count))
                  (let ((output-buffer (cond
                                        ((bufferp destination) destination)
                                        ((consp destination) (car destination))
                                        (t nil))))
                    (when output-buffer
                      (with-current-buffer output-buffer
                        (cond
                         ((= call-count 1) (insert status-json))
                         ((= call-count 2) (insert stop-json))
                         (t (insert start-json))))))
                  0))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () (setq cache-invalidated t)))
               ((symbol-function 'sit-for)
                (lambda (_sec) nil)))
       (beads-daemon--restart)
       (should cache-invalidated)))))

(ert-deftest beads-daemon-test-status-stop-invalidates-cache ()
  "Test that daemon status buffer stop invalidates completion cache."
  (beads-test-with-temp-config
   (let ((status-json (json-encode '((running . t) (pid . 12345))))
         (stop-json (json-encode '((stopped . t))))
         (cache-invalidated nil)
         (call-count 0))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (setq call-count (1+ call-count))
                  (let ((output-buffer (cond
                                        ((bufferp destination) destination)
                                        ((consp destination) (car destination))
                                        (t nil))))
                    (when output-buffer
                      (with-current-buffer output-buffer
                        (cond
                         ((= call-count 1) (insert status-json))
                         (t (insert stop-json))))))
                  0))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () (setq cache-invalidated t)))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt) t))
               ((symbol-function 'beads-daemon-status-refresh)
                (lambda () nil)))
       (with-temp-buffer
         (beads-daemon-status-mode)
         (beads-daemon-status--stop)
         (should cache-invalidated))))))

(ert-deftest beads-daemon-test-status-restart-invalidates-cache ()
  "Test that daemon status buffer restart invalidates completion cache."
  (beads-test-with-temp-config
   (let ((status-json (json-encode '((running . t) (pid . 12345))))
         (stop-json (json-encode '((stopped . t))))
         (start-json (json-encode '((running . t) (pid . 12346))))
         (cache-invalidated nil)
         (call-count 0))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (setq call-count (1+ call-count))
                  (let ((output-buffer (cond
                                        ((bufferp destination) destination)
                                        ((consp destination) (car destination))
                                        (t nil))))
                    (when output-buffer
                      (with-current-buffer output-buffer
                        (cond
                         ((= call-count 1) (insert status-json))
                         ((= call-count 2) (insert stop-json))
                         (t (insert start-json))))))
                  0))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () (setq cache-invalidated t)))
               ((symbol-function 'sit-for)
                (lambda (_sec) nil))
               ((symbol-function 'beads-daemon-status-refresh)
                (lambda () nil)))
       (with-temp-buffer
         (beads-daemon-status-mode)
         (beads-daemon-status--restart)
         (should cache-invalidated))))))

(provide 'beads-daemon-test)
;;; beads-daemon-test.el ends here
