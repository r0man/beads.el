;;; beads-daemon-test.el --- Tests for beads-daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-daemon.el transient menu.
;; Tests cover daemon start, stop, status, and log viewing.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-daemon)

;;; Test Utilities

(defun beads-daemon-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defun beads-daemon-test--mock-start-file-process (exit-code)
  "Create a mock for `start-file-process' returning mock process.
EXIT-CODE determines process exit status."
  (lambda (name buffer program &rest args)
    (let ((proc (make-process
                 :name name
                 :buffer buffer
                 :command (cons program args)
                 :connection-type 'pipe
                 :sentinel #'ignore)))
      ;; Simulate process completion
      (run-at-time 0.1 nil
                   (lambda ()
                     (when (process-live-p proc)
                       (set-process-exit-status proc exit-code)
                       (delete-process proc))))
      proc)))

(defmacro beads-daemon-test-with-state (state &rest body)
  "Execute BODY with beads-daemon state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-daemon--auto-commit nil
           beads-daemon--auto-push nil
           beads-daemon--interval nil
           beads-daemon--log-path nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

;;; ============================================================
;;; State Management Tests
;;; ============================================================

(ert-deftest beads-daemon-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-daemon-test-with-state
   '((beads-daemon--auto-commit . t)
     (beads-daemon--auto-push . t)
     (beads-daemon--interval . "10m")
     (beads-daemon--log-path . "/tmp/test.log"))
   (beads-daemon--reset-state)
   (should (null beads-daemon--auto-commit))
   (should (null beads-daemon--auto-push))
   (should (null beads-daemon--interval))
   (should (null beads-daemon--log-path))))

(ert-deftest beads-daemon-test-format-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-daemon--format-value "5m")))
    (should (stringp result))
    (should (string-match-p "5m" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-daemon-test-format-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-daemon--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

;;; ============================================================
;;; Daemon Start Tests
;;; ============================================================

(ert-deftest beads-daemon-test-execute-start-basic ()
  "Test basic daemon start without flags."
  (let ((process-started nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq process-started t)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil nil nil)
      (should process-started))))

(ert-deftest beads-daemon-test-execute-start-with-auto-commit ()
  "Test daemon start with auto-commit flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start t nil nil nil)
      (should (member "--auto-commit" captured-args)))))

(ert-deftest beads-daemon-test-execute-start-with-auto-push ()
  "Test daemon start with auto-push flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil t nil nil)
      (should (member "--auto-push" captured-args)))))

(ert-deftest beads-daemon-test-execute-start-with-interval ()
  "Test daemon start with custom interval."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil "10m" nil)
      (should (member "--interval" captured-args))
      (should (member "10m" captured-args)))))

(ert-deftest beads-daemon-test-execute-start-with-log-path ()
  "Test daemon start with custom log path."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil nil "/tmp/custom.log")
      (should (member "--log" captured-args))
      (should (member "/tmp/custom.log" captured-args)))))

(ert-deftest beads-daemon-test-execute-start-with-all-flags ()
  "Test daemon start with all flags enabled."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start t t "15m" "/var/log/daemon.log")
      (should (member "--auto-commit" captured-args))
      (should (member "--auto-push" captured-args))
      (should (member "--interval" captured-args))
      (should (member "15m" captured-args))
      (should (member "--log" captured-args))
      (should (member "/var/log/daemon.log" captured-args)))))

(ert-deftest beads-daemon-test-execute-start-empty-interval ()
  "Test daemon start with empty interval is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil "" nil)
      (should-not (member "--interval" captured-args)))))

(ert-deftest beads-daemon-test-execute-start-whitespace-interval ()
  "Test daemon start with whitespace interval is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil "   " nil)
      (should-not (member "--interval" captured-args)))))

(ert-deftest beads-daemon-test-execute-start-no-json-flag ()
  "Test that daemon start does not include --json flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil nil nil)
      (should-not (member "--json" captured-args)))))

;;; ============================================================
;;; Daemon Stop Tests
;;; ============================================================

(ert-deftest beads-daemon-test-execute-stop-success ()
  "Test successful daemon stop."
  (cl-letf (((symbol-function 'call-process)
             (beads-daemon-test--mock-call-process 0 "Daemon stopped")))
    (should-not (beads-daemon--execute-stop))))

(ert-deftest beads-daemon-test-execute-stop-failure ()
  "Test daemon stop failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-daemon-test--mock-call-process 1 "Error")))
    (should-error (beads-daemon--execute-stop))))

(ert-deftest beads-daemon-test-execute-stop-command-args ()
  "Test daemon stop uses correct command."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args (cons program args))
                 0)))
      (beads-daemon--execute-stop)
      (should (member "daemon" captured-args))
      (should (member "--stop" captured-args))
      (should-not (member "--json" captured-args)))))

;;; ============================================================
;;; Daemon Status Tests
;;; ============================================================

(ert-deftest beads-daemon-test-execute-status-success ()
  "Test successful daemon status check."
  (let ((status-output "Daemon running (PID: 12345)"))
    (cl-letf (((symbol-function 'call-process)
               (beads-daemon-test--mock-call-process 0 status-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-daemon--execute-status))
      (should (get-buffer "*beads-daemon-status*")))))

(ert-deftest beads-daemon-test-execute-status-failure ()
  "Test daemon status check failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-daemon-test--mock-call-process 1 "Error")))
    (should-error (beads-daemon--execute-status))))

(ert-deftest beads-daemon-test-execute-status-buffer-content ()
  "Test status buffer contains output."
  (let ((status-output "Daemon running\nLast sync: 2025-10-18"))
    (cl-letf (((symbol-function 'call-process)
               (beads-daemon-test--mock-call-process 0 status-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-daemon--execute-status)
      (with-current-buffer "*beads-daemon-status*"
        (should (string-match-p "Daemon running" (buffer-string)))
        (should (string-match-p "Last sync" (buffer-string)))))))

(ert-deftest beads-daemon-test-execute-status-buffer-mode ()
  "Test status buffer uses special-mode."
  (let ((status-output "Status"))
    (cl-letf (((symbol-function 'call-process)
               (beads-daemon-test--mock-call-process 0 status-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-daemon--execute-status)
      (with-current-buffer "*beads-daemon-status*"
        (should (eq major-mode 'special-mode))))))

(ert-deftest beads-daemon-test-execute-status-keybindings ()
  "Test status buffer has proper keybindings."
  (let ((status-output "Status"))
    (cl-letf (((symbol-function 'call-process)
               (beads-daemon-test--mock-call-process 0 status-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-daemon--execute-status)
      (with-current-buffer "*beads-daemon-status*"
        (should (local-key-binding (kbd "q")))
        (should (local-key-binding (kbd "g")))))))

(ert-deftest beads-daemon-test-execute-status-refresh ()
  "Test status buffer refresh functionality."
  (let ((status-output "Initial")
        (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 (with-current-buffer (current-buffer)
                   (insert status-output))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-daemon--execute-status)
      (should (= call-count 1))
      (with-current-buffer "*beads-daemon-status*"
        (setq status-output "Refreshed")
        (funcall (local-key-binding (kbd "g"))))
      (should (= call-count 2)))))

(ert-deftest beads-daemon-test-execute-status-command-args ()
  "Test daemon status uses correct command."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args (cons program args))
                 (with-current-buffer (current-buffer)
                   (insert "Status"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-daemon--execute-status)
      (should (member "daemon" captured-args))
      (should (member "--status" captured-args))
      (should-not (member "--json" captured-args)))))

;;; ============================================================
;;; Daemon Log Viewing Tests
;;; ============================================================

(ert-deftest beads-daemon-test-view-log-success ()
  "Test successful log viewing."
  (let ((temp-log (make-temp-file "beads-daemon-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-log
            (insert "Log line 1\nLog line 2\n"))
          (cl-letf (((symbol-function 'display-buffer) (lambda (_buf) nil)))
            (let ((beads-daemon--log-path temp-log))
              (should-not (beads-daemon--view-log))
              (should (get-buffer "*beads-daemon-log*"))
              (with-current-buffer "*beads-daemon-log*"
                (should (string-match-p "Log line 1" (buffer-string)))
                (should (string-match-p "Log line 2" (buffer-string)))))))
      (delete-file temp-log))))

(ert-deftest beads-daemon-test-view-log-no-path ()
  "Test log viewing without path configured."
  (cl-letf (((symbol-function 'beads-daemon--get-default-log-path)
             (lambda () nil)))
    (let ((beads-daemon--log-path nil))
      (should-error (beads-daemon--view-log)
                    :type 'user-error))))

(ert-deftest beads-daemon-test-view-log-file-not-exists ()
  "Test log viewing with non-existent file."
  (let ((beads-daemon--log-path "/nonexistent/daemon.log"))
    (should-error (beads-daemon--view-log)
                  :type 'user-error)))

(ert-deftest beads-daemon-test-view-log-buffer-mode ()
  "Test log buffer uses special-mode."
  (let ((temp-log (make-temp-file "beads-daemon-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-log
            (insert "Test log"))
          (cl-letf (((symbol-function 'display-buffer) (lambda (_buf) nil)))
            (let ((beads-daemon--log-path temp-log))
              (beads-daemon--view-log)
              (with-current-buffer "*beads-daemon-log*"
                (should (eq major-mode 'special-mode))))))
      (delete-file temp-log))))

(ert-deftest beads-daemon-test-view-log-keybindings ()
  "Test log buffer has proper keybindings."
  (let ((temp-log (make-temp-file "beads-daemon-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-log
            (insert "Test"))
          (cl-letf (((symbol-function 'display-buffer) (lambda (_buf) nil)))
            (let ((beads-daemon--log-path temp-log))
              (beads-daemon--view-log)
              (with-current-buffer "*beads-daemon-log*"
                (should (local-key-binding (kbd "q")))
                (should (local-key-binding (kbd "g")))))))
      (delete-file temp-log))))

(ert-deftest beads-daemon-test-view-log-header ()
  "Test log buffer has header line."
  (let ((temp-log (make-temp-file "beads-daemon-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-log
            (insert "Test"))
          (cl-letf (((symbol-function 'display-buffer) (lambda (_buf) nil)))
            (let ((beads-daemon--log-path temp-log))
              (beads-daemon--view-log)
              (with-current-buffer "*beads-daemon-log*"
                (should header-line-format)
                (should (string-match-p "Daemon Log"
                                        (format "%s" header-line-format)))))))
      (delete-file temp-log))))

;;; ============================================================
;;; Transient Tests
;;; ============================================================

(ert-deftest beads-daemon-test-transient-defined ()
  "Test that beads-daemon transient is defined."
  (should (fboundp 'beads-daemon)))

(ert-deftest beads-daemon-test-transient-is-prefix ()
  "Test that beads-daemon is a transient prefix."
  (should (get 'beads-daemon 'transient--prefix)))

(ert-deftest beads-daemon-test-infix-commands-defined ()
  "Test that daemon infix commands are defined."
  (should (fboundp 'beads-daemon--infix-auto-commit))
  (should (fboundp 'beads-daemon--infix-auto-push))
  (should (fboundp 'beads-daemon--infix-interval))
  (should (fboundp 'beads-daemon--infix-log)))

(ert-deftest beads-daemon-test-suffix-commands-defined ()
  "Test that daemon suffix commands are defined."
  (should (fboundp 'beads-daemon--start-command))
  (should (fboundp 'beads-daemon--stop-command))
  (should (fboundp 'beads-daemon--status-command))
  (should (fboundp 'beads-daemon--view-log-command))
  (should (fboundp 'beads-daemon--reset)))

(ert-deftest beads-daemon-test-standalone-commands-defined ()
  "Test that standalone daemon commands are defined."
  (should (fboundp 'beads-daemon-start))
  (should (fboundp 'beads-daemon-stop))
  (should (fboundp 'beads-daemon-status)))

;;; ============================================================
;;; Edge Cases and Integration Tests
;;; ============================================================

(ert-deftest beads-daemon-test-start-empty-log-path ()
  "Test daemon start with empty log path is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil nil "")
      (should-not (member "--log" captured-args)))))

(ert-deftest beads-daemon-test-start-whitespace-log-path ()
  "Test daemon start with whitespace log path is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq captured-args args)
                 (make-process
                  :name name
                  :buffer buffer
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))))
      (beads-daemon--execute-start nil nil nil "   ")
      (should-not (member "--log" captured-args)))))

(ert-deftest beads-daemon-test-multiple-status-checks ()
  "Test multiple status checks work correctly."
  (let ((call-count 0)
        (outputs '("Status 1" "Status 2" "Status 3")))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (with-current-buffer (current-buffer)
                   (insert (nth call-count outputs)))
                 (setq call-count (1+ call-count))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-daemon--execute-status)
      (with-current-buffer "*beads-daemon-status*"
        (funcall (local-key-binding (kbd "g")))
        (funcall (local-key-binding (kbd "g"))))
      (should (= call-count 3)))))

(ert-deftest beads-daemon-test-log-view-large-file ()
  "Test viewing large log file."
  (let ((temp-log (make-temp-file "beads-daemon-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-log
            (dotimes (i 1000)
              (insert (format "Log line %d\n" i))))
          (cl-letf (((symbol-function 'display-buffer) (lambda (_buf) nil)))
            (let ((beads-daemon--log-path temp-log))
              (beads-daemon--view-log)
              (with-current-buffer "*beads-daemon-log*"
                ;; Verify some content
                (should (string-match-p "Log line 0" (buffer-string)))
                (should (string-match-p "Log line 999" (buffer-string)))))))
      (delete-file temp-log))))

(ert-deftest beads-daemon-test-status-empty-output ()
  "Test status with empty output."
  (let ((status-output ""))
    (cl-letf (((symbol-function 'call-process)
               (beads-daemon-test--mock-call-process 0 status-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-daemon--execute-status))
      (should (get-buffer "*beads-daemon-status*")))))

(provide 'beads-daemon-test)
;;; beads-daemon-test.el ends here
