;;; beads-agent-claudemacs-test.el --- Tests for claudemacs backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for beads-agent-claudemacs.el - the claudemacs backend.
;; All tests mock the claudemacs and eat packages since they may not be installed.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-claudemacs)

;;; Backend Registration Tests

(ert-deftest beads-agent-claudemacs-test-backend-registered ()
  "Test that claudemacs backend is registered."
  (let ((backends (beads-agent--get-all-backends)))
    (should (cl-some (lambda (b) (equal (oref b name) "claudemacs")) backends))))

(ert-deftest beads-agent-claudemacs-test-backend-priority ()
  "Test claudemacs backend has correct priority."
  (let ((backend (beads-agent-backend-claudemacs)))
    (should (= (oref backend priority) 35))))

(ert-deftest beads-agent-claudemacs-test-backend-name ()
  "Test claudemacs backend has correct name."
  (let ((backend (beads-agent-backend-claudemacs)))
    (should (equal (oref backend name) "claudemacs"))))

(ert-deftest beads-agent-claudemacs-test-backend-description ()
  "Test claudemacs backend has a description."
  (let ((backend (beads-agent-backend-claudemacs)))
    (should (stringp (oref backend description)))
    (should (> (length (oref backend description)) 0))))

;;; Availability Tests

(ert-deftest beads-agent-claudemacs-test-available-all-deps ()
  "Test availability when all dependencies are present."
  (let ((backend (beads-agent-backend-claudemacs)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (memq f '(eat claudemacs))))
              ((symbol-function 'fboundp)
               (lambda (f) (memq f '(claudemacs--start
                                     claudemacs-kill
                                     claudemacs--send-message-to-claude
                                     eat-term-set-parameter))))
              ((symbol-function 'executable-find)
               (lambda (name) (when (equal name "claude") "/usr/bin/claude")))
              ((symbol-function 'beads-agent-claudemacs--ensure-eat-gv-setter)
               #'ignore))
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claudemacs-test-not-available-no-eat ()
  "Test unavailability when eat is missing."
  (let ((backend (beads-agent-backend-claudemacs)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claudemacs-test-not-available-no-claudemacs ()
  "Test unavailability when claudemacs is missing."
  (let ((backend (beads-agent-backend-claudemacs)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'eat)))
              ((symbol-function 'require)
               (lambda (f &rest _) (eq f 'eat)))
              ((symbol-function 'fboundp) (lambda (_) nil))
              ((symbol-function 'beads-agent-claudemacs--ensure-eat-gv-setter)
               #'ignore))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claudemacs-test-not-available-no-claude ()
  "Test unavailability when claude executable is missing."
  (let ((backend (beads-agent-backend-claudemacs)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'fboundp) (lambda (_) t))
              ((symbol-function 'executable-find) (lambda (_) nil))
              ((symbol-function 'beads-agent-claudemacs--ensure-eat-gv-setter)
               #'ignore))
      (should-not (beads-agent-backend-available-p backend)))))

;;; Start Session Tests

(ert-deftest beads-agent-claudemacs-test-start-error-no-eat ()
  "Test that start errors when eat is missing."
  (let ((backend (beads-agent-backend-claudemacs)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-claudemacs-test-start-error-no-claudemacs ()
  "Test that start errors when claudemacs is missing."
  (let ((backend (beads-agent-backend-claudemacs)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'eat)))
              ((symbol-function 'require)
               (lambda (f &rest _) (eq f 'eat)))
              ((symbol-function 'beads-agent-claudemacs--ensure-eat-gv-setter)
               #'ignore))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-claudemacs-test-start-error-no-claude ()
  "Test that start errors when claude executable is missing."
  (let ((backend (beads-agent-backend-claudemacs)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'executable-find) (lambda (_) nil))
              ((symbol-function 'beads-agent-claudemacs--ensure-eat-gv-setter)
               #'ignore)
              ((symbol-function 'beads-agent-claudemacs--install-bell-handler-advice)
               #'ignore))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

;;; Buffer Finding Tests

(ert-deftest beads-agent-claudemacs-test-find-buffers-with-cwd ()
  "Test finding claudemacs buffers by working directory."
  ;; Use a real temp directory to avoid symlink resolution issues
  (let* ((temp-dir (make-temp-file "beads-claudemacs-test-" t))
         (test-buf (generate-new-buffer "*claudemacs:claude:test*")))
    (unwind-protect
        (with-current-buffer test-buf
          (setq-local claudemacs--cwd temp-dir)
          (let ((found (beads-agent-claudemacs--find-buffers temp-dir)))
            (should (= (length found) 1))
            (should (equal (car found) test-buf))))
      (kill-buffer test-buf)
      (delete-directory temp-dir t))))

(ert-deftest beads-agent-claudemacs-test-find-buffers-no-match ()
  "Test finding buffers when none match."
  (let ((test-buf (generate-new-buffer "*claudemacs:claude:other*")))
    (unwind-protect
        (with-current-buffer test-buf
          (setq-local claudemacs--cwd "/tmp/other/")
          (let ((found (beads-agent-claudemacs--find-buffers "/tmp/test")))
            (should (null found))))
      (kill-buffer test-buf))))

;;; Buffer Has Process Tests

(ert-deftest beads-agent-claudemacs-test-buffer-has-process ()
  "Test buffer-has-process-p with a live process."
  (let ((test-buf (generate-new-buffer "*test-claudemacs-proc*")))
    (unwind-protect
        (progn
          (let ((proc (start-process "test" test-buf "sleep" "60")))
            (should (beads-agent-claudemacs--buffer-has-process-p test-buf))
            (delete-process proc)))
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claudemacs-test-buffer-no-process ()
  "Test buffer-has-process-p without a process."
  (let ((test-buf (generate-new-buffer "*test-claudemacs-no-proc*")))
    (unwind-protect
        (should-not (beads-agent-claudemacs--buffer-has-process-p test-buf))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claudemacs-test-buffer-dead ()
  "Test buffer-has-process-p with a dead buffer."
  (let ((test-buf (generate-new-buffer "*test-claudemacs-dead*")))
    (kill-buffer test-buf)
    (should-not (beads-agent-claudemacs--buffer-has-process-p test-buf))))

;;; Stop Session Tests

(ert-deftest beads-agent-claudemacs-test-stop-kills-process-and-buffer ()
  "Test that stop kills process and buffer."
  (let* ((test-buf (generate-new-buffer "*test-claudemacs-stop*"))
         (backend (beads-agent-backend-claudemacs))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claudemacs"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (unwind-protect
        (progn
          (let ((proc (start-process "test" test-buf "sleep" "60")))
            (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                      ((symbol-function 'eat-kill-process)
                       (lambda () (delete-process proc))))
              (beads-agent-backend-stop backend session)
              (should-not (buffer-live-p test-buf)))))
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claudemacs-test-stop-nil-buffer ()
  "Test that stop handles nil buffer gracefully."
  (let* ((backend (beads-agent-backend-claudemacs))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claudemacs"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer nil)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t)))
      (should-not (condition-case nil
                      (progn (beads-agent-backend-stop backend session) nil)
                    (error t))))))

;;; Session Active Tests

(ert-deftest beads-agent-claudemacs-test-session-active-with-process ()
  "Test session-active-p when buffer has process."
  (let* ((test-buf (generate-new-buffer "*test-claudemacs-active*"))
         (backend (beads-agent-backend-claudemacs))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claudemacs"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (unwind-protect
        (let ((proc (start-process "test" test-buf "sleep" "60")))
          (should (beads-agent-backend-session-active-p backend session))
          (delete-process proc))
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claudemacs-test-session-not-active-no-process ()
  "Test session-active-p when buffer has no process."
  (let* ((test-buf (generate-new-buffer "*test-claudemacs-inactive*"))
         (backend (beads-agent-backend-claudemacs))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claudemacs"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (unwind-protect
        (should-not (beads-agent-backend-session-active-p backend session))
      (kill-buffer test-buf))))

;;; Switch to Buffer Tests

(ert-deftest beads-agent-claudemacs-test-switch-to-buffer-success ()
  "Test switching to an existing live buffer."
  (let* ((test-buf (generate-new-buffer "*test-claudemacs-switch*"))
         (backend (beads-agent-backend-claudemacs))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claudemacs"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf))
         (switched-to nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--pop-to-buffer-other-window)
                   (lambda (buf) (setq switched-to buf))))
          (beads-agent-backend-switch-to-buffer backend session)
          (should (equal switched-to test-buf)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claudemacs-test-switch-to-buffer-killed ()
  "Test switching to a killed buffer errors appropriately."
  (let* ((test-buf (generate-new-buffer "*test-claudemacs-switch-killed*"))
         (backend (beads-agent-backend-claudemacs))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claudemacs"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (kill-buffer test-buf)
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

(ert-deftest beads-agent-claudemacs-test-switch-to-buffer-nil ()
  "Test switching when no buffer is set errors appropriately."
  (let ((backend (beads-agent-backend-claudemacs))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claudemacs"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"
                  :buffer nil)))
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

;;; Send Prompt Tests

(ert-deftest beads-agent-claudemacs-test-send-prompt-success ()
  "Test sending a prompt to an active session."
  (let* ((test-buf (generate-new-buffer "*test-claudemacs-send*"))
         (backend (beads-agent-backend-claudemacs))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claudemacs"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf))
         (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (setq-local claudemacs--cwd "/tmp/test/"))
          (let ((proc (start-process "test" test-buf "sleep" "60")))
            (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                      ((symbol-function 'claudemacs--send-message-to-claude)
                       (lambda (prompt) (setq sent-prompt prompt))))
              (beads-agent-backend-send-prompt backend session "Test message")
              (should (equal sent-prompt "Test message")))
            (delete-process proc)))
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

;;; Customization Tests

(ert-deftest beads-agent-claudemacs-test-tool-customization ()
  "Test that tool customization variable exists."
  (should (boundp 'beads-agent-claudemacs-tool)))

(ert-deftest beads-agent-claudemacs-test-tool-default-value ()
  "Test that tool defaults to claude."
  (should (eq (default-value 'beads-agent-claudemacs-tool) 'claude)))

(ert-deftest beads-agent-claudemacs-test-customization-group ()
  "Test that customization group exists."
  (should (get 'beads-agent-claudemacs 'group-documentation)))

;;; Bell Handler Advice Tests

(ert-deftest beads-agent-claudemacs-test-advice-installed-variable ()
  "Test that advice tracking variable exists."
  (should (boundp 'beads-agent-claudemacs--bell-handler-advice-installed)))

(ert-deftest beads-agent-claudemacs-test-setup-bell-handler-fixed-defined ()
  "Test that the fixed bell handler function is defined."
  (should (fboundp 'beads-agent-claudemacs--setup-bell-handler-fixed)))

(ert-deftest beads-agent-claudemacs-test-advice-function-defined ()
  "Test that the advice function is defined."
  (should (fboundp 'beads-agent-claudemacs--advice-setup-bell-handler)))

(ert-deftest beads-agent-claudemacs-test-install-advice-function-defined ()
  "Test that the install advice function is defined."
  (should (fboundp 'beads-agent-claudemacs--install-bell-handler-advice)))

(ert-deftest beads-agent-claudemacs-test-ensure-gv-setter-defined ()
  "Test that the gv-setter ensure function is defined."
  (should (fboundp 'beads-agent-claudemacs--ensure-eat-gv-setter)))

;;; Additional Bell Handler Tests

(ert-deftest beads-agent-claudemacs-test-advice-handles-void-function ()
  "Test that advice catches void-function errors for setf."
  (let ((fixed-called nil))
    (cl-letf (((symbol-function 'beads-agent-claudemacs--setup-bell-handler-fixed)
               (lambda () (setq fixed-called t))))
      ;; Simulate void-function error for the setf symbol
      (beads-agent-claudemacs--advice-setup-bell-handler
       (lambda ()
         (signal 'void-function '(\(setf\ eat-term-parameter\)))))
      (should fixed-called))))

(ert-deftest beads-agent-claudemacs-test-advice-handles-wrong-type ()
  "Test that advice catches wrong-type-argument errors."
  (let ((fixed-called nil))
    (cl-letf (((symbol-function 'beads-agent-claudemacs--setup-bell-handler-fixed)
               (lambda () (setq fixed-called t))))
      (beads-agent-claudemacs--advice-setup-bell-handler
       (lambda ()
         (signal 'wrong-type-argument '(nil))))
      (should fixed-called))))

(ert-deftest beads-agent-claudemacs-test-advice-reraises-other-void-function ()
  "Test that advice re-raises void-function for other symbols."
  (should-error
   (beads-agent-claudemacs--advice-setup-bell-handler
    (lambda ()
      (signal 'void-function '(other-symbol))))
   :type 'void-function))

(ert-deftest beads-agent-claudemacs-test-advice-passes-through ()
  "Test that advice passes through on success."
  (let ((original-called nil))
    (beads-agent-claudemacs--advice-setup-bell-handler
     (lambda () (setq original-called t)))
    (should original-called)))

(ert-deftest beads-agent-claudemacs-test-buffer-has-process-nil-buffer ()
  "Test buffer-has-process-p with nil buffer."
  (should-not (beads-agent-claudemacs--buffer-has-process-p nil)))

(ert-deftest beads-agent-claudemacs-test-buffer-has-process-dead-buffer ()
  "Test buffer-has-process-p with killed buffer."
  (let ((buf (generate-new-buffer "*test*")))
    (kill-buffer buf)
    (should-not (beads-agent-claudemacs--buffer-has-process-p buf))))

(ert-deftest beads-agent-claudemacs-test-buffer-has-process-no-process ()
  "Test buffer-has-process-p with live buffer but no process."
  (let ((buf (generate-new-buffer "*test-no-proc*")))
    (unwind-protect
        (should-not (beads-agent-claudemacs--buffer-has-process-p buf))
      (kill-buffer buf))))

(provide 'beads-agent-claudemacs-test)
;;; beads-agent-claudemacs-test.el ends here
