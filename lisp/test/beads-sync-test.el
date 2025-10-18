;;; beads-sync-test.el --- Tests for beads-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-sync.el transient menu.
;; Tests cover all flags (--dry-run, -m/--message, --no-pull,
;; --no-push) and sync execution scenarios.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-sync)

;;; Test Utilities

(defun beads-sync-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defmacro beads-sync-test-with-state (state &rest body)
  "Execute BODY with beads-sync state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-sync--message nil
           beads-sync--dry-run nil
           beads-sync--no-pull nil
           beads-sync--no-push nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

;;; ============================================================
;;; State management tests
;;; ============================================================

(ert-deftest beads-sync-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-sync-test-with-state
   '((beads-sync--message . "Test message")
     (beads-sync--dry-run . t)
     (beads-sync--no-pull . t)
     (beads-sync--no-push . t))
   (beads-sync--reset-state)
   (should (null beads-sync--message))
   (should (null beads-sync--dry-run))
   (should (null beads-sync--no-pull))
   (should (null beads-sync--no-push))))

(ert-deftest beads-sync-test-format-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-sync--format-value "test message")))
    (should (stringp result))
    (should (string-match-p "test message" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-sync-test-format-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-sync--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-sync-test-format-flag-enabled ()
  "Test flag formatting when flag is enabled."
  (let ((result (beads-sync--format-flag t)))
    (should (stringp result))
    (should (string-match-p "enabled" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-sync-test-format-flag-disabled ()
  "Test flag formatting when flag is disabled."
  (let ((result (beads-sync--format-flag nil)))
    (should (stringp result))
    (should (string-match-p "disabled" result))))

;;; ============================================================
;;; Execution tests
;;; ============================================================

(ert-deftest beads-sync-test-execute-success ()
  "Test successful sync execution."
  (let ((sync-output "Exported 5 issues\nCommitted changes\nPulled from remote\nImported 7 issues\nPushed to remote\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 sync-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-sync--execute nil nil nil nil))
      (should (get-buffer "*beads-sync*")))))

(ert-deftest beads-sync-test-execute-with-message ()
  "Test sync with custom commit message."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync completed"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute "Custom commit message" nil nil nil)
      (should (member "-m" captured-args))
      (should (member "Custom commit message" captured-args)))))

(ert-deftest beads-sync-test-execute-with-dry-run ()
  "Test sync with dry-run flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Dry run preview"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil t nil nil)
      (should (member "--dry-run" captured-args)))))

(ert-deftest beads-sync-test-execute-with-no-pull ()
  "Test sync with no-pull flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync without pull"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil t nil)
      (should (member "--no-pull" captured-args)))))

(ert-deftest beads-sync-test-execute-with-no-push ()
  "Test sync with no-push flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync without push"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil nil t)
      (should (member "--no-push" captured-args)))))

(ert-deftest beads-sync-test-execute-with-all-flags ()
  "Test sync with all flags enabled."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "All flags enabled"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute "Test message" t t t)
      (should (member "-m" captured-args))
      (should (member "Test message" captured-args))
      (should (member "--dry-run" captured-args))
      (should (member "--no-pull" captured-args))
      (should (member "--no-push" captured-args)))))

(ert-deftest beads-sync-test-execute-failure ()
  "Test sync execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-sync-test--mock-call-process 1 "Error: sync failed")))
    (should-error (beads-sync--execute nil nil nil nil))))

(ert-deftest beads-sync-test-execute-empty-message ()
  "Test that empty message is not included in command."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync completed"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute "" nil nil nil)
      (should-not (member "-m" captured-args)))))

(ert-deftest beads-sync-test-execute-whitespace-message ()
  "Test that whitespace-only message is not included in command."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync completed"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute "   \n\t  " nil nil nil)
      (should-not (member "-m" captured-args)))))

;;; ============================================================
;;; Buffer and display tests
;;; ============================================================

(ert-deftest beads-sync-test-buffer-created ()
  "Test that sync creates *beads-sync* buffer."
  (let ((sync-output "Sync completed"))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 sync-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil nil nil)
      (should (get-buffer "*beads-sync*")))))

(ert-deftest beads-sync-test-buffer-content ()
  "Test that sync buffer contains output."
  (let ((sync-output "Test sync output"))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 sync-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil nil nil)
      (with-current-buffer "*beads-sync*"
        (should (string-match-p "Beads Sync Results" (buffer-string)))
        (should (string-match-p "Test sync output" (buffer-string)))))))

(ert-deftest beads-sync-test-buffer-mode ()
  "Test that sync buffer uses special-mode."
  (let ((sync-output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 sync-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil nil nil)
      (with-current-buffer "*beads-sync*"
        (should (eq major-mode 'special-mode))))))

(ert-deftest beads-sync-test-buffer-keybindings ()
  "Test that sync buffer has proper keybindings."
  (let ((sync-output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 sync-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil nil nil)
      (with-current-buffer "*beads-sync*"
        (should (local-key-binding (kbd "q")))
        (should (local-key-binding (kbd "g")))))))

(ert-deftest beads-sync-test-buffer-visual-line-mode ()
  "Test that sync buffer enables visual-line-mode."
  (let ((sync-output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 sync-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil nil nil)
      (with-current-buffer "*beads-sync*"
        (should visual-line-mode)))))

;;; ============================================================
;;; Preview tests
;;; ============================================================

(ert-deftest beads-sync-test-preview-no-flags ()
  "Test preview with no flags set."
  (beads-sync-test-with-state nil
    (let ((message-shown nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-shown
                         (apply #'format format-string args)))))
        (beads-sync--preview)
        (should (string-match-p "bd sync" message-shown))
        (should-not (string-match-p "--dry-run" message-shown))
        (should-not (string-match-p "--no-pull" message-shown))
        (should-not (string-match-p "--no-push" message-shown))))))

(ert-deftest beads-sync-test-preview-with-message ()
  "Test preview with custom message."
  (beads-sync-test-with-state
   '((beads-sync--message . "Custom message"))
   (let ((message-shown nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (setq message-shown
                        (apply #'format format-string args)))))
       (beads-sync--preview)
       (should (string-match-p "bd sync" message-shown))
       (should (string-match-p "-m" message-shown))
       (should (string-match-p "Custom message" message-shown))))))

(ert-deftest beads-sync-test-preview-with-dry-run ()
  "Test preview with dry-run flag."
  (beads-sync-test-with-state
   '((beads-sync--dry-run . t))
   (let ((message-shown nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (setq message-shown
                        (apply #'format format-string args)))))
       (beads-sync--preview)
       (should (string-match-p "--dry-run" message-shown))))))

(ert-deftest beads-sync-test-preview-with-no-pull ()
  "Test preview with no-pull flag."
  (beads-sync-test-with-state
   '((beads-sync--no-pull . t))
   (let ((message-shown nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (setq message-shown
                        (apply #'format format-string args)))))
       (beads-sync--preview)
       (should (string-match-p "--no-pull" message-shown))))))

(ert-deftest beads-sync-test-preview-with-no-push ()
  "Test preview with no-push flag."
  (beads-sync-test-with-state
   '((beads-sync--no-push . t))
   (let ((message-shown nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (setq message-shown
                        (apply #'format format-string args)))))
       (beads-sync--preview)
       (should (string-match-p "--no-push" message-shown))))))

(ert-deftest beads-sync-test-preview-with-all-flags ()
  "Test preview with all flags enabled."
  (beads-sync-test-with-state
   '((beads-sync--message . "Test")
     (beads-sync--dry-run . t)
     (beads-sync--no-pull . t)
     (beads-sync--no-push . t))
   (let ((message-shown nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (setq message-shown
                        (apply #'format format-string args)))))
       (beads-sync--preview)
       (should (string-match-p "bd sync" message-shown))
       (should (string-match-p "--dry-run" message-shown))
       (should (string-match-p "--no-pull" message-shown))
       (should (string-match-p "--no-push" message-shown))
       (should (string-match-p "-m" message-shown))
       (should (string-match-p "Test" message-shown))))))

;;; ============================================================
;;; Transient definition tests
;;; ============================================================

(ert-deftest beads-sync-test-transient-defined ()
  "Test that beads-sync transient is defined."
  (should (fboundp 'beads-sync)))

(ert-deftest beads-sync-test-transient-is-prefix ()
  "Test that beads-sync is a transient prefix."
  (should (get 'beads-sync 'transient--prefix)))

(ert-deftest beads-sync-test-infix-commands-defined ()
  "Test that sync infix commands are defined."
  (should (fboundp 'beads-sync--infix-message))
  (should (fboundp 'beads-sync--infix-dry-run))
  (should (fboundp 'beads-sync--infix-no-pull))
  (should (fboundp 'beads-sync--infix-no-push)))

(ert-deftest beads-sync-test-suffix-commands-defined ()
  "Test that sync suffix commands are defined."
  (should (fboundp 'beads-sync--execute-command))
  (should (fboundp 'beads-sync--preview))
  (should (fboundp 'beads-sync--reset)))

;;; ============================================================
;;; Edge cases and integration tests
;;; ============================================================

(ert-deftest beads-sync-test-execute-with-special-chars-in-message ()
  "Test sync with special characters in commit message."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync completed"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute "Fix \"quoted\" & special chars" nil nil nil)
      (should (member "-m" captured-args))
      (should (member "Fix \"quoted\" & special chars" captured-args)))))

(ert-deftest beads-sync-test-execute-buffer-refresh ()
  "Test that sync buffer can be refreshed."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 (with-current-buffer (current-buffer)
                   (insert (format "Call %d" call-count)))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute nil nil nil nil)
      (should (= call-count 1))
      (with-current-buffer "*beads-sync*"
        (funcall (local-key-binding (kbd "g"))))
      (should (= call-count 2)))))

(ert-deftest beads-sync-test-execute-large-output ()
  "Test sync with large output."
  (let ((large-output (make-string 10000 ?x)))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 large-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-sync--execute nil nil nil nil))
      (with-current-buffer "*beads-sync*"
        ;; Buffer should contain header + large output
        (should (> (buffer-size) 10000))))))

(ert-deftest beads-sync-test-execute-multiline-message ()
  "Test sync with multiline commit message."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync completed"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute "First line\nSecond line\nThird line"
                           nil nil nil)
      (should (member "-m" captured-args))
      (should (member "First line\nSecond line\nThird line"
                      captured-args)))))

(ert-deftest beads-sync-test-execute-command-args-order ()
  "Test that sync command arguments are in correct order."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Sync completed"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-sync--execute "Message" t t t)
      (should (equal (car captured-args) "sync"))
      (should (member "--dry-run" captured-args))
      (should (member "--no-pull" captured-args))
      (should (member "--no-push" captured-args))
      (should (member "-m" captured-args))
      (should (member "Message" captured-args)))))

(ert-deftest beads-sync-test-dry-run-no-buffer-refresh ()
  "Test that dry-run does not trigger buffer refresh."
  (let ((refresh-called nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 0 "Dry run"))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads-list-refresh)
               (lambda () (setq refresh-called t))))
      (let ((beads-auto-refresh t))
        (beads-sync--execute nil t nil nil)
        (should-not refresh-called)))))

(ert-deftest beads-sync-test-conflict-handling-output ()
  "Test that conflict messages are displayed in buffer."
  (let ((conflict-output "Error: merge conflict detected\nPlease resolve conflicts manually"))
    (cl-letf (((symbol-function 'call-process)
               (beads-sync-test--mock-call-process 1 conflict-output)))
      (should-error (beads-sync--execute nil nil nil nil)))))

(provide 'beads-sync-test)
;;; beads-sync-test.el ends here
