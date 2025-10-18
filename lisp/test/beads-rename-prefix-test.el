;;; beads-rename-prefix-test.el --- Tests for beads-rename-prefix -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-rename-prefix.el transient menu.
;; Tests cover all validation, execution, and preview functionality.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-rename-prefix)

;;; Test Utilities

(defun beads-rename-prefix-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defmacro beads-rename-prefix-test-with-state (state &rest body)
  "Execute BODY with beads-rename-prefix state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-rename-prefix--new-prefix nil
           beads-rename-prefix--dry-run nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

;;; ============================================================
;;; State Management Tests
;;; ============================================================

(ert-deftest beads-rename-prefix-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "kw-")
     (beads-rename-prefix--dry-run . t))
   (beads-rename-prefix--reset-state)
   (should (null beads-rename-prefix--new-prefix))
   (should (null beads-rename-prefix--dry-run))))

(ert-deftest beads-rename-prefix-test-format-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-rename-prefix--format-value "kw-")))
    (should (stringp result))
    (should (string-match-p "kw-" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-rename-prefix-test-format-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-rename-prefix--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

;;; ============================================================
;;; Validation Tests
;;; ============================================================

(ert-deftest beads-rename-prefix-test-validate-prefix-nil ()
  "Test prefix validation when prefix is nil."
  (beads-rename-prefix-test-with-state nil
   (should (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-validate-prefix-empty ()
  "Test prefix validation when prefix is empty."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . ""))
   (should (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-validate-prefix-whitespace ()
  "Test prefix validation when prefix is whitespace."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "   "))
   (should (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-validate-prefix-no-hyphen ()
  "Test prefix validation when prefix does not end with hyphen."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "kw"))
   (should (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-validate-prefix-no-letter-start ()
  "Test prefix validation when prefix does not start with letter."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "1kw-"))
   (should (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-validate-prefix-uppercase ()
  "Test prefix validation when prefix contains uppercase."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "KW-"))
   (should (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-validate-prefix-too-long ()
  "Test prefix validation when prefix is too long."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "verylongprefix-"))
   (should (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-validate-prefix-valid-short ()
  "Test prefix validation with valid short prefix."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "kw-"))
   (should (null (beads-rename-prefix--validate-prefix)))))

(ert-deftest beads-rename-prefix-test-validate-prefix-valid-with-numbers ()
  "Test prefix validation with valid prefix containing numbers."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "w1k2-"))
   (should (null (beads-rename-prefix--validate-prefix)))))

(ert-deftest beads-rename-prefix-test-validate-prefix-valid-with-hyphens ()
  "Test prefix validation with valid prefix containing hyphens."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "k-w-"))
   (should (null (beads-rename-prefix--validate-prefix)))))

(ert-deftest beads-rename-prefix-test-validate-prefix-valid-max-length ()
  "Test prefix validation with max length prefix."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "maxlen8-"))
   (should (null (beads-rename-prefix--validate-prefix)))))

(ert-deftest beads-rename-prefix-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "kw-"))
   (should (null (beads-rename-prefix--validate-all)))))

(ert-deftest beads-rename-prefix-test-validate-all-failure ()
  "Test validate-all with missing parameters."
  (beads-rename-prefix-test-with-state nil
   (let ((errors (beads-rename-prefix--validate-all)))
     (should errors)
     (should (listp errors))
     (should (>= (length errors) 1)))))

;;; ============================================================
;;; Preview Tests
;;; ============================================================

(ert-deftest beads-rename-prefix-test-preview-success ()
  "Test successful preview generation."
  (let ((preview-output "bd-1 -> kw-1\nbd-2 -> kw-2\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 preview-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-rename-prefix--execute-preview "kw-"))
      (should (get-buffer "*beads-rename-prefix-preview*"))
      (with-current-buffer "*beads-rename-prefix-preview*"
        (should (string-match-p "bd-1 -> kw-1" (buffer-string)))
        (should (string-match-p "bd-2 -> kw-2" (buffer-string)))))))

(ert-deftest beads-rename-prefix-test-preview-failure ()
  "Test preview generation failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-rename-prefix-test--mock-call-process 1 "Error")))
    (should-error (beads-rename-prefix--execute-preview "kw-"))))

(ert-deftest beads-rename-prefix-test-preview-buffer-mode ()
  "Test that preview buffer uses special-mode."
  (let ((preview-output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 preview-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-rename-prefix--execute-preview "kw-")
      (with-current-buffer "*beads-rename-prefix-preview*"
        (should (eq major-mode 'special-mode))))))

(ert-deftest beads-rename-prefix-test-preview-buffer-keybindings ()
  "Test that preview buffer has proper keybindings."
  (let ((preview-output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 preview-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-rename-prefix--execute-preview "kw-")
      (with-current-buffer "*beads-rename-prefix-preview*"
        (should (local-key-binding (kbd "q")))))))

;;; ============================================================
;;; Execute Tests
;;; ============================================================

(ert-deftest beads-rename-prefix-test-execute-dry-run ()
  "Test execution in dry-run mode."
  (let ((output "Dry run: bd-1 -> kw-1\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-rename-prefix--execute "kw-" t))
      (should (get-buffer "*beads-rename-prefix*")))))

(ert-deftest beads-rename-prefix-test-execute-dry-run-args ()
  "Test that dry-run mode passes correct arguments."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Test"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-rename-prefix--execute "kw-" t)
      (should (member "--dry-run" captured-args))
      (should (member "rename-prefix" captured-args))
      (should (member "kw-" captured-args)))))

(ert-deftest beads-rename-prefix-test-execute-real ()
  "Test execution in real mode."
  (let ((output "Renamed bd-1 -> kw-1\n")
        (cache-invalidated nil)
        (project-cache-invalidated nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () (setq cache-invalidated t)))
              ((symbol-function 'beads--invalidate-project-cache)
               (lambda () (setq project-cache-invalidated t))))
      (should-not (beads-rename-prefix--execute "kw-" nil))
      (should (get-buffer "*beads-rename-prefix*"))
      (should cache-invalidated)
      (should project-cache-invalidated))))

(ert-deftest beads-rename-prefix-test-execute-real-args ()
  "Test that real mode passes correct arguments."
  (let ((captured-args nil)
        (beads-auto-refresh nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Test"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda ()))
              ((symbol-function 'beads--invalidate-project-cache)
               (lambda ())))
      (beads-rename-prefix--execute "kw-" nil)
      (should (member "rename-prefix" captured-args))
      (should (member "kw-" captured-args))
      (should-not (member "--dry-run" captured-args)))))

(ert-deftest beads-rename-prefix-test-execute-failure ()
  "Test execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-rename-prefix-test--mock-call-process 1 "Error")))
    (should-error (beads-rename-prefix--execute "kw-" nil))))

(ert-deftest beads-rename-prefix-test-execute-buffer-content ()
  "Test execute buffer contains output."
  (let ((output "bd-1 -> kw-1\nbd-2 -> kw-2\n")
        (beads-auto-refresh nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda ()))
              ((symbol-function 'beads--invalidate-project-cache)
               (lambda ())))
      (beads-rename-prefix--execute "kw-" nil)
      (with-current-buffer "*beads-rename-prefix*"
        (should (string-match-p "bd-1 -> kw-1" (buffer-string)))
        (should (string-match-p "bd-2 -> kw-2" (buffer-string)))))))

;;; ============================================================
;;; Confirmation Tests
;;; ============================================================

(ert-deftest beads-rename-prefix-test-confirm-rename-match ()
  "Test confirmation when user types matching prefix."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "kw-")))
    (should (beads-rename-prefix--confirm-rename "kw-"))))

(ert-deftest beads-rename-prefix-test-confirm-rename-no-match ()
  "Test confirmation when user types non-matching prefix."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "wrong-")))
    (should-not (beads-rename-prefix--confirm-rename "kw-"))))

(ert-deftest beads-rename-prefix-test-confirm-rename-empty ()
  "Test confirmation when user types empty string."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "")))
    (should-not (beads-rename-prefix--confirm-rename "kw-"))))

;;; ============================================================
;;; Transient Tests
;;; ============================================================

(ert-deftest beads-rename-prefix-test-transient-defined ()
  "Test that beads-rename-prefix transient is defined."
  (should (fboundp 'beads-rename-prefix)))

(ert-deftest beads-rename-prefix-test-transient-is-prefix ()
  "Test that beads-rename-prefix is a transient prefix."
  (should (get 'beads-rename-prefix 'transient--prefix)))

(ert-deftest beads-rename-prefix-test-infix-commands-defined ()
  "Test that rename-prefix infix commands are defined."
  (should (fboundp 'beads-rename-prefix--infix-new-prefix))
  (should (fboundp 'beads-rename-prefix--infix-dry-run)))

(ert-deftest beads-rename-prefix-test-suffix-commands-defined ()
  "Test that rename-prefix suffix commands are defined."
  (should (fboundp 'beads-rename-prefix--preview-command))
  (should (fboundp 'beads-rename-prefix--execute-command))
  (should (fboundp 'beads-rename-prefix--reset)))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-rename-prefix-test-valid-prefix-a ()
  "Test valid prefix 'a-'."
  (setq beads-rename-prefix--new-prefix "a-")
  (should (null (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-valid-prefix-kw ()
  "Test valid prefix 'kw-'."
  (setq beads-rename-prefix--new-prefix "kw-")
  (should (null (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-valid-prefix-work ()
  "Test valid prefix 'work-'."
  (setq beads-rename-prefix--new-prefix "work-")
  (should (null (beads-rename-prefix--validate-prefix))))

(ert-deftest beads-rename-prefix-test-invalid-prefix-uppercase ()
  "Test invalid prefix 'A-' (uppercase)."
  (setq beads-rename-prefix--new-prefix "A-")
  (should (beads-rename-prefix--validate-prefix)))

(ert-deftest beads-rename-prefix-test-invalid-prefix-number-start ()
  "Test invalid prefix '1abc-' (starts with number)."
  (setq beads-rename-prefix--new-prefix "1abc-")
  (should (beads-rename-prefix--validate-prefix)))

(ert-deftest beads-rename-prefix-test-invalid-prefix-no-hyphen ()
  "Test invalid prefix 'abc' (no ending hyphen)."
  (setq beads-rename-prefix--new-prefix "abc")
  (should (beads-rename-prefix--validate-prefix)))

(ert-deftest beads-rename-prefix-test-invalid-prefix-too-long ()
  "Test invalid prefix 'verylongname-' (too long)."
  (setq beads-rename-prefix--new-prefix "verylongname-")
  (should (beads-rename-prefix--validate-prefix)))

(ert-deftest beads-rename-prefix-test-preview-then-execute ()
  "Test preview followed by execute."
  (let ((output "bd-1 -> kw-1\n")
        (beads-auto-refresh nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda ()))
              ((symbol-function 'beads--invalidate-project-cache)
               (lambda ())))
      ;; First preview
      (beads-rename-prefix--execute-preview "kw-")
      (should (get-buffer "*beads-rename-prefix-preview*"))
      ;; Then execute
      (beads-rename-prefix--execute "kw-" nil)
      (should (get-buffer "*beads-rename-prefix*")))))

(ert-deftest beads-rename-prefix-test-buffer-refresh ()
  "Test that buffers are refreshed after rename."
  (let ((output "bd-1 -> kw-1\n")
        (beads-auto-refresh t)
        (refresh-called nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-rename-prefix-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda ()))
              ((symbol-function 'beads--invalidate-project-cache)
               (lambda ()))
              ((symbol-function 'beads-list-refresh)
               (lambda () (setq refresh-called t))))
      ;; Create a mock beads-list-mode buffer
      (with-temp-buffer
        (setq major-mode 'beads-list-mode)
        (beads-rename-prefix--execute "kw-" nil)
        (should refresh-called)))))

(ert-deftest beads-rename-prefix-test-edge-case-single-char ()
  "Test edge case with single character prefix."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "a-"))
   (should (null (beads-rename-prefix--validate-prefix)))))

(ert-deftest beads-rename-prefix-test-edge-case-all-numbers ()
  "Test edge case with all numbers after first letter."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "a123-"))
   (should (null (beads-rename-prefix--validate-prefix)))))

(ert-deftest beads-rename-prefix-test-edge-case-multiple-hyphens ()
  "Test edge case with multiple hyphens."
  (beads-rename-prefix-test-with-state
   '((beads-rename-prefix--new-prefix . "a-b-c-"))
   (should (null (beads-rename-prefix--validate-prefix)))))

(provide 'beads-rename-prefix-test)
;;; beads-rename-prefix-test.el ends here
