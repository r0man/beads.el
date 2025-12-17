;;; beads-agent-claude-code-test.el --- Tests for claude-code backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-agent-claude-code.el - the claude-code.el backend.
;; These tests verify the buffer finding logic and other backend methods
;; without requiring the actual claude-code package.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-claude-code)

;;; Buffer Finding Tests
;;
;; These tests verify that `beads-agent-claude-code--find-buffers' correctly
;; identifies Claude buffers based on the naming convention used by claude-code.el.
;;
;; claude-code buffer naming convention:
;; - Main buffer: *claude:DIRECTORY/*
;; - Named instance: *claude:DIRECTORY/:INSTANCE*
;; Where DIRECTORY is the abbreviated truename with trailing slash.

(ert-deftest beads-agent-claude-code-test-find-buffers-main-buffer ()
  "Test finding main Claude buffer (no instance name)."
  (let ((test-buf (generate-new-buffer "*claude:~/workspace/test/*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*claude:~/workspace/test/*")))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-without-trailing-slash ()
  "Test that input without trailing slash still finds buffers."
  (let ((test-buf (generate-new-buffer "*claude:~/workspace/test/*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers
                      "/home/roman/workspace/test")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*claude:~/workspace/test/*")))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-instance-buffer ()
  "Test finding Claude buffer with instance name."
  (let ((test-buf (generate-new-buffer "*claude:~/workspace/test/:my-instance*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found))
                         "*claude:~/workspace/test/:my-instance*")))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-multiple ()
  "Test finding multiple Claude buffers for same directory."
  (let ((buf1 (generate-new-buffer "*claude:~/workspace/test/*"))
        (buf2 (generate-new-buffer "*claude:~/workspace/test/:tests*"))
        (buf3 (generate-new-buffer "*claude:~/workspace/test/:dev*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 3 (length found)))
          (should (member "*claude:~/workspace/test/*"
                          (mapcar #'buffer-name found)))
          (should (member "*claude:~/workspace/test/:tests*"
                          (mapcar #'buffer-name found)))
          (should (member "*claude:~/workspace/test/:dev*"
                          (mapcar #'buffer-name found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(ert-deftest beads-agent-claude-code-test-find-buffers-different-projects ()
  "Test that we don't find buffers from other projects."
  (let ((our-buf (generate-new-buffer "*claude:~/workspace/test/*"))
        (other-buf (generate-new-buffer "*claude:~/workspace/other/*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*claude:~/workspace/test/*")))
      (kill-buffer our-buf)
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-no-match ()
  "Test that we return nil when no matching buffers exist."
  (let ((other-buf (generate-new-buffer "*claude:~/workspace/other/*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers
                      "/home/roman/workspace/test/")))
          (should (null found)))
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-similar-prefix ()
  "Test that similar directory prefixes don't cause false matches."
  ;; Buffer for /workspace/test-extended should NOT match /workspace/test
  (let ((extended-buf (generate-new-buffer "*claude:~/workspace/test-extended/*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers
                      "/home/roman/workspace/test/")))
          (should (null found)))
      (kill-buffer extended-buf))))

;;; Backend Registration Test

(ert-deftest beads-agent-claude-code-test-backend-registered ()
  "Test that claude-code backend is registered."
  ;; Backends are stored as a list of EIEIO objects
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claude-code"))
                  beads-agent--backends)))
    (should backend)
    (should (beads-agent-backend-claude-code-p backend))))

;;; Backend Availability Test

(ert-deftest beads-agent-claude-code-test-backend-available-checks-package ()
  "Test that backend availability checks for claude-code package."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claude-code"))
                  beads-agent--backends)))
    (should backend)
    ;; When claude-code package and claude executable are both available,
    ;; the backend should report available
    (if (and (or (featurep 'claude-code)
                 (require 'claude-code nil t))
             (executable-find "claude"))
        (should (beads-agent-backend-available-p backend))
      ;; Otherwise it should be unavailable
      (should (not (beads-agent-backend-available-p backend))))))

(provide 'beads-agent-claude-code-test)
;;; beads-agent-claude-code-test.el ends here
