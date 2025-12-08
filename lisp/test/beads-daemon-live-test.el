;;; beads-daemon-live-test.el --- Live tests for daemon cache invalidation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Live tests for verifying that daemon operations invalidate the completion
;; cache.  These tests verify the actual behavior of the cache invalidation
;; function and daemon operations without mocking the cache.
;;
;; These tests can be run with:
;;   eldev -p -dtT test -s 'beads-daemon-live-test.*'

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-daemon)
(require 'beads-daemons)
(require 'beads-test)

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defun beads-daemon-live-test--setup-cache ()
  "Set up a valid cache state for testing.
Returns t if cache setup succeeded."
  (setq beads--completion-cache '(("bd-1" . "Test Issue")))
  (setq beads--completion-cache-time (float-time))
  ;; Verify setup
  (and beads--completion-cache beads--completion-cache-time))

;;; ============================================================
;;; ERT Test Cases
;;; ============================================================

(ert-deftest beads-daemon-live-test-cache-function-exists ()
  "Test that beads--invalidate-completion-cache function is defined."
  (should (fboundp 'beads--invalidate-completion-cache)))

(ert-deftest beads-daemon-live-test-cache-function-clears-cache ()
  "Test that beads--invalidate-completion-cache actually clears the cache."
  (beads-daemon-live-test--setup-cache)
  ;; Verify cache is set
  (should beads--completion-cache)
  ;; Call the invalidation function
  (beads--invalidate-completion-cache)
  ;; Verify cache was cleared
  (should (null beads--completion-cache)))

(ert-deftest beads-daemon-live-test-start-clears-cache ()
  "Test that daemon start clears the completion cache (live integration)."
  (beads-test-with-temp-config
   (beads-daemon-live-test--setup-cache)
   ;; Verify cache is set
   (should beads--completion-cache)
   ;; Mock daemon start execution
   (cl-letf (((symbol-function 'beads-command-execute)
              (lambda (cmd)
                (when (beads-daemon-command-start-p cmd)
                  (beads-daemon-status :running t :pid 12345)))))
     (beads-daemon-start--execute)
     ;; Cache should be cleared
     (should (null beads--completion-cache)))))

(ert-deftest beads-daemon-live-test-stop-clears-cache ()
  "Test that daemon stop clears the completion cache (live integration)."
  (beads-test-with-temp-config
   (beads-daemon-live-test--setup-cache)
   ;; Verify cache is set
   (should beads--completion-cache)
   ;; Mock daemon operations
   (cl-letf (((symbol-function 'beads-daemon--running-p)
              (lambda () t))
             ((symbol-function 'beads-command-execute)
              (lambda (cmd)
                (when (beads-daemon-command-stop-p cmd)
                  (beads-daemon-status :running nil))))
             ((symbol-function 'y-or-n-p)
              (lambda (_prompt) t)))
     (beads-daemon--stop)
     ;; Cache should be cleared
     (should (null beads--completion-cache)))))

(ert-deftest beads-daemon-live-test-restart-clears-cache ()
  "Test that daemon restart clears the completion cache (live integration)."
  (beads-test-with-temp-config
   (beads-daemon-live-test--setup-cache)
   ;; Verify cache is set
   (should beads--completion-cache)
   ;; Mock daemon operations
   (cl-letf (((symbol-function 'beads-daemon--running-p)
              (lambda () t))
             ((symbol-function 'beads-command-execute)
              (lambda (cmd)
                (cond
                 ((beads-daemon-command-stop-p cmd)
                  (beads-daemon-status :running nil))
                 ((beads-daemon-command-start-p cmd)
                  (beads-daemon-status :running t :pid 12346)))))
             ((symbol-function 'sit-for)
              (lambda (_sec) nil)))
     (beads-daemon--restart)
     ;; Cache should be cleared
     (should (null beads--completion-cache)))))

(provide 'beads-daemon-live-test)
;;; beads-daemon-live-test.el ends here
