;;; beads-modeline-live-test.el --- Live test for mode-line-process integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Live test to verify that beads-modeline uses `mode-line-process'
;; correctly.  This test requires the beads.el library to be loaded
;; and verifies the implementation against the acceptance criteria
;; for issue beads.el-jbha.
;;
;; Run this test with:
;;   emacs --batch -L lisp -l beads -l lisp/test/beads-modeline-live-test.el -f beads-modeline-live-test-run
;;
;; Or within Emacs:
;;   M-x load-file RET lisp/test/beads-modeline-live-test.el RET
;;   M-x beads-modeline-live-test-run RET

;;; Code:

(require 'beads-modeline)
(require 'beads-daemon)

(defvar beads-modeline-live-test--failures nil
  "List of test failures.")

(defun beads-modeline-live-test--assert (condition message)
  "Assert that CONDITION is non-nil, recording MESSAGE on failure."
  (unless condition
    (push message beads-modeline-live-test--failures)
    (message "FAIL: %s" message))
  (when condition
    (message "PASS: %s" message))
  condition)

(defun beads-modeline-live-test--test-mode-line-process-is-buffer-local ()
  "Test that mode-line-process is set as buffer-local in beads buffers."
  (message "\n=== Test: mode-line-process is buffer-local ===")
  (let ((test-buf (generate-new-buffer "*beads-live-test*")))
    (unwind-protect
        (progn
          ;; Enable beads-modeline-mode
          (beads-modeline-mode 1)

          (with-current-buffer test-buf
            ;; Simulate being in a beads buffer by calling setup
            (beads-modeline--setup-buffer)

            ;; Verify mode-line-process is set
            (beads-modeline-live-test--assert
             mode-line-process
             "mode-line-process should be set after setup")

            ;; Verify it's our construct
            (beads-modeline-live-test--assert
             (eq mode-line-process beads-modeline--mode-line-construct)
             "mode-line-process should be beads-modeline--mode-line-construct")

            ;; Verify it's buffer-local
            (beads-modeline-live-test--assert
             (local-variable-p 'mode-line-process)
             "mode-line-process should be buffer-local")))
      (kill-buffer test-buf)
      (beads-modeline-mode -1))))

(defun beads-modeline-live-test--test-segment-evaluation ()
  "Test that the mode-line segment evaluates correctly."
  (message "\n=== Test: segment evaluation ===")
  ;; Mock the daemon status functions
  (cl-letf (((symbol-function 'beads-daemon--get-status)
             (lambda () (beads-daemon-status :running t :pid 12345)))
            ((symbol-function 'beads-daemon--get-health)
             (lambda () (beads-daemon-health :status "healthy" :version "0.28.0"))))
    (let ((segment (beads-modeline-segment)))
      (beads-modeline-live-test--assert
       (stringp segment)
       "Segment should return a string")

      (beads-modeline-live-test--assert
       (string-match-p ":bd" segment)
       "Segment should contain the :bd prefix")

      (beads-modeline-live-test--assert
       (get-text-property 0 'help-echo segment)
       "Segment should have help-echo property")

      (beads-modeline-live-test--assert
       (get-text-property 0 'local-map segment)
       "Segment should have local-map property for mouse clicks")

      (beads-modeline-live-test--assert
       (eq (get-text-property 0 'mouse-face segment) 'mode-line-highlight)
       "Segment should have mode-line-highlight mouse-face"))))

(defun beads-modeline-live-test--test-hooks-installed ()
  "Test that hooks are installed when mode is enabled."
  (message "\n=== Test: hooks installation ===")
  (unwind-protect
      (progn
        (beads-modeline-mode 1)

        (dolist (hook beads-modeline--hooks)
          (beads-modeline-live-test--assert
           (memq #'beads-modeline--setup-buffer (symbol-value hook))
           (format "Hook %s should contain beads-modeline--setup-buffer" hook))))
    (beads-modeline-mode -1)))

(defun beads-modeline-live-test--test-hooks-removed ()
  "Test that hooks are removed when mode is disabled."
  (message "\n=== Test: hooks removal ===")
  (beads-modeline-mode 1)
  (beads-modeline-mode -1)

  (dolist (hook beads-modeline--hooks)
    (beads-modeline-live-test--assert
     (not (memq #'beads-modeline--setup-buffer (symbol-value hook)))
     (format "Hook %s should NOT contain beads-modeline--setup-buffer after disable" hook))))

(defun beads-modeline-live-test--test-teardown-clears-buffer ()
  "Test that teardown clears mode-line-process from buffer."
  (message "\n=== Test: teardown clears buffer ===")
  (let ((test-buf (generate-new-buffer "*beads-teardown-test*")))
    (unwind-protect
        (with-current-buffer test-buf
          (beads-modeline--setup-buffer)
          (beads-modeline-live-test--assert
           mode-line-process
           "mode-line-process should be set after setup")

          (beads-modeline--teardown-buffer)
          (beads-modeline-live-test--assert
           (not mode-line-process)
           "mode-line-process should be cleared after teardown"))
      (kill-buffer test-buf))))

(defun beads-modeline-live-test--test-teardown-preserves-other-process ()
  "Test that teardown doesn't clear mode-line-process set by other modes."
  (message "\n=== Test: teardown preserves other mode-line-process ===")
  (let ((test-buf (generate-new-buffer "*beads-preserve-test*")))
    (unwind-protect
        (with-current-buffer test-buf
          (setq-local mode-line-process '(:eval "other-mode"))
          (beads-modeline--teardown-buffer)
          (beads-modeline-live-test--assert
           (equal mode-line-process '(:eval "other-mode"))
           "Other mode's mode-line-process should be preserved"))
      (kill-buffer test-buf))))

(defun beads-modeline-live-test--format-contains-process-p (format)
  "Return non-nil if FORMAT contains mode-line-process somewhere."
  (cond
   ((eq format 'mode-line-process) t)
   ((eq format 'mode-line-modes)
    ;; mode-line-modes contains mode-line-process
    (beads-modeline-live-test--format-contains-process-p mode-line-modes))
   ((and (listp format) (not (keywordp (car-safe format))))
    (cl-some #'beads-modeline-live-test--format-contains-process-p format))
   (t nil)))

(defun beads-modeline-live-test--test-mode-line-position ()
  "Test that the indicator appears in the expected position (after mode name)."
  (message "\n=== Test: mode-line position convention ===")
  ;; mode-line-process is part of mode-line-modes in the standard
  ;; mode-line-format, appearing right after the mode name.
  (beads-modeline-live-test--assert
   (beads-modeline-live-test--format-contains-process-p
    (default-value 'mode-line-format))
   "mode-line-process should be part of default mode-line-format (possibly nested)"))

;;;###autoload
(defun beads-modeline-live-test-run ()
  "Run all live tests for beads-modeline.
Returns t if all tests pass, nil otherwise."
  (interactive)
  (setq beads-modeline-live-test--failures nil)

  (message "\n========================================")
  (message "Running beads-modeline live tests")
  (message "========================================\n")

  ;; Run all tests
  (beads-modeline-live-test--test-mode-line-process-is-buffer-local)
  (beads-modeline-live-test--test-segment-evaluation)
  (beads-modeline-live-test--test-hooks-installed)
  (beads-modeline-live-test--test-hooks-removed)
  (beads-modeline-live-test--test-teardown-clears-buffer)
  (beads-modeline-live-test--test-teardown-preserves-other-process)
  (beads-modeline-live-test--test-mode-line-position)

  ;; Report results
  (message "\n========================================")
  (if beads-modeline-live-test--failures
      (progn
        (message "FAILED: %d test(s) failed:" (length beads-modeline-live-test--failures))
        (dolist (failure (reverse beads-modeline-live-test--failures))
          (message "  - %s" failure))
        nil)
    (message "PASSED: All tests passed!")
    t))

;; When loaded in batch mode, run tests automatically
(when noninteractive
  (unless (beads-modeline-live-test-run)
    (kill-emacs 1)))

(provide 'beads-modeline-live-test)
;;; beads-modeline-live-test.el ends here
