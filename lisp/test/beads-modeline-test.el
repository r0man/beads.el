;;; beads-modeline-test.el --- Tests for beads-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-modeline.el.
;; Tests cover:
;; - Status cache functionality
;; - Mode-line segment generation
;; - Buffer detection
;; - Minor mode installation/uninstallation
;; - Click handlers

;;; Code:

(require 'ert)
(require 'beads-modeline)
(require 'beads-test)
(require 'beads-daemon)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar beads-modeline-test--sample-status-running
  (beads-daemon-status :running t :pid 12345 :started "2025-12-08 10:00:00")
  "Sample running daemon status.")

(defvar beads-modeline-test--sample-status-stopped
  (beads-daemon-status :running nil)
  "Sample stopped daemon status.")

(defvar beads-modeline-test--sample-health-healthy
  (beads-daemon-health :status "healthy" :version "0.28.0")
  "Sample healthy daemon health.")

(defvar beads-modeline-test--sample-health-degraded
  (beads-daemon-health :status "degraded" :version "0.28.0")
  "Sample degraded daemon health.")

(defmacro beads-modeline-test-with-clean-state (&rest body)
  "Execute BODY with clean mode-line state."
  (declare (indent 0))
  `(let ((beads-modeline--status-cache nil)
         (beads-modeline--refresh-timer nil)
         (beads-modeline-refresh-interval 30)
         (beads-modeline-mode nil)
         (saved-hooks (mapcar (lambda (h) (cons h (symbol-value h)))
                              beads-modeline--hooks)))
     (unwind-protect
         (progn ,@body)
       ;; Cleanup
       (when beads-modeline--refresh-timer
         (cancel-timer beads-modeline--refresh-timer)
         (setq beads-modeline--refresh-timer nil))
       (setq beads-modeline--status-cache nil)
       ;; Restore hooks
       (dolist (pair saved-hooks)
         (set (car pair) (cdr pair))))))

;;; ============================================================
;;; Cache Tests
;;; ============================================================

(ert-deftest beads-modeline-test-cache-initially-nil ()
  "Test that cache is initially nil."
  (beads-modeline-test-with-clean-state
    (should (null beads-modeline--status-cache))))

(ert-deftest beads-modeline-test-cache-valid-when-fresh ()
  "Test that cache is valid when freshly populated."
  (beads-modeline-test-with-clean-state
    ;; Populate cache
    (setq beads-modeline--status-cache (cons (float-time) 'running))
    (should (beads-modeline--cache-valid-p))))

(ert-deftest beads-modeline-test-cache-invalid-when-expired ()
  "Test that cache is invalid after expiration."
  (beads-modeline-test-with-clean-state
    (let ((beads-modeline-refresh-interval 1))
      ;; Populate cache with old timestamp
      (setq beads-modeline--status-cache (cons (- (float-time) 2) 'running))
      (should-not (beads-modeline--cache-valid-p)))))

(ert-deftest beads-modeline-test-cache-invalid-when-nil ()
  "Test that cache is invalid when nil."
  (beads-modeline-test-with-clean-state
    (should-not (beads-modeline--cache-valid-p))))

(ert-deftest beads-modeline-test-cache-invalid-when-interval-nil ()
  "Test that cache is invalid when refresh-interval is nil."
  (beads-modeline-test-with-clean-state
    (let ((beads-modeline-refresh-interval nil))
      (setq beads-modeline--status-cache (cons (float-time) 'running))
      (should-not (beads-modeline--cache-valid-p)))))

(ert-deftest beads-modeline-test-invalidate-cache ()
  "Test that cache invalidation works."
  (beads-modeline-test-with-clean-state
    (setq beads-modeline--status-cache (cons (float-time) 'running))
    (should beads-modeline--status-cache)
    (beads-modeline-invalidate-cache)
    (should-not beads-modeline--status-cache)))

;;; ============================================================
;;; Status Fetching Tests
;;; ============================================================

(ert-deftest beads-modeline-test-fetch-status-running ()
  "Test fetching status when daemon is running and healthy."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () beads-modeline-test--sample-health-healthy)))
      (should (eq (beads-modeline--fetch-status) 'running)))))

(ert-deftest beads-modeline-test-fetch-status-stopped ()
  "Test fetching status when daemon is stopped."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-stopped)))
      (should (eq (beads-modeline--fetch-status) 'stopped)))))

(ert-deftest beads-modeline-test-fetch-status-nil ()
  "Test fetching status when daemon status is nil."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () nil)))
      (should (eq (beads-modeline--fetch-status) 'stopped)))))

(ert-deftest beads-modeline-test-fetch-status-degraded-health ()
  "Test fetching status when daemon is running but health is degraded."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () beads-modeline-test--sample-health-degraded)))
      (should (eq (beads-modeline--fetch-status) 'degraded)))))

(ert-deftest beads-modeline-test-fetch-status-nil-health ()
  "Test fetching status when daemon is running but health is nil."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () nil)))
      (should (eq (beads-modeline--fetch-status) 'degraded)))))

(ert-deftest beads-modeline-test-get-status-uses-cache ()
  "Test that get-status uses cache when valid."
  (beads-modeline-test-with-clean-state
    (let ((fetch-count 0))
      (cl-letf (((symbol-function 'beads-daemon--get-status)
                 (lambda ()
                   (cl-incf fetch-count)
                   beads-modeline-test--sample-status-running))
                ((symbol-function 'beads-daemon--get-health)
                 (lambda () beads-modeline-test--sample-health-healthy)))
        ;; First call should fetch
        (beads-modeline--get-status)
        (should (= fetch-count 1))
        ;; Second call should use cache
        (beads-modeline--get-status)
        (should (= fetch-count 1))
        ;; Third call should still use cache
        (beads-modeline--get-status)
        (should (= fetch-count 1))))))

(ert-deftest beads-modeline-test-refresh-status-updates-cache ()
  "Test that refresh-status updates the cache."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () beads-modeline-test--sample-health-healthy)))
      (should-not beads-modeline--status-cache)
      (beads-modeline--refresh-status)
      (should beads-modeline--status-cache)
      (should (eq (cdr beads-modeline--status-cache) 'running)))))

(ert-deftest beads-modeline-test-get-status-handles-errors ()
  "Test that get-status returns unknown on errors."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () (error "Connection failed"))))
      (should (eq (beads-modeline--get-status) 'unknown)))))

;;; ============================================================
;;; Mode-line Segment Tests
;;; ============================================================

(ert-deftest beads-modeline-test-status-icon-running ()
  "Test status icon for running state."
  (let ((icon (beads-modeline--status-icon 'running)))
    (should (stringp icon))
    (should (string= icon "\u25cf"))  ; ●
    (should (eq (get-text-property 0 'face icon) 'beads-modeline-running))))

(ert-deftest beads-modeline-test-status-icon-stopped ()
  "Test status icon for stopped state."
  (let ((icon (beads-modeline--status-icon 'stopped)))
    (should (stringp icon))
    (should (string= icon "\u25cb"))  ; ○
    (should (eq (get-text-property 0 'face icon) 'beads-modeline-stopped))))

(ert-deftest beads-modeline-test-status-icon-degraded ()
  "Test status icon for degraded state."
  (let ((icon (beads-modeline--status-icon 'degraded)))
    (should (stringp icon))
    (should (string= icon "\u26a0"))  ; ⚠
    (should (eq (get-text-property 0 'face icon) 'beads-modeline-degraded))))

(ert-deftest beads-modeline-test-status-icon-unknown ()
  "Test status icon for unknown state."
  (let ((icon (beads-modeline--status-icon 'unknown)))
    (should (stringp icon))
    (should (string= icon "?"))))

(ert-deftest beads-modeline-test-help-echo-running ()
  "Test help-echo for running state."
  (let ((help (beads-modeline--help-echo 'running)))
    (should (stringp help))
    (should (string-match-p "Running" help))
    (should (string-match-p "mouse-1" help))
    (should (string-match-p "mouse-3" help))
    (should (string-match-p "Keyboard" help))
    (should (string-match-p "beads-daemon" help))))

(ert-deftest beads-modeline-test-help-echo-stopped ()
  "Test help-echo for stopped state."
  (let ((help (beads-modeline--help-echo 'stopped)))
    (should (string-match-p "Stopped" help))))

(ert-deftest beads-modeline-test-help-echo-degraded ()
  "Test help-echo for degraded state."
  (let ((help (beads-modeline--help-echo 'degraded)))
    (should (string-match-p "Degraded" help))))

(ert-deftest beads-modeline-test-segment-always-returns-string ()
  "Test that segment always returns a string (buffer detection is via hooks)."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () beads-modeline-test--sample-health-healthy)))
      (with-temp-buffer
        ;; Segment always returns string - buffer filtering is done by hooks
        (should (stringp (beads-modeline-segment)))))))

(ert-deftest beads-modeline-test-segment-has-expected-properties ()
  "Test that segment has expected text and properties."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () beads-modeline-test--sample-health-healthy)))
      (with-temp-buffer
        (let ((segment (beads-modeline-segment)))
          (should (stringp segment))
          (should (string-match-p ":bd" segment))
          (should (get-text-property 0 'help-echo segment))
          (should (get-text-property 0 'local-map segment)))))))

(ert-deftest beads-modeline-test-segment-has-mouse-face ()
  "Test that segment has mouse-face property."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () beads-modeline-test--sample-health-healthy)))
      (with-temp-buffer
        (let ((segment (beads-modeline-segment)))
          (should (eq (get-text-property 0 'mouse-face segment)
                      'mode-line-highlight)))))))

;;; ============================================================
;;; Buffer Detection Tests
;;; ============================================================

(ert-deftest beads-modeline-test-in-beads-buffer-by-name ()
  "Test buffer detection by name pattern."
  (with-temp-buffer
    (rename-buffer "*beads-list*" t)
    (should (beads-modeline--in-beads-buffer-p))))

(ert-deftest beads-modeline-test-in-beads-buffer-by-name-various ()
  "Test buffer detection with various beads buffer names."
  (dolist (name '("*beads-list*" "*beads-show*" "*beads-daemon*"
                  "*beads-daemons*" "*beads-graph*"))
    (with-temp-buffer
      (rename-buffer name t)
      (should (beads-modeline--in-beads-buffer-p)))))

(ert-deftest beads-modeline-test-not-in-beads-buffer ()
  "Test buffer detection returns nil for non-beads buffer."
  (with-temp-buffer
    (should-not (beads-modeline--in-beads-buffer-p))))

;;; ============================================================
;;; Minor Mode Tests
;;; ============================================================

(ert-deftest beads-modeline-test-mode-defined ()
  "Test that the minor mode is defined."
  (should (fboundp 'beads-modeline-mode)))

(ert-deftest beads-modeline-test-mode-is-global ()
  "Test that the minor mode is global."
  (beads-modeline-test-with-clean-state
    (beads-modeline-mode 1)
    (unwind-protect
        (should beads-modeline-mode)
      (beads-modeline-mode -1))))

(ert-deftest beads-modeline-test-mode-installs-hooks ()
  "Test that enabling mode installs hooks on beads modes."
  (beads-modeline-test-with-clean-state
    (unwind-protect
        (progn
          (beads-modeline-mode 1)
          ;; Check that hooks are installed
          (dolist (hook beads-modeline--hooks)
            (should (memq #'beads-modeline--setup-buffer
                          (symbol-value hook)))))
      (beads-modeline-mode -1))))

(ert-deftest beads-modeline-test-mode-uninstalls-hooks ()
  "Test that disabling mode removes hooks from beads modes."
  (beads-modeline-test-with-clean-state
    (beads-modeline-mode 1)
    (beads-modeline-mode -1)
    ;; Check that hooks are removed
    (dolist (hook beads-modeline--hooks)
      (should-not (memq #'beads-modeline--setup-buffer
                        (symbol-value hook))))))

(ert-deftest beads-modeline-test-mode-starts-timer ()
  "Test that enabling mode starts the refresh timer."
  (beads-modeline-test-with-clean-state
    (let ((beads-modeline-refresh-interval 60))
      (unwind-protect
          (progn
            (beads-modeline-mode 1)
            (should beads-modeline--refresh-timer))
        (beads-modeline-mode -1)))))

(ert-deftest beads-modeline-test-mode-stops-timer ()
  "Test that disabling mode stops the refresh timer."
  (beads-modeline-test-with-clean-state
    (let ((beads-modeline-refresh-interval 60))
      (beads-modeline-mode 1)
      (beads-modeline-mode -1)
      (should-not beads-modeline--refresh-timer))))

(ert-deftest beads-modeline-test-mode-no-timer-when-interval-nil ()
  "Test that no timer is started when refresh-interval is nil."
  (beads-modeline-test-with-clean-state
    (let ((beads-modeline-refresh-interval nil))
      (unwind-protect
          (progn
            (beads-modeline-mode 1)
            (should-not beads-modeline--refresh-timer))
        (beads-modeline-mode -1)))))

(ert-deftest beads-modeline-test-mode-clears-cache-on-disable ()
  "Test that disabling mode clears the cache."
  (beads-modeline-test-with-clean-state
    (setq beads-modeline--status-cache (cons (float-time) 'running))
    (beads-modeline-mode 1)
    (beads-modeline-mode -1)
    (should-not beads-modeline--status-cache)))

;;; ============================================================
;;; Setup Function Tests
;;; ============================================================

(ert-deftest beads-modeline-test-setup-buffer-sets-mode-line-process ()
  "Test that setup-buffer sets mode-line-process."
  (beads-modeline-test-with-clean-state
    (with-temp-buffer
      (should-not mode-line-process)
      (beads-modeline--setup-buffer)
      (should (eq mode-line-process beads-modeline--mode-line-construct)))))

(ert-deftest beads-modeline-test-teardown-buffer-clears-mode-line-process ()
  "Test that teardown-buffer clears mode-line-process."
  (beads-modeline-test-with-clean-state
    (with-temp-buffer
      (beads-modeline--setup-buffer)
      (should mode-line-process)
      (beads-modeline--teardown-buffer)
      (should-not mode-line-process))))

(ert-deftest beads-modeline-test-teardown-preserves-other-mode-line-process ()
  "Test that teardown doesn't clear mode-line-process set by other modes."
  (beads-modeline-test-with-clean-state
    (with-temp-buffer
      (setq-local mode-line-process '(:eval "other"))
      (beads-modeline--teardown-buffer)
      ;; Should not clear since it wasn't our construct
      (should (equal mode-line-process '(:eval "other"))))))

(ert-deftest beads-modeline-test-setup-enables-mode ()
  "Test that setup enables the minor mode."
  (beads-modeline-test-with-clean-state
    (unwind-protect
        (progn
          (should-not beads-modeline-mode)
          (beads-modeline-setup)
          (should beads-modeline-mode))
      (beads-modeline-mode -1))))

(ert-deftest beads-modeline-test-setup-sets-buffer-local-mode-line-process ()
  "Test that setup sets buffer-local mode-line-process."
  (beads-modeline-test-with-clean-state
    (with-temp-buffer
      (unwind-protect
          (progn
            (beads-modeline-setup)
            (should (eq mode-line-process beads-modeline--mode-line-construct)))
        (beads-modeline-mode -1)))))

(ert-deftest beads-modeline-test-setup-is-interactive ()
  "Test that setup is an interactive command."
  (should (commandp 'beads-modeline-setup)))

;;; ============================================================
;;; Customization Tests
;;; ============================================================

(ert-deftest beads-modeline-test-customization-group-exists ()
  "Test that customization group is defined."
  (should (get 'beads-modeline 'group-documentation)))

(ert-deftest beads-modeline-test-customization-refresh-interval ()
  "Test refresh-interval customization."
  (should (get 'beads-modeline-refresh-interval 'custom-type)))

(ert-deftest beads-modeline-test-customization-prefix ()
  "Test prefix customization."
  (should (get 'beads-modeline-prefix 'custom-type)))

(ert-deftest beads-modeline-test-prefix-in-segment ()
  "Test that prefix appears in segment."
  (beads-modeline-test-with-clean-state
    (let ((beads-modeline-prefix ":TEST"))
      (cl-letf (((symbol-function 'beads-daemon--get-status)
                 (lambda () beads-modeline-test--sample-status-running))
                ((symbol-function 'beads-daemon--get-health)
                 (lambda () beads-modeline-test--sample-health-healthy)))
        (with-temp-buffer
          (let ((segment (beads-modeline-segment)))
            (should (string-match-p ":TEST" segment))))))))

(ert-deftest beads-modeline-test-empty-prefix ()
  "Test segment with empty prefix."
  (beads-modeline-test-with-clean-state
    (let ((beads-modeline-prefix ""))
      (cl-letf (((symbol-function 'beads-daemon--get-status)
                 (lambda () beads-modeline-test--sample-status-running))
                ((symbol-function 'beads-daemon--get-health)
                 (lambda () beads-modeline-test--sample-health-healthy)))
        (with-temp-buffer
          (let ((segment (beads-modeline-segment)))
            ;; Should have space + icon (● U+25CF)
            (should (string= (substring-no-properties segment) " \u25cf"))))))))

;;; ============================================================
;;; Face Tests
;;; ============================================================

(ert-deftest beads-modeline-test-face-running-defined ()
  "Test that running face is defined."
  (should (facep 'beads-modeline-running)))

(ert-deftest beads-modeline-test-face-stopped-defined ()
  "Test that stopped face is defined."
  (should (facep 'beads-modeline-stopped)))

(ert-deftest beads-modeline-test-face-degraded-defined ()
  "Test that degraded face is defined."
  (should (facep 'beads-modeline-degraded)))

(ert-deftest beads-modeline-test-face-prefix-defined ()
  "Test that prefix face is defined."
  (should (facep 'beads-modeline-prefix)))

;;; ============================================================
;;; Keymap Tests
;;; ============================================================

(ert-deftest beads-modeline-test-keymap-defined ()
  "Test that keymap is defined."
  (should (keymapp beads-modeline--keymap)))

(ert-deftest beads-modeline-test-keymap-mouse-1 ()
  "Test that mouse-1 is bound."
  (should (lookup-key beads-modeline--keymap [mode-line mouse-1])))

(ert-deftest beads-modeline-test-keymap-mouse-3 ()
  "Test that mouse-3 is bound."
  (should (lookup-key beads-modeline--keymap [mode-line mouse-3])))

;;; ============================================================
;;; Click Handler Tests
;;; ============================================================

(ert-deftest beads-modeline-test-click-calls-daemon ()
  "Test that click handler calls beads-daemon."
  (let ((daemon-called nil))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &optional record-flag keys)
                 (when (eq fn #'beads-daemon)
                   (setq daemon-called t)))))
      (beads-modeline--click nil)
      (should daemon-called))))

(ert-deftest beads-modeline-test-refresh-click-invalidates-cache ()
  "Test that right-click invalidates cache."
  (beads-modeline-test-with-clean-state
    (cl-letf (((symbol-function 'beads-daemon--get-status)
               (lambda () beads-modeline-test--sample-status-running))
              ((symbol-function 'beads-daemon--get-health)
               (lambda () beads-modeline-test--sample-health-healthy))
              ((symbol-function 'force-mode-line-update)
               (lambda (&optional _all) nil)))
      ;; Populate cache
      (setq beads-modeline--status-cache (cons (float-time) 'stopped))
      ;; Right-click should refresh
      (beads-modeline--refresh-click nil)
      ;; Cache should be updated with new status
      (should beads-modeline--status-cache)
      (should (eq (cdr beads-modeline--status-cache) 'running)))))

(provide 'beads-modeline-test)
;;; beads-modeline-test.el ends here
