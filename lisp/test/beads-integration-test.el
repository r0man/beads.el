;;; beads-integration-test.el --- Cross-module integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Cross-module integration tests for beads.el.
;; Tests workflows that span multiple modules and verify proper
;; integration between components.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-list)
(require 'beads-show)
(require 'beads-create)
(require 'beads-update)
(require 'beads-close)
(require 'beads-delete)
(require 'beads-misc)
(require 'beads-dep)
(require 'beads-graph)
(require 'beads-stats)

;; Load test utilities
(unless (featurep 'beads-test-helper)
  (load (expand-file-name "beads-test-helper"
                          (file-name-directory
                           (or load-file-name buffer-file-name)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-integration-test-list-to-show-workflow ()
  "Integration test: Navigate from list to show buffer."
  :tags '(integration)
  (require 'beads-list)
  (require 'beads-show)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has show command
    (should (fboundp 'beads-list-show))
    ;; Verify RET is bound to show
    (let ((binding (lookup-key beads-list-mode-map (kbd "RET"))))
      (should (eq binding 'beads-list-show)))))

(ert-deftest beads-integration-test-list-to-update-workflow ()
  "Integration test: Update from list buffer."
  :tags '(integration)
  (require 'beads-list)
  (require 'beads-update)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has update command
    (should (fboundp 'beads-list-update))
    ;; Verify e is bound to update
    (let ((binding (lookup-key beads-list-mode-map (kbd "e"))))
      (should (eq binding 'beads-list-update)))))

(ert-deftest beads-integration-test-list-to-close-workflow ()
  "Integration test: Close from list buffer."
  :tags '(integration)
  (require 'beads-list)
  (require 'beads-close)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has close command
    (should (fboundp 'beads-list-close))
    ;; Verify d is bound to close
    (let ((binding (lookup-key beads-list-mode-map (kbd "d"))))
      (should (eq binding 'beads-list-close)))))

(ert-deftest beads-integration-test-list-to-delete-workflow ()
  "Integration test: Delete from list buffer."
  :tags '(integration)
  (require 'beads-list)
  (require 'beads-delete)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has delete command
    (should (fboundp 'beads-list-delete))
    ;; Verify D is bound to delete
    (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
      (should (eq binding 'beads-list-delete)))))

(ert-deftest beads-integration-test-list-to-create-workflow ()
  "Integration test: Create from list buffer."
  :tags '(integration)
  (require 'beads-list)
  (require 'beads-create)
  (with-temp-buffer
    (beads-list-mode)
    ;; Verify that list mode has create command
    (should (fboundp 'beads-list-create))
    ;; Verify c is bound to create
    (let ((binding (lookup-key beads-list-mode-map (kbd "c"))))
      (should (eq binding 'beads-list-create)))))

(ert-deftest beads-integration-test-cache-invalidation-workflow ()
  "Integration test: Verify cache invalidation functions exist."
  :tags '(integration)
  ;; The cache invalidation function should be defined
  (should (fboundp 'beads--invalidate-completion-cache)))

(ert-deftest beads-integration-test-context-detection-list-mode ()
  "Integration test: Context detection works in list mode."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    ;; List mode should be active
    (should (eq major-mode 'beads-list-mode))))

(ert-deftest beads-integration-test-context-detection-show-mode ()
  "Integration test: Context detection works in show mode."
  :tags '(integration)
  (require 'beads-show)
  (with-temp-buffer
    (beads-show-mode)
    ;; Show mode should be active
    (should (eq major-mode 'beads-show-mode))))

(ert-deftest beads-integration-test-all-commands-autoloaded ()
  "Integration test: Verify all main commands are autoloaded."
  :tags '(integration)
  (should (fboundp 'beads-list))
  (should (fboundp 'beads-ready))
  (should (fboundp 'beads-blocked))
  (should (fboundp 'beads-show))
  (should (fboundp 'beads-create))
  (should (fboundp 'beads-update))
  (should (fboundp 'beads-close))
  (should (fboundp 'beads-delete))
  (should (fboundp 'beads-stats))
  (should (fboundp 'beads-graph-all))
  (should (fboundp 'beads-import))
  (should (fboundp 'beads-export)))

(provide 'beads-integration-test)
;;; beads-integration-test.el ends here
