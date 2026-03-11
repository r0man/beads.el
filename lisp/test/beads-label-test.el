;;; beads-label-test.el --- Tests for beads-command-label.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for beads-command-label.el label management functionality.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-buffer)
(require 'beads-command-label)
(require 'beads-test)

;;; Test Utilities

(defun beads-label-test--with-mock-project (body)
  "Execute BODY with mocked git functions for consistent naming."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "test-proj"))
            ((symbol-function 'beads-git-get-branch)
             (lambda () "main"))
            ((symbol-function 'beads-buffer-is-main-branch-p)
             (lambda () t))
            ((symbol-function 'beads-git-in-worktree-p)
             (lambda () nil)))
    (funcall body)))

;;; Test Fixtures

(defvar beads-label-test--sample-labels-json
  "[{\"label\":\"backend\",\"count\":5},{\"label\":\"frontend\",\"count\":3},{\"label\":\"bug\",\"count\":10}]"
  "Sample JSON output from bd label list-all.")

;;; ============================================================
;;; Label Fetching and Caching Tests
;;; ============================================================

(ert-deftest beads-label-test-list-all-success ()
  "Test successful label fetching from bd label list-all."
  (let ((beads--label-cache nil))  ; Isolate cache for this test
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 beads-label-test--sample-labels-json)))
      (let ((result (beads-label-list-all)))
        (should (listp result))
        (should (= (length result) 3))
        ;; Result should be list of beads-label-count objects
        (should (cl-every #'beads-label-count-p result))
        (should (cl-some (lambda (obj) (string= (oref obj label) "backend")) result))
        (should (cl-some (lambda (obj) (string= (oref obj label) "frontend")) result))
        (should (cl-some (lambda (obj) (string= (oref obj label) "bug")) result))
        ;; Verify count fields exist
        (should (cl-every (lambda (obj) (numberp (oref obj count))) result))))))

(ert-deftest beads-label-test-cache-works ()
  "Test that label cache stores and retrieves labels."
  (let ((beads--label-cache nil))  ; Isolate cache for this test
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 beads-label-test--sample-labels-json)))
      ;; Clear cache first
      (beads--invalidate-label-cache)

      ;; First call should populate cache
      (let ((result1 (beads--get-cached-labels)))
        (should (listp result1))
        ;; Result should be list of beads-label-count objects
        (should (cl-some (lambda (obj) (string= (oref obj label) "backend")) result1))

        ;; Cache should be populated
        (should beads--label-cache)
        (should (consp beads--label-cache))

        ;; Second call should use cache (no process-file call)
        (cl-letf (((symbol-function 'process-file)
                   (lambda (&rest _args)
                     (error "Should not call process-file when cache is valid"))))
          (let ((result2 (beads--get-cached-labels)))
            (should (equal result1 result2))))))))

(ert-deftest beads-label-test-cache-invalidation ()
  "Test that cache can be manually invalidated."
  (let ((beads--label-cache nil))  ; Isolate cache for this test
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 beads-label-test--sample-labels-json)))
      ;; Populate cache
      (beads--get-cached-labels)
      (should beads--label-cache)

      ;; Invalidate
      (beads--invalidate-label-cache)
      (should-not beads--label-cache))))

(ert-deftest beads-label-test-completion-table ()
  "Test that label completion table returns correct list."
  (let ((beads--label-cache nil))  ; Isolate cache for this test
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 beads-label-test--sample-labels-json)))
      ;; Clear cache
      (beads--invalidate-label-cache)

      (let ((table (beads--label-completion-table)))
        (should (listp table))
        (should (= (length table) 3))
        (should (member "backend" table))
        (should (member "frontend" table))
        (should (member "bug" table))))))

;;; ============================================================
;;; Label Add Command Tests
;;; ============================================================

(ert-deftest beads-label-add-test-parse-args ()
  "Test parsing transient args for label add."
  (let ((args '("--issue-ids=bd-1,bd-2" "--label=urgent")))
    (let ((parsed (beads-label-add--parse-transient-args args)))
      (should (equal (plist-get parsed :issue-ids) "bd-1,bd-2"))
      (should (equal (plist-get parsed :label) "urgent")))))

(ert-deftest beads-label-add-test-validate-issue-ids ()
  "Test validation of issue IDs."
  (should-not (beads-label-add--validate-issue-ids "bd-1"))
  (should-not (beads-label-add--validate-issue-ids "bd-1,bd-2"))
  (should (beads-label-add--validate-issue-ids nil))
  (should (beads-label-add--validate-issue-ids ""))
  (should (beads-label-add--validate-issue-ids "  ")))

(ert-deftest beads-label-add-test-validate-label ()
  "Test validation of label name."
  (should-not (beads-label-add--validate-label "backend"))
  (should-not (beads-label-add--validate-label "bug"))
  (should (beads-label-add--validate-label nil))
  (should (beads-label-add--validate-label ""))
  (should (beads-label-add--validate-label "  ")))

(ert-deftest beads-label-add-test-validate-all-success ()
  "Test validation succeeds with valid parameters."
  (let ((parsed '(:issue-ids "bd-1" :label "urgent")))
    (should-not (beads-label-add--validate-all parsed))))

(ert-deftest beads-label-add-test-validate-all-missing-issue ()
  "Test validation fails with missing issue ID."
  (let ((parsed '(:issue-ids nil :label "urgent")))
    (should (beads-label-add--validate-all parsed))))

(ert-deftest beads-label-add-test-validate-all-missing-label ()
  "Test validation fails with missing label."
  (let ((parsed '(:issue-ids "bd-1" :label nil)))
    (should (beads-label-add--validate-all parsed))))

(ert-deftest beads-label-add-test-build-command-args-single ()
  "Test building command args for single issue."
  (let ((parsed '(:issue-ids "bd-1" :label "urgent")))
    (let ((args (beads-label-add--build-command-args parsed)))
      (should (equal args '("bd-1" "urgent"))))))

(ert-deftest beads-label-add-test-build-command-args-multiple ()
  "Test building command args for multiple issues."
  (let ((parsed '(:issue-ids "bd-1,bd-2,bd-3" :label "backend")))
    (let ((args (beads-label-add--build-command-args parsed)))
      (should (equal args '("bd-1" "bd-2" "bd-3" "backend"))))))

(ert-deftest beads-label-add-test-build-command-args-spaces ()
  "Test building command args with spaces in issue list."
  (let ((parsed '(:issue-ids "bd-1, bd-2, bd-3" :label "frontend")))
    (let ((args (beads-label-add--build-command-args parsed)))
      (should (equal args '("bd-1" "bd-2" "bd-3" "frontend"))))))

;;; ============================================================
;;; Label List-All View Tests
;;; ============================================================

(ert-deftest beads-label-test-current-label ()
  "Test extracting current label from tabulated list."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () "backend")))
    (should (equal (beads-label-list-all--current-label) "backend"))))

(ert-deftest beads-label-test-current-label-nil ()
  "Test extracting current label when no label at point."
  ;; Test that the function returns nil when there's no label
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () nil)))
    (should-not (beads-label-list-all--current-label))))

(ert-deftest beads-label-test-show-issues-no-label ()
  "Test showing issues when no label at point."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () nil)))
    (should-error (beads-label-list-all-show-issues)
                  :type 'user-error)))

(ert-deftest beads-label-test-show-issues-with-label ()
  "Test showing issues for a label."
  (beads-label-test--with-mock-project
   (lambda ()
     (let* ((beads-label-test--mock-issues
             (list (beads-issue :id "bd-1" :title "Test 1"
                               :status "open" :priority 2)
                   (beads-issue :id "bd-2" :title "Test 2"
                               :status "in_progress" :priority 1)))
            (expected-buf-name (beads-buffer-name-list nil "label=backend")))
       (cl-letf (((symbol-function 'tabulated-list-get-id)
                  (lambda () "backend"))
                 ((symbol-function 'beads-command-execute)
                  (lambda (_cmd) beads-label-test--mock-issues))
                 ((symbol-function 'beads-list-mode)
                  (lambda () (setq major-mode 'beads-list-mode)))
                 ((symbol-function 'beads-list--populate-buffer)
                  (lambda (issues _cmd _cmd-obj)
                    (should (= (length issues) 2))))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (_buffer) nil)))
         (let ((default-directory "/tmp/"))
           (beads-label-list-all-show-issues)
           ;; Should create buffer with correct name
           (should (get-buffer expected-buf-name))
           (kill-buffer expected-buf-name)))))))

(ert-deftest beads-label-test-hl-line-mode-enabled ()
  "Test that hl-line-mode is enabled when entering beads-label-list-all-mode."
  (with-temp-buffer
    (beads-label-list-all-mode)
    (should (bound-and-true-p hl-line-mode))))

;;; ============================================================
;;; Label Remove Command Tests
;;; ============================================================

(ert-deftest beads-label-remove-test-parse-args ()
  "Test parsing transient args for label remove."
  (let ((args '("--issue-ids=bd-1,bd-2" "--label=backend")))
    (let ((parsed (beads-label-remove--parse-transient-args args)))
      (should (equal (plist-get parsed :issue-ids) "bd-1,bd-2"))
      (should (equal (plist-get parsed :label) "backend")))))

(ert-deftest beads-label-remove-test-parse-args-empty ()
  "Test parsing empty transient args for label remove."
  (let ((args nil))
    (let ((parsed (beads-label-remove--parse-transient-args args)))
      (should (null (plist-get parsed :issue-ids)))
      (should (null (plist-get parsed :label))))))

(ert-deftest beads-label-remove-test-validate-issue-ids ()
  "Test validation of issue IDs for remove."
  (should-not (beads-label-remove--validate-issue-ids "bd-1"))
  (should-not (beads-label-remove--validate-issue-ids "bd-1,bd-2"))
  (should (beads-label-remove--validate-issue-ids nil))
  (should (beads-label-remove--validate-issue-ids ""))
  (should (beads-label-remove--validate-issue-ids "  ")))

(ert-deftest beads-label-remove-test-validate-label ()
  "Test validation of label name for remove."
  (should-not (beads-label-remove--validate-label "backend"))
  (should-not (beads-label-remove--validate-label "bug"))
  (should (beads-label-remove--validate-label nil))
  (should (beads-label-remove--validate-label ""))
  (should (beads-label-remove--validate-label "  ")))

(ert-deftest beads-label-remove-test-validate-all-success ()
  "Test validation succeeds with valid parameters for remove."
  (let ((parsed '(:issue-ids "bd-1" :label "urgent")))
    (should-not (beads-label-remove--validate-all parsed))))

(ert-deftest beads-label-remove-test-validate-all-missing-issue ()
  "Test validation fails with missing issue ID for remove."
  (let ((parsed '(:issue-ids nil :label "urgent")))
    (should (beads-label-remove--validate-all parsed))))

(ert-deftest beads-label-remove-test-validate-all-missing-label ()
  "Test validation fails with missing label for remove."
  (let ((parsed '(:issue-ids "bd-1" :label nil)))
    (should (beads-label-remove--validate-all parsed))))

(ert-deftest beads-label-remove-test-validate-all-both-missing ()
  "Test validation fails with both missing for remove."
  (let ((parsed '(:issue-ids nil :label nil)))
    (let ((errors (beads-label-remove--validate-all parsed)))
      (should (= (length errors) 2)))))

(ert-deftest beads-label-remove-test-build-command-args-single ()
  "Test building command args for single issue remove."
  (let ((parsed '(:issue-ids "bd-1" :label "urgent")))
    (let ((args (beads-label-remove--build-command-args parsed)))
      (should (equal args '("bd-1" "urgent"))))))

(ert-deftest beads-label-remove-test-build-command-args-multiple ()
  "Test building command args for multiple issues remove."
  (let ((parsed '(:issue-ids "bd-1,bd-2,bd-3" :label "backend")))
    (let ((args (beads-label-remove--build-command-args parsed)))
      (should (equal args '("bd-1" "bd-2" "bd-3" "backend"))))))

;;; ============================================================
;;; Context Detection Tests
;;; ============================================================

(ert-deftest beads-label-test-detect-issue-id-from-list-mode ()
  "Test detecting issue ID from beads-list-mode."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'beads-list-mode)))
              ((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (should (equal (beads-label--detect-issue-id) "bd-42")))))

(ert-deftest beads-label-test-detect-issue-id-from-show-mode ()
  "Test detecting issue ID from beads-show-mode."
  (with-temp-buffer
    (let ((beads-show--issue-id "bd-123"))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (mode)
                   (cond ((eq mode 'beads-list-mode) nil)
                         ((eq mode 'beads-show-mode) t)))))
        (should (equal (beads-label--detect-issue-id) "bd-123"))))))

(ert-deftest beads-label-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-show[proj]/bd-456*")
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (_mode) nil)))
      (should (equal (beads-label--detect-issue-id) "bd-456")))))

(ert-deftest beads-label-test-detect-issue-id-no-context ()
  "Test detecting issue ID when no context available."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (_mode) nil)))
      (should-not (beads-label--detect-issue-id)))))

;;; ============================================================
;;; Execute/Preview Tests (Mocked Transient Args)
;;; ============================================================

;; beads-label-add-test-execute-success removed - integration test requiring
;; full bd setup which is not available in batch mode. Execute functionality
;; is covered by unit tests of individual components.

(ert-deftest beads-label-add-test-execute-validation-error ()
  "Test label add execution with validation errors."
  (beads-test-with-transient-args 'beads-label-add
      '("--issue-ids=" "--label=")
    (should-error (beads-label-add--execute)
                  :type 'user-error)))

(ert-deftest beads-label-add-test-preview-success ()
  "Test label add preview with valid parameters."
  (beads-test-with-transient-args 'beads-label-add
      '("--issue-ids=bd-1" "--label=urgent")
    (let ((result (beads-label-add--preview)))
      (should (stringp result))
      (should (string-match-p "label" result))
      (should (string-match-p "add" result))
      (should (string-match-p "bd-1" result))
      (should (string-match-p "urgent" result)))))

(ert-deftest beads-label-add-test-preview-validation-error ()
  "Test label add preview with validation errors."
  (beads-test-with-transient-args 'beads-label-add
      '("--issue-ids=" "--label=")
    (let ((result (beads-label-add--preview)))
      (should (stringp result))
      (should (string-match-p "Validation" result)))))

;; beads-label-remove-test-execute-success removed - integration test requiring
;; full bd setup which is not available in batch mode. Execute functionality
;; is covered by unit tests of individual components.

(ert-deftest beads-label-remove-test-execute-validation-error ()
  "Test label remove execution with validation errors."
  (beads-test-with-transient-args 'beads-label-remove
      '("--issue-ids=" "--label=")
    (should-error (beads-label-remove--execute)
                  :type 'user-error)))

(ert-deftest beads-label-remove-test-preview-success ()
  "Test label remove preview with valid parameters."
  (beads-test-with-transient-args 'beads-label-remove
      '("--issue-ids=bd-1,bd-2" "--label=backend")
    (let ((result (beads-label-remove--preview)))
      (should (stringp result))
      (should (string-match-p "label" result))
      (should (string-match-p "remove" result))
      (should (string-match-p "bd-1" result))
      (should (string-match-p "backend" result)))))

(ert-deftest beads-label-remove-test-preview-validation-error ()
  "Test label remove preview with validation errors."
  (beads-test-with-transient-args 'beads-label-remove
      '("--issue-ids=" "--label=")
    (let ((result (beads-label-remove--preview)))
      (should (stringp result))
      (should (string-match-p "Validation" result)))))

;;; ============================================================
;;; Label List Tests
;;; ============================================================

;; beads-label-test-list-for-issue removed - integration test requiring
;; full bd setup which is not available in batch mode.

;;; ============================================================
;;; Label List-All Refresh Tests
;;; ============================================================

(ert-deftest beads-label-test-list-all-refresh ()
  "Test refreshing the label list-all view."
  (with-temp-buffer
    (beads-label-list-all-mode)
    (cl-letf (((symbol-function 'beads-label-list-all)
               (lambda ()
                 (list (beads-label-count :label "backend" :count 5)
                       (beads-label-count :label "frontend" :count 3)))))
      (beads-label-list-all-refresh)
      (should (= (length tabulated-list-entries) 2))
      ;; Check entries have correct format
      (let ((first-entry (car tabulated-list-entries)))
        (should (equal (car first-entry) "backend"))
        (should (vectorp (cadr first-entry)))))))

(ert-deftest beads-label-test-list-all-view-creates-buffer ()
  "Test that list-all-view creates the correct buffer."
  (beads-label-test--with-mock-project
   (lambda ()
     (let ((beads--label-cache nil)
           (expected-buf-name (beads-buffer-name-utility "labels")))
       (cl-letf (((symbol-function 'beads-label-list-all)
                  (lambda ()
                    (list (beads-label-count :label "test" :count 1))))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (_buf) nil)))
         (beads-label-list-all-view)
         (should (get-buffer expected-buf-name))
         (with-current-buffer expected-buf-name
           (should (eq major-mode 'beads-label-list-all-mode)))
         (kill-buffer expected-buf-name))))))

;;; ============================================================
;;; Cache TTL Tests
;;; ============================================================

(ert-deftest beads-label-test-cache-expires ()
  "Test that label cache expires after TTL."
  (let ((beads--label-cache nil)
        (beads-label-cache-ttl 1)  ; 1 second TTL for test
        (call-count 0))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 0))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (setq call-count (1+ call-count))
                 '(((label . "test") (count . 1))))))
      ;; First call
      (beads--get-cached-labels)
      (let ((first-call-count call-count))
        ;; Wait for cache to expire
        (sleep-for 1.1)
        ;; Second call should refresh
        (beads--get-cached-labels)
        (should (> call-count first-call-count))))))

;;; ============================================================
;;; Transient Definition Tests
;;; ============================================================

(ert-deftest beads-label-test-transient-defined ()
  "Test that beads-label transient is defined."
  (should (fboundp 'beads-label))
  (should (get 'beads-label 'transient--prefix)))

(ert-deftest beads-label-test-mode-map ()
  "Test that beads-label-list-all-mode-map has expected bindings."
  (should (keymapp beads-label-list-all-mode-map))
  (should (eq (lookup-key beads-label-list-all-mode-map (kbd "RET"))
              'beads-label-list-all-show-issues))
  (should (eq (lookup-key beads-label-list-all-mode-map (kbd "g"))
              'beads-label-list-all-refresh))
  (should (eq (lookup-key beads-label-list-all-mode-map (kbd "q"))
              'quit-window)))

;;; ============================================================
;;; Customization Tests
;;; ============================================================

(ert-deftest beads-label-test-cache-ttl-customization ()
  "Test that cache TTL is customizable."
  (should (boundp 'beads-label-cache-ttl))
  (should (integerp beads-label-cache-ttl))
  (should (> beads-label-cache-ttl 0)))

(ert-deftest beads-label-test-customization-group ()
  "Test that beads-label customization group exists."
  (should (get 'beads-label 'group-documentation)))

;;; ============================================================
;;; Transient Suffix Command Tests
;;; ============================================================

(ert-deftest beads-label-test-add-execute-success ()
  "Test label add execute suffix success flow."
  :tags '(:unit)
  (let ((executed nil)
        (message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (setq executed t)))
              ((symbol-function 'beads--invalidate-label-cache)
               (lambda () nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-label-add--execute)
      (should executed)
      (should message-output)
      (should (string-match-p "Added label" message-output)))))

(ert-deftest beads-label-test-add-execute-validation-error ()
  "Test label add execute handles validation errors."
  :tags '(:unit)
  (let ((error-caught nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '())))  ; Missing required args
      (condition-case nil
          (beads-label-add--execute)
        (user-error (setq error-caught t)))
      (should error-caught))))

(ert-deftest beads-label-test-add-execute-command-error ()
  "Test label add execute handles command errors."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (error "Command failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (let ((result (beads-label-add--execute)))
        (should (stringp result))
        (should (string-match-p "Failed" result))))))

(ert-deftest beads-label-test-add-reset-confirmed ()
  "Test label add reset when user confirms."
  :tags '(:unit)
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (_fmt &rest _args) nil)))
      (beads-label-add--reset)
      (should reset-called))))

(ert-deftest beads-label-test-add-reset-declined ()
  "Test label add reset when user declines."
  :tags '(:unit)
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-label-add--reset)
      (should-not reset-called))))

(ert-deftest beads-label-test-remove-execute-success ()
  "Test label remove execute suffix success flow."
  :tags '(:unit)
  (let ((executed nil)
        (message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (setq executed t)))
              ((symbol-function 'beads--invalidate-label-cache)
               (lambda () nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-label-remove--execute)
      (should executed)
      (should message-output)
      (should (string-match-p "Removed label" message-output)))))

(ert-deftest beads-label-test-remove-execute-validation-error ()
  "Test label remove execute handles validation errors."
  :tags '(:unit)
  (let ((error-caught nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '())))
      (condition-case nil
          (beads-label-remove--execute)
        (user-error (setq error-caught t)))
      (should error-caught))))

(ert-deftest beads-label-test-remove-reset-confirmed ()
  "Test label remove reset when user confirms."
  :tags '(:unit)
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (_fmt &rest _args) nil)))
      (beads-label-remove--reset)
      (should reset-called))))

(ert-deftest beads-label-test-add-preview-valid ()
  "Test label add preview with valid args."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-label-add--preview)
      (should message-output)
      (should (string-match-p "label" message-output)))))

(ert-deftest beads-label-test-add-preview-invalid ()
  "Test label add preview with invalid args."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '()))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (let ((result (beads-label-add--preview)))
        (should (stringp result))
        (should (string-match-p "Validation errors" result))))))

(ert-deftest beads-label-test-remove-preview-valid ()
  "Test label remove preview with valid args."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-label-remove--preview)
      (should message-output)
      (should (string-match-p "label" message-output)))))

;;; Tests for label-add validation

(ert-deftest beads-label-test-add-validate-no-issue ()
  "Test label-add validation fails without issue IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-label-add :label "bug")))
    (should (beads-command-validate cmd))
    (should (string-match-p "issue" (beads-command-validate cmd)))))

(ert-deftest beads-label-test-add-validate-no-label ()
  "Test label-add validation fails without label."
  :tags '(:unit)
  (let ((cmd (beads-command-label-add :issue-ids '("bd-1"))))
    (should (beads-command-validate cmd))
    (should (string-match-p "label" (beads-command-validate cmd)))))

(ert-deftest beads-label-test-add-validate-empty-label ()
  "Test label-add validation fails with empty label."
  :tags '(:unit)
  (let ((cmd (beads-command-label-add :issue-ids '("bd-1") :label "")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-label-test-add-validate-valid ()
  "Test label-add validation passes with valid args."
  :tags '(:unit)
  (let ((cmd (beads-command-label-add :issue-ids '("bd-1") :label "bug")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-label-test-add-validate-non-string-ids ()
  "Test label-add validation fails with non-string issue IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-label-add :issue-ids '("bd-1" 42) :label "bug")))
    (should (beads-command-validate cmd))))

;;; Tests for label-remove validation

(ert-deftest beads-label-test-remove-validate-no-issue ()
  "Test label-remove validation fails without issue IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-label-remove :label "bug")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-label-test-remove-validate-no-label ()
  "Test label-remove validation fails without label."
  :tags '(:unit)
  (let ((cmd (beads-command-label-remove :issue-ids '("bd-1"))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-label-test-remove-validate-valid ()
  "Test label-remove validation passes with valid args."
  :tags '(:unit)
  (let ((cmd (beads-command-label-remove :issue-ids '("bd-1") :label "bug")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for label-add execute-interactive

(ert-deftest beads-label-test-add-execute-interactive ()
  "Test label-add execute-interactive calls execute and messages."
  :tags '(:unit)
  (let ((cmd (beads-command-label-add :issue-ids '("bd-1") :label "bug"))
        (exec-called nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (c)
                 (setq exec-called t)
                 (beads-command-execution :command c :exit-code 0
                                          :stdout "" :stderr "")))
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore))
      (beads-command-execute-interactive cmd)
      (should exec-called))))

;;; Tests for label-remove execute-interactive

(ert-deftest beads-label-test-remove-execute-interactive ()
  "Test label-remove execute-interactive calls execute and messages."
  :tags '(:unit)
  (let ((cmd (beads-command-label-remove :issue-ids '("bd-1") :label "bug"))
        (exec-called nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (c)
                 (setq exec-called t)
                 (beads-command-execution :command c :exit-code 0
                                          :stdout "" :stderr "")))
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore))
      (beads-command-execute-interactive cmd)
      (should exec-called))))

;;; Tests for label-list command

(ert-deftest beads-label-test-list-class-exists ()
  "Test label-list class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-label-list)))

(ert-deftest beads-label-test-list-subcommand ()
  "Test label-list subcommand."
  :tags '(:unit)
  (let ((cmd (beads-command-label-list)))
    (should (string= (beads-command-subcommand cmd) "label list"))))

(ert-deftest beads-label-test-list-command-line ()
  "Test label-list command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-label-list :issue-id "bd-1" :json t))
         (args (beads-command-line cmd)))
    (should (member "label" args))
    (should (member "list" args))
    (should (member "--json" args))
    (should (member "bd-1" args))))

;;; Tests for beads-command-label-list validate (lines 251-254)

(ert-deftest beads-label-test-list-validate-no-issue-id ()
  "Test label-list validate fails when issue-id is nil."
  :tags '(:unit)
  (let ((cmd (beads-command-label-list :issue-id nil)))
    (should (equal (beads-command-validate cmd) "Must provide issue ID"))))

(ert-deftest beads-label-test-list-validate-empty-issue-id ()
  "Test label-list validate fails when issue-id is empty string."
  :tags '(:unit)
  (let ((cmd (beads-command-label-list :issue-id "")))
    (should (equal (beads-command-validate cmd) "Must provide issue ID"))))

(ert-deftest beads-label-test-list-validate-valid-issue-id ()
  "Test label-list validate passes with a valid issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-label-list :issue-id "bd-42")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for label-add execute in list-mode context (lines 431-434)

(ert-deftest beads-label-test-add-execute-refreshes-list-buffer ()
  "Test that label-add execute refreshes beads-list buffer."
  :tags '(:unit)
  (let ((list-refreshed nil)
        (message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-label-cache)
               #'ignore)
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore)
              ((symbol-function 'beads-list-refresh)
               (lambda (_force) (setq list-refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (with-temp-buffer
        (beads-list-mode)
        (setq-local beads-list--command 'list)
        (beads-label-add--execute)
        (should list-refreshed)))))

(ert-deftest beads-label-test-add-execute-refreshes-show-buffer ()
  "Test that label-add execute refreshes beads-show buffer."
  :tags '(:unit)
  (let ((show-refreshed nil)
        (message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-label-cache)
               #'ignore)
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore)
              ((symbol-function 'beads-refresh-show)
               (lambda () (setq show-refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (with-temp-buffer
        (beads-show-mode)
        (beads-label-add--execute)
        (should show-refreshed)))))

;;; Tests for label-remove execute in list/show context (lines 546-555)

(ert-deftest beads-label-test-remove-execute-refreshes-list-buffer ()
  "Test that label-remove execute refreshes beads-list buffer."
  :tags '(:unit)
  (let ((list-refreshed nil)
        (message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-label-cache)
               #'ignore)
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore)
              ((symbol-function 'beads-list-refresh)
               (lambda (_force) (setq list-refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (with-temp-buffer
        (beads-list-mode)
        (setq-local beads-list--command 'list)
        (beads-label-remove--execute)
        (should list-refreshed)))))

(ert-deftest beads-label-test-remove-execute-refreshes-show-buffer ()
  "Test that label-remove execute refreshes beads-show buffer."
  :tags '(:unit)
  (let ((show-refreshed nil)
        (message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-label-cache)
               #'ignore)
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore)
              ((symbol-function 'beads-refresh-show)
               (lambda () (setq show-refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (with-temp-buffer
        (beads-show-mode)
        (beads-label-remove--execute)
        (should show-refreshed)))))

(ert-deftest beads-label-test-remove-execute-command-error ()
  "Test that label-remove execute handles command errors gracefully."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--issue-ids=bd-1" "--label=bug")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (error "Command failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (let ((result (beads-label-remove--execute)))
        (should (stringp result))
        (should (string-match-p "Failed to remove label" result))))))

;;; Tests for beads-label-list and beads-label-list-interactive (lines 595-610)

(ert-deftest beads-label-test-label-list-calls-execute ()
  "Test beads-label-list creates command and calls execute."
  :tags '(:unit)
  (let ((exec-cmd nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq exec-cmd cmd)
                 (beads-command-execution
                  :command cmd :exit-code 0
                  :stdout "" :stderr ""))))
      (beads-label-list "bd-42")
      (should exec-cmd)
      (should (beads-command-label-list-p exec-cmd))
      (should (equal (oref exec-cmd issue-id) "bd-42")))))

(ert-deftest beads-label-test-label-list-interactive-with-labels ()
  "Test beads-label-list-interactive shows labels when present."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'beads-label-list)
               (lambda (_id) '("bug" "feature" "docs")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-label-list-interactive "bd-42")
      (should message-output)
      (should (string-match-p "Labels for bd-42" message-output))
      (should (string-match-p "bug" message-output))
      (should (string-match-p "feature" message-output))
      (should (string-match-p "docs" message-output)))))

(ert-deftest beads-label-test-label-list-interactive-no-labels ()
  "Test beads-label-list-interactive shows message when no labels."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'beads-label-list)
               (lambda (_id) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-label-list-interactive "bd-42")
      (should message-output)
      (should (string-match-p "No labels for bd-42" message-output)))))

;;; Tests for beads-label-list-all-show-issues no-issues branch (lines 636-643)

(ert-deftest beads-label-test-show-issues-no-results ()
  "Test show-issues displays empty state when no issues match label."
  :tags '(:unit)
  (beads-label-test--with-mock-project
   (lambda ()
     (let ((expected-buf-name (beads-buffer-name-list
                               nil "label=nonexistent")))
       (cl-letf (((symbol-function 'tabulated-list-get-id)
                  (lambda () "nonexistent"))
                 ((symbol-function 'beads-command-execute)
                  (lambda (_cmd) nil))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (_buffer) nil)))
         (let ((default-directory "/tmp/"))
           (beads-label-list-all-show-issues)
           ;; Should create buffer
           (should (get-buffer expected-buf-name))
           ;; Buffer should have empty entries
           (with-current-buffer expected-buf-name
             (should (null tabulated-list-entries)))
           (kill-buffer expected-buf-name)))))))

;;; Tests for label propagate command

(ert-deftest beads-label-test-propagate-command-class-exists ()
  "Test that beads-command-label-propagate class is defined."
  :tags '(:unit)
  (should (fboundp 'beads-command-label-propagate)))

(ert-deftest beads-label-test-propagate-command-line-basic ()
  "Test label propagate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-label-propagate
               :parent-id "bd-1" :label "feature"))
         (args (beads-command-line cmd)))
    (should (member "label" args))
    (should (member "propagate" args))
    (should (member "bd-1" args))
    (should (member "feature" args))))

(ert-deftest beads-label-test-propagate-validation-missing-parent ()
  "Test label propagate validation fails without parent ID."
  :tags '(:unit)
  (let ((cmd (beads-command-label-propagate :label "feature")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-label-test-propagate-validation-missing-label ()
  "Test label propagate validation fails without label."
  :tags '(:unit)
  (let ((cmd (beads-command-label-propagate :parent-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-label-test-propagate-validation-success ()
  "Test label propagate validation succeeds with parent ID and label."
  :tags '(:unit)
  (let ((cmd (beads-command-label-propagate
              :parent-id "bd-1" :label "feature")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-label-test-propagate-transient-defined ()
  "Test that beads-label-propagate transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-label-propagate)))

(ert-deftest beads-label-test-propagate-transient-is-prefix ()
  "Test that beads-label-propagate is a transient prefix."
  :tags '(:unit)
  (should (get 'beads-label-propagate 'transient--prefix)))

(ert-deftest beads-label-test-label-menu-has-propagate ()
  "Test that beads-label menu includes propagate command."
  :tags '(:unit)
  (let* ((layout (get 'beads-label 'transient--layout))
         (layout-str (format "%s" layout)))
    (should (string-match-p "beads-label-propagate" layout-str))))

(provide 'beads-label-test)
;;; beads-label-test.el ends here
