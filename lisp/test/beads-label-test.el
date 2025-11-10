;;; beads-label-test.el --- Tests for beads-label.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for beads-label.el label management functionality.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-label)
(require 'beads-test-helper)

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
        (should (member "backend" result))
        (should (member "frontend" result))
        (should (member "bug" result))))))

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
        (should (member "backend" result1))

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

(provide 'beads-label-test)
;;; beads-label-test.el ends here
