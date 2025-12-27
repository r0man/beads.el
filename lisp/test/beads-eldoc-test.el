;;; beads-eldoc-test.el --- Tests for beads-eldoc.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-eldoc.el including:
;; - Issue reference detection at point
;; - Cache management (TTL, invalidation)
;; - Issue fetching with error handling
;; - Eldoc documentation formatting
;; - Minor mode activation/deactivation
;; - Integration with beads completion cache invalidation

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-eldoc)
(require 'beads-types)

;;; Test Fixtures

(defvar beads-eldoc-test--sample-issue
  (beads-issue :id "beads.el-22"
               :title "Add eldoc support for beads issue references"
               :description "When cursor is on a beads.el-N reference..."
               :status "in_progress"
               :priority 3
               :issue-type "feature"
               :created-at "2025-10-20T17:34:01.191949541Z"
               :updated-at "2025-10-20T17:34:01.191949541Z"
               :assignee "claude"
               :notes "Working on implementation")
  "Sample issue for testing.")

(defvar beads-eldoc-test--sample-issue-bd
  (beads-issue :id "bd-123"
               :title "Fix bug in parser"
               :description "Parser fails on edge case"
               :status "open"
               :priority 1
               :issue-type "bug"
               :created-at "2025-10-19T10:00:00Z"
               :updated-at "2025-10-19T10:00:00Z")
  "Sample bd-prefixed issue for testing.")

;;; Test Utilities

(defun beads-eldoc-test--with-temp-buffer-and-point (content point-marker fn)
  "Execute FN in temp buffer with CONTENT and point at POINT-MARKER.
POINT-MARKER should be a string like '|' that indicates where
point should be positioned."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (when (search-forward point-marker nil t)
      (delete-char -1))
    (funcall fn)))

(defmacro beads-eldoc-test--at-point (content point-marker &rest body)
  "Execute BODY in temp buffer with CONTENT and point at POINT-MARKER."
  (declare (indent 2))
  `(beads-eldoc-test--with-temp-buffer-and-point
    ,content ,point-marker (lambda () ,@body)))

;;; ========================================
;;; Issue Reference Detection Tests
;;; ========================================

(ert-deftest beads-eldoc-test-issue-id-at-point-beads-prefix ()
  "Test detection of beads.el-N format at point."
  (beads-eldoc-test--at-point "See beads.el-|22 for details" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "beads.el-22"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-bd-prefix ()
  "Test detection of bd-N format at point."
  (beads-eldoc-test--at-point "Fixed in bd-|123" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "bd-123"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-beginning ()
  "Test detection when point is at beginning of issue ID."
  (beads-eldoc-test--at-point "|beads.el-42" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "beads.el-42"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-middle ()
  "Test detection when point is in middle of issue ID."
  (beads-eldoc-test--at-point "beads.el-4|2" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "beads.el-42"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-end ()
  "Test detection when point is at end of issue ID."
  (beads-eldoc-test--at-point "beads.el-42|" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "beads.el-42"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-in-comment ()
  "Test detection in code comment."
  (beads-eldoc-test--at-point
      "  ;; TODO: Fix beads.el-|99\n" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "beads.el-99"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-in-markdown ()
  "Test detection in markdown text."
  (beads-eldoc-test--at-point
      "- Implemented beads.el-|50\n- Fixed bd-|25\n" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "beads.el-50"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-not-found ()
  "Test that nil is returned when not on issue reference."
  (beads-eldoc-test--at-point "No issue |here" "|"
    (should (null (beads-eldoc--issue-id-at-point)))))

(ert-deftest beads-eldoc-test-issue-id-at-point-partial-match ()
  "Test that partial matches are not detected."
  (beads-eldoc-test--at-point "beads|.el" "|"
    (should (null (beads-eldoc--issue-id-at-point)))))

(ert-deftest beads-eldoc-test-issue-id-at-point-uppercase ()
  "Test that uppercase issue IDs are detected.
While issue IDs are typically lowercase by convention, the pattern
supports uppercase characters."
  (beads-eldoc-test--at-point "BEADS.EL-|22" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "BEADS.EL-22"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-worker-prefix ()
  "Test detection of worker-N format at point."
  (beads-eldoc-test--at-point "See worker-|42 for details" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "worker-42"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-api-prefix ()
  "Test detection of api-N format at point."
  (beads-eldoc-test--at-point "Fixed in api-|123" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "api-123"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-underscore-prefix ()
  "Test detection of prefix with underscores."
  (beads-eldoc-test--at-point "Related to my_project-|99" "|"
    (should (equal (beads-eldoc--issue-id-at-point) "my_project-99"))))

(ert-deftest beads-eldoc-test-issue-id-at-point-button-property ()
  "Test that button property is checked first."
  (with-temp-buffer
    (insert "See beads.el-22 here")
    (goto-char (point-min))
    (search-forward "beads.el-22")
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (make-button start end 'issue-id "custom-999")
      (goto-char (+ start 5))
      ;; Should return button property, not text match
      (should (equal (beads-eldoc--issue-id-at-point) "custom-999")))))

;;; ========================================
;;; Cache Management Tests
;;; ========================================

(ert-deftest beads-eldoc-test-cache-issue ()
  "Test caching an issue."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (beads-eldoc--cache-issue "beads.el-22"
                              beads-eldoc-test--sample-issue)
    (let ((cached (beads-eldoc--get-cached-issue "beads.el-22")))
      (should (equal cached beads-eldoc-test--sample-issue)))))

(ert-deftest beads-eldoc-test-cache-miss ()
  "Test cache miss returns nil."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (should (null (beads-eldoc--get-cached-issue "nonexistent")))))

(ert-deftest beads-eldoc-test-cache-ttl ()
  "Test that stale cache entries are removed."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (beads-eldoc-cache-ttl 1))
    ;; Cache an issue with old timestamp
    (puthash "beads.el-1"
             (list :timestamp (- (float-time) 2)
                   :issue beads-eldoc-test--sample-issue)
             beads-eldoc--cache)
    ;; Should return nil because entry is stale
    (should (null (beads-eldoc--get-cached-issue "beads.el-1")))
    ;; Entry should be removed from cache
    (should (null (gethash "beads.el-1" beads-eldoc--cache)))))

(ert-deftest beads-eldoc-test-cache-invalidate-single ()
  "Test invalidating a single cache entry."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (beads-eldoc--cache-issue "beads.el-22"
                              beads-eldoc-test--sample-issue)
    (beads-eldoc--cache-issue "beads.el-23"
                              beads-eldoc-test--sample-issue-bd)
    (beads-eldoc--invalidate-cache "beads.el-22")
    ;; First issue should be invalidated
    (should (null (gethash "beads.el-22" beads-eldoc--cache)))
    ;; Second issue should still be cached
    (should (gethash "beads.el-23" beads-eldoc--cache))))

(ert-deftest beads-eldoc-test-cache-invalidate-all ()
  "Test invalidating entire cache."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (beads-eldoc--cache-issue "beads.el-22"
                              beads-eldoc-test--sample-issue)
    (beads-eldoc--cache-issue "bd-123"
                              beads-eldoc-test--sample-issue-bd)
    (beads-eldoc--invalidate-cache)
    ;; Both entries should be gone
    (should (zerop (hash-table-count beads-eldoc--cache)))))

;;; ========================================
;;; Issue Fetching Tests
;;; ========================================

(ert-deftest beads-eldoc-test-fetch-issue-success ()
  "Test successful issue fetching with caching."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'beads-command-show!)
               (lambda (&rest _)
                 beads-eldoc-test--sample-issue)))
      (let ((issue (beads-eldoc--fetch-issue "beads.el-22")))
        ;; Should return parsed issue
        (should (equal (oref issue id) "beads.el-22"))
        ;; Should be cached
        (should (beads-eldoc--get-cached-issue "beads.el-22"))))))

(ert-deftest beads-eldoc-test-fetch-issue-uses-cache ()
  "Test that fetch uses cache on second call."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (call-count 0))
    (cl-letf (((symbol-function 'beads-command-show!)
               (lambda (&rest _)
                 (setq call-count (1+ call-count))
                 beads-eldoc-test--sample-issue)))
      ;; First call should fetch from bd
      (beads-eldoc--fetch-issue "beads.el-22")
      (should (= call-count 1))
      ;; Second call should use cache
      (beads-eldoc--fetch-issue "beads.el-22")
      (should (= call-count 1)))))

(ert-deftest beads-eldoc-test-fetch-issue-error ()
  "Test that fetch errors are handled gracefully."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'beads-command-show!)
               (lambda (&rest _)
                 (error "Command failed"))))
      ;; Should return nil on error, not signal
      (should (null (beads-eldoc--fetch-issue "beads.el-999"))))))

;;; ========================================
;;; Formatting Tests
;;; ========================================

(ert-deftest beads-eldoc-test-format-echo-area ()
  "Test echo area formatting."
  (let ((result (beads-eldoc--format-echo-area
                 beads-eldoc-test--sample-issue)))
    (should (string-match-p "beads\\.el-22" result))
    (should (string-match-p "in_progress" result))
    (should (string-match-p "Add eldoc support" result))))

(ert-deftest beads-eldoc-test-format-doc-buffer ()
  "Test documentation buffer formatting."
  (let ((result (beads-eldoc--format-doc-buffer
                 beads-eldoc-test--sample-issue)))
    (should (string-match-p "Issue: beads\\.el-22" result))
    (should (string-match-p "Title: Add eldoc support" result))
    (should (string-match-p "Status: in_progress" result))
    (should (string-match-p "Type: feature" result))
    (should (string-match-p "Priority: 3" result))
    (should (string-match-p "Assignee: claude" result))
    (should (string-match-p "Description:" result))
    (should (string-match-p "Notes:" result))))

(ert-deftest beads-eldoc-test-format-doc-buffer-minimal ()
  "Test documentation buffer formatting with minimal issue data."
  (let* ((minimal-issue (beads-issue :id "bd-1"
                                     :title "Test"
                                     :status "open"
                                     :priority 1
                                     :issue-type "bug"))
         (result (beads-eldoc--format-doc-buffer minimal-issue)))
    (should (string-match-p "Issue: bd-1" result))
    (should (string-match-p "Title: Test" result))
    ;; Should not include empty sections
    (should-not (string-match-p "Assignee:" result))
    (should-not (string-match-p "Description:" result))))

;;; ========================================
;;; Eldoc Function Tests
;;; ========================================

(ert-deftest beads-eldoc-test-eldoc-function-success ()
  "Test eldoc function calls callback with documentation."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (callback-called nil)
        (callback-args nil))
    (cl-letf (((symbol-function 'beads-eldoc--issue-id-at-point)
               (lambda () "beads.el-22"))
              ((symbol-function 'beads-eldoc--fetch-issue)
               (lambda (_) beads-eldoc-test--sample-issue)))
      (beads-eldoc-function
       (lambda (&rest args)
         (setq callback-called t)
         (setq callback-args args)))
      ;; Callback should be called
      (should callback-called)
      ;; First arg should be echo area string
      (should (string-match-p "beads\\.el-22" (car callback-args))))))

(ert-deftest beads-eldoc-test-eldoc-function-no-issue-at-point ()
  "Test eldoc function when not on issue reference."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'beads-eldoc--issue-id-at-point)
               (lambda () nil)))
      (beads-eldoc-function (lambda (&rest _) (setq callback-called t)))
      ;; Callback should not be called
      (should-not callback-called))))

(ert-deftest beads-eldoc-test-eldoc-function-fetch-error ()
  "Test eldoc function when fetch fails."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'beads-eldoc--issue-id-at-point)
               (lambda () "beads.el-999"))
              ((symbol-function 'beads-eldoc--fetch-issue)
               (lambda (_) nil)))
      (beads-eldoc-function (lambda (&rest _) (setq callback-called t)))
      ;; Callback should not be called if fetch returns nil
      (should-not callback-called))))

;;; ========================================
;;; Minor Mode Tests
;;; ========================================

(ert-deftest beads-eldoc-test-mode-enable ()
  "Test enabling beads-eldoc-mode."
  (let ((beads-eldoc-mode nil))
    (beads-eldoc-mode 1)
    (should beads-eldoc-mode)
    ;; Cleanup
    (beads-eldoc-mode -1)))

(ert-deftest beads-eldoc-test-mode-disable ()
  "Test disabling beads-eldoc-mode."
  (beads-eldoc-mode 1)
  (beads-eldoc-mode -1)
  (should-not beads-eldoc-mode))

(ert-deftest beads-eldoc-test-mode-adds-eldoc-function ()
  "Test that mode adds eldoc documentation function."
  (let ((beads-eldoc-mode nil))
    (beads-eldoc-mode 1)
    (should (memq 'beads-eldoc-function
                  eldoc-documentation-functions))
    ;; Cleanup
    (beads-eldoc-mode -1)))

(ert-deftest beads-eldoc-test-mode-removes-eldoc-function ()
  "Test that mode removes eldoc documentation function."
  (beads-eldoc-mode 1)
  (beads-eldoc-mode -1)
  (should-not (memq 'beads-eldoc-function
                    eldoc-documentation-functions)))

(ert-deftest beads-eldoc-test-mode-clears-cache-on-disable ()
  "Test that disabling mode clears cache."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (beads-eldoc--cache-issue "beads.el-22"
                              beads-eldoc-test--sample-issue)
    (beads-eldoc-mode 1)
    (beads-eldoc-mode -1)
    ;; Cache should be cleared
    (should (zerop (hash-table-count beads-eldoc--cache)))))

;;; ========================================
;;; Integration Tests
;;; ========================================

(ert-deftest beads-eldoc-test-integration-cache-invalidation ()
  "Test that cache is invalidated when completion cache is invalidated."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (beads-eldoc-mode 1)
    ;; Cache an issue
    (beads-eldoc--cache-issue "beads.el-22"
                              beads-eldoc-test--sample-issue)
    (should (= (hash-table-count beads-eldoc--cache) 1))
    ;; Invalidate completion cache (simulates update/close)
    (beads--invalidate-completion-cache)
    ;; Eldoc cache should also be invalidated
    (should (zerop (hash-table-count beads-eldoc--cache)))
    ;; Cleanup
    (beads-eldoc-mode -1)))

(ert-deftest beads-eldoc-test-integration-full-workflow ()
  "Test full workflow: detect issue, fetch, format, display."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (result-echo nil)
        (result-buffer nil))
    (cl-letf (((symbol-function 'beads-command-show!)
               (lambda (&rest _)
                 beads-eldoc-test--sample-issue)))
      (beads-eldoc-test--at-point
          "Implementing beads.el-|22 now" "|"
        (beads-eldoc-function
         (lambda (echo &rest plist)
           (setq result-echo echo)
           (setq result-buffer (plist-get plist :buffer))))
        ;; Echo area should have brief info
        (should (string-match-p "beads\\.el-22" result-echo))
        (should (string-match-p "in_progress" result-echo))
        ;; Buffer should have full details
        (should (string-match-p "Issue: beads\\.el-22" result-buffer))
        (should (string-match-p "Description:" result-buffer))))))

(provide 'beads-eldoc-test)

;;; beads-eldoc-test.el ends here
