;;; beads-completion-test.el --- Tests for beads-completion.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-completion.el functionality including:
;; - Custom completion style `beads-issue-title'
;; - Title-aware matching
;; - Text property handling on candidates
;; - Annotation and grouping functions
;; - Cache management

;;; Code:

(require 'ert)
(require 'beads-completion)
(require 'beads-types)

;;; Test Helpers

(defun beads-completion-test--make-issue (id title status &optional priority type)
  "Create a mock beads-issue object for testing.
ID is the issue ID, TITLE is the issue title, STATUS is the status string,
PRIORITY is the priority (default 2), TYPE is the issue type (default \"task\")."
  (beads-issue :id id
               :title title
               :status status
               :priority (or priority 2)
               :issue-type (or type "task")))

(defun beads-completion-test--make-mock-issues ()
  "Create a list of mock issues for testing."
  (list
   (beads-completion-test--make-issue "bd-abc1" "Fix authentication bug" "open" 1 "bug")
   (beads-completion-test--make-issue "bd-def2" "Add user dashboard" "in_progress" 2 "feature")
   (beads-completion-test--make-issue "bd-ghi3" "Refactor database queries" "blocked" 3 "task")
   (beads-completion-test--make-issue "bd-jkl4" "Update documentation" "closed" 4 "chore")))

;;; Completion Style Tests

(ert-deftest beads-completion-test-style-match-id ()
  "Test that completion style matches on issue ID."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (matches (beads-completion--issue-style-all "bd-abc" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "bd-abc1" (car matches))))))

(ert-deftest beads-completion-test-style-match-title ()
  "Test that completion style matches on issue title."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (matches (beads-completion--issue-style-all "authentication" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "bd-abc1" (car matches))))))

(ert-deftest beads-completion-test-style-match-title-partial ()
  "Test that completion style matches partial title."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (matches (beads-completion--issue-style-all "dashboard" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "bd-def2" (car matches))))))

(ert-deftest beads-completion-test-style-case-insensitive ()
  "Test that completion matching is case-insensitive."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (matches-lower (beads-completion--issue-style-all "authentication" table nil nil))
           (matches-upper (beads-completion--issue-style-all "AUTHENTICATION" table nil nil))
           (matches-mixed (beads-completion--issue-style-all "Authentication" table nil nil)))
      (should (= 1 (length matches-lower)))
      (should (= 1 (length matches-upper)))
      (should (= 1 (length matches-mixed)))
      (should (string= "bd-abc1" (car matches-lower)))
      (should (string= "bd-abc1" (car matches-upper)))
      (should (string= "bd-abc1" (car matches-mixed))))))

(ert-deftest beads-completion-test-style-match-multiple ()
  "Test that style returns multiple matches when appropriate."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (matches (beads-completion--issue-style-all "bd-" table nil nil)))
      (should (= 4 (length matches))))))

(ert-deftest beads-completion-test-style-no-match ()
  "Test that style returns empty list when no match."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (matches (beads-completion--issue-style-all "nonexistent" table nil nil)))
      (should (= 0 (length matches))))))

(ert-deftest beads-completion-test-style-try-single-match ()
  "Test try-completion returns single match when only one."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (result (beads-completion--issue-style-try "authentication" table nil nil)))
      (should (string= "bd-abc1" result)))))

(ert-deftest beads-completion-test-style-try-multiple-matches ()
  "Test try-completion returns input string when multiple matches."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (result (beads-completion--issue-style-try "bd-" table nil nil)))
      (should (string= "bd-" result)))))

(ert-deftest beads-completion-test-style-try-exact-match ()
  "Test try-completion returns t for exact unique match."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (result (beads-completion--issue-style-try "bd-abc1" table nil nil)))
      (should (eq t result)))))

(ert-deftest beads-completion-test-style-try-no-match ()
  "Test try-completion returns nil when no match."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (result (beads-completion--issue-style-try "nonexistent" table nil nil)))
      (should (null result)))))

;;; Text Property Tests

(ert-deftest beads-completion-test-candidates-have-properties ()
  "Test that completion candidates have expected text properties."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (candidates (all-completions "" table nil)))
      (should (= 4 (length candidates)))
      (dolist (candidate candidates)
        (should (get-text-property 0 'beads-title candidate))
        (should (get-text-property 0 'beads-issue candidate))))))

(ert-deftest beads-completion-test-candidate-title-property ()
  "Test that beads-title property contains the correct title."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (candidates (all-completions "" table nil))
           (auth-candidate (seq-find (lambda (c) (string= "bd-abc1" c)) candidates)))
      (should auth-candidate)
      (should (string= "Fix authentication bug"
                       (get-text-property 0 'beads-title auth-candidate))))))

;;; Annotation Function Tests

(ert-deftest beads-completion-test-annotation-format ()
  "Test that annotation returns expected format."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (candidates (all-completions "" table nil))
           (open-candidate (seq-find (lambda (c) (string= "bd-abc1" c)) candidates))
           (annotation (beads-completion--issue-annotate open-candidate)))
      (should annotation)
      (should (string-match-p "\\[P1\\]" annotation))
      (should (string-match-p "\\[bug\\]" annotation))
      (should (string-match-p "OPEN" annotation))
      (should (string-match-p "Fix authentication bug" annotation)))))

(ert-deftest beads-completion-test-annotation-truncates-long-titles ()
  "Test that annotation truncates titles longer than 50 characters."
  (let* ((long-title "This is a very long title that exceeds fifty characters significantly")
         (issues (list (beads-completion-test--make-issue "bd-long" long-title "open")))
         (beads-completion--cache (cons (float-time) issues)))
    (let* ((table (beads-completion-issue-table))
           (candidates (all-completions "" table nil))
           (annotation (beads-completion--issue-annotate (car candidates))))
      (should annotation)
      (should (string-match-p "\\.\\.\\." annotation))
      (should (< (length annotation) (+ 50 30)))))) ; 50 for title + overhead

(ert-deftest beads-completion-test-annotation-handles-nil-gracefully ()
  "Test that annotation handles invalid input gracefully.
Annotation functions may return nil or empty string for missing data."
  ;; For strings without text properties, returns nil (no annotation)
  (should (null (beads-completion--issue-annotate "nonexistent-id")))
  ;; For nil input, returns empty string due to error handling
  (should (string= "" (beads-completion--issue-annotate nil))))

;;; Group Function Tests

(ert-deftest beads-completion-test-group-by-status ()
  "Test that grouping function groups by status correctly."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (candidates (all-completions "" table nil)))
      ;; Find candidates by status
      (let ((open-candidate (seq-find (lambda (c) (string= "bd-abc1" c)) candidates))
            (in-progress-candidate (seq-find (lambda (c) (string= "bd-def2" c)) candidates))
            (blocked-candidate (seq-find (lambda (c) (string= "bd-ghi3" c)) candidates))
            (closed-candidate (seq-find (lambda (c) (string= "bd-jkl4" c)) candidates)))
        (should (string= "Open" (beads-completion--issue-group open-candidate nil)))
        (should (string= "In Progress" (beads-completion--issue-group in-progress-candidate nil)))
        (should (string= "Blocked" (beads-completion--issue-group blocked-candidate nil)))
        (should (string= "Closed" (beads-completion--issue-group closed-candidate nil)))))))

(ert-deftest beads-completion-test-group-transform ()
  "Test that group function returns candidate when transform is non-nil."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (candidates (all-completions "" table nil))
           (candidate (car candidates)))
      (should (string= candidate (beads-completion--issue-group candidate t))))))

;;; Cache Tests

(ert-deftest beads-completion-test-cache-invalidation ()
  "Test that cache can be invalidated."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (should beads-completion--cache)
    (beads-completion-invalidate-cache)
    (should (null beads-completion--cache))))

;;; Truncate Utility Tests

(ert-deftest beads-completion-test-truncate-string-short ()
  "Test truncation of strings shorter than max."
  (should (string= "short" (beads-completion--truncate-string "short" 10))))

(ert-deftest beads-completion-test-truncate-string-long ()
  "Test truncation of strings longer than max."
  (let ((result (beads-completion--truncate-string "this is a long string" 10)))
    (should (= 10 (length result)))
    (should (string-suffix-p "..." result))))

(ert-deftest beads-completion-test-truncate-string-nil ()
  "Test truncation handles nil gracefully."
  (should (string= "" (beads-completion--truncate-string nil 10))))

;;; Style Registration Tests
;;
;; Note: Custom style registration is currently disabled due to
;; compatibility issues with Emacs completion machinery.
;; The basic completion style works correctly for CAPF.

;;; Metadata Tests

(ert-deftest beads-completion-test-table-metadata ()
  "Test that completion table provides correct metadata."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let* ((table (beads-completion-issue-table))
           (metadata (funcall table "" nil 'metadata)))
      (should (eq 'metadata (car metadata)))
      (should (eq 'beads-issue (cdr (assq 'category metadata))))
      (should (eq 'beads-completion--issue-annotate
                  (cdr (assq 'annotation-function metadata))))
      (should (eq 'beads-completion--issue-group
                  (cdr (assq 'group-function metadata)))))))

;;; Backward Compatibility Tests

(ert-deftest beads-completion-test-alias-issue-completion-table ()
  "Test that beads--issue-completion-table alias works."
  (should (fboundp 'beads--issue-completion-table))
  ;; Alias should return a working completion table
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (let ((table (beads--issue-completion-table)))
      (should (functionp table))
      ;; Should work the same as the original
      (let ((candidates (all-completions "" table nil)))
        (should (= 4 (length candidates)))))))

(ert-deftest beads-completion-test-alias-invalidate-cache ()
  "Test that beads--invalidate-completion-cache alias works."
  (should (fboundp 'beads--invalidate-completion-cache))
  ;; Alias should invalidate the cache
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (should beads-completion--cache)
    (beads--invalidate-completion-cache)
    (should (null beads-completion--cache))))

;;; Completion-at-Point (CAPF) Tests

(ert-deftest beads-completion-test-capf-returns-nil-in-empty-buffer ()
  "Test CAPF returns nil when buffer is empty."
  (with-temp-buffer
    (should (null (beads-completion-at-point)))))

(ert-deftest beads-completion-test-capf-returns-nil-at-whitespace ()
  "Test CAPF returns nil when at whitespace."
  (with-temp-buffer
    (insert "   ")
    (should (null (beads-completion-at-point)))))

(ert-deftest beads-completion-test-capf-returns-nil-at-number ()
  "Test CAPF returns nil when at a plain number (not starting with letter)."
  (with-temp-buffer
    (insert "12345")
    (should (null (beads-completion-at-point)))))

(ert-deftest beads-completion-test-capf-returns-nil-single-char ()
  "Test CAPF returns nil when text is only one character."
  (with-temp-buffer
    (insert "b")
    (should (null (beads-completion-at-point)))))

(ert-deftest beads-completion-test-capf-triggers-with-two-chars ()
  "Test CAPF triggers with 2+ characters."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (with-temp-buffer
      (insert "bd")
      (should (beads-completion-at-point)))))

(ert-deftest beads-completion-test-capf-at-partial-id ()
  "Test CAPF returns correct bounds and table at partial issue ID."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (with-temp-buffer
      (insert "bd-ab")
      (let ((result (beads-completion-at-point)))
        (should result)
        (should (= 1 (nth 0 result)))  ; START
        (should (= 6 (nth 1 result)))  ; END (point-max)
        (should (functionp (nth 2 result)))  ; TABLE
        (should (eq 'no (plist-get (nthcdr 3 result) :exclusive)))))))

(ert-deftest beads-completion-test-capf-at-project-prefix ()
  "Test CAPF works with various project prefixes."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    ;; Test with beads.el- prefix
    (with-temp-buffer
      (insert "beads.el-")
      (let ((result (beads-completion-at-point)))
        (should result)
        (should (= 1 (nth 0 result)))
        (should (= 10 (nth 1 result)))))
    ;; Test with worker- prefix
    (with-temp-buffer
      (insert "worker-")
      (let ((result (beads-completion-at-point)))
        (should result)
        (should (= 1 (nth 0 result)))
        (should (= 8 (nth 1 result)))))))

(ert-deftest beads-completion-test-capf-mid-word ()
  "Test CAPF detects ID when cursor is in the middle of text."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (with-temp-buffer
      (insert "bd-abc1")
      (goto-char 4)  ; Position after "bd-"
      (let ((result (beads-completion-at-point)))
        (should result)
        (should (= 1 (nth 0 result)))
        (should (= 8 (nth 1 result)))))))

(ert-deftest beads-completion-test-capf-with-surrounding-text ()
  "Test CAPF works when issue ID is surrounded by other text."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (with-temp-buffer
      (insert "See issue bd-abc for details")
      (goto-char 16)  ; Position at "bd-abc"
      (let ((result (beads-completion-at-point)))
        (should result)
        (should (= 11 (nth 0 result)))
        (should (= 17 (nth 1 result)))))))

(ert-deftest beads-completion-test-capf-exclusive-no ()
  "Test CAPF sets :exclusive to no."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (with-temp-buffer
      (insert "bd-")
      (let ((result (beads-completion-at-point)))
        (should result)
        (should (eq 'no (plist-get (nthcdr 3 result) :exclusive)))))))

;;; Minor Mode Tests

(ert-deftest beads-completion-test-mode-adds-to-capf ()
  "Test that enabling beads-completion-mode adds to completion-at-point-functions."
  (unwind-protect
      (progn
        (beads-completion-mode 1)
        ;; Should be in buffer-local value
        (should (memq 'beads-completion-at-point
                      (buffer-local-value 'completion-at-point-functions
                                          (current-buffer)))))
    (beads-completion-mode -1)))

(ert-deftest beads-completion-test-mode-removes-from-capf ()
  "Test that disabling beads-completion-mode removes from completion-at-point-functions."
  (beads-completion-mode 1)
  (beads-completion-mode -1)
  (should-not (memq 'beads-completion-at-point
                    (buffer-local-value 'completion-at-point-functions
                                        (current-buffer)))))

(provide 'beads-completion-test)
;;; beads-completion-test.el ends here
