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
(require 'beads-agent-backend)

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

(ert-deftest beads-completion-test-cache-preserved-on-error ()
  "Test that stale cache is preserved when refresh fails."
  (let* ((mock-issues (beads-completion-test--make-mock-issues))
         ;; Use an old timestamp so refresh is attempted
         (beads-completion--cache (cons 0 mock-issues))
         (message-shown nil))
    (cl-letf (((symbol-function 'beads-command-list!)
               (lambda ()
                 (error "Connection failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-shown (apply #'format fmt args)))))
      ;; Get issues - should fail to refresh but return stale data
      (let ((result (beads-completion--get-cached-issues)))
        ;; Should return the stale cached issues
        (should (= (length result) (length mock-issues)))
        (should (equal (oref (car result) id) "bd-abc1"))
        ;; Should show warning
        (should message-shown)
        (should (string-match "using cached data" message-shown))))))

(ert-deftest beads-completion-test-nil-returned-when-no-cache-and-error ()
  "Test that nil is returned when refresh fails with no cache."
  (let ((beads-completion--cache nil))
    (cl-letf (((symbol-function 'beads-command-list!)
               (lambda ()
                 (error "Connection failed")))
              ;; Suppress any messages
              ((symbol-function 'message) #'ignore))
      ;; Get issues - should fail to refresh with no stale data
      (let ((result (beads-completion--get-cached-issues)))
        ;; Should return nil since no cache available
        (should (null result))))))

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

(ert-deftest beads-completion-test-style-registered ()
  "Test that beads-issue-title completion style is registered."
  (should (assq 'beads-issue-title completion-styles-alist)))

;;; Read Issue Function Tests

(ert-deftest beads-completion-test-read-issue-uses-title-style ()
  "Test that beads-completion-read-issue enables title-aware completion."
  (let ((beads-completion--cache (cons (float-time) (beads-completion-test--make-mock-issues))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt table &rest _args)
                 ;; Verify category override is set
                 (should (assq 'beads-issue completion-category-overrides))
                 ;; completion-category-overrides format: ((cat (styles ...)))
                 (let ((override (cdr (assq 'beads-issue completion-category-overrides))))
                   (should (member '(styles beads-issue-title basic) override)))
                 "bd-abc1")))
      (should (string= "bd-abc1" (beads-completion-read-issue "Test: "))))))

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

;;; Backend Completion Tests

;; Mock backend class for testing
(defclass beads-completion-test--mock-backend (beads-agent-backend)
  ((available :initarg :available :initform t))
  :documentation "Mock backend for completion tests.")

(cl-defmethod beads-agent-backend-available-p
  ((backend beads-completion-test--mock-backend))
  "Return availability status for mock BACKEND."
  (oref backend available))

(defun beads-completion-test--make-mock-backends ()
  "Create a list of mock backends for testing."
  (list
   (beads-completion-test--mock-backend
    :name "claudemacs" :priority 10 :available t
    :description "AI pair programming via eat terminal")
   (beads-completion-test--mock-backend
    :name "agent-shell" :priority 20 :available t
    :description "AI agent via shell integration")
   (beads-completion-test--mock-backend
    :name "dummy-backend" :priority 99 :available nil
    :description "Unavailable test backend")))

(ert-deftest beads-completion-test-backend-table-metadata ()
  "Test that backend completion table provides correct metadata."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (metadata (funcall table "" nil 'metadata)))
      (should (eq 'metadata (car metadata)))
      (should (eq 'beads-agent-backend (cdr (assq 'category metadata))))
      (should (eq 'beads-completion--backend-annotate
                  (cdr (assq 'annotation-function metadata))))
      (should (eq 'beads-completion--backend-group
                  (cdr (assq 'group-function metadata)))))))

(ert-deftest beads-completion-test-backend-table-candidates ()
  "Test that backend completion table returns all backends."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil)))
      (should (= 3 (length candidates)))
      (should (member "claudemacs" candidates))
      (should (member "agent-shell" candidates))
      (should (member "dummy-backend" candidates)))))

(ert-deftest beads-completion-test-backend-candidates-have-properties ()
  "Test that backend candidates have expected text properties."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil)))
      (dolist (candidate candidates)
        (should (get-text-property 0 'beads-backend candidate))
        ;; Check that beads-available property exists (it may be t or nil)
        (should (plist-member (text-properties-at 0 candidate) 'beads-available))))))

(ert-deftest beads-completion-test-backend-available-property ()
  "Test that beads-available property reflects actual availability."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil))
           (claudemacs (seq-find (lambda (c) (string= "claudemacs" c)) candidates))
           (dummy (seq-find (lambda (c) (string= "dummy-backend" c)) candidates)))
      (should (eq t (get-text-property 0 'beads-available claudemacs)))
      (should (eq nil (get-text-property 0 'beads-available dummy))))))

(ert-deftest beads-completion-test-backend-annotate-available ()
  "Test annotation for available backend includes description."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil))
           (claudemacs (seq-find (lambda (c) (string= "claudemacs" c)) candidates))
           (annotation (beads-completion--backend-annotate claudemacs)))
      (should annotation)
      (should (string-match-p "\\[P10\\]" annotation))
      (should (string-match-p "Available" annotation))
      (should (string-match-p "AI pair programming via eat terminal" annotation)))))

(ert-deftest beads-completion-test-backend-annotate-unavailable ()
  "Test annotation for unavailable backend includes description."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil))
           (dummy (seq-find (lambda (c) (string= "dummy-backend" c)) candidates))
           (annotation (beads-completion--backend-annotate dummy)))
      (should annotation)
      (should (string-match-p "\\[P99\\]" annotation))
      (should (string-match-p "Unavailable" annotation))
      (should (string-match-p "Unavailable test backend" annotation)))))

(ert-deftest beads-completion-test-backend-annotate-handles-nil ()
  "Test that backend annotation handles invalid input gracefully."
  (should (string= "" (beads-completion--backend-annotate nil)))
  (should (null (beads-completion--backend-annotate "nonexistent"))))

(ert-deftest beads-completion-test-backend-annotate-empty-description ()
  "Test annotation for backend with empty description."
  (let ((backends (list (beads-completion-test--mock-backend
                         :name "no-desc" :priority 50 :available t
                         :description ""))))
    (cl-letf (((symbol-function 'beads-agent--get-all-backends)
               (lambda () backends)))
      (let* ((table (beads-completion-backend-table))
             (candidates (all-completions "" table nil))
             (no-desc (car candidates))
             (annotation (beads-completion--backend-annotate no-desc)))
        (should annotation)
        (should (string-match-p "\\[P50\\]" annotation))
        (should (string-match-p "Available" annotation))
        ;; Should NOT contain " - " since description is empty
        (should-not (string-match-p " - " annotation))))))

(ert-deftest beads-completion-test-backend-group-available ()
  "Test grouping for available backends."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil))
           (claudemacs (seq-find (lambda (c) (string= "claudemacs" c)) candidates)))
      (should (string= "Available"
                       (beads-completion--backend-group claudemacs nil))))))

(ert-deftest beads-completion-test-backend-group-unavailable ()
  "Test grouping for unavailable backends."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil))
           (dummy (seq-find (lambda (c) (string= "dummy-backend" c)) candidates)))
      (should (string= "Unavailable"
                       (beads-completion--backend-group dummy nil))))))

(ert-deftest beads-completion-test-backend-group-transform ()
  "Test that group function returns candidate when transform is non-nil."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil))
           (candidate (car candidates)))
      (should (string= candidate
                       (beads-completion--backend-group candidate t))))))

;;; Show Unavailable Backends Customization Tests

(ert-deftest beads-completion-test-backend-table-shows-all-when-setting-t ()
  "Test that all backends are shown when show-unavailable is t."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends)
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda ()
               (seq-filter #'beads-agent-backend-available-p
                           (beads-completion-test--make-mock-backends)))))
    (let ((beads-completion-show-unavailable-backends t))
      (let* ((table (beads-completion-backend-table))
             (candidates (all-completions "" table nil)))
        (should (= 3 (length candidates)))
        (should (member "claudemacs" candidates))
        (should (member "agent-shell" candidates))
        (should (member "dummy-backend" candidates))))))

(ert-deftest beads-completion-test-backend-table-hides-unavailable-when-setting-nil ()
  "Test that only available backends are shown when show-unavailable is nil."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends)
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda ()
               (seq-filter #'beads-agent-backend-available-p
                           (beads-completion-test--make-mock-backends)))))
    (let ((beads-completion-show-unavailable-backends nil))
      (let* ((table (beads-completion-backend-table))
             (candidates (all-completions "" table nil)))
        (should (= 2 (length candidates)))
        (should (member "claudemacs" candidates))
        (should (member "agent-shell" candidates))
        (should-not (member "dummy-backend" candidates))))))

(ert-deftest beads-completion-test-backend-table-default-shows-all ()
  "Test that default value of show-unavailable-backends is t."
  ;; This test verifies the default behavior matches the documented default
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends)
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda ()
               (seq-filter #'beads-agent-backend-available-p
                           (beads-completion-test--make-mock-backends)))))
    ;; Use default value (should be t)
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil)))
      ;; With default t, all 3 backends should be visible
      (should (= 3 (length candidates))))))

(ert-deftest beads-completion-test-backend-available-property-when-hiding ()
  "Test that available property is correct even when hiding unavailable."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends)
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda ()
               (seq-filter #'beads-agent-backend-available-p
                           (beads-completion-test--make-mock-backends)))))
    (let ((beads-completion-show-unavailable-backends nil))
      (let* ((table (beads-completion-backend-table))
             (candidates (all-completions "" table nil)))
        ;; All candidates should have beads-available = t since we only show available
        (dolist (candidate candidates)
          (should (eq t (get-text-property 0 'beads-available candidate))))))))

;;; Backend Description-Aware Completion Tests

(ert-deftest beads-completion-test-backend-style-match-name ()
  "Test that backend completion style matches on name."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (matches (beads-completion--backend-style-all "claudemacs" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "claudemacs" (car matches))))))

(ert-deftest beads-completion-test-backend-style-match-description ()
  "Test that backend completion style matches on description."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (matches (beads-completion--backend-style-all "terminal" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "claudemacs" (car matches))))))

(ert-deftest beads-completion-test-backend-style-match-description-partial ()
  "Test that backend completion style matches partial description."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (matches (beads-completion--backend-style-all "shell integration" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "agent-shell" (car matches))))))

(ert-deftest beads-completion-test-backend-style-case-insensitive ()
  "Test that backend matching is case-insensitive."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (matches-lower (beads-completion--backend-style-all "terminal" table nil nil))
           (matches-upper (beads-completion--backend-style-all "TERMINAL" table nil nil))
           (matches-mixed (beads-completion--backend-style-all "Terminal" table nil nil)))
      (should (= 1 (length matches-lower)))
      (should (= 1 (length matches-upper)))
      (should (= 1 (length matches-mixed)))
      (should (string= "claudemacs" (car matches-lower)))
      (should (string= "claudemacs" (car matches-upper)))
      (should (string= "claudemacs" (car matches-mixed))))))

(ert-deftest beads-completion-test-backend-style-no-match ()
  "Test that backend style returns empty list when no match."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (matches (beads-completion--backend-style-all "nonexistent" table nil nil)))
      (should (= 0 (length matches))))))

(ert-deftest beads-completion-test-backend-style-try-single-match ()
  "Test try-completion returns single match when only one."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (result (beads-completion--backend-style-try "terminal" table nil nil)))
      (should (string= "claudemacs" result)))))

(ert-deftest beads-completion-test-backend-style-try-multiple-matches ()
  "Test try-completion returns input string when multiple matches."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           ;; "agent" matches both "agent-shell" (in name) and "claudemacs" (via "AI" in description)
           ;; Actually let's use a pattern that matches multiple backends
           (result (beads-completion--backend-style-try "AI" table nil nil)))
      ;; "AI" appears in descriptions of both claudemacs and agent-shell
      ;; Should return input unchanged when multiple matches
      (should (string= "AI" result)))))

(ert-deftest beads-completion-test-backend-style-try-exact-match ()
  "Test try-completion returns t for exact unique match."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (result (beads-completion--backend-style-try "claudemacs" table nil nil)))
      (should (eq t result)))))

(ert-deftest beads-completion-test-backend-style-try-no-match ()
  "Test try-completion returns nil when no match."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (result (beads-completion--backend-style-try "nonexistent" table nil nil)))
      (should (null result)))))

(ert-deftest beads-completion-test-backend-style-registered ()
  "Test that beads-backend-description completion style is registered."
  (should (assq 'beads-backend-description completion-styles-alist)))

(ert-deftest beads-completion-test-read-backend-uses-description-style ()
  "Test that beads-completion-read-backend enables description-aware completion."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends)
            ((symbol-function 'completing-read)
             (lambda (prompt table &rest _args)
               ;; Verify category override is set
               (should (assq 'beads-agent-backend completion-category-overrides))
               ;; completion-category-overrides format: ((cat (styles ...)))
               (let ((override (cdr (assq 'beads-agent-backend completion-category-overrides))))
                 (should (member '(styles beads-backend-description basic) override)))
               "claudemacs")))
    (should (string= "claudemacs" (beads-completion-read-backend "Test: ")))))

(ert-deftest beads-completion-test-backend-candidates-have-description-property ()
  "Test that backend candidates have description text property."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-completion-test--make-mock-backends))
    (let* ((table (beads-completion-backend-table))
           (candidates (all-completions "" table nil)))
      (dolist (candidate candidates)
        (should (plist-member (text-properties-at 0 candidate) 'beads-description))))))

;;; Worktree Completion Tests

(defun beads-completion-test--make-mock-worktree (name path branch is-main beads-state)
  "Create a mock beads-worktree object for testing.
NAME is the worktree name, PATH is the worktree path, BRANCH is the branch,
IS-MAIN is whether it's the main worktree, BEADS-STATE is the beads state."
  (require 'beads-command-worktree)
  (beads-worktree
   :name name
   :path path
   :branch branch
   :is-main is-main
   :beads-state beads-state))

(defun beads-completion-test--make-mock-worktrees ()
  "Create a list of mock worktrees for testing."
  (list
   (beads-completion-test--make-mock-worktree
    "beads.el" "/home/user/beads.el" "main" t "shared")
   (beads-completion-test--make-mock-worktree
    "feature-auth" "/home/user/worktrees/feature-auth" "feature/auth" nil "redirect")
   (beads-completion-test--make-mock-worktree
    "bugfix-login" "/home/user/worktrees/bugfix-login" "fix/login" nil "redirect")
   (beads-completion-test--make-mock-worktree
    "experiment" "/home/user/worktrees/experiment" "experiment" nil "none")))

;;; Worktree Completion Table Tests

(ert-deftest beads-completion-test-worktree-table-metadata ()
  "Test that worktree completion table provides correct metadata."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (metadata (funcall table "" nil 'metadata)))
      (should (eq 'metadata (car metadata)))
      (should (eq 'beads-worktree (cdr (assq 'category metadata))))
      (should (eq 'beads-completion--worktree-annotate
                  (cdr (assq 'annotation-function metadata))))
      (should (eq 'beads-completion--worktree-group
                  (cdr (assq 'group-function metadata)))))))

(ert-deftest beads-completion-test-worktree-table-candidates ()
  "Test that worktree completion table returns all worktrees."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil)))
      (should (= 4 (length candidates)))
      (should (member "beads.el" candidates))
      (should (member "feature-auth" candidates))
      (should (member "bugfix-login" candidates))
      (should (member "experiment" candidates)))))

(ert-deftest beads-completion-test-worktree-candidates-have-properties ()
  "Test that worktree candidates have expected text properties."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil)))
      (dolist (candidate candidates)
        (should (get-text-property 0 'beads-worktree candidate))
        (should (plist-member (text-properties-at 0 candidate) 'beads-branch))
        (should (plist-member (text-properties-at 0 candidate) 'beads-state))
        (should (plist-member (text-properties-at 0 candidate) 'beads-is-main))))))

(ert-deftest beads-completion-test-worktree-branch-property ()
  "Test that beads-branch property contains the correct branch."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil))
           (main-candidate (seq-find (lambda (c) (string= "beads.el" c)) candidates))
           (feature-candidate (seq-find (lambda (c) (string= "feature-auth" c)) candidates)))
      (should main-candidate)
      (should (string= "main" (get-text-property 0 'beads-branch main-candidate)))
      (should feature-candidate)
      (should (string= "feature/auth" (get-text-property 0 'beads-branch feature-candidate))))))

(ert-deftest beads-completion-test-worktree-state-property ()
  "Test that beads-state property contains the correct state."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil))
           (main-candidate (seq-find (lambda (c) (string= "beads.el" c)) candidates))
           (redirect-candidate (seq-find (lambda (c) (string= "feature-auth" c)) candidates))
           (none-candidate (seq-find (lambda (c) (string= "experiment" c)) candidates)))
      (should (string= "shared" (get-text-property 0 'beads-state main-candidate)))
      (should (string= "redirect" (get-text-property 0 'beads-state redirect-candidate)))
      (should (string= "none" (get-text-property 0 'beads-state none-candidate))))))

;;; Worktree Annotation Function Tests

(ert-deftest beads-completion-test-worktree-annotate-format ()
  "Test that worktree annotation returns expected format."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil))
           (main-candidate (seq-find (lambda (c) (string= "beads.el" c)) candidates))
           (annotation (beads-completion--worktree-annotate main-candidate)))
      (should annotation)
      (should (string-match-p "\\[main\\]" annotation))
      (should (string-match-p "shared" annotation))
      (should (string-match-p "(main)" annotation))
      (should (string-match-p "/home/user/beads.el" annotation)))))

(ert-deftest beads-completion-test-worktree-annotate-redirect ()
  "Test annotation for redirect worktree."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil))
           (redirect-candidate (seq-find (lambda (c) (string= "feature-auth" c)) candidates))
           (annotation (beads-completion--worktree-annotate redirect-candidate)))
      (should annotation)
      (should (string-match-p "\\[feature/auth\\]" annotation))
      (should (string-match-p "redirect" annotation))
      ;; Should NOT have (main) marker
      (should-not (string-match-p "(main)" annotation)))))

(ert-deftest beads-completion-test-worktree-annotate-none-state ()
  "Test annotation for worktree with no beads state."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil))
           (none-candidate (seq-find (lambda (c) (string= "experiment" c)) candidates))
           (annotation (beads-completion--worktree-annotate none-candidate)))
      (should annotation)
      (should (string-match-p "none" annotation)))))

(ert-deftest beads-completion-test-worktree-annotate-truncates-long-paths ()
  "Test that annotation truncates paths longer than 40 characters."
  (let* ((long-path "/home/user/very/long/path/to/worktree/directory/name")
         (worktrees (list (beads-completion-test--make-mock-worktree
                           "long-path-wt" long-path "main" nil "redirect")))
         (beads-completion--worktree-cache (cons (float-time) worktrees)))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil))
           (annotation (beads-completion--worktree-annotate (car candidates))))
      (should annotation)
      (should (string-match-p "\\.\\.\\." annotation)))))

(ert-deftest beads-completion-test-worktree-annotate-handles-nil ()
  "Test that worktree annotation handles invalid input gracefully."
  (should (null (beads-completion--worktree-annotate "nonexistent")))
  (should (string= "" (beads-completion--worktree-annotate nil))))

;;; Worktree Group Function Tests

(ert-deftest beads-completion-test-worktree-group-by-state ()
  "Test that grouping function groups by beads state correctly."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil)))
      (let ((shared-candidate (seq-find (lambda (c) (string= "beads.el" c)) candidates))
            (redirect-candidate (seq-find (lambda (c) (string= "feature-auth" c)) candidates))
            (none-candidate (seq-find (lambda (c) (string= "experiment" c)) candidates)))
        (should (string= "Shared (Main Repository)"
                         (beads-completion--worktree-group shared-candidate nil)))
        (should (string= "Redirect (Linked Worktrees)"
                         (beads-completion--worktree-group redirect-candidate nil)))
        (should (string= "None (No Beads)"
                         (beads-completion--worktree-group none-candidate nil)))))))

(ert-deftest beads-completion-test-worktree-group-transform ()
  "Test that group function returns candidate when transform is non-nil."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (candidates (all-completions "" table nil))
           (candidate (car candidates)))
      (should (string= candidate (beads-completion--worktree-group candidate t))))))

;;; Worktree Completion Style Tests

(ert-deftest beads-completion-test-worktree-style-match-name ()
  "Test that worktree completion style matches on name."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (matches (beads-completion--worktree-style-all "feature-auth" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "feature-auth" (car matches))))))

(ert-deftest beads-completion-test-worktree-style-match-branch ()
  "Test that worktree completion style matches on branch."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (matches (beads-completion--worktree-style-all "feature/auth" table nil nil)))
      (should (= 1 (length matches)))
      (should (string= "feature-auth" (car matches))))))

(ert-deftest beads-completion-test-worktree-style-match-state ()
  "Test that worktree completion style matches on beads state."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (matches (beads-completion--worktree-style-all "redirect" table nil nil)))
      ;; Should match both redirect worktrees
      (should (= 2 (length matches)))
      (should (member "feature-auth" matches))
      (should (member "bugfix-login" matches)))))

(ert-deftest beads-completion-test-worktree-style-case-insensitive ()
  "Test that worktree matching is case-insensitive."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (matches-lower (beads-completion--worktree-style-all "shared" table nil nil))
           (matches-upper (beads-completion--worktree-style-all "SHARED" table nil nil))
           (matches-mixed (beads-completion--worktree-style-all "Shared" table nil nil)))
      (should (= 1 (length matches-lower)))
      (should (= 1 (length matches-upper)))
      (should (= 1 (length matches-mixed)))
      (should (string= "beads.el" (car matches-lower)))
      (should (string= "beads.el" (car matches-upper)))
      (should (string= "beads.el" (car matches-mixed))))))

(ert-deftest beads-completion-test-worktree-style-no-match ()
  "Test that worktree style returns empty list when no match."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (matches (beads-completion--worktree-style-all "nonexistent" table nil nil)))
      (should (= 0 (length matches))))))

(ert-deftest beads-completion-test-worktree-style-try-single-match ()
  "Test try-completion returns single match when only one."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           ;; Use partial match "experi" which uniquely matches "experiment"
           (result (beads-completion--worktree-style-try "experi" table nil nil)))
      (should (string= "experiment" result)))))

(ert-deftest beads-completion-test-worktree-style-try-multiple-matches ()
  "Test try-completion returns input string when multiple matches."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (result (beads-completion--worktree-style-try "redirect" table nil nil)))
      (should (string= "redirect" result)))))

(ert-deftest beads-completion-test-worktree-style-try-exact-match ()
  "Test try-completion returns t for exact unique match."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (result (beads-completion--worktree-style-try "beads.el" table nil nil)))
      (should (eq t result)))))

(ert-deftest beads-completion-test-worktree-style-try-no-match ()
  "Test try-completion returns nil when no match."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (let* ((table (beads-completion-worktree-table))
           (result (beads-completion--worktree-style-try "nonexistent" table nil nil)))
      (should (null result)))))

(ert-deftest beads-completion-test-worktree-style-registered ()
  "Test that beads-worktree-name completion style is registered."
  (should (assq 'beads-worktree-name completion-styles-alist)))

;;; Worktree Read Function Tests

(ert-deftest beads-completion-test-read-worktree-uses-worktree-style ()
  "Test that beads-completion-read-worktree enables worktree-aware completion."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt table &rest _args)
                 ;; Verify category override is set
                 (should (assq 'beads-worktree completion-category-overrides))
                 ;; completion-category-overrides format: ((cat (styles ...)))
                 (let ((override (cdr (assq 'beads-worktree completion-category-overrides))))
                   (should (member '(styles beads-worktree-name basic) override)))
                 "feature-auth")))
      (should (string= "feature-auth" (beads-completion-read-worktree "Test: "))))))

;;; Worktree Cache Tests

(ert-deftest beads-completion-test-worktree-cache-invalidation ()
  "Test that worktree cache can be invalidated."
  (let ((beads-completion--worktree-cache
         (cons (float-time) (beads-completion-test--make-mock-worktrees))))
    (should beads-completion--worktree-cache)
    (beads-completion-invalidate-worktree-cache)
    (should (null beads-completion--worktree-cache))))

(ert-deftest beads-completion-test-worktree-cache-preserved-on-error ()
  "Test that stale worktree cache is preserved when refresh fails."
  (let* ((mock-worktrees (beads-completion-test--make-mock-worktrees))
         ;; Use an old timestamp so refresh is attempted
         (beads-completion--worktree-cache (cons 0 mock-worktrees))
         (message-shown nil))
    (cl-letf (((symbol-function 'beads-command-worktree-list!)
               (lambda ()
                 (error "Connection failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-shown (apply #'format fmt args)))))
      ;; Get worktrees - should fail to refresh but return stale data
      (let ((result (beads-completion--get-cached-worktrees)))
        ;; Should return the stale cached worktrees
        (should (= (length result) (length mock-worktrees)))
        (should (equal (oref (car result) name) "beads.el"))
        ;; Should show warning
        (should message-shown)
        (should (string-match "using cached data" message-shown))))))

(ert-deftest beads-completion-test-worktree-nil-returned-when-no-cache-and-error ()
  "Test that nil is returned when worktree refresh fails with no cache."
  (let ((beads-completion--worktree-cache nil))
    (cl-letf (((symbol-function 'beads-command-worktree-list!)
               (lambda ()
                 (error "Connection failed")))
              ;; Suppress any messages
              ((symbol-function 'message) #'ignore))
      ;; Get worktrees - should fail to refresh with no stale data
      (let ((result (beads-completion--get-cached-worktrees)))
        ;; Should return nil since no cache available
        (should (null result))))))

(provide 'beads-completion-test)
;;; beads-completion-test.el ends here
