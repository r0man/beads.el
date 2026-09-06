;;; beads-eldoc-test.el --- Tests for beads-eldoc.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-eldoc.el including:
;; - Issue reference detection at point
;; - Cache management (TTL, invalidation)
;; - Asynchronous issue fetching with negative caching and dedup
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
;;; Issue ID Regexp Tests
;;; ========================================

(ert-deftest beads-eldoc-test-issue-id-base36 ()
  "Real bd ids are base-36, not hex; every one of these must be found."
  (dolist (id '("bs-lc1lb" "bs-8vh9x" "gce-hck" "bde-21fu" "beads.el-22"
                "bd-a1b2.1" "worker-f14c.2.3"))
    (with-temp-buffer
      (insert "see " id " for details")
      (goto-char (+ (point-min) 5))
      (should (equal (beads-issue-id-at-point) id)))))

(ert-deftest beads-eldoc-test-issue-id-rejects-non-ids ()
  "Uppercase hashes, dates and dotted abbreviations are not ids."
  (dolist (text '("see foo-BAR here" "on 2024-01 we" "e.g. this" "no-"))
    (with-temp-buffer
      (insert text)
      (goto-char (+ (point-min) 4))
      (should-not (beads-issue-id-at-point)))))

(ert-deftest beads-eldoc-test-issue-id-boundary ()
  "A hyphenated compound does not yield its tail as an id."
  (with-temp-buffer
    (insert "the gc-agent-abc buffer")
    (goto-char (+ (point-min) 14))
    ;; Without an allowlist the whole compound is the candidate.
    (should (equal (beads-issue-id-at-point) "gc-agent-abc"))
    ;; With one, neither the compound nor its tail qualifies.
    (should-not (beads-issue-id-at-point '("agent" "gce")))))

(ert-deftest beads-eldoc-test-issue-id-prefix-allowlist ()
  "Only ids with an allowed prefix are recognised."
  (with-temp-buffer
    (insert "post-command runs bs-lc1lb and gce-hck")
    (let ((prefixes '("bs" "gce")))
      (goto-char (+ (point-min) 3))
      (should-not (beads-issue-id-at-point prefixes))
      (goto-char (+ (point-min) 20))
      (should (equal (beads-issue-id-at-point prefixes) "bs-lc1lb"))
      (goto-char (- (point-max) 2))
      (should (equal (beads-issue-id-at-point prefixes) "gce-hck")))))

(ert-deftest beads-eldoc-test-issue-id-prefixes-buffer-local ()
  "`beads-eldoc--issue-id-at-point' honours a buffer-local allowlist."
  (with-temp-buffer
    (insert "post-command runs bs-lc1lb")
    (setq-local beads-issue-id-prefixes '("bs"))
    (goto-char (+ (point-min) 3))
    (should-not (beads-eldoc--issue-id-at-point))
    (goto-char (- (point-max) 2))
    (should (equal (beads-eldoc--issue-id-at-point) "bs-lc1lb"))))

(ert-deftest beads-eldoc-test-issue-id-trailing-punctuation ()
  "Trailing punctuation does not break an id."
  (with-temp-buffer
    (insert "fixed by bs-lc1lb, see gce-hck.")
    (goto-char (+ (point-min) 11))
    (should (equal (beads-issue-id-at-point) "bs-lc1lb"))
    (goto-char (- (point-max) 3))
    (should (equal (beads-issue-id-at-point) "gce-hck"))))

(ert-deftest beads-eldoc-test-issue-id-search-forward ()
  "`beads-issue-id-search-forward' walks every id in a region."
  (with-temp-buffer
    (insert "bs-lc1lb then gc-agent then gce-hck\n")
    (goto-char (point-min))
    (let (found)
      (while (beads-issue-id-search-forward nil nil '("bs" "gce"))
        (push (match-string 1) found))
      (should (equal (nreverse found) '("bs-lc1lb" "gce-hck"))))))

(ert-deftest beads-eldoc-test-issue-id-search-backward ()
  "`beads-issue-id-search-backward' finds whole ids, not their tails."
  (with-temp-buffer
    (insert "See bd-1\nand gce-hck for details")
    (goto-char (point-max))
    (should (equal (beads-issue-id-search-backward) "gce-hck"))
    (should (= (point) (match-beginning 1)))
    (should (equal (beads-issue-id-search-backward) "bd-1"))
    (should (= (point) 5))
    (should-not (beads-issue-id-search-backward))
    (should (= (point) 5))
    ;; The allowlist applies.
    (goto-char (point-max))
    (should (equal (beads-issue-id-search-backward nil nil '("bd")) "bd-1"))))

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
        (beads-eldoc-cache-ttl 1)
        (key (beads-eldoc--cache-key "beads.el-1")))
    ;; Cache an issue with old timestamp
    (puthash key
             (list :status 'ok
                   :timestamp (- (float-time) 2)
                   :issue beads-eldoc-test--sample-issue)
             beads-eldoc--cache)
    ;; Should return nil because entry is stale
    (should (null (beads-eldoc--get-cached-issue "beads.el-1")))
    ;; Entry should be removed from cache
    (should (null (gethash key beads-eldoc--cache)))))

(ert-deftest beads-eldoc-test-cache-negative ()
  "A cached miss answers nil without a lookup, then expires."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (beads-eldoc-negative-cache-ttl 1))
    (beads-eldoc--cache-missing "bd-nope")
    (should (null (beads-eldoc--get-cached-issue "bd-nope")))
    (should (eq (plist-get (beads-eldoc--cache-get "bd-nope") :status) 'missing))
    (let ((beads-eldoc-negative-cache-ttl 0))
      (should (null (beads-eldoc--cache-get "bd-nope"))))))

(ert-deftest beads-eldoc-test-cache-scoped ()
  "The same id in two stores are two cache entries."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (let ((default-directory "/tmp/a/"))
      (beads-eldoc--cache-issue "bd-1" beads-eldoc-test--sample-issue))
    (let ((default-directory "/tmp/b/"))
      (should (null (beads-eldoc--get-cached-issue "bd-1")))
      (beads-eldoc--cache-issue "bd-1" beads-eldoc-test--sample-issue-bd))
    (should (= (hash-table-count beads-eldoc--cache) 2))
    (let ((default-directory "/tmp/a/"))
      (should (equal (oref (beads-eldoc--get-cached-issue "bd-1") id)
                     "beads.el-22")))))

(ert-deftest beads-eldoc-test-cache-invalidate-single ()
  "Test invalidating a single cache entry, across scopes."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal)))
    (let ((default-directory "/tmp/a/"))
      (beads-eldoc--cache-issue "beads.el-22" beads-eldoc-test--sample-issue))
    (let ((default-directory "/tmp/b/"))
      (beads-eldoc--cache-issue "beads.el-22" beads-eldoc-test--sample-issue))
    (beads-eldoc--cache-issue "beads.el-23" beads-eldoc-test--sample-issue-bd)
    (beads-eldoc--invalidate-cache "beads.el-22")
    ;; Both scopes of the first issue should be invalidated
    (let ((default-directory "/tmp/a/"))
      (should (null (beads-eldoc--get-cached-issue "beads.el-22"))))
    (let ((default-directory "/tmp/b/"))
      (should (null (beads-eldoc--get-cached-issue "beads.el-22"))))
    ;; Second issue should still be cached
    (should (beads-eldoc--get-cached-issue "beads.el-23"))))

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
;;; Issue Fetching Tests (asynchronous)
;;; ========================================

(defmacro beads-eldoc-test--with-async-mock (spec &rest body)
  "Run BODY with `beads-command-execute-async' mocked.
SPEC is (SPAWNS COMMANDS SUCCESSES ERRORS): variables bound to a spawn
counter, the list of command objects, and the captured on-success /
on-error closures (newest first), so a test can complete a lookup
whenever it likes.  Also binds fresh cache and pending tables."
  (declare (indent 1))
  (let ((spawns (nth 0 spec)) (commands (nth 1 spec))
        (successes (nth 2 spec)) (errors (nth 3 spec)))
    `(let ((beads-eldoc--cache (make-hash-table :test 'equal))
           (beads-eldoc--pending (make-hash-table :test 'equal))
           (,spawns 0) (,commands nil) (,successes nil) (,errors nil))
       (cl-letf (((symbol-function 'beads-command-execute-async)
                  (lambda (cmd on-success &optional on-error &rest _kw)
                    (cl-incf ,spawns)
                    (push cmd ,commands)
                    (push on-success ,successes)
                    (push on-error ,errors)
                    'mock-process)))
         (ignore ,commands ,successes ,errors)
         ,@body))))

(ert-deftest beads-eldoc-test-request-issue-pending-then-delivers ()
  "A cold lookup spawns once, returns `pending', and delivers later."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (let ((got 'unset))
      (should (eq (beads-eldoc--request-issue
                   "beads.el-22" (lambda (issue) (setq got issue)))
                  'pending))
      (should (= spawns 1))
      (should (eq got 'unset))
      (should (equal (oref (car commands) issue-ids) '("beads.el-22")))
      (funcall (car successes) beads-eldoc-test--sample-issue)
      (should (equal (oref got id) "beads.el-22"))
      ;; Now cached: the next request answers synchronously.
      (should (eq (beads-eldoc--request-issue "beads.el-22" #'ignore) 'cached))
      (should (= spawns 1)))))

(ert-deftest beads-eldoc-test-request-issue-dedups-pending ()
  "Two requests before completion share one spawn and both get the result."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (let ((a nil) (b nil))
      (beads-eldoc--request-issue "beads.el-22" (lambda (i) (setq a i)))
      (should (eq (beads-eldoc--request-issue "beads.el-22" (lambda (i) (setq b i)))
                  'pending))
      (should (= spawns 1))
      (funcall (car successes) beads-eldoc-test--sample-issue)
      (should (and a b))
      (should (zerop (hash-table-count beads-eldoc--pending))))))

(ert-deftest beads-eldoc-test-request-issue-negative-cache ()
  "A failed lookup is cached as missing and not retried within the TTL."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (let ((got 'unset))
      (beads-eldoc--request-issue "bd-nope" (lambda (i) (setq got i)))
      (funcall (car errors) '(error "no issue found"))
      (should (null got))
      (setq got 'unset)
      (should (eq (beads-eldoc--request-issue "bd-nope" (lambda (i) (setq got i)))
                  'cached))
      (should (null got))
      (should (= spawns 1))
      ;; Expired negative entry: one more spawn.
      (let ((beads-eldoc-negative-cache-ttl 0))
        (beads-eldoc--request-issue "bd-nope" #'ignore))
      (should (= spawns 2)))))

(ert-deftest beads-eldoc-test-request-issue-scope-and-directory ()
  "Requests are keyed per store; `beads-eldoc-directory' sets the store."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (let ((default-directory "/tmp/a/"))
      (beads-eldoc--request-issue "bd-1" #'ignore))
    (let ((default-directory "/tmp/b/"))
      (beads-eldoc--request-issue "bd-1" #'ignore))
    (should (= spawns 2))
    (should (null (oref (car commands) directory)))
    ;; A string directory becomes the command's --directory and a new scope.
    (let ((default-directory "/tmp/a/")
          (beads-eldoc-directory "/tmp/store/"))
      (beads-eldoc--request-issue "bd-1" #'ignore))
    (should (= spawns 3))
    (should (equal (oref (car commands) directory) "/tmp/store/"))
    ;; A function directory is called with the id.
    (let ((default-directory "/tmp/a/")
          (beads-eldoc-directory (lambda (id) (and (equal id "bd-1") "/tmp/fn/"))))
      (beads-eldoc--request-issue "bd-1" #'ignore))
    (should (= spawns 4))
    (should (equal (oref (car commands) directory) "/tmp/fn/"))
    ;; The scoped result lands under its own key.
    (funcall (car successes) beads-eldoc-test--sample-issue)
    (should (beads-eldoc--get-cached-issue "bd-1" "/tmp/fn"))
    (should-not (beads-eldoc--get-cached-issue "bd-1" "/tmp/a"))))

(ert-deftest beads-eldoc-test-request-issue-remote-not-connected ()
  "No lookup is spawned for a remote store that is not connected."
  (require 'tramp)
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (let ((default-directory "/ssh:nohost.invalid:/tmp/")
          (called nil))
      (should (null (beads-eldoc--request-issue "bd-1" (lambda (_) (setq called t)))))
      (should-not called)
      (should (= spawns 0))
      (should (zerop (hash-table-count beads-eldoc--pending))))))

(ert-deftest beads-eldoc-test-request-issue-spawn-error ()
  "A spawn that signals is recorded as a miss, not raised."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (beads-eldoc--pending (make-hash-table :test 'equal))
        (got 'unset))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (&rest _) (error "Command failed"))))
      (should (eq (beads-eldoc--request-issue "beads.el-999"
                                              (lambda (i) (setq got i)))
                  'pending))
      (should (null got))
      (should (eq (plist-get (beads-eldoc--cache-get "beads.el-999") :status)
                  'missing)))))

(ert-deftest beads-eldoc-test-invalidation-keeps-pending ()
  "Invalidating results leaves in-flight lookups untouched."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (beads-eldoc--cache-issue "bd-2" beads-eldoc-test--sample-issue-bd)
    (beads-eldoc--request-issue "bd-1" #'ignore)
    (beads-eldoc--invalidate-cache)
    (should (zerop (hash-table-count beads-eldoc--cache)))
    (should (= (hash-table-count beads-eldoc--pending) 1))))

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
  "Test eldoc function calls callback with documentation from the cache."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (callback-called nil)
        (callback-args nil))
    (beads-eldoc--cache-issue "beads.el-22" beads-eldoc-test--sample-issue)
    (cl-letf (((symbol-function 'beads-eldoc--issue-id-at-point)
               (lambda () "beads.el-22")))
      (should (beads-eldoc-function
               (lambda (&rest args)
                 (setq callback-called t)
                 (setq callback-args args))))
      ;; Callback should be called
      (should callback-called)
      ;; First arg should be echo area string
      (should (string-match-p "beads\\.el-22" (car callback-args))))))

(ert-deftest beads-eldoc-test-eldoc-function-no-issue-at-point ()
  "Test eldoc function when not on issue reference."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'beads-eldoc--issue-id-at-point)
               (lambda () nil)))
      (should-not (beads-eldoc-function
                   (lambda (&rest _) (setq callback-called t))))
      ;; Callback should not be called
      (should-not callback-called))))

(ert-deftest beads-eldoc-test-eldoc-function-fetch-error ()
  "Test eldoc function when the id is cached as missing."
  (let ((beads-eldoc--cache (make-hash-table :test 'equal))
        (callback-called nil))
    (beads-eldoc--cache-missing "beads.el-999")
    (cl-letf (((symbol-function 'beads-eldoc--issue-id-at-point)
               (lambda () "beads.el-999")))
      (should-not (beads-eldoc-function
                   (lambda (&rest _) (setq callback-called t))))
      ;; Callback should not be called if the issue is unknown
      (should-not callback-called))))

(ert-deftest beads-eldoc-test-eldoc-function-async ()
  "A cold lookup returns t and delivers when the process completes."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (beads-eldoc-test--at-point "Implementing beads.el-|22 now" "|"
      (let ((echo nil) (plist nil))
        (should (eq (beads-eldoc-function
                     (lambda (e &rest p) (setq echo e plist p)))
                    t))
        (should (= spawns 1))
        (should (null echo))
        (funcall (car successes) beads-eldoc-test--sample-issue)
        (should (string-match-p "beads\\.el-22" echo))
        (should (equal (plist-get plist :thing) "beads.el-22"))
        (should (string-match-p "Issue: beads\\.el-22" (plist-get plist :buffer)))))))

(ert-deftest beads-eldoc-test-eldoc-function-drops-late-callback ()
  "A result arriving after point left the id is not shown, but is cached."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (beads-eldoc-test--at-point "Implementing beads.el-|22 now" "|"
      (let ((echo nil))
        (beads-eldoc-function (lambda (e &rest _) (setq echo e)))
        (goto-char (point-max))
        (funcall (car successes) beads-eldoc-test--sample-issue)
        (should (null echo))
        (should (beads-eldoc--get-cached-issue "beads.el-22"))))))

(ert-deftest beads-eldoc-test-eldoc-function-dead-buffer ()
  "A result arriving after the buffer died is cached and nothing else."
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (let ((echo nil))
      (with-temp-buffer
        (insert "Implementing beads.el-22 now")
        (goto-char (+ (point-min) 15))
        (beads-eldoc-function (lambda (e &rest _) (setq echo e))))
      (funcall (car successes) beads-eldoc-test--sample-issue)
      (should (null echo))
      (should (beads-eldoc--get-cached-issue "beads.el-22")))))

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
  (beads-eldoc-test--with-async-mock (spawns commands successes errors)
    (let ((result-echo nil)
          (result-buffer nil))
      (beads-eldoc-test--at-point
          "Implementing beads.el-|22 now" "|"
        (beads-eldoc-function
         (lambda (echo &rest plist)
           (setq result-echo echo)
           (setq result-buffer (plist-get plist :buffer))))
        ;; The lookup completes asynchronously.
        (funcall (car successes) beads-eldoc-test--sample-issue)
        ;; Echo area should have brief info
        (should (string-match-p "beads\\.el-22" result-echo))
        (should (string-match-p "in_progress" result-echo))
        ;; Buffer should have full details
        (should (string-match-p "Issue: beads\\.el-22" result-buffer))
        (should (string-match-p "Description:" result-buffer))))))

(provide 'beads-eldoc-test)

;;; beads-eldoc-test.el ends here
