;;; beads-parity-test.el --- Parity tests for from-json functions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; P0 of the beads-defcommand redesign (beads-el-l6h).
;;
;; These tests capture real `bd --json` output as fixtures and verify
;; that the hand-written *-from-json functions produce correct domain
;; objects.  Once `beads-from-json' (Phase 3) is implemented, these
;; same fixtures will be used to verify parity between the new generic
;; introspection-based parser and the existing hand-written parsers.
;;
;; Fixture provenance: All JSON fixtures below were captured from
;; real `bd` CLI output (bd v0.21+) on 2026-04-03.
;;
;; Test naming convention:
;;   beads-parity-test-<type>-from-json-<scenario>
;;
;; Future parity tests will follow:
;;   beads-parity-test-<type>-parity-<scenario>

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-types)

;;; ========================================
;;; Parity Test Infrastructure
;;; ========================================

(defun beads-parity--slots-equal-p (obj-a obj-b)
  "Return t if OBJ-A and OBJ-B have equal slot values.
Both must be instances of the same EIEIO class."
  (when (eq (eieio-object-class obj-a)
            (eieio-object-class obj-b))
    (let ((slots (eieio-class-slots (eieio-object-class obj-a)))
          (equal t))
      (dolist (slot slots equal)
        (let ((name (eieio-slot-descriptor-name slot)))
          (when (and (slot-boundp obj-a name)
                     (slot-boundp obj-b name))
            (unless (equal (slot-value obj-a name)
                           (slot-value obj-b name))
              (setq equal nil))))))))

(defun beads-parity--slot-diff (obj-a obj-b)
  "Return alist of slot differences between OBJ-A and OBJ-B.
Each entry is (SLOT-NAME A-VALUE B-VALUE)."
  (let ((slots (eieio-class-slots (eieio-object-class obj-a)))
        diffs)
    (dolist (slot slots (nreverse diffs))
      (let ((name (eieio-slot-descriptor-name slot)))
        (when (and (slot-boundp obj-a name)
                   (slot-boundp obj-b name))
          (let ((va (slot-value obj-a name))
                (vb (slot-value obj-b name)))
            (unless (equal va vb)
              (push (list name va vb) diffs))))))))

(defmacro beads-parity-test--assert-slots (obj &rest slot-value-pairs)
  "Assert that OBJ has the given slot values.
SLOT-VALUE-PAIRS is a plist of slot-name value."
  (declare (indent 1))
  (let (forms)
    (while slot-value-pairs
      (let ((slot (pop slot-value-pairs))
            (expected (pop slot-value-pairs)))
        (push `(should (equal (slot-value ,obj ',slot) ,expected))
              forms)))
    `(progn ,@(nreverse forms))))

;;; ========================================
;;; Real bd --json Fixtures
;;; ========================================

;; Captured from: bd create "Test bug" -t bug -p 2 --json
(defvar beads-parity--create-json
  '((id . "beadsfixtest-9n4")
    (title . "Test bug")
    (description . "A bug for testing")
    (status . "open")
    (priority . 2)
    (issue_type . "bug")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:16Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:16Z"))
  "Real bd create --json output.")

;; Captured from: bd show <id> --json (with deps, labels, comments)
(defvar beads-parity--show-json
  '((id . "beadsfixtest-9n4")
    (title . "Test bug")
    (description . "A bug for testing")
    (status . "open")
    (priority . 2)
    (issue_type . "bug")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:16Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:16Z")
    (labels . ["urgent"])
    (dependents .
     [((id . "beadsfixtest-wl8")
       (title . "Child task")
       (description . "A child of the epic")
       (status . "open")
       (priority . 2)
       (issue_type . "task")
       (owner . "roman@burningswell.com")
       (created_at . "2026-04-03T13:55:16Z")
       (created_by . "Roman Scherer")
       (updated_at . "2026-04-03T13:55:16Z")
       (dependency_type . "blocks"))])
    (comments .
     [((id . "019d53a0-b0aa-7852-9d78-426530ca71ab")
       (issue_id . "beadsfixtest-9n4")
       (author . "Roman Scherer")
       (text . "This is a test comment")
       (created_at . "2026-04-03T13:55:33Z"))]))
  "Real bd show --json output with labels, dependents, comments.")

;; Captured from: bd list --json (list view, no nested deps)
(defvar beads-parity--list-json
  [((id . "beadsfixtest-ara")
    (title . "Test epic")
    (description . "An epic for testing")
    (status . "open")
    (priority . 1)
    (issue_type . "epic")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:12Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:12Z")
    (dependency_count . 0)
    (dependent_count . 0)
    (comment_count . 0))
   ((id . "beadsfixtest-9n4")
    (title . "Test bug")
    (description . "A bug for testing")
    (status . "open")
    (priority . 2)
    (issue_type . "bug")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:16Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:16Z")
    (labels . ["urgent"])
    (dependency_count . 0)
    (dependent_count . 1)
    (comment_count . 1))]
  "Real bd list --json output (array of issues).")

;; Captured from: bd close <id> --reason "Fixed" --json
(defvar beads-parity--close-json
  [((id . "beadsfixtest-9n4")
    (title . "Test bug")
    (description . "A bug for testing")
    (status . "closed")
    (priority . 2)
    (issue_type . "bug")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:16Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:50Z")
    (closed_at . "2026-04-03T13:55:50Z")
    (close_reason . "Fixed the bug")
    (labels . ["urgent"]))]
  "Real bd close --json output.")

;; Captured from: bd update <id> --notes "Updated" --json
(defvar beads-parity--update-json
  [((id . "beadsfixtest-ara")
    (title . "Test epic")
    (description . "An epic for testing")
    (notes . "Updated notes")
    (status . "open")
    (priority . 1)
    (issue_type . "epic")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:12Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:51Z"))]
  "Real bd update --json output.")

;; Captured from: bd blocked --json
(defvar beads-parity--blocked-json
  [((id . "beadsfixtest-wl8")
    (title . "Child task")
    (description . "A child of the epic")
    (status . "open")
    (priority . 2)
    (issue_type . "task")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:16Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:16Z")
    (blocked_by_count . 1)
    (blocked_by . ["beadsfixtest-9n4"]))]
  "Real bd blocked --json output.")

;; Captured from: bd stats --json
(defvar beads-parity--stats-json
  '((summary .
     ((total_issues . 3)
      (open_issues . 3)
      (in_progress_issues . 0)
      (closed_issues . 0)
      (blocked_issues . 1)
      (deferred_issues . 0)
      (ready_issues . 2)
      (pinned_issues . 0)
      (epics_eligible_for_closure . 0)
      (average_lead_time_hours . 0))))
  "Real bd stats --json output (nested summary, no recent_activity).")

;; Captured from: bd dep add <id1> <id2> -t blocks --json
(defvar beads-parity--dep-add-json
  '((depends_on_id . "beadsfixtest-9n4")
    (issue_id . "beadsfixtest-wl8")
    (status . "added")
    (type . "blocks"))
  "Real bd dep add --json output.")

;; Captured from: bd dep remove <id1> <id2> --json
(defvar beads-parity--dep-remove-json
  '((depends_on_id . "beadsfixtest-9n4")
    (issue_id . "beadsfixtest-wl8")
    (status . "removed"))
  "Real bd dep remove --json output.")

;; Captured from: bd dep list <id> --json (IssueWithDependencyMetadata)
(defvar beads-parity--dep-list-json
  [((id . "beadsfixtest-9n4")
    (title . "Test bug")
    (description . "A bug for testing")
    (status . "open")
    (priority . 2)
    (issue_type . "bug")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:16Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:16Z")
    (labels . ["urgent"])
    (dependency_type . "blocks"))
   ((id . "beadsfixtest-ara")
    (title . "Test epic")
    (description . "An epic for testing")
    (status . "open")
    (priority . 1)
    (issue_type . "epic")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:12Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:12Z")
    (dependency_type . "parent-child"))]
  "Real bd dep list --json output (IssueWithDependencyMetadata).")

;; Captured from: bd dep tree <id> --json
(defvar beads-parity--dep-tree-json
  [((id . "beadsfixtest-ara")
    (title . "Test epic")
    (description . "An epic for testing")
    (status . "open")
    (priority . 1)
    (issue_type . "epic")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:12Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:12Z")
    (depth . 0)
    (parent_id . "")
    (truncated . :json-false))]
  "Real bd dep tree --json output.")

;; Captured from: bd ready --json
(defvar beads-parity--ready-json
  [((id . "beadsfixtest-ara")
    (title . "Test epic")
    (description . "An epic for testing")
    (status . "open")
    (priority . 1)
    (issue_type . "epic")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:12Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:12Z")
    (dependency_count . 0)
    (dependent_count . 0)
    (comment_count . 0))]
  "Real bd ready --json output.")

;; Captured from: bd reopen <id> --json
(defvar beads-parity--reopen-json
  [((id . "beadsfixtest-9n4")
    (title . "Test bug")
    (description . "A bug for testing")
    (status . "open")
    (priority . 2)
    (issue_type . "bug")
    (owner . "roman@burningswell.com")
    (created_at . "2026-04-03T13:55:16Z")
    (created_by . "Roman Scherer")
    (updated_at . "2026-04-03T13:55:57Z")
    (labels . ["urgent"]))]
  "Real bd reopen --json output.")

;; Captured from: bd label list-all --json
(defvar beads-parity--label-list-all-json
  [((label . "urgent")
    (count . 1))]
  "Real bd label list-all --json output.")

;; Captured from: bd formula list --json
(defvar beads-parity--formula-list-json
  [((name . "mol-deacon-patrol")
    (type . "workflow")
    (description . "Mol Deacon Patrol patrol.")
    (source . "/home/roman/.beads/formulas/mol-deacon-patrol.formula.toml")
    (steps . 1)
    (vars . 0))
   ((name . "mol-witness-patrol")
    (type . "workflow")
    (description . "Per-rig worker monitor patrol loop.")
    (source . "/home/roman/.beads/formulas/mol-witness-patrol.formula.toml")
    (steps . 9)
    (vars . 1))]
  "Real bd formula list --json output.")

;; Captured from: bd formula show mol-witness-patrol --json
;; (trimmed to relevant structure with one step and one var)
(defvar beads-parity--formula-show-json
  '((formula . "mol-witness-patrol")
    (description . "Per-rig worker monitor patrol loop.")
    (version . 9)
    (type . "workflow")
    (vars .
     ((wisp_type .
       ((description . "Type of wisp created for this molecule")
        (default . "patrol")))))
    (steps .
     [((id . "inbox-check")
       (title . "Process witness mail")
       (description . "Check inbox for messages."))
      ((id . "process-cleanups")
       (title . "Process pending cleanup wisps")
       (description . "Handle cleanup wisps.")
       (needs . ["inbox-check"]))]))
  "Real bd formula show --json output (trimmed).")

;; Captured from: bd worktree list --json
(defvar beads-parity--worktree-list-json
  [((name . "beadsfixtest")
    (path . "/tmp/beadsfixtest")
    (branch . "master")
    (is_main . t)
    (beads_state . "shared"))]
  "Real bd worktree list --json output.")

;; Captured from: bd worktree info --json (not in a worktree)
(defvar beads-parity--worktree-info-json
  '((is_worktree . :json-false))
  "Real bd worktree info --json output (not a worktree).")

;; Synthetic fixture for worktree info in a worktree context
(defvar beads-parity--worktree-info-linked-json
  '((is_worktree . t)
    (name . "feature-branch")
    (path . "/tmp/beadsfixtest/worktrees/feature-branch")
    (branch . "feature-branch")
    (main_path . "/tmp/beadsfixtest")
    (beads_state . "redirect"))
  "Representative bd worktree info --json for a linked worktree.")

;; Captured from: bd epic close-eligible --json (with data)
;; Synthetic since real run returned [] (no eligible epics)
(defvar beads-parity--epic-status-json
  [((epic .
     ((id . "beadsfixtest-ara")
      (title . "Test epic")
      (description . "An epic for testing")
      (status . "open")
      (priority . 1)
      (issue_type . "epic")
      (owner . "roman@burningswell.com")
      (created_at . "2026-04-03T13:55:12Z")
      (created_by . "Roman Scherer")
      (updated_at . "2026-04-03T13:55:12Z")))
    (total_children . 2)
    (closed_children . 2)
    (eligible_for_close . t))]
  "Representative bd epic close-eligible --json output.")

;; Synthetic fixture for recent_activity (not always present in stats)
(defvar beads-parity--stats-with-activity-json
  '((summary .
     ((total_issues . 10)
      (open_issues . 5)
      (in_progress_issues . 2)
      (closed_issues . 3)
      (blocked_issues . 1)
      (deferred_issues . 0)
      (ready_issues . 4)
      (tombstone_issues . 0)
      (pinned_issues . 0)
      (epics_eligible_for_closure . 1)
      (average_lead_time_hours . 24.5)))
    (recent_activity .
     ((hours_tracked . 24)
      (commit_count . 15)
      (issues_created . 3)
      (issues_closed . 2)
      (issues_updated . 8)
      (issues_reopened . 1)
      (total_changes . 14))))
  "Representative bd stats --json with recent_activity.")

;;; ========================================
;;; Baseline Tests: beads-issue-from-json
;;; ========================================

(ert-deftest beads-parity-test-issue-from-json-create ()
  "Test beads-issue-from-json with real bd create output."
  (let ((issue (beads-issue-from-json beads-parity--create-json)))
    (should (beads-issue-p issue))
    (beads-parity-test--assert-slots issue
      id "beadsfixtest-9n4"
      title "Test bug"
      description "A bug for testing"
      status "open"
      priority 2
      issue-type "bug"
      created-at "2026-04-03T13:55:16Z"
      created-by "Roman Scherer"
      updated-at "2026-04-03T13:55:16Z")))

(ert-deftest beads-parity-test-issue-from-json-show ()
  "Test beads-issue-from-json with real bd show output.
Verifies nested dependents and comments are parsed correctly."
  (let ((issue (beads-issue-from-json beads-parity--show-json)))
    (should (beads-issue-p issue))
    (beads-parity-test--assert-slots issue
      id "beadsfixtest-9n4"
      title "Test bug"
      status "open")
    ;; Labels
    (should (equal (oref issue labels) '("urgent")))
    ;; Dependents
    (let ((deps (oref issue dependents)))
      (should (= (length deps) 1))
      (let ((dep (car deps)))
        (should (beads-dependency-p dep))
        (should (equal (oref dep depends-on-id)
                       "beadsfixtest-wl8"))
        (should (equal (oref dep type) "blocks"))))
    ;; Comments
    (let ((comments (oref issue comments)))
      (should (= (length comments) 1))
      (let ((comment (car comments)))
        (should (beads-comment-p comment))
        (should (equal (oref comment text)
                       "This is a test comment"))
        (should (equal (oref comment author) "Roman Scherer"))))))

(ert-deftest beads-parity-test-issue-from-json-closed ()
  "Test beads-issue-from-json with real bd close output."
  (let ((issue (beads-issue-from-json (aref beads-parity--close-json 0))))
    (should (beads-issue-p issue))
    (beads-parity-test--assert-slots issue
      id "beadsfixtest-9n4"
      status "closed"
      closed-at "2026-04-03T13:55:50Z")))

(ert-deftest beads-parity-test-issue-from-json-list ()
  "Test beads-issue-from-json with real bd list output."
  (let ((issues (mapcar #'beads-issue-from-json
                        (append beads-parity--list-json nil))))
    (should (= (length issues) 2))
    (let ((epic (car issues))
          (bug (cadr issues)))
      (should (equal (oref epic id) "beadsfixtest-ara"))
      (should (equal (oref epic issue-type) "epic"))
      (should (equal (oref bug id) "beadsfixtest-9n4"))
      (should (equal (oref bug labels) '("urgent"))))))

;;; ========================================
;;; Baseline Tests: beads-dependency-from-json
;;; ========================================

(ert-deftest beads-parity-test-dependency-from-json-dep-list ()
  "Test beads-dependency-from-json with IssueWithDependencyMetadata.
This is the format returned by bd dep list and bd show dependents."
  (let ((dep (beads-dependency-from-json
              (aref beads-parity--dep-list-json 0))))
    (should (beads-dependency-p dep))
    ;; In IssueWithDependencyMetadata format, 'id' maps to depends-on-id
    (should (equal (oref dep depends-on-id) "beadsfixtest-9n4"))
    (should (equal (oref dep type) "blocks"))
    ;; Additional metadata from the issue
    (should (equal (oref dep title) "Test bug"))
    (should (equal (oref dep status) "open"))
    (should (equal (oref dep priority) 2))))

(ert-deftest beads-parity-test-dependency-from-json-parent-child ()
  "Test beads-dependency-from-json for parent-child type."
  (let ((dep (beads-dependency-from-json
              (aref beads-parity--dep-list-json 1))))
    (should (beads-dependency-p dep))
    (should (equal (oref dep depends-on-id) "beadsfixtest-ara"))
    (should (equal (oref dep type) "parent-child"))))

;;; ========================================
;;; Baseline Tests: beads-comment-from-json
;;; ========================================

(ert-deftest beads-parity-test-comment-from-json ()
  "Test beads-comment-from-json with real bd comment output."
  (let* ((comment-json (aref (alist-get 'comments
                                        beads-parity--show-json)
                             0))
         (comment (beads-comment-from-json comment-json)))
    (should (beads-comment-p comment))
    (beads-parity-test--assert-slots comment
      id "019d53a0-b0aa-7852-9d78-426530ca71ab"
      issue-id "beadsfixtest-9n4"
      author "Roman Scherer"
      text "This is a test comment"
      created-at "2026-04-03T13:55:33Z")))

;;; ========================================
;;; Baseline Tests: beads-blocked-issue-from-json
;;; ========================================

(ert-deftest beads-parity-test-blocked-issue-from-json ()
  "Test beads-blocked-issue-from-json with real bd blocked output."
  (let ((blocked (beads-blocked-issue-from-json
                  (aref beads-parity--blocked-json 0))))
    (should (beads-blocked-issue-p blocked))
    (beads-parity-test--assert-slots blocked
      id "beadsfixtest-wl8"
      title "Child task"
      status "open"
      blocked-by-count 1)
    ;; blocked-by is a list of ID strings
    (should (equal (oref blocked blocked-by)
                   '("beadsfixtest-9n4")))))

;;; ========================================
;;; Baseline Tests: beads-tree-node-from-json
;;; ========================================

(ert-deftest beads-parity-test-tree-node-from-json ()
  "Test beads-tree-node-from-json with real bd dep tree output."
  (let ((node (beads-tree-node-from-json
               (aref beads-parity--dep-tree-json 0))))
    (should (beads-tree-node-p node))
    (beads-parity-test--assert-slots node
      id "beadsfixtest-ara"
      title "Test epic"
      depth 0
      truncated nil)
    ;; parent_id is empty string in the JSON
    (should (equal (oref node parent-id) ""))))

;;; ========================================
;;; Baseline Tests: beads-statistics-from-json
;;; ========================================

(ert-deftest beads-parity-test-statistics-from-json ()
  "Test beads-statistics-from-json with real bd stats summary."
  (let ((stats (beads-statistics-from-json
                (alist-get 'summary beads-parity--stats-json))))
    (should (beads-statistics-p stats))
    (beads-parity-test--assert-slots stats
      total-issues 3
      open-issues 3
      in-progress-issues 0
      closed-issues 0
      blocked-issues 1
      deferred-issues 0
      ready-issues 2)))

;;; ========================================
;;; Baseline Tests: beads-recent-activity-from-json
;;; ========================================

(ert-deftest beads-parity-test-recent-activity-from-json ()
  "Test beads-recent-activity-from-json with representative data."
  (let ((activity (beads-recent-activity-from-json
                   (alist-get 'recent_activity
                              beads-parity--stats-with-activity-json))))
    (should (beads-recent-activity-p activity))
    (beads-parity-test--assert-slots activity
      hours-tracked 24
      commit-count 15
      issues-created 3
      issues-closed 2
      issues-updated 8
      issues-reopened 1
      total-changes 14)))

;;; ========================================
;;; Baseline Tests: beads-stats-data-from-json
;;; ========================================

(ert-deftest beads-parity-test-stats-data-from-json-no-activity ()
  "Test beads-stats-data-from-json with real stats (no activity)."
  (let ((data (beads-stats-data-from-json beads-parity--stats-json)))
    (should (beads-stats-data-p data))
    ;; Summary should be populated
    (let ((summary (oref data summary)))
      (should (beads-statistics-p summary))
      (should (= (oref summary total-issues) 3)))
    ;; No recent_activity in this fixture
    (should (null (oref data recent-activity)))))

(ert-deftest beads-parity-test-stats-data-from-json-with-activity ()
  "Test beads-stats-data-from-json with representative stats+activity."
  (let ((data (beads-stats-data-from-json
               beads-parity--stats-with-activity-json)))
    (should (beads-stats-data-p data))
    ;; Summary
    (let ((summary (oref data summary)))
      (should (beads-statistics-p summary))
      (should (= (oref summary total-issues) 10))
      (should (= (oref summary average-lead-time) 24.5)))
    ;; Recent activity
    (let ((activity (oref data recent-activity)))
      (should (beads-recent-activity-p activity))
      (should (= (oref activity commit-count) 15)))))

;;; ========================================
;;; Baseline Tests: beads-epic-status-from-json
;;; ========================================

(ert-deftest beads-parity-test-epic-status-from-json ()
  "Test beads-epic-status-from-json with representative data."
  (let ((status (beads-epic-status-from-json
                 (aref beads-parity--epic-status-json 0))))
    (should (beads-epic-status-p status))
    (beads-parity-test--assert-slots status
      total-children 2
      closed-children 2
      eligible-for-close t)
    ;; Nested epic should be a beads-issue
    (let ((epic (oref status epic)))
      (should (beads-issue-p epic))
      (should (equal (oref epic id) "beadsfixtest-ara"))
      (should (equal (oref epic title) "Test epic")))))

;;; ========================================
;;; Baseline Tests: beads-dep-op-result-from-json
;;; ========================================

(ert-deftest beads-parity-test-dep-op-result-from-json-add ()
  "Test beads-dep-op-result-from-json with real dep add output."
  (let ((result (beads-dep-op-result-from-json
                 beads-parity--dep-add-json)))
    (should (beads-dep-op-result-p result))
    (beads-parity-test--assert-slots result
      op-status "added"
      issue-id "beadsfixtest-wl8"
      depends-on-id "beadsfixtest-9n4"
      dep-type "blocks")))

(ert-deftest beads-parity-test-dep-op-result-from-json-remove ()
  "Test beads-dep-op-result-from-json with real dep remove output."
  (let ((result (beads-dep-op-result-from-json
                 beads-parity--dep-remove-json)))
    (should (beads-dep-op-result-p result))
    (beads-parity-test--assert-slots result
      op-status "removed"
      issue-id "beadsfixtest-wl8"
      depends-on-id "beadsfixtest-9n4")
    ;; dep-type is nil when not present in remove output
    (should (null (oref result dep-type)))))

;;; ========================================
;;; Baseline Tests: beads-label-count-from-json
;;; ========================================

(ert-deftest beads-parity-test-label-count-from-json ()
  "Test beads-label-count-from-json with real label list-all output."
  (let ((lc (beads-label-count-from-json
             (aref beads-parity--label-list-all-json 0))))
    (should (beads-label-count-p lc))
    (beads-parity-test--assert-slots lc
      label "urgent"
      count 1)))

;;; ========================================
;;; Baseline Tests: beads-formula-summary-from-json
;;; ========================================

(ert-deftest beads-parity-test-formula-summary-from-json ()
  "Test beads-formula-summary-from-json with real formula list output."
  (let ((summary (beads-formula-summary-from-json
                  (aref beads-parity--formula-list-json 0))))
    (should (beads-formula-summary-p summary))
    (beads-parity-test--assert-slots summary
      name "mol-deacon-patrol"
      formula-type "workflow"
      description "Mol Deacon Patrol patrol."
      steps 1
      vars 0)))

(ert-deftest beads-parity-test-formula-summary-from-json-with-vars ()
  "Test beads-formula-summary-from-json for formula with vars."
  (let ((summary (beads-formula-summary-from-json
                  (aref beads-parity--formula-list-json 1))))
    (should (beads-formula-summary-p summary))
    (beads-parity-test--assert-slots summary
      name "mol-witness-patrol"
      steps 9
      vars 1)))

;;; ========================================
;;; Baseline Tests: beads-formula-from-json
;;; ========================================

(ert-deftest beads-parity-test-formula-from-json ()
  "Test beads-formula-from-json with real formula show output."
  (let ((formula (beads-formula-from-json
                  beads-parity--formula-show-json)))
    (should (beads-formula-p formula))
    (beads-parity-test--assert-slots formula
      name "mol-witness-patrol"
      description "Per-rig worker monitor patrol loop."
      version 9
      formula-type "workflow")
    ;; Vars
    (let ((vars (oref formula vars)))
      (should (= (length vars) 1))
      (let ((var (car vars)))
        (should (beads-formula-var-p var))
        (should (equal (oref var name) "wisp_type"))
        (should (equal (oref var description)
                       "Type of wisp created for this molecule"))
        (should (equal (oref var default) "patrol"))))
    ;; Steps
    (let ((steps (oref formula steps)))
      (should (= (length steps) 2))
      (let ((step1 (car steps))
            (step2 (cadr steps)))
        (should (beads-formula-step-p step1))
        (should (equal (oref step1 id) "inbox-check"))
        (should (equal (oref step1 title) "Process witness mail"))
        ;; Step 2 has needs
        (should (beads-formula-step-p step2))
        (should (equal (oref step2 id) "process-cleanups"))
        (should (equal (oref step2 needs) '("inbox-check")))))))

;;; ========================================
;;; Baseline Tests: beads-formula-var-from-json
;;; ========================================

(ert-deftest beads-parity-test-formula-var-from-json ()
  "Test beads-formula-var-from-json directly."
  (let ((var-json '((description . "Type of wisp")
                    (default . "patrol")
                    (required . t)
                    (enum . ["workflow" "patrol"])
                    (type . "string"))))
    (let ((var (beads-formula-var-from-json 'wisp_type var-json)))
      (should (beads-formula-var-p var))
      (beads-parity-test--assert-slots var
        name "wisp_type"
        description "Type of wisp"
        default "patrol"
        required t
        var-type "string")
      ;; enum should be a list (converted from vector)
      (should (equal (oref var enum) '("workflow" "patrol"))))))

;;; ========================================
;;; Baseline Tests: beads-formula-step-from-json
;;; ========================================

(ert-deftest beads-parity-test-formula-step-from-json ()
  "Test beads-formula-step-from-json directly."
  (let* ((step-json '((id . "check-refinery")
                      (title . "Check refinery health")
                      (description . "Verify refinery is running.")
                      (notes . "Run every cycle.")
                      (needs . ["process-cleanups"])
                      (depends_on . ["inbox-check"])))
         (step (beads-formula-step-from-json step-json)))
    (should (beads-formula-step-p step))
    (beads-parity-test--assert-slots step
      id "check-refinery"
      title "Check refinery health"
      description "Verify refinery is running."
      notes "Run every cycle.")
    ;; needs and depends-on should be lists (converted from vectors)
    (should (equal (oref step needs) '("process-cleanups")))
    (should (equal (oref step depends-on) '("inbox-check")))))

;;; ========================================
;;; Baseline Tests: beads-worktree-from-json
;;; ========================================

(ert-deftest beads-parity-test-worktree-from-json ()
  "Test beads-worktree-from-json with real worktree list output."
  (let ((wt (beads-worktree-from-json
             (aref beads-parity--worktree-list-json 0))))
    (should (beads-worktree-p wt))
    (beads-parity-test--assert-slots wt
      name "beadsfixtest"
      path "/tmp/beadsfixtest"
      branch "master"
      is-main t
      beads-state "shared")))

;;; ========================================
;;; Baseline Tests: beads-worktree-info-from-json
;;; ========================================

(ert-deftest beads-parity-test-worktree-info-from-json-not-worktree ()
  "Test beads-worktree-info-from-json when not in a worktree."
  (let ((info (beads-worktree-info-from-json
               beads-parity--worktree-info-json)))
    (should (beads-worktree-info-p info))
    (should-not (oref info is-worktree))
    (should (null (oref info name)))))

(ert-deftest beads-parity-test-worktree-info-from-json-linked ()
  "Test beads-worktree-info-from-json for a linked worktree."
  (let ((info (beads-worktree-info-from-json
               beads-parity--worktree-info-linked-json)))
    (should (beads-worktree-info-p info))
    (should (oref info is-worktree))
    (beads-parity-test--assert-slots info
      name "feature-branch"
      path "/tmp/beadsfixtest/worktrees/feature-branch"
      branch "feature-branch"
      main-path "/tmp/beadsfixtest"
      beads-state "redirect")))

;;; ========================================
;;; Command-Level Parse Tests
;;; ========================================
;;
;; These tests verify that the full command parse pipeline
;; (JSON string -> beads-command-parse -> domain objects) works
;; correctly with real fixtures.  They exercise the same code path
;; as beads-command-execute but with mocked process output.

;; Note: These tests require the command modules.  They are kept
;; separate from the from-json unit tests above to allow running
;; just the from-json tests without loading all command modules.

;;; ========================================
;;; Fixture Manifest
;;; ========================================
;;
;; This section documents the mapping between command parse methods,
;; from-json functions, and test fixtures for reference during the
;; Phase 3 migration to beads-from-json.
;;
;; Command                    from-json Function           Fixture Var
;; -------                    ------------------           -----------
;; beads-command-create        beads-issue-from-json        create-json
;; beads-command-show           beads-issue-from-json        show-json
;; beads-command-list           beads-issue-from-json        list-json
;; beads-command-close          beads-issue-from-json        close-json
;; beads-command-update         beads-issue-from-json        update-json
;; beads-command-reopen         beads-issue-from-json        reopen-json
;; beads-command-ready          beads-issue-from-json        ready-json
;; beads-command-blocked        beads-blocked-issue-from-json  blocked-json
;; beads-command-dep-add        beads-dep-op-result-from-json  dep-add-json
;; beads-command-dep-remove     beads-dep-op-result-from-json  dep-remove-json
;; beads-command-dep-list       beads-dependency-from-json     dep-list-json
;; beads-command-dep-tree       beads-tree-node-from-json      dep-tree-json
;; beads-command-epic-status    beads-epic-status-from-json    epic-status-json
;; beads-command-formula-list   beads-formula-summary-from-json formula-list-json
;; beads-command-formula-show   beads-formula-from-json        formula-show-json
;; beads-command-worktree-list  beads-worktree-from-json       worktree-list-json
;; beads-command-worktree-info  beads-worktree-info-from-json  worktree-info-json
;; beads-command-worktree-create beads-worktree-from-json      worktree-list-json
;; beads-command-label-list-all beads-label-count-from-json    label-list-all-json
;; beads-command-stats          beads-stats-data-from-json     stats-json

(provide 'beads-parity-test)
;;; beads-parity-test.el ends here
