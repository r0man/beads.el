;;; beads-buffer-test.el --- Tests for beads-buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the centralized buffer naming module.

;;; Code:

(require 'ert)
(require 'beads-buffer)

;;; Title Truncation Tests

(ert-deftest beads-buffer-test--truncate-title-nil ()
  "Test truncation of nil title."
  (should (string= "" (beads-buffer--truncate-title nil))))

(ert-deftest beads-buffer-test--truncate-title-empty ()
  "Test truncation of empty title."
  (should (string= "" (beads-buffer--truncate-title ""))))

(ert-deftest beads-buffer-test--truncate-title-short ()
  "Test truncation of short title (no truncation needed)."
  (should (string= "Fix bug" (beads-buffer--truncate-title "Fix bug"))))

(ert-deftest beads-buffer-test--truncate-title-exact-length ()
  "Test title at exactly max length."
  (let ((title (make-string 30 ?a)))
    (should (string= title (beads-buffer--truncate-title title)))))

(ert-deftest beads-buffer-test--truncate-title-long ()
  "Test truncation of long title."
  (let ((result (beads-buffer--truncate-title
                 "This is a very long title that needs to be truncated")))
    (should (= 30 (length result)))
    (should (string-suffix-p "..." result))))

;;; Branch Sanitization Tests

(ert-deftest beads-buffer-test--sanitize-branch-nil ()
  "Test that nil returns nil."
  (should (null (beads-buffer--sanitize-branch nil))))

(ert-deftest beads-buffer-test--sanitize-branch-simple ()
  "Test branch without slashes."
  (should (string= "main" (beads-buffer--sanitize-branch "main"))))

(ert-deftest beads-buffer-test--sanitize-branch-with-slash ()
  "Test branch names with slashes are sanitized to hyphens."
  (should (string= "feature-auth"
                   (beads-buffer--sanitize-branch "feature/auth"))))

(ert-deftest beads-buffer-test--sanitize-branch-multiple-slashes ()
  "Test branch with multiple slashes."
  (should (string= "user-feature-auth"
                   (beads-buffer--sanitize-branch "user/feature/auth"))))

;;; Main Branch Detection Tests

(ert-deftest beads-buffer-test-is-main-branch-p-main ()
  "Test that main is recognized as main branch."
  (should (beads-buffer-is-main-branch-p "main")))

(ert-deftest beads-buffer-test-is-main-branch-p-master ()
  "Test that master is recognized as main branch."
  (should (beads-buffer-is-main-branch-p "master")))

(ert-deftest beads-buffer-test-is-main-branch-p-feature ()
  "Test that feature branch is not main."
  (should-not (beads-buffer-is-main-branch-p "feature-auth")))

;;; Issue Branch Detection Tests

(ert-deftest beads-buffer-test-branch-is-issue-p-true ()
  "Test that issue-like branches are detected."
  (should (beads-buffer-branch-is-issue-p "bd-123"))
  (should (beads-buffer-branch-is-issue-p "worker-1"))
  (should (beads-buffer-branch-is-issue-p "beads-abc123")))

(ert-deftest beads-buffer-test-branch-is-issue-p-false ()
  "Test that non-issue branches are not detected as issues."
  (should-not (beads-buffer-branch-is-issue-p "main"))
  (should-not (beads-buffer-branch-is-issue-p "feature-auth"))
  (should-not (beads-buffer-branch-is-issue-p "develop")))

;;; Project Context Tests

(ert-deftest beads-buffer-test-project-context-simple ()
  "Test project context without branch."
  (cl-letf (((symbol-function 'beads-git-get-project-name) (lambda () "myproject"))
            ((symbol-function 'beads-git-get-branch) (lambda () "main")))
    (should (string= "myproject" (beads-buffer-project-context)))))

(ert-deftest beads-buffer-test-project-context-with-feature-branch ()
  "Test project context on feature branch."
  (cl-letf (((symbol-function 'beads-git-get-project-name) (lambda () "myproject"))
            ((symbol-function 'beads-git-get-branch) (lambda () "feature/auth")))
    (should (string= "myproject@feature-auth" (beads-buffer-project-context)))))

(ert-deftest beads-buffer-test-project-context-explicit ()
  "Test project context with explicit values."
  (should (string= "proj" (beads-buffer-project-context "proj" nil)))
  (should (string= "proj@branch" (beads-buffer-project-context "proj" "branch"))))

(ert-deftest beads-buffer-test-project-context-unknown ()
  "Test project context when project is not detected."
  (cl-letf (((symbol-function 'beads-git-get-project-name) (lambda () nil))
            ((symbol-function 'beads-git-get-branch) (lambda () "main")))
    (should (string= "unknown" (beads-buffer-project-context)))))

;;; List Buffer Name Tests

(ert-deftest beads-buffer-test-list-basic ()
  "Test basic list buffer name."
  (should (string= "*beads-list[proj]*"
                   (beads-buffer-list nil nil "proj" nil))))

(ert-deftest beads-buffer-test-list-ready ()
  "Test ready list buffer name."
  (should (string= "*beads-ready[proj]*"
                   (beads-buffer-list "ready" nil "proj" nil))))

(ert-deftest beads-buffer-test-list-blocked ()
  "Test blocked list buffer name."
  (should (string= "*beads-blocked[proj]*"
                   (beads-buffer-list "blocked" nil "proj" nil))))

(ert-deftest beads-buffer-test-list-with-filter ()
  "Test list buffer name with filter."
  (should (string= "*beads-list[proj] label=api*"
                   (beads-buffer-list nil "label=api" "proj" nil))))

(ert-deftest beads-buffer-test-list-with-status-filter ()
  "Test list buffer name with status filter."
  (should (string= "*beads-list[proj] open*"
                   (beads-buffer-list nil "open" "proj" nil))))

(ert-deftest beads-buffer-test-list-with-branch ()
  "Test list buffer name with branch."
  (should (string= "*beads-list[proj@branch]*"
                   (beads-buffer-list nil nil "proj" "branch"))))

(ert-deftest beads-buffer-test-list-branch-and-filter ()
  "Test list buffer name with branch and filter."
  (should (string= "*beads-list[proj@branch] label=backend*"
                   (beads-buffer-list nil "label=backend" "proj" "branch"))))

;;; Show Buffer Name Tests

(ert-deftest beads-buffer-test-show-basic ()
  "Test basic show buffer name."
  (should (string= "*beads-show[proj]/bd-42*"
                   (beads-buffer-show "bd-42" nil "proj" nil))))

(ert-deftest beads-buffer-test-show-with-title ()
  "Test show buffer name with title."
  (should (string= "*beads-show[proj]/bd-42 Fix login bug*"
                   (beads-buffer-show "bd-42" "Fix login bug" "proj" nil))))

(ert-deftest beads-buffer-test-show-with-long-title ()
  "Test show buffer name with long title (truncated)."
  (let ((result (beads-buffer-show
                 "bd-42"
                 "This is a very long title that should be truncated"
                 "proj"
                 nil)))
    (should (string-prefix-p "*beads-show[proj]/bd-42 " result))
    (should (string-suffix-p "...*" result))))

(ert-deftest beads-buffer-test-show-with-branch ()
  "Test show buffer name with branch."
  (should (string= "*beads-show[proj@branch]/bd-42*"
                   (beads-buffer-show "bd-42" nil "proj" "branch"))))

(ert-deftest beads-buffer-test-show-full ()
  "Test show buffer name with all components."
  (should (string= "*beads-show[proj@branch]/bd-42 Fix bug*"
                   (beads-buffer-show "bd-42" "Fix bug" "proj" "branch"))))

;;; Agent Buffer Name Tests

(ert-deftest beads-buffer-test-agent-basic ()
  "Test basic agent buffer name."
  (should (string= "*beads-agent[proj]/Task#1*"
                   (beads-buffer-agent "Task" 1 nil nil "proj" nil))))

(ert-deftest beads-buffer-test-agent-with-issue ()
  "Test agent buffer name with issue ID."
  (should (string= "*beads-agent[proj]/Plan#2 bd-42*"
                   (beads-buffer-agent "Plan" 2 "bd-42" nil "proj" nil))))

(ert-deftest beads-buffer-test-agent-with-issue-and-title ()
  "Test agent buffer name with issue ID and title."
  (should (string= "*beads-agent[proj]/Review#3 bd-42 Fix auth*"
                   (beads-buffer-agent "Review" 3 "bd-42" "Fix auth" "proj" nil))))

(ert-deftest beads-buffer-test-agent-with-branch ()
  "Test agent buffer name with branch."
  (should (string= "*beads-agent[proj@branch]/Task#1*"
                   (beads-buffer-agent "Task" 1 nil nil "proj" "branch"))))

(ert-deftest beads-buffer-test-agent-full ()
  "Test agent buffer name with all components."
  (should (string= "*beads-agent[proj@branch]/Plan#2 bd-42 Fix bug*"
                   (beads-buffer-agent "Plan" 2 "bd-42" "Fix bug" "proj" "branch"))))

;;; Utility Buffer Name Tests

(ert-deftest beads-buffer-test-utility-basic ()
  "Test basic utility buffer name."
  (should (string= "*beads-stats[proj]*"
                   (beads-buffer-utility "stats" nil "proj" nil))))

(ert-deftest beads-buffer-test-utility-with-suffix ()
  "Test utility buffer name with suffix."
  (should (string= "*beads-dep-tree[proj]/bd-42*"
                   (beads-buffer-utility "dep-tree" "bd-42" "proj" nil))))

(ert-deftest beads-buffer-test-utility-with-branch ()
  "Test utility buffer name with branch."
  (should (string= "*beads-graph[proj@branch]*"
                   (beads-buffer-utility "graph" nil "proj" "branch"))))

(ert-deftest beads-buffer-test-utility-full ()
  "Test utility buffer name with all components."
  (should (string= "*beads-dep-cycles[proj@branch]/epic*"
                   (beads-buffer-utility "dep-cycles" "epic" "proj" "branch"))))

;;; Parse List Buffer Tests

(ert-deftest beads-buffer-test-parse-list-basic ()
  "Test parsing basic list buffer name."
  (let ((parsed (beads-buffer-parse-list "*beads-list[myproject]*")))
    (should parsed)
    (should (string= "list" (plist-get parsed :type)))
    (should (string= "myproject" (plist-get parsed :project)))
    (should (null (plist-get parsed :branch)))
    (should (null (plist-get parsed :filter)))))

(ert-deftest beads-buffer-test-parse-list-ready ()
  "Test parsing ready list buffer name."
  (let ((parsed (beads-buffer-parse-list "*beads-ready[proj]*")))
    (should parsed)
    (should (string= "ready" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))))

(ert-deftest beads-buffer-test-parse-list-blocked ()
  "Test parsing blocked list buffer name."
  (let ((parsed (beads-buffer-parse-list "*beads-blocked[proj]*")))
    (should parsed)
    (should (string= "blocked" (plist-get parsed :type)))))

(ert-deftest beads-buffer-test-parse-list-with-branch ()
  "Test parsing list buffer name with branch."
  (let ((parsed (beads-buffer-parse-list "*beads-list[proj@branch]*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))))

(ert-deftest beads-buffer-test-parse-list-with-filter ()
  "Test parsing list buffer name with filter."
  (let ((parsed (beads-buffer-parse-list "*beads-list[proj] label=api*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "label=api" (plist-get parsed :filter)))))

(ert-deftest beads-buffer-test-parse-list-branch-and-filter ()
  "Test parsing list buffer name with branch and filter."
  (let ((parsed (beads-buffer-parse-list "*beads-list[proj@branch] label=backend*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "label=backend" (plist-get parsed :filter)))))

(ert-deftest beads-buffer-test-parse-list-invalid ()
  "Test parsing invalid list buffer name."
  (should (null (beads-buffer-parse-list "*beads-show[proj]/bd-1*")))
  (should (null (beads-buffer-parse-list "*other-buffer*"))))

;;; Parse Show Buffer Tests

(ert-deftest beads-buffer-test-parse-show-basic ()
  "Test parsing basic show buffer name."
  (let ((parsed (beads-buffer-parse-show "*beads-show[proj]/bd-42*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (null (plist-get parsed :branch)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (null (plist-get parsed :title)))))

(ert-deftest beads-buffer-test-parse-show-with-title ()
  "Test parsing show buffer name with title."
  (let ((parsed (beads-buffer-parse-show "*beads-show[proj]/bd-42 Fix bug*")))
    (should parsed)
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix bug" (plist-get parsed :title)))))

(ert-deftest beads-buffer-test-parse-show-with-branch ()
  "Test parsing show buffer name with branch."
  (let ((parsed (beads-buffer-parse-show "*beads-show[proj@branch]/bd-42*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))))

(ert-deftest beads-buffer-test-parse-show-full ()
  "Test parsing show buffer name with all components."
  (let ((parsed (beads-buffer-parse-show "*beads-show[proj@branch]/bd-42 Fix login bug*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix login bug" (plist-get parsed :title)))))

(ert-deftest beads-buffer-test-parse-show-invalid ()
  "Test parsing invalid show buffer name."
  (should (null (beads-buffer-parse-show "*beads-list[proj]*")))
  (should (null (beads-buffer-parse-show "*beads-show[proj]*"))))

;;; Parse Agent Buffer Tests

(ert-deftest beads-buffer-test-parse-agent-basic ()
  "Test parsing basic agent buffer name."
  (let ((parsed (beads-buffer-parse-agent "*beads-agent[proj]/Task#1*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (null (plist-get parsed :branch)))
    (should (string= "Task" (plist-get parsed :type)))
    (should (= 1 (plist-get parsed :instance)))
    (should (null (plist-get parsed :issue-id)))
    (should (null (plist-get parsed :title)))))

(ert-deftest beads-buffer-test-parse-agent-with-issue ()
  "Test parsing agent buffer name with issue."
  (let ((parsed (beads-buffer-parse-agent "*beads-agent[proj]/Plan#2 bd-42*")))
    (should parsed)
    (should (string= "Plan" (plist-get parsed :type)))
    (should (= 2 (plist-get parsed :instance)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))))

(ert-deftest beads-buffer-test-parse-agent-with-issue-and-title ()
  "Test parsing agent buffer name with issue and title."
  (let ((parsed (beads-buffer-parse-agent "*beads-agent[proj]/Review#3 bd-42 Fix auth*")))
    (should parsed)
    (should (string= "Review" (plist-get parsed :type)))
    (should (= 3 (plist-get parsed :instance)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix auth" (plist-get parsed :title)))))

(ert-deftest beads-buffer-test-parse-agent-with-branch ()
  "Test parsing agent buffer name with branch."
  (let ((parsed (beads-buffer-parse-agent "*beads-agent[proj@branch]/Task#1*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "Task" (plist-get parsed :type)))
    (should (= 1 (plist-get parsed :instance)))))

(ert-deftest beads-buffer-test-parse-agent-invalid ()
  "Test parsing invalid agent buffer name."
  (should (null (beads-buffer-parse-agent "*beads-list[proj]*")))
  (should (null (beads-buffer-parse-agent "*beads-agent[proj]/backend*"))))

;;; Parse Utility Buffer Tests

(ert-deftest beads-buffer-test-parse-utility-basic ()
  "Test parsing basic utility buffer name."
  (let ((parsed (beads-buffer-parse-utility "*beads-stats[proj]*")))
    (should parsed)
    (should (string= "stats" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))
    (should (null (plist-get parsed :branch)))
    (should (null (plist-get parsed :suffix)))))

(ert-deftest beads-buffer-test-parse-utility-with-suffix ()
  "Test parsing utility buffer name with suffix."
  (let ((parsed (beads-buffer-parse-utility "*beads-dep-tree[proj]/bd-42*")))
    (should parsed)
    (should (string= "dep-tree" (plist-get parsed :type)))
    (should (string= "bd-42" (plist-get parsed :suffix)))))

(ert-deftest beads-buffer-test-parse-utility-with-branch ()
  "Test parsing utility buffer name with branch."
  (let ((parsed (beads-buffer-parse-utility "*beads-graph[proj@branch]*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))))

(ert-deftest beads-buffer-test-parse-utility-excludes-special ()
  "Test that utility parser excludes list/show/agent types."
  (should (null (beads-buffer-parse-utility "*beads-list[proj]*")))
  (should (null (beads-buffer-parse-utility "*beads-ready[proj]*")))
  (should (null (beads-buffer-parse-utility "*beads-blocked[proj]*")))
  (should (null (beads-buffer-parse-utility "*beads-show[proj]/bd-1*")))
  (should (null (beads-buffer-parse-utility "*beads-agent[proj]/cc#1*"))))

;;; Predicate Tests

(ert-deftest beads-buffer-test-list-p ()
  "Test list predicate."
  (should (beads-buffer-list-p "*beads-list[proj]*"))
  (should (beads-buffer-list-p "*beads-ready[proj]*"))
  (should (beads-buffer-list-p "*beads-blocked[proj@branch]*"))
  (should-not (beads-buffer-list-p "*beads-show[proj]/bd-1*"))
  (should-not (beads-buffer-list-p "*other*")))

(ert-deftest beads-buffer-test-show-p ()
  "Test show predicate."
  (should (beads-buffer-show-p "*beads-show[proj]/bd-42*"))
  (should (beads-buffer-show-p "*beads-show[proj@branch]/bd-1 Title*"))
  (should-not (beads-buffer-show-p "*beads-list[proj]*"))
  (should-not (beads-buffer-show-p "*other*")))

(ert-deftest beads-buffer-test-agent-p ()
  "Test agent predicate."
  (should (beads-buffer-agent-p "*beads-agent[proj]/Task#1*"))
  (should (beads-buffer-agent-p "*beads-agent[proj@branch]/Plan:cm#2 bd-1*"))
  (should-not (beads-buffer-agent-p "*beads-list[proj]*"))
  (should-not (beads-buffer-agent-p "*other*")))

(ert-deftest beads-buffer-test-utility-p ()
  "Test utility predicate."
  (should (beads-buffer-utility-p "*beads-stats[proj]*"))
  (should (beads-buffer-utility-p "*beads-graph[proj@branch]*"))
  (should (beads-buffer-utility-p "*beads-dep-tree[proj]/bd-1*"))
  (should-not (beads-buffer-utility-p "*beads-list[proj]*"))
  (should-not (beads-buffer-utility-p "*other*")))

(ert-deftest beads-buffer-test-beads-p ()
  "Test beads predicate (any beads buffer)."
  (should (beads-buffer-beads-p "*beads-list[proj]*"))
  (should (beads-buffer-beads-p "*beads-show[proj]/bd-1*"))
  (should (beads-buffer-beads-p "*beads-agent[proj]/Task#1*"))
  (should (beads-buffer-beads-p "*beads-stats[proj]*"))
  (should-not (beads-buffer-beads-p "*scratch*"))
  (should-not (beads-buffer-beads-p "*Messages*")))

;;; Buffer Finding Tests

(ert-deftest beads-buffer-test-find-list-buffers ()
  "Test finding list buffers."
  (let ((buf1 (generate-new-buffer "*beads-list[testproj]*"))
        (buf2 (generate-new-buffer "*beads-ready[testproj]*"))
        (buf3 (generate-new-buffer "*beads-list[other]*"))
        (buf4 (generate-new-buffer "*beads-show[testproj]/bd-1*")))
    (unwind-protect
        (progn
          ;; Find all list buffers
          (let ((found (beads-buffer-find-list-buffers)))
            (should (member buf1 found))
            (should (member buf2 found))
            (should (member buf3 found))
            (should-not (member buf4 found)))
          ;; Find list buffers for specific project
          (let ((found (beads-buffer-find-list-buffers "testproj")))
            (should (member buf1 found))
            (should (member buf2 found))
            (should-not (member buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3)
      (kill-buffer buf4))))

(ert-deftest beads-buffer-test-find-show-buffers ()
  "Test finding show buffers."
  (let ((buf1 (generate-new-buffer "*beads-show[testproj]/bd-1*"))
        (buf2 (generate-new-buffer "*beads-show[testproj]/bd-2 Title*"))
        (buf3 (generate-new-buffer "*beads-show[other]/bd-1*"))
        (buf4 (generate-new-buffer "*beads-list[testproj]*")))
    (unwind-protect
        (progn
          ;; Find all show buffers
          (let ((found (beads-buffer-find-show-buffers)))
            (should (member buf1 found))
            (should (member buf2 found))
            (should (member buf3 found))
            (should-not (member buf4 found)))
          ;; Find show buffers for specific project
          (let ((found (beads-buffer-find-show-buffers "testproj")))
            (should (member buf1 found))
            (should (member buf2 found))
            (should-not (member buf3 found)))
          ;; Find show buffer for specific issue
          (let ((found (beads-buffer-find-show-buffers nil "bd-1")))
            (should (member buf1 found))
            (should-not (member buf2 found))
            (should (member buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3)
      (kill-buffer buf4))))

(ert-deftest beads-buffer-test-find-agent-buffers ()
  "Test finding agent buffers."
  (let ((buf1 (generate-new-buffer "*beads-agent[testproj]/Task#1*"))
        (buf2 (generate-new-buffer "*beads-agent[testproj]/Plan#1*"))
        (buf3 (generate-new-buffer "*beads-agent[other]/Task#1*"))
        (buf4 (generate-new-buffer "*beads-list[testproj]*")))
    (unwind-protect
        (progn
          ;; Find all agent buffers
          (let ((found (beads-buffer-find-agent-buffers)))
            (should (member buf1 found))
            (should (member buf2 found))
            (should (member buf3 found))
            (should-not (member buf4 found)))
          ;; Find agent buffers for specific project
          (let ((found (beads-buffer-find-agent-buffers "testproj")))
            (should (member buf1 found))
            (should (member buf2 found))
            (should-not (member buf3 found)))
          ;; Find agent buffers for specific type
          (let ((found (beads-buffer-find-agent-buffers nil "Task")))
            (should (member buf1 found))
            (should-not (member buf2 found))
            (should (member buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3)
      (kill-buffer buf4))))

;;; Roundtrip Tests (generate then parse)

(ert-deftest beads-buffer-test-roundtrip-list ()
  "Test that list buffer names roundtrip correctly."
  (let* ((name (beads-buffer-list "ready" "label=api" "proj" "branch"))
         (parsed (beads-buffer-parse-list name)))
    (should parsed)
    (should (string= "ready" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "label=api" (plist-get parsed :filter)))))

(ert-deftest beads-buffer-test-roundtrip-show ()
  "Test that show buffer names roundtrip correctly."
  (let* ((name (beads-buffer-show "bd-42" "Fix bug" "proj" "branch"))
         (parsed (beads-buffer-parse-show name)))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix bug" (plist-get parsed :title)))))

(ert-deftest beads-buffer-test-roundtrip-agent ()
  "Test that agent buffer names roundtrip correctly."
  (let* ((name (beads-buffer-agent "Task" 5 "bd-42" "Fix auth" "proj" "branch"))
         (parsed (beads-buffer-parse-agent name)))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "Task" (plist-get parsed :type)))
    (should (= 5 (plist-get parsed :instance)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix auth" (plist-get parsed :title)))))

(ert-deftest beads-buffer-test-roundtrip-utility ()
  "Test that utility buffer names roundtrip correctly."
  (let* ((name (beads-buffer-utility "dep-tree" "bd-42" "proj" "branch"))
         (parsed (beads-buffer-parse-utility name)))
    (should parsed)
    (should (string= "dep-tree" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "branch" (plist-get parsed :branch)))
    (should (string= "bd-42" (plist-get parsed :suffix)))))

;;; Edge Case Tests

(ert-deftest beads-buffer-test-project-with-dots ()
  "Test project names containing dots (e.g., beads.el)."
  (let* ((name (beads-buffer-show "bd-42" "Fix bug" "beads.el" nil))
         (parsed (beads-buffer-parse-show name)))
    (should (string= "*beads-show[beads.el]/bd-42 Fix bug*" name))
    (should (string= "beads.el" (plist-get parsed :project)))))

(ert-deftest beads-buffer-test-branch-with-hyphen ()
  "Test branch names containing hyphens (common pattern)."
  (let* ((name (beads-buffer-list nil nil "proj" "feature-auth"))
         (parsed (beads-buffer-parse-list name)))
    (should (string= "*beads-list[proj@feature-auth]*" name))
    (should (string= "feature-auth" (plist-get parsed :branch)))))

;;; Backward Compatibility Tests

(ert-deftest beads-buffer-test-backward-compat-aliases ()
  "Test that backward compatibility aliases work."
  ;; Test function aliases
  (should (fboundp 'beads-buffer-name-list))
  (should (fboundp 'beads-buffer-name-show))
  (should (fboundp 'beads-buffer-name-agent))
  (should (fboundp 'beads-buffer-name-utility))
  (should (fboundp 'beads-buffer-name-parse-list))
  (should (fboundp 'beads-buffer-name-parse-show))
  (should (fboundp 'beads-buffer-name-parse-agent))
  (should (fboundp 'beads-buffer-name-parse-utility))
  ;; Test that aliases produce same results
  (should (string= (beads-buffer-list nil nil "proj" nil)
                   (beads-buffer-name-list nil nil "proj" nil))))

(provide 'beads-buffer-test)
;;; beads-buffer-test.el ends here
