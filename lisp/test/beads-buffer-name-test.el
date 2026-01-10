;;; beads-buffer-name-test.el --- Tests for beads-buffer-name -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the centralized buffer naming module.

;;; Code:

(require 'ert)
(require 'beads-buffer-name)

;;; Title Truncation Tests

(ert-deftest beads-buffer-name-test--truncate-title-nil ()
  "Test truncation of nil title."
  (should (string= "" (beads-buffer-name--truncate-title nil))))

(ert-deftest beads-buffer-name-test--truncate-title-empty ()
  "Test truncation of empty title."
  (should (string= "" (beads-buffer-name--truncate-title ""))))

(ert-deftest beads-buffer-name-test--truncate-title-short ()
  "Test truncation of short title (no truncation needed)."
  (should (string= "Fix bug" (beads-buffer-name--truncate-title "Fix bug"))))

(ert-deftest beads-buffer-name-test--truncate-title-exact-length ()
  "Test title at exactly max length."
  (let ((title (make-string 30 ?a)))
    (should (string= title (beads-buffer-name--truncate-title title)))))

(ert-deftest beads-buffer-name-test--truncate-title-long ()
  "Test truncation of long title."
  (let ((result (beads-buffer-name--truncate-title
                 "This is a very long title that needs to be truncated")))
    (should (= 30 (length result)))
    (should (string-suffix-p "..." result))))

;;; Project Prefix Tests

(ert-deftest beads-buffer-name-test--project-prefix-simple ()
  "Test project prefix without worktree."
  (cl-letf (((symbol-function 'beads-git-get-project-name) (lambda () "myproject"))
            ((symbol-function 'beads-git-in-worktree-p) (lambda () nil))
            ((symbol-function 'beads-git-find-project-root) (lambda () "/tmp/myproject")))
    (should (string= "myproject" (beads-buffer-name--project-prefix)))))

(ert-deftest beads-buffer-name-test--project-prefix-with-worktree ()
  "Test project prefix in worktree (uses branch name for disambiguation)."
  (cl-letf (((symbol-function 'beads-git-get-project-name) (lambda () "myproject"))
            ((symbol-function 'beads-git-in-worktree-p) (lambda () t))
            ((symbol-function 'beads-git-get-branch) (lambda () "feature-auth")))
    (should (string= "myproject@feature-auth" (beads-buffer-name--project-prefix)))))

(ert-deftest beads-buffer-name-test--project-prefix-explicit ()
  "Test project prefix with explicit values."
  (should (string= "proj" (beads-buffer-name--project-prefix "proj" nil)))
  (should (string= "proj@wt" (beads-buffer-name--project-prefix "proj" "wt"))))

(ert-deftest beads-buffer-name-test--project-prefix-unknown ()
  "Test project prefix when project is not detected."
  (cl-letf (((symbol-function 'beads-git-get-project-name) (lambda () nil))
            ((symbol-function 'beads-git-in-worktree-p) (lambda () nil)))
    (should (string= "unknown" (beads-buffer-name--project-prefix)))))

;;; List Buffer Name Tests

(ert-deftest beads-buffer-name-test-list-basic ()
  "Test basic list buffer name."
  (should (string= "*beads-list: proj*"
                   (beads-buffer-name-list nil nil "proj" nil))))

(ert-deftest beads-buffer-name-test-list-ready ()
  "Test ready list buffer name."
  (should (string= "*beads-ready: proj*"
                   (beads-buffer-name-list "ready" nil "proj" nil))))

(ert-deftest beads-buffer-name-test-list-blocked ()
  "Test blocked list buffer name."
  (should (string= "*beads-blocked: proj*"
                   (beads-buffer-name-list "blocked" nil "proj" nil))))

(ert-deftest beads-buffer-name-test-list-with-filter ()
  "Test list buffer name with filter."
  (should (string= "*beads-list: proj label=api*"
                   (beads-buffer-name-list nil "label=api" "proj" nil))))

(ert-deftest beads-buffer-name-test-list-with-status-filter ()
  "Test list buffer name with status filter."
  (should (string= "*beads-list: proj open*"
                   (beads-buffer-name-list nil "open" "proj" nil))))

(ert-deftest beads-buffer-name-test-list-with-worktree ()
  "Test list buffer name with worktree."
  (should (string= "*beads-list: proj@wt*"
                   (beads-buffer-name-list nil nil "proj" "wt"))))

(ert-deftest beads-buffer-name-test-list-worktree-and-filter ()
  "Test list buffer name with worktree and filter."
  (should (string= "*beads-list: proj@wt label=backend*"
                   (beads-buffer-name-list nil "label=backend" "proj" "wt"))))

;;; Show Buffer Name Tests

(ert-deftest beads-buffer-name-test-show-basic ()
  "Test basic show buffer name."
  (should (string= "*beads-show: proj/bd-42*"
                   (beads-buffer-name-show "bd-42" nil "proj" nil))))

(ert-deftest beads-buffer-name-test-show-with-title ()
  "Test show buffer name with title."
  (should (string= "*beads-show: proj/bd-42 Fix login bug*"
                   (beads-buffer-name-show "bd-42" "Fix login bug" "proj" nil))))

(ert-deftest beads-buffer-name-test-show-with-long-title ()
  "Test show buffer name with long title (truncated)."
  (let ((result (beads-buffer-name-show
                 "bd-42"
                 "This is a very long title that should be truncated"
                 "proj"
                 nil)))
    (should (string-prefix-p "*beads-show: proj/bd-42 " result))
    (should (string-suffix-p "...*" result))))

(ert-deftest beads-buffer-name-test-show-with-worktree ()
  "Test show buffer name with worktree."
  (should (string= "*beads-show: proj@wt/bd-42*"
                   (beads-buffer-name-show "bd-42" nil "proj" "wt"))))

(ert-deftest beads-buffer-name-test-show-full ()
  "Test show buffer name with all components."
  (should (string= "*beads-show: proj@wt/bd-42 Fix bug*"
                   (beads-buffer-name-show "bd-42" "Fix bug" "proj" "wt"))))

;;; Agent Buffer Name Tests

(ert-deftest beads-buffer-name-test-agent-basic ()
  "Test basic agent buffer name."
  (should (string= "*beads-agent: proj/claude-code#1*"
                   (beads-buffer-name-agent "claude-code" 1 nil nil "proj" nil))))

(ert-deftest beads-buffer-name-test-agent-with-issue ()
  "Test agent buffer name with issue ID."
  (should (string= "*beads-agent: proj/claudemacs#2 bd-42*"
                   (beads-buffer-name-agent "claudemacs" 2 "bd-42" nil "proj" nil))))

(ert-deftest beads-buffer-name-test-agent-with-issue-and-title ()
  "Test agent buffer name with issue ID and title."
  (should (string= "*beads-agent: proj/efrit#3 bd-42 Fix auth*"
                   (beads-buffer-name-agent "efrit" 3 "bd-42" "Fix auth" "proj" nil))))

(ert-deftest beads-buffer-name-test-agent-with-worktree ()
  "Test agent buffer name with worktree."
  (should (string= "*beads-agent: proj@wt/claude-code#1*"
                   (beads-buffer-name-agent "claude-code" 1 nil nil "proj" "wt"))))

(ert-deftest beads-buffer-name-test-agent-full ()
  "Test agent buffer name with all components."
  (should (string= "*beads-agent: proj@wt/claudemacs#2 bd-42 Fix bug*"
                   (beads-buffer-name-agent "claudemacs" 2 "bd-42" "Fix bug" "proj" "wt"))))

;;; Utility Buffer Name Tests

(ert-deftest beads-buffer-name-test-utility-basic ()
  "Test basic utility buffer name."
  (should (string= "*beads-stats: proj*"
                   (beads-buffer-name-utility "stats" nil "proj" nil))))

(ert-deftest beads-buffer-name-test-utility-with-suffix ()
  "Test utility buffer name with suffix."
  (should (string= "*beads-dep-tree: proj/bd-42*"
                   (beads-buffer-name-utility "dep-tree" "bd-42" "proj" nil))))

(ert-deftest beads-buffer-name-test-utility-with-worktree ()
  "Test utility buffer name with worktree."
  (should (string= "*beads-graph: proj@wt*"
                   (beads-buffer-name-utility "graph" nil "proj" "wt"))))

(ert-deftest beads-buffer-name-test-utility-full ()
  "Test utility buffer name with all components."
  (should (string= "*beads-dep-cycles: proj@wt/epic*"
                   (beads-buffer-name-utility "dep-cycles" "epic" "proj" "wt"))))

;;; Parse List Buffer Tests

(ert-deftest beads-buffer-name-test-parse-list-basic ()
  "Test parsing basic list buffer name."
  (let ((parsed (beads-buffer-name-parse-list "*beads-list: myproject*")))
    (should parsed)
    (should (string= "list" (plist-get parsed :type)))
    (should (string= "myproject" (plist-get parsed :project)))
    (should (null (plist-get parsed :worktree)))
    (should (null (plist-get parsed :filter)))))

(ert-deftest beads-buffer-name-test-parse-list-ready ()
  "Test parsing ready list buffer name."
  (let ((parsed (beads-buffer-name-parse-list "*beads-ready: proj*")))
    (should parsed)
    (should (string= "ready" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))))

(ert-deftest beads-buffer-name-test-parse-list-blocked ()
  "Test parsing blocked list buffer name."
  (let ((parsed (beads-buffer-name-parse-list "*beads-blocked: proj*")))
    (should parsed)
    (should (string= "blocked" (plist-get parsed :type)))))

(ert-deftest beads-buffer-name-test-parse-list-with-worktree ()
  "Test parsing list buffer name with worktree."
  (let ((parsed (beads-buffer-name-parse-list "*beads-list: proj@wt*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))))

(ert-deftest beads-buffer-name-test-parse-list-with-filter ()
  "Test parsing list buffer name with filter."
  (let ((parsed (beads-buffer-name-parse-list "*beads-list: proj label=api*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "label=api" (plist-get parsed :filter)))))

(ert-deftest beads-buffer-name-test-parse-list-worktree-and-filter ()
  "Test parsing list buffer name with worktree and filter."
  (let ((parsed (beads-buffer-name-parse-list "*beads-list: proj@wt label=backend*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))
    (should (string= "label=backend" (plist-get parsed :filter)))))

(ert-deftest beads-buffer-name-test-parse-list-invalid ()
  "Test parsing invalid list buffer name."
  (should (null (beads-buffer-name-parse-list "*beads-show: proj*")))
  (should (null (beads-buffer-name-parse-list "*other-buffer*"))))

;;; Parse Show Buffer Tests

(ert-deftest beads-buffer-name-test-parse-show-basic ()
  "Test parsing basic show buffer name."
  (let ((parsed (beads-buffer-name-parse-show "*beads-show: proj/bd-42*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (null (plist-get parsed :worktree)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (null (plist-get parsed :title)))))

(ert-deftest beads-buffer-name-test-parse-show-with-title ()
  "Test parsing show buffer name with title."
  (let ((parsed (beads-buffer-name-parse-show "*beads-show: proj/bd-42 Fix bug*")))
    (should parsed)
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix bug" (plist-get parsed :title)))))

(ert-deftest beads-buffer-name-test-parse-show-with-worktree ()
  "Test parsing show buffer name with worktree."
  (let ((parsed (beads-buffer-name-parse-show "*beads-show: proj@wt/bd-42*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))))

(ert-deftest beads-buffer-name-test-parse-show-full ()
  "Test parsing show buffer name with all components."
  (let ((parsed (beads-buffer-name-parse-show "*beads-show: proj@wt/bd-42 Fix login bug*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix login bug" (plist-get parsed :title)))))

(ert-deftest beads-buffer-name-test-parse-show-invalid ()
  "Test parsing invalid show buffer name."
  (should (null (beads-buffer-name-parse-show "*beads-list: proj*")))
  (should (null (beads-buffer-name-parse-show "*beads-show: proj*"))))

;;; Parse Agent Buffer Tests

(ert-deftest beads-buffer-name-test-parse-agent-basic ()
  "Test parsing basic agent buffer name."
  (let ((parsed (beads-buffer-name-parse-agent "*beads-agent: proj/claude-code#1*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (null (plist-get parsed :worktree)))
    (should (string= "claude-code" (plist-get parsed :backend)))
    (should (= 1 (plist-get parsed :instance)))
    (should (null (plist-get parsed :issue-id)))
    (should (null (plist-get parsed :title)))))

(ert-deftest beads-buffer-name-test-parse-agent-with-issue ()
  "Test parsing agent buffer name with issue."
  (let ((parsed (beads-buffer-name-parse-agent "*beads-agent: proj/claudemacs#2 bd-42*")))
    (should parsed)
    (should (string= "claudemacs" (plist-get parsed :backend)))
    (should (= 2 (plist-get parsed :instance)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))))

(ert-deftest beads-buffer-name-test-parse-agent-with-issue-and-title ()
  "Test parsing agent buffer name with issue and title."
  (let ((parsed (beads-buffer-name-parse-agent "*beads-agent: proj/efrit#3 bd-42 Fix auth*")))
    (should parsed)
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix auth" (plist-get parsed :title)))))

(ert-deftest beads-buffer-name-test-parse-agent-with-worktree ()
  "Test parsing agent buffer name with worktree."
  (let ((parsed (beads-buffer-name-parse-agent "*beads-agent: proj@wt/claude-code#1*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))))

(ert-deftest beads-buffer-name-test-parse-agent-invalid ()
  "Test parsing invalid agent buffer name."
  (should (null (beads-buffer-name-parse-agent "*beads-list: proj*")))
  (should (null (beads-buffer-name-parse-agent "*beads-agent: proj/backend*"))))

;;; Parse Utility Buffer Tests

(ert-deftest beads-buffer-name-test-parse-utility-basic ()
  "Test parsing basic utility buffer name."
  (let ((parsed (beads-buffer-name-parse-utility "*beads-stats: proj*")))
    (should parsed)
    (should (string= "stats" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))
    (should (null (plist-get parsed :worktree)))
    (should (null (plist-get parsed :suffix)))))

(ert-deftest beads-buffer-name-test-parse-utility-with-suffix ()
  "Test parsing utility buffer name with suffix."
  (let ((parsed (beads-buffer-name-parse-utility "*beads-dep-tree: proj/bd-42*")))
    (should parsed)
    (should (string= "dep-tree" (plist-get parsed :type)))
    (should (string= "bd-42" (plist-get parsed :suffix)))))

(ert-deftest beads-buffer-name-test-parse-utility-with-worktree ()
  "Test parsing utility buffer name with worktree."
  (let ((parsed (beads-buffer-name-parse-utility "*beads-graph: proj@wt*")))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))))

(ert-deftest beads-buffer-name-test-parse-utility-excludes-special ()
  "Test that utility parser excludes list/show/agent types."
  (should (null (beads-buffer-name-parse-utility "*beads-list: proj*")))
  (should (null (beads-buffer-name-parse-utility "*beads-ready: proj*")))
  (should (null (beads-buffer-name-parse-utility "*beads-blocked: proj*")))
  (should (null (beads-buffer-name-parse-utility "*beads-show: proj/bd-1*")))
  (should (null (beads-buffer-name-parse-utility "*beads-agent: proj/cc#1*"))))

;;; Predicate Tests

(ert-deftest beads-buffer-name-test-list-p ()
  "Test list predicate."
  (should (beads-buffer-name-list-p "*beads-list: proj*"))
  (should (beads-buffer-name-list-p "*beads-ready: proj*"))
  (should (beads-buffer-name-list-p "*beads-blocked: proj@wt*"))
  (should-not (beads-buffer-name-list-p "*beads-show: proj/bd-1*"))
  (should-not (beads-buffer-name-list-p "*other*")))

(ert-deftest beads-buffer-name-test-show-p ()
  "Test show predicate."
  (should (beads-buffer-name-show-p "*beads-show: proj/bd-42*"))
  (should (beads-buffer-name-show-p "*beads-show: proj@wt/bd-1 Title*"))
  (should-not (beads-buffer-name-show-p "*beads-list: proj*"))
  (should-not (beads-buffer-name-show-p "*other*")))

(ert-deftest beads-buffer-name-test-agent-p ()
  "Test agent predicate."
  (should (beads-buffer-name-agent-p "*beads-agent: proj/cc#1*"))
  (should (beads-buffer-name-agent-p "*beads-agent: proj@wt/cm#2 bd-1*"))
  (should-not (beads-buffer-name-agent-p "*beads-list: proj*"))
  (should-not (beads-buffer-name-agent-p "*other*")))

(ert-deftest beads-buffer-name-test-utility-p ()
  "Test utility predicate."
  (should (beads-buffer-name-utility-p "*beads-stats: proj*"))
  (should (beads-buffer-name-utility-p "*beads-graph: proj@wt*"))
  (should (beads-buffer-name-utility-p "*beads-dep-tree: proj/bd-1*"))
  (should-not (beads-buffer-name-utility-p "*beads-list: proj*"))
  (should-not (beads-buffer-name-utility-p "*other*")))

(ert-deftest beads-buffer-name-test-beads-p ()
  "Test beads predicate (any beads buffer)."
  (should (beads-buffer-name-beads-p "*beads-list: proj*"))
  (should (beads-buffer-name-beads-p "*beads-show: proj/bd-1*"))
  (should (beads-buffer-name-beads-p "*beads-agent: proj/cc#1*"))
  (should (beads-buffer-name-beads-p "*beads-stats: proj*"))
  (should-not (beads-buffer-name-beads-p "*scratch*"))
  (should-not (beads-buffer-name-beads-p "*Messages*")))

;;; Buffer Finding Tests

(ert-deftest beads-buffer-name-test-find-list-buffers ()
  "Test finding list buffers."
  (let ((buf1 (generate-new-buffer "*beads-list: testproj*"))
        (buf2 (generate-new-buffer "*beads-ready: testproj*"))
        (buf3 (generate-new-buffer "*beads-list: other*"))
        (buf4 (generate-new-buffer "*beads-show: testproj/bd-1*")))
    (unwind-protect
        (progn
          ;; Find all list buffers
          (let ((found (beads-buffer-name-find-list-buffers)))
            (should (member buf1 found))
            (should (member buf2 found))
            (should (member buf3 found))
            (should-not (member buf4 found)))
          ;; Find list buffers for specific project
          (let ((found (beads-buffer-name-find-list-buffers "testproj")))
            (should (member buf1 found))
            (should (member buf2 found))
            (should-not (member buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3)
      (kill-buffer buf4))))

(ert-deftest beads-buffer-name-test-find-show-buffers ()
  "Test finding show buffers."
  (let ((buf1 (generate-new-buffer "*beads-show: testproj/bd-1*"))
        (buf2 (generate-new-buffer "*beads-show: testproj/bd-2 Title*"))
        (buf3 (generate-new-buffer "*beads-show: other/bd-1*"))
        (buf4 (generate-new-buffer "*beads-list: testproj*")))
    (unwind-protect
        (progn
          ;; Find all show buffers
          (let ((found (beads-buffer-name-find-show-buffers)))
            (should (member buf1 found))
            (should (member buf2 found))
            (should (member buf3 found))
            (should-not (member buf4 found)))
          ;; Find show buffers for specific project
          (let ((found (beads-buffer-name-find-show-buffers "testproj")))
            (should (member buf1 found))
            (should (member buf2 found))
            (should-not (member buf3 found)))
          ;; Find show buffer for specific issue
          (let ((found (beads-buffer-name-find-show-buffers nil "bd-1")))
            (should (member buf1 found))
            (should-not (member buf2 found))
            (should (member buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3)
      (kill-buffer buf4))))

(ert-deftest beads-buffer-name-test-find-agent-buffers ()
  "Test finding agent buffers."
  (let ((buf1 (generate-new-buffer "*beads-agent: testproj/cc#1*"))
        (buf2 (generate-new-buffer "*beads-agent: testproj/cm#1*"))
        (buf3 (generate-new-buffer "*beads-agent: other/cc#1*"))
        (buf4 (generate-new-buffer "*beads-list: testproj*")))
    (unwind-protect
        (progn
          ;; Find all agent buffers
          (let ((found (beads-buffer-name-find-agent-buffers)))
            (should (member buf1 found))
            (should (member buf2 found))
            (should (member buf3 found))
            (should-not (member buf4 found)))
          ;; Find agent buffers for specific project
          (let ((found (beads-buffer-name-find-agent-buffers "testproj")))
            (should (member buf1 found))
            (should (member buf2 found))
            (should-not (member buf3 found)))
          ;; Find agent buffers for specific backend
          (let ((found (beads-buffer-name-find-agent-buffers nil "cc")))
            (should (member buf1 found))
            (should-not (member buf2 found))
            (should (member buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3)
      (kill-buffer buf4))))

;;; Roundtrip Tests (generate then parse)

(ert-deftest beads-buffer-name-test-roundtrip-list ()
  "Test that list buffer names roundtrip correctly."
  (let* ((name (beads-buffer-name-list "ready" "label=api" "proj" "wt"))
         (parsed (beads-buffer-name-parse-list name)))
    (should parsed)
    (should (string= "ready" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))
    (should (string= "label=api" (plist-get parsed :filter)))))

(ert-deftest beads-buffer-name-test-roundtrip-show ()
  "Test that show buffer names roundtrip correctly."
  (let* ((name (beads-buffer-name-show "bd-42" "Fix bug" "proj" "wt"))
         (parsed (beads-buffer-name-parse-show name)))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix bug" (plist-get parsed :title)))))

(ert-deftest beads-buffer-name-test-roundtrip-agent ()
  "Test that agent buffer names roundtrip correctly."
  (let* ((name (beads-buffer-name-agent "claude-code" 5 "bd-42" "Fix auth" "proj" "wt"))
         (parsed (beads-buffer-name-parse-agent name)))
    (should parsed)
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))
    (should (string= "claude-code" (plist-get parsed :backend)))
    (should (= 5 (plist-get parsed :instance)))
    (should (string= "bd-42" (plist-get parsed :issue-id)))
    (should (string= "Fix auth" (plist-get parsed :title)))))

(ert-deftest beads-buffer-name-test-roundtrip-utility ()
  "Test that utility buffer names roundtrip correctly."
  (let* ((name (beads-buffer-name-utility "dep-tree" "bd-42" "proj" "wt"))
         (parsed (beads-buffer-name-parse-utility name)))
    (should parsed)
    (should (string= "dep-tree" (plist-get parsed :type)))
    (should (string= "proj" (plist-get parsed :project)))
    (should (string= "wt" (plist-get parsed :worktree)))
    (should (string= "bd-42" (plist-get parsed :suffix)))))

(provide 'beads-buffer-name-test)
;;; beads-buffer-name-test.el ends here
