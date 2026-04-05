;;; beads-command-search-test.el --- Tests for beads-command-search -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the beads-command-search module.

;;; Code:

(require 'ert)
(require 'beads-command-search)
(require 'beads-test)

;;; Class Tests

(ert-deftest beads-command-search-test-class-exists ()
  "Test that beads-command-search class is defined."
  (should (find-class 'beads-command-search)))

(ert-deftest beads-command-search-test-inherits-from-json ()
  "Test that beads-command-search inherits from beads-command."
  (should (child-of-class-p 'beads-command-search 'beads-command)))

(ert-deftest beads-command-search-test-create-default ()
  "Test creating search command with defaults."
  (let ((cmd (beads-command-search)))
    (should (null (oref cmd query)))
    (should (null (oref cmd status)))
    (should (null (oref cmd assignee)))
    (should (null (oref cmd limit)))))

(ert-deftest beads-command-search-test-create-with-query ()
  "Test creating search command with query."
  (let ((cmd (beads-command-search :query "authentication")))
    (should (equal "authentication" (oref cmd query)))))

(ert-deftest beads-command-search-test-create-with-status ()
  "Test creating search command with status filter."
  (let ((cmd (beads-command-search :status "open")))
    (should (equal "open" (oref cmd status)))))

(ert-deftest beads-command-search-test-create-with-assignee ()
  "Test creating search command with assignee filter."
  (let ((cmd (beads-command-search :assignee "alice")))
    (should (equal "alice" (oref cmd assignee)))))

(ert-deftest beads-command-search-test-create-with-type ()
  "Test creating search command with type filter."
  (let ((cmd (beads-command-search :issue-type "bug")))
    (should (equal "bug" (oref cmd issue-type)))))

(ert-deftest beads-command-search-test-create-with-sort ()
  "Test creating search command with sort option."
  (let ((cmd (beads-command-search :sort "priority" :reverse t)))
    (should (equal "priority" (oref cmd sort)))
    (should (eq t (oref cmd reverse)))))

;;; Subcommand Tests

(ert-deftest beads-command-search-test-subcommand ()
  "Test subcommand method returns 'search'."
  (let ((cmd (beads-command-search)))
    (should (equal "search" (beads-command-subcommand cmd)))))

;;; Validation Tests

(ert-deftest beads-command-search-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-search)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-search-test-validate-with-options ()
  "Test validation passes with various options."
  (let ((cmd (beads-command-search :query "bug"
                                    :status "open"
                                    :assignee "bob"
                                    :limit 20)))
    (should (null (beads-command-validate cmd)))))

;;; Command Line Tests

(ert-deftest beads-command-search-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-search :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "search" args)))))

(ert-deftest beads-command-search-test-command-line-with-json ()
  "Test command line includes --json when enabled."
  (let ((cmd (beads-command-search :json t)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-command-search-test-command-line-with-query ()
  "Test command line includes query as positional argument."
  (let ((cmd (beads-command-search :query "authentication")))
    (let ((args (beads-command-line cmd)))
      (should (member "authentication" args)))))

(ert-deftest beads-command-search-test-command-line-with-status ()
  "Test command line includes --status option."
  (let ((cmd (beads-command-search :status "open")))
    (let ((args (beads-command-line cmd)))
      (should (member "--status" args))
      (should (member "open" args)))))

(ert-deftest beads-command-search-test-command-line-with-type ()
  "Test command line includes --type option."
  (let ((cmd (beads-command-search :issue-type "bug")))
    (let ((args (beads-command-line cmd)))
      (should (member "--type" args))
      (should (member "bug" args)))))

(ert-deftest beads-command-search-test-command-line-with-sort ()
  "Test command line includes --sort and --reverse options."
  (let ((cmd (beads-command-search :sort "priority" :reverse t)))
    (let ((args (beads-command-line cmd)))
      (should (member "--sort" args))
      (should (member "priority" args))
      (should (member "--reverse" args)))))

(ert-deftest beads-command-search-test-command-line-with-limit ()
  "Test command line includes --limit option."
  (let ((cmd (beads-command-search :limit 100)))
    (let ((args (beads-command-line cmd)))
      (should (member "--limit" args))
      (should (member "100" args)))))

(ert-deftest beads-command-search-test-command-line-with-dates ()
  "Test command line includes date filter options."
  (let ((cmd (beads-command-search :created-after "2025-01-01"
                                    :updated-before "2025-12-31")))
    (let ((args (beads-command-line cmd)))
      (should (member "--created-after" args))
      (should (member "2025-01-01" args))
      (should (member "--updated-before" args))
      (should (member "2025-12-31" args)))))

;;; Slot Property Tests

(ert-deftest beads-command-search-test-query-slot-properties ()
  "Test query slot has correct transient properties."
  (should (equal "q" (beads-meta-slot-property
                      'beads-command-search 'query :transient-key)))
  (should (eq 1 (beads-meta-slot-property
                 'beads-command-search 'query :positional))))

(ert-deftest beads-command-search-test-status-slot-properties ()
  "Test status slot has correct transient properties."
  (should (equal "s" (beads-meta-slot-property
                      'beads-command-search 'status :transient-key)))
  (should (equal "status" (beads-meta-slot-property
                           'beads-command-search 'status :long-option))))

(ert-deftest beads-command-search-test-sort-slot-properties ()
  "Test sort slot has correct transient properties."
  (should (equal "o" (beads-meta-slot-property
                      'beads-command-search 'sort :transient-key)))
  (should (equal "sort" (beads-meta-slot-property
                         'beads-command-search 'sort :long-option)))
  (should (equal '("priority" "created" "updated" "closed" "status" "id"
                   "title" "type" "assignee")
                 (beads-meta-slot-property
                  'beads-command-search 'sort :transient-choices))))

;;; Transient Tests

(ert-deftest beads-command-search-test-transient-defined ()
  "Test that beads-search transient prefix is defined."
  (should (fboundp 'beads-search)))

(ert-deftest beads-command-search-test-transient-is-prefix ()
  "Test that beads-search is a transient prefix."
  (should (get 'beads-search 'transient--prefix)))

(ert-deftest beads-command-search-test-execute-suffix-defined ()
  "Test that execute suffix is defined."
  (should (fboundp 'beads-search--execute)))

(ert-deftest beads-command-search-test-preview-suffix-defined ()
  "Test that preview suffix is defined."
  (should (fboundp 'beads-search--preview)))

(ert-deftest beads-command-search-test-reset-suffix-defined ()
  "Test that reset suffix is defined."
  (should (fboundp 'beads-search--reset)))

;;; Parse Tests

(ert-deftest beads-command-search-test-parse-as-issues ()
  "Test that search command parses JSON arrays into beads-issue objects."
  (let* ((cmd (beads-command-search :json t))
         (json-output "[{\"id\":\"bd-1\",\"title\":\"Test\",\"status\":\"open\",\"priority\":2}]"))
    (let ((result (beads-command-parse cmd json-output)))
      (should (listp result))
      (should (= 1 (length result)))
      (should (beads-issue-p (car result)))
      (should (equal "bd-1" (oref (car result) id))))))

;;; Interactive Execution Tests

(ert-deftest beads-command-search-test-execute-interactive-creates-list-buffer ()
  "Test that execute-interactive creates a beads-list-mode buffer."
  :tags '(:integration :transient)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'beads-command-execute)
             (lambda (cmd &rest _args)
               (list
                (beads-issue :id "bd-1"
                             :title "Search result"
                             :status "open"
                             :priority 2
                             :issue-type "bug"
                             :created-at "2025-10-20T14:30:00Z"))))
            ((symbol-function 'beads-git-find-project-root)
             (lambda () default-directory))
            ((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-git-get-branch)
             (lambda () "main"))
            ((symbol-function 'beads-buffer-is-main-branch-p)
             (lambda () t))
            ((symbol-function 'beads-git-in-worktree-p)
             (lambda () nil)))
    (let ((cmd (beads-command-search :query "test")))
      (beads-command-execute-interactive cmd))
    ;; Verify buffer was created with search type
    (should (get-buffer "*beads-search[testproj]*"))
    (with-current-buffer "*beads-search[testproj]*"
      (should (eq major-mode 'beads-list-mode))
      (should (= 1 (length tabulated-list-entries)))
      (should (eq 'search beads-list--command))
      (should beads-search--command-obj))
    ;; Clean up
    (kill-buffer "*beads-search[testproj]*")))

(ert-deftest beads-command-search-test-execute-interactive-empty-results ()
  "Test that execute-interactive handles empty search results."
  :tags '(:integration :transient)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'beads-command-execute)
             (lambda (cmd &rest _args)
               nil))
            ((symbol-function 'beads-git-find-project-root)
             (lambda () default-directory))
            ((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-git-get-branch)
             (lambda () "main"))
            ((symbol-function 'beads-buffer-is-main-branch-p)
             (lambda () t))
            ((symbol-function 'beads-git-in-worktree-p)
             (lambda () nil)))
    (let ((cmd (beads-command-search :query "nonexistent")))
      (beads-command-execute-interactive cmd))
    (should (get-buffer "*beads-search[testproj]*"))
    (with-current-buffer "*beads-search[testproj]*"
      (should (eq major-mode 'beads-list-mode))
      (should (null tabulated-list-entries)))
    ;; Clean up
    (kill-buffer "*beads-search[testproj]*")))

(ert-deftest beads-command-search-test-execute-interactive-forces-json ()
  "Test that execute-interactive sets json=t on the command."
  :tags '(:integration)
  (let ((captured-cmd nil))
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () t))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd &rest _args)
                 (setq captured-cmd cmd)
                 nil))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () default-directory))
              ((symbol-function 'beads-git-get-project-name)
               (lambda () "testproj"))
              ((symbol-function 'beads-git-get-branch)
               (lambda () "main"))
              ((symbol-function 'beads-buffer-is-main-branch-p)
               (lambda () t))
              ((symbol-function 'beads-git-in-worktree-p)
               (lambda () nil)))
      (let ((cmd (beads-command-search :query "test" :json nil)))
        (beads-command-execute-interactive cmd)
        ;; JSON should have been forced on
        (should (oref captured-cmd json))))
    ;; Clean up
    (when (get-buffer "*beads-search[testproj]*")
      (kill-buffer "*beads-search[testproj]*"))))

(ert-deftest beads-command-search-test-execute-interactive-error-handling ()
  "Test that execute-interactive handles errors gracefully."
  :tags '(:integration)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'beads-command-execute)
             (lambda (_cmd &rest _args)
               (error "Search failed: connection refused")))
            ((symbol-function 'beads-git-find-project-root)
             (lambda () default-directory))
            ((symbol-function 'beads-git-get-project-name)
             (lambda () "testproj"))
            ((symbol-function 'beads-git-get-branch)
             (lambda () "main"))
            ((symbol-function 'beads-buffer-is-main-branch-p)
             (lambda () t))
            ((symbol-function 'beads-git-in-worktree-p)
             (lambda () nil)))
    ;; Should not signal an error
    (let ((cmd (beads-command-search :query "test")))
      (beads-command-execute-interactive cmd))
    ;; Clean up any buffer that may have been created
    (when (get-buffer "*beads-search[testproj]*")
      (kill-buffer "*beads-search[testproj]*"))))

;;; Buffer Name Tests

(ert-deftest beads-command-search-test-buffer-name ()
  "Test that search buffers get correct names."
  (should (equal "*beads-search[myproj]*"
                 (beads-buffer-list "search" nil "myproj"))))

(ert-deftest beads-command-search-test-buffer-parse ()
  "Test that search buffer names are parsed correctly."
  (let ((parsed (beads-buffer-parse-list "*beads-search[myproj]*")))
    (should parsed)
    (should (equal "search" (plist-get parsed :type)))
    (should (equal "myproj" (plist-get parsed :project)))))

(ert-deftest beads-command-search-test-buffer-not-utility ()
  "Test that search buffers are not misidentified as utility buffers."
  (should-not (beads-buffer-parse-utility "*beads-search[myproj]*")))

(provide 'beads-command-search-test)
;;; beads-command-search-test.el ends here
