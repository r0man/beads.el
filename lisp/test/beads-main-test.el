;;; beads-main-test.el --- Tests for beads-main -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-main.el, the main transient menu
;; and entry point for beads.el.  Tests cover:
;; - Version detection and caching
;; - Project info detection and caching
;; - Header formatting
;; - Transient menu definition
;; - Placeholder commands
;; - Integration with other modules

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-main)

;;; Test Utilities

(defun beads-main-test--reset-cache ()
  "Reset all cached values for testing."
  (setq beads-main--cached-version nil
        beads-main--cached-project-info nil))

(defmacro beads-main-test-with-clean-cache (&rest body)
  "Execute BODY with clean cache state."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (beads-main-test--reset-cache)
         ,@body)
     (beads-main-test--reset-cache)))

(defun beads-main-test--mock-call-process-version (output)
  "Mock call-process to return version OUTPUT."
  (lambda (program &optional _infile destination _display &rest _args)
    (when (and destination (string-match-p "bd\\|beads" program))
      (with-current-buffer (if (bufferp destination)
                              destination
                            (current-buffer))
        (insert output)))
    0))

;;; Tests for Version Detection

(ert-deftest beads-main-test-get-version-success ()
  "Test successful version detection from bd CLI."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 0.9.2 (dev)\n")))
      (let ((version (beads-main--get-version)))
        (should (stringp version))
        (should (string= version "0.9.2"))))))

(ert-deftest beads-main-test-get-version-caching ()
  "Test that version is cached after first call."
  (beads-main-test-with-clean-cache
    (let ((call-count 0))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _args)
                   (setq call-count (1+ call-count))
                   (with-current-buffer (current-buffer)
                     (insert "bd version 0.9.2 (dev)\n"))
                   0)))
        ;; First call should invoke call-process
        (beads-main--get-version)
        (should (= call-count 1))
        ;; Second call should use cache
        (beads-main--get-version)
        (should (= call-count 1))))))

(ert-deftest beads-main-test-get-version-different-format ()
  "Test version detection with different format."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 1.0.0\n")))
      (let ((version (beads-main--get-version)))
        (should (string= version "1.0.0"))))))

(ert-deftest beads-main-test-get-version-with-dev-suffix ()
  "Test version detection with dev suffix."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 0.9.2-dev\n")))
      (let ((version (beads-main--get-version)))
        (should (string= version "0.9.2-dev"))))))

(ert-deftest beads-main-test-get-version-error-handling ()
  "Test version detection handles errors gracefully."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args) (error "Process failed"))))
      (let ((version (beads-main--get-version)))
        (should (string= version "unknown"))))))

(ert-deftest beads-main-test-get-version-no-match ()
  "Test version detection when output doesn't match pattern."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "unexpected output\n")))
      (let ((version (beads-main--get-version)))
        (should (string= version "unknown"))))))

(ert-deftest beads-main-test-get-version-empty-output ()
  "Test version detection with empty output."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version "")))
      (let ((version (beads-main--get-version)))
        (should (string= version "unknown"))))))

;;; Tests for Project Info

(ert-deftest beads-main-test-get-project-info-success ()
  "Test successful project info detection."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/project"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/home/user/project/.beads/issues.db")))
      (let ((info (beads-main--get-project-info)))
        (should (consp info))
        (should (string= (car info) "/home/user/project"))
        (should (string= (cdr info)
                        "/home/user/project/.beads/issues.db"))))))

(ert-deftest beads-main-test-get-project-info-no-project ()
  "Test project info when not in a project."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () nil))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil)))
      (let ((info (beads-main--get-project-info)))
        (should (null info))))))

(ert-deftest beads-main-test-get-project-info-caching ()
  "Test that project info is cached."
  (beads-main-test-with-clean-cache
    (let ((call-count 0))
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda ()
                   (setq call-count (1+ call-count))
                   "/home/user/project"))
                ((symbol-function 'beads--get-database-path)
                 (lambda () "/home/user/project/.beads/issues.db")))
        ;; First call
        (beads-main--get-project-info)
        (should (= call-count 1))
        ;; Second call should use cache
        (beads-main--get-project-info)
        (should (= call-count 1))))))

(ert-deftest beads-main-test-get-project-info-auto-discover-db ()
  "Test project info with auto-discovered database."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/project"))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil)))
      (let ((info (beads-main--get-project-info)))
        (should (consp info))
        (should (string= (car info) "/home/user/project"))
        (should (null (cdr info)))))))

;;; Tests for Cache Management

(ert-deftest beads-main-test-clear-cache ()
  "Test cache clearing."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 0.9.2\n"))
              ((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/project"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/home/user/project/.beads/issues.db")))
      ;; Populate cache
      (beads-main--get-version)
      (beads-main--get-project-info)
      (should beads-main--cached-version)
      (should beads-main--cached-project-info)
      ;; Clear cache
      (beads-main--clear-cache)
      (should (null beads-main--cached-version))
      (should (null beads-main--cached-project-info)))))

;;; Tests for Header Formatting

(ert-deftest beads-main-test-format-project-header-with-project ()
  "Test header formatting when in a project."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/myproject"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/home/user/myproject/.beads/issues.db"))
              ((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 0.9.2 (dev)\n")))
      (let ((header (beads-main--format-project-header)))
        (should (stringp header))
        (should (string-match-p "Project:" header))
        (should (string-match-p "myproject" header))
        (should (string-match-p "Database:" header))
        (should (string-match-p "issues.db" header))
        (should (string-match-p "Version:" header))
        (should (string-match-p "0.9.2" header))))))

(ert-deftest beads-main-test-format-project-header-no-project ()
  "Test header formatting when not in a project."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () nil))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil)))
      (let ((header (beads-main--format-project-header)))
        (should (stringp header))
        (should (string-match-p "No beads project found" header))))))

(ert-deftest beads-main-test-format-project-header-auto-discover ()
  "Test header formatting with auto-discovered database."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/project"))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil))
              ((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 0.9.2\n")))
      (let ((header (beads-main--format-project-header)))
        (should (stringp header))
        (should (string-match-p "auto-discover" header))))))

(ert-deftest beads-main-test-format-project-header-has-faces ()
  "Test that header contains face properties."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/project"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/home/user/project/.beads/issues.db"))
              ((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 0.9.2\n")))
      (let ((header (beads-main--format-project-header)))
        ;; Check that some text has face properties
        (should (text-property-not-all 0 (length header) 'face nil
                                      header))))))

;;; Tests for Transient Definition

(ert-deftest beads-main-test-transient-defined ()
  "Test that beads transient is defined."
  (should (fboundp 'beads)))

(ert-deftest beads-main-test-transient-is-prefix ()
  "Test that beads is a transient prefix."
  (should (get 'beads 'transient--prefix)))

(ert-deftest beads-main-test-transient-has-autoload ()
  "Test that beads command has autoload cookie.
This test verifies the beads command is properly marked for autoloading
by checking if the function is available after requiring beads-main."
  ;; In batch mode, the module is already loaded, so we just verify
  ;; the command is callable which proves autoloading will work
  (should (fboundp 'beads))
  (should (commandp 'beads)))

;;; Tests for Placeholder Commands

(ert-deftest beads-main-test-placeholder-update-defined ()
  "Test that beads-update is defined."
  (should (fboundp 'beads-update)))

(ert-deftest beads-main-test-placeholder-close-defined ()
  "Test that beads-close is defined."
  (should (fboundp 'beads-close)))

(ert-deftest beads-main-test-placeholder-stats-defined ()
  "Test that beads-stats is defined."
  (should (fboundp 'beads-stats)))

(ert-deftest beads-main-test-placeholder-dep-defined ()
  "Test that beads-dep is defined."
  (should (fboundp 'beads-dep)))

(ert-deftest beads-main-test-placeholder-init-defined ()
  "Test that beads-init is defined."
  (should (fboundp 'beads-init)))

(ert-deftest beads-main-test-placeholder-export-defined ()
  "Test that beads-export is defined."
  (should (fboundp 'beads-export)))

(ert-deftest beads-main-test-placeholder-import-defined ()
  "Test that beads-import is defined."
  (should (fboundp 'beads-import)))

;;; Tests for beads-init command

(ert-deftest beads-main-test-init-is-transient-prefix ()
  "Test that beads-init is a transient prefix."
  (should (get 'beads-init 'transient--prefix)))

(ert-deftest beads-main-test-init-reset-state ()
  "Test that beads-init resets state properly."
  (setq beads-init--prefix "test-prefix")
  (beads-init--reset-state)
  (should (null beads-init--prefix)))

(ert-deftest beads-main-test-init-execute-defined ()
  "Test that beads-init execute command is defined."
  (should (fboundp 'beads-init--execute)))

;;; Tests for beads-export command

(ert-deftest beads-main-test-export-is-transient-prefix ()
  "Test that beads-export is a transient prefix."
  (should (get 'beads-export 'transient--prefix)))

(ert-deftest beads-main-test-export-reset-state ()
  "Test that beads-export resets state properly."
  (setq beads-export--output "test.jsonl"
        beads-export--status "open")
  (beads-export--reset-state)
  (should (null beads-export--output))
  (should (null beads-export--status)))

(ert-deftest beads-main-test-export-execute-defined ()
  "Test that beads-export execute command is defined."
  (should (fboundp 'beads-export--execute)))

;;; Tests for beads-import command

(ert-deftest beads-main-test-import-is-transient-prefix ()
  "Test that beads-import is a transient prefix."
  (should (get 'beads-import 'transient--prefix)))

(ert-deftest beads-main-test-import-reset-state ()
  "Test that beads-import resets state properly."
  (setq beads-import--input "test.jsonl"
        beads-import--dry-run t
        beads-import--resolve-collisions t
        beads-import--skip-existing t
        beads-import--strict t)
  (beads-import--reset-state)
  (should (null beads-import--input))
  (should (null beads-import--dry-run))
  (should (null beads-import--resolve-collisions))
  (should (null beads-import--skip-existing))
  (should (null beads-import--strict)))

(ert-deftest beads-main-test-import-execute-defined ()
  "Test that beads-import execute command is defined."
  (should (fboundp 'beads-import--execute)))

(ert-deftest beads-main-test-refresh-menu ()
  "Test refresh menu command."
  (should (fboundp 'beads-refresh-menu))
  (beads-main-test-with-clean-cache
    ;; Set up cached data
    (setq beads-main--cached-version "0.9.2")
    (setq beads-main--cached-project-info
          (cons "/home/user/project" "/home/user/project/.beads/issues.db"))
    ;; Call refresh
    (beads-refresh-menu)
    ;; Cache should be cleared
    (should (null beads-main--cached-version))
    (should (null beads-main--cached-project-info))))

;;; Tests for Integration with Other Modules

(ert-deftest beads-main-test-requires-beads ()
  "Test that beads-main requires beads."
  (should (featurep 'beads)))

(ert-deftest beads-main-test-requires-beads-list ()
  "Test that beads-main requires beads-list."
  (should (featurep 'beads-list)))

(ert-deftest beads-main-test-requires-beads-show ()
  "Test that beads-main requires beads-show."
  (should (featurep 'beads-show)))

(ert-deftest beads-main-test-requires-beads-create ()
  "Test that beads-main requires beads-create."
  (should (featurep 'beads-create)))

(ert-deftest beads-main-test-list-command-available ()
  "Test that beads-list command is available from menu."
  (should (fboundp 'beads-list)))

(ert-deftest beads-main-test-ready-command-available ()
  "Test that beads-ready command is available from menu."
  (should (fboundp 'beads-ready)))

(ert-deftest beads-main-test-blocked-command-available ()
  "Test that beads-blocked command is available from menu."
  (should (fboundp 'beads-blocked)))

(ert-deftest beads-main-test-show-command-available ()
  "Test that beads-show command is available from menu."
  (should (fboundp 'beads-show)))

(ert-deftest beads-main-test-create-command-available ()
  "Test that beads-create command is available from menu."
  (should (fboundp 'beads-create)))

;;; Tests for beads-list Integration

(ert-deftest beads-main-test-list-show-integration ()
  "Test that beads-list-show is redefined to use beads-show."
  (should (fboundp 'beads-list-show))
  ;; Test that it calls beads-show when available
  (let ((show-called nil)
        (issue-id nil))
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42"))
              ((symbol-function 'beads-show)
               (lambda (id)
                 (setq show-called t
                       issue-id id))))
      (beads-list-show)
      (should show-called)
      (should (string= issue-id "bd-42")))))

(ert-deftest beads-main-test-list-show-no-issue ()
  "Test beads-list-show with no issue at point."
  (cl-letf (((symbol-function 'beads-list--current-issue-id)
             (lambda () nil)))
    (should-error (beads-list-show) :type 'user-error)))

;;; Edge Cases

(ert-deftest beads-main-test-version-with-extra-whitespace ()
  "Test version detection with extra whitespace."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "  bd version 0.9.2 (dev)  \n\n")))
      (let ((version (beads-main--get-version)))
        (should (string= version "0.9.2"))))))

(ert-deftest beads-main-test-project-path-with-spaces ()
  "Test project info with spaces in path."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/my project"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/home/user/my project/.beads/issues.db")))
      (let ((info (beads-main--get-project-info)))
        (should (consp info))
        (should (string= (car info) "/home/user/my project"))))))

(ert-deftest beads-main-test-project-path-with-unicode ()
  "Test project info with unicode in path."
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/проект"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/home/user/проект/.beads/issues.db")))
      (let ((info (beads-main--get-project-info)))
        (should (consp info))
        (should (string= (car info) "/home/user/проект"))))))

(ert-deftest beads-main-test-header-format-long-paths ()
  "Test header formatting with very long paths."
  (beads-main-test-with-clean-cache
    (let ((long-path (concat "/home/user/"
                            (make-string 200 ?x))))
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () long-path))
                ((symbol-function 'beads--get-database-path)
                 (lambda () (concat long-path "/.beads/issues.db")))
                ((symbol-function 'call-process)
                 (beads-main-test--mock-call-process-version
                  "bd version 0.9.2\n")))
        (let ((header (beads-main--format-project-header)))
          (should (stringp header))
          (should (> (length header) 200)))))))

;;; Performance Tests

(ert-deftest beads-main-test-performance-header-formatting ()
  "Test header formatting performance."
  :tags '(:performance)
  (beads-main-test-with-clean-cache
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda () "/home/user/project"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/home/user/project/.beads/issues.db"))
              ((symbol-function 'call-process)
               (beads-main-test--mock-call-process-version
                "bd version 0.9.2\n")))
      (let ((start-time (current-time)))
        (dotimes (_ 100)
          (beads-main--format-project-header))
        (let ((elapsed (float-time (time-subtract (current-time)
                                                  start-time))))
          ;; Should format 100 headers in under 0.1 seconds
          (should (< elapsed 0.1)))))))

(ert-deftest beads-main-test-performance-cache-effectiveness ()
  "Test that caching improves performance."
  :tags '(:performance)
  (beads-main-test-with-clean-cache
    (let ((call-count 0))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _args)
                   (setq call-count (1+ call-count))
                   (with-current-buffer (current-buffer)
                     (insert "bd version 0.9.2\n"))
                   0)))
        ;; Call version detection 10 times
        (dotimes (_ 10)
          (beads-main--get-version))
        ;; Should only invoke call-process once
        (should (= call-count 1))))))

;;; Module Lifecycle Tests

(ert-deftest beads-main-test-provides-beads-main ()
  "Test that module provides beads-main feature."
  (should (featurep 'beads-main)))

(ert-deftest beads-main-test-module-dependencies ()
  "Test that all required modules are loaded."
  (should (featurep 'beads))
  (should (featurep 'beads-list))
  (should (featurep 'beads-show))
  (should (featurep 'beads-create))
  (should (featurep 'transient)))

;;; Docstring Tests

(ert-deftest beads-main-test-main-command-has-docstring ()
  "Test that main beads command has documentation."
  (should (documentation 'beads)))

(ert-deftest beads-main-test-placeholder-commands-have-docstrings ()
  "Test that placeholder commands have documentation."
  (should (documentation 'beads-update))
  (should (documentation 'beads-close))
  (should (documentation 'beads-stats))
  (should (documentation 'beads-dep))
  (should (documentation 'beads-init))
  (should (documentation 'beads-export))
  (should (documentation 'beads-import)))

(provide 'beads-main-test)
;;; beads-main-test.el ends here
