;;; beads-tramp-test.el --- Tests for beads.el Tramp support
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads.el Tramp support.  These tests verify that
;; beads.el works correctly over Tramp remote connections.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)

;;; Test Utilities

(defmacro beads-tramp-test-with-temp-config (&rest body)
  "Execute BODY with temporary beads configuration."
  `(let ((beads-executable "bd")
         (beads-database-path nil)
         (beads-actor nil)
         (beads-enable-debug nil)
         (beads-auto-refresh t)
         (beads--project-cache (make-hash-table :test 'equal)))
     ;; Mock beads--find-beads-dir to prevent auto-discovery
     (cl-letf (((symbol-function 'beads--find-beads-dir)
                (lambda (&optional _dir) nil)))
       ,@body)))

(defun beads-tramp-test--mock-process-file (exit-code output-text)
  "Create a mock for `process-file' returning EXIT-CODE and OUTPUT-TEXT."
  (let ((exit exit-code)
        (text output-text))
    (lambda (program &optional infile destination display &rest args)
      (when destination
        (with-current-buffer (if (bufferp destination)
                                 destination
                               (current-buffer))
          (insert text)))
      exit)))

;;; Tests for Tramp-aware synchronous execution

(ert-deftest beads-tramp-test-run-command-with-local-path ()
  "Test that process-file is used with local default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/tmp/")
         (json-output "{}")
         (process-file-called nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display
                                &rest args)
                  (setq process-file-called t)
                  (when destination
                    (with-current-buffer (current-buffer)
                      (insert json-output)))
                  0)))
       (beads--run-command "list")
       (should process-file-called)))))

(ert-deftest beads-tramp-test-run-command-with-tramp-path ()
  "Test that process-file respects Tramp default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (json-output "{}")
         (process-file-called nil)
         (captured-default-directory nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display
                                &rest args)
                  (setq process-file-called t)
                  (setq captured-default-directory default-directory)
                  (when destination
                    (with-current-buffer (current-buffer)
                      (insert json-output)))
                  0)))
       (beads--run-command "list")
       (should process-file-called)
       (should (string= captured-default-directory
                        "/ssh:remote:/home/user/project/"))))))

(ert-deftest beads-tramp-test-run-command-different-tramp-methods ()
  "Test process-file with different Tramp methods."
  (beads-tramp-test-with-temp-config
   (dolist (path '("/ssh:host:/path/"
                   "/scp:host:/path/"
                   "/sudo:root@localhost:/root/"))
     (let ((default-directory path)
           (json-output "{}")
           (process-file-called nil))
       (cl-letf (((symbol-function 'process-file)
                  (lambda (program &optional infile destination display
                                  &rest args)
                    (setq process-file-called t)
                    (when destination
                      (with-current-buffer (current-buffer)
                        (insert json-output)))
                    0)))
         (beads--run-command "list")
         (should process-file-called))))))

;;; Tests for Tramp-aware asynchronous execution

(ert-deftest beads-tramp-test-run-command-async-with-local-path ()
  "Test that start-file-process is used with local default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/tmp/")
         (start-file-process-called nil))
     (cl-letf (((symbol-function 'start-file-process)
                (lambda (name buffer program &rest args)
                  (setq start-file-process-called t)
                  (make-process
                   :name name
                   :buffer buffer
                   :command (list "true")
                   :noquery t))))
       (beads--run-command-async (lambda (_result) nil) "list")
       (should start-file-process-called)))))

(ert-deftest beads-tramp-test-run-command-async-with-tramp-path ()
  "Test that start-file-process respects Tramp default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (start-file-process-called nil)
         (captured-default-directory nil))
     (cl-letf (((symbol-function 'start-file-process)
                (lambda (name buffer program &rest args)
                  (setq start-file-process-called t)
                  (setq captured-default-directory default-directory)
                  (make-process
                   :name name
                   :buffer buffer
                   :command (list "true")
                   :noquery t))))
       (beads--run-command-async (lambda (_result) nil) "list")
       (should start-file-process-called)
       (should (string= captured-default-directory
                        "/ssh:remote:/home/user/project/"))))))

;;; Tests for project discovery over Tramp

(ert-deftest beads-tramp-test-find-beads-dir-with-tramp ()
  "Test that beads--find-beads-dir works with Tramp paths."
  (let ((beads--project-cache (make-hash-table :test 'equal))
        (locate-dominating-file-called nil)
        (remote-dir "/ssh:host:/home/user/project/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (file name)
                 (setq locate-dominating-file-called t)
                 (should (string-prefix-p "/ssh:host:" file))
                 (when (equal name ".beads")
                   remote-dir)))
              ((symbol-function 'beads--find-project-root)
               (lambda () nil)))
      (let ((result (beads--find-beads-dir remote-dir)))
        (should locate-dominating-file-called)
        (should (string-prefix-p "/ssh:host:" result))))))

(ert-deftest beads-tramp-test-get-database-path-with-tramp ()
  "Test that beads--get-database-path works with Tramp paths."
  (let ((beads-database-path nil)
        (beads--project-cache (make-hash-table :test 'equal))
        (remote-beads-dir "/ssh:host:/home/user/project/.beads"))
    (cl-letf (((symbol-function 'beads--find-beads-dir)
               (lambda (&optional _dir) remote-beads-dir))
              ((symbol-function 'directory-files)
               (lambda (directory &optional full match _nosort)
                 (when (and match (string-match-p "\\.db$" "beads.db"))
                   (if full
                       (list (concat directory "/beads.db"))
                     (list "beads.db"))))))
      (let ((result (beads--get-database-path)))
        (should result)
        (should (string-prefix-p "/ssh:host:" result))
        (should (string-suffix-p ".db" result))))))

;;; Tests for command building with Tramp paths

(ert-deftest beads-tramp-test-build-command-with-tramp-db-path ()
  "Test command building with Tramp database path strips Tramp prefix."
  (beads-tramp-test-with-temp-config
   (let ((beads-database-path "/ssh:host:/home/user/.beads/beads.db"))
     (let ((cmd (beads--build-command "list")))
       (should (member "--db" cmd))
       ;; Tramp prefix should be stripped so bd can understand the path
       (should (member "/home/user/.beads/beads.db" cmd))
       (should-not (member "/ssh:host:/home/user/.beads/beads.db" cmd))))))

;;; Tests for end-to-end Tramp workflow

(ert-deftest beads-tramp-test-full-workflow-with-tramp ()
  "Test complete workflow: find project, get DB, run command over Tramp."
  (let ((default-directory "/ssh:remote:/home/user/project/src/")
        (beads--project-cache (make-hash-table :test 'equal))
        (beads-database-path nil)
        (json-output "[{\"id\":\"bd-1\",\"title\":\"Remote Issue\"}]")
        (workflow-steps '()))
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda ()
                 (push 'find-project-root workflow-steps)
                 "/ssh:remote:/home/user/project/"))
              ((symbol-function 'locate-dominating-file)
               (lambda (file name)
                 (push 'locate-dominating-file workflow-steps)
                 (when (equal name ".beads")
                   "/ssh:remote:/home/user/project/")))
              ((symbol-function 'directory-files)
               (lambda (directory &optional full match _nosort)
                 (push 'directory-files workflow-steps)
                 (when (and match (string-match-p "\\.db$" "beads.db"))
                   (if full
                       (list (concat directory "beads.db"))
                     (list "beads.db")))))
              ((symbol-function 'process-file)
               (lambda (program &optional infile destination display
                               &rest args)
                 (push 'process-file workflow-steps)
                 (when destination
                   (with-current-buffer (current-buffer)
                     (insert json-output)))
                 0)))
      ;; Find .beads directory
      (let ((beads-dir (beads--find-beads-dir)))
        (should beads-dir)
        (should (string-prefix-p "/ssh:remote:" beads-dir)))

      ;; Get database path
      (let ((db-path (beads--get-database-path)))
        (should db-path)
        (should (string-prefix-p "/ssh:remote:" db-path)))

      ;; Run command
      (let ((result (beads--run-command "list")))
        (should (vectorp result))
        (should (= (length result) 1)))

      ;; Verify all steps were executed
      (should (member 'process-file workflow-steps))
      (should (member 'locate-dominating-file workflow-steps)))))

;;; Tests for debug logging with Tramp

(ert-deftest beads-tramp-test-debug-logging-shows-remote-directory ()
  "Test that debug logging includes remote directory information."
  (beads-tramp-test-with-temp-config
   (let ((beads-enable-debug t)
         (default-directory "/ssh:remote:/home/user/project/")
         (json-output "{}"))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (when destination
                    (with-current-buffer (if (bufferp destination)
                                             destination
                                           (current-buffer))
                      (insert json-output)))
                  0)))
       (with-current-buffer (get-buffer-create "*beads-debug*")
         (erase-buffer))
       (beads--run-command "list")
       (with-current-buffer "*beads-debug*"
         (should (> (buffer-size) 0))
         (should (string-match-p "/ssh:remote:" (buffer-string)))
         (should (string-match-p "In directory:" (buffer-string))))))))

;;; Edge cases and error handling

(ert-deftest beads-tramp-test-command-failure-over-tramp ()
  "Test that command failures over Tramp are handled correctly."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (error-msg "Error: connection refused"))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (when destination
                    (with-current-buffer (if (bufferp destination)
                                             destination
                                           (current-buffer))
                      (insert error-msg)))
                  1)))
       (should-error
        (beads--run-command "list")
        :type 'user-error)))))

(ert-deftest beads-tramp-test-json-parsing-over-tramp ()
  "Test JSON parsing works correctly over Tramp."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (json-output "{\"id\":\"bd-1\",\"title\":\"远程问题\",\"priority\":1}"))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (when destination
                    (with-current-buffer (if (bufferp destination)
                                             destination
                                           (current-buffer))
                      (insert json-output)))
                  0)))
       (let ((result (beads--run-command "show" "bd-1")))
         (should (listp result))
         (should (consp result))
         (should (equal (alist-get 'id result) "bd-1"))
         (should (string-match-p "远程" (alist-get 'title result)))
         (should (= (alist-get 'priority result) 1)))))))

(provide 'beads-tramp-test)
;;; beads-tramp-test.el ends here
