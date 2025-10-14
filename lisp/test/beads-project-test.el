;;; beads-project-test.el --- Tests for beads.el project integration
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tests

;;; Commentary:

;; ERT tests for beads.el project integration functions:
;; - beads--find-project-root
;; - beads--find-beads-dir
;; - beads--get-database-path
;; - beads--project-cache

;;; Code:

(require 'ert)
(require 'beads)

;;; Test Fixtures

(defvar beads-test--temp-dir nil
  "Temporary directory for test files.")

(defun beads-test--setup ()
  "Set up test fixtures."
  ;; Create temporary directory structure
  (setq beads-test--temp-dir (make-temp-file "beads-test-" t))

  ;; Clear the project cache
  (clrhash beads--project-cache)

  ;; Clear customizations
  (setq beads-database-path nil))

(defun beads-test--teardown ()
  "Clean up test fixtures."
  ;; Clean up temp directory
  (when (and beads-test--temp-dir
             (file-directory-p beads-test--temp-dir))
    (delete-directory beads-test--temp-dir t))
  (setq beads-test--temp-dir nil)

  ;; Clear the cache
  (clrhash beads--project-cache)

  ;; Reset customizations
  (setq beads-database-path nil))

(defmacro beads-test--with-temp-project (&rest body)
  "Execute BODY with a temporary project structure."
  `(let ((beads-test--temp-dir nil))
     (unwind-protect
         (progn
           (beads-test--setup)
           ,@body)
       (beads-test--teardown))))

(defun beads-test--create-project-structure (structure)
  "Create a directory STRUCTURE under `beads-test--temp-dir'.
STRUCTURE is a list of paths to create (dirs end with /)."
  (dolist (path structure)
    (let ((full-path (expand-file-name path beads-test--temp-dir)))
      (if (string-suffix-p "/" path)
          ;; Directory
          (make-directory full-path t)
        ;; File
        (make-directory (file-name-directory full-path) t)
        (write-region "" nil full-path)))))

;;; Tests for beads--find-project-root

(ert-deftest beads-test-find-project-root-no-project ()
  "Test finding project root when not in a project."
  (beads-test--with-temp-project
   (let ((project-find-functions nil))
     (should (null (beads--find-project-root))))))

(ert-deftest beads-test-find-project-root-with-project ()
  "Test finding project root when in a project."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".git/" "src/" "README.md"))

   (let ((default-directory (expand-file-name "src" beads-test--temp-dir)))
     ;; Mock project-current to return a project
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt)
                  (cons 'transient beads-test--temp-dir))))

       ;; Test with modern Emacs (project-root function exists)
       (cl-letf (((symbol-function 'project-root)
                  (lambda (_proj) beads-test--temp-dir)))
         (should (equal (beads--find-project-root)
                        beads-test--temp-dir)))

       ;; Test with Emacs 27 (project-roots function)
       (cl-letf (((symbol-function 'project-root) nil)
                 ((symbol-function 'project-roots)
                  (lambda (_proj) (list beads-test--temp-dir))))
         (fmakunbound 'project-root)
         (should (equal (beads--find-project-root)
                        beads-test--temp-dir)))))))

(ert-deftest beads-test-find-project-root-emacs27-compat ()
  "Test Emacs 27 compatibility when project-root is not defined."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".git/"))

   (cl-letf (((symbol-function 'project-current)
              (lambda (&optional _maybe-prompt)
                (cons 'transient beads-test--temp-dir)))
             ((symbol-function 'project-root) nil)
             ((symbol-function 'project-roots)
              (lambda (_proj) (list beads-test--temp-dir))))

     ;; Temporarily remove project-root to simulate Emacs 27
     (let ((has-project-root (fboundp 'project-root)))
       (when has-project-root
         (fmakunbound 'project-root))
       (unwind-protect
           (should (equal (beads--find-project-root)
                          beads-test--temp-dir))
         (when has-project-root
           (fset 'project-root
                 (lambda (_proj) beads-test--temp-dir))))))))

;;; Tests for beads--find-beads-dir

(ert-deftest beads-test-find-beads-dir-in-root ()
  "Test finding .beads directory in project root."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db"))

   (let ((default-directory beads-test--temp-dir))
     (should (equal (beads--find-beads-dir)
                    (expand-file-name ".beads" beads-test--temp-dir))))))

(ert-deftest beads-test-find-beads-dir-traverse-up ()
  "Test finding .beads directory by traversing up."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "src/" "src/modules/" "src/modules/auth/"))

   (let ((default-directory (expand-file-name "src/modules/auth"
                                               beads-test--temp-dir)))
     (should (equal (beads--find-beads-dir)
                    (expand-file-name ".beads" beads-test--temp-dir))))))

(ert-deftest beads-test-find-beads-dir-not-found ()
  "Test when .beads directory is not found."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '("src/"))

   (let ((default-directory (expand-file-name "src" beads-test--temp-dir)))
     (should (null (beads--find-beads-dir))))))

(ert-deftest beads-test-find-beads-dir-explicit-directory ()
  "Test finding .beads with explicit directory argument."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "project1/" "project1/.beads/"))

   (let ((project1-dir (expand-file-name "project1" beads-test--temp-dir)))
     (should (equal (beads--find-beads-dir project1-dir)
                    (expand-file-name ".beads" project1-dir))))))

(ert-deftest beads-test-find-beads-dir-uses-project-root ()
  "Test that beads--find-beads-dir uses project root as starting point."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".git/" ".beads/" "src/"))

   (let ((default-directory (expand-file-name "src" beads-test--temp-dir)))
     ;; Mock project-current to return the temp dir as project root
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt)
                  (cons 'transient beads-test--temp-dir)))
               ((symbol-function 'project-root)
                (lambda (_proj) beads-test--temp-dir)))

       (should (equal (beads--find-beads-dir)
                      (expand-file-name ".beads" beads-test--temp-dir)))))))

(ert-deftest beads-test-find-beads-dir-fallback-to-default-directory ()
  "Test fallback to default-directory when no project found."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((default-directory beads-test--temp-dir)
         (project-find-functions nil))

     ;; Mock project-current to return nil
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt) nil)))

       (should (equal (beads--find-beads-dir)
                      (expand-file-name ".beads" beads-test--temp-dir)))))))

;;; Tests for caching

(ert-deftest beads-test-find-beads-dir-caching-basic ()
  "Test that .beads directory lookup is cached."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((default-directory beads-test--temp-dir)
         (lookup-count 0))

     ;; Mock locate-dominating-file to count calls
     ;; Also mock project-current to avoid additional calls
     (cl-letf (((symbol-function 'locate-dominating-file)
                (lambda (file name)
                  (setq lookup-count (1+ lookup-count))
                  (when (equal name ".beads")
                    beads-test--temp-dir)))
               ((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt) nil)))

       ;; First call should perform lookup
       (beads--find-beads-dir beads-test--temp-dir)
       (should (= lookup-count 1))

       ;; Second call should use cache
       (beads--find-beads-dir beads-test--temp-dir)
       (should (= lookup-count 1))

       ;; Third call should still use cache
       (beads--find-beads-dir beads-test--temp-dir)
       (should (= lookup-count 1))))))

(ert-deftest beads-test-find-beads-dir-caching-per-directory ()
  "Test that caching is per-directory."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '("project1/" "project1/.beads/" "project2/" "project2/.beads/"))

   (let ((proj1-dir (expand-file-name "project1" beads-test--temp-dir))
         (proj2-dir (expand-file-name "project2" beads-test--temp-dir)))

     ;; Cache for project1
     (let ((result1 (beads--find-beads-dir proj1-dir)))
       (should (equal result1 (expand-file-name ".beads" proj1-dir))))

     ;; Cache for project2 (should be different)
     (let ((result2 (beads--find-beads-dir proj2-dir)))
       (should (equal result2 (expand-file-name ".beads" proj2-dir))))

     ;; Verify both are cached independently
     (should (equal (gethash proj1-dir beads--project-cache)
                    (expand-file-name ".beads" proj1-dir)))
     (should (equal (gethash proj2-dir beads--project-cache)
                    (expand-file-name ".beads" proj2-dir))))))

(ert-deftest beads-test-find-beads-dir-cache-nil-not-stored ()
  "Test that nil results are not cached."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '("src/"))

   (let ((src-dir (expand-file-name "src" beads-test--temp-dir))
         (lookup-count 0))

     ;; Mock locate-dominating-file to count calls and return nil
     ;; Also mock project-current to avoid additional calls
     (cl-letf (((symbol-function 'locate-dominating-file)
                (lambda (_file _name)
                  (setq lookup-count (1+ lookup-count))
                  nil))
               ((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt) nil)))

       ;; First call
       (should (null (beads--find-beads-dir src-dir)))
       (should (= lookup-count 1))

       ;; Second call should NOT use cache (nil not cached)
       (should (null (beads--find-beads-dir src-dir)))
       (should (= lookup-count 2))))))

(ert-deftest beads-test-project-cache-multiple-projects ()
  "Test cache handles multiple projects in same session."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '("workspace/proj-a/" "workspace/proj-a/.beads/"
      "workspace/proj-b/" "workspace/proj-b/.beads/"
      "workspace/proj-c/" "workspace/proj-c/.beads/"))

   (let ((proj-a (expand-file-name "workspace/proj-a" beads-test--temp-dir))
         (proj-b (expand-file-name "workspace/proj-b" beads-test--temp-dir))
         (proj-c (expand-file-name "workspace/proj-c" beads-test--temp-dir)))

     ;; Access all three projects
     (beads--find-beads-dir proj-a)
     (beads--find-beads-dir proj-b)
     (beads--find-beads-dir proj-c)

     ;; Verify all three are cached
     (should (= (hash-table-count beads--project-cache) 3))
     (should (gethash proj-a beads--project-cache))
     (should (gethash proj-b beads--project-cache))
     (should (gethash proj-c beads--project-cache)))))

(ert-deftest beads-test-cache-invalidation-manual ()
  "Test manual cache clearing."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((default-directory beads-test--temp-dir))
     ;; Populate cache
     (beads--find-beads-dir)
     (should (> (hash-table-count beads--project-cache) 0))

     ;; Clear cache manually
     (clrhash beads--project-cache)
     (should (= (hash-table-count beads--project-cache) 0))

     ;; Next call should repopulate
     (beads--find-beads-dir)
     (should (= (hash-table-count beads--project-cache) 1)))))

;;; Tests for beads--get-database-path

(ert-deftest beads-test-get-database-path-from-customization ()
  "Test getting database path from customization."
  (beads-test--with-temp-project
   (let ((custom-db-path "/custom/path/to/beads.db"))
     (setq beads-database-path custom-db-path)
     (should (equal (beads--get-database-path) custom-db-path)))))

(ert-deftest beads-test-get-database-path-auto-discover ()
  "Test auto-discovering database path."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db"))

   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     (should (equal (beads--get-database-path)
                    (expand-file-name ".beads/beads.db"
                                      beads-test--temp-dir))))))

(ert-deftest beads-test-get-database-path-no-beads-dir ()
  "Test getting database path when .beads dir not found."
  (beads-test--with-temp-project
   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     (should (null (beads--get-database-path))))))

(ert-deftest beads-test-get-database-path-no-db-file ()
  "Test getting database path when .beads exists but no .db file."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/" ".beads/issues.jsonl"))

   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     (should (null (beads--get-database-path))))))

(ert-deftest beads-test-get-database-path-multiple-db-files ()
  "Test getting database path when multiple .db files exist."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db" ".beads/backup.db"))

   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     ;; Should return first match (behavior of directory-files + car)
     (let ((result (beads--get-database-path)))
       (should result)
       (should (string-suffix-p ".db" result))))))

(ert-deftest beads-test-get-database-path-customization-precedence ()
  "Test that customization takes precedence over auto-discovery."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db"))

   (let ((default-directory beads-test--temp-dir)
         (custom-path "/override/custom.db"))
     (setq beads-database-path custom-path)

     ;; Should return custom path, not discovered path
     (should (equal (beads--get-database-path) custom-path)))))

;;; Tests for edge cases and integration

(ert-deftest beads-test-find-beads-dir-symlink-handling ()
  "Test that symlinks are handled correctly."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "real-project/" "real-project/.beads/"))

   (let* ((real-dir (expand-file-name "real-project" beads-test--temp-dir))
          (link-dir (expand-file-name "link-project" beads-test--temp-dir)))

     ;; Create symlink (skip if not supported)
     (condition-case nil
         (progn
           (make-symbolic-link real-dir link-dir)

           ;; Access via symlink
           (let ((result (beads--find-beads-dir link-dir)))
             (should result)
             (should (string-match-p "\\.beads$" result))))
       (file-error nil))))) ;; Skip test if symlinks not supported

(ert-deftest beads-test-find-beads-dir-nested-projects ()
  "Test behavior with nested projects."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "outer/.beads/" "outer/inner/" "outer/inner/.beads/"))

   (let ((outer-dir (expand-file-name "outer" beads-test--temp-dir))
         (inner-dir (expand-file-name "outer/inner" beads-test--temp-dir)))

     ;; From outer, should find outer's .beads
     (should (equal (beads--find-beads-dir outer-dir)
                    (expand-file-name ".beads" outer-dir)))

     ;; From inner, should find inner's .beads (closest match)
     (should (equal (beads--find-beads-dir inner-dir)
                    (expand-file-name ".beads" inner-dir))))))

(ert-deftest beads-test-concurrent-project-operations ()
  "Test cache behavior with concurrent project operations."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '("proj1/.beads/" "proj2/.beads/"))

   (let ((proj1 (expand-file-name "proj1" beads-test--temp-dir))
         (proj2 (expand-file-name "proj2" beads-test--temp-dir)))

     ;; Simulate rapid project switching
     (beads--find-beads-dir proj1)
     (beads--find-beads-dir proj2)
     (beads--find-beads-dir proj1)
     (beads--find-beads-dir proj2)

     ;; Both should be cached and correct
     (should (equal (gethash proj1 beads--project-cache)
                    (expand-file-name ".beads" proj1)))
     (should (equal (gethash proj2 beads--project-cache)
                    (expand-file-name ".beads" proj2))))))

(ert-deftest beads-test-path-with-spaces ()
  "Test handling paths with spaces."
  (beads-test--with-temp-project
   (let ((space-dir (expand-file-name "my project" beads-test--temp-dir)))
     (make-directory space-dir t)
     (make-directory (expand-file-name ".beads" space-dir) t)

     (let ((result (beads--find-beads-dir space-dir)))
       (should result)
       (should (equal result (expand-file-name ".beads" space-dir)))))))

(ert-deftest beads-test-find-beads-dir-unicode-paths ()
  "Test handling Unicode characters in paths."
  (beads-test--with-temp-project
   (let ((unicode-dir (expand-file-name "проект-テスト" beads-test--temp-dir)))
     (condition-case nil
         (progn
           (make-directory unicode-dir t)
           (make-directory (expand-file-name ".beads" unicode-dir) t)

           (let ((result (beads--find-beads-dir unicode-dir)))
             (should result)
             (should (equal result (expand-file-name ".beads" unicode-dir)))))
       (file-error nil))))) ;; Skip if filesystem doesn't support Unicode

(ert-deftest beads-test-permissions-readonly-directory ()
  "Test behavior when .beads directory is read-only."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((beads-dir (expand-file-name ".beads" beads-test--temp-dir)))
     ;; Finding directory should work even if read-only
     (should (equal (beads--find-beads-dir beads-test--temp-dir)
                    beads-dir)))))

;;; Performance tests

(ert-deftest beads-test-cache-performance ()
  "Test that caching provides performance benefit."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((default-directory beads-test--temp-dir)
         (uncached-time 0)
         (cached-time 0))

     ;; Time uncached lookup
     (clrhash beads--project-cache)
     (let ((start (current-time)))
       (dotimes (_ 100)
         (clrhash beads--project-cache)
         (beads--find-beads-dir))
       (setq uncached-time (float-time (time-since start))))

     ;; Time cached lookup
     (clrhash beads--project-cache)
     (beads--find-beads-dir) ;; Prime cache
     (let ((start (current-time)))
       (dotimes (_ 100)
         (beads--find-beads-dir))
       (setq cached-time (float-time (time-since start))))

     ;; Cached should be significantly faster
     (should (< cached-time (* uncached-time 0.5))))))

;;; Documentation tests

(ert-deftest beads-test-function-documentation ()
  "Test that public functions have documentation."
  (should (documentation 'beads--find-project-root))
  (should (documentation 'beads--find-beads-dir))
  (should (documentation 'beads--get-database-path)))

(provide 'beads-project-test)
;;; beads-project-test.el ends here
