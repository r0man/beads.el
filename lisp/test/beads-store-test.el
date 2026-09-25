;;; beads-store-test.el --- Tests for explicit store scoping -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Explicit store scoping (dashboard-v3 §12 B1): `beads-show', the list
;; entry points and `beads-dashboard' take :directory, which becomes the
;; buffer's store (`beads-store-directory').  Every bd command run while
;; such a buffer is current gets --directory (host-local), refreshes
;; included, and buffers opened from it inherit the store.  bd is
;; mocked at `beads-command-execute'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-command-show)
(require 'beads-command-list)
(require 'beads-spec)

(defconst beads-store-test--issue
  '((id . "bd-1") (title . "One") (status . "open") (priority . 2)
    (issue_type . "task") (created_at . "2025-01-01T00:00:00Z")
    (updated_at . "2025-01-01T00:00:00Z"))
  "Minimal issue JSON.")

(defmacro beads-store-test--capturing (var &rest body)
  "Run BODY with bd mocked; push each command line built onto VAR."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'beads-command-execute)
              (lambda (cmd)
                (push (beads-command-line cmd) ,var)
                (if (object-of-class-p cmd 'beads-command-show)
                    (beads-issue-from-json beads-store-test--issue)
                  (list (beads-issue-from-json beads-store-test--issue)))))
             ((symbol-function 'beads-git-get-branch) (lambda () "main"))
             ((symbol-function 'beads-check-executable) #'ignore)
             ((symbol-function 'beads-buffer-display-detail) #'ignore)
             ((symbol-function 'beads-list--display-buffer) #'ignore)
             ((symbol-function 'beads-show--register-with-session) #'ignore)
             ((symbol-function 'beads--project-root) (lambda () nil)))
     ,@body))

(defun beads-store-test--directory-arg (line)
  "Return the value following --directory in command LINE, or nil."
  (cadr (member "--directory" line)))

(defun beads-store-test--kill (buffers)
  "Kill the live buffers among BUFFERS."
  (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b))) buffers))

(ert-deftest beads-store-test-resolve ()
  "Host-local store names are taken on the remote cwd's host."
  :tags '(:unit)
  (should-not (beads-store-resolve nil))
  (should-not (beads-store-resolve ""))
  (let ((default-directory "/tmp/"))
    (should (equal (beads-store-resolve "/srv/rig") "/srv/rig/")))
  (let ((default-directory "/ssh:u@h:/home/u/"))
    (should (equal (beads-store-resolve "/srv/rig") "/ssh:u@h:/srv/rig/"))
    (should (equal (beads-store-resolve "/ssh:o@x:/srv/rig")
                   "/ssh:o@x:/srv/rig/"))))

(ert-deftest beads-store-test-global-option-fallback ()
  "An unscoped command picks up the current buffer's store, host-local."
  :tags '(:unit)
  (with-temp-buffer
    (setq-local beads-store-directory "/ssh:u@h:/srv/rig/")
    (should (equal (beads-store-test--directory-arg
                    (beads-command-line (beads-command-ready :json t)))
                   "/srv/rig/"))
    ;; An explicit slot wins.
    (should (equal (beads-store-test--directory-arg
                    (beads-command-line
                     (beads-command-ready :json t :directory "/other")))
                   "/other")))
  (with-temp-buffer
    (should-not (member "--directory"
                        (beads-command-line (beads-command-ready :json t))))))

(ert-deftest beads-store-test-show-scopes-buffer-and-refresh ()
  "`beads-show' :directory scopes the fetch, the buffer and its refresh."
  :tags '(:unit)
  (let (lines buf)
    (unwind-protect
        (beads-store-test--capturing lines
          (let ((default-directory "/tmp/"))
            (beads-show "bd-1" :directory "/tmp/store-a"))
          (setq buf (seq-find (lambda (b)
                                (equal (buffer-local-value
                                        'beads-store-directory b)
                                       "/tmp/store-a/"))
                              (buffer-list)))
          (should buf)
          (should (equal (beads-store-test--directory-arg (car lines))
                         "/tmp/store-a"))
          (with-current-buffer buf
            (should (equal default-directory "/tmp/store-a/"))
            (should (derived-mode-p 'beads-show-mode))
            (setq lines nil)
            (beads-show-update-buffer "bd-1" buf)
            (should (equal (beads-store-test--directory-arg (car lines))
                           "/tmp/store-a"))))
      (beads-store-test--kill (list buf)))))

(ert-deftest beads-store-test-show-inherits-store ()
  "A `beads-show' from a store-scoped buffer inherits its store."
  :tags '(:unit)
  (let (lines)
    (beads-store-test--capturing lines
      (with-temp-buffer
        (setq-local beads-store-directory "/tmp/store-b/")
        (beads-show "bd-1"))
      (should (equal (beads-store-test--directory-arg (car lines))
                     "/tmp/store-b")))
    (beads-store-test--kill
     (seq-filter (lambda (b) (equal (buffer-local-value
                                     'beads-store-directory b)
                                    "/tmp/store-b/"))
                 (buffer-list)))))

(ert-deftest beads-store-test-project-root ()
  "A remote store is its own root; a local one resolves from itself."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads--project-root)
             (lambda () (if (file-remote-p default-directory)
                            (error "No walk over TRAMP")
                          (concat default-directory "root/")))))
    (should (equal (beads-store-project-root "/ssh:u@h:/srv/rig/")
                   "/ssh:u@h:/srv/rig/"))
    (should (equal (beads-store-project-root "/tmp/x/") "/tmp/x/root/"))))

(ert-deftest beads-store-test-list-entry-points-scope ()
  "`beads-ready', `beads-blocked' and `beads-list-issues' take :directory."
  :tags '(:unit)
  (dolist (entry (list (lambda () (beads-ready :directory "/tmp/store-c"))
                       (lambda () (beads-blocked :directory "/tmp/store-c"))
                       (lambda () (beads-list-issues :directory "/tmp/store-c"))))
    (let (lines bufs)
      (unwind-protect
          (beads-store-test--capturing lines
            (let ((default-directory "/tmp/"))
              (funcall entry))
            (should lines)
            (dolist (line lines)
              (should (equal (beads-store-test--directory-arg line)
                             "/tmp/store-c/")))
            (setq bufs (seq-filter
                        (lambda (b)
                          (with-current-buffer b
                            (and (derived-mode-p 'beads-list-mode)
                                 (equal beads-store-directory
                                        "/tmp/store-c/"))))
                        (buffer-list)))
            (should (= (length bufs) 1))
            (with-current-buffer (car bufs)
              (should (equal default-directory "/tmp/store-c/"))
              (setq lines nil)
              (beads-list-refresh t)
              (should (equal (beads-store-test--directory-arg (car lines))
                             "/tmp/store-c/"))))
        (beads-store-test--kill bufs)))))

(provide 'beads-store-test)
;;; beads-store-test.el ends here
