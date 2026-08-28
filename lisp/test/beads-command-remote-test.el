;;; beads-command-remote-test.el --- TRAMP support unit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Unit tests for beads.el's remote (TRAMP) support (bde-hku):
;;
;;   1. `beads-meta-build-global-options' hands the `db' and
;;      `directory' global options to bd host-local — bd runs where
;;      the store lives, so a TRAMP-prefixed value must be stripped
;;      with `file-local-name' or remote bd tries to open the
;;      "/ssh:user@host:..." name literally.
;;
;;   2. `beads-command-resolve-executable' resolves a bare
;;      `beads-executable' to a host-local absolute path for a remote
;;      `default-directory' (cached per connection), and is the
;;      identity everywhere else.
;;
;; All remote interaction is mocked — no test opens a connection.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-command)
(require 'beads-command-ready)

(defvar beads-executable)
(defvar beads-remote-search-path)

(defconst beads-command-remote-test--prefix "/ssh:user@example.com:"
  "TRAMP connection prefix used by these tests.
`file-remote-p' parses it without opening a connection.")

;;; --db / --directory are serialized host-local

(ert-deftest beads-command-remote-test-db-stripped ()
  "A TRAMP-prefixed `db' slot serializes as the host-local path."
  (let ((cmd (beads-command-ready :json t)))
    (oset cmd db (concat beads-command-remote-test--prefix
                         "/home/user/rig/.beads/dolt"))
    (let ((line (beads-command-line cmd)))
      (should (member "--db" line))
      (should (member "/home/user/rig/.beads/dolt" line))
      (should-not (seq-find (lambda (arg) (string-prefix-p "/ssh:" arg))
                            line)))))

(ert-deftest beads-command-remote-test-directory-stripped ()
  "A TRAMP-prefixed `directory' slot serializes as the host-local path."
  (let ((cmd (beads-command-ready :json t)))
    (oset cmd directory (concat beads-command-remote-test--prefix
                                "/home/user/rig"))
    (let ((line (beads-command-line cmd)))
      (should (member "--directory" line))
      (should (member "/home/user/rig" line)))))

(ert-deftest beads-command-remote-test-local-db-identity ()
  "A local `db' value is serialized unchanged."
  (let ((cmd (beads-command-ready :json t)))
    (oset cmd db "/home/user/rig/.beads/dolt")
    (should (member "/home/user/rig/.beads/dolt"
                    (beads-command-line cmd)))))

(ert-deftest beads-command-remote-test-actor-not-stripped ()
  "Non-path global options are not run through `file-local-name'.
An actor value that merely looks like a remote name must survive."
  (let ((cmd (beads-command-ready :json t)))
    (oset cmd actor "/ssh:literal-actor:x")
    (should (member "/ssh:literal-actor:x" (beads-command-line cmd)))))

;;; Executable resolution

(ert-deftest beads-command-remote-test-resolve-local-identity ()
  "A local `default-directory' returns `beads-executable' untouched."
  (let ((beads-executable "bd")
        (default-directory temporary-file-directory)
        (beads-command--remote-executable-cache
         (make-hash-table :test 'equal)))
    (should (equal "bd" (beads-command-resolve-executable)))))

(ert-deftest beads-command-remote-test-resolve-absolute-identity ()
  "An absolute `beads-executable' is never re-resolved, even remotely."
  (let ((beads-executable "/opt/bin/bd")
        (default-directory (concat beads-command-remote-test--prefix
                                   "/home/user/"))
        (beads-command--remote-executable-cache
         (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) (error "Must not probe"))))
      (should (equal "/opt/bin/bd" (beads-command-resolve-executable))))))

(ert-deftest beads-command-remote-test-resolve-via-tramp-remote-path ()
  "A bare name found on `tramp-remote-path' is used and cached."
  (let ((beads-executable "bd")
        (default-directory (concat beads-command-remote-test--prefix
                                   "/home/user/"))
        (beads-command--remote-executable-cache
         (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name &optional _remote) "/usr/bin/bd")))
      (should (equal "/usr/bin/bd" (beads-command-resolve-executable))))
    ;; Cached: a second resolution must not probe again.
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) (error "Must not probe twice"))))
      (should (equal "/usr/bin/bd" (beads-command-resolve-executable))))))

(ert-deftest beads-command-remote-test-resolve-via-search-path ()
  "A bare name missed by `tramp-remote-path' is probed in
`beads-remote-search-path' and returned host-local."
  (let ((beads-executable "bd")
        (default-directory (concat beads-command-remote-test--prefix
                                   "/home/user/"))
        (beads-remote-search-path '("/opt/profile/bin"))
        (beads-command--remote-executable-cache
         (make-hash-table :test 'equal))
        (probed nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) nil))
              ((symbol-function 'file-executable-p)
               (lambda (name) (setq probed name)
                 (equal name (concat beads-command-remote-test--prefix
                                     "/opt/profile/bin/bd")))))
      (should (equal "/opt/profile/bin/bd"
                     (beads-command-resolve-executable)))
      (should (equal probed (concat beads-command-remote-test--prefix
                                    "/opt/profile/bin/bd"))))))

(ert-deftest beads-command-remote-test-resolve-miss-returns-bare ()
  "An unresolvable bare name falls back unchanged — and the failure
is not cached, so a later install is picked up."
  (let ((beads-executable "bd")
        (default-directory (concat beads-command-remote-test--prefix
                                   "/home/user/"))
        (beads-remote-search-path '("/opt/profile/bin"))
        (beads-command--remote-executable-cache
         (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'file-executable-p) (lambda (_) nil)))
      (should (equal "bd" (beads-command-resolve-executable))))
    (should (zerop (hash-table-count
                    beads-command--remote-executable-cache)))))

(provide 'beads-command-remote-test)
;;; beads-command-remote-test.el ends here
