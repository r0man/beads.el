;;; beads-remote-open-test.el --- Opening a remote store without TRAMP -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; A remote `beads-dashboard' (no :directory) used to block ~1.6 s on
;; first open: a TRAMP marker walk (with `abbreviate-file-name') plus a
;; database-path directory scan.  On an ssh-transport host both are
;; gone: the root is found by one shell command over the ssh pipe,
;; remembered per directory, and the board is scoped to it with
;; --directory instead of a database path.  Synchronous bd commands run
;; over the pipe too, so no executable probe runs.  The ssh process is
;; faked by running its remote command string with a local sh.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-remote)
(require 'beads-util)
(require 'beads-command)
(require 'beads-command-ready)
(require 'beads-dashboard)

(defmacro beads-remote-open-test--with-local-ssh (&rest body)
  "Run BODY with ssh pipe commands executed by a local sh.
`beads-remote-ssh-command' builds the real remote command string; the
fake runs it with sh -c instead of handing it to ssh."
  (declare (indent 0))
  `(let ((beads-remote-open-test--real (symbol-function 'beads-remote-ssh-command)))
     (cl-letf (((symbol-function 'beads-remote-ssh-command)
                (lambda (&rest args)
                  (list "sh" "-c" (car (last (apply beads-remote-open-test--real
                                                    args)))))))
       ,@body)))

(defmacro beads-remote-open-test--no-tramp (&rest body)
  "Run BODY with every file operation a remote walk could make signalling."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'file-exists-p)
              (lambda (f) (if (file-remote-p f) (error "TRAMP I/O: %s" f)
                            (file-attributes f))))
             ((symbol-function 'locate-dominating-file)
              (lambda (&rest _) (error "No TRAMP walk")))
             ((symbol-function 'project-current)
              (lambda (&rest _) (error "No VC walk")))
             ((symbol-function 'executable-find)
              (lambda (&rest _) (error "No executable probe"))))
     ,@body))

(defun beads-remote-open-test--tree ()
  "Return a temp dir holding proj/.beads and proj/sub/dir."
  (let ((dir (make-temp-file "beads open " t)))
    (make-directory (expand-file-name "proj/.beads" dir) t)
    (make-directory (expand-file-name "proj/sub/dir" dir) t)
    dir))

(ert-deftest beads-remote-open-test-find-up-over-ssh ()
  "The marker walk runs as one host command; spaces and absence handled."
  :tags '(:unit)
  (let ((dir (beads-remote-open-test--tree)))
    (unwind-protect
        (beads-remote-open-test--with-local-ssh
          (should (equal (beads-remote-ssh-find-up
                          (concat "/ssh:h:" dir "/proj/sub/dir/") '(".git" ".beads"))
                         (concat "/ssh:h:" dir "/proj/")))
          (should-not (beads-remote-ssh-find-up
                       (concat "/ssh:h:" dir "/") '("no-such-marker"))))
      (delete-directory dir t))))

(ert-deftest beads-remote-open-test-project-root-remembered ()
  "A remote project root is found without TRAMP and remembered, the
negative answer too, until `beads-forget-project-roots'.  The host walk
is stubbed: what lies above a real temp directory depends on the
machine (on a CI runner a marker sits in the home directory)."
  :tags '(:unit)
  (let ((beads-remote-transport 'ssh)
        (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-remote-ssh-find-up)
                   (lambda (dir _markers)
                     (push dir calls)
                     (and (string-prefix-p "/ssh:h:/srv/proj/" dir)
                          "/ssh:h:/srv/proj/"))))
          (beads-forget-project-roots)
          (beads-remote-open-test--no-tramp
            (let ((default-directory "/ssh:h:/srv/proj/sub/"))
              (should (equal (beads--project-root) "/ssh:h:/srv/proj/"))
              (should (equal (beads--project-root) "/ssh:h:/srv/proj/")))
            (let ((default-directory "/ssh:h:/srv/"))
              (should-not (beads--project-root))
              (should-not (beads--project-root)))
            (should (= (length calls) 2))
            (beads-forget-project-roots)
            (let ((default-directory "/ssh:h:/srv/"))
              (beads--project-root))
            (should (= (length calls) 3))))
      (beads-forget-project-roots))))

(ert-deftest beads-remote-open-test-sync-command-over-ssh ()
  "A synchronous bd command on an ssh store runs over the pipe: cd to the
store, bd found on the host PATH, stdout parsed, no executable probe."
  :tags '(:unit)
  (let* ((dir (beads-remote-open-test--tree))
         (bin (expand-file-name "bin" dir))
         (beads-remote-transport 'ssh)
         (beads-executable "bd")
         (beads-remote-search-path (list bin)))
    (unwind-protect
        (progn
          (make-directory bin)
          (with-temp-file (expand-file-name "bd" bin)
            (insert "#!/bin/sh\nprintf '[{\"id\":\"%s-1\",\"title\":\"t\","
                    "\"status\":\"open\",\"priority\":1,\"issue_type\":\"task\"}]' "
                    "\"$(basename \"$PWD\")\"\necho warn >&2\n"))
          (set-file-modes (expand-file-name "bd" bin) #o755)
          (beads-remote-open-test--with-local-ssh
            (beads-remote-open-test--no-tramp
              (let* ((default-directory (concat "/ssh:h:" dir "/proj/"))
                     (issues (beads-command-execute (beads-command-ready :json t))))
                (should (equal (oref (car issues) id) "proj-1"))
                (should (beads-check-executable))))))
      (delete-directory dir t))))

(ert-deftest beads-remote-open-test-sync-command-timeout ()
  "A bd command that does not finish is killed at the deadline."
  :tags '(:unit)
  (let ((beads-remote-transport 'ssh)
        (beads-remote-sync-timeout 0.3))
    (cl-letf (((symbol-function 'beads-remote-ssh-command)
               (lambda (&rest _) (list "sleep" "5"))))
      (let ((default-directory "/ssh:h:/srv/"))
        (should-error (beads-command-execute (beads-command-ready :json t))
                      :type 'beads-command-error)))))

(ert-deftest beads-remote-open-test-dashboard-scopes-to-root ()
  "A remote board without :directory is scoped to its root (--directory)
and never scans for the database path."
  :tags '(:unit)
  (let ((beads-remote-transport 'ssh)
        (beads-command--policy t))
    (cl-letf (((symbol-function 'beads--project-root)
               (lambda () "/ssh:h:/srv/proj/"))
              ((symbol-function 'beads--get-database-path)
               (lambda () (error "No database scan on a remote store")))
              ((symbol-function 'vui-mount) #'ignore)
              ((symbol-function 'pop-to-buffer) #'ignore))
      (let ((default-directory "/ssh:h:/srv/proj/sub/")
            buf)
        (unwind-protect
            (progn
              (beads-dashboard)
              (setq buf (seq-find (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'beads-dashboard-mode)))
                                  (buffer-list)))
              (should buf)
              (with-current-buffer buf
                (should (equal beads-store-directory "/ssh:h:/srv/proj/"))
                (should (equal beads-dashboard--root "/ssh:h:/srv/proj/"))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(provide 'beads-remote-open-test)
;;; beads-remote-open-test.el ends here
