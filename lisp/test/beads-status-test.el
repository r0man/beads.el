;;; beads-status-test.el --- Tests for beads-status.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-status.el: the beads status buffer with section
;; hooks.  Tests cover:
;; - beads-status-mode activation and derivation (from vui-mode)
;; - Keymap bindings (g=refresh, q=quit)
;; - revert-buffer-function set correctly
;; - beads-status--header-vnode produces a vnode
;; - beads-status-refresh calls section hooks
;; - beads-status command creates and displays a buffer

;;; Code:

(require 'ert)
(require 'beads-status)
(require 'beads-section)
(require 'beads-types)

;;; Helpers

(defun beads-status-test--make-issue (&rest args)
  "Return a beads-issue with ARGS merged into defaults."
  (apply #'beads-issue
         (append '(:id "bd-001"
                   :title "Test issue"
                   :status "open"
                   :priority 2
                   :issue-type "task")
                 args)))

(defmacro beads-status-test--with-mode-buffer (&rest body)
  "Create a temp buffer with beads-status-mode active, run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (beads-status-mode)
     (let ((inhibit-read-only t))
       ,@body)))

(defun beads-status-test--mock-execute (issues)
  "Return a mock execution result containing ISSUES."
  (let ((exec (beads-command-execution)))
    (oset exec result issues)
    (oset exec exit-code 0)
    (oset exec stdout "")
    (oset exec stderr "")
    exec))

;;; Mode Tests

(ert-deftest beads-status-test-mode-derived-from-section-mode ()
  "Verify beads-status-mode is derived from beads-section-mode."
  (beads-status-test--with-mode-buffer
    (should (derived-mode-p 'beads-section-mode))))

(ert-deftest beads-status-test-mode-derived-from-vui-mode ()
  "Verify beads-status-mode is derived from vui-mode."
  (beads-status-test--with-mode-buffer
    (should (derived-mode-p 'vui-mode))))

(ert-deftest beads-status-test-mode-activates ()
  "Verify beads-status-mode activates without error."
  (beads-status-test--with-mode-buffer
    (should (eq major-mode 'beads-status-mode))))

;;; Keymap Tests

(ert-deftest beads-status-test-keymap-g-refresh ()
  "Verify g is bound to beads-status-refresh in beads-status-mode-map."
  (should (eq (lookup-key beads-status-mode-map (kbd "g"))
              #'beads-status-refresh)))

(ert-deftest beads-status-test-keymap-q-quit ()
  "Verify q is bound to quit-window in beads-status-mode-map."
  (should (eq (lookup-key beads-status-mode-map (kbd "q"))
              #'quit-window)))

;;; Revert Buffer Function Tests

(ert-deftest beads-status-test-revert-buffer-function-set ()
  "Verify revert-buffer-function is set to beads-status--revert."
  (beads-status-test--with-mode-buffer
    (should (eq revert-buffer-function #'beads-status--revert))))

;;; Header Vnode Tests

(ert-deftest beads-status-test-header-vnode-returns-vnode ()
  "Verify beads-status--header-vnode returns a non-nil vnode."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/home/user/myproject"))
            ((symbol-function 'beads--get-database-path)
             (lambda () "/home/user/myproject/.beads/beads.db")))
    (let ((vnode (beads-status--header-vnode)))
      (should vnode)
      (should-not (stringp vnode)))))

(ert-deftest beads-status-test-header-vnode-no-project ()
  "Verify beads-status--header-vnode handles missing project gracefully."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () nil))
            ((symbol-function 'beads--get-database-path)
             (lambda () nil)))
    ;; Should not signal an error; returns a vnode
    (let ((vnode (beads-status--header-vnode)))
      (should vnode))))

(ert-deftest beads-status-test-header-vnode-dolt-mode ()
  "Verify header shows dolt dir path when db is a directory."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/home/user/myproject"))
            ((symbol-function 'beads--get-database-path)
             (lambda () "/home/user/myproject/.beads/dolt")))
    (let ((vnode (beads-status--header-vnode)))
      (should vnode)
      ;; Vnode should contain the dolt dir path
      (should (string-match-p "/home/user/myproject/.beads/dolt"
                              (format "%s" vnode))))))

;;; Refresh Tests

(ert-deftest beads-status-test-refresh-runs-section-hooks ()
  "Verify beads-status-refresh calls functions in beads-status-sections-hook."
  (let (hook-called)
    (let ((beads-status-sections-hook
           (list (lambda () (setq hook-called t) nil))))
      (beads-status-test--with-mode-buffer
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () nil))
                  ((symbol-function 'beads--get-database-path)
                   (lambda () nil))
                  ((symbol-function 'vui-mount)
                   (lambda (_component _name)
                     (beads-section-build-vnode))))
          (beads-status-refresh))))
    (should hook-called)))

(ert-deftest beads-status-test-refresh-calls-vui-mount ()
  "Verify beads-status-refresh calls vui-mount."
  (let (mount-called)
    (beads-status-test--with-mode-buffer
      (cl-letf (((symbol-function 'beads-git-find-project-root)
                 (lambda () nil))
                ((symbol-function 'beads--get-database-path)
                 (lambda () nil))
                ((symbol-function 'vui-mount)
                 (lambda (_component _name)
                   (setq mount-called t))))
        (beads-status-refresh)))
    (should mount-called)))

(ert-deftest beads-status-test-refresh-clears-buffer ()
  "Verify beads-status-refresh replaces buffer content on re-run."
  (beads-status-test--with-mode-buffer
    (let ((inhibit-read-only t))
      (insert "old content\n"))
    (cl-letf (((symbol-function 'beads-git-find-project-root)
               (lambda () "/tmp/proj"))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil))
              ((symbol-function 'vui-mount)
               (lambda (_component _name)
                 (let ((inhibit-read-only t))
                   (erase-buffer)))))
      (beads-status-refresh))
    (should-not (string-match-p "old content" (buffer-string)))))

;;; beads-status Command Tests

(ert-deftest beads-status-test-command-creates-buffer ()
  "Verify beads-status creates a buffer in beads-status-mode."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () nil))
            ((symbol-function 'beads--get-database-path)
             (lambda () nil))
            ((symbol-function 'vui-mount)
             (lambda (_component _name) nil))
            ((symbol-function 'pop-to-buffer)
             (lambda (_buf) nil)))
    (beads-status)
    (let ((buf (get-buffer beads-status--buffer-name)))
      (unwind-protect
          (progn
            (should buf)
            (with-current-buffer buf
              (should (eq major-mode 'beads-status-mode))))
        (when buf (kill-buffer buf))))))

(ert-deftest beads-status-test-command-uses-project-buffer-name ()
  "Verify beads-status uses a project-specific buffer name when in a project."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/tmp/coolproject"))
            ((symbol-function 'beads--get-database-path)
             (lambda () nil))
            ((symbol-function 'vui-mount)
             (lambda (_component _name) nil))
            ((symbol-function 'pop-to-buffer)
             (lambda (_buf) nil)))
    (beads-status)
    (let ((buf (get-buffer "*beads-status<coolproject>*")))
      (unwind-protect
          (should buf)
        (when buf (kill-buffer buf))))))

(provide 'beads-status-test)
;;; beads-status-test.el ends here
