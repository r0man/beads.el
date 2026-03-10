;;; beads-status-test.el --- Tests for beads-status.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-status.el: the beads status buffer with section
;; hooks.  Tests cover:
;; - beads-status-mode activation and derivation
;; - Keymap bindings (g=refresh, q=quit)
;; - revert-buffer-function set correctly
;; - beads-status--format-header produces expected output
;; - beads-status-refresh populates buffer via section hooks
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

(ert-deftest beads-status-test-mode-derived-from-magit-section-mode ()
  "Verify beads-status-mode is derived from magit-section-mode."
  (beads-status-test--with-mode-buffer
    (should (derived-mode-p 'magit-section-mode))))

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

;;; Header Tests

(ert-deftest beads-status-test-format-header-returns-string ()
  "Verify beads-status--format-header returns a string."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/home/user/myproject"))
            ((symbol-function 'beads--get-database-path)
             (lambda () "/home/user/myproject/.beads/beads.db")))
    (let ((header (beads-status--format-header)))
      (should (stringp header)))))

(ert-deftest beads-status-test-format-header-contains-project-name ()
  "Verify beads-status--format-header includes the project name."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/home/user/myproject"))
            ((symbol-function 'beads--get-database-path)
             (lambda () "/home/user/myproject/.beads/beads.db")))
    (let ((header (beads-status--format-header)))
      (should (string-match-p "myproject" header)))))

(ert-deftest beads-status-test-format-header-contains-db-path ()
  "Verify beads-status--format-header includes the database path."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/home/user/myproject"))
            ((symbol-function 'beads--get-database-path)
             (lambda () "/home/user/myproject/.beads/beads.db")))
    (let ((header (beads-status--format-header)))
      (should (string-match-p "\\.beads" header)))))

(ert-deftest beads-status-test-format-header-no-project ()
  "Verify beads-status--format-header handles missing project gracefully."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () nil))
            ((symbol-function 'beads--get-database-path)
             (lambda () nil)))
    ;; Should not signal an error
    (let ((header (beads-status--format-header)))
      (should (stringp header)))))

;;; Refresh Tests

(ert-deftest beads-status-test-refresh-runs-section-hooks ()
  "Verify beads-status-refresh calls functions in beads-status-sections-hook."
  (let (hook-called)
    (beads-status-test--with-mode-buffer
      (cl-letf (((symbol-function 'beads-git-find-project-root)
                 (lambda () "/tmp/testproject"))
                ((symbol-function 'beads--get-database-path)
                 (lambda () nil)))
        (let ((beads-status-sections-hook
               (list (lambda () (setq hook-called t)))))
          (beads-status-refresh))))
    (should hook-called)))

(ert-deftest beads-status-test-refresh-inserts-header ()
  "Verify beads-status-refresh inserts header text into buffer."
  (beads-status-test--with-mode-buffer
    (cl-letf (((symbol-function 'beads-git-find-project-root)
               (lambda () "/tmp/myproj"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/tmp/myproj/.beads/beads.db"))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-status-test--mock-execute nil))))
      (beads-status-refresh))
    (should (string-match-p "Beads" (buffer-string)))))

(ert-deftest beads-status-test-refresh-clears-buffer ()
  "Verify beads-status-refresh replaces buffer content on re-run."
  (beads-status-test--with-mode-buffer
    (let ((inhibit-read-only t))
      (insert "old content\n"))
    (cl-letf (((symbol-function 'beads-git-find-project-root)
               (lambda () "/tmp/proj"))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-status-test--mock-execute nil))))
      (beads-status-refresh))
    (should-not (string-match-p "old content" (buffer-string)))))

;;; beads-status Command Tests

(ert-deftest beads-status-test-command-creates-buffer ()
  "Verify beads-status creates a buffer in beads-status-mode."
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () nil))
            ((symbol-function 'beads--get-database-path)
             (lambda () nil))
            ((symbol-function 'beads-command-execute)
             (lambda (_cmd)
               (beads-status-test--mock-execute nil)))
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
            ((symbol-function 'beads-command-execute)
             (lambda (_cmd)
               (beads-status-test--mock-execute nil)))
            ((symbol-function 'pop-to-buffer)
             (lambda (_buf) nil)))
    (beads-status)
    (let ((buf (get-buffer "*beads-status<coolproject>*")))
      (unwind-protect
          (should buf)
        (when buf (kill-buffer buf))))))

(provide 'beads-status-test)
;;; beads-status-test.el ends here
