;;; beads-section-test.el --- Tests for beads-section.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-section.el: magit-section-mode base and section
;; hooks.  Tests cover:
;; - Section class definitions
;; - beads-section-mode activation
;; - beads-status-sections-hook default value
;; - Insert functions (with mocked command execution)
;; - beads--insert-issue-line rendering
;; - beads-section-visit-issue command

;;; Code:

(require 'ert)
(require 'eieio)
(require 'beads-section)
(require 'beads-types)

;;; Helpers

(defun beads-section-test--make-issue (&rest args)
  "Return a beads-issue with ARGS merged into defaults."
  (apply #'beads-issue
         (append '(:id "bd-001"
                   :title "Test issue"
                   :status "open"
                   :priority 2
                   :issue-type "task")
                 args)))

(defmacro beads-section-test--with-mode-buffer (&rest body)
  "Create a temp buffer with beads-section-mode active, run BODY.
Insertions are allowed via `inhibit-read-only'."
  (declare (indent 0))
  `(with-temp-buffer
     (beads-section-mode)
     (let ((inhibit-read-only t))
       ,@body)))

;;; Section Class Tests

(ert-deftest beads-section-test-classes-defined ()
  "Verify all section classes are defined as EIEIO classes."
  (should (class-p 'beads-issues-section))
  (should (class-p 'beads-issue-section))
  (should (class-p 'beads-blocked-section))
  (should (class-p 'beads-ready-section)))

(ert-deftest beads-section-test-classes-derive-from-magit-section ()
  "Verify all section classes derive from magit-section."
  (should (child-of-class-p 'beads-issues-section 'magit-section))
  (should (child-of-class-p 'beads-issue-section 'magit-section))
  (should (child-of-class-p 'beads-blocked-section 'magit-section))
  (should (child-of-class-p 'beads-ready-section 'magit-section)))

(ert-deftest beads-section-test-issue-section-has-issue-slot ()
  "Verify beads-issue-section has an :issue slot."
  (let ((section (beads-issue-section :issue nil)))
    (should (slot-exists-p section 'issue))))

(ert-deftest beads-section-test-issue-section-issue-slot-set ()
  "Verify beads-issue-section :issue slot stores the issue."
  (let* ((issue (beads-section-test--make-issue))
         (section (beads-issue-section :issue issue)))
    (should (eq (oref section issue) issue))))

;;; Mode Tests

(ert-deftest beads-section-test-mode-derived-from-magit-section-mode ()
  "Verify beads-section-mode is derived from magit-section-mode."
  (beads-section-test--with-mode-buffer
    (should (derived-mode-p 'magit-section-mode))))

(ert-deftest beads-section-test-mode-activates ()
  "Verify beads-section-mode activates without error."
  (beads-section-test--with-mode-buffer
    (should (eq major-mode 'beads-section-mode))))

;;; Hook Tests

(ert-deftest beads-section-test-hook-is-defcustom ()
  "Verify beads-status-sections-hook is a custom variable."
  (should (custom-variable-p 'beads-status-sections-hook)))

(ert-deftest beads-section-test-hook-default-functions ()
  "Verify the hook contains the three default insert functions."
  (should (memq 'beads-insert-open-issues beads-status-sections-hook))
  (should (memq 'beads-insert-blocked-issues beads-status-sections-hook))
  (should (memq 'beads-insert-ready-work beads-status-sections-hook)))

;;; Insert Function Tests

(defun beads-section-test--mock-execute (issues)
  "Return a mock execution result containing ISSUES."
  (let ((exec (beads-command-execution)))
    (oset exec result issues)
    (oset exec exit-code 0)
    (oset exec stdout "")
    (oset exec stderr "")
    exec))

(ert-deftest beads-section-test-insert-open-issues-empty ()
  "Verify beads-insert-open-issues inserts nothing when no issues."
  (beads-section-test--with-mode-buffer
    (magit-insert-section (magit-section)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-section-test--mock-execute nil))))
        (beads-insert-open-issues)))
    (should (string= (buffer-string) ""))))

(ert-deftest beads-section-test-insert-open-issues-with-data ()
  "Verify beads-insert-open-issues inserts a section with issues."
  (beads-section-test--with-mode-buffer
    (let ((issues (list (beads-section-test--make-issue
                         :id "bd-001" :title "First" :priority 1)
                        (beads-section-test--make-issue
                         :id "bd-002" :title "Second" :priority 2))))
      (magit-insert-section (magit-section)
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (_cmd)
                     (beads-section-test--mock-execute issues))))
          (beads-insert-open-issues))))
    (let ((content (buffer-string)))
      (should (string-match-p "Open Issues" content))
      (should (string-match-p "bd-001" content))
      (should (string-match-p "First" content)))))

(ert-deftest beads-section-test-insert-blocked-issues-empty ()
  "Verify beads-insert-blocked-issues inserts nothing when no issues."
  (beads-section-test--with-mode-buffer
    (magit-insert-section (magit-section)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-section-test--mock-execute nil))))
        (beads-insert-blocked-issues)))
    (should (string= (buffer-string) ""))))

(ert-deftest beads-section-test-insert-blocked-issues-with-data ()
  "Verify beads-insert-blocked-issues inserts a section when blocked issues exist."
  (beads-section-test--with-mode-buffer
    (let ((issues (list (beads-section-test--make-issue
                         :id "bd-003" :status "blocked"
                         :title "Blocked task"))))
      (magit-insert-section (magit-section)
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (_cmd)
                     (beads-section-test--mock-execute issues))))
          (beads-insert-blocked-issues))))
    (let ((content (buffer-string)))
      (should (string-match-p "Blocked Issues" content))
      (should (string-match-p "bd-003" content)))))

(ert-deftest beads-section-test-insert-ready-work-empty ()
  "Verify beads-insert-ready-work inserts nothing when no ready work."
  (beads-section-test--with-mode-buffer
    (magit-insert-section (magit-section)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-section-test--mock-execute nil))))
        (beads-insert-ready-work)))
    (should (string= (buffer-string) ""))))

(ert-deftest beads-section-test-insert-ready-work-with-data ()
  "Verify beads-insert-ready-work inserts a section when ready work exists."
  (beads-section-test--with-mode-buffer
    (let ((issues (list (beads-section-test--make-issue
                         :id "bd-004" :title "Ready task"))))
      (magit-insert-section (magit-section)
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (_cmd)
                     (beads-section-test--mock-execute issues))))
          (beads-insert-ready-work))))
    (let ((content (buffer-string)))
      (should (string-match-p "Ready Work" content))
      (should (string-match-p "bd-004" content)))))

;;; beads--insert-issue-line Tests

(ert-deftest beads-section-test-insert-issue-line-content ()
  "Verify beads--insert-issue-line renders id, priority, type, status, title."
  (beads-section-test--with-mode-buffer
    (let ((issue (beads-section-test--make-issue
                  :id "bd-999" :title "My title"
                  :priority 1 :issue-type "bug" :status "open")))
      (magit-insert-section (magit-section)
        (beads--insert-issue-line issue)))
    (let ((content (buffer-string)))
      (should (string-match-p "bd-999" content))
      (should (string-match-p "My title" content))
      (should (string-match-p "P1" content))
      (should (string-match-p "bug" content))
      (should (string-match-p "open" content)))))

(ert-deftest beads-section-test-insert-issue-line-nil-priority ()
  "Verify beads--insert-issue-line handles nil priority gracefully."
  (beads-section-test--with-mode-buffer
    (let ((issue (beads-section-test--make-issue :priority nil)))
      (magit-insert-section (magit-section)
        (beads--insert-issue-line issue)))
    ;; Should not signal an error; "--" rendered for nil priority
    (should (string-match-p "--" (buffer-string)))))

(ert-deftest beads-section-test-insert-issue-line-creates-section ()
  "Verify beads--insert-issue-line creates a beads-issue-section."
  (beads-section-test--with-mode-buffer
    (let ((issue (beads-section-test--make-issue)))
      (magit-insert-section (magit-section)
        (beads--insert-issue-line issue))
      ;; The root section contains the beads-issue-section as a child.
      ;; magit-root-section is the outer (magit-section) created above.
      (let* ((root magit-root-section)
             (child (car (oref root children))))
        (should child)
        (should (object-of-class-p child 'beads-issue-section))
        (should (eq (oref child issue) issue))))))

;;; Issue Priority Sort Tests

(ert-deftest beads-section-test-open-issues-sorted-by-priority ()
  "Verify beads-insert-open-issues renders issues sorted by priority ascending."
  (beads-section-test--with-mode-buffer
    (let ((issues (list (beads-section-test--make-issue
                         :id "bd-low" :title "Low prio" :priority 3)
                        (beads-section-test--make-issue
                         :id "bd-high" :title "High prio" :priority 0))))
      (magit-insert-section (magit-section)
        (cl-letf (((symbol-function 'beads-command-execute)
                   (lambda (_cmd)
                     (beads-section-test--mock-execute issues))))
          (beads-insert-open-issues))))
    (let ((content (buffer-string)))
      ;; bd-high (P0) should appear before bd-low (P3)
      (should (< (string-match "bd-high" content)
                 (string-match "bd-low" content))))))

;;; Visit Command Tests

(ert-deftest beads-section-test-visit-issue-no-section ()
  "Verify beads-section-visit-issue is a no-op when not on an issue section."
  (beads-section-test--with-mode-buffer
    (magit-insert-section (magit-section)
      (insert "plain text\n"))
    (goto-char (point-min))
    ;; Should not error even without beads-issue-section at point
    (cl-letf (((symbol-function 'beads-show)
               (lambda (_id) (error "Should not be called"))))
      (should-not (beads-section-visit-issue)))))

(ert-deftest beads-section-test-visit-issue-calls-beads-show ()
  "Verify beads-section-visit-issue calls beads-show with the issue id."
  (beads-section-test--with-mode-buffer
    (let* ((issue (beads-section-test--make-issue :id "bd-visit"))
           visited-id)
      (magit-insert-section (magit-section)
        (beads--insert-issue-line issue))
      ;; The issue line starts at the beginning of the buffer content.
      ;; Position point somewhere within the issue line text.
      (goto-char (point-min))
      ;; Point is now inside the beads-issue-section text region.
      (cl-letf (((symbol-function 'beads-show)
                 (lambda (id) (setq visited-id id))))
        (beads-section-visit-issue))
      (should (equal visited-id "bd-visit")))))

(provide 'beads-section-test)
;;; beads-section-test.el ends here
