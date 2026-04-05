;;; beads-section-test.el --- Tests for beads-section.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-section.el: vui-mode base and section hooks.
;; Tests cover:
;; - Section class definitions (plain EIEIO, no magit-section)
;; - beads-section-mode activation (derives from vui-mode)
;; - beads-status-sections-hook default value
;; - Insert functions return vnodes (with mocked command execution)
;; - beads-section-issue-id-at-point via text property
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
  "Create a temp buffer with beads-section-mode active, run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (beads-section-mode)
     ,@body))

;;; Section Class Tests

(ert-deftest beads-section-test-classes-defined ()
  "Verify all section classes are defined as EIEIO classes."
  (should (class-p 'beads-issues-section))
  (should (class-p 'beads-issue-section))
  (should (class-p 'beads-blocked-section))
  (should (class-p 'beads-ready-section)))

(ert-deftest beads-section-test-classes-are-plain-eieio ()
  "Verify section classes do NOT inherit from magit-section."
  ;; magit-section may not even be loaded; just confirm no magit inheritance
  (should-not (and (class-p 'magit-section)
                   (child-of-class-p 'beads-issues-section 'magit-section)))
  (should-not (and (class-p 'magit-section)
                   (child-of-class-p 'beads-issue-section 'magit-section)))
  (should-not (and (class-p 'magit-section)
                   (child-of-class-p 'beads-blocked-section 'magit-section)))
  (should-not (and (class-p 'magit-section)
                   (child-of-class-p 'beads-ready-section 'magit-section))))

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

(ert-deftest beads-section-test-mode-derived-from-vui-mode ()
  "Verify beads-section-mode is derived from vui-mode."
  (beads-section-test--with-mode-buffer
    (should (derived-mode-p 'vui-mode))))

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

;;; Insert Function Tests (vui vnode return values)

(defun beads-section-test--mock-execute (issues)
  "Return ISSUES as a mock result for `beads-command-execute'."
  issues)

(ert-deftest beads-section-test-insert-open-issues-empty ()
  "Verify beads-insert-open-issues returns nil when no issues."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd)
               (beads-section-test--mock-execute nil))))
    (should-not (beads-insert-open-issues))))

(ert-deftest beads-section-test-insert-open-issues-with-data ()
  "Verify beads-insert-open-issues returns a non-nil vnode when issues exist."
  (let ((issues (list (beads-section-test--make-issue
                       :id "bd-001" :title "First" :priority 1)
                      (beads-section-test--make-issue
                       :id "bd-002" :title "Second" :priority 2))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-section-test--mock-execute issues))))
      (let ((vnode (beads-insert-open-issues)))
        (should vnode)
        (should-not (stringp vnode))))))

(ert-deftest beads-section-test-insert-blocked-issues-empty ()
  "Verify beads-insert-blocked-issues returns nil when no issues."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd)
               (beads-section-test--mock-execute nil))))
    (should-not (beads-insert-blocked-issues))))

(ert-deftest beads-section-test-insert-blocked-issues-with-data ()
  "Verify beads-insert-blocked-issues returns a non-nil vnode when blocked issues exist."
  (let ((issues (list (beads-section-test--make-issue
                       :id "bd-003" :status "blocked"
                       :title "Blocked task"))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-section-test--mock-execute issues))))
      (let ((vnode (beads-insert-blocked-issues)))
        (should vnode)
        (should-not (stringp vnode))))))

(ert-deftest beads-section-test-insert-blocked-filters-hooked ()
  "Verify beads-insert-blocked-issues excludes hooked issues."
  (let ((issues (list (beads-section-test--make-issue
                       :id "bd-h01" :status "hooked"
                       :title "Hooked task"))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-section-test--mock-execute issues))))
      (should-not (beads-insert-blocked-issues)))))

(ert-deftest beads-section-test-insert-blocked-filters-in-progress ()
  "Verify beads-insert-blocked-issues excludes in_progress issues."
  (let ((issues (list (beads-section-test--make-issue
                       :id "bd-ip1" :status "in_progress"
                       :title "In-progress task"))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-section-test--mock-execute issues))))
      (should-not (beads-insert-blocked-issues)))))

(ert-deftest beads-section-test-insert-blocked-mixed-statuses ()
  "Verify beads-insert-blocked-issues only shows non-active blocked issues."
  (let ((issues (list (beads-section-test--make-issue
                       :id "bd-h01" :status "hooked"
                       :title "Hooked task")
                      (beads-section-test--make-issue
                       :id "bd-ip1" :status "in_progress"
                       :title "In-progress task")
                      (beads-section-test--make-issue
                       :id "bd-003" :status "open"
                       :title "Open blocked task"))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-section-test--mock-execute issues))))
      ;; Only the open issue should appear — hooked and in_progress filtered out
      (let ((vnode (beads-insert-blocked-issues)))
        (should vnode)))))

(ert-deftest beads-section-test-insert-ready-work-empty ()
  "Verify beads-insert-ready-work returns nil when no ready work."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd)
               (beads-section-test--mock-execute nil))))
    (should-not (beads-insert-ready-work))))

(ert-deftest beads-section-test-insert-ready-work-with-data ()
  "Verify beads-insert-ready-work returns a non-nil vnode when ready work exists."
  (let ((issues (list (beads-section-test--make-issue
                       :id "bd-004" :title "Ready task"))))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd)
                 (beads-section-test--mock-execute issues))))
      (let ((vnode (beads-insert-ready-work)))
        (should vnode)
        (should-not (stringp vnode))))))

;;; Context Detection Tests (text property)

(ert-deftest beads-section-test-propertize-adds-text-property ()
  "Verify beads-section--propertize stores section as text property."
  (let* ((section (beads-issue-section :issue nil))
         (str (beads-section--propertize "hello" section)))
    (should (eq (get-text-property 0 'beads-section str) section))))

(ert-deftest beads-section-test-issue-id-at-point-nil-when-no-property ()
  "Verify beads-section-issue-id-at-point returns nil when no text property."
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-min))
    (should-not (beads-section-issue-id-at-point))))

(ert-deftest beads-section-test-issue-id-at-point-nil-wrong-class ()
  "Verify beads-section-issue-id-at-point returns nil for non-issue sections."
  (with-temp-buffer
    (let* ((section (beads-issues-section))
           (str (beads-section--propertize "group" section)))
      (insert str)
      (goto-char (point-min))
      (should-not (beads-section-issue-id-at-point)))))

(ert-deftest beads-section-test-issue-id-at-point-returns-id ()
  "Verify beads-section-issue-id-at-point returns the issue id via text property."
  (with-temp-buffer
    (let* ((issue (beads-section-test--make-issue :id "bd-42"))
           (section (beads-issue-section :issue issue))
           (str (beads-section--propertize "issue line text" section)))
      (insert str)
      (goto-char (point-min))
      (should (equal (beads-section-issue-id-at-point) "bd-42")))))

;;; Visit Command Tests

(ert-deftest beads-section-test-visit-issue-no-section ()
  "Verify beads-section-visit-issue is a no-op when not on an issue section."
  (with-temp-buffer
    (insert "plain text\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'beads-show)
               (lambda (_id) (error "Should not be called"))))
      (should-not (beads-section-visit-issue)))))

(ert-deftest beads-section-test-visit-issue-calls-beads-show ()
  "Verify beads-section-visit-issue calls beads-show with the issue id."
  (with-temp-buffer
    (let* ((issue (beads-section-test--make-issue :id "bd-visit"))
           (section (beads-issue-section :issue issue))
           (str (beads-section--propertize "issue line" section))
           visited-id)
      (insert str)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'beads-show)
                 (lambda (id) (setq visited-id id))))
        (beads-section-visit-issue))
      (should (equal visited-id "bd-visit")))))

;;; Section Build Vnode Tests

(ert-deftest beads-section-test-build-vnode-empty-hook ()
  "Verify beads-section-build-vnode works with an empty hook."
  (let ((beads-status-sections-hook nil))
    (let ((result (beads-section-build-vnode)))
      ;; Should return a vstack (not nil, not error)
      (should result))))

(ert-deftest beads-section-test-build-vnode-collects-non-nil ()
  "Verify beads-section-build-vnode calls all hook functions."
  (let* ((called nil)
         (beads-status-sections-hook
          (list (lambda () (push 'first called) nil)
                (lambda () (push 'second called) nil))))
    (beads-section-build-vnode)
    (should (memq 'first called))
    (should (memq 'second called))))

(provide 'beads-section-test)
;;; beads-section-test.el ends here
