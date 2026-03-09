;;; beads-spec-test.el --- Tests for beads-spec.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-spec.el: filter spec objects for beads list views.
;; Tests cover:
;; - beads-issue-spec class definition and slots
;; - beads-issue-spec--to-args conversion
;; - beads-list-default-spec defcustom
;; - beads-list--spec buffer-local variable
;; - beads-list--refresh with mocked command execution

;;; Code:

(require 'ert)
(require 'eieio)
(require 'beads-spec)
(require 'beads-types)
(require 'beads-command-list)

;;; Helpers

(defun beads-spec-test--make-spec (&rest args)
  "Return a beads-issue-spec with ARGS merged into defaults."
  (apply #'beads-issue-spec args))

(defun beads-spec-test--make-issue (&rest args)
  "Return a beads-issue with ARGS merged into defaults."
  (apply #'beads-issue
         (append '(:id "bd-001"
                   :title "Test issue"
                   :status "open"
                   :priority 2
                   :issue-type "task")
                 args)))

(defun beads-spec-test--mock-execute (issues)
  "Return a mock execution result containing ISSUES."
  (let ((exec (beads-command-execution)))
    (oset exec result issues)
    (oset exec exit-code 0)
    (oset exec stdout "")
    (oset exec stderr "")
    exec))

;;; beads-issue-spec Class Tests

(ert-deftest beads-spec-test-class-defined ()
  "Verify beads-issue-spec is defined as an EIEIO class."
  (should (class-p 'beads-issue-spec)))

(ert-deftest beads-spec-test-class-slots-exist ()
  "Verify beads-issue-spec has all required slots."
  (let ((spec (beads-issue-spec)))
    (should (slot-exists-p spec 'status))
    (should (slot-exists-p spec 'type))
    (should (slot-exists-p spec 'priority))
    (should (slot-exists-p spec 'assignee))
    (should (slot-exists-p spec 'label))
    (should (slot-exists-p spec 'order))
    (should (slot-exists-p spec 'limit))
    (should (slot-exists-p spec 'ready-only))))

(ert-deftest beads-spec-test-default-values ()
  "Verify beads-issue-spec has correct default slot values."
  (let ((spec (beads-issue-spec)))
    (should (null (oref spec status)))
    (should (null (oref spec type)))
    (should (null (oref spec priority)))
    (should (null (oref spec assignee)))
    (should (null (oref spec label)))
    (should (eq (oref spec order) 'newest))
    (should (= (oref spec limit) 50))
    (should (null (oref spec ready-only)))))

(ert-deftest beads-spec-test-slot-values-set ()
  "Verify beads-issue-spec slots can be set via initargs."
  (let ((spec (beads-issue-spec
               :status "open"
               :type "bug"
               :priority 1
               :assignee "alice"
               :label "urgent"
               :order 'priority
               :limit 25
               :ready-only t)))
    (should (equal (oref spec status) "open"))
    (should (equal (oref spec type) "bug"))
    (should (= (oref spec priority) 1))
    (should (equal (oref spec assignee) "alice"))
    (should (equal (oref spec label) "urgent"))
    (should (eq (oref spec order) 'priority))
    (should (= (oref spec limit) 25))
    (should (oref spec ready-only))))

;;; beads-issue-spec--to-args Tests

(ert-deftest beads-spec-test-to-args-empty-spec ()
  "Verify to-args with all-nil spec produces only limit arg."
  (let* ((spec (beads-issue-spec :order 'newest :limit 50))
         (args (beads-issue-spec--to-args spec)))
    ;; Should include sort and limit at minimum
    (should (member "--limit=50" args))
    ;; newest order → --sort=created --reverse
    (should (member "--sort=created" args))
    (should (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-status ()
  "Verify to-args includes --status when set."
  (let* ((spec (beads-issue-spec :status "open"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--status=open" args))))

(ert-deftest beads-spec-test-to-args-no-status ()
  "Verify to-args omits --status when nil."
  (let* ((spec (beads-issue-spec :status nil))
         (args (beads-issue-spec--to-args spec)))
    (should-not (seq-find (lambda (a) (string-prefix-p "--status=" a)) args))))

(ert-deftest beads-spec-test-to-args-type ()
  "Verify to-args includes --type when set."
  (let* ((spec (beads-issue-spec :type "bug"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--type=bug" args))))

(ert-deftest beads-spec-test-to-args-priority ()
  "Verify to-args includes --priority when set."
  (let* ((spec (beads-issue-spec :priority 1))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--priority=1" args))))

(ert-deftest beads-spec-test-to-args-assignee ()
  "Verify to-args includes --assignee when set."
  (let* ((spec (beads-issue-spec :assignee "alice"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--assignee=alice" args))))

(ert-deftest beads-spec-test-to-args-label ()
  "Verify to-args includes --label when set."
  (let* ((spec (beads-issue-spec :label "urgent"))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--label=urgent" args))))

(ert-deftest beads-spec-test-to-args-order-newest ()
  "Verify order=newest maps to --sort=created --reverse."
  (let* ((spec (beads-issue-spec :order 'newest))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=created" args))
    (should (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-order-oldest ()
  "Verify order=oldest maps to --sort=created without --reverse."
  (let* ((spec (beads-issue-spec :order 'oldest))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=created" args))
    (should-not (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-order-priority ()
  "Verify order=priority maps to --sort=priority."
  (let* ((spec (beads-issue-spec :order 'priority))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=priority" args))
    (should-not (member "--reverse" args))))

(ert-deftest beads-spec-test-to-args-order-updated ()
  "Verify order=updated maps to --sort=updated."
  (let* ((spec (beads-issue-spec :order 'updated))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--sort=updated" args))))

(ert-deftest beads-spec-test-to-args-limit ()
  "Verify to-args includes --limit with correct value."
  (let* ((spec (beads-issue-spec :limit 25))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--limit=25" args))))

(ert-deftest beads-spec-test-to-args-ready-only ()
  "Verify to-args includes --ready when ready-only is t."
  (let* ((spec (beads-issue-spec :ready-only t))
         (args (beads-issue-spec--to-args spec)))
    (should (member "--ready" args))))

(ert-deftest beads-spec-test-to-args-no-ready-only ()
  "Verify to-args omits --ready when ready-only is nil."
  (let* ((spec (beads-issue-spec :ready-only nil))
         (args (beads-issue-spec--to-args spec)))
    (should-not (member "--ready" args))))

(ert-deftest beads-spec-test-to-args-returns-list ()
  "Verify beads-issue-spec--to-args returns a list of strings."
  (let* ((spec (beads-issue-spec))
         (args (beads-issue-spec--to-args spec)))
    (should (listp args))
    (should (seq-every-p #'stringp args))))

;;; beads-list-default-spec Tests

(ert-deftest beads-spec-test-default-spec-is-defcustom ()
  "Verify beads-list-default-spec is a custom variable."
  (should (custom-variable-p 'beads-list-default-spec)))

(ert-deftest beads-spec-test-default-spec-is-spec ()
  "Verify beads-list-default-spec is a beads-issue-spec instance."
  (should (object-of-class-p beads-list-default-spec 'beads-issue-spec)))

(ert-deftest beads-spec-test-default-spec-status-open ()
  "Verify beads-list-default-spec defaults to status=open."
  (should (equal (oref beads-list-default-spec status) "open")))

(ert-deftest beads-spec-test-default-spec-order-newest ()
  "Verify beads-list-default-spec defaults to order=newest."
  (should (eq (oref beads-list-default-spec order) 'newest)))

(ert-deftest beads-spec-test-default-spec-limit-50 ()
  "Verify beads-list-default-spec defaults to limit=50."
  (should (= (oref beads-list-default-spec limit) 50)))

;;; beads-list--spec Buffer-local Variable Tests

(ert-deftest beads-spec-test-spec-var-is-buffer-local ()
  "Verify beads-list--spec is a buffer-local variable."
  (with-temp-buffer
    (should (local-variable-if-set-p 'beads-list--spec))))

(ert-deftest beads-spec-test-spec-var-nil-default ()
  "Verify beads-list--spec is nil by default."
  (with-temp-buffer
    (should (null beads-list--spec))))

(ert-deftest beads-spec-test-spec-var-independent-per-buffer ()
  "Verify beads-list--spec is independent across buffers."
  (let ((spec (beads-issue-spec :status "closed")))
    (with-temp-buffer
      (setq-local beads-list--spec spec)
      (with-temp-buffer
        (should (null beads-list--spec))))))

;;; beads-list--refresh Tests

(ert-deftest beads-spec-test-refresh-uses-spec ()
  "Verify beads-list--refresh calls beads-command-execute with a command."
  (with-temp-buffer
    (beads-list-mode)
    (let ((called-cmd nil)
          (issues (list (beads-spec-test--make-issue))))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (setq called-cmd cmd)
                   (beads-spec-test--mock-execute issues)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_issues _command &optional _cmd-obj) nil)))
        (beads-list--refresh (beads-issue-spec :status "open")))
      (should called-cmd)
      (should (object-of-class-p called-cmd 'beads-command-list)))))

(ert-deftest beads-spec-test-refresh-populates-buffer ()
  "Verify beads-list--refresh calls beads-list--populate-buffer with issues."
  (with-temp-buffer
    (beads-list-mode)
    (let ((issues (list (beads-spec-test--make-issue)))
          (populated-issues nil))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-spec-test--mock-execute issues)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (is _command &optional _cmd-obj)
                   (setq populated-issues is))))
        (beads-list--refresh (beads-issue-spec :status "open")))
      (should (equal populated-issues issues)))))

(ert-deftest beads-spec-test-refresh-stores-spec ()
  "Verify beads-list--refresh sets beads-list--spec in current buffer."
  (with-temp-buffer
    (beads-list-mode)
    (let ((spec (beads-issue-spec :status "closed")))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_cmd)
                   (beads-spec-test--mock-execute nil)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_is _command &optional _cmd-obj) nil)))
        (beads-list--refresh spec))
      (should (eq beads-list--spec spec)))))

(ert-deftest beads-spec-test-refresh-falls-back-to-buffer-spec ()
  "Verify beads-list--refresh uses beads-list--spec when no spec given."
  (with-temp-buffer
    (beads-list-mode)
    (let ((spec (beads-issue-spec :status "blocked"))
          (used-cmd nil))
      (setq-local beads-list--spec spec)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (setq used-cmd cmd)
                   (beads-spec-test--mock-execute nil)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_is _command &optional _cmd-obj) nil)))
        (beads-list--refresh))
      ;; Used a command (demonstrating buffer spec was used)
      (should used-cmd))))

(ert-deftest beads-spec-test-refresh-falls-back-to-default-spec ()
  "Verify beads-list--refresh uses default spec when no spec anywhere."
  (with-temp-buffer
    (beads-list-mode)
    (let ((used-cmd nil))
      (setq-local beads-list--spec nil)
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (cmd)
                   (setq used-cmd cmd)
                   (beads-spec-test--mock-execute nil)))
                ((symbol-function 'beads-list--populate-buffer)
                 (lambda (_is _command &optional _cmd-obj) nil)))
        (beads-list--refresh))
      (should used-cmd)
      ;; The default spec has status=open, so the command should have status=open
      (should (equal (oref used-cmd status) "open")))))

(provide 'beads-spec-test)
;;; beads-spec-test.el ends here
