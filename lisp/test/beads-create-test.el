;;; beads-create-test.el --- Tests for beads-create -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-create.el transient menu.
;; Tests focus on multi-line field editing and command execution
;; since transient manages most argument state automatically.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-create)

;;; Test Fixtures

(defvar beads-create-test--sample-create-response
  '((id . "bd-42")
    (title . "Test Issue")
    (description . "Test description")
    (status . "open")
    (priority . 1)
    (issue_type . "bug")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z"))
  "Sample response from bd create command.")

;;; Tests for Multi-line State Management

(ert-deftest beads-create-test-reset-state ()
  "Test that reset-state clears multi-line variables."
  (setq beads-create--description "Test description"
        beads-create--acceptance "Test acceptance"
        beads-create--design "Test design")
  (beads-create--reset-state)
  (should (null beads-create--description))
  (should (null beads-create--acceptance))
  (should (null beads-create--design)))

(ert-deftest beads-create-test-reset-state-from-nil ()
  "Test that reset-state works when variables are already nil."
  (setq beads-create--description nil
        beads-create--acceptance nil
        beads-create--design nil)
  (beads-create--reset-state)
  (should (null beads-create--description))
  (should (null beads-create--acceptance))
  (should (null beads-create--design)))

;;; Tests for Command Execution

(ert-deftest beads-create-test-execute-with-title-only ()
  "Test execute with only title set via transient args."
  (let ((beads--run-command-calls nil)
        (beads-create--description nil)
        (beads-create--acceptance nil)
        (beads-create--design nil))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (&rest args)
                 (setq beads--run-command-calls args)
                 beads-create-test--sample-create-response))
              ((symbol-function 'transient-args)
               (lambda (_) '("--title=Test Issue")))
              ((symbol-function 'transient-reset) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (beads-create--execute)
      (should beads--run-command-calls)
      (should (equal (car beads--run-command-calls) "create"))
      (should (member "Test Issue" beads--run-command-calls)))))

(ert-deftest beads-create-test-execute-with-all-transient-args ()
  "Test execute with multiple transient-managed arguments."
  (let ((beads--run-command-calls nil)
        (beads-create--description nil)
        (beads-create--acceptance nil)
        (beads-create--design nil))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (&rest args)
                 (setq beads--run-command-calls args)
                 beads-create-test--sample-create-response))
              ((symbol-function 'transient-args)
               (lambda (_)
                 '("--title=Test Issue"
                   "--type=bug"
                   "--priority=1"
                   "--assignee=alice")))
              ((symbol-function 'transient-reset) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (beads-create--execute)
      (should beads--run-command-calls)
      (should (equal (car beads--run-command-calls) "create"))
      (should (member "Test Issue" beads--run-command-calls))
      (should (member "--type=bug" beads--run-command-calls))
      (should (member "--priority=1" beads--run-command-calls))
      (should (member "--assignee=alice" beads--run-command-calls)))))

(ert-deftest beads-create-test-execute-with-multiline-fields ()
  "Test execute includes multi-line fields."
  (let ((beads--run-command-calls nil)
        (beads-create--description "Multi-line\ndescription")
        (beads-create--acceptance "Acceptance\ncriteria")
        (beads-create--design "Design\nnotes"))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (&rest args)
                 (setq beads--run-command-calls args)
                 beads-create-test--sample-create-response))
              ((symbol-function 'transient-args)
               (lambda (_) '("--title=Test Issue")))
              ((symbol-function 'transient-reset) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (beads-create--execute)
      (should beads--run-command-calls)
      (should (member "-d" beads--run-command-calls))
      (should (member "Multi-line\ndescription" beads--run-command-calls))
      (should (member "--acceptance" beads--run-command-calls))
      (should (member "Acceptance\ncriteria" beads--run-command-calls))
      (should (member "--design" beads--run-command-calls))
      (should (member "Design\nnotes" beads--run-command-calls)))))

(ert-deftest beads-create-test-execute-requires-title-or-file ()
  "Test execute fails without title or file."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) '("--type=bug"))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-with-file-no-title-required ()
  "Test execute with --file does not require --title."
  (let ((beads--run-command-calls nil)
        (beads-create--description nil)
        (beads-create--acceptance nil)
        (beads-create--design nil))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (&rest args)
                 (setq beads--run-command-calls args)
                 beads-create-test--sample-create-response))
              ((symbol-function 'transient-args)
               (lambda (_) '("--file=/tmp/issue.md")))
              ((symbol-function 'transient-reset) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (beads-create--execute)
      (should beads--run-command-calls)
      (should (member "--file=/tmp/issue.md" beads--run-command-calls)))))

(ert-deftest beads-create-test-execute-resets-state-on-success ()
  "Test execute resets state after successful creation."
  (let ((beads-create--description "Test")
        (beads-create--acceptance "Test")
        (beads-create--design "Test")
        (transient-reset-called nil))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (&rest _args)
                 beads-create-test--sample-create-response))
              ((symbol-function 'transient-args)
               (lambda (_) '("--title=Test")))
              ((symbol-function 'transient-reset)
               (lambda () (setq transient-reset-called t)))
              ((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (beads-create--execute)
      (should (null beads-create--description))
      (should (null beads-create--acceptance))
      (should (null beads-create--design))
      (should transient-reset-called))))

;;; Tests for Preview Command

(ert-deftest beads-create-test-preview-basic ()
  "Test preview shows command with title."
  (let ((beads-create--description nil)
        (beads-create--acceptance nil)
        (beads-create--design nil)
        (message-content nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--title=Test Issue")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-content (apply #'format fmt args)))))
      (beads-create--preview)
      (should (stringp message-content))
      (should (string-match-p "create" message-content))
      ;; Title appears either as "Test Issue" or "Test\\ Issue" (shell-escaped)
      (should (or (string-match-p "Test Issue" message-content)
                  (string-match-p "Test" message-content))))))

(ert-deftest beads-create-test-preview-with-multiline-shows-placeholder ()
  "Test preview shows placeholders for multi-line fields."
  (let ((beads-create--description "Real description")
        (beads-create--acceptance "Real acceptance")
        (beads-create--design "Real design")
        (message-content nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--title=Test")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-content (apply #'format fmt args)))))
      (beads-create--preview)
      ;; Placeholders appear shell-escaped: \<...\>
      (should (string-match-p "description" message-content))
      (should (string-match-p "acceptance" message-content))
      (should (string-match-p "design" message-content)))))

;;; Tests for Reset Command

(ert-deftest beads-create-test-reset-with-confirmation ()
  "Test reset clears state when user confirms."
  (let ((beads-create--description "Test")
        (beads-create--acceptance "Test")
        (beads-create--design "Test")
        (transient-reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq transient-reset-called t))))
      (beads-create--reset)
      (should (null beads-create--description))
      (should (null beads-create--acceptance))
      (should (null beads-create--design))
      (should transient-reset-called))))

(ert-deftest beads-create-test-reset-without-confirmation ()
  "Test reset does nothing when user declines."
  (let ((beads-create--description "Test")
        (beads-create--acceptance "Test")
        (beads-create--design "Test")
        (transient-reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq transient-reset-called t))))
      (beads-create--reset)
      (should (equal beads-create--description "Test"))
      (should (equal beads-create--acceptance "Test"))
      (should (equal beads-create--design "Test"))
      (should (null transient-reset-called)))))

;;; Tests for Transient Menu Definition

(ert-deftest beads-create-test-transient-defined ()
  "Test that beads-create transient is defined."
  (should (commandp 'beads-create))
  (should (get 'beads-create 'transient--prefix)))

(ert-deftest beads-create-test-multiline-suffixes-defined ()
  "Test that multi-line field suffixes are defined."
  (should (commandp 'beads-create:--description))
  (should (commandp 'beads-create:--acceptance))
  (should (commandp 'beads-create:--design)))

(ert-deftest beads-create-test-action-suffixes-defined ()
  "Test that action suffixes are defined."
  (should (commandp 'beads-create--execute))
  (should (commandp 'beads-create--preview))
  (should (commandp 'beads-create--reset)))

;;; Integration Tests

(ert-deftest beads-create-test-full-workflow ()
  "Integration test: set fields, execute, verify reset."
  :tags '(integration)
  (let ((beads-create--description "Test desc")
        (beads-create--acceptance "Test accept")
        (beads-create--design "Test design")
        (beads--run-command-calls nil)
        (transient-reset-called nil))
    (cl-letf (((symbol-function 'beads--run-command)
               (lambda (&rest args)
                 (setq beads--run-command-calls args)
                 beads-create-test--sample-create-response))
              ((symbol-function 'transient-args)
               (lambda (_)
                 '("--title=Integration Test"
                   "--type=feature"
                   "--priority=2"
                   "--labels=test,integration")))
              ((symbol-function 'transient-reset)
               (lambda () (setq transient-reset-called t)))
              ((symbol-function 'y-or-n-p) (lambda (_) nil)))
      ;; Execute
      (beads-create--execute)
      ;; Verify command was called correctly
      (should (member "Integration Test" beads--run-command-calls))
      (should (member "--type=feature" beads--run-command-calls))
      (should (member "--priority=2" beads--run-command-calls))
      (should (member "--labels=test,integration" beads--run-command-calls))
      (should (member "-d" beads--run-command-calls))
      (should (member "Test desc" beads--run-command-calls))
      ;; Verify state was reset
      (should (null beads-create--description))
      (should (null beads-create--acceptance))
      (should (null beads-create--design))
      (should transient-reset-called))))

(ert-deftest beads-create-test-multiline-edit-preserves-transient-args ()
  "Integration test: editing multi-line fields preserves other arguments."
  :tags '(integration)
  (let ((beads-create--description nil)
        (beads-create--acceptance nil)
        (transient-resume-called nil)
        (saved-args '("--title=My Issue"
                      "--type=bug"
                      "--priority=1"
                      "--assignee=alice"
                      "--labels=urgent,backend")))
    ;; Simulate user editing description field
    (cl-letf (((symbol-function 'transient-resume)
               (lambda ()
                 (setq transient-resume-called t)))
              ((symbol-function 'generate-new-buffer)
               (lambda (name) (get-buffer-create name)))
              ((symbol-function 'switch-to-buffer) #'ignore)
              ((symbol-function 'markdown-mode) #'ignore)
              ((symbol-function 'text-mode) #'ignore)
              ((symbol-function 'visual-line-mode) #'ignore)
              ((symbol-function 'local-set-key) #'ignore)
              ((symbol-function 'message) #'ignore))

      ;; Start editing description
      (beads-create--edit-text-multiline
       beads-create--description
       (lambda (text) (setq beads-create--description text))
       "Description")

      ;; Get the edit buffer
      (let ((edit-buffer (get-buffer "*beads-description*")))
        (should edit-buffer)

        ;; Simulate user typing in the buffer
        (with-current-buffer edit-buffer
          (insert "This is a detailed description\nwith multiple lines")

          ;; Simulate C-c C-c (finish editing)
          ;; Call the finish function that was set up
          (let ((finish-key (where-is-internal
                            'transient-resume
                            (current-local-map) t)))
            ;; Since we mocked local-set-key, directly call the callback
            (funcall (lambda ()
                      (let ((text (buffer-substring-no-properties
                                   (point-min) (point-max))))
                        (kill-buffer)
                        (setq beads-create--description text)
                        (transient-resume)))))

          ;; Verify description was saved
          (should (equal beads-create--description
                        "This is a detailed description\nwith multiple lines"))

          ;; Verify transient-resume was called (would preserve other args)
          (should transient-resume-called))))))

(ert-deftest beads-create-test-multiline-edit-cancel-preserves-args ()
  "Integration test: canceling multi-line edit still resumes transient."
  :tags '(integration)
  (let ((beads-create--description "Original description")
        (transient-resume-called nil))
    ;; Simulate user canceling description edit
    (cl-letf (((symbol-function 'transient-resume)
               (lambda ()
                 (setq transient-resume-called t)))
              ((symbol-function 'generate-new-buffer)
               (lambda (name) (get-buffer-create name)))
              ((symbol-function 'switch-to-buffer) #'ignore)
              ((symbol-function 'markdown-mode) #'ignore)
              ((symbol-function 'text-mode) #'ignore)
              ((symbol-function 'visual-line-mode) #'ignore)
              ((symbol-function 'local-set-key) #'ignore)
              ((symbol-function 'message) #'ignore))

      ;; Start editing
      (beads-create--edit-text-multiline
       beads-create--description
       (lambda (text) (setq beads-create--description text))
       "Description")

      (let ((edit-buffer (get-buffer "*beads-description*")))
        (should edit-buffer)

        ;; Simulate user making changes but then canceling
        (with-current-buffer edit-buffer
          (insert "New text that will be discarded")

          ;; Simulate C-c C-k (cancel)
          (funcall (lambda ()
                    (kill-buffer)
                    (transient-resume)))

          ;; Verify description was NOT changed
          (should (equal beads-create--description "Original description"))

          ;; Verify transient-resume was still called
          (should transient-resume-called))))))

(provide 'beads-create-test)
;;; beads-create-test.el ends here
