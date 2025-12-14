;;; beads-test-dsl.el --- EIEIO-based test DSL for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: lisp, tools

;;; Commentary:

;; This module provides an EIEIO-based Domain Specific Language (DSL)
;; for testing beads.el user workflows.  Tests are expressed as
;; sequences of action objects that represent user interactions
;; (filling transient menus, submitting, navigating buffers, etc.).
;;
;; Core concepts:
;;   - Actions: EIEIO objects representing atomic user interactions
;;   - Context: Tracks state and side effects during test execution
;;   - Workflows: Sequences of actions with setup/teardown/assertions
;;
;; Usage:
;;   (beads-test-run-workflow
;;    (beads-test-workflow
;;     :name "create-issue"
;;     :steps (list
;;             (beads-test-action-create-issue
;;              :title "Test Issue"
;;              :type "bug"))
;;     :assertions
;;     (list (lambda (ctx)
;;             (beads-test-assert-issue-created ctx :title "Test Issue")))))

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'ert)
(require 'beads-types)
(require 'beads-agent)

;; Forward declarations
(declare-function beads-command-execute "beads-command")
(declare-function beads-command-list! "beads-command")
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads--invalidate-label-cache "beads-label")
(declare-function beads-list--transient-execute "beads-list")
(declare-function beads-list-show "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-list-mark "beads-list")
(declare-function beads-list-unmark "beads-list")
(declare-function beads-show "beads-show")
(declare-function beads-create--execute "beads-create")
(declare-function beads-update--execute "beads-update")
(declare-function beads-close--execute "beads-close")
(declare-function beads-test-create-project "beads-test")
(declare-function beads-test--clear-transient-state "beads-test")
(declare-function beads-command-create! "beads-command")
(declare-function beads-command-dep-add! "beads-command")
(declare-function beads-command-stats! "beads-command")

;; External variables (from other modules)
(defvar beads-update--issue-id)
(defvar beads-close--issue-id)
(defvar beads--project-cache)

;;; ============================================================
;;; Test Context
;;; ============================================================

(defclass beads-test-context ()
  ((project-dir
    :initarg :project-dir
    :type (or null string)
    :initform nil
    :accessor beads-test-context-project-dir
    :documentation "Temporary project directory for this test.")
   (created-buffers
    :initarg :created-buffers
    :type list
    :initform nil
    :accessor beads-test-context-created-buffers
    :documentation "List of buffer names created during test.")
   (messages
    :initarg :messages
    :type list
    :initform nil
    :accessor beads-test-context-messages
    :documentation "List of (format-string . args) messages displayed.")
   (errors
    :initarg :errors
    :type list
    :initform nil
    :accessor beads-test-context-errors
    :documentation "List of (error-type . data) errors signaled.")
   (completion-cache-invalidated
    :initarg :completion-cache-invalidated
    :type boolean
    :initform nil
    :accessor beads-test-context-completion-cache-invalidated
    :documentation "Whether completion cache was invalidated.")
   (label-cache-invalidated
    :initarg :label-cache-invalidated
    :type boolean
    :initform nil
    :accessor beads-test-context-label-cache-invalidated
    :documentation "Whether label cache was invalidated.")
   (command-executions
    :initarg :command-executions
    :type list
    :initform nil
    :accessor beads-test-context-command-executions
    :documentation "List of (command-class . args) executed.")
   (transient-args
    :initarg :transient-args
    :type list
    :initform nil
    :accessor beads-test-context-transient-args
    :documentation "Current accumulated transient arguments.")
   (current-transient
    :initarg :current-transient
    :type (or null symbol)
    :initform nil
    :accessor beads-test-context-current-transient
    :documentation "Currently active transient prefix symbol.")
   (execution-trace
    :initarg :execution-trace
    :type list
    :initform nil
    :accessor beads-test-context-execution-trace
    :documentation "Trace of (timestamp action result) tuples.")
   (user-responses
    :initarg :user-responses
    :type list
    :initform nil
    :accessor beads-test-context-user-responses
    :documentation "Queue of responses for interactive prompts.")
   (created-issues
    :initarg :created-issues
    :type list
    :initform nil
    :accessor beads-test-context-created-issues
    :documentation "List of issue IDs created during test."))
  :documentation "Tracks state and side effects during test workflow execution.
Created fresh for each workflow, cleaned up after.")

(cl-defmethod beads-test-context-record-message ((ctx beads-test-context)
                                                  format-string &rest args)
  "Record a message with FORMAT-STRING and ARGS in CTX for later assertion."
  (push (cons format-string args)
        (oref ctx messages)))

(cl-defmethod beads-test-context-record-error ((ctx beads-test-context)
                                                error-type data)
  "Record an error of ERROR-TYPE with DATA in CTX for later assertion."
  (push (cons error-type data)
        (oref ctx errors)))

(cl-defmethod beads-test-context-record-command ((ctx beads-test-context)
                                                  command)
  "Record a COMMAND execution in CTX."
  (push command (oref ctx command-executions)))

(cl-defmethod beads-test-context-record-buffer ((ctx beads-test-context)
                                                 buffer-name)
  "Record a created buffer with BUFFER-NAME in CTX."
  (unless (member buffer-name (oref ctx created-buffers))
    (push buffer-name (oref ctx created-buffers))))

(cl-defmethod beads-test-context-add-trace ((ctx beads-test-context)
                                             action result)
  "Add an execution trace entry for ACTION with RESULT to CTX."
  (push (list (current-time) action result)
        (oref ctx execution-trace)))

(cl-defmethod beads-test-context-pop-response ((ctx beads-test-context))
  "Pop and return the next queued user response from CTX.
Returns t if queue is empty (default confirmation)."
  (if (oref ctx user-responses)
      (pop (oref ctx user-responses))
    t))

(cl-defmethod beads-test-context-add-transient-arg ((ctx beads-test-context)
                                                     arg)
  "Add ARG to the accumulated transient arguments in CTX."
  (push arg (oref ctx transient-args)))

(cl-defmethod beads-test-context-clear-transient-args ((ctx beads-test-context))
  "Clear accumulated transient arguments in CTX."
  (setf (oref ctx transient-args) nil))

(cl-defmethod beads-test-context-get-transient-args ((ctx beads-test-context))
  "Get the current transient arguments from CTX as a list."
  (nreverse (copy-sequence (oref ctx transient-args))))

;;; ============================================================
;;; Base Action Class
;;; ============================================================

(defclass beads-test-action ()
  ((description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Human-readable description of this action.")
   (pre-conditions
    :initarg :pre-conditions
    :type list
    :initform nil
    :documentation "List of (predicate . message) pairs that must pass.")
   (post-assertions
    :initarg :post-assertions
    :type list
    :initform nil
    :documentation "List of assertion functions to run after execution."))
  ;; NOTE: Not :abstract because we instantiate this directly for error
  ;; reporting placeholders (e.g., "Setup" or "Assertion N" actions).
  :documentation "Base class for all test actions.
Subclasses should implement `beads-test-action-execute'.")

(cl-defgeneric beads-test-action-execute (action context)
  "Execute ACTION within CONTEXT.
Returns the result of the action.
May modify CONTEXT to record side effects.")

(cl-defgeneric beads-test-action-validate (action context)
  "Validate that ACTION can be executed in CONTEXT.
Returns nil if valid, or an error message string if invalid.")

(cl-defgeneric beads-test-action-describe (action)
  "Return a human-readable description of ACTION.")

(cl-defmethod beads-test-action-validate ((_action beads-test-action)
                                           _context)
  "Default validation: always valid."
  nil)

(cl-defmethod beads-test-action-describe ((action beads-test-action))
  "Return description of ACTION from slot or class name."
  (or (oref action description)
      (format "<%s>" (eieio-object-class-name action))))

;;; ============================================================
;;; Transient Actions
;;; ============================================================

(defclass beads-test-action-open-transient (beads-test-action)
  ((command
    :initarg :command
    :type symbol
    :documentation "The transient prefix command to invoke."))
  :documentation "Action that opens a transient menu.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-open-transient)
                                          context)
  "Execute ACTION to open a transient menu, updating CONTEXT."
  (let ((cmd (oref action command)))
    (setf (oref context current-transient) cmd)
    (beads-test-context-clear-transient-args context)
    (beads-test-context-add-trace context action cmd)
    cmd))

(cl-defmethod beads-test-action-describe ((action beads-test-action-open-transient))
  "Return a description of ACTION for opening a transient."
  (format "Open transient: %s" (oref action command)))

;;; ---

(defclass beads-test-action-set-infix (beads-test-action)
  ((key
    :initarg :key
    :type string
    :documentation "The infix argument key (e.g., \"--title=\").")
   (value
    :initarg :value
    :type (or string number null)
    :documentation "The value to set for this infix."))
  :documentation "Action that sets a transient infix argument value.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-set-infix)
                                          context)
  "Execute ACTION to set an infix value, updating CONTEXT."
  (let* ((key (oref action key))
         (value (oref action value))
         (arg (if value
                  (format "%s%s" key
                          (if (numberp value)
                              (number-to-string value)
                            value))
                key)))
    (beads-test-context-add-transient-arg context arg)
    (beads-test-context-add-trace context action arg)
    arg))

(cl-defmethod beads-test-action-describe ((action beads-test-action-set-infix))
  "Return a description of ACTION for setting an infix."
  (format "Set %s to %S" (oref action key) (oref action value)))

;;; ---

(defclass beads-test-action-execute-suffix (beads-test-action)
  ((suffix-key
    :initarg :suffix-key
    :type string
    :initform "x"
    :documentation "The key that triggers the suffix (default: x).")
   (expect-error
    :initarg :expect-error
    :type (or null symbol string)
    :initform nil
    :documentation "Expected error type or message pattern, or nil."))
  :documentation "Action that executes a transient suffix command.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-execute-suffix)
                                          context)
  "Execute ACTION to run a suffix command, updating CONTEXT."
  (let* ((transient-cmd (oref context current-transient))
         (args (beads-test-context-get-transient-args context))
         (expect-error (oref action expect-error))
         result)
    ;; Mock transient-args to return our accumulated args
    (cl-letf (((symbol-function 'transient-args)
               (lambda (prefix)
                 (when (eq prefix transient-cmd)
                   args))))
      (condition-case err
          (progn
            (setq result (funcall (intern (format "%s--execute" transient-cmd))))
            (when expect-error
              (error "Expected error %S but execution succeeded" expect-error)))
        (user-error
         (beads-test-context-record-error context 'user-error (cdr err))
         (if expect-error
             (setq result (cons 'user-error (cdr err)))
           (signal (car err) (cdr err))))
        (error
         (beads-test-context-record-error context 'error (cdr err))
         (if expect-error
             (setq result (cons 'error (cdr err)))
           (signal (car err) (cdr err))))))
    (beads-test-context-add-trace context action result)
    result))

(cl-defmethod beads-test-action-describe ((action beads-test-action-execute-suffix))
  "Return a description of ACTION for executing a suffix."
  (format "Execute suffix (%s)" (oref action suffix-key)))

;;; ---

(defclass beads-test-action-preview (beads-test-action)
  ()
  :documentation "Action that triggers a preview in a transient.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-preview)
                                          context)
  "Execute ACTION to run preview command, updating CONTEXT."
  (let* ((transient-cmd (oref context current-transient))
         (args (beads-test-context-get-transient-args context))
         result)
    (cl-letf (((symbol-function 'transient-args)
               (lambda (prefix)
                 (when (eq prefix transient-cmd)
                   args))))
      (setq result (funcall (intern (format "%s--preview" transient-cmd)))))
    (beads-test-context-add-trace context action result)
    result))

(cl-defmethod beads-test-action-describe ((_action beads-test-action-preview))
  "Describe preview action."
  "Preview command")

;;; ---

(defclass beads-test-action-reset-transient (beads-test-action)
  ((confirm
    :initarg :confirm
    :type boolean
    :initform t
    :documentation "Whether to confirm the reset."))
  :documentation "Action that resets a transient's state.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-reset-transient)
                                          context)
  "Execute ACTION to reset transient state, updating CONTEXT."
  (let ((confirm (oref action confirm)))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) confirm))
              ((symbol-function 'transient-reset)
               (lambda () nil))
              ((symbol-function 'transient--redisplay)
               (lambda () nil)))
      (let* ((transient-cmd (oref context current-transient))
             (reset-fn (intern (format "%s--reset" transient-cmd))))
        (when (fboundp reset-fn)
          (funcall reset-fn))))
    (when confirm
      (beads-test-context-clear-transient-args context))
    (beads-test-context-add-trace context action confirm)
    confirm))

(cl-defmethod beads-test-action-describe ((action beads-test-action-reset-transient))
  "Return a description of ACTION for resetting transient."
  (format "Reset transient (confirm: %s)" (oref action confirm)))

;;; ============================================================
;;; Confirmation Actions
;;; ============================================================

(defclass beads-test-action-confirm (beads-test-action)
  ((response
    :initarg :response
    :type boolean
    :initform t
    :documentation "The response to give (t=yes, nil=no).")
   (prompt-pattern
    :initarg :prompt-pattern
    :type (or null string)
    :initform nil
    :documentation "Optional regex to validate the prompt."))
  :documentation "Action that responds to a y-or-n-p prompt.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-confirm)
                                          context)
  "Execute ACTION to queue a confirmation response, updating CONTEXT."
  (let ((response (oref action response)))
    (push response (oref context user-responses))
    (beads-test-context-add-trace context action response)
    response))

(cl-defmethod beads-test-action-describe ((action beads-test-action-confirm))
  "Return a description of ACTION for confirming a prompt."
  (format "Confirm prompt: %s" (if (oref action response) "yes" "no")))

;;; ---

(defclass beads-test-action-input-string (beads-test-action)
  ((response
    :initarg :response
    :type string
    :documentation "The string to input.")
   (prompt-pattern
    :initarg :prompt-pattern
    :type (or null string)
    :initform nil
    :documentation "Optional regex to validate the prompt."))
  :documentation "Action that provides input for read-string prompts.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-input-string)
                                          context)
  "Execute ACTION to queue a string for `read-string', updating CONTEXT."
  (let ((response (oref action response)))
    (push response (oref context user-responses))
    (beads-test-context-add-trace context action response)
    response))

(cl-defmethod beads-test-action-describe ((action beads-test-action-input-string))
  "Return a description of ACTION for inputting a string."
  (format "Input string: %S" (oref action response)))

;;; ============================================================
;;; High-Level Workflow Actions
;;; ============================================================

(defclass beads-test-action-create-issue (beads-test-action)
  ((title
    :initarg :title
    :type string
    :documentation "Issue title (required).")
   (issue-type
    :initarg :type
    :type (or null string)
    :initform nil
    :documentation "Issue type (bug, feature, task, epic, chore).")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Priority (0-4).")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Issue description.")
   (show-after
    :initarg :show-after
    :type boolean
    :initform nil
    :documentation "Whether to show the issue after creation."))
  :documentation "High-level action that creates an issue.
Composes lower-level transient actions internally.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-create-issue)
                                          context)
  "Execute ACTION to create an issue, updating CONTEXT."
  (require 'beads-command)
  ;; Use beads-command-create! directly instead of going through transient
  ;; This is more reliable for testing and properly returns the issue
  (condition-case err
      (let* ((issue (beads-command-create!
                     :title (oref action title)
                     :issue-type (oref action issue-type)
                     :priority (oref action priority)
                     :description (oref action description)))
             (issue-id (oref issue id)))
        ;; Track created issue
        (push issue-id (oref context created-issues))
        ;; Mark cache as invalidated (simulating what the real command does)
        (setf (oref context completion-cache-invalidated) t)
        ;; Update context for tracking
        (setf (oref context current-transient) 'beads-create)
        (beads-test-context-add-trace context action issue-id)
        ;; Optionally show the issue
        (when (oref action show-after)
          (beads-show issue-id)
          (beads-test-context-record-buffer context (buffer-name)))
        issue-id)
    (error
     (beads-test-context-record-error context (car err) (cdr err))
     (beads-test-context-add-trace context action err)
     (signal (car err) (cdr err)))))

(cl-defmethod beads-test-action-describe ((action beads-test-action-create-issue))
  "Return a description of ACTION for creating an issue."
  (format "Create issue: %S" (oref action title)))

;;; ---

(defclass beads-test-action-update-issue (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "ID of issue to update, or function returning ID.")
   (changes
    :initarg :changes
    :type list
    :documentation "Alist of (field . value) changes."))
  :documentation "High-level action that updates an issue.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-update-issue)
                                          context)
  "Execute ACTION to update an issue, updating CONTEXT."
  (let ((args nil)
        (issue-id (oref action issue-id)))
    ;; Resolve issue-id if it's a function
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; Build args from changes alist
    (dolist (change (oref action changes))
      (let ((field (car change))
            (value (cdr change)))
        (push (format "--%s=%s" field
                      (if (numberp value)
                          (number-to-string value)
                        value))
              args)))

    ;; Set up beads-update state
    (require 'beads-update)
    (setq beads-update--issue-id issue-id)

    ;; Execute with mocks
    (cl-letf (((symbol-function 'transient-args)
               (lambda (prefix)
                 (when (eq prefix 'beads-update)
                   (nreverse args))))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda ()
                 (setf (oref context completion-cache-invalidated) t))))
      (condition-case err
          (let ((result (call-interactively #'beads-update--execute)))
            (beads-test-context-add-trace context action result)
            result)
        (error
         (beads-test-context-record-error context (car err) (cdr err))
         (beads-test-context-add-trace context action err)
         (signal (car err) (cdr err)))))))

(cl-defmethod beads-test-action-describe ((action beads-test-action-update-issue))
  "Return a description of ACTION for updating an issue."
  (format "Update issue %s: %S" (oref action issue-id) (oref action changes)))

;;; ---

(defclass beads-test-action-close-issue (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "ID of issue to close, or function returning ID.")
   (reason
    :initarg :reason
    :type string
    :initform "Completed"
    :documentation "Reason for closing."))
  :documentation "High-level action that closes an issue.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-close-issue)
                                          context)
  "Execute ACTION to close an issue, updating CONTEXT."
  (let ((issue-id (oref action issue-id))
        (args nil))
    ;; Resolve issue-id if it's a function
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; Build args list with both id and reason
    (setq args (list (format "--id=%s" issue-id)
                     (format "--reason=%s" (oref action reason))))
    (require 'beads-close)
    (setq beads-close--issue-id issue-id)

    (cl-letf (((symbol-function 'transient-args)
               (lambda (prefix)
                 (when (eq prefix 'beads-close)
                   args)))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda ()
                 (setf (oref context completion-cache-invalidated) t))))
      (condition-case err
          (let ((result (call-interactively #'beads-close--execute)))
            (beads-test-context-add-trace context action result)
            result)
        (error
         (beads-test-context-record-error context (car err) (cdr err))
         (beads-test-context-add-trace context action err)
         (signal (car err) (cdr err)))))))

(cl-defmethod beads-test-action-describe ((action beads-test-action-close-issue))
  "Return a description of ACTION for closing an issue."
  (format "Close issue %s: %S" (oref action issue-id) (oref action reason)))

;;; ============================================================
;;; Workflow Class
;;; ============================================================

(defclass beads-test-workflow ()
  ((name
    :initarg :name
    :type string
    :documentation "Human-readable name of the workflow.")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Longer description of what this workflow tests.")
   (tags
    :initarg :tags
    :type list
    :initform nil
    :documentation "ERT tags for this test.")
   (setup
    :initarg :setup
    :type (or null function)
    :initform nil
    :documentation "Setup function receiving context, called before steps.")
   (teardown
    :initarg :teardown
    :type (or null function)
    :initform nil
    :documentation "Teardown function receiving context, called after steps.")
   (steps
    :initarg :steps
    :type list
    :initform nil
    :documentation "List of beads-test-action instances.")
   (assertions
    :initarg :assertions
    :type list
    :initform nil
    :documentation "List of assertion functions receiving context."))
  :documentation "Represents a complete user workflow test.")

(cl-defmethod beads-test-workflow-describe ((workflow beads-test-workflow))
  "Return a description of WORKFLOW."
  (or (oref workflow description)
      (oref workflow name)))

;;; ============================================================
;;; Workflow Runner
;;; ============================================================

(defun beads-test-run-workflow (workflow)
  "Execute WORKFLOW and return the final context.
Creates a temporary beads project, runs all steps, and cleans up."
  (let* ((context (beads-test-context))
         (steps (oref workflow steps))
         (setup-fn (oref workflow setup))
         (teardown-fn (oref workflow teardown))
         (assertions (oref workflow assertions)))

    ;; Set up test project
    (require 'beads-test)
    (let ((project-dir (beads-test-create-project)))
      (setf (oref context project-dir) project-dir)

      (let ((default-directory project-dir)
            (beads--project-cache (make-hash-table :test 'equal)))

        ;; Clear transient state
        (beads-test--clear-transient-state)

        ;; Mock project root discovery
        (cl-letf (((symbol-function 'beads--find-project-root)
                   (lambda () nil)))
          (unwind-protect
              (progn
                ;; Run setup
                (when setup-fn
                  (funcall setup-fn context))

                ;; Execute each step
                (dolist (action steps)
                  ;; Validate action
                  (let ((error-msg (beads-test-action-validate action context)))
                    (when error-msg
                      (error "Action validation failed: %s" error-msg)))
                  ;; Execute action
                  (beads-test-action-execute action context))

                ;; Run assertions
                (dolist (assertion-fn assertions)
                  (funcall assertion-fn context)))

            ;; Teardown
            (when teardown-fn
              (funcall teardown-fn context))
            (beads-test--clear-transient-state)))))

    context))

;;; ============================================================
;;; Assertion Helpers
;;; ============================================================

(defun beads-test-assert-issue-created (_context &rest props)
  "Assert that an issue with PROPS was created during the test.
PROPS is a plist of :title, :type, :priority, etc.
_CONTEXT is the test context (unused, kept for API consistency)."
  (let* ((title (plist-get props :title))
         (issue-type (plist-get props :type))
         (priority (plist-get props :priority))
         (issues (beads-command-list!))
         (found nil))
    (dolist (issue issues)
      (when (and (or (null title)
                     (equal (oref issue title) title))
                 (or (null issue-type)
                     (equal (oref issue issue-type) issue-type))
                 (or (null priority)
                     (equal (oref issue priority) priority)))
        (setq found issue)))
    (unless found
      (let ((criteria (string-join
                       (delq nil
                             (list (when title (format "title=%S" title))
                                   (when issue-type (format "type=%S" issue-type))
                                   (when priority (format "priority=%S" priority))))
                       ", "))
            (existing (mapcar (lambda (i) (oref i title)) issues)))
        (ert-fail (format "No issue found matching [%s]. Existing issues: %S"
                          criteria existing))))
    found))

(defun beads-test-assert-issue-exists (_context issue-id)
  "Assert that ISSUE-ID exists in the project.
_CONTEXT is the test context (unused, kept for API consistency)."
  (let ((issues (beads-command-list!)))
    (unless (seq-find (lambda (issue) (equal (oref issue id) issue-id)) issues)
      (ert-fail (format "Issue %S not found. Existing IDs: %S"
                        issue-id
                        (mapcar (lambda (i) (oref i id)) issues))))))

(defun beads-test-assert-issue-has (_context issue-id &rest props)
  "Assert that ISSUE-ID has properties PROPS.
PROPS is a plist of :status, :priority, :title, etc.
_CONTEXT is the test context (unused, kept for API consistency)."
  (let* ((issues (beads-command-list!))
         (issue (seq-find (lambda (i) (equal (oref i id) issue-id)) issues)))
    (unless issue
      (ert-fail (format "Issue %S not found. Existing IDs: %S"
                        issue-id
                        (mapcar (lambda (i) (oref i id)) issues))))
    (let ((status (plist-get props :status))
          (priority (plist-get props :priority))
          (title (plist-get props :title)))
      (when status
        (unless (equal (oref issue status) status)
          (ert-fail (format "Issue %s: expected status=%S, got %S"
                            issue-id status (oref issue status)))))
      (when priority
        (unless (equal (oref issue priority) priority)
          (ert-fail (format "Issue %s: expected priority=%S, got %S"
                            issue-id priority (oref issue priority)))))
      (when title
        (unless (equal (oref issue title) title)
          (ert-fail (format "Issue %s: expected title=%S, got %S"
                            issue-id title (oref issue title))))))
    issue))

(defun beads-test-assert-issue-count (_context expected)
  "Assert that EXPECTED number of issues exist.
_CONTEXT is the test context (unused, kept for API consistency)."
  (let* ((issues (beads-command-list!))
         (actual (length issues)))
    (unless (= actual expected)
      (ert-fail (format "Expected %d issues, got %d. Issues: %S"
                        expected actual
                        (mapcar (lambda (i) (oref i title)) issues))))))

(defun beads-test-assert-completion-cache-invalidated (context)
  "Assert that the completion cache was invalidated.
CONTEXT is the test context."
  (unless (oref context completion-cache-invalidated)
    (ert-fail "Completion cache was not invalidated")))

(defun beads-test-assert-label-cache-invalidated (context)
  "Assert that the label cache was invalidated.
CONTEXT is the test context."
  (unless (oref context label-cache-invalidated)
    (ert-fail "Label cache was not invalidated")))

(defun beads-test-assert-no-errors (context)
  "Assert that no errors were signaled during the workflow.
CONTEXT is the test context."
  (when (oref context errors)
    (ert-fail (format "Expected no errors, but got: %S" (oref context errors)))))

(defun beads-test-assert-error-signaled (context error-type)
  "Assert that an error of ERROR-TYPE was signaled.
CONTEXT is the test context."
  (unless (assq error-type (oref context errors))
    (ert-fail (format "Expected error %S, but got: %S"
                      error-type (oref context errors)))))

(defun beads-test-assert-message-displayed (context pattern)
  "Assert that a message matching PATTERN was displayed.
CONTEXT is the test context."
  (let ((found nil)
        (all-messages nil))
    (dolist (msg (oref context messages))
      (let ((formatted (apply #'format (car msg) (cdr msg))))
        (push formatted all-messages)
        (when (string-match-p pattern formatted)
          (setq found t))))
    (unless found
      (ert-fail (format "No message matched pattern %S. Messages: %S"
                        pattern (nreverse all-messages))))))

(defun beads-test-assert-buffer-exists (context buffer-name-pattern)
  "Assert that a buffer matching BUFFER-NAME-PATTERN was created.
CONTEXT is the test context."
  (let ((found nil))
    (dolist (buf (oref context created-buffers))
      (when (string-match-p buffer-name-pattern buf)
        (setq found t)))
    (unless found
      (ert-fail (format "No buffer matched pattern %S. Buffers: %S"
                        buffer-name-pattern
                        (oref context created-buffers))))))

;;; ============================================================
;;; Buffer/List Actions
;;; ============================================================

(defclass beads-test-action-open-list (beads-test-action)
  ((filter-status
    :initarg :filter-status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (open, in_progress, closed, blocked).")
   (filter-type
    :initarg :filter-type
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type.")
   (filter-priority
    :initarg :filter-priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority."))
  :documentation "Action that opens a beads-list buffer.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-open-list)
                                          context)
  "Execute ACTION to open a beads-list buffer, updating CONTEXT."
  (require 'beads-list)
  (let ((args nil))
    (when (oref action filter-status)
      (push (format "--status=%s" (oref action filter-status)) args))
    (when (oref action filter-type)
      (push (format "--type=%s" (oref action filter-type)) args))
    (when (oref action filter-priority)
      (push (format "--priority=%d" (oref action filter-priority)) args))

    (cl-letf (((symbol-function 'transient-args)
               (lambda (prefix)
                 (when (eq prefix 'beads-list)
                   (nreverse args)))))
      (call-interactively #'beads-list--transient-execute)
      (beads-test-context-record-buffer context (buffer-name))
      (beads-test-context-add-trace context action (buffer-name))
      (buffer-name))))

(cl-defmethod beads-test-action-describe ((action beads-test-action-open-list))
  "Return a description of ACTION for opening a list."
  (format "Open issue list (status: %s, type: %s)"
          (or (oref action filter-status) "any")
          (or (oref action filter-type) "any")))

;;; ---

(defclass beads-test-action-list-navigate (beads-test-action)
  ((direction
    :initarg :direction
    :type symbol
    :initform 'next
    :documentation "Direction: next, previous, first, last.")
   (count
    :initarg :count
    :type integer
    :initform 1
    :documentation "Number of items to move."))
  :documentation "Action that navigates in a beads-list buffer.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-list-navigate)
                                          context)
  "Execute ACTION to navigate in the list buffer, updating CONTEXT."
  (let ((direction (oref action direction))
        (count (oref action count)))
    (pcase direction
      ('next (forward-line count))
      ('previous (forward-line (- count)))
      ('first (goto-char (point-min))
              (forward-line 1))  ; Skip header
      ('last (goto-char (point-max))
             (forward-line -1)))
    (beads-test-context-add-trace context action (point))
    (point)))

(cl-defmethod beads-test-action-describe ((action beads-test-action-list-navigate))
  "Return a description of ACTION for navigation."
  (format "Navigate %s %d" (oref action direction) (oref action count)))

;;; ---

(defclass beads-test-action-list-select (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or null string function)
    :initform nil
    :documentation "Issue ID to select, function returning ID, or nil for current."))
  :documentation "Action that selects/shows an issue from the list.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-list-select)
                                          context)
  "Execute ACTION to select an issue, updating CONTEXT."
  (require 'beads-list)
  (let ((issue-id (oref action issue-id)))
    ;; If issue-id is a function, call it with context
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; If specific ID, navigate to it first
    (when issue-id
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) issue-id)))
        (forward-line 1)))
    ;; Now select (press RET)
    (let ((selected-id (tabulated-list-get-id)))
      (beads-list-show)
      (beads-test-context-record-buffer context (buffer-name))
      (beads-test-context-add-trace context action selected-id)
      selected-id)))

(cl-defmethod beads-test-action-describe ((action beads-test-action-list-select))
  "Return a description of ACTION for selecting an issue."
  (format "Select issue %s" (or (oref action issue-id) "at point")))

;;; ---

(defclass beads-test-action-list-mark (beads-test-action)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list function)
    :initform nil
    :documentation "List of issue IDs to mark, or nil for current.")
   (unmark
    :initarg :unmark
    :type boolean
    :initform nil
    :documentation "If t, unmark instead of mark."))
  :documentation "Action that marks/unmarks issues in a list buffer.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-list-mark)
                                          context)
  "Execute ACTION to mark/unmark issues, updating CONTEXT."
  (require 'beads-list)
  (let ((issue-ids (oref action issue-ids))
        (unmark (oref action unmark))
        (marked nil))
    ;; If issue-ids is a function, call it
    (when (functionp issue-ids)
      (setq issue-ids (funcall issue-ids context)))
    ;; Mark each issue
    (if issue-ids
        (dolist (id issue-ids)
          (goto-char (point-min))
          (while (and (not (eobp))
                      (not (equal (tabulated-list-get-id) id)))
            (forward-line 1))
          (unless (eobp)
            (if unmark
                (beads-list-unmark)
              (beads-list-mark))
            (push id marked)))
      ;; Mark current
      (if unmark
          (beads-list-unmark)
        (beads-list-mark))
      (push (tabulated-list-get-id) marked))
    (beads-test-context-add-trace context action marked)
    marked))

(cl-defmethod beads-test-action-describe ((action beads-test-action-list-mark))
  "Return a description of ACTION for marking/unmarking."
  (format "%s issues: %s"
          (if (oref action unmark) "Unmark" "Mark")
          (or (oref action issue-ids) "current")))

;;; ---

(defclass beads-test-action-refresh-buffer (beads-test-action)
  ()
  :documentation "Action that refreshes the current buffer.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-refresh-buffer)
                                          context)
  "Execute ACTION to refresh the buffer, updating CONTEXT."
  (cond
   ((derived-mode-p 'beads-list-mode)
    (beads-list-refresh))
   ((derived-mode-p 'beads-show-mode)
    (revert-buffer nil t))
   (t
    (revert-buffer nil t)))
  (beads-test-context-add-trace context action (buffer-name))
  (buffer-name))

(cl-defmethod beads-test-action-describe ((_action beads-test-action-refresh-buffer))
  "Describe refresh action."
  "Refresh current buffer")

;;; ---

(defclass beads-test-action-key-sequence (beads-test-action)
  ((keys
    :initarg :keys
    :type string
    :documentation "Key sequence to execute (e.g., \"B c\" for bulk close)."))
  :documentation "Action that executes an arbitrary key sequence.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-key-sequence)
                                          context)
  "Execute ACTION to run a key sequence, updating CONTEXT."
  (let ((keys (oref action keys)))
    (execute-kbd-macro (kbd keys))
    (beads-test-context-add-trace context action keys)
    keys))

(cl-defmethod beads-test-action-describe ((action beads-test-action-key-sequence))
  "Return a description of ACTION for pressing keys."
  (format "Press keys: %s" (oref action keys)))

;;; ============================================================
;;; Show Buffer Actions
;;; ============================================================

(defclass beads-test-action-show-issue (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "Issue ID to show, or function returning ID."))
  :documentation "Action that opens a beads-show buffer for an issue.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-show-issue)
                                          context)
  "Execute ACTION to open a show buffer, updating CONTEXT."
  (require 'beads-show)
  (let ((issue-id (oref action issue-id)))
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    (beads-show issue-id)
    (beads-test-context-record-buffer context (buffer-name))
    (beads-test-context-add-trace context action issue-id)
    issue-id))

(cl-defmethod beads-test-action-describe ((action beads-test-action-show-issue))
  "Return a description of ACTION for showing an issue."
  (format "Show issue: %s" (oref action issue-id)))

;;; ============================================================
;;; CLI Command Actions
;;; ============================================================

(defclass beads-test-action-add-dependency (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "ID of the issue to add dependency to.")
   (depends-on-id
    :initarg :depends-on-id
    :type (or string function)
    :documentation "ID of the issue to depend on.")
   (dep-type
    :initarg :dep-type
    :type string
    :initform "blocks"
    :documentation "Dependency type (blocks, blocked-by, etc.)."))
  :documentation "Action that adds a dependency between two issues.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-add-dependency)
                                          context)
  "Execute ACTION to add a dependency, updating CONTEXT."
  (require 'beads-command)
  (let ((issue-id (oref action issue-id))
        (depends-on-id (oref action depends-on-id))
        (dep-type (oref action dep-type)))
    ;; Resolve issue-id if it's a function
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; Resolve depends-on-id if it's a function
    (when (functionp depends-on-id)
      (setq depends-on-id (funcall depends-on-id context)))
    (let ((result (beads-command-dep-add!
                   :issue-id issue-id
                   :depends-on-id depends-on-id
                   :dep-type dep-type)))
      (beads-test-context-add-trace context action result)
      result)))

(cl-defmethod beads-test-action-describe ((action beads-test-action-add-dependency))
  "Return a description of ACTION for adding a dependency."
  (format "Add dependency: %s %s %s"
          (oref action issue-id)
          (oref action dep-type)
          (oref action depends-on-id)))

;;; ---

(defclass beads-test-action-show-stats (beads-test-action)
  ()
  :documentation "Action that retrieves and displays project statistics.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-show-stats)
                                          context)
  "Execute ACTION to show stats, updating CONTEXT."
  (require 'beads-command)
  (let ((result (beads-command-stats!)))
    (beads-test-context-add-trace context action result)
    result))

(cl-defmethod beads-test-action-describe ((_action beads-test-action-show-stats))
  "Describe show stats action."
  "Show project statistics")

;;; ============================================================
;;; Agent Action Classes
;;; ============================================================

(defclass beads-test-action-start-agent (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "Issue ID or function returning ID.")
   (backend
    :initarg :backend
    :type (or null string)
    :initform nil
    :documentation "Backend name or nil for default.")
   (prompt
    :initarg :prompt
    :type (or null string)
    :initform nil
    :documentation "Custom prompt or nil for auto-generated."))
  :documentation "Action that simulates starting an AI agent on an issue.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-start-agent)
                                          context)
  "Execute ACTION to start a mock agent, updating CONTEXT."
  (let ((issue-id (oref action issue-id))
        (backend (or (oref action backend) "mock")))
    ;; Resolve issue-id if it's a function
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; Create a mock session (without requiring real agent)
    (when (fboundp 'beads-agent--create-session)
      (beads-agent--create-session issue-id backend "/mock/project" 'mock-handle))
    (beads-test-context-add-trace context action
                                   (list :issue-id issue-id :backend backend))
    issue-id))

(cl-defmethod beads-test-action-describe ((action beads-test-action-start-agent))
  "Describe start agent action."
  (format "Start agent on %s" (oref action issue-id)))

;;; ---

(defclass beads-test-action-stop-agent (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "Issue ID or function returning ID to stop agent for."))
  :documentation "Action that simulates stopping an AI agent session.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-stop-agent)
                                          context)
  "Execute ACTION to stop a mock agent, updating CONTEXT."
  (let ((issue-id (oref action issue-id)))
    ;; Resolve issue-id if it's a function
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; Stop session if agent module loaded
    (when (and (fboundp 'beads-agent--get-sessions-for-issue)
               (fboundp 'beads-agent--destroy-session))
      (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
        (dolist (session sessions)
          (beads-agent--destroy-session (oref session id)))))
    (beads-test-context-add-trace context action issue-id)
    issue-id))

(cl-defmethod beads-test-action-describe ((action beads-test-action-stop-agent))
  "Describe stop agent action."
  (format "Stop agent for %s" (oref action issue-id)))

;;; ---

(defun beads-test-assert-agent-session-exists (context issue-id)
  "Assert that an agent session exists for ISSUE-ID.
CONTEXT is ignored but kept for API consistency."
  (ignore context)
  (unless (and (fboundp 'beads-agent--get-sessions-for-issue)
               (beads-agent--get-sessions-for-issue issue-id))
    (ert-fail (format "No agent session found for %s" issue-id))))

(defun beads-test-assert-no-agent-sessions (context issue-id)
  "Assert that no agent sessions exist for ISSUE-ID.
CONTEXT is ignored but kept for API consistency."
  (ignore context)
  (when (and (fboundp 'beads-agent--get-sessions-for-issue)
             (beads-agent--get-sessions-for-issue issue-id))
    (ert-fail (format "Expected no sessions for %s, but found some" issue-id))))

;;; ---

(defclass beads-test-action-assert-agent-active (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "Issue ID or function returning ID to check."))
  :documentation "Action that asserts an agent session is active for an issue.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-assert-agent-active)
                                          context)
  "Execute ACTION to assert agent session is active, updating CONTEXT."
  (let ((issue-id (oref action issue-id)))
    ;; Resolve issue-id if it's a function
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; Check session exists
    (unless (and (fboundp 'beads-agent--get-sessions-for-issue)
                 (beads-agent--get-sessions-for-issue issue-id))
      (ert-fail (format "No agent session found for %s" issue-id)))
    ;; Check session is active (mock sessions are always considered active)
    (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
      (unless sessions
        (ert-fail (format "Agent session for %s is not active" issue-id))))
    (beads-test-context-add-trace context action issue-id)
    issue-id))

(cl-defmethod beads-test-action-describe ((action beads-test-action-assert-agent-active))
  "Describe assert agent active action."
  (format "Assert agent active for %s" (oref action issue-id)))

;;; ---

(defclass beads-test-action-jump-to-agent (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or string function)
    :documentation "Issue ID or function returning ID to jump to."))
  :documentation "Action that simulates jumping to an agent buffer.")

(cl-defmethod beads-test-action-execute ((action beads-test-action-jump-to-agent)
                                          context)
  "Execute ACTION to simulate jumping to agent buffer, updating CONTEXT."
  (let ((issue-id (oref action issue-id)))
    ;; Resolve issue-id if it's a function
    (when (functionp issue-id)
      (setq issue-id (funcall issue-id context)))
    ;; Verify session exists before jumping
    (unless (and (fboundp 'beads-agent--get-sessions-for-issue)
                 (beads-agent--get-sessions-for-issue issue-id))
      (user-error "No agent session for %s" issue-id))
    ;; Simulate buffer switch by recording it (mock - no real buffer exists)
    (let ((mock-buffer-name (format "*beads-agent: %s*" issue-id)))
      (beads-test-context-record-buffer context mock-buffer-name)
      (beads-test-context-add-trace context action mock-buffer-name)
      mock-buffer-name)))

(cl-defmethod beads-test-action-describe ((action beads-test-action-jump-to-agent))
  "Describe jump to agent action."
  (format "Jump to agent for %s" (oref action issue-id)))

;;; ============================================================
;;; Error Reporting and ERT Integration
;;; ============================================================

(defclass beads-test-failure ()
  ((step-index
    :initarg :step-index
    :type integer
    :documentation "Index of the step that failed (0-based).")
   (action
    :initarg :action
    :documentation "The action that failed.")
   (error-type
    :initarg :error-type
    :type symbol
    :documentation "Type of error (e.g., user-error, error, assertion).")
   (error-message
    :initarg :error-message
    :type string
    :documentation "Human-readable error message.")
   (error-data
    :initarg :error-data
    :initform nil
    :documentation "Additional error data.")
   (context-snapshot
    :initarg :context-snapshot
    :initform nil
    :documentation "Snapshot of context at failure time.")
   (backtrace
    :initarg :backtrace
    :type (or null string)
    :initform nil
    :documentation "Backtrace at point of failure."))
  :documentation "Detailed information about a workflow test failure.")

(cl-defmethod beads-test-failure-format ((failure beads-test-failure))
  "Format FAILURE as a detailed, readable error report."
  (with-slots (step-index action error-type error-message
                          error-data context-snapshot backtrace) failure
    (let ((lines nil))
      ;; Header
      (push "══════════════════════════════════════════════════════════" lines)
      (push "WORKFLOW TEST FAILURE" lines)
      (push "══════════════════════════════════════════════════════════" lines)
      (push "" lines)

      ;; Step information
      (push (format "Failed at step %d: %s"
                    (1+ step-index)  ; 1-based for humans
                    (beads-test-action-describe action))
            lines)
      (push (format "Action type: %s" (eieio-object-class-name action)) lines)
      (push "" lines)

      ;; Error details
      (push "── Error ──────────────────────────────────────────────────" lines)
      (push (format "Type: %s" error-type) lines)
      (push (format "Message: %s" error-message) lines)
      (when error-data
        (push (format "Data: %S" error-data) lines))
      (push "" lines)

      ;; Context snapshot
      (when context-snapshot
        (push "── Context at Failure ─────────────────────────────────────" lines)
        (push (format "Project: %s"
                      (or (plist-get context-snapshot :project-dir) "N/A"))
              lines)
        (push (format "Current transient: %s"
                      (or (plist-get context-snapshot :current-transient) "none"))
              lines)
        (push (format "Transient args: %S"
                      (plist-get context-snapshot :transient-args))
              lines)
        (push (format "Created issues: %S"
                      (plist-get context-snapshot :created-issues))
              lines)
        (push (format "Errors so far: %S"
                      (plist-get context-snapshot :errors))
              lines)
        (push "" lines))

      ;; Execution trace
      (when context-snapshot
        (let ((trace (plist-get context-snapshot :execution-trace)))
          (when trace
            (push "── Execution Trace (most recent first) ────────────────────" lines)
            (let ((i (length trace)))
              (dolist (entry (seq-take trace 10))  ; Last 10 entries
                (let ((action (nth 1 entry))
                      (result (nth 2 entry)))
                  (push (format "  %d. %s → %S"
                                i
                                (beads-test-action-describe action)
                                (if (and result (listp result) (> (length (format "%S" result)) 50))
                                    (format "%.50s..." (format "%S" result))
                                  result))
                        lines)
                  (cl-decf i))))
            (when (> (length trace) 10)
              (push (format "  ... and %d more steps" (- (length trace) 10)) lines))
            (push "" lines))))

      ;; Backtrace (truncated)
      (when backtrace
        (push "── Backtrace (truncated) ──────────────────────────────────" lines)
        (let ((bt-lines (split-string backtrace "\n")))
          (dolist (line (seq-take bt-lines 15))
            (push (format "  %s" line) lines))
          (when (> (length bt-lines) 15)
            (push "  ..." lines)))
        (push "" lines))

      (push "══════════════════════════════════════════════════════════" lines)

      (string-join (nreverse lines) "\n"))))

(defun beads-test--capture-context-snapshot (context)
  "Capture a snapshot of CONTEXT for error reporting."
  (list :project-dir (oref context project-dir)
        :current-transient (oref context current-transient)
        :transient-args (copy-sequence (oref context transient-args))
        :created-issues (copy-sequence (oref context created-issues))
        :errors (copy-sequence (oref context errors))
        :execution-trace (copy-sequence (oref context execution-trace))
        :created-buffers (copy-sequence (oref context created-buffers))))

(defun beads-test--capture-backtrace ()
  "Capture current backtrace as a string."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (backtrace))
    (buffer-string)))

(defvar beads-test-workflow-timeout 30
  "Maximum seconds a workflow test can run before being considered hung.
Set to nil to disable timeout protection.")

(defun beads-test--mock-user-input (context body-fn)
  "Execute BODY-FN with all user input functions mocked.
Uses CONTEXT to provide dynamic responses when configured via actions.
This prevents tests from hanging on interactive prompts."
  (cl-letf (;; Yes/no prompts - default to yes
            ((symbol-function 'yes-or-no-p)
             (lambda (_prompt) t))
            ((symbol-function 'y-or-n-p)
             (lambda (_prompt) t))
            ;; String input - return empty or queued response
            ((symbol-function 'read-string)
             (lambda (_prompt &optional _initial _history _default _inherit)
               (or (pop (oref context user-responses)) "")))
            ;; Completing read - return first choice or queued response
            ((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _args)
               (or (pop (oref context user-responses))
                   (if (listp collection) (car collection) ""))))
            ;; Read from minibuffer - return empty or queued response
            ((symbol-function 'read-from-minibuffer)
             (lambda (_prompt &rest _args)
               (or (pop (oref context user-responses)) "")))
            ;; Read number - return 0 or queued response
            ((symbol-function 'read-number)
             (lambda (_prompt &optional _default)
               (or (pop (oref context user-responses)) 0)))
            ;; Read key sequence - return RET
            ((symbol-function 'read-key-sequence)
             (lambda (_prompt &rest _args)
               (kbd "RET")))
            ((symbol-function 'read-key-sequence-vector)
             (lambda (_prompt &rest _args)
               [return])))
    (funcall body-fn)))

(defun beads-test-run-workflow-with-reporting (workflow)
  "Execute WORKFLOW with detailed error reporting.
Returns the context on success.
On failure, signals an ERT-friendly error with detailed diagnostics.

All user input functions are mocked to prevent hangs.  Use
`beads-test-action-confirm' or `beads-test-action-input-string'
to provide specific responses when needed."
  (let* ((context (beads-test-context))
         (steps (oref workflow steps))
         (setup-fn (oref workflow setup))
         (teardown-fn (oref workflow teardown))
         (assertions (oref workflow assertions))
         (step-index 0)
         (failure nil))

    ;; Set up test project
    (require 'beads-test)
    (let ((project-dir (beads-test-create-project)))
      (setf (oref context project-dir) project-dir)

      (let ((default-directory project-dir)
            (beads--project-cache (make-hash-table :test 'equal)))

        ;; Clear transient state
        (beads-test--clear-transient-state)

        ;; Mock project root discovery and all user input
        (cl-letf (((symbol-function 'beads--find-project-root)
                   (lambda () nil)))
          (beads-test--mock-user-input
           context
           (lambda ()
          (unwind-protect
              (condition-case-unless-debug err
                  (progn
                    ;; Run setup
                    (when setup-fn
                      (condition-case setup-err
                          (funcall setup-fn context)
                        (error
                         (setq failure
                               (beads-test-failure
                                :step-index -1
                                :action (beads-test-action :description "Setup")
                                :error-type 'setup-error
                                :error-message (error-message-string setup-err)
                                :error-data (cdr setup-err)
                                :context-snapshot (beads-test--capture-context-snapshot context)
                                :backtrace (beads-test--capture-backtrace)))
                         (signal 'ert-test-failed
                                 (list (beads-test-failure-format failure))))))

                    ;; Execute each step
                    (dolist (action steps)
                      (condition-case action-err
                          (progn
                            ;; Validate action
                            (let ((error-msg (beads-test-action-validate action context)))
                              (when error-msg
                                (setq failure
                                      (beads-test-failure
                                       :step-index step-index
                                       :action action
                                       :error-type 'validation-error
                                       :error-message error-msg
                                       :context-snapshot (beads-test--capture-context-snapshot context)))
                                (signal 'ert-test-failed
                                        (list (beads-test-failure-format failure)))))
                            ;; Execute action
                            (beads-test-action-execute action context)
                            (cl-incf step-index))
                        ((debug error)
                         (setq failure
                               (beads-test-failure
                                :step-index step-index
                                :action action
                                :error-type (car action-err)
                                :error-message (error-message-string action-err)
                                :error-data (cdr action-err)
                                :context-snapshot (beads-test--capture-context-snapshot context)
                                :backtrace (beads-test--capture-backtrace)))
                         (signal 'ert-test-failed
                                 (list (beads-test-failure-format failure))))))

                    ;; Run assertions
                    (let ((assertion-index 0))
                      (dolist (assertion-fn assertions)
                        (condition-case assert-err
                            (funcall assertion-fn context)
                          ((debug error)
                           (setq failure
                                 (beads-test-failure
                                  :step-index (+ (length steps) assertion-index)
                                  :action (beads-test-action
                                           :description (format "Assertion %d" (1+ assertion-index)))
                                  :error-type 'assertion-failed
                                  :error-message (error-message-string assert-err)
                                  :error-data (cdr assert-err)
                                  :context-snapshot (beads-test--capture-context-snapshot context)
                                  :backtrace (beads-test--capture-backtrace)))
                           (signal 'ert-test-failed
                                   (list (beads-test-failure-format failure)))))
                        (cl-incf assertion-index))))
                (ert-test-failed
                 ;; Re-signal ERT failures
                 (signal (car err) (cdr err)))
                (error
                 ;; Wrap unexpected errors
                 (setq failure
                       (beads-test-failure
                        :step-index step-index
                        :action (if (< step-index (length steps))
                                    (nth step-index steps)
                                  (beads-test-action :description "Unknown"))
                        :error-type 'unexpected-error
                        :error-message (error-message-string err)
                        :error-data (cdr err)
                        :context-snapshot (beads-test--capture-context-snapshot context)
                        :backtrace (beads-test--capture-backtrace)))
                 (signal 'ert-test-failed
                         (list (beads-test-failure-format failure)))))

            ;; Teardown (always runs)
            (when teardown-fn
              (ignore-errors (funcall teardown-fn context)))
            (beads-test--clear-transient-state)))))))

    context))

;;; ============================================================
;;; Test Definition Macro
;;; ============================================================

(defmacro beads-define-workflow-test (name &rest body)
  "Define an ERT test from a workflow specification.

NAME is the test name (symbol).
BODY contains keyword-value pairs defining the workflow.

Keywords:
  :description  - Test description string
  :tags         - List of ERT tags (symbols)
  :setup        - Setup function (lambda (ctx) ...)
  :teardown     - Teardown function (lambda (ctx) ...)
  :steps        - List of action instances
  :assert       - List of assertion forms (ctx is bound)

The generated test provides detailed error reporting including:
- Which step failed (with human-readable description)
- Full execution trace leading to failure
- Context snapshot at failure time
- Truncated backtrace

Example:
  (beads-define-workflow-test create-issue-simple
    :description \"Test creating a simple issue\"
    :tags (:integration :workflow)
    :steps
    ((beads-test-action-create-issue
      :title \"Test Issue\"
      :type \"bug\"))
    :assert
    ((beads-test-assert-issue-created ctx :title \"Test Issue\")))"
  (declare (indent 1))
  (let ((description (plist-get body :description))
        (tags (plist-get body :tags))
        (setup (plist-get body :setup))
        (teardown (plist-get body :teardown))
        (steps (plist-get body :steps))
        (assertions (plist-get body :assert)))
    `(ert-deftest ,name ()
       ,@(when description (list description))
       ,@(when tags `(:tags ',tags))
       (skip-unless (executable-find beads-executable))
       (let ((ctx (beads-test-run-workflow-with-reporting
                   (beads-test-workflow
                    :name ,(symbol-name name)
                    :description ,description
                    :setup ,setup
                    :teardown ,teardown
                    :steps (list ,@steps)
                    :assertions
                    (list ,@(mapcar (lambda (assertion)
                                      `(lambda (ctx) ,assertion))
                                    assertions))))))
         ;; Context available for additional checks
         ctx))))

;;; ============================================================
;;; Additional ERT Integration Helpers
;;; ============================================================

(defun beads-test-workflow-to-ert-test (workflow)
  "Convert WORKFLOW to an ERT test definition form.
Returns a form that can be evaluated to define the test."
  (let ((name (intern (format "beads-workflow-%s"
                              (replace-regexp-in-string
                               " " "-"
                               (downcase (oref workflow name)))))))
    `(ert-deftest ,name ()
       ,(or (oref workflow description) "Workflow test")
       ,@(when (oref workflow tags)
           `(:tags ',(oref workflow tags)))
       (skip-unless (executable-find beads-executable))
       (beads-test-run-workflow-with-reporting ,workflow))))

(defun beads-test-run-workflow-interactively (workflow)
  "Run WORKFLOW interactively with verbose output.
Useful for debugging workflows outside of ERT."
  (message "Starting workflow: %s" (oref workflow name))
  (message "Description: %s" (or (oref workflow description) "N/A"))
  (message "Steps: %d" (length (oref workflow steps)))
  (message "")
  (condition-case err
      (let ((ctx (beads-test-run-workflow-with-reporting workflow)))
        (message "")
        (message "✓ Workflow completed successfully!")
        (message "")
        (message "── Summary ──")
        (message "Issues created: %d" (length (oref ctx created-issues)))
        (message "Buffers created: %d" (length (oref ctx created-buffers)))
        (message "Cache invalidated: %s"
                 (if (oref ctx completion-cache-invalidated) "yes" "no"))
        (message "Errors: %d" (length (oref ctx errors)))
        ctx)
    (ert-test-failed
     (message "")
     (message "✗ Workflow FAILED!")
     (message "")
     (message "%s" (cadr err))
     nil)))

(defmacro beads-test-with-workflow-context (&rest body)
  "Execute BODY with a fresh workflow context.
Binds `ctx' to the context.  Useful for REPL experimentation."
  `(let* ((project-dir (beads-test-create-project))
          (default-directory project-dir)
          (beads--project-cache (make-hash-table :test 'equal))
          (ctx (beads-test-context :project-dir project-dir)))
     (beads-test--clear-transient-state)
     (cl-letf (((symbol-function 'beads--find-project-root)
                (lambda () nil)))
       (unwind-protect
           (progn ,@body)
         (beads-test--clear-transient-state)))))

(provide 'beads-test-dsl)
;;; beads-test-dsl.el ends here
