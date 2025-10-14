;;; beads-misc.el --- Transient menus for misc bd commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; Provides transient menu interfaces for miscellaneous Beads commands:
;; - bd close: Close issues with reason
;; - bd dep: Dependency management (add, remove, tree, list)
;; - bd stats: Project statistics
;; - bd export: Export to JSONL
;; - bd import: Import from JSONL
;; - bd init: Initialize beads project
;;
;; All menus follow the patterns established in beads-create.el and
;; beads-update.el. They provide context-aware issue detection,
;; validation, and integration with beads-list and beads-show buffers.

;;; Code:

(require 'beads)
(require 'transient)

;;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-show--issue-id "beads-show")
(declare-function beads-refresh-show "beads-show")

;;; ============================================================
;;; bd close
;;; ============================================================

;;; Transient State Variables

(defvar beads-close--issue-id nil
  "Issue ID to close.")

(defvar beads-close--reason nil
  "Reason for closing the issue.")

;;; Utility Functions

(defun beads-close--reset-state ()
  "Reset close transient state."
  (setq beads-close--issue-id nil
        beads-close--reason nil))

(defun beads-close--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (and (fboundp 'beads-list--current-issue-id)
              (derived-mode-p 'beads-list-mode))
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (and (boundp 'beads-show--issue-id)
              (derived-mode-p 'beads-show-mode))
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N*)
   (when (string-match "\\*beads-show: \\(bd-[0-9]+\\)\\*"
                       (buffer-name))
     (match-string 1 (buffer-name)))))

(defun beads-close--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-close--validate-reason ()
  "Validate that reason is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-close--reason)
            (string-empty-p (string-trim beads-close--reason)))
    "Reason is required"))

(defun beads-close--validate-issue-id ()
  "Validate that issue ID is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-close--issue-id)
            (string-empty-p (string-trim beads-close--issue-id)))
    "Issue ID is required"))

(defun beads-close--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-close--validate-issue-id)
              (beads-close--validate-reason))))

;;; Infix Commands

(transient-define-infix beads-close--infix-reason ()
  "Set the reason for closing the issue."
  :class 'transient-option
  :description (lambda ()
                 (concat "Reason (--reason, required)"
                         (beads-close--format-value
                          beads-close--reason)))
  :key "r"
  :argument "reason="
  :prompt "Close reason: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((reason (read-string "Close reason: "
                                       beads-close--reason)))
              (setq beads-close--reason reason)
              reason)))

;;; Suffix Commands

(defun beads-close--execute (issue-id reason)
  "Execute bd close for ISSUE-ID with REASON."
  (condition-case err
      (let* ((result (beads--run-command "close" issue-id
                                         "--reason" reason))
             (issue (beads--parse-issue result)))
        (message "Closed issue: %s" issue-id)
        ;; Refresh buffers if auto-refresh is enabled
        (when beads-auto-refresh
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (cond
               ((and (fboundp 'beads-list-refresh)
                     (derived-mode-p 'beads-list-mode))
                (beads-list-refresh))
               ((and (fboundp 'beads-refresh-show)
                     (derived-mode-p 'beads-show-mode)
                     (boundp 'beads-show--issue-id)
                     (string= beads-show--issue-id issue-id))
                (beads-refresh-show))))))
        issue)
    (error
     (beads--error "Failed to close issue: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-close--execute-command ()
  "Execute the bd close command."
  :key "c"
  :description "Close issue"
  (interactive)
  (let ((errors (beads-close--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-close--execute beads-close--issue-id beads-close--reason)
      (beads-close--reset-state))))

(transient-define-suffix beads-close--reset ()
  "Reset all parameters."
  :key "R"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (setq beads-close--reason nil)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-close "beads-misc" nil t)
(transient-define-prefix beads-close (&optional issue-id)
  "Close an issue in Beads.

This transient menu provides an interface for closing issues with
a reason. It detects the issue ID from context (beads-list or
beads-show buffers) or prompts the user.

If ISSUE-ID is provided, use it directly."
  :value (lambda () nil)
  (interactive
   (list (or (beads-close--detect-issue-id)
             (completing-read
              "Close issue: "
              (lambda (string pred action)
                (if (eq action 'metadata)
                    '(metadata (category . beads-issue))
                  (complete-with-action
                   action
                   (mapcar (lambda (i) (alist-get 'id i))
                           (condition-case nil
                               (beads--parse-issues
                                (beads--run-command "list"))
                             (error nil)))
                   string pred)))
              nil t))))
  (beads-check-executable)
  (unless issue-id
    (user-error "No issue ID specified"))
  (setq beads-close--issue-id issue-id)
  ["Close Issue"
   ["Parameters"
    (beads-close--infix-reason)]]
  ["Actions"
   ("c" "Close issue" beads-close--execute-command)
   ("R" "Reset fields" beads-close--reset)
   ("q" "Quit" transient-quit-one)])

;;; ============================================================
;;; bd dep
;;; ============================================================

;;; Transient State Variables

(defvar beads-dep--from-issue nil
  "Source issue ID for dependency operations.")

(defvar beads-dep--to-issue nil
  "Target issue ID for dependency operations.")

(defvar beads-dep--dep-type nil
  "Dependency type (blocks, related, parent-child, discovered-from).")

;;; Utility Functions

(defun beads-dep--reset-state ()
  "Reset dep transient state."
  (setq beads-dep--from-issue nil
        beads-dep--to-issue nil
        beads-dep--dep-type nil))

(defun beads-dep--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-dep--validate-from-issue ()
  "Validate that from-issue is set."
  (when (or (null beads-dep--from-issue)
            (string-empty-p (string-trim beads-dep--from-issue)))
    "From issue ID is required"))

(defun beads-dep--validate-to-issue ()
  "Validate that to-issue is set."
  (when (or (null beads-dep--to-issue)
            (string-empty-p (string-trim beads-dep--to-issue)))
    "To issue ID is required"))

(defun beads-dep--validate-type ()
  "Validate that dep-type is valid."
  (when (and beads-dep--dep-type
             (not (member beads-dep--dep-type
                          '("blocks" "related" "parent-child"
                            "discovered-from"))))
    "Type must be: blocks, related, parent-child, or discovered-from"))

(defun beads-dep--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-dep--validate-from-issue)
              (beads-dep--validate-to-issue)
              (beads-dep--validate-type))))

(defun beads-dep--get-available-issues ()
  "Get list of available issue IDs."
  (condition-case nil
      (mapcar (lambda (i) (alist-get 'id i))
              (beads--parse-issues (beads--run-command "list")))
    (error nil)))

;;; Infix Commands

(transient-define-infix beads-dep--infix-from ()
  "Set the source issue ID."
  :class 'transient-option
  :description (lambda ()
                 (concat "From Issue"
                         (beads-dep--format-value
                          beads-dep--from-issue)))
  :key "f"
  :argument "from="
  :prompt "From issue: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (completing-read
                       "From issue: "
                       (beads-dep--get-available-issues)
                       nil nil beads-dep--from-issue)))
              (setq beads-dep--from-issue id)
              id)))

(transient-define-infix beads-dep--infix-to ()
  "Set the target issue ID."
  :class 'transient-option
  :description (lambda ()
                 (concat "To Issue"
                         (beads-dep--format-value
                          beads-dep--to-issue)))
  :key "t"
  :argument "to="
  :prompt "To issue: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (completing-read
                       "To issue: "
                       (beads-dep--get-available-issues)
                       nil nil beads-dep--to-issue)))
              (setq beads-dep--to-issue id)
              id)))

(transient-define-infix beads-dep--infix-type ()
  "Set the dependency type."
  :class 'transient-option
  :description (lambda ()
                 (concat "Type (--type)"
                         (beads-dep--format-value
                          beads-dep--dep-type)))
  :key "T"
  :argument "type="
  :prompt "Dependency type: "
  :choices '("blocks" "related" "parent-child" "discovered-from")
  :reader (lambda (_prompt _initial-input _history)
            (let ((type (completing-read
                         "Type: "
                         '("blocks" "related" "parent-child"
                           "discovered-from")
                         nil t beads-dep--dep-type)))
              (setq beads-dep--dep-type type)
              type)))

;;; Suffix Commands - Add

(defun beads-dep--execute-add (from-id to-id dep-type)
  "Execute bd dep add FROM-ID TO-ID with DEP-TYPE."
  (condition-case err
      (progn
        (beads--run-command "dep" "add" from-id to-id
                            "--type" dep-type)
        (message "Added dependency: %s -[%s]-> %s"
                 from-id dep-type to-id)
        nil)
    (error
     (beads--error "Failed to add dependency: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-dep--add-command ()
  "Execute bd dep add command."
  :key "a"
  :description "Add dependency"
  (interactive)
  (let ((errors (beads-dep--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-dep--execute-add beads-dep--from-issue
                              beads-dep--to-issue
                              beads-dep--dep-type)
      (beads-dep--reset-state))))

;;; Suffix Commands - Remove

(defun beads-dep--execute-remove (from-id to-id)
  "Execute bd dep remove FROM-ID TO-ID."
  (condition-case err
      (progn
        (beads--run-command "dep" "remove" from-id to-id)
        (message "Removed dependency: %s -> %s" from-id to-id)
        nil)
    (error
     (beads--error "Failed to remove dependency: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-dep--remove-command ()
  "Execute bd dep remove command."
  :key "r"
  :description "Remove dependency"
  (interactive)
  (let ((from-err (beads-dep--validate-from-issue))
        (to-err (beads-dep--validate-to-issue)))
    (when (or from-err to-err)
      (user-error "Validation failed: %s"
                  (string-join (delq nil (list from-err to-err)) "; ")))
    (when (y-or-n-p (format "Remove dependency %s -> %s? "
                            beads-dep--from-issue
                            beads-dep--to-issue))
      (beads-dep--execute-remove beads-dep--from-issue
                                 beads-dep--to-issue)
      (beads-dep--reset-state))))

;;; Suffix Commands - Tree

(defun beads-dep--execute-tree (issue-id)
  "Execute bd dep tree for ISSUE-ID and display in buffer."
  (condition-case err
      (let* ((output (with-temp-buffer
                       (let ((exit-code
                              (call-process beads-executable nil t nil
                                            "dep" "tree" issue-id)))
                         (unless (zerop exit-code)
                           (error "Command failed: %s" (buffer-string)))
                         (buffer-string))))
             (buf (get-buffer-create
                   (format "*beads-dep-tree: %s*" issue-id))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-dep--execute-tree issue-id)))))
        (display-buffer buf)
        (message "Dependency tree for %s" issue-id)
        nil)
    (error
     (beads--error "Failed to generate dependency tree: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-dep--tree-command ()
  "Show dependency tree for an issue."
  :key "t"
  :description "Show dependency tree"
  (interactive)
  (let* ((issue-id (or beads-dep--from-issue
                       (beads-close--detect-issue-id)
                       (completing-read
                        "Show tree for issue: "
                        (beads-dep--get-available-issues)
                        nil t))))
    (when (string-empty-p (string-trim issue-id))
      (user-error "No issue ID specified"))
    (beads-dep--execute-tree issue-id)))

;;; Suffix Commands - List

(defun beads-dep--execute-list (issue-id)
  "Execute bd dep list for ISSUE-ID and display in buffer."
  (condition-case err
      (let* ((result (beads--run-command "dep" "list" issue-id))
             (deps (if (vectorp result) (append result nil) nil))
             (buf (get-buffer-create
                   (format "*beads-dep-list: %s*" issue-id))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Dependencies for %s\n" issue-id))
            (insert "==============================\n\n")
            (if (null deps)
                (insert "No dependencies found.\n")
              (dolist (dep deps)
                (let ((from (alist-get 'from_issue dep))
                      (to (alist-get 'to_issue dep))
                      (type (alist-get 'dependency_type dep)))
                  (insert (format "%s -[%s]-> %s\n" from type to)))))
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-dep--execute-list issue-id)))))
        (display-buffer buf)
        (message "Dependencies for %s" issue-id)
        nil)
    (error
     (beads--error "Failed to list dependencies: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-dep--list-command ()
  "List dependencies for an issue."
  :key "l"
  :description "List dependencies"
  (interactive)
  (let* ((issue-id (or beads-dep--from-issue
                       (beads-close--detect-issue-id)
                       (completing-read
                        "List dependencies for issue: "
                        (beads-dep--get-available-issues)
                        nil t))))
    (when (string-empty-p (string-trim issue-id))
      (user-error "No issue ID specified"))
    (beads-dep--execute-list issue-id)))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-dep "beads-misc" nil t)
(transient-define-prefix beads-dep ()
  "Manage issue dependencies in Beads.

This transient menu provides an interface for managing dependencies
between issues. It supports adding, removing, viewing trees, and
listing dependencies."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ;; Try to set from-issue from context
  (unless beads-dep--from-issue
    (setq beads-dep--from-issue (beads-close--detect-issue-id)))
  ["Dependency Parameters"
   ["Issues"
    (beads-dep--infix-from)
    (beads-dep--infix-to)]
   ["Type"
    (beads-dep--infix-type)]]
  ["Actions"
   ["Modify"
    ("a" "Add dependency" beads-dep--add-command)
    ("r" "Remove dependency" beads-dep--remove-command)]
   ["View"
    ("t" "Show tree" beads-dep--tree-command)
    ("l" "List dependencies" beads-dep--list-command)]
   ["Other"
    ("R" "Reset fields" beads-dep--reset)
    ("q" "Quit" transient-quit-one)]])

(transient-define-suffix beads-dep--reset ()
  "Reset all dependency parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-dep--reset-state)
    (message "Fields reset")))

;;; ============================================================
;;; bd stats
;;; ============================================================

(defun beads-stats--execute ()
  "Execute bd stats and display in buffer."
  (condition-case err
      (let* ((output (with-temp-buffer
                       (let ((exit-code
                              (call-process beads-executable nil t nil
                                            "stats")))
                         (unless (zerop exit-code)
                           (error "Command failed: %s" (buffer-string)))
                         (buffer-string))))
             (buf (get-buffer-create "*beads-stats*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-stats--execute)))))
        (display-buffer buf)
        (message "Project statistics")
        nil)
    (error
     (beads--error "Failed to get statistics: %s"
                   (error-message-string err)))))

;;;###autoload
(defun beads-stats ()
  "Show project statistics."
  (interactive)
  (beads-check-executable)
  (beads-stats--execute))

;;; ============================================================
;;; bd export
;;; ============================================================

;;; Transient State Variables

(defvar beads-export--output nil
  "Output file path for export.")

(defvar beads-export--no-auto-flush nil
  "Whether to disable auto-flush.")

;;; Utility Functions

(defun beads-export--reset-state ()
  "Reset export transient state."
  (setq beads-export--output nil
        beads-export--no-auto-flush nil))

(defun beads-export--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-export--get-default-output ()
  "Get default output path (.beads/issues.jsonl)."
  (when-let ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

;;; Infix Commands

(transient-define-infix beads-export--infix-output ()
  "Set the output file path."
  :class 'transient-option
  :description (lambda ()
                 (concat "Output (--output)"
                         (beads-export--format-value
                          (or beads-export--output
                              (beads-export--get-default-output)))))
  :key "o"
  :argument "output="
  :prompt "Output file: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((path (read-file-name
                         "Output file: "
                         nil
                         (or beads-export--output
                             (beads-export--get-default-output)))))
              (setq beads-export--output path)
              path)))

(transient-define-infix beads-export--infix-no-auto-flush ()
  "Toggle no-auto-flush flag."
  :class 'transient-switch
  :description "Disable auto-flush (--no-auto-flush)"
  :key "n"
  :argument "--no-auto-flush"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-export--no-auto-flush
                  (not beads-export--no-auto-flush))
            beads-export--no-auto-flush))

;;; Suffix Commands

(defun beads-export--execute (output-path no-auto-flush)
  "Execute bd export to OUTPUT-PATH with NO-AUTO-FLUSH flag."
  (condition-case err
      (let ((args (list "export" "-o" output-path)))
        (when no-auto-flush
          (setq args (append args (list "--no-auto-flush"))))
        (with-temp-buffer
          (let ((exit-code (apply #'call-process
                                  beads-executable nil t nil args)))
            (unless (zerop exit-code)
              (error "Export failed: %s" (buffer-string)))))
        (message "Exported to: %s" output-path)
        nil)
    (error
     (beads--error "Failed to export: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-export--execute-command ()
  "Execute the bd export command."
  :key "e"
  :description "Export to JSONL"
  (interactive)
  (let ((output (or beads-export--output
                    (beads-export--get-default-output))))
    (when (or (null output) (string-empty-p (string-trim output)))
      (user-error "Output path is required"))
    (beads-export--execute output beads-export--no-auto-flush)
    (beads-export--reset-state)))

(transient-define-suffix beads-export--reset ()
  "Reset all export parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-export--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-export "beads-misc" nil t)
(transient-define-prefix beads-export ()
  "Export issues to JSONL.

This transient menu provides an interface for exporting issues to
JSONL format. The default output is .beads/issues.jsonl."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ["Export Parameters"
   (beads-export--infix-output)
   (beads-export--infix-no-auto-flush)]
  ["Actions"
   ("e" "Export" beads-export--execute-command)
   ("R" "Reset fields" beads-export--reset)
   ("q" "Quit" transient-quit-one)])

;;; ============================================================
;;; bd import
;;; ============================================================

;;; Transient State Variables

(defvar beads-import--input nil
  "Input file path for import.")

(defvar beads-import--dry-run nil
  "Whether to run in dry-run mode.")

(defvar beads-import--resolve-collisions nil
  "Whether to auto-resolve collisions.")

;;; Utility Functions

(defun beads-import--reset-state ()
  "Reset import transient state."
  (setq beads-import--input nil
        beads-import--dry-run nil
        beads-import--resolve-collisions nil))

(defun beads-import--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-import--get-default-input ()
  "Get default input path (.beads/issues.jsonl)."
  (when-let ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-import--validate-input ()
  "Validate that input path is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-import--input)
            (string-empty-p (string-trim beads-import--input)))
    "Input file path is required"))

(defun beads-import--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil (list (beads-import--validate-input))))

;;; Infix Commands

(transient-define-infix beads-import--infix-input ()
  "Set the input file path."
  :class 'transient-option
  :description (lambda ()
                 (concat "Input (--input, required)"
                         (beads-import--format-value
                          beads-import--input)))
  :key "i"
  :argument "input="
  :prompt "Input file: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((path (read-file-name
                         "Input file: "
                         nil
                         (or beads-import--input
                             (beads-import--get-default-input))
                         t)))
              (setq beads-import--input path)
              path)))

(transient-define-infix beads-import--infix-dry-run ()
  "Toggle dry-run flag."
  :class 'transient-switch
  :description "Dry run (--dry-run)"
  :key "d"
  :argument "--dry-run"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-import--dry-run
                  (not beads-import--dry-run))
            beads-import--dry-run))

(transient-define-infix beads-import--infix-resolve-collisions ()
  "Toggle resolve-collisions flag."
  :class 'transient-switch
  :description "Resolve collisions (--resolve-collisions)"
  :key "r"
  :argument "--resolve-collisions"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-import--resolve-collisions
                  (not beads-import--resolve-collisions))
            beads-import--resolve-collisions))

;;; Suffix Commands

(defun beads-import--execute (input-path dry-run resolve-collisions)
  "Execute bd import from INPUT-PATH with flags.
DRY-RUN and RESOLVE-COLLISIONS control behavior."
  (condition-case err
      (let ((args (list "import" "-i" input-path)))
        (when dry-run
          (setq args (append args (list "--dry-run"))))
        (when resolve-collisions
          (setq args (append args (list "--resolve-collisions"))))
        (let ((output (with-temp-buffer
                        (let ((exit-code
                               (apply #'call-process
                                      beads-executable nil t nil args)))
                          (unless (zerop exit-code)
                            (error "Import failed: %s" (buffer-string)))
                          (buffer-string)))))
          ;; Display output in buffer if dry-run or there are collisions
          (when (or dry-run
                    (string-match-p "collision" output))
            (let ((buf (get-buffer-create "*beads-import*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert output)
                  (goto-char (point-min))
                  (special-mode)
                  (local-set-key (kbd "q") 'quit-window)))
              (display-buffer buf)))
          (if dry-run
              (message "Dry run completed (see *beads-import* buffer)")
            (message "Import completed from: %s" input-path))
          nil))
    (error
     (beads--error "Failed to import: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-import--execute-command ()
  "Execute the bd import command."
  :key "i"
  :description "Import from JSONL"
  (interactive)
  (let ((errors (beads-import--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-import--execute beads-import--input
                             beads-import--dry-run
                             beads-import--resolve-collisions)
      (unless beads-import--dry-run
        (beads-import--reset-state)))))

(transient-define-suffix beads-import--reset ()
  "Reset all import parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-import--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-import "beads-misc" nil t)
(transient-define-prefix beads-import ()
  "Import issues from JSONL.

This transient menu provides an interface for importing issues from
JSONL format. It supports dry-run mode for previewing changes and
automatic collision resolution for branch merges."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ["Import Parameters"
   (beads-import--infix-input)]
  ["Options"
   (beads-import--infix-dry-run)
   (beads-import--infix-resolve-collisions)]
  ["Actions"
   ("i" "Import" beads-import--execute-command)
   ("R" "Reset fields" beads-import--reset)
   ("q" "Quit" transient-quit-one)])

;;; ============================================================
;;; bd init
;;; ============================================================

;;; Transient State Variables

(defvar beads-init--prefix nil
  "Issue ID prefix for new project.")

(defvar beads-init--db-path nil
  "Database path for new project.")

;;; Utility Functions

(defun beads-init--reset-state ()
  "Reset init transient state."
  (setq beads-init--prefix nil
        beads-init--db-path nil))

(defun beads-init--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

;;; Infix Commands

(transient-define-infix beads-init--infix-prefix ()
  "Set the issue ID prefix."
  :class 'transient-option
  :description (lambda ()
                 (concat "Prefix (--prefix)"
                         (beads-init--format-value
                          beads-init--prefix)))
  :key "p"
  :argument "prefix="
  :prompt "Issue ID prefix: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((prefix (read-string "Issue ID prefix (e.g., bd): "
                                       beads-init--prefix)))
              (setq beads-init--prefix prefix)
              prefix)))

(transient-define-infix beads-init--infix-db ()
  "Set the database path."
  :class 'transient-option
  :description (lambda ()
                 (concat "Database (--db)"
                         (beads-init--format-value
                          beads-init--db-path)))
  :key "d"
  :argument "db="
  :prompt "Database path: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((path (read-file-name
                         "Database path: "
                         nil
                         beads-init--db-path)))
              (setq beads-init--db-path path)
              path)))

;;; Suffix Commands

(defun beads-init--execute (prefix db-path)
  "Execute bd init with PREFIX and DB-PATH."
  (condition-case err
      (let ((args (list "init")))
        (when (and prefix (not (string-empty-p (string-trim prefix))))
          (setq args (append args (list "--prefix" prefix))))
        (when (and db-path (not (string-empty-p (string-trim db-path))))
          (setq args (append args (list "--db" db-path))))
        (with-temp-buffer
          (let ((exit-code (apply #'call-process
                                  beads-executable nil t nil args)))
            (unless (zerop exit-code)
              (error "Init failed: %s" (buffer-string)))))
        (message "Beads project initialized%s"
                 (if prefix (format " with prefix '%s'" prefix) ""))
        nil)
    (error
     (beads--error "Failed to initialize: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-init--execute-command ()
  "Execute the bd init command."
  :key "i"
  :description "Initialize project"
  (interactive)
  (when (y-or-n-p "Initialize Beads project in current directory? ")
    (beads-init--execute beads-init--prefix beads-init--db-path)
    (beads-init--reset-state)))

(transient-define-suffix beads-init--reset ()
  "Reset all init parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-init--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-init "beads-misc" nil t)
(transient-define-prefix beads-init ()
  "Initialize a new Beads project.

This transient menu provides an interface for initializing a new
Beads project in the current directory. It allows setting the
issue ID prefix and database path."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ["Initialization Parameters"
   (beads-init--infix-prefix)
   (beads-init--infix-db)]
  ["Actions"
   ("i" "Initialize" beads-init--execute-command)
   ("R" "Reset fields" beads-init--reset)
   ("q" "Quit" transient-quit-one)])

;;; Footer

(provide 'beads-misc)
;;; beads-misc.el ends here
