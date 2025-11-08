;;; beads-misc.el --- Transient menus for misc bd commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

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
;; beads-update.el.  They provide context-aware issue detection,
;; validation, and integration with beads-list and beads-show buffers.

;;; Code:

(require 'beads)
(require 'beads-option)
(require 'transient)

;;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-show--issue-id "beads-show")
(declare-function beads-refresh-show "beads-show")

;;; ============================================================
;;; bd dep
;;; ============================================================

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
between issues.  It supports adding, removing, viewing trees, and
listing dependencies."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ;; Try to set from-issue from context
  (unless beads-dep--from-issue
    (setq beads-dep--from-issue (beads-close--detect-issue-id)))
  ["Dependency Parameters"
   ["Issues"
    (beads-option-dep-from)
    (beads-option-dep-to)]
   ["Type"
    (beads-option-dep-type)]]
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
;;; bd quickstart
;;; ============================================================

(defun beads-quickstart--execute ()
  "Execute bd quickstart and display in buffer."
  (condition-case err
      (let* ((output (with-temp-buffer
                       (let ((exit-code
                              (call-process beads-executable nil t nil
                                            "quickstart")))
                         (unless (zerop exit-code)
                           (error "Command failed: %s" (buffer-string)))
                         (buffer-string))))
             (buf (get-buffer-create "*beads-quickstart*")))
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
                             (beads-quickstart--execute)))
            ;; Make the buffer more readable
            (visual-line-mode 1)
            (setq header-line-format
                  "Beads Quick Start Guide - Press 'q' to quit")))
        (display-buffer buf)
        (message "Beads quickstart guide")
        nil)
    (error
     (beads--error "Failed to get quickstart guide: %s"
                   (error-message-string err)))))

;;;###autoload
(defun beads-quickstart ()
  "Show Beads quickstart guide."
  (interactive)
  (beads-check-executable)
  (beads-quickstart--execute))

;;; ============================================================
;;; bd export
;;; ============================================================

;;; Utility Functions

(defun beads-export--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-export--get-default-output ()
  "Get default output path (.beads/issues.jsonl)."
  (when-let* ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-export--parse-transient-args (args)
  "Parse transient ARGS and return plist with export parameters.
Returns (:output OUTPUT :no-auto-flush NO-AUTO-FLUSH)."
  (list :output (transient-arg-value "--output=" args)
        :no-auto-flush (transient-arg-value "--no-auto-flush" args)))

;;; Suffix Commands

(defun beads-export--execute (output-path no-auto-flush)
  "Execute bd export to OUTPUT-PATH with NO-AUTO-FLUSH flag."
  (condition-case err
      (let (args)
        ;; Build args in reverse order for push/nreverse
        (when no-auto-flush
          (push "--no-auto-flush" args))
        (push output-path args)
        (push "-o" args)
        (push "export" args)
        (setq args (nreverse args))
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
  (let* ((args (transient-args 'beads-export--menu))
         (params (beads-export--parse-transient-args args))
         (output (or (plist-get params :output)
                     (beads-export--get-default-output)))
         (no-auto-flush (plist-get params :no-auto-flush)))
    (when (or (null output) (string-empty-p (string-trim output)))
      (user-error "Output path is required"))
    (beads-export--execute output no-auto-flush)))

(transient-define-suffix beads-export--reset ()
  "Reset all export parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

(transient-define-prefix beads-export--menu ()
  "Transient menu for exporting issues to JSONL."
  :value (lambda () nil)
  ["Export Parameters"
   (beads-option-export-output)
   (beads-option-export-no-auto-flush)]
  ["Actions"
   ("e" "Export" beads-export--execute-command)
   ("R" "Reset fields" beads-export--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-export ()
  "Export issues to JSONL.

This command provides an interface for exporting issues to
JSONL format.  The default output is .beads/issues.jsonl."
  (interactive)
  (beads-check-executable)
  (call-interactively #'beads-export--menu))

;;; ============================================================
;;; bd import
;;; ============================================================

;;; Utility Functions

(defun beads-import--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-import--get-default-input ()
  "Get default input path (.beads/issues.jsonl)."
  (when-let* ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-import--parse-transient-args (args)
  "Parse transient ARGS and return plist with import parameters.
Returns (:input INPUT :dry-run DRY-RUN :resolve-collisions RESOLVE-COLLISIONS)."
  (list :input (transient-arg-value "--input=" args)
        :dry-run (transient-arg-value "--dry-run" args)
        :resolve-collisions (transient-arg-value "--resolve-collisions" args)))

(defun beads-import--validate-input (input)
  "Validate that INPUT path is set.
Returns error message string if invalid, nil if valid."
  (when (or (null input)
            (string-empty-p (string-trim input)))
    "Input file path is required"))

(defun beads-import--validate-all (input)
  "Validate all parameters with INPUT.
Returns list of error messages, or nil if all valid."
  (delq nil (list (beads-import--validate-input input))))

;;; Suffix Commands

(defun beads-import--execute (input-path dry-run resolve-collisions)
  "Execute bd import from INPUT-PATH with flags.
DRY-RUN and RESOLVE-COLLISIONS control behavior."
  (condition-case err
      (let (args)
        ;; Build args in reverse order for push/nreverse
        (when resolve-collisions
          (push "--resolve-collisions" args))
        (when dry-run
          (push "--dry-run" args))
        (push input-path args)
        (push "-i" args)
        (push "import" args)
        (setq args (nreverse args))
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
  (let* ((args (transient-args 'beads-import--menu))
         (params (beads-import--parse-transient-args args))
         (input (plist-get params :input))
         (dry-run (plist-get params :dry-run))
         (resolve-collisions (plist-get params :resolve-collisions))
         (errors (beads-import--validate-all input)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-import--execute input dry-run resolve-collisions))))

(transient-define-suffix beads-import--reset ()
  "Reset all import parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

(transient-define-prefix beads-import--menu ()
  "Transient menu for importing issues from JSONL."
  :value (lambda () nil)
  ["Import Parameters"
   (beads-option-import-input)]
  ["Options"
   (beads-option-import-dry-run)
   (beads-option-import-resolve-collisions)]
  ["Actions"
   ("i" "Import" beads-import--execute-command)
   ("R" "Reset fields" beads-import--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-import ()
  "Import issues from JSONL.

This command provides an interface for importing issues from
JSONL format.  It supports dry-run mode for previewing changes and
automatic collision resolution for branch merges."
  (interactive)
  (beads-check-executable)
  (call-interactively #'beads-import--menu))

;;; ============================================================
;;; bd init
;;; ============================================================

;;; Utility Functions

(defun beads-init--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-init--parse-transient-args (args)
  "Parse transient ARGS and return plist with init parameters.
Returns (:prefix PREFIX :db-path DB-PATH)."
  (list :prefix (transient-arg-value "--prefix=" args)
        :db-path (transient-arg-value "--db=" args)))

;;; Suffix Commands

(defun beads-init--execute (prefix db-path)
  "Execute bd init with PREFIX and DB-PATH."
  (condition-case err
      (let (args)
        ;; Build args in reverse order for push/nreverse
        (when db-path
          (let ((trimmed (string-trim db-path)))
            (unless (string-empty-p trimmed)
              (push trimmed args)
              (push "--db" args))))
        (when prefix
          (let ((trimmed (string-trim prefix)))
            (unless (string-empty-p trimmed)
              (push trimmed args)
              (push "--prefix" args))))
        (push "init" args)
        (setq args (nreverse args))
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
    (let* ((args (transient-args 'beads-init--menu))
           (params (beads-init--parse-transient-args args))
           (prefix (plist-get params :prefix))
           (db-path (plist-get params :db-path)))
      (beads-init--execute prefix db-path))))

(transient-define-suffix beads-init--reset ()
  "Reset all init parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

(transient-define-prefix beads-init--menu ()
  "Transient menu for initializing a new Beads project."
  :value (lambda () nil)
  ["Initialization Parameters"
   (beads-option-init-prefix)
   (beads-option-init-db)]
  ["Actions"
   ("i" "Initialize" beads-init--execute-command)
   ("R" "Reset fields" beads-init--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-init ()
  "Initialize a new Beads project.

This command provides an interface for initializing a new
Beads project in the current directory.  It allows setting the
issue ID prefix and database path."
  (interactive)
  (beads-check-executable)
  (call-interactively #'beads-init--menu))

;;; Footer

(provide 'beads-misc)
;;; beads-misc.el ends here
