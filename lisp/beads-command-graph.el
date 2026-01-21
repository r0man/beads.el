;;; beads-command-graph.el --- Graph command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command class for `bd graph' operation.
;; Graph displays a visualization of issue dependency graphs.

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-command-dep)
(require 'beads-command-list)
(require 'beads-completion)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-graph
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-graph (beads-command-json)
    ((issue-id
      :initarg :issue-id
      :type (or null string)
      :initform nil
      :documentation "Issue ID to show graph for."
      :positional 1)
     (all
      :initarg :all
      :type boolean
      :initform nil
      :documentation "Show graph for all open issues."
      :long-option "all"
      :option-type :boolean
      :key "a"
      :transient "--all"
      :class transient-switch
      :argument "--all"
      :transient-group "Options"
      :level 1
      :order 1)
     (box
      :initarg :box
      :type boolean
      :initform t
      :documentation "ASCII boxes showing layers (default)."
      :long-option "box"
      :option-type :boolean
      :key "b"
      :transient "--box"
      :class transient-switch
      :argument "--box"
      :transient-group "Display"
      :level 1
      :order 2)
     (compact
      :initarg :compact
      :type boolean
      :initform nil
      :documentation "Tree format, one line per issue."
      :long-option "compact"
      :option-type :boolean
      :key "c"
      :transient "--compact"
      :class transient-switch
      :argument "--compact"
      :transient-group "Display"
      :level 1
      :order 3))
    :documentation "Represents bd graph command.
  Displays issue dependency graph visualization."))

(cl-defmethod beads-command-subcommand ((_command beads-command-graph))
  "Return \"graph\" as the CLI subcommand."
  "graph")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-graph))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menu

;;;###autoload (autoload 'beads-graph-transient "beads-command-graph" nil t)
(beads-meta-define-transient beads-command-graph "beads-graph-transient"
  "Display issue dependency graph.

For epics, shows all children and their dependencies.
For regular issues, shows direct dependencies.
With --all, shows all open issues grouped by component.

Display formats:
  --box (default): ASCII boxes showing layers
  --compact: Tree format, one line per issue

Status icons: open in_progress blocked closed deferred"
  beads-option-global-section)

;;; ============================================================
;;; Graphviz-based Graph Visualization
;;; ============================================================

;;; Customization

(defgroup beads-graph nil
  "Visual dependency graph for Beads issues."
  :group 'beads
  :prefix "beads-graph-")

(defcustom beads-graph-dot-executable "dot"
  "Path to the graphviz dot executable."
  :type 'string
  :group 'beads-graph)

(defcustom beads-graph-default-format "svg"
  "Default output format for graphs (svg, png, pdf)."
  :type '(choice (const "svg")
                 (const "png")
                 (const "pdf"))
  :group 'beads-graph)

(defcustom beads-graph-layout "dot"
  "Graphviz layout engine (dot, neato, fdp, sfdp, circo, twopi)."
  :type '(choice (const "dot")
                 (const "neato")
                 (const "fdp")
                 (const "sfdp")
                 (const "circo")
                 (const "twopi"))
  :group 'beads-graph)

(defcustom beads-graph-label-max-length 30
  "Maximum length for issue labels in graph nodes."
  :type 'integer
  :group 'beads-graph)

;;; Variables

(defvar-local beads-graph--filter-status nil
  "Current status filter for graph.")

(defvar-local beads-graph--filter-priority nil
  "Current priority filter for graph.")

(defvar-local beads-graph--filter-type nil
  "Current issue type filter for graph.")

(defvar-local beads-graph--root-issue nil
  "Root issue for focused graph view.")

;;; Utilities

(defun beads-graph--check-dot ()
  "Check if dot executable is available."
  (unless (executable-find beads-graph-dot-executable)
    (user-error "Graphviz 'dot' command not found.  Please install graphviz")))

(defun beads-graph--issue-color (issue)
  "Return color for ISSUE based on status.
ISSUE is a beads-issue EIEIO object."
  (pcase (oref issue status)
    ("open" "lightblue")
    ("in_progress" "yellow")
    ("blocked" "red")
    ("closed" "lightgray")
    (_ "white")))

(defun beads-graph--issue-shape (issue)
  "Return shape for ISSUE based on type.
ISSUE is a beads-issue EIEIO object."
  (pcase (oref issue issue-type)
    ("epic" "box3d")
    ("feature" "box")
    ("bug" "octagon")
    ("task" "ellipse")
    ("chore" "note")
    (_ "box")))

(defun beads-graph--issue-label (issue)
  "Create label for ISSUE in graph.
ISSUE is a beads-issue EIEIO object."
  (let ((id (oref issue id))
        (title (oref issue title))
        (status (oref issue status))
        (priority (oref issue priority)))
    (format "%s\\n%s\\n[P%s %s]"
            id
            (if (> (length title) beads-graph-label-max-length)
                (concat (substring title 0
                                   (- beads-graph-label-max-length 3))
                        "...")
              title)
            priority
            status)))

(defun beads-graph--filter-issue (issue)
  "Return non-nil if ISSUE passes current filters.
ISSUE is a beads-issue EIEIO object."
  (and (or (null beads-graph--filter-status)
           (string= (oref issue status) beads-graph--filter-status))
       (or (null beads-graph--filter-priority)
           (equal (oref issue priority) beads-graph--filter-priority))
       (or (null beads-graph--filter-type)
           (string= (oref issue issue-type) beads-graph--filter-type))))

(defun beads-graph--get-dependencies ()
  "Get all dependencies from all issues."
  (let ((issues (beads-command-list!))
        (deps nil))
    (dolist (issue issues)
      (let ((issue-id (oref issue id)))
        ;; Get dependencies for this issue using EIEIO command class
        ;; Use text output (json nil) to preserve parsing logic
        (condition-case nil
            (let* ((cmd (beads-command-dep-list :issue-id issue-id :json nil))
                   (_ (beads-command-execute cmd))
                   (output (oref cmd stdout))
                   (lines (split-string output "\n" t)))
              (dolist (line lines)
                (when (string-match
                       "\\([a-zA-Z0-9._-]+-[a-zA-Z0-9]+\\) -\\[\\([^]]+\\)\\]-> \
\\([a-zA-Z0-9._-]+-[a-zA-Z0-9]+\\)" line)
                  (push (list :from (match-string 1 line)
                              :to (match-string 3 line)
                              :type (match-string 2 line))
                        deps))))
          (error nil))))
    (nreverse deps)))

(defun beads-graph--generate-dot (issues deps)
  "Generate DOT format graph from ISSUES and DEPS.
ISSUES is a list of beads-issue EIEIO objects.
DEPS is a list of plists with :from, :to, and :type keys."
  (with-temp-buffer
    (insert "digraph beads {\n")
    (insert "  rankdir=TB;\n")
    (insert "  node [style=filled];\n")
    (insert "  \n")

    ;; Generate nodes
    (dolist (issue issues)
      (when (beads-graph--filter-issue issue)
        (let ((id (oref issue id))
              (label (beads-graph--issue-label issue))
              (color (beads-graph--issue-color issue))
              (shape (beads-graph--issue-shape issue)))
          (insert (format "  \"%s\" [label=\"%s\", fillcolor=\"%s\", \
shape=%s];\n"
                          id label color shape)))))

    (insert "  \n")

    ;; Generate edges
    (dolist (dep deps)
      (let ((from (plist-get dep :from))
            (to (plist-get dep :to))
            (type (plist-get dep :type)))
        ;; Only include edge if both nodes are in filtered issues
        (when (and (seq-find (lambda (i) (string= (oref i id) from)) issues)
                   (seq-find (lambda (i) (string= (oref i id) to)) issues)
                   (beads-graph--filter-issue
                    (seq-find (lambda (i) (string= (oref i id) from)) issues))
                   (beads-graph--filter-issue
                    (seq-find (lambda (i) (string= (oref i id) to)) issues)))
          (let ((style (pcase type
                         ("blocks" "solid")
                         ("related" "dashed")
                         ("parent-child" "bold")
                         ("discovered-from" "dotted")
                         (_ "solid")))
                (color (pcase type
                         ("blocks" "red")
                         ("related" "blue")
                         ("parent-child" "green")
                         ("discovered-from" "gray")
                         (_ "black"))))
            (insert (format "  \"%s\" -> \"%s\" [style=%s, color=\"%s\", \
label=\"%s\"];\n"
                            from to style color type))))))

    (insert "}\n")
    (buffer-string)))

(defun beads-graph--render-dot (dot-string format)
  "Render DOT-STRING to FORMAT using graphviz.
Returns the path to the generated image file."
  (let ((dot-file (make-temp-file "beads-graph-" nil ".dot"))
        (out-file (make-temp-file "beads-graph-" nil (concat "." format))))
    (with-temp-file dot-file
      (insert dot-string))
    (let ((exit-code (call-process beads-graph-dot-executable nil nil nil
                                  (concat "-T" format)
                                  "-K" beads-graph-layout
                                  dot-file
                                  "-o" out-file)))
      (delete-file dot-file)
      (if (zerop exit-code)
          out-file
        (delete-file out-file)
        (error "Failed to render graph with dot")))))

;;; Commands

(defun beads-graph--display-image (image-file)
  "Display IMAGE-FILE in a buffer."
  (let ((buffer (get-buffer-create (beads-buffer-name-utility "graph"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (beads-graph-mode)
        (insert-image (create-image image-file))
        (insert "\n")
        (goto-char (point-min))))
    (display-buffer buffer)))

(defun beads-graph-refresh ()
  "Refresh the current graph."
  (interactive)
  (if beads-graph--root-issue
      (beads-graph-issue beads-graph--root-issue)
    (beads-graph-all)))

(defun beads-graph-export ()
  "Export current graph to a file."
  (interactive)
  (let* ((format (completing-read "Export format: "
                                 '("svg" "png" "pdf" "dot")
                                 nil t nil nil beads-graph-default-format))
         (default-name (format "beads-graph.%s" format))
         (file (read-file-name "Export to: " nil nil nil default-name)))
    (beads-graph--check-dot)
    (let* ((issues (beads-command-list!))
           (deps (beads-graph--get-dependencies))
           (dot (beads-graph--generate-dot issues deps)))
      (if (string= format "dot")
          (with-temp-file file
            (insert dot))
        (let ((temp-dot (make-temp-file "beads-export-" nil ".dot")))
          (with-temp-file temp-dot
            (insert dot))
          (let ((exit-code (call-process beads-graph-dot-executable nil nil nil
                                        (concat "-T" format)
                                        "-K" beads-graph-layout
                                        temp-dot
                                        "-o" file)))
            (delete-file temp-dot)
            (if (zerop exit-code)
                (message "Graph exported to: %s" file)
              (error "Failed to export graph")))))
      file)))

(defun beads-graph-filter ()
  "Apply filters to graph."
  (interactive)
  (let ((filter-type (completing-read "Filter by (empty to skip): "
                                     '("status" "priority" "type" "clear-all")
                                     nil t)))
    (pcase filter-type
      ("status"
       (setq beads-graph--filter-status
             (let ((s (completing-read "Status (empty to clear): "
                                      '("open" "in_progress" "blocked" "closed")
                                      nil t)))
               (if (string-empty-p s) nil s))))
      ("priority"
       (setq beads-graph--filter-priority
             (let ((p (completing-read "Priority (empty to clear): "
                                      '("0" "1" "2" "3" "4")
                                      nil t)))
               (if (string-empty-p p) nil (string-to-number p)))))
      ("type"
       (setq beads-graph--filter-type
             (let ((typ (completing-read "Type (empty to clear): "
                                      '("bug" "feature" "task" "epic" "chore")
                                      nil t)))
               (if (string-empty-p typ) nil typ))))
      ("clear-all"
       (setq beads-graph--filter-status nil
             beads-graph--filter-priority nil
             beads-graph--filter-type nil)))
    (beads-graph-refresh)
    (message "Filters applied: status=%s priority=%s type=%s"
             (or beads-graph--filter-status "none")
             (or beads-graph--filter-priority "none")
             (or beads-graph--filter-type "none"))))

;;;###autoload
(defun beads-graph-all ()
  "Show dependency graph for all issues using Graphviz."
  (interactive)
  (beads-check-executable)
  (beads-graph--check-dot)
  (message "Generating graph...")
  (let* ((issues (beads-command-list!))
         (deps (beads-graph--get-dependencies))
         (dot (beads-graph--generate-dot issues deps))
         (image-file (beads-graph--render-dot dot beads-graph-default-format)))
    (beads-graph--display-image image-file)
    (setq beads-graph--root-issue nil)
    (message "Graph generated (%d issues, %d dependencies)"
             (length issues) (length deps))))

;;;###autoload
(defun beads-graph-issue (issue-id)
  "Show dependency graph focused on ISSUE-ID using Graphviz."
  (interactive
   (list (beads-completion-read-issue "Graph for issue: "
                         nil t nil 'beads--issue-id-history)))
  (beads-check-executable)
  (beads-graph--check-dot)
  (message "Generating graph for %s..." issue-id)
  (let* ((all-issues (beads-command-list!))
         (all-deps (beads-graph--get-dependencies))
         ;; Find connected issues (simplified - includes all for now)
         (issues all-issues)
         (deps all-deps)
         (dot (beads-graph--generate-dot issues deps))
         (image-file (beads-graph--render-dot dot beads-graph-default-format)))
    (beads-graph--display-image image-file)
    (setq beads-graph--root-issue issue-id)
    (message "Graph generated for %s" issue-id)))

;;; Mode Definition

(defvar beads-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'beads-graph-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "f") #'beads-graph-filter)
    (define-key map (kbd "c") #'beads-graph-filter)  ; clear/filter
    (define-key map (kbd "e") #'beads-graph-export)
    map)
  "Keymap for `beads-graph-mode'.")

(define-derived-mode beads-graph-mode special-mode "Beads-Graph"
  "Major mode for displaying Beads dependency graphs.

\\{beads-graph-mode-map}"
  (setq buffer-read-only t))

(provide 'beads-command-graph)
;;; beads-command-graph.el ends here
