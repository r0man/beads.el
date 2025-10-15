;;; beads-graph.el --- Dependency graph visualization for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, project, issues

;;; Commentary:

;; beads-graph.el provides visual dependency graph generation using
;; Graphviz dot.  Generates interactive graphs showing issue
;; dependencies with support for filtering and navigation.
;;
;; Commands:
;;   M-x beads-graph-all      ; Show graph of all issues
;;   M-x beads-graph-issue    ; Show graph for specific issue
;;
;; Requirements:
;;   - graphviz (dot command) must be installed
;;
;; Key bindings in beads-graph-mode:
;;   RET     - Jump to issue at point
;;   g       - Refresh graph
;;   f       - Apply filters
;;   c       - Clear filters
;;   e       - Export graph to file
;;   q       - Quit

;;; Code:

(require 'beads)

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
    (user-error "Graphviz 'dot' command not found. Please install graphviz")))

(defun beads-graph--issue-color (issue)
  "Return color for ISSUE based on status."
  (pcase (alist-get 'status issue)
    ("open" "lightblue")
    ("in_progress" "yellow")
    ("blocked" "red")
    ("closed" "lightgray")
    (_ "white")))

(defun beads-graph--issue-shape (issue)
  "Return shape for ISSUE based on type."
  (pcase (alist-get 'issue-type issue)
    ("epic" "box3d")
    ("feature" "box")
    ("bug" "octagon")
    ("task" "ellipse")
    ("chore" "note")
    (_ "box")))

(defun beads-graph--issue-label (issue)
  "Create label for ISSUE in graph."
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue))
        (status (alist-get 'status issue))
        (priority (alist-get 'priority issue)))
    (format "%s\\n%s\\n[P%s %s]"
            id
            (if (> (length title) 30)
                (concat (substring title 0 27) "...")
              title)
            priority
            status)))

(defun beads-graph--filter-issue (issue)
  "Return non-nil if ISSUE passes current filters."
  (and (or (null beads-graph--filter-status)
           (string= (alist-get 'status issue) beads-graph--filter-status))
       (or (null beads-graph--filter-priority)
           (equal (alist-get 'priority issue) beads-graph--filter-priority))
       (or (null beads-graph--filter-type)
           (string= (alist-get 'issue-type issue) beads-graph--filter-type))))

(defun beads-graph--get-dependencies ()
  "Get all dependencies from all issues."
  (let ((issues (beads--parse-issues (beads--run-command "list")))
        (deps nil))
    (dolist (issue issues)
      (let ((issue-id (alist-get 'id issue)))
        ;; Get dependencies for this issue
        (condition-case nil
            (let* ((output (with-temp-buffer
                            (call-process beads-executable nil t nil
                                        "dep" "list" issue-id)
                            (buffer-string)))
                   (lines (split-string output "\n" t)))
              (dolist (line lines)
                (when (string-match "\\(bd-[0-9]+\\) -\\[\\([^]]+\\)\\]-> \\(bd-[0-9]+\\)" line)
                  (push (list :from (match-string 1 line)
                            :to (match-string 3 line)
                            :type (match-string 2 line))
                        deps))))
          (error nil))))
    (nreverse deps)))

(defun beads-graph--generate-dot (issues deps)
  "Generate DOT format graph from ISSUES and DEPS."
  (with-temp-buffer
    (insert "digraph beads {\n")
    (insert "  rankdir=TB;\n")
    (insert "  node [style=filled];\n")
    (insert "  \n")

    ;; Generate nodes
    (dolist (issue issues)
      (when (beads-graph--filter-issue issue)
        (let ((id (alist-get 'id issue))
              (label (beads-graph--issue-label issue))
              (color (beads-graph--issue-color issue))
              (shape (beads-graph--issue-shape issue)))
          (insert (format "  \"%s\" [label=\"%s\", fillcolor=\"%s\", shape=%s];\n"
                         id label color shape)))))

    (insert "  \n")

    ;; Generate edges
    (dolist (dep deps)
      (let ((from (plist-get dep :from))
            (to (plist-get dep :to))
            (type (plist-get dep :type)))
        ;; Only include edge if both nodes are in filtered issues
        (when (and (seq-find (lambda (i) (string= (alist-get 'id i) from)) issues)
                   (seq-find (lambda (i) (string= (alist-get 'id i) to)) issues)
                   (beads-graph--filter-issue
                    (seq-find (lambda (i) (string= (alist-get 'id i) from)) issues))
                   (beads-graph--filter-issue
                    (seq-find (lambda (i) (string= (alist-get 'id i) to)) issues)))
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
            (insert (format "  \"%s\" -> \"%s\" [style=%s, color=\"%s\", label=\"%s\"];\n"
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
  (let ((buffer (get-buffer-create "*beads-graph*")))
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
    (let* ((issues (beads--parse-issues (beads--run-command "list")))
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
             (let ((t (completing-read "Type (empty to clear): "
                                      '("bug" "feature" "task" "epic" "chore")
                                      nil t)))
               (if (string-empty-p t) nil t))))
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
  "Show dependency graph for all issues."
  (interactive)
  (beads-check-executable)
  (beads-graph--check-dot)
  (message "Generating graph...")
  (let* ((issues (beads--parse-issues (beads--run-command "list")))
         (deps (beads-graph--get-dependencies))
         (dot (beads-graph--generate-dot issues deps))
         (image-file (beads-graph--render-dot dot beads-graph-default-format)))
    (beads-graph--display-image image-file)
    (setq beads-graph--root-issue nil)
    (message "Graph generated (%d issues, %d dependencies)"
             (length issues) (length deps))))

;;;###autoload
(defun beads-graph-issue (issue-id)
  "Show dependency graph focused on ISSUE-ID."
  (interactive
   (list (completing-read "Graph for issue: "
                         (beads--issue-completion-table)
                         nil t nil 'beads--issue-id-history)))
  (beads-check-executable)
  (beads-graph--check-dot)
  (message "Generating graph for %s..." issue-id)
  (let* ((all-issues (beads--parse-issues (beads--run-command "list")))
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

;;; Footer

(provide 'beads-graph)
;;; beads-graph.el ends here
