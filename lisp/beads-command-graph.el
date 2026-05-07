;;; beads-command-graph.el --- Graph command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command class for `bd graph' operation.
;; Graph displays a visualization of issue dependency graphs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'beads-util)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-command-dep)
(require 'beads-command-list)
(require 'beads-completion)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;; Forward declarations

;;; ============================================================
;;; Command Class: beads-command-graph
;;; ============================================================

(beads-defcommand beads-command-graph (beads-command-global-options)
  ((issue-id
    :positional 1)
   (all
    :type boolean
    :short-option "a"
    :group "Options"
    :level 1
    :order 1)
   (box
    :initform t
    :type boolean
    :short-option "b"
    :group "Display"
    :level 1
    :order 2)
   (compact
    :type boolean
    :short-option "c"
    :group "Display"
    :level 1
    :order 3)
   (dot
    :type boolean
    :group "Options"
    :level 2)
   (html
    :type boolean
    :group "Options"
    :level 2))
  :documentation "Represents bd graph command.
Displays issue dependency graph visualization."
  :transient :manual)


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
;;; Command Class: beads-command-graph-check
;;; ============================================================

;;;###autoload (autoload 'beads-graph-check "beads-command-graph" nil t)
(beads-defcommand beads-command-graph-check (beads-command-global-options)
  ()
  :documentation "Check the dependency graph for cycles, orphans,
and other integrity issues.

Returns exit code 0 if the graph is clean, 1 if issues are found."
  :cli-command "graph check")


;;; ============================================================
;;; Parent Transient Menu: beads-graph-menu
;;; ============================================================

;;;###autoload (autoload 'beads-graph-menu "beads-command-graph" nil t)
(transient-define-prefix beads-graph-menu ()
  "Manage dependency graph operations."
  ["Graph Commands"
   ("d" "Display graph (bd graph)" beads-graph-transient)
   ("c" "Check integrity" beads-graph-check)
   ("v" "Visual graph (Graphviz)" beads-graph-all)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

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

(defvar-local beads-graph--current-image-file nil
  "Path to the temp image file currently displayed in this buffer.
Cleaned up on refresh and on `kill-buffer'.")

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

(defun beads-graph--dot-escape (s)
  "Escape S for safe inclusion inside a DOT double-quoted string.
Doubles backslashes first, then escapes double quotes.  Apply this
to user-supplied values (titles, dependency types, IDs) before
substituting them into a DOT attribute like label=\"...\".  Do
not apply it to format strings that already contain intentional
DOT escapes such as `\\n'."
  (replace-regexp-in-string
   "\"" "\\\\\""
   (replace-regexp-in-string "\\\\" "\\\\\\\\" s)))

(defun beads-graph--issue-label (issue)
  "Create label for ISSUE in graph.
ISSUE is a beads-issue EIEIO object."
  (let* ((id (oref issue id))
         (title (oref issue title))
         (status (oref issue status))
         (priority (oref issue priority))
         (truncated (if (> (length title) beads-graph-label-max-length)
                        (concat (substring title 0
                                           (max 0 (- beads-graph-label-max-length 3)))
                                "...")
                      title)))
    (format "%s\\n%s\\n[P%s %s]"
            (beads-graph--dot-escape id)
            (beads-graph--dot-escape truncated)
            priority
            (beads-graph--dot-escape status))))

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
  "Get all dependencies from all issues.
Returns a list of plists with :from, :to, and :type keys.
On per-issue fetch failure, reports the first error and suppresses
the rest so a broken bd binary or Dolt outage isn't silently hidden.

Performance: this issues one `bd dep list' call per issue, so it
is O(n) RPC calls for n issues.  `bd dep list' accepts multiple
positional IDs in a single invocation, but the current
`beads-command-dep-list' class is single-positional; tracked as
bde-g5b1."
  (let ((issues (beads-list-execute))
        (deps nil)
        (failures 0)
        (first-error nil)
        (first-failed-id nil))
    (dolist (issue issues)
      (let ((issue-id (oref issue id)))
        (condition-case err
            (let ((dep-list (beads-execute 'beads-command-dep-list :issue-id issue-id)))
              (dolist (dep dep-list)
                (push (list :from (oref dep issue-id)
                            :to (oref dep depends-on-id)
                            :type (oref dep type))
                      deps)))
          (error
           (cl-incf failures)
           (unless first-error
             (setq first-error (error-message-string err)
                   first-failed-id issue-id))))))
    (when (> failures 0)
      (message "beads-graph: failed to fetch deps for %s: %s%s"
               first-failed-id first-error
               (if (> failures 1)
                   (format " (and %d more failure%s)"
                           (1- failures)
                           (if (= failures 2) "" "s"))
                 "")))
    (nreverse deps)))

(defun beads-graph--connected-ids (root-id deps)
  "Return list of issue IDs reachable from ROOT-ID via DEPS.
Treats edges as undirected so both ancestors and descendants are
included.  DEPS is a list of plists with :from and :to keys.  The
returned list always contains ROOT-ID itself."
  (let ((visited (make-hash-table :test 'equal))
        (queue (list root-id)))
    (puthash root-id t visited)
    (while queue
      (let ((current (pop queue)))
        (dolist (dep deps)
          (let ((from (plist-get dep :from))
                (to (plist-get dep :to)))
            (cond
             ((and (equal from current) (not (gethash to visited)))
              (puthash to t visited)
              (push to queue))
             ((and (equal to current) (not (gethash from visited)))
              (puthash from t visited)
              (push from queue)))))))
    (hash-table-keys visited)))

(defun beads-graph--generate-dot (issues deps)
  "Generate DOT format graph from ISSUES and DEPS.
ISSUES is a list of beads-issue EIEIO objects.
DEPS is a list of plists with :from, :to, and :type keys."
  (let ((issue-by-id (make-hash-table :test 'equal)))
    (dolist (issue issues)
      (puthash (oref issue id) issue issue-by-id))
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
                            (beads-graph--dot-escape id)
                            label
                            color
                            shape)))))

      (insert "  \n")

      ;; Generate edges
      (dolist (dep deps)
        (let* ((from (plist-get dep :from))
               (to (plist-get dep :to))
               (type (plist-get dep :type))
               (from-issue (gethash from issue-by-id))
               (to-issue (gethash to issue-by-id)))
          (when (and from-issue to-issue
                     (beads-graph--filter-issue from-issue)
                     (beads-graph--filter-issue to-issue))
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
                              (beads-graph--dot-escape from)
                              (beads-graph--dot-escape to)
                              style
                              color
                              (beads-graph--dot-escape type)))))))

      (insert "}\n")
      (buffer-string))))

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

(defun beads-graph--cleanup-image-file ()
  "Delete the temp image file tracked in this buffer, if any."
  (when (and beads-graph--current-image-file
             (file-exists-p beads-graph--current-image-file))
    (ignore-errors (delete-file beads-graph--current-image-file)))
  (setq beads-graph--current-image-file nil))

(defun beads-graph--display-image (image-file)
  "Display IMAGE-FILE in a buffer.
Takes ownership of IMAGE-FILE: the previously displayed temp file
is deleted, IMAGE-FILE is tracked buffer-locally, and a
`kill-buffer-hook' deletes it when the buffer goes away."
  (let ((buffer (get-buffer-create (beads-buffer-name-utility "graph"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (beads-graph--cleanup-image-file)
        (erase-buffer)
        (beads-graph-mode)
        (setq beads-graph--current-image-file image-file)
        (add-hook 'kill-buffer-hook #'beads-graph--cleanup-image-file nil t)
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
  "Export current graph to a file.
When `beads-graph--root-issue' is set, exports only the connected
component the user is viewing, mirroring the scoping logic of
`beads-graph-issue'.  Otherwise exports the full graph."
  (interactive)
  (let* ((format (completing-read "Export format: "
                                 '("svg" "png" "pdf" "dot")
                                 nil t nil nil beads-graph-default-format))
         (default-name (format "beads-graph.%s" format))
         (file (read-file-name "Export to: " nil nil nil default-name)))
    (beads-graph--check-dot)
    (let* ((all-issues (beads-list-execute))
           (all-deps (beads-graph--get-dependencies))
           (id-set (when beads-graph--root-issue
                     (let ((h (make-hash-table :test 'equal)))
                       (dolist (id (beads-graph--connected-ids
                                    beads-graph--root-issue all-deps))
                         (puthash id t h))
                       h)))
           (issues (if id-set
                       (seq-filter (lambda (i) (gethash (oref i id) id-set))
                                   all-issues)
                     all-issues))
           (deps (if id-set
                     (seq-filter (lambda (d)
                                   (and (gethash (plist-get d :from) id-set)
                                        (gethash (plist-get d :to) id-set)))
                                 all-deps)
                   all-deps))
           (dot (beads-graph--generate-dot issues deps)))
      (if (string= format "dot")
          (with-temp-file file
            (insert dot))
        (let ((rendered (beads-graph--render-dot dot format)))
          (unwind-protect
              (copy-file rendered file t)
            (when (file-exists-p rendered)
              (delete-file rendered)))
          (message "Graph exported to: %s" file)))
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
  (let* ((issues (beads-list-execute))
         (deps (beads-graph--get-dependencies))
         (dot (beads-graph--generate-dot issues deps))
         (image-file (beads-graph--render-dot dot beads-graph-default-format)))
    (beads-graph--display-image image-file)
    (setq beads-graph--root-issue nil)
    (message "Graph generated (%d issues, %d dependencies)"
             (length issues) (length deps))))

;;;###autoload
(defun beads-graph-issue (issue-id)
  "Show dependency graph focused on ISSUE-ID using Graphviz.
Restricts the graph to issues reachable from ISSUE-ID through the
dependency edges (in either direction)."
  (interactive
   (list (beads-completion-read-issue "Graph for issue: "
                         nil t nil 'beads--issue-id-history)))
  (beads-check-executable)
  (beads-graph--check-dot)
  (message "Generating graph for %s..." issue-id)
  (let* ((all-issues (beads-list-execute))
         (all-deps (beads-graph--get-dependencies))
         (connected-ids (beads-graph--connected-ids issue-id all-deps))
         (id-set (let ((h (make-hash-table :test 'equal)))
                   (dolist (id connected-ids) (puthash id t h))
                   h))
         (issues (seq-filter (lambda (i) (gethash (oref i id) id-set))
                             all-issues))
         (deps (seq-filter (lambda (d)
                             (and (gethash (plist-get d :from) id-set)
                                  (gethash (plist-get d :to) id-set)))
                           all-deps))
         (dot (beads-graph--generate-dot issues deps))
         (image-file (beads-graph--render-dot dot beads-graph-default-format)))
    (beads-graph--display-image image-file)
    (setq beads-graph--root-issue issue-id)
    (message "Graph generated for %s (%d issues, %d dependencies)"
             issue-id (length issues) (length deps))))

;;; Mode Definition

(defvar beads-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'beads-graph-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "f") #'beads-graph-filter)
    (define-key map (kbd "e") #'beads-graph-export)
    (define-key map (kbd "i") #'beads-graph-issue)
    map)
  "Keymap for `beads-graph-mode'.")

(define-derived-mode beads-graph-mode special-mode "Beads-Graph"
  "Major mode for displaying Beads dependency graphs.

\\{beads-graph-mode-map}"
  (setq buffer-read-only t))

(provide 'beads-command-graph)
;;; beads-command-graph.el ends here
