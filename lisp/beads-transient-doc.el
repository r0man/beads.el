;;; beads-transient-doc.el --- Generate docs from transient prefixes -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, docs, transient

;;; Commentary:

;; Generic introspection of transient prefix menus for documentation.
;;
;; The hand-written dispatch menus in `beads-menu.el' are the source of
;; truth for the documented key contract.  This module reads the parsed
;; transient layout (the same structure `transient-define-prefix'
;; installs) without rendering a menu, flattens it to a canonical list of
;; suffix entries, and renders that list as Markdown or Org.  A checked-in
;; snapshot generated from the code can then be compared byte-for-byte, so
;; a documented key can never silently drift from the menu that implements
;; it (plan F2, large item 12).
;;
;; The mechanism is deliberately package-agnostic: nothing here knows
;; about beads.el's menus.  A caller passes a list of (PREFIX . TITLE)
;; cells, which makes the same generator usable by downstream packages such
;; as gascity.el for their own dispatch reference.
;;
;; The public entry points are:
;;
;; - `beads-transient-doc-suffixes': flatten one prefix to entries.
;; - `beads-transient-doc-markdown': render menus as a Markdown document.
;; - `beads-transient-doc-menu-markdown': render one menu as a block.
;; - `beads-transient-doc-file-check': verify a generated file.
;; - `beads-transient-doc-file-write': write a generated file.
;; - `beads-transient-doc-region-read' / `-region-write': the same for a
;;   marked region of a larger hand-written file (README, the Org manual).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

;;; Project root

(defun beads-transient-doc-project-root ()
  "Return the root directory of the project, or nil.
Uses Eldev's `eldev-project-dir' when running under Eldev (as in CI),
otherwise derives the root from the location of the loaded `beads.el'."
  (or (bound-and-true-p eldev-project-dir)
      (when-let* ((beads (locate-library "beads.el")))
        (file-name-directory
         (directory-file-name (file-name-directory beads))))))

;;; Layout introspection

(defun beads-transient-doc--suffix-p (spec)
  "Return non-nil when SPEC is a transient suffix with a non-empty key.
Spacer rows and information lines carry no usable key and are ignored."
  (and (consp spec)
       (symbolp (car spec))
       (let ((key (plist-get (cdr spec) :key)))
         (and (stringp key) (not (string-empty-p key))))))

(defun beads-transient-doc--group-desc (args children)
  "Return the description of a group from ARGS or CHILDREN.
A group description lives either in the `:description' property of ARGS
or, as transient's parser emits for hand-written column groups, as the
leading string of CHILDREN."
  (let ((desc (and (listp args) (plist-get args :description))))
    (cond ((and (stringp desc) (not (string-empty-p desc))) desc)
          ((and (consp children) (stringp (car children)))
           (let ((s (car children)))
             (and (not (string-empty-p s)) s)))
          (t nil))))

(defun beads-transient-doc--walk (spec path)
  "Walk transient layout SPEC, collecting suffix entries.
PATH is the list of (DESCRIPTION . LEVEL) group cells seen so far,
outermost first.  Entries are plists with `:key', `:command',
`:description', `:level', `:group' (the nearest group description, or
nil), and `:path' (all group descriptions)."
  (cond
   ((vectorp spec)
    (let* ((args (and (>= (length spec) 2) (aref spec 1)))
           (children (and (>= (length spec) 3) (aref spec 2)))
           (level (if (listp args) (or (plist-get args :level) 1) 1))
           (desc (beads-transient-doc--group-desc args children))
           (path (if desc (append path (list (cons desc level))) path)))
      (mapcan (lambda (child) (beads-transient-doc--walk child path))
              (append children nil))))
   ((beads-transient-doc--suffix-p spec)
    (let* ((props (cdr spec))
           (nearest (car (last path))))
      (list (list :key (plist-get props :key)
                  :command (plist-get props :command)
                  :description (plist-get props :description)
                  :level (if nearest (cdr nearest) 1)
                  :group (if nearest (car nearest) nil)
                  :path (mapcar #'car path)))))
   ((consp spec)
    (mapcan (lambda (child) (beads-transient-doc--walk child path)) spec))
   (t nil)))

(defun beads-transient-doc-suffixes (prefix)
  "Return the documented suffix entries of transient PREFIX.
PREFIX is a symbol naming a `transient-define-prefix' command.  The
parsed layout is read from the `transient--layout' symbol property, so
the prefix is never rendered and no buffer is created.  See
`beads-transient-doc--walk' for the shape of each entry."
  (unless (get prefix 'transient--layout)
    (error "Not a transient prefix (no parsed layout): %s" prefix))
  (beads-transient-doc--walk (get prefix 'transient--layout) nil))

;;; Rendering

(defun beads-transient-doc--group-order (entries)
  "Return the distinct `:group' values of ENTRIES in first-seen order."
  (let (seen result)
    (dolist (entry entries)
      (let ((group (plist-get entry :group)))
        (unless (member group seen)
          (push group seen)
          (push group result))))
    (nreverse result)))

(defun beads-transient-doc--markdown-table (entries)
  "Render ENTRIES as a Markdown table string.
The table has columns Key, Command, and Description."
  (concat "| Key | Command | Description |\n"
          "| --- | --- | --- |\n"
          (mapconcat
           (lambda (entry)
             (format "| `%s` | `%s` | %s |"
                     (plist-get entry :key)
                     (plist-get entry :command)
                     (or (plist-get entry :description) "")))
           entries "\n")))

(defun beads-transient-doc-menu-markdown (prefix title)
  "Render transient PREFIX as a Markdown block under TITLE.
Suffixes are grouped by their menu group; each group becomes a level-4
heading followed by a table.  Returns a string ending in a newline."
  (let ((entries (beads-transient-doc-suffixes prefix)))
    (concat
     (format "#### %s\n\n" title)
     (mapconcat
      (lambda (group)
        (let ((rows (cl-remove-if-not
                     (lambda (entry) (equal (plist-get entry :group) group))
                     entries)))
          (format "##### %s\n\n%s\n"
                  (or group "Commands")
                  (beads-transient-doc--markdown-table rows))))
      (beads-transient-doc--group-order entries)
      "\n"))))

(defun beads-transient-doc-markdown (menus &optional title intro)
  "Render MENUS as a Markdown reference document.
MENUS is a list of (PREFIX . TITLE) cells.  TITLE defaults to
\"Dispatch reference\" and INTRO to a standard generated-file notice.
The result is a string suitable for a checked-in snapshot."
  (concat
   (format "# %s\n\n" (or title "Dispatch reference"))
   (or intro
       (concat
        "This file is generated from the transient prefix definitions by\n"
        "`scripts/dispatch-docs`.  Do not edit it by hand: run\n"
        "`scripts/dispatch-docs --write` to regenerate it, and the test\n"
        "suite fails when the checked-in copy and the code disagree.\n\n"
        "Groups contributed at runtime through `beads-menu-providers' (for\n"
        "example gascity.el's city group) are not listed here; only the\n"
        "prefixes owned by beads.el are part of this contract.\n\n"))
   (mapconcat (lambda (cell)
                (beads-transient-doc-menu-markdown (car cell) (cdr cell)))
              menus "\n")
   "\nGenerated by `beads-transient-doc-markdown'.\n"))

(defun beads-transient-doc--org-table (entries)
  "Render ENTRIES as an Org table string.
The table has columns Group, Level, Key, Command, and Description; the
Group column is repeated on every row so the table can be sorted and
grepped without reading surrounding headings."
  (concat "| Group | Level | Key | Command | Description |\n"
          "|-------+-------+-----+---------+-------------|\n"
          (mapconcat
           (lambda (entry)
             (format "| %s | %s | =%s= | =%s= | %s |"
                     (or (plist-get entry :group) "")
                     (or (plist-get entry :level) "")
                     (plist-get entry :key)
                     (plist-get entry :command)
                     (or (plist-get entry :description) "")))
           entries "\n")
          (when entries "\n")))

(defun beads-transient-doc-menu-org (prefix title)
  "Render transient PREFIX as an Org block under TITLE.
The block is a level-2 Org heading followed by a table of every suffix,
in menu order.  Returns a string ending in a newline."
  (concat (format "** %s\n\n" title)
          (beads-transient-doc--org-table
           (beads-transient-doc-suffixes prefix))
          "\n"))

;;; Generated files and marked regions

(defun beads-transient-doc--read-file (file)
  "Return the contents of FILE as a string, or nil when FILE is missing."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun beads-transient-doc-file-check (file expected)
  "Signal an error unless FILE is byte-identical to EXPECTED.
Return non-nil when FILE is current.  Used to keep a checked-in
snapshot from drifting away from the generator."
  (let ((actual (beads-transient-doc--read-file file)))
    (unless actual
      (error "Generated file is missing: %s" file))
    (unless (string= actual expected)
      (error (concat "Generated file is out of date: %s "
                     "(run `scripts/dispatch-docs --write')")
             file))
    t))

(defun beads-transient-doc-file-write (file content)
  "Write CONTENT to FILE, creating parent directories as needed.
Return FILE."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) file nil 'silent))
  file)

(defun beads-transient-doc-region-read (file begin end)
  "Return the text between markers BEGIN and END in FILE, or nil.
The markers themselves are excluded.  When either marker is missing,
return nil."
  (when-let* ((text (beads-transient-doc--read-file file)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (search-forward begin nil t)
        (let ((start (point)))
          (when (search-forward end nil t)
            (let ((stop (match-beginning 0)))
              (buffer-substring-no-properties start stop))))))))

(defun beads-transient-doc-region-write (file begin end content)
  "Replace the text between markers BEGIN and END in FILE with CONTENT.
Signal an error when FILE or either marker is missing."
  (let ((text (beads-transient-doc--read-file file)))
    (unless text
      (error "File is missing: %s" file))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (unless (search-forward begin nil t)
        (error "Begin marker not found in %s: %s" file begin))
      (let ((start (point)))
        (unless (search-forward end nil t)
          (error "End marker not found in %s: %s" file end))
        (let ((stop (match-beginning 0)))
          (delete-region start stop)
          (goto-char start)
          (insert content)))
      (write-region (point-min) (point-max) file nil 'silent))
    file))

(defun beads-transient-doc-region-check (file begin end expected)
  "Signal an error unless the region between BEGIN and END equals EXPECTED.
FILE names the file, BEGIN and END the markers.  Return non-nil when
the region is current."
  (let ((actual (beads-transient-doc-region-read file begin end)))
    (unless actual
      (error "Generated region not found in %s (%s .. %s)" file begin end))
    (unless (string= actual expected)
      (error (concat "Generated region is out of date in %s "
                     "(run `scripts/dispatch-docs --write')")
             file))
    t))

(provide 'beads-transient-doc)

;;; beads-transient-doc.el ends here
