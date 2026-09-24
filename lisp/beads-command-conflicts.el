;;; beads-command-conflicts.el --- Conflicts command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command classes for the `bd conflicts'
;; command group (bd 1.3.x live merge-conflict surface, category 1 of
;; the CLI sync audit, workflow be-j2b):
;;
;; - `beads-command-conflicts-list'    - list tables/issues in conflict
;; - `beads-command-conflicts-resolve' - resolve with --ours/--theirs
;; - `beads-command-conflicts-show'    - show conflicted rows side by side
;;
;; `bd conflicts' itself is a mid-level router group: per project
;; policy it gets a parent transient (`beads-conflicts') and no class.
;;
;; `bd conflicts list' and `bd conflicts show' emit a single JSON
;; object with --json; `bd conflicts resolve' prints plain text even
;; under --json when there is nothing to resolve, so it is declared
;; `:json nil' and its interactive run displays the raw output.

;;; Code:

(require 'json)
(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)

;;; Conflicts List Command

;;;###autoload (autoload 'beads-conflicts-list "beads-command-conflicts" nil t)
(beads-defcommand beads-command-conflicts-list (beads-command-global-options)
  ()
  :documentation "List tables and issues with live merge conflicts.
Reads conflict state positively from the merge's own conflict rows and
Dolt's conflict tables."
  :cli-command "conflicts list"
  :result beads-conflicts-list-result)

(cl-defmethod beads-command-parse ((_command beads-command-conflicts-list) stdout)
  "Parse the conflicts-list envelope from STDOUT.
The generic parser would leave the raw `tables' array as a vector;
bind `json-array-type' to the symbol list so the slot type holds."
  (when (and stdout (not (string-empty-p stdout)))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (beads-conflicts-list-result-from-json
       (json-read-from-string stdout)))))

;;; Conflicts Resolve Command

;;;###autoload (autoload 'beads-conflicts-resolve "beads-command-conflicts" nil t)
(beads-defcommand beads-command-conflicts-resolve (beads-command-global-options)
  ((issue-ids
    :positional 1
    :type (list-of string)
    :separator " "
    :prompt "Issue IDs to resolve (comma-separated): "
    :group "Selection"
    :level 1
    :order 1
    :documentation "Named issue IDs resolved row by row")
   (all
    :type boolean
    :group "Selection"
    :level 1
    :order 2
    :documentation "Resolve whole tables instead of named issues")
   (ours
    :type boolean
    :group "Strategy"
    :level 1
    :order 1
    :documentation "Keep our side")
   (theirs
    :type boolean
    :group "Strategy"
    :level 1
    :order 2
    :documentation "Take their side")
   (strategy
    :type (or null string)
    :long-option "strategy"
    :choices ("ours" "theirs")
    :group "Strategy"
    :level 2
    :order 1
    :documentation "Resolution strategy: ours|theirs")
   (table
    :type (or null string)
    :long-option "table"
    :group "Scope"
    :level 2
    :order 1
    :documentation "Table to resolve (default: issues)")
   (no-commit
    :type boolean
    :long-option "no-commit"
    :group "Options"
    :level 2
    :order 1
    :documentation "Resolve without committing the merge")
   (conclude
    :type boolean
    :long-option "conclude"
    :group "Options"
    :level 2
    :order 2
    :documentation "Commit a merge whose conflicts are already resolved"))
  :documentation "Resolve live merge conflicts, then conclude the merge.
Named issue IDs are resolved row by row; --all resolves whole tables.
The merge commits only once NO conflicts remain."
  :cli-command "conflicts resolve"
  :json nil)

(cl-defmethod beads-command-validate ((command beads-command-conflicts-resolve))
  "Validate conflicts-resolve COMMAND.
Requires exactly one selection mode (IDs or --all), and requires a
resolution strategy unless --conclude is used."
  (with-slots (issue-ids all ours theirs strategy conclude) command
    (cond
     ((and issue-ids all)
      "Name issue IDs or pass --all (not both)")
     ((and (not issue-ids) (not all) (not conclude))
      "Name the issue IDs to resolve, or pass --all")
     ((and (not conclude)
           (not (or ours theirs (and strategy (not (string-empty-p strategy))))))
      "A resolution strategy is required: --ours or --theirs")
     (t nil))))

;;; Conflicts Show Command

;;;###autoload (autoload 'beads-conflicts-show "beads-command-conflicts" nil t)
(beads-defcommand beads-command-conflicts-show (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :argument "--issue-id="
    :prompt "Issue ID (optional): "
    :reader beads-reader-issue-id
    :group "Selection"
    :level 1
    :order 1
    :documentation "Restrict to one issue (omit for every row)")
   (all-fields
    :type boolean
    :long-option "all-fields"
    :group "Options"
    :level 2
    :order 1
    :documentation "Show every column, not just divergent fields")
   (table
    :type (or null string)
    :long-option "table"
    :group "Options"
    :level 2
    :order 2
    :documentation "Restrict to one conflicted table (default: all)"))
  :documentation "Show each conflicted row with its fields side by side.
Only fields where our side and their side disagree are shown unless
--all-fields is used."
  :cli-command "conflicts show"
  :result beads-conflicts-show-result)

(cl-defmethod beads-command-parse ((_command beads-command-conflicts-show) stdout)
  "Parse the conflicts-show envelope from STDOUT.
Bind `json-array-type' to the symbol list so the raw `rows' array
holds as a list."
  (when (and stdout (not (string-empty-p stdout)))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (beads-conflicts-show-result-from-json
       (json-read-from-string stdout)))))

;;; Parent Router Transient (no class, per project policy)

;;;###autoload (autoload 'beads-conflicts "beads-command-conflicts" nil t)
(transient-define-prefix beads-conflicts ()
  "Inspect and resolve live merge conflicts.

Parent transient for the `bd conflicts' group; per project policy the
group itself never gets an EIEIO class."
  [["Conflicts"
    ("l" "List conflicts" beads-conflicts-list)
    ("s" "Show conflicted rows" beads-conflicts-show)
    ("r" "Resolve conflicts" beads-conflicts-resolve)
    ("q" "Quit" transient-quit-one)]])

(provide 'beads-command-conflicts)
;;; beads-command-conflicts.el ends here
