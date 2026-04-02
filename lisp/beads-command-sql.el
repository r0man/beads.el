;;; beads-command-sql.el --- SQL command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-sql' EIEIO class for the
;; `bd sql' command.  Executes raw SQL queries against the beads
;; database.
;;
;; Usage:
;;   (beads-command-sql! :query "SELECT * FROM issues")
;;   (beads-sql)  ; invoke transient menu

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; SQL Command

(beads-defcommand beads-command-sql (beads-command-global-options)
  ((query
    :positional 1
    :option-type :string
    :key "q"
    :argument "--query="
    :prompt "SQL query: "
    :group "SQL"
    :level 1
    :order 1
    :required t)
   (csv
    :option-type :boolean
    :key "c"
    :group "Options"
    :level 2
    :order 2))
  :documentation "Execute raw SQL against the beads database.
Supports SELECT, INSERT, UPDATE, DELETE, and other SQL statements.")


(cl-defmethod beads-command-validate ((command beads-command-sql))
  "Validate sql COMMAND.
Query is required."
  (with-slots (query) command
    (when (or (null query) (string-empty-p query))
      "Must provide a SQL query")))


;;; Transient Menu

;;;###autoload (autoload 'beads-sql "beads-command-sql" nil t)
(beads-meta-define-transient beads-command-sql "beads-sql"
  "Execute raw SQL against the beads database.

Supports SELECT, INSERT, UPDATE, DELETE, and other SQL statements.
Use --csv for CSV output format on SELECT queries.
Use --json for structured JSON output."
  beads-option-global-section)

(provide 'beads-command-sql)
;;; beads-command-sql.el ends here
