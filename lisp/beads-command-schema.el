;;; beads-command-schema.el --- Schema command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-schema' EIEIO class for the
;; `bd schema' command, category 1 of the CLI sync audit (workflow
;; be-j2b).
;;
;; `bd schema' prints the JSON Schema for bd's canonical output record
;; types, reflected from the same Go structs bd serializes — useful
;; for validating beads-types.el against the wire format.

;;; Code:

(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)

;;; Schema Command

;;;###autoload (autoload 'beads-schema "beads-command-schema" nil t)
(beads-defcommand beads-command-schema (beads-command-global-options)
  ()
  :documentation "Print the JSON Schema for bd's canonical output records.
Reflected from the same Go structs bd serializes, so it stays in
lockstep with the actual --json / export output.  Use it to generate
typed consumer models rather than hand-maintaining them:
`bd schema | jq .types.issue'.")

(provide 'beads-command-schema)
;;; beads-command-schema.el ends here
