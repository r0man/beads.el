;;; beads-command-prime.el --- `bd prime' command class -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command class for the `bd prime'
;; subcommand, split out of `beads-command-misc.el' per the one-file-per-
;; subcommand convention (WI-SF-11).  `bd prime' emits human-readable
;; Markdown, never JSON, so the class is declared `:json nil' and
;; `beads-command-execute' returns the raw stdout string.  The
;; `beads-context' view consumes that string (see `beads-handoff.el').

;;; Code:

(require 'beads-command)
(require 'beads-meta)

;;; ============================================================
;;; Command Class: beads-command-prime
;;; ============================================================

;;;###autoload (autoload 'beads-prime "beads-command-prime" nil t)
(beads-defcommand beads-command-prime (beads-command-global-options)
  ((export
    :type boolean
    :group "Options"
    :level 1
    :order 1
    :documentation "Output default content (ignores PRIME.md override)")
   (full
    :type boolean
    :group "Options"
    :level 1
    :order 2
    :documentation "Force full CLI output (ignore MCP detection)")
   (mcp
    :type boolean
    :group "Options"
    :level 1
    :order 3
    :documentation "Force MCP mode (minimal output)")
   (stealth
    :type boolean
    :group "Options"
    :level 1
    :order 4
    :documentation "Stealth mode (no git operations, flush only)")
   (hook-json
    :type boolean
    :long-option "hook-json"
    :group "Memories"
    :level 2
    :order 1
    :documentation "Wrap output in the SessionStart hook JSON envelope
(Claude Code, Gemini CLI, Codex)")
   (memories-only
    :type boolean
    :long-option "memories-only"
    :group "Memories"
    :level 2
    :order 2
    :documentation "Output only persistent memories for compact hook contexts")
   (max-memories
    :type (or null string integer)
    :long-option "max-memories"
    :prompt "Max memories to inject: "
    :group "Memories"
    :level 2
    :order 3
    :documentation "Cap injected persistent memories to N entries
(0 = unlimited; falls back to the prime.max-memories config key)")
   (max-memory-chars
    :type (or null string integer)
    :long-option "max-memory-chars"
    :prompt "Max total memory bytes: "
    :group "Memories"
    :level 2
    :order 4
    :documentation "Cap the total bytes of injected memory entries, at
whole-memory boundaries (0 = unlimited; falls back to the
prime.max-memory-chars config key)")
   (no-memories
    :type boolean
    :long-option "no-memories"
    :group "Memories"
    :level 2
    :order 5
    :documentation "Omit the persistent memories section (ignored when
--memories-only is set, which wins)"))
  :documentation "Represents bd prime command.
Outputs AI-optimized workflow context.  Output is human-readable
Markdown, not JSON, so the class is `:json nil'."
  :json nil)

(provide 'beads-command-prime)
;;; beads-command-prime.el ends here
