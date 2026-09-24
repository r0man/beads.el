;;; beads-command-sync.el --- Sync command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-sync' EIEIO class for the
;; `bd sync' command — one pull / conflict-check / is_blocked repair /
;; push federation cycle against the Dolt remote (category 1 of the
;; CLI sync audit, workflow be-j2b).
;;
;; This is not `bd federation sync', which targets named peer towns
;; with an --strategy ours|theirs switch; `bd sync' targets the
;; configured remote and halts on conflicts it cannot settle.

;;; Code:

(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)

;;; Sync Command

;;;###autoload (autoload 'beads-sync "beads-command-sync" nil t)
(beads-defcommand beads-command-sync (beads-command-global-options)
  ((remote
    :type (or null string)
    :long-option "remote"
    :prompt "Remote name: "
    :group "Remote"
    :level 1
    :order 1
    :documentation "Sync with a specific named remote instead of the default")
   (attempts
    :type (or null string integer)
    :long-option "attempts"
    :prompt "Maximum pull/push attempts: "
    :group "Options"
    :level 1
    :order 2
    :documentation "Maximum pull/push attempts before reporting a
transient retry exhaustion (exit 3)")
   (yes
    :type boolean
    :long-option "yes"
    :short-option "y"
    :group "Options"
    :level 2
    :order 1
    :documentation "Consent to adopting a Dolt remote derived from git
origin when none is configured")
   (no-adopt
    :type boolean
    :long-option "no-adopt"
    :group "Options"
    :level 2
    :order 2
    :documentation "Never derive a Dolt remote from git origin (also
BD_NO_REMOTE_ADOPT=1)"))
  :documentation "Run one full synchronization cycle against the Dolt remote.
Pull, positively check for merge conflicts, recompute the denormalized
is_blocked flag, and push with bounded retries on a push race.  Exit
codes: 0 synced, 1 error, 2 merge conflict halted, 3 retries
exhausted (transient), 4 dirty working set stuck."
  :result beads-sync-result)

(provide 'beads-command-sync)
;;; beads-command-sync.el ends here
