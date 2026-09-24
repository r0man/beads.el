;;; beads-command-heartbeat.el --- Heartbeat command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-heartbeat' EIEIO class for
;; the `bd heartbeat' command (aliases: `bd hb'), part of the bd 1.3.x
;; worker-lease loop and category 1 of the CLI sync audit (workflow
;; be-j2b).
;;
;; A claim carries a lease that expires after a TTL; a worker keeps the
;; claim alive by heartbeating faster than the TTL.  Heartbeat pushes
;; `lease_expires_at' forward and stamps `heartbeat_at = now'.  Only
;; the current owner may heartbeat.

;;; Code:

(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'beads-reader)

;;; Heartbeat Command

;;;###autoload (autoload 'beads-heartbeat "beads-command-heartbeat" nil t)
(beads-defcommand beads-command-heartbeat (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :argument "--issue-id="
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Heartbeat"
    :level 1
    :order 1
    :required t))
  :documentation "Refresh the lease on an issue you hold in_progress.
Only the current owner may heartbeat; a reclaimed or closed issue
fails so the worker learns to stop.  Leases are node-local and write
no Dolt commit or history."
  :result beads-heartbeat-result)

(provide 'beads-command-heartbeat)
;;; beads-command-heartbeat.el ends here
