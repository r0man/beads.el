;;; beads-command-reclaim.el --- Reclaim command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-reclaim' EIEIO class for the
;; `bd reclaim' command (dead-worker recovery), part of the bd 1.3.x
;; worker-lease loop and category 1 of the CLI sync audit (workflow
;; be-j2b).
;;
;; reclaim is the reaper: it reverts in_progress issues whose lease
;; expired more than --older-than ago back to open and clears the
;; assignee.  A lease is only meaningful on the replica that granted
;; it; reclaim skips leases another replica granted unless
;; --any-replica is passed.

;;; Code:

(require 'json)
(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'beads-reader)

;;; Reclaim Command

;;;###autoload (autoload 'beads-reclaim "beads-command-reclaim" nil t)
(beads-defcommand beads-command-reclaim (beads-command-global-options)
  ((older-than
    :type (or null string)
    :long-option "older-than"
    :prompt "Grace window past lease expiry (e.g. 10m): "
    :group "Scope"
    :level 1
    :order 1
    :documentation "Only reclaim leases that expired at least this long
ago (grace window; default 10m)")
   (label
    :type (list-of string)
    :long-option "label"
    :short-option "l"
    :prompt "Labels (AND): "
    :reader beads-reader-issue-labels
    :group "Scope"
    :level 2
    :order 1
    :documentation "Only reclaim issues with ALL these labels")
   (label-any
    :type (list-of string)
    :long-option "label-any"
    :prompt "Labels (OR): "
    :reader beads-reader-issue-labels
    :group "Scope"
    :level 2
    :order 2
    :documentation "Only reclaim issues with AT LEAST ONE of these labels")
   (exclude-label
    :type (list-of string)
    :long-option "exclude-label"
    :prompt "Exclude labels: "
    :reader beads-reader-issue-labels
    :group "Scope"
    :level 2
    :order 3
    :documentation "Never reclaim issues carrying ANY of these labels")
   (assignee
    :type (list-of string)
    :long-option "assignee"
    :short-option "a"
    :prompt "Assignees (comma-separated): "
    :group "Scope"
    :level 2
    :order 4
    :documentation "Only reclaim leases held by these assignees (repeatable)")
   (ids
    :type (list-of string)
    :long-option "id"
    :prompt "Issue IDs (comma-separated): "
    :reader beads-reader-issue-id
    :group "Scope"
    :level 2
    :order 5
    :documentation "Only reclaim these issue IDs (repeatable)")
   (any-replica
    :type boolean
    :long-option "any-replica"
    :group "Federation"
    :level 2
    :order 1
    :documentation "Also reclaim leases granted by ANOTHER replica
(unsafe unless that replica is gone)"))
  :documentation "Revert stale-lease issues to ready (dead-worker recovery).
Finds in_progress issues whose lease expired more than --older-than
ago, clears the assignee, and sets them back to open.  Filters
AND-combine and never widen the set."
  :result beads-reclaim-result)

(cl-defmethod beads-command-parse ((_command beads-command-reclaim) stdout)
  "Parse the reclaim envelope from STDOUT.
Bind `json-array-type' to the symbol list so the `reclaimed' array
holds as a list."
  (when (and stdout (not (string-empty-p stdout)))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (beads-reclaim-result-from-json (json-read-from-string stdout)))))

(provide 'beads-command-reclaim)
;;; beads-command-reclaim.el ends here
