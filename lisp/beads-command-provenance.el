;;; beads-command-provenance.el --- Provenance command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command classes for the `bd
;; provenance' command group (bd 1.3.x append-only provenance event
;; log, category 1 of the CLI sync audit, workflow be-j2b):
;;
;; - `beads-command-provenance-log'    - list events for an issue
;; - `beads-command-provenance-by-ref' - list events bound to a ref
;; - `beads-command-provenance-record' - record an event (idempotent)
;;
;; `bd provenance' itself is a mid-level router group: per project
;; policy it gets a parent transient (`beads-provenance') and no class.

;;; Code:

(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'beads-reader)

;;; Provenance Log Command

;;;###autoload (autoload 'beads-provenance-log "beads-command-provenance" nil t)
(beads-defcommand beads-command-provenance-log
    (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :argument "--issue-id="
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Log"
    :level 1
    :order 1
    :required t)
   (kind
    :type (or null string)
    :long-option "kind"
    :group "Filters"
    :level 2
    :order 1
    :documentation "Filter by event kind (optional)"))
  :documentation "List provenance events for an issue."
  :cli-command "provenance log"
  :result (list-of beads-provenance-event))

;;; Provenance By-Ref Command

;;;###autoload (autoload 'beads-provenance-by-ref "beads-command-provenance" nil t)
(beads-defcommand beads-command-provenance-by-ref
    (beads-command-global-options)
  ((ref
    :positional 1
    :type (or null string)
    :argument "--ref="
    :prompt "External reference (SHA, PR URL, ...): "
    :group "By Ref"
    :level 1
    :order 1
    :required t))
  :documentation "List provenance events bound to a ref."
  :cli-command "provenance by-ref"
  :result (list-of beads-provenance-event))

;;; Provenance Record Command

;;;###autoload (autoload 'beads-provenance-record "beads-command-provenance" nil t)
(beads-defcommand beads-command-provenance-record
    (beads-command-global-options)
  ((issue
    :type (or null string)
    :long-option "issue"
    :prompt "Issue ID (required): "
    :reader beads-reader-issue-id
    :group "Event"
    :level 1
    :order 1
    :required t
    :documentation "Issue id the event is bound to")
   (kind
    :type (or null string)
    :long-option "kind"
    :prompt "Kind: "
    :choices ("cut" "claim" "suspend" "resume" "handoff"
              "commit" "land" "used")
    :group "Event"
    :level 1
    :order 2
    :required t
    :documentation "Event kind: cut|claim|suspend|resume|handoff|commit|land|used")
   (source
    :type (or null string)
    :long-option "source"
    :prompt "Source: "
    :group "Event"
    :level 1
    :order 3
    :required t
    :documentation "Producer of the event, e.g. git-hook, orchestrator")
   (ref
    :type (or null string)
    :long-option "ref"
    :prompt "External reference: "
    :group "Event"
    :level 2
    :order 1
    :documentation "Opaque external reference, e.g. a SHA or PR url")
   (ref-kind
    :type (or null string)
    :long-option "ref-kind"
    :prompt "Ref kind: "
    :choices ("git-sha" "pr" "work-id" "transcript" "branch")
    :group "Event"
    :level 2
    :order 2
    :documentation "Ref kind: git-sha|pr|work-id|transcript|branch")
   (actor
    :type (or null string)
    :long-option "actor"
    :prompt "Actor: "
    :group "Event"
    :level 2
    :order 3
    :documentation "Opaque actor identifier (optional)")
   (at
    :type (or null string)
    :long-option "at"
    :prompt "Event time (RFC3339): "
    :group "Event"
    :level 2
    :order 4
    :documentation "Event-time as RFC3339 (required for ref-less kinds)")
   (payload
    :type (or null string)
    :long-option "payload"
    :prompt "Payload: "
    :group "Event"
    :level 3
    :order 1
    :documentation "Opaque payload, e.g. JSON (optional)"))
  :documentation "Record a provenance event (idempotent).
A deterministic id is computed from source:issue:kind:(ref or --at),
so re-running the same record is a no-op.  An event recorded without
--ref requires --at so the id is caller-owned."
  :cli-command "provenance record"
  :result beads-provenance-record-result)

;;; Parent Router Transient (no class, per project policy)

;;;###autoload (autoload 'beads-provenance "beads-command-provenance" nil t)
(transient-define-prefix beads-provenance ()
  "Append-only provenance event log.

Parent transient for the `bd provenance' group; per project policy the
group itself never gets an EIEIO class."
  [["Provenance"
    ("l" "Log for issue" beads-provenance-log)
    ("b" "By external ref" beads-provenance-by-ref)
    ("r" "Record event" beads-provenance-record)
    ("q" "Quit" transient-quit-one)]])

(provide 'beads-command-provenance)
;;; beads-command-provenance.el ends here
