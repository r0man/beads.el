;;; beads-command-events.el --- Events journal command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command classes for the `bd events'
;; journal commands (bd 1.3.x durable events journal, category 1 of the
;; CLI sync audit, workflow be-j2b):
;;
;; - `beads-command-events-tail'  - print records after a sequence number
;; - `beads-command-events-export' - print the whole journal as JSON lines
;; - `beads-command-events-prune' - delete records below a sequence number
;;
;; `bd events' itself is a mid-level router group: per project policy
;; it gets a parent transient (`beads-events') and no class.
;;
;; `tail' and `export' print one JSON object per line (both with and
;; without --json); `beads-command-parse' is overridden to split the
;; lines and build `beads-event-record' objects.  `prune --json'
;; reports {\"pruned\": N}.

;;; Code:

(require 'json)
(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'beads-prefix)

;;; Events Tail Command

;;;###autoload (autoload 'beads-events-tail "beads-command-events" nil t)
(beads-defcommand beads-command-events-tail (beads-command-global-options)
  ((since
    :type (or null string integer)
    :long-option "since"
    :group "Options"
    :level 1
    :order 1
    :documentation "Return records with seq greater than this value")
   (limit
    :type (or null string integer)
    :long-option "limit"
    :group "Options"
    :level 1
    :order 2
    :documentation "Maximum records to return (0 = no limit)")
   (follow
    :type boolean
    :long-option "follow"
    :group "Options"
    :level 2
    :order 1
    :documentation "Keep printing new records as they are committed"))
  :documentation "Print events journal records after a sequence number.
Each record is a durable mutation event (create, update, close, delete,
dep_add, dep_remove, comment) with the acting identity and snapshot."
  :cli-command "events tail"
  :result (list-of beads-event-record))

(cl-defmethod beads-command-parse ((_command beads-command-events-tail) stdout)
  "Parse journal JSON-lines STDOUT into beads-event-record objects.
`bd events tail' emits one JSON object per line in both human and
--json mode, so both routes parse identically."
  (beads-events-records-from-json-lines stdout))

;;; Events Export Command

;;;###autoload (autoload 'beads-events-export "beads-command-events" nil t)
(beads-defcommand beads-command-events-export (beads-command-global-options)
  ((limit
    :type (or null string integer)
    :long-option "limit"
    :group "Options"
    :level 1
    :order 1
    :documentation "Maximum records to return (0 = no limit)"))
  :documentation "Print every events journal record from seq 1.
Equivalent to `bd events tail --since 0'.  Fails rather than present a
pruned journal's surviving suffix as a complete history."
  :cli-command "events export"
  :result (list-of beads-event-record))

(cl-defmethod beads-command-parse ((_command beads-command-events-export) stdout)
  "Parse journal JSON-lines STDOUT into beads-event-record objects."
  (beads-events-records-from-json-lines stdout))

;;; Events Prune Command

;;;###autoload (autoload 'beads-events-prune "beads-command-events" nil t)
(beads-defcommand beads-command-events-prune (beads-command-global-options)
  ((before
    :type (or null string integer)
    :long-option "before"
    :group "Options"
    :level 1
    :order 1
    :required t
    :documentation "Delete records with seq less than this value"))
  :documentation "Delete events journal records below a sequence number.
Retention floors (events-journal-retain-days / -rows) can only reduce
what a prune removes; the journal is clone-local operational state, so
pruning never affects issue data."
  :cli-command "events prune"
  :result beads-events-prune-result)

;;; Parent Router Transient (no class, per project policy)

;;;###autoload (autoload 'beads-events "beads-command-events" nil t)
(beads-define-prefix beads-events ()
  "Read and manage the durable events journal.

Parent transient for the `bd events' group; per project policy the
group itself never gets an EIEIO class."
  [["Journal"
    ("t" "Tail records" beads-events-tail)
    ("e" "Export journal" beads-events-export)
    ("p" "Prune records" beads-events-prune)
    ("q" "Quit" transient-quit-one)]])

(provide 'beads-command-events)
;;; beads-command-events.el ends here
