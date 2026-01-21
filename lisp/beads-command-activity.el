;;; beads-command-activity.el --- Activity command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd activity' operations.
;; Activity shows real-time molecule state feed.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-activity
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-activity (beads-command-json)
    ((follow
      :initarg :follow
      :type boolean
      :initform nil
      :documentation "Stream events in real-time."
      :long-option "follow"
      :short-option "f"
      :option-type :boolean
      :key "f"
      :transient "--follow"
      :class transient-switch
      :argument "--follow"
      :transient-group "Options"
      :level 1
      :order 1)
     (limit
      :initarg :limit
      :type (or null integer)
      :initform nil
      :documentation "Maximum number of events to show (default 100)."
      :long-option "limit"
      :option-type :integer
      :key "l"
      :transient "--limit"
      :class transient-option
      :argument "--limit="
      :prompt "Limit: "
      :transient-group "Options"
      :level 1
      :order 2)
     (mol
      :initarg :mol
      :type (or null string)
      :initform nil
      :documentation "Filter by molecule/issue ID prefix."
      :long-option "mol"
      :option-type :string
      :key "m"
      :transient "--mol"
      :class transient-option
      :argument "--mol="
      :prompt "Molecule/issue prefix: "
      :transient-group "Filters"
      :level 1
      :order 1)
     (since
      :initarg :since
      :type (or null string)
      :initform nil
      :documentation "Show events since duration (e.g., 5m, 1h, 30s)."
      :long-option "since"
      :option-type :string
      :key "s"
      :transient "--since"
      :class transient-option
      :argument "--since="
      :prompt "Since (e.g., 5m, 1h): "
      :transient-group "Filters"
      :level 1
      :order 2)
     (event-type
      :initarg :event-type
      :type (or null string)
      :initform nil
      :documentation "Filter by event type (create, update, delete, comment)."
      :long-option "type"
      :option-type :string
      :key "t"
      :transient "--type"
      :class transient-option
      :argument "--type="
      :prompt "Event type: "
      :choices ("create" "update" "delete" "comment")
      :transient-group "Filters"
      :level 1
      :order 3)
     (town
      :initarg :town
      :type boolean
      :initform nil
      :documentation "Aggregated feed from all rigs."
      :long-option "town"
      :option-type :boolean
      :key "T"
      :transient "--town"
      :class transient-switch
      :argument "--town"
      :transient-group "Options"
      :level 2
      :order 3)
     (interval
      :initarg :interval
      :type (or null string)
      :initform nil
      :documentation "Polling interval for --follow mode (default 500ms)."
      :long-option "interval"
      :option-type :string
      :key "i"
      :transient "--interval"
      :class transient-option
      :argument "--interval="
      :prompt "Interval: "
      :transient-group "Options"
      :level 2
      :order 4))
    :documentation "Represents bd activity command.
  Displays a real-time feed of issue and molecule state changes."))

(cl-defmethod beads-command-subcommand ((_command beads-command-activity))
  "Return \"activity\" as the CLI subcommand."
  "activity")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-activity))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-activity "beads-command-activity" nil t)
(beads-meta-define-transient beads-command-activity "beads-activity"
  "Display real-time activity feed."
  beads-option-global-section)

(provide 'beads-command-activity)
;;; beads-command-activity.el ends here
