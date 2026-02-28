;;; beads-command-federation.el --- Federation command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd federation'
;; operations.  Provides peer-to-peer federation management:
;; - federation add-peer: Add a federation peer
;; - federation remove-peer: Remove a federation peer
;; - federation list-peers: List configured peers
;; - federation sync: Synchronize with peers
;; - federation status: Show federation status

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-federation-add-peer
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-federation-add-peer (beads-command-json)
    ((peer-name
      :initarg :peer-name
      :type (or null string)
      :initform nil
      :documentation "Peer name (positional argument)."
      :positional 1
      :option-type :string
      :key "n"
      :transient "Name (required)"
      :class transient-option
      :argument "--name="
      :prompt "Peer name: "
      :transient-group "Add Peer"
      :level 1
      :order 1
      :required t)
     (url
      :initarg :url
      :type (or null string)
      :initform nil
      :documentation "Peer URL (positional argument).
Formats: dolthub://org/repo, host:port/database, file:///path."
      :positional 2
      :option-type :string
      :key "u"
      :transient "URL (required)"
      :class transient-option
      :argument "--url="
      :prompt "Peer URL: "
      :transient-group "Add Peer"
      :level 1
      :order 2
      :required t)
     (user
      :initarg :user
      :type (or null string)
      :initform nil
      :documentation "SQL username for authentication (-u, --user)."
      :long-option "user"
      :short-option "u"
      :option-type :string
      :key "U"
      :transient "--user"
      :class transient-option
      :argument "--user="
      :prompt "SQL username: "
      :transient-group "Auth"
      :level 2
      :order 3)
     (password
      :initarg :password
      :type (or null string)
      :initform nil
      :documentation "SQL password (-p, --password)."
      :long-option "password"
      :short-option "p"
      :option-type :string
      :key "p"
      :transient "--password"
      :class transient-option
      :argument "--password="
      :prompt "SQL password: "
      :transient-group "Auth"
      :level 2
      :order 4)
     (sovereignty
      :initarg :sovereignty
      :type (or null string)
      :initform nil
      :documentation "Sovereignty tier (--sovereignty).
Values: T1, T2, T3, T4."
      :long-option "sovereignty"
      :option-type :string
      :key "s"
      :transient "--sovereignty"
      :class transient-option
      :argument "--sovereignty="
      :prompt "Sovereignty tier (T1|T2|T3|T4): "
      :transient-group "Options"
      :level 2
      :order 5))
    :documentation "Add a federation peer.
  Requires Dolt backend."))

(cl-defmethod beads-command-subcommand
    ((_command beads-command-federation-add-peer))
  "Return \"federation add-peer\" as the CLI subcommand."
  "federation add-peer")

(cl-defmethod beads-command-validate
    ((command beads-command-federation-add-peer))
  "Validate federation add-peer COMMAND."
  (with-slots (peer-name url) command
    (cond
     ((or (null peer-name) (string-empty-p peer-name))
      "Must provide a peer name")
     ((or (null url) (string-empty-p url))
      "Must provide a peer URL"))))

;;; ============================================================
;;; Command Class: beads-command-federation-remove-peer
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-federation-remove-peer (beads-command-json)
    ((peer-name
      :initarg :peer-name
      :type (or null string)
      :initform nil
      :documentation "Peer name to remove (positional argument)."
      :positional 1
      :option-type :string
      :key "n"
      :transient "Name (required)"
      :class transient-option
      :argument "--name="
      :prompt "Peer name to remove: "
      :transient-group "Remove Peer"
      :level 1
      :order 1
      :required t))
    :documentation "Remove a federation peer.
  Requires Dolt backend."))

(cl-defmethod beads-command-subcommand
    ((_command beads-command-federation-remove-peer))
  "Return \"federation remove-peer\" as the CLI subcommand."
  "federation remove-peer")

(cl-defmethod beads-command-validate
    ((command beads-command-federation-remove-peer))
  "Validate federation remove-peer COMMAND."
  (with-slots (peer-name) command
    (when (or (null peer-name) (string-empty-p peer-name))
      "Must provide a peer name")))

;;; ============================================================
;;; Command Class: beads-command-federation-list-peers
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-federation-list-peers (beads-command-json)
    ()
    :documentation "List configured federation peers.
  Requires Dolt backend."))

(cl-defmethod beads-command-subcommand
    ((_command beads-command-federation-list-peers))
  "Return \"federation list-peers\" as the CLI subcommand."
  "federation list-peers")

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-federation-list-peers))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-federation-sync
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-federation-sync (beads-command-json)
    ((peer
      :initarg :peer
      :type (or null string)
      :initform nil
      :documentation "Specific peer to sync with (--peer).
When nil, syncs with all peers."
      :long-option "peer"
      :option-type :string
      :key "p"
      :transient "--peer"
      :class transient-option
      :argument "--peer="
      :prompt "Peer (empty=all): "
      :transient-group "Options"
      :level 1
      :order 1)
     (strategy
      :initarg :strategy
      :type (or null string)
      :initform nil
      :documentation "Conflict resolution strategy (--strategy).
Values: ours, theirs."
      :long-option "strategy"
      :option-type :string
      :key "s"
      :transient "--strategy"
      :class transient-option
      :argument "--strategy="
      :prompt "Strategy (ours|theirs): "
      :transient-group "Options"
      :level 2
      :order 2))
    :documentation "Synchronize with federation peers.
  Requires Dolt backend."))

(cl-defmethod beads-command-subcommand
    ((_command beads-command-federation-sync))
  "Return \"federation sync\" as the CLI subcommand."
  "federation sync")

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-federation-sync))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-federation-status
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-federation-status (beads-command-json)
    ((peer
      :initarg :peer
      :type (or null string)
      :initform nil
      :documentation "Specific peer to check (--peer).
When nil, shows all peers."
      :long-option "peer"
      :option-type :string
      :key "p"
      :transient "--peer"
      :class transient-option
      :argument "--peer="
      :prompt "Peer (empty=all): "
      :transient-group "Options"
      :level 1
      :order 1))
    :documentation "Show federation sync status.
  Requires Dolt backend."))

(cl-defmethod beads-command-subcommand
    ((_command beads-command-federation-status))
  "Return \"federation status\" as the CLI subcommand."
  "federation status")

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-federation-status))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-federation-add-peer "beads-command-federation" nil t)
(beads-meta-define-transient beads-command-federation-add-peer
  "beads-federation-add-peer"
  "Add a federation peer."
  beads-option-global-section)

;;;###autoload (autoload 'beads-federation-remove-peer "beads-command-federation" nil t)
(beads-meta-define-transient beads-command-federation-remove-peer
  "beads-federation-remove-peer"
  "Remove a federation peer."
  beads-option-global-section)

;;;###autoload (autoload 'beads-federation-list-peers "beads-command-federation" nil t)
(beads-meta-define-transient beads-command-federation-list-peers
  "beads-federation-list-peers"
  "List configured federation peers."
  beads-option-global-section)

;;;###autoload (autoload 'beads-federation-sync "beads-command-federation" nil t)
(beads-meta-define-transient beads-command-federation-sync
  "beads-federation-sync"
  "Synchronize with federation peers."
  beads-option-global-section)

;;;###autoload (autoload 'beads-federation-status "beads-command-federation" nil t)
(beads-meta-define-transient beads-command-federation-status
  "beads-federation-status"
  "Show federation sync status."
  beads-option-global-section)

;;; Parent Menu

;;;###autoload (autoload 'beads-federation "beads-command-federation" nil t)
(transient-define-prefix beads-federation ()
  "Peer-to-peer federation management.

Manage federation peers and synchronize data between
Dolt-backed beads databases."
  [["Peers"
    ("a" "Add peer" beads-federation-add-peer)
    ("r" "Remove peer" beads-federation-remove-peer)
    ("l" "List peers" beads-federation-list-peers)]
   ["Sync"
    ("s" "Sync" beads-federation-sync)
    ("i" "Status" beads-federation-status)
    ("q" "Quit" transient-quit-one)]])

(provide 'beads-command-federation)
;;; beads-command-federation.el ends here
