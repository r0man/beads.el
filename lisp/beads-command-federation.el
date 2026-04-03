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

(beads-defcommand beads-command-federation-add-peer (beads-command-global-options)
  ((peer-name
    :positional 1
    :option-type :string
    :short-option "n"
    :argument "--name="
    :prompt "Peer name: "
    :group "Add Peer"
    :level 1
    :order 1
    :required t)
   (url
    :positional 2
    :option-type :string
    :short-option "u"
    :argument "--url="
    :prompt "Peer URL: "
    :group "Add Peer"
    :level 1
    :order 2
    :required t)
   (user
    :short-option "u"
    :option-type :string
    :transient-key "U"
    :prompt "SQL username: "
    :group "Auth"
    :level 2
    :order 3)
   (password
    :short-option "p"
    :option-type :string
    :prompt "SQL password: "
    :group "Auth"
    :level 2
    :order 4)
   (sovereignty
    :option-type :string
    :short-option "s"
    :prompt "Sovereignty tier (T1|T2|T3|T4): "
    :group "Options"
    :level 2
    :order 5))
  :documentation "Add a federation peer.
Requires Dolt backend."
  :cli-command "federation add-peer")

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

(beads-defcommand beads-command-federation-remove-peer (beads-command-global-options)
  ((peer-name
    :positional 1
    :option-type :string
    :short-option "n"
    :argument "--name="
    :prompt "Peer name to remove: "
    :group "Remove Peer"
    :level 1
    :order 1
    :required t))
  :documentation "Remove a federation peer.
Requires Dolt backend."
  :cli-command "federation remove-peer")

(cl-defmethod beads-command-validate
    ((command beads-command-federation-remove-peer))
  "Validate federation remove-peer COMMAND."
  (with-slots (peer-name) command
    (when (or (null peer-name) (string-empty-p peer-name))
      "Must provide a peer name")))

;;; ============================================================
;;; Command Class: beads-command-federation-list-peers
;;; ============================================================

(beads-defcommand beads-command-federation-list-peers (beads-command-global-options)
  ()
  :documentation "List configured federation peers.
Requires Dolt backend."
  :cli-command "federation list-peers")

;;; ============================================================
;;; Command Class: beads-command-federation-sync
;;; ============================================================

(beads-defcommand beads-command-federation-sync (beads-command-global-options)
  ((peer
    :option-type :string
    :short-option "p"
    :prompt "Peer (empty=all): "
    :group "Options"
    :level 1
    :order 1)
   (strategy
    :option-type :string
    :short-option "s"
    :prompt "Strategy (ours|theirs): "
    :group "Options"
    :level 2
    :order 2))
  :documentation "Synchronize with federation peers.
Requires Dolt backend.")


(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-federation-sync))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-federation-status
;;; ============================================================

(beads-defcommand beads-command-federation-status (beads-command-global-options)
  ((peer
    :option-type :string
    :short-option "p"
    :prompt "Peer (empty=all): "
    :group "Options"
    :level 1
    :order 1))
  :documentation "Show federation sync status.
Requires Dolt backend.")


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
