;;; beads-faces.el --- The one beads.el face palette -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; The one face palette (REQ-017).  Every beads.el face that renders
;; issue status, priority, agent state, headers, ids, keys or
;; validation messages derives from a canonical `beads-face-*' face
;; defined here via `:inherit'.  A module never re-invents a colour:
;; it either uses a canonical face directly or defines a local face
;; that inherits one.
;;
;; The symbol names are the public contract, documented in
;; `menu-mockups.md' section 13.  Extensions derive their own faces
;; with `:inherit' -- there is no face hook:
;;
;;   (defface my-city-face ((t (:inherit beads-face-header)))
;;     "Header for my city.")
;;
;; This module defines the palette and the shared status/priority/agent
;; glyph mappings so every surface spells a status the same way
;; (REQ-017) with the same face (REQ-018).  It deliberately requires
;; only `beads-custom' so it can be loaded by any module without
;; creating a cycle.

;;; Code:

(require 'beads-custom)

;;; Faces

(defface beads-face-header
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for buffer and section titles."
  :group 'beads)

(defface beads-face-section
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section header lines."
  :group 'beads)

(defface beads-face-issue-line
  '((t :inherit default))
  "Face for clickable issue rows."
  :group 'beads)

(defface beads-face-id
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for bead ids."
  :group 'beads)

(defface beads-face-key
  '((t :inherit shadow))
  "Face for key hints."
  :group 'beads)

(defface beads-face-status-open
  '((t :inherit success))
  "Face for the `open' status."
  :group 'beads)

(defface beads-face-status-in-progress
  '((t :inherit warning))
  "Face for the `in_progress' status."
  :group 'beads)

(defface beads-face-status-blocked
  '((t :inherit error))
  "Face for the `blocked' status."
  :group 'beads)

(defface beads-face-status-closed
  '((t :inherit shadow))
  "Face for the `closed' status."
  :group 'beads)

(defface beads-face-priority-critical
  '((t :inherit error :weight bold))
  "Face for priority 0 (critical)."
  :group 'beads)

(defface beads-face-priority-high
  '((t :inherit warning :weight bold))
  "Face for priority 1 (high)."
  :group 'beads)

(defface beads-face-priority-medium
  '((t :inherit default))
  "Face for priority 2 (medium)."
  :group 'beads)

(defface beads-face-priority-low
  '((t :inherit shadow))
  "Face for priority 3-4 (low/backlog)."
  :group 'beads)

(defface beads-face-agent-running
  '((t :inherit success :weight bold))
  "Face for a running agent state."
  :group 'beads)

(defface beads-face-agent-idle
  '((t :inherit shadow))
  "Face for an idle (touched, stopped or stale) agent state."
  :group 'beads)

(defface beads-face-agent-failed
  '((t :inherit error :weight bold))
  "Face for a failed agent state."
  :group 'beads)

(defface beads-face-success
  '((t :inherit success))
  "Face for success footers and validation."
  :group 'beads)

(defface beads-face-warning
  '((t :inherit warning))
  "Face for warning footers and validation."
  :group 'beads)

(defface beads-face-error
  '((t :inherit error))
  "Face for error footers and validation."
  :group 'beads)

;; Standalone formula/molecule/swarm surfaces (WI-SF-12, design.md §13b).
;; Each derives from a canonical palette face so there is still one
;; palette; these only name the new semantic roles.

(defface beads-face-molecule-root
  '((t :inherit beads-face-header :weight bold))
  "Face for a molecule root (persistent `◆' or vapor `◇')."
  :group 'beads)

(defface beads-face-molecule-step
  '((t :inherit beads-face-issue-line))
  "Face for a molecule step row."
  :group 'beads)

(defface beads-face-gate
  '((t :inherit beads-face-key))
  "Face for a gate type glyph or label."
  :group 'beads)

(defface beads-face-swarm-coordinator
  '((t :inherit beads-face-header :weight bold))
  "Face for a swarm molecule or its coordinator."
  :group 'beads)

(defface beads-face-swarm-lane
  '((t :inherit beads-face-issue-line))
  "Face for a swarm worker/parallelism lane."
  :group 'beads)

;;; Palette inventory

(defconst beads-face-palette
  '(beads-face-header
    beads-face-section
    beads-face-issue-line
    beads-face-id
    beads-face-key
    beads-face-status-open
    beads-face-status-in-progress
    beads-face-status-blocked
    beads-face-status-closed
    beads-face-priority-critical
    beads-face-priority-high
    beads-face-priority-medium
    beads-face-priority-low
    beads-face-agent-running
    beads-face-agent-idle
    beads-face-agent-failed
    beads-face-success
    beads-face-warning
    beads-face-error
    beads-face-molecule-root
    beads-face-molecule-step
    beads-face-gate
    beads-face-swarm-coordinator
    beads-face-swarm-lane)
  "The documented `beads-face-*' palette.
This is the public contract: extensions derive faces from these with
`:inherit'.  It is also the list the face-coverage test walks, so a
new canonical face must be added here or it is not part of the
palette.")

;;; Glyphs

(defconst beads-face-status-glyphs
  '(("open" . "○")
    ("in_progress" . "◐")
    ("blocked" . "⛔")
    ("closed" . "✓"))
  "Canonical issue-status glyphs, keyed by the bd status string.
Every surface that renders a status glyph reads it from here so the
one glyph set holds across list, detail, formula and agent views.")

(defconst beads-face-agent-outcome-glyphs
  '((running . "")
    (touched . "")
    (stopped . "")
    (finished . "✓")
    (failed . "✗"))
  "Canonical agent outcome prefix glyphs, keyed by state symbol.
`running', `touched' and `stopped' have no prefix; terminal states
carry a single-cell mark.")

;;; Mappings

(defun beads-face-status-glyph (status)
  "Return the canonical glyph for issue STATUS.
STATUS is the bd status string (`open', `in_progress', `blocked',
`closed').  Unknown statuses render as \"?\"."
  (or (cdr (assoc status beads-face-status-glyphs)) "?"))

(defun beads-face-agent-outcome-glyph (state)
  "Return the canonical outcome prefix for agent STATE.
STATE is one of `running', `touched', `stopped', `finished',
`failed'.  Unknown states render with no prefix."
  (or (cdr (assq state beads-face-agent-outcome-glyphs)) ""))

(defun beads-face-priority-glyph (priority)
  "Return the canonical priority glyph for PRIORITY.
PRIORITY is a number 0-4; nil or a non-number renders as an empty
string.  The glyph is `P0'..`P4'."
  (if (numberp priority) (format "P%d" priority) ""))

(defun beads-face-status-face (status)
  "Return the canonical status face for STATUS."
  (pcase status
    ("open" 'beads-face-status-open)
    ("in_progress" 'beads-face-status-in-progress)
    ("blocked" 'beads-face-status-blocked)
    ("closed" 'beads-face-status-closed)
    (_ 'default)))

(defun beads-face-priority-face (priority)
  "Return the canonical priority face for PRIORITY."
  (pcase priority
    (0 'beads-face-priority-critical)
    (1 'beads-face-priority-high)
    (2 'beads-face-priority-medium)
    ((or 3 4) 'beads-face-priority-low)
    (_ 'default)))

(defun beads-face-agent-state-face (state)
  "Return the canonical agent face for STATE.
STATE is a `beads-agent' state symbol.  `finished' maps to
`beads-face-success' rather than a running face."
  (pcase state
    ('running 'beads-face-agent-running)
    ((or 'touched 'stopped 'stale) 'beads-face-agent-idle)
    ('finished 'beads-face-success)
    ('failed 'beads-face-agent-failed)
    (_ 'beads-face-agent-running)))

(provide 'beads-faces)

;;; beads-faces.el ends here
