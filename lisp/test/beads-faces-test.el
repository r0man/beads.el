;;; beads-faces-test.el --- One palette and glyph set (WI-17) -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `beads-faces.el' (REQ-017, REQ-018):
;;
;; - face coverage: every documented `beads-face-*' exists and every
;;   semantic module face derives from one of them with `:inherit';
;; - one glyph set: the status glyphs rendered by the show buffer, the
;;   status lookup and the agent outcome marks all resolve to the
;;   canonical glyphs defined in one place.

;;; Code:

(require 'ert)
(require 'beads-faces)
(require 'beads-agent-display)
(require 'beads-agent-list)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-command-epic)
(require 'beads-command-formula)
(require 'beads-section)

;;; Helpers

(defun beads-faces-test--inherits (face)
  "Return the `:inherit' value of FACE's `defface' specification, or nil."
  (let ((spec (get face 'face-defface-spec)))
    (plist-get (cdr (car spec)) :inherit)))

;;; Face coverage

(ert-deftest beads-faces-test-palette-faces-exist ()
  "Every face named in the documented palette exists."
  :tags '(:unit)
  (dolist (face beads-face-palette)
    (should (facep face))))

(ert-deftest beads-faces-test-palette-is-the-documented-set ()
  "The palette inventory is exactly the documented `beads-face-*' names."
  :tags '(:unit)
  (should (equal beads-face-palette
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
                   beads-face-error))))

(ert-deftest beads-faces-test-module-faces-derive-from-palette ()
  "Semantic module faces are aliases that `:inherit' a palette face."
  :tags '(:unit)
  (let ((mapping
         '((beads-issue-line . beads-face-issue-line)
           (beads-list-status-open . beads-face-status-open)
           (beads-list-status-in-progress . beads-face-status-in-progress)
           (beads-list-status-blocked . beads-face-status-blocked)
           (beads-list-status-closed . beads-face-status-closed)
           (beads-list-priority-critical . beads-face-priority-critical)
           (beads-list-priority-high . beads-face-priority-high)
           (beads-list-priority-medium . beads-face-priority-medium)
           (beads-list-priority-low . beads-face-priority-low)
           (beads-list-agent-working . beads-face-agent-running)
           (beads-list-agent-finished . beads-face-success)
           (beads-list-agent-failed . beads-face-agent-failed)
           (beads-show-status-open-face . beads-face-status-open)
           (beads-show-status-in-progress-face . beads-face-status-in-progress)
           (beads-show-status-blocked-face . beads-face-status-blocked)
           (beads-show-status-closed-face . beads-face-status-closed)
           (beads-show-priority-critical-face . beads-face-priority-critical)
           (beads-show-priority-high-face . beads-face-priority-high)
           (beads-show-priority-medium-face . beads-face-priority-medium)
           (beads-show-priority-low-face . beads-face-priority-low)
           (beads-show-header-face . beads-face-section)
           (beads-epic-status-status-open-face . beads-face-status-open)
           (beads-epic-status-status-in-progress-face . beads-face-status-in-progress)
           (beads-epic-status-status-blocked-face . beads-face-status-blocked)
           (beads-epic-status-status-closed-face . beads-face-status-closed)
           (beads-epic-status-progress-low-face . beads-face-error)
           (beads-epic-status-progress-medium-face . beads-face-warning)
           (beads-epic-status-progress-high-face . beads-face-success)
           (beads-epic-status-id-face . beads-face-id)
           (beads-formula-header-face . beads-face-section)
           (beads-formula-title-face . beads-face-header)
           (beads-agent-list-running . beads-face-agent-running)
           (beads-agent-list-stale . beads-face-agent-idle)
           (beads-agent-list-finished . beads-face-success)
           (beads-agent-list-failed . beads-face-agent-failed))))
    (dolist (entry mapping)
      (let ((face (car entry))
            (palette-face (cdr entry)))
        (should (facep face))
        (should (memq palette-face beads-face-palette))
        (should (eq (beads-faces-test--inherits face) palette-face))))))

;;; Status glyphs

(ert-deftest beads-faces-test-status-glyphs ()
  "The one status glyph set is `○ ◐ ⛔ ✓'."
  :tags '(:unit)
  (should (equal (beads-face-status-glyph "open") "○"))
  (should (equal (beads-face-status-glyph "in_progress") "◐"))
  (should (equal (beads-face-status-glyph "blocked") "⛔"))
  (should (equal (beads-face-status-glyph "closed") "✓"))
  (should (equal (beads-face-status-glyph "unknown") "?")))

(ert-deftest beads-faces-test-status-glyphs-distinct ()
  "No two statuses share a glyph."
  :tags '(:unit)
  (let ((glyphs (mapcar (lambda (status)
                          (beads-face-status-glyph status))
                        '("open" "in_progress" "blocked" "closed"))))
    (should (= (length glyphs) (length (delete-dups (copy-sequence glyphs)))))))

(ert-deftest beads-faces-test-show-uses-canonical-status-glyphs ()
  "The show buffer renders the same status glyph as the palette."
  :tags '(:unit)
  (dolist (status '("open" "in_progress" "blocked" "closed" "unknown"))
    (should (equal (beads-show--status-icon status)
                   (beads-face-status-glyph status)))))

(ert-deftest beads-faces-test-status-face-mapping ()
  "Statuses map to canonical faces; unknown falls back to `default'."
  :tags '(:unit)
  (should (eq (beads-face-status-face "open") 'beads-face-status-open))
  (should (eq (beads-face-status-face "in_progress") 'beads-face-status-in-progress))
  (should (eq (beads-face-status-face "blocked") 'beads-face-status-blocked))
  (should (eq (beads-face-status-face "closed") 'beads-face-status-closed))
  (should (eq (beads-face-status-face "nope") 'default)))

(ert-deftest beads-faces-test-list-and-show-faces-match-palette ()
  "List and show status/priority lookups resolve to palette-backed faces."
  :tags '(:unit)
  (should (eq (beads-faces-test--inherits
               (beads-list--status-face "open"))
              'beads-face-status-open))
  (should (eq (beads-faces-test--inherits
               (beads-list--priority-face 0))
              'beads-face-priority-critical))
  (should (eq (beads-faces-test--inherits
               (beads-show--status-face "blocked"))
              'beads-face-status-blocked)))

;;; Priority glyphs and faces

(ert-deftest beads-faces-test-priority-glyphs ()
  "Priority glyphs are `P0'..`P4'."
  :tags '(:unit)
  (should (equal (beads-face-priority-glyph 0) "P0"))
  (should (equal (beads-face-priority-glyph 2) "P2"))
  (should (equal (beads-face-priority-glyph 4) "P4"))
  (should (equal (beads-face-priority-glyph nil) "")))

(ert-deftest beads-faces-test-priority-face-mapping ()
  "Priorities map to canonical faces."
  :tags '(:unit)
  (should (eq (beads-face-priority-face 0) 'beads-face-priority-critical))
  (should (eq (beads-face-priority-face 1) 'beads-face-priority-high))
  (should (eq (beads-face-priority-face 2) 'beads-face-priority-medium))
  (should (eq (beads-face-priority-face 3) 'beads-face-priority-low))
  (should (eq (beads-face-priority-face 4) 'beads-face-priority-low))
  (should (eq (beads-face-priority-face 9) 'default)))

;;; Agent glyphs and faces

(ert-deftest beads-faces-test-agent-outcome-glyphs ()
  "Agent outcome prefixes come from the one glyph set."
  :tags '(:unit)
  (should (equal (beads-face-agent-outcome-glyph 'running) ""))
  (should (equal (beads-face-agent-outcome-glyph 'touched) ""))
  (should (equal (beads-face-agent-outcome-glyph 'stopped) ""))
  (should (equal (beads-face-agent-outcome-glyph 'finished) "✓"))
  (should (equal (beads-face-agent-outcome-glyph 'failed) "✗"))
  (should (equal beads-agent-display--outcome-mark-finished
                 (beads-face-agent-outcome-glyph 'finished)))
  (should (equal beads-agent-display--outcome-mark-failed
                 (beads-face-agent-outcome-glyph 'failed))))

(ert-deftest beads-faces-test-agent-state-face-mapping ()
  "Agent states map to canonical faces."
  :tags '(:unit)
  (should (eq (beads-face-agent-state-face 'running) 'beads-face-agent-running))
  (should (eq (beads-face-agent-state-face 'touched) 'beads-face-agent-idle))
  (should (eq (beads-face-agent-state-face 'stopped) 'beads-face-agent-idle))
  (should (eq (beads-face-agent-state-face 'stale) 'beads-face-agent-idle))
  (should (eq (beads-face-agent-state-face 'finished) 'beads-face-success))
  (should (eq (beads-face-agent-state-face 'failed) 'beads-face-agent-failed)))

(provide 'beads-faces-test)

;;; beads-faces-test.el ends here
