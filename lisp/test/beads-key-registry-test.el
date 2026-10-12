;;; beads-key-registry-test.el --- Effective-binding key registry test -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Enforces `docs/key-registry.md', the beads.el half of the shared
;; cross-package key registry (plan F1/F11).  The test asserts the
;; *effective* (post-inheritance) binding of every reserved key in the
;; mode's keymap, so a parent redefinition cannot silently re-point a
;; contract key.  The gascity.el registry test asserts the matching
;; cross-package rule from its side.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-section)
(require 'beads-dashboard)

(defconst beads-key-registry-test--bindings
  '((beads-list-mode-map
     ("q" . beads-list-quit)
     ("g" . beads-list-refresh)
     ("TAB" . beads-thing-forward)
     ("<backtab>" . beads-thing-backward)
     ("SPC" . beads-thing-toggle)
     ("RET" . beads-list-show)
     ("n" . beads-list-next)
     ("p" . beads-list-previous)
     ("N" . beads-list-next-section)
     ("P" . beads-list-previous-section)
     ("?" . beads-dispatch)
     ("d" . beads-actions-close)
     ("s" . beads-actions-set-status)
     ("C" . beads-actions-claim)
     ("#" . beads-actions-set-priority)
     ("w" . beads-list-copy-id)
     ("x" . beads-list-clear-filter))
    (beads-show-mode-map
     ("q" . quit-window)
     ("g" . beads-refresh-show)
     ("TAB" . beads-thing-forward)
     ("<backtab>" . beads-thing-backward)
     ("SPC" . beads-thing-toggle)
     ("RET" . beads-show-follow-reference)
     ("n" . beads-show-next-section)
     ("p" . beads-show-previous-section)
     ("?" . beads-dispatch)
     ("d" . beads-actions-close)
     ("s" . beads-actions-set-status)
     ("C" . beads-actions-claim)
     ("#" . beads-actions-set-priority)
     ("w" . beads-show-copy-id))
    (beads-dashboard-mode-map
     ("q" . quit-window)
     ("g" . beads-dashboard-refresh-dispatch)
     ("TAB" . beads-thing-forward)
     ("<backtab>" . beads-thing-backward)
     ("SPC" . beads-thing-toggle)
     ("RET" . beads-dashboard-visit-at-point)
     ("n" . beads-dashboard-next-item)
     ("p" . beads-dashboard-previous-item)
     ("N" . beads-dashboard-next-section)
     ("P" . beads-dashboard-previous-section)
     ("?" . beads-dispatch))
    (beads-section-mode-map
     ("q" . beads-section-quit)
     ("g" . beads-section-refresh)
     ("TAB" . beads-thing-forward)
     ("<backtab>" . beads-thing-backward)
     ("SPC" . beads-thing-toggle)
     ("RET" . beads-section-visit-issue)
     ("?" . beads-dispatch)))
  "Effective (mode-map . (key . command)) bindings from `docs/key-registry.md'.")

(ert-deftest beads-key-registry-test-effective-bindings ()
  "Every reserved key resolves to its registered command in its mode.
Keyed by at-point type, not by mode: a bead row and a session row may
share a key only when the verb is the same.  See `docs/key-registry.md'."
  :tags '(:unit)
  (dolist (entry beads-key-registry-test--bindings)
    (let ((map (symbol-value (car entry))))
      (dolist (pair (cdr entry))
        (should (eq (lookup-key map (kbd (car pair))) (cdr pair)))))))

(ert-deftest beads-key-registry-test-close-key-is-d ()
  "`d' closes in list and show; `k' no longer closes (F8)."
  :tags '(:unit)
  (should (eq (lookup-key beads-list-mode-map (kbd "d"))
              #'beads-actions-close))
  (should (eq (lookup-key beads-show-mode-map (kbd "d"))
              #'beads-actions-close))
  (should-not (eq (lookup-key beads-list-mode-map (kbd "k"))
                  #'beads-actions-close))
  (should-not (lookup-key beads-list-mode-map (kbd "k"))))

(ert-deftest beads-key-registry-test-x-is-clear-filter-only ()
  "`x' is clear-filter in list mode and nothing else (F8)."
  :tags '(:unit)
  (should (eq (lookup-key beads-list-mode-map (kbd "x"))
              #'beads-list-clear-filter)))

(provide 'beads-key-registry-test)
;;; beads-key-registry-test.el ends here
