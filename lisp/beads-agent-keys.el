;;; beads-agent-keys.el --- Agent keybinding prefix map -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;;; Commentary:

;; Defines `beads-agent-prefix-map', a keymap that groups all agent
;; commands under a single prefix key (`a' in list/show modes).
;;
;; This avoids polluting mode-maps with 8 uppercase single-letter
;; bindings (T, R, P, Q, C, X, J, A) and frees those keys for
;; issue-related commands in later phases of the UX redesign.

;;; Code:

(declare-function beads-agent-start-at-point "beads-agent")
(declare-function beads-agent-start-task "beads-agent" (&optional arg))
(declare-function beads-agent-start-review "beads-agent" (&optional arg))
(declare-function beads-agent-start-plan "beads-agent" (&optional arg))
(declare-function beads-agent-start-qa "beads-agent" (&optional arg))
(declare-function beads-agent-start-custom "beads-agent" (&optional arg))
(declare-function beads-agent-stop-at-point "beads-agent")
(declare-function beads-agent-jump-at-point "beads-agent")

(defvar beads-agent-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'beads-agent-start-task)
    (define-key map (kbd "r") #'beads-agent-start-review)
    (define-key map (kbd "p") #'beads-agent-start-plan)
    (define-key map (kbd "q") #'beads-agent-start-qa)
    (define-key map (kbd "c") #'beads-agent-start-custom)
    (define-key map (kbd "x") #'beads-agent-stop-at-point)
    (define-key map (kbd "j") #'beads-agent-jump-at-point)
    (define-key map (kbd "a") #'beads-agent-start-at-point)
    map)
  "Prefix keymap for agent commands.
Bound to `a' in `beads-list-mode-map' and `beads-show-mode-map'.")

(provide 'beads-agent-keys)

;;; beads-agent-keys.el ends here
