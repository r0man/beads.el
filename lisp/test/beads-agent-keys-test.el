;;; beads-agent-keys-test.el --- Tests for agent key relocation -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;;; Commentary:

;; Tests for the agent key prefix map (`a' prefix in list/show modes).
;; Verifies that agent commands are accessible via `a' prefix and that
;; old single-letter bindings (T, R, P, Q, C, X, J, A) are removed.

;;; Code:

(require 'ert)
(require 'beads-command-list)
(require 'beads-command-show)

;;; Agent prefix map tests

(ert-deftest beads-agent-keys-test-prefix-map-exists ()
  "Test that `beads-agent-prefix-map' is defined as a keymap."
  (should (boundp 'beads-agent-prefix-map))
  (should (keymapp beads-agent-prefix-map)))

(ert-deftest beads-agent-keys-test-prefix-bindings ()
  "Test that all agent commands are bound in the prefix map."
  (should (eq (lookup-key beads-agent-prefix-map (kbd "t"))
              #'beads-agent-start-task))
  (should (eq (lookup-key beads-agent-prefix-map (kbd "r"))
              #'beads-agent-start-review))
  (should (eq (lookup-key beads-agent-prefix-map (kbd "p"))
              #'beads-agent-start-plan))
  (should (eq (lookup-key beads-agent-prefix-map (kbd "q"))
              #'beads-agent-start-qa))
  (should (eq (lookup-key beads-agent-prefix-map (kbd "c"))
              #'beads-agent-start-custom))
  (should (eq (lookup-key beads-agent-prefix-map (kbd "x"))
              #'beads-agent-stop-at-point))
  (should (eq (lookup-key beads-agent-prefix-map (kbd "j"))
              #'beads-agent-jump-at-point))
  (should (eq (lookup-key beads-agent-prefix-map (kbd "a"))
              #'beads-agent-start-at-point)))

;;; List-mode "a" prefix tests

(ert-deftest beads-agent-keys-test-list-mode-a-prefix ()
  "Test that `a' in list-mode opens the agent prefix map."
  (should (eq (lookup-key beads-list-mode-map (kbd "a"))
              beads-agent-prefix-map)))

(ert-deftest beads-agent-keys-test-list-mode-a-t ()
  "Test that `a t' in list-mode starts task agent."
  (should (eq (lookup-key beads-list-mode-map (kbd "a t"))
              #'beads-agent-start-task)))

(ert-deftest beads-agent-keys-test-list-mode-a-r ()
  "Test that `a r' in list-mode starts review agent."
  (should (eq (lookup-key beads-list-mode-map (kbd "a r"))
              #'beads-agent-start-review)))

(ert-deftest beads-agent-keys-test-list-mode-a-x ()
  "Test that `a x' in list-mode stops agent."
  (should (eq (lookup-key beads-list-mode-map (kbd "a x"))
              #'beads-agent-stop-at-point)))

(ert-deftest beads-agent-keys-test-list-mode-a-j ()
  "Test that `a j' in list-mode jumps to agent."
  (should (eq (lookup-key beads-list-mode-map (kbd "a j"))
              #'beads-agent-jump-at-point)))

;;; Show-mode "a" prefix tests

(ert-deftest beads-agent-keys-test-show-mode-a-prefix ()
  "Test that `a' in show-mode opens the agent prefix map."
  (should (eq (lookup-key beads-show-mode-map (kbd "a"))
              beads-agent-prefix-map)))

(ert-deftest beads-agent-keys-test-show-mode-a-t ()
  "Test that `a t' in show-mode starts task agent."
  (should (eq (lookup-key beads-show-mode-map (kbd "a t"))
              #'beads-agent-start-task)))

(ert-deftest beads-agent-keys-test-show-mode-a-r ()
  "Test that `a r' in show-mode starts review agent."
  (should (eq (lookup-key beads-show-mode-map (kbd "a r"))
              #'beads-agent-start-review)))

(ert-deftest beads-agent-keys-test-show-mode-a-x ()
  "Test that `a x' in show-mode stops agent."
  (should (eq (lookup-key beads-show-mode-map (kbd "a x"))
              #'beads-agent-stop-at-point)))

(ert-deftest beads-agent-keys-test-show-mode-a-j ()
  "Test that `a j' in show-mode jumps to agent."
  (should (eq (lookup-key beads-show-mode-map (kbd "a j"))
              #'beads-agent-jump-at-point)))

;;; Old single-letter bindings removed

(ert-deftest beads-agent-keys-test-list-mode-old-keys-removed ()
  "Test that old single-letter agent keys are removed from list-mode."
  (dolist (key '("T" "R" "P" "Q" "C" "X" "J" "A"))
    (let ((binding (lookup-key beads-list-mode-map (kbd key))))
      (should-not (and binding
                       (symbolp binding)
                       (string-match-p "beads-agent" (symbol-name binding)))))))

(ert-deftest beads-agent-keys-test-show-mode-old-keys-removed ()
  "Test that old single-letter agent keys are removed from show-mode."
  (dolist (key '("T" "R" "P" "Q" "C" "X" "J" "A"))
    (let ((binding (lookup-key beads-show-mode-map (kbd key))))
      (should-not (and binding
                       (symbolp binding)
                       (string-match-p "beads-agent" (symbol-name binding)))))))

(provide 'beads-agent-keys-test)

;;; beads-agent-keys-test.el ends here
