;;; beads-navigation-test.el --- Tests for the universal navigation contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the universal navigation contract (design.md §3.3):
;; every porcelain mode resolves `q', `g', TAB/S-TAB, SPC, `RET' and
;; `?' to the contract commands, and reserves `C-c b' for downstream
;; extension keys without shadowing a core key.
;;
;; These are keymap-resolution tests: they do not need a live store.

;;; Code:

(require 'ert)
(require 'beads-buffer)
(require 'beads-thing)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-command-epic)
(require 'beads-dashboard)
(require 'beads-agent-list)

(defmacro beads-navigation-test--expect (map key command)
  "Assert that KEY in MAP resolves to COMMAND.
MAP is evaluated once; COMMAND is quoted for readability."
  (declare (indent 2))
  `(let ((m ,map))
     (should (eq (keymap-lookup m ,key) ,command))))

(defun beads-navigation-test--assert-thing-keys (map name)
  "Assert the thing-motion keys resolve in MAP; NAME is for messages."
  (dolist (key '("TAB" "<tab>"))
    (should (eq (keymap-lookup map key) #'beads-thing-forward)))
  (dolist (key '("<backtab>" "S-TAB" "S-<tab>"))
    (should (eq (keymap-lookup map key) #'beads-thing-backward)))
  (should (eq (keymap-lookup map "SPC") #'beads-thing-toggle)))

(defun beads-navigation-test--assert-extension-keys (map name)
  "Assert the reserved C-c b extension prefix in MAP; NAME is for messages."
  (should (keymapp (keymap-lookup map "C-c b")))
  (should (eq (keymap-lookup map "C-c b b")
              #'beads-mode-extension-bead-at-point))
  (should (eq (keymap-lookup map "C-c b ?") #'beads-dispatch))
  ;; The extension prefix must not shadow a reserved core key; `?'
  ;; under the prefix is the one intentional extra (`C-c b ?' dispatch).
  (dolist (key '("q" "g" "TAB" "S-TAB" "SPC" "RET"))
    (should-not (lookup-key beads-mode-extension-map (kbd key)))))

(defun beads-navigation-test--assert-contract (map q g ret name)
  "Assert the full contract for MAP using Q, G and RET; NAME names the mode."
  (beads-navigation-test--assert-thing-keys map name)
  (beads-navigation-test--assert-extension-keys map name)
  (should (eq (keymap-lookup map "q") q))
  (should (eq (keymap-lookup map "g") g))
  (should (eq (keymap-lookup map "RET") ret))
  (should (eq (keymap-lookup map "?") #'beads-dispatch)))

(ert-deftest beads-navigation-test-installer-binds-contract ()
  "The installer wires the contract into a bare keymap."
  :tags '(:unit)
  (let ((map (make-sparse-keymap)))
    (beads-mode--install-navigation-keys map)
    (beads-navigation-test--assert-thing-keys map "bare")
    (beads-navigation-test--assert-extension-keys map "bare")
    (should (eq (keymap-lookup map "?") #'beads-dispatch))))

(ert-deftest beads-navigation-test-list-mode ()
  "The list mode obeys the navigation contract."
  :tags '(:unit)
  (beads-navigation-test--assert-contract
   beads-list-mode-map
   #'beads-list-quit #'beads-list-refresh #'beads-list-show
   "list"))

(ert-deftest beads-navigation-test-show-mode ()
  "The show mode obeys the navigation contract."
  :tags '(:unit)
  (beads-navigation-test--assert-contract
   beads-show-mode-map
   #'quit-window #'beads-refresh-show #'beads-show-follow-reference
   "show"))

(ert-deftest beads-navigation-test-dashboard-mode ()
  "The dashboard mode obeys the navigation contract."
  :tags '(:unit)
  (beads-navigation-test--assert-contract
   beads-dashboard-mode-map
   #'quit-window #'beads-dashboard-refresh-dispatch
   #'beads-dashboard-visit-at-point
   "dashboard"))

(ert-deftest beads-navigation-test-agent-list-mode ()
  "The agent-list mode obeys the navigation contract."
  :tags '(:unit)
  (beads-navigation-test--assert-contract
   beads-agent-list-mode-map
   #'beads-agent-list-quit #'beads-agent-list-refresh
   #'beads-agent-list-jump
   "agent-list"))

(ert-deftest beads-navigation-test-epic-status-mode ()
  "The epic status mode obeys the navigation contract."
  :tags '(:unit)
  (beads-navigation-test--assert-contract
   beads-epic-status-mode-map
   #'quit-window #'beads-epic-status-refresh
   #'beads-epic-status-show-at-point
   "epic-status"))

(ert-deftest beads-navigation-test-section-mode ()
  "The section base mode installs the thing, dispatch and extension keys."
  :tags '(:unit)
  (beads-navigation-test--assert-thing-keys beads-section-mode-map "section")
  (beads-navigation-test--assert-extension-keys beads-section-mode-map "section")
  (should (eq (keymap-lookup beads-section-mode-map "RET")
              #'beads-section-visit-issue))
  (should (eq (keymap-lookup beads-section-mode-map "?") #'beads-dispatch)))

(ert-deftest beads-navigation-test-dispatch-delegates-in-show ()
  "`beads-dispatch' opens the show quick-actions in a show buffer."
  :tags '(:unit)
  (let (called)
    (cl-letf (((symbol-function 'beads-show-actions)
               (lambda () (setq called t))))
      (with-temp-buffer
        (beads-show-mode)
        (beads-dispatch)))
    (should called)))

(ert-deftest beads-navigation-test-dispatch-opens-main-menu ()
  "`beads-dispatch' opens the main transient outside a show buffer."
  :tags '(:unit)
  (let (called)
    (cl-letf (((symbol-function 'beads)
               (lambda () (setq called t))))
      (beads-dispatch))
    (should called)))

(provide 'beads-navigation-test)
;;; beads-navigation-test.el ends here
