;;; beads-discoverability-test.el --- Discoverability pass tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Guards the discoverability contract: every primary view binds `?' to
;; the shared `beads-dispatch' help/dispatch menu and advertises that
;; entry in its footer/header-line via `beads-help-hint' (see
;; `docs/key-registry.md').  This complements
;; `beads-key-registry-test.el', which asserts the reserved-key table
;; against the raw mode maps; several views install the universal
;; navigation contract when the mode runs, so this test invokes each
;; mode and inspects its effective local map.

;;; Code:

(require 'ert)
(require 'beads-buffer)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-dashboard)
(require 'beads-agent-list)
(require 'beads-events)
(require 'beads-events-history)
(require 'beads-events-rewind)
(require 'beads-molecule)
(require 'beads-command-epic)
(require 'beads-command-formula)
(require 'beads-wisp)
(require 'beads-gate)
(require 'beads-swarm)

(defconst beads-discoverability-test-primary-modes
  '(beads-list-mode
    beads-show-mode
    beads-dashboard-mode
    beads-agent-list-mode
    beads-events-mode
    beads-events-history-mode
    beads-events-rewind-mode
    beads-molecule-mode
    beads-formula-list-mode
    beads-formula-show-mode
    beads-wisp-list-mode
    beads-epic-status-mode
    beads-gate-list-mode
    beads-gate-detail-mode
    beads-swarm-list-mode
    beads-swarm-status-mode
    beads-swarm-waves-mode)
  "Primary beads.el view modes that must bind `?' to `beads-dispatch'.
One element per audited view; the discoverability pass added the
formula list/show and swarm waves views, which previously inherited
`describe-mode' from their parent maps.")

(ert-deftest beads-discoverability-test-primary-views-bind-help ()
  "Every primary view binds `?' to `beads-dispatch'."
  :tags '(:unit)
  (dolist (mode beads-discoverability-test-primary-modes)
    (should (fboundp mode))
    (with-temp-buffer
      (funcall mode)
      (should (eq (lookup-key (current-local-map) (kbd "?"))
                  #'beads-dispatch)))))

(ert-deftest beads-discoverability-test-help-hint ()
  "`beads-help-hint' names the verbs and always ends with the help entry."
  :tags '(:unit)
  (should (equal beads-help-hint-entry "? dispatch"))
  (should (equal (beads-help-hint) "? dispatch"))
  (should (equal (beads-help-hint "g refresh" "RET visit")
                 "g refresh · RET visit · ? dispatch")))

(ert-deftest beads-discoverability-test-headers-name-help ()
  "The callable header/mode-line builders advertise the `?' help entry."
  :tags '(:unit)
  (with-temp-buffer
    (beads-list-mode)
    (should (string-match-p "\\? dispatch" (beads-list--mode-line))))
  (with-temp-buffer
    (beads-events-mode)
    (should (string-match-p "\\? dispatch" (beads-events--header-line))))
  (with-temp-buffer
    (beads-events-rewind-mode)
    (should (string-match-p "\\? dispatch" (beads-events-rewind--header-line))))
  (with-temp-buffer
    (should (string-match-p "\\? dispatch" (beads-events-history--header-line)))))

(provide 'beads-discoverability-test)
;;; beads-discoverability-test.el ends here
