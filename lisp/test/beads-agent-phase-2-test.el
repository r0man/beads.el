;;; beads-agent-phase-2-test.el --- Phase 2 acceptance gates -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Acceptance gates for Phase 2 (bde-xle9.4): the terminal
;; `beads-agent-backend-claude' is registered and selectable but NOT
;; wired as any per-type default (opt-in); the `efrit' backend is
;; fully removed; `beads-reader-terminal' resolves a real terminal
;; class.  The per-wrapper system-prompt seam wiring is spike-gated on
;; upstream source that is absent in this environment, so every
;; wrapper holds the Phase 1a-i concat shim (documented in NEWS); no
;; assertion here depends on an unverified upstream seam.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-backend)
(require 'beads-agent-backend-terminal)
(require 'beads-agent-types)
(require 'beads-reader)

;;; claude backend registered, opt-in

(ert-deftest beads-agent-phase-2-test-claude-registered ()
  "`beads-agent-backend-claude' is registered and retrievable."
  (let ((b (beads-agent--get-backend "claude")))
    (should b)
    (should (object-of-class-p b 'beads-agent-backend-claude))))

(ert-deftest beads-agent-phase-2-test-claude-opt-in-not-default ()
  "The per-type backend defcustoms are NOT flipped to claude."
  ;; Opt-in means these stay nil (resolve to global/first-available),
  ;; never the string \"claude\".
  (should (null beads-agent-task-backend))
  (should (null beads-agent-review-backend))
  (should (null beads-agent-plan-backend))
  (should (null beads-agent-qa-backend)))

(ert-deftest beads-agent-phase-2-test-pi-registered ()
  "`beads-agent-backend-pi' is registered and retrievable."
  (let ((b (beads-agent--get-backend "pi")))
    (should b)
    (should (object-of-class-p b 'beads-agent-backend-pi))))

(ert-deftest beads-agent-phase-2-test-pi-argv ()
  "The pi backend builds claude-identical argv shape with its command."
  (let ((b (beads-agent-backend-pi)))
    (should (equal (beads-agent-backend-terminal-build-argv b nil "u" nil)
                   '("pi" "u")))
    (should (equal (beads-agent-backend-terminal-build-argv b "S" "u" nil)
                   '("pi" "--append-system-prompt" "S" "u")))))

;;; efrit fully removed

(ert-deftest beads-agent-phase-2-test-efrit-class-absent ()
  "No efrit class, feature, or file remains."
  (should-not (find-class 'beads-agent-backend-efrit))
  (should-not (featurep 'beads-agent-efrit))
  (should-not (locate-library "beads-agent-efrit"))
  (should-not (locate-library "beads-agent-efrit-test")))

(ert-deftest beads-agent-phase-2-test-efrit-not-registered ()
  "No registered backend is named \"efrit\"."
  (should-not (cl-find-if
               (lambda (b) (equal (oref b name) "efrit"))
               (beads-agent--get-all-backends))))

;;; beads-reader-terminal

(ert-deftest beads-agent-phase-2-test-reader-terminal-resolves ()
  "`beads-reader-terminal' returns a real `beads-terminal' subclass."
  (require 'beads-terminal)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "vterm")))
    (let ((sym (beads-reader-terminal "p" nil nil)))
      (should (eq sym 'beads-terminal-vterm))
      (should (child-of-class-p sym 'beads-terminal)))))

(provide 'beads-agent-phase-2-test)
;;; beads-agent-phase-2-test.el ends here
