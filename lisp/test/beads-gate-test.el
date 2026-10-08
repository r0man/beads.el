;;; beads-gate-test.el --- Tests for beads-gate-ui -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for the gate porcelain in `beads-gate.el': command assembly,
;; row formatting, normalization, the display contract, the molecule
;; integration seam (REQ-SF-030 .. REQ-SF-033), and two integration
;; round-trips (create -> resolve -> ready, and check --dry-run).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-gate)
(require 'beads-command-gate)
(require 'beads-integration-test)

;;; Command assembly (REQ-SF-032)

(ert-deftest beads-gate-create-command-test-args ()
  "Unit test: create command carries the documented flags."
  :tags '(:unit)
  (let* ((cmd (beads-gate-create-command
               "gh:run" "bd-1"
               :await-id "release.yml"
               :timeout "30m"
               :reason "Wait for CI"
               :title "Gate: release"))
         (args (beads-command-line cmd)))
    (should (member "gate" args))
    (should (member "create" args))
    (should (member "--json" args))
    (should (member "--blocks" args))
    (should (member "bd-1" args))
    (should (member "--type" args))
    (should (member "gh:run" args))
    (should (member "--await-id" args))
    (should (member "release.yml" args))
    (should (member "--timeout" args))
    (should (member "30m" args))
    (should (member "--reason" args))
    (should (member "Wait for CI" args))
    (should (member "--title" args))
    (should (member "Gate: release" args))))

(ert-deftest beads-gate-create-command-test-drops-nil ()
  "Unit test: nil optional arguments are not serialized."
  :tags '(:unit)
  (let* ((cmd (beads-gate-create-command
               "human" "bd-1" :await-id nil :timeout nil :reason nil))
         (args (beads-command-line cmd)))
    (should-not (member "--await-id" args))
    (should-not (member "--timeout" args))
    (should-not (member "--reason" args))))

(ert-deftest beads-gate-create-command-test-validates-blocks ()
  "Unit test: create without --blocks fails validation."
  :tags '(:unit)
  (let ((cmd (beads-command-gate-create :gate-type "human")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-gate-check-command-test-type-and-dry-run ()
  "Unit test: check command carries --type and --dry-run without --json.
bd prints a human report followed by a JSON summary on the same
stream, so the UI must not request JSON."
  :tags '(:unit)
  (let* ((cmd (beads-gate-check-command :type "timer" :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "timer" args))
    (should (member "--dry-run" args))
    (should-not (member "--json" args))))

;;; Type glyphs and formatting (REQ-SF-030)

(ert-deftest beads-gate-type-glyph-test-all-types ()
  "Unit test: every bd gate type has a glyph."
  :tags '(:unit)
  (should (equal (beads-gate-type-glyph "human") "⚑"))
  (should (equal (beads-gate-type-glyph "timer") "⏱"))
  (should (equal (beads-gate-type-glyph "gh:run") "⚙"))
  (should (equal (beads-gate-type-glyph "gh:pr") "⇄"))
  (should (equal (beads-gate-type-glyph "bead") "⇄"))
  (should (equal (beads-gate-type-glyph "unknown") "")))

(ert-deftest beads-gate--format-duration-test ()
  "Unit test: nanosecond durations render compactly."
  :tags '(:unit)
  (should (equal (beads-gate--format-duration nil) ""))
  (should (equal (beads-gate--format-duration 0) ""))
  (should (equal (beads-gate--format-duration 45000000000) "45s"))
  (should (equal (beads-gate--format-duration 600000000000) "10m"))
  (should (equal (beads-gate--format-duration 7200000000000) "2h"))
  (should (equal (beads-gate--format-duration 90000000000000) "1d1h"))
  (should (equal (beads-gate--format-duration "2h") "2h")))

(ert-deftest beads-gate--parse-blocks-test ()
  "Unit test: blocked issue ids are parsed from the description."
  :tags '(:unit)
  (should (equal (beads-gate--parse-blocks
                  "Ad-hoc gate blocking bd-abc\n\nReason: x")
                 '("bd-abc")))
  (should (equal (beads-gate--parse-blocks
                  "Ad-hoc gate blocking bd-abc and also blocking bd-def")
                 '("bd-abc" "bd-def")))
  (should (equal (beads-gate--parse-blocks
                  "blocking bd-abc and blocking bd-abc")
                 '("bd-abc")))
  (should-not (beads-gate--parse-blocks "no fragment here"))
  (should-not (beads-gate--parse-blocks nil)))

(ert-deftest beads-gate--parse-reason-test ()
  "Unit test: the reason is parsed from the description."
  :tags '(:unit)
  (should (equal (beads-gate--parse-reason
                  "Ad-hoc gate blocking bd-abc\n\nReason: Need review")
                 "Need review"))
  (should-not (beads-gate--parse-reason "no reason"))
  (should-not (beads-gate--parse-reason nil)))

(ert-deftest beads-gate--format-waiters-test ()
  "Unit test: the waiter cell counts waiters."
  :tags '(:unit)
  (should (equal (beads-gate--format-waiters
                  '((waiters . nil)))
                 ""))
  (should (equal (beads-gate--format-waiters
                  '((waiters . ("a"))))
                 "1"))
  (should (equal (beads-gate--format-waiters
                  '((waiters . ("a" "b" "c"))))
                 "3")))

;;; Normalization and display

(defconst beads-gate-test--gate-alist
  '((id . "gate-1")
    (issue_type . "gate")
    (status . "open")
    (title . "Gate: timer")
    (await_type . "timer")
    (timeout . 7200000000000)
    (waiters . ("beads.el/task"))
    (created_at . "2026-10-07T10:00:00Z")
    (description . "Ad-hoc gate blocking bd-abc\n\nReason: Wait"))
  "A representative raw gate alist used across unit tests.")

(ert-deftest beads-gate--normalize-test ()
  "Unit test: vectors and lists of gate alists normalize to issues."
  :tags '(:unit)
  (let ((from-vector (beads-gate--normalize
                      (vector beads-gate-test--gate-alist)))
        (from-list (beads-gate--normalize (list beads-gate-test--gate-alist))))
    (should (= 1 (length from-vector)))
    (should (= 1 (length from-list)))
    (should (beads-issue-p (car from-vector)))
    (should (equal (oref (car from-vector) id) "gate-1"))
    (should (equal (oref (car from-vector) await-type) "timer"))))

(ert-deftest beads-gate--entry-test ()
  "Unit test: a row has seven columns keyed by gate id (REQ-SF-030)."
  :tags '(:unit)
  (let* ((gate (beads-gate--coerce beads-gate-test--gate-alist))
         (entry (beads-gate--entry gate))
         (cols (cadr entry)))
    (should (equal (car entry) "gate-1"))
    (should (= 7 (length cols)))
    (should (equal (aref cols 0) "gate-1"))
    (should (string-match-p "timer" (aref cols 1)))
    (should (equal (aref cols 3) "2h"))
    (should (equal (aref cols 4) "1"))
    (should (equal (aref cols 5) "bd-abc"))))

(ert-deftest beads-gate-display-test ()
  "Unit test: the display plist exposes the molecule contract keys."
  :tags '(:unit)
  (let ((display (beads-gate-display beads-gate-test--gate-alist)))
    (should (equal (plist-get display :id) "gate-1"))
    (should (equal (plist-get display :type) "timer"))
    (should (equal (plist-get display :status) "open"))
    (should (equal (plist-get display :timeout-label) "2h"))
    (should (equal (plist-get display :blocks) '("bd-abc")))
    (should (equal (plist-get display :reason) "Wait"))
    (should (equal (plist-get display :waiters) '("beads.el/task")))
    (should (plist-get display :expiry))))

;;; Filtering

(ert-deftest beads-gate--filter-type-test ()
  "Unit test: the type filter keeps only matching gates."
  :tags '(:unit)
  (let ((gates (list (beads-gate--coerce
                      '((id . "g1") (await_type . "timer")))
                     (beads-gate--coerce
                      '((id . "g2") (await_type . "human"))))))
    (let ((beads-gate-list--type-filter nil))
      (should (= 2 (length (beads-gate--filter-type gates)))))
    (let ((beads-gate-list--type-filter "timer"))
      (should (equal (mapcar (lambda (g) (beads-gate--get g 'id))
                             (beads-gate--filter-type gates))
                     '("g1"))))))

;;; Resolve contract

(ert-deftest beads-gate--require-reason-test ()
  "Unit test: resolving a gate requires a non-empty reason."
  :tags '(:unit)
  (should (equal (beads-gate--require-reason "  done ") "done"))
  (should-error (beads-gate--require-reason "") :type 'user-error)
  (should-error (beads-gate--require-reason "   ") :type 'user-error)
  (should-error (beads-gate--require-reason nil) :type 'user-error))

(ert-deftest beads-gate--resolved-ids-test ()
  "Unit test: resolved gate ids are extracted from check output."
  :tags '(:unit)
  (should (equal (beads-gate--resolved-ids
                  "✓ be-ig05: resolved - timer expired 2s ago\n\
○ be-uh8j: pending - expires in 2h\n\
Checked 3 gates: 1 resolved")
                 '("be-ig05")))
  (should-not (beads-gate--resolved-ids nil)))

;;; Molecule integration (REQ-SF-033)

(ert-deftest beads-gate-step-glyph-test ()
  "Unit test: a gated step glyph carries the gate id and marker."
  :tags '(:unit)
  (let ((glyph (beads-gate-step-glyph "gate-1")))
    (should (string-match-p "\\[blocked-gate\\]" glyph))
    (should (string-match-p "gate-1" glyph)))
  (let ((glyph (beads-gate-step-glyph beads-gate-test--gate-alist)))
    (should (string-match-p "⏱" glyph))
    (should (string-match-p "gate-1" glyph))))

;;; Mode wiring

(ert-deftest beads-gate-list-mode-test-keymap ()
  "Unit test: list mode binds RET and the action keys (REQ-SF-032)."
  :tags '(:unit)
  (should (eq (lookup-key beads-gate-list-mode-map (kbd "RET"))
              #'beads-gate-open))
  (should (eq (lookup-key beads-gate-list-mode-map (kbd "c"))
              #'beads-gate-create))
  (should (eq (lookup-key beads-gate-list-mode-map (kbd "C"))
              #'beads-gate-check))
  (should (eq (lookup-key beads-gate-list-mode-map (kbd "R"))
              #'beads-gate-resolve))
  (should (eq (lookup-key beads-gate-list-mode-map (kbd "w"))
              #'beads-gate-add-waiter))
  (should (eq (lookup-key beads-gate-list-mode-map (kbd "D"))
              #'beads-gate-discover)))

(ert-deftest beads-gate-detail-mode-test-keymap ()
  "Unit test: detail mode binds the action keys and refresh (REQ-SF-031)."
  :tags '(:unit)
  (should (eq (lookup-key beads-gate-detail-mode-map (kbd "g"))
              #'beads-gate-detail-refresh))
  (should (eq (lookup-key beads-gate-detail-mode-map (kbd "R"))
              #'beads-gate-resolve))
  (should (eq (lookup-key beads-gate-detail-mode-map (kbd "w"))
              #'beads-gate-add-waiter)))

;;; Integration (REQ-SF-030 .. REQ-SF-032)

(ert-deftest beads-gate-integration-create-resolve-ready ()
  "Integration: create -> resolve -> the blocked issue becomes ready."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (require 'beads-command-create)
    (require 'beads-command-ready)
    (let* ((issue (beads-execute 'beads-command-create
                                 :title "Blocked work" :issue-type "task"))
           (issue-id (oref issue id))
           (cmd (beads-gate-create-command
                 "human" issue-id :reason "integration test"))
           (result (beads-command-execute cmd))
           (gate-id (beads-gate--result-id result)))
      (should (and gate-id (stringp gate-id)))
      ;; The blocked issue is not ready while the gate is open.
      (let ((ready-ids (mapcar (lambda (i) (oref i id))
                               (beads-execute 'beads-command-ready))))
        (should-not (member issue-id ready-ids)))
      ;; Resolving the gate unblocks it.
      (beads-gate-resolve gate-id "integration done")
      (let ((ready-ids (mapcar (lambda (i) (oref i id))
                               (beads-execute 'beads-command-ready))))
        (should (member issue-id ready-ids)))
      ;; The closed gate is included with --all.
      (let* ((all (beads-command-execute
                   (beads-command-gate-list :all t :json t)))
             (gates (beads-gate--normalize all)))
        (should (cl-find gate-id gates
                         :key (lambda (g) (beads-gate--get g 'id))
                         :test #'equal))))))

(ert-deftest beads-gate-integration-check-dry-run ()
  "Integration: gate check --dry-run reports without closing."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (require 'beads-command-create)
    (let* ((issue (beads-execute 'beads-command-create
                                 :title "Timed work" :issue-type "task"))
           (issue-id (oref issue id))
           (res (beads-command-execute
                 (beads-gate-create-command "timer" issue-id
                                            :timeout "1s" :reason "timer test")))
           (gate-id (beads-gate--result-id res)))
      (should (stringp gate-id))
      (sleep-for 2)
      (let ((output (beads-command-execute
                     (beads-gate-check-command :dry-run t))))
        (should (stringp output))
        (should (string-match-p "Checked" output))
        ;; The dry run must not close the gate.
        (let* ((gates (beads-gate--normalize
                       (beads-command-execute
                        (beads-command-gate-list :all t :json t))))
               (gate (cl-find gate-id gates
                              :key (lambda (g) (beads-gate--get g 'id))
                              :test #'equal)))
          (should gate)
          (should (equal (beads-gate--get gate 'status) "open"))))
      ;; The applying check closes the expired timer gate.
      (beads-command-execute (beads-gate-check-command))
      (let* ((gates (beads-gate--normalize
                     (beads-command-execute
                      (beads-command-gate-list :all t :json t))))
             (gate (cl-find gate-id gates
                            :key (lambda (g) (beads-gate--get g 'id))
                            :test #'equal)))
        (should gate)
        (should (equal (beads-gate--get gate 'status) "closed"))))))

(provide 'beads-gate-test)
;;; beads-gate-test.el ends here
