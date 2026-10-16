;;; beads-manual-walkthrough-test.el --- Fresh-Emacs manual walkthrough -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; The manual's documented fresh-`emacs -Q' walkthrough, automated
;; (implementation plan WS-8; REQ-011, REQ-012).
;;
;; This test runs with the setup the manual documents only -- the
;; package loaded from `lisp/' plus the `bd' executable on PATH -- inside
;; a throwaway repository, and walks the documented path:
;;
;;   install/load beads.el
;;     -> open the dashboard
;;     -> create/list/show/close an issue
;;     -> add a dependency
;;     -> run a formula
;;     -> inspect the molecule
;;     -> open the dispatch (`?') menu
;;     -> open the maintenance (`!') menu
;;
;; Each step asserts that the documented entry point exists and that the
;; documented result is reached.  The literal interactive `emacs -Q'
;; evidence (steps run, gaps found, fixes) is recorded in
;; `plans/manual-standard/walkthrough.md'; this test is the automated
;; half of the same walkthrough.
;;
;; `:integration' because it drives the real `bd' CLI against a temporary
;; embedded-Dolt store (see `beads-integration-test').

;;; Code:

(require 'ert)
(require 'seq)
(require 'beads)
;; The dispatch and maintenance prefixes live here; a real install gets
;; them from the generated autoloads, which a plain `-L lisp' checkout
;; has not produced yet.
(require 'beads-menu)
(require 'beads-command-create)
(require 'beads-command-close)
(require 'beads-command-dep)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-command-cook)
(require 'beads-command-mol)
(require 'beads-integration-test)

(defconst beads-manual-walkthrough-test--formula
  "formula = \"manual-walkthrough-fixture\"
description = \"Walkthrough fixture\"
type = \"workflow\"
version = 1

[[steps]]
id = \"first\"
title = \"First walkthrough step\"

[[steps]]
id = \"second\"
title = \"Second walkthrough step\"
needs = [\"first\"]
"
  "A minimal two-step formula for the walkthrough.")

(defun beads-manual-walkthrough-test--issue-by-title (title)
  "Return the live issue titled TITLE, or nil."
  (seq-find (lambda (issue)
              (equal title (oref issue title)))
            (beads-list-execute :limit nil)))

(defun beads-manual-walkthrough-test--compose-create (title body)
  "Create an issue via the documented compose flow, then return it.
TITLE and BODY fill the compose buffer the manual describes."
  (let ((buffer (beads-compose-create)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert title "\n\n" body "\n")
      (beads-compose-submit)))
  (or (beads-manual-walkthrough-test--issue-by-title title)
      (ert-fail (format "compose create did not produce an issue titled %S"
                        title))))

(defun beads-manual-walkthrough-test--issue-status (issue-id)
  "Return the current status of ISSUE-ID from the store."
  (oref (beads-command-execute
         (beads-command-show :issue-ids (list issue-id) :json t))
        status))

(defun beads-manual-walkthrough-test--write-formula (name source)
  "Write formula SOURCE as NAME into the store's .beads/formulas/."
  (let ((dir (expand-file-name ".beads/formulas" default-directory)))
    (make-directory dir t)
    (write-region source nil
                  (expand-file-name (concat name ".formula.toml") dir)
                  nil 'silent)))

(ert-deftest beads-manual-walkthrough-test-fresh-emacs-walkthrough ()
  "The manual's documented walkthrough reaches every documented result.
REQ-011: documented entry points and results.  REQ-012: gaps this
walkthrough surfaced are fixed in the manual and its images."
  :tags '(:integration)
  (beads-test-skip-unless-bd)
  (beads-test-with-temp-repo (:init-beads t)
    ;; ---------------------------------------------------------------
    ;; Install/load: the manual's "Installing from Source" setup.
    ;; ---------------------------------------------------------------
    (should (featurep 'beads))
    (dolist (entry '(beads                    ; "M-x beads"
                     beads-dashboard          ; the board
                     beads-dashboard-status
                     beads-list-issues        ; the list view
                     beads-list-mode
                     beads-show               ; the detail view
                     beads-show-mode
                     beads-compose-create     ; compose create
                     beads-actions-close      ; context close
                     beads-dep                ; dependency menu
                     beads-dep-add
                     beads-formula-menu       ; formula browser
                     beads-cook               ; cook a formula (maintenance `K')
                     beads-mol                ; molecule menu
                     beads-mol-pour
                     beads-mol-show
                     beads-epic-menu          ; epic menu (documented M-x entry)
                     beads-show-actions       ; stay-open per-issue actions
                     beads-dispatch           ; the `?' dispatch menu
                     beads-maintenance))      ; the `!' maintenance menu
      (should (fboundp entry)))
    (dolist (menu '(beads-dispatch beads-maintenance))
      (should (get menu 'transient--prefix)))

    ;; ---------------------------------------------------------------
    ;; Open the dashboard: `M-x beads' is `beads-dashboard'.
    ;; ---------------------------------------------------------------
    (let ((dashboard (beads-dashboard :directory default-directory)))
      (should (buffer-live-p dashboard))
      (with-current-buffer dashboard
        (should (derived-mode-p 'beads-dashboard-mode))
        ;; The board brackets the project with angle brackets (manual
        ;; "Modes and Buffers"); the other views use square brackets.
        (should (string-match-p "\\`\\*beads-dashboard<" (buffer-name)))
        ;; Store scoping: the board acts on the directory it was opened
        ;; for (manual "Store Scoping").
        (should (equal beads-store-directory default-directory))))

    ;; ---------------------------------------------------------------
    ;; Create / list / show / close an issue.
    ;; ---------------------------------------------------------------
    (let* ((first (beads-manual-walkthrough-test--compose-create
                   "Walkthrough issue"
                   "Created by the documented fresh-`emacs -Q' walkthrough."))
           (second (beads-manual-walkthrough-test--compose-create
                    "Walkthrough dependent issue"
                    "Depends on the first walkthrough issue."))
           (first-id (oref first id))
           (second-id (oref second id)))
      ;; The list view opens in `beads-list-mode' and shows both rows.
      (let ((list-buffer (beads-list-issues :directory default-directory)))
        (should (buffer-live-p list-buffer))
        (with-current-buffer list-buffer
          (should (derived-mode-p 'beads-list-mode))
          (should (string-match-p (regexp-quote first-id) (buffer-string)))
          (should (string-match-p (regexp-quote second-id) (buffer-string)))))
      ;; The show view opens in `beads-show-mode' and names the issue.
      (let ((show-buffer (beads-show first-id :directory default-directory)))
        (should (buffer-live-p show-buffer))
        (with-current-buffer show-buffer
          (should (derived-mode-p 'beads-show-mode))
          (should (string-match-p (regexp-quote first-id) (buffer-string)))))
      ;; Closing reaches the documented closed status.
      (beads-command-execute
       (beads-command-close :issue-ids (list first-id)
                            :reason "walkthrough close"))
      (should (equal "closed"
                     (beads-manual-walkthrough-test--issue-status first-id)))

      ;; -------------------------------------------------------------
      ;; Add a dependency: `bd dep add <source> <target>'.
      ;; -------------------------------------------------------------
      (let ((result (beads-command-execute
                     (beads-command-dep-add :issue-id second-id
                                            :depends-on first-id
                                            :dep-type "blocks"
                                            :json t))))
        (should (equal first-id (oref result depends-on-id)))
        (should (equal second-id (oref result issue-id))))
      (let ((deps (beads-command-execute
                   (beads-command-dep-list :issue-id second-id :json t))))
        (should (seq-some (lambda (dep)
                            (equal first-id (oref dep depends-on-id)))
                          deps))))

    ;; ---------------------------------------------------------------
    ;; Run a formula and inspect the molecule it pours.
    ;; ---------------------------------------------------------------
    (beads-manual-walkthrough-test--write-formula
     "manual-walkthrough-fixture" beads-manual-walkthrough-test--formula)
    (let ((cooked (beads-command-execute
                   (beads-command-cook :formula-id "manual-walkthrough-fixture"
                                       :persist t :json t))))
      (should (equal "manual-walkthrough-fixture"
                     (alist-get 'proto_id cooked))))
    (let* ((poured (beads-command-execute
                    (beads-command-mol-pour
                     :proto-id "manual-walkthrough-fixture" :json t)))
           (root-id (alist-get 'new_epic_id poured)))
      (should (stringp root-id))
      ;; Inspect the molecule: `bd mol show' lists the root and its steps.
      (let* ((shown (beads-command-execute
                     (beads-command-mol-show :mol-id root-id :json t)))
             (root (alist-get 'root shown))
             (issues (append (alist-get 'issues shown) nil)))
        (should (equal root-id (alist-get 'id root)))
        (should (>= (length issues) 2))))

    ;; ---------------------------------------------------------------
    ;; Open the dispatch (`?') menu and the maintenance (`!') menu.
    ;; ---------------------------------------------------------------
    (dolist (menu '((beads-dispatch . ("Issues" "Workflow" "Views"))
                    (beads-maintenance . ("Issue Lifecycle" "Workflow"
                                          "Database"))))
      (unwind-protect
          (progn
            (transient-setup (car menu))
            (should (equal (car menu) (oref transient--prefix command)))
            (with-current-buffer " *transient*"
              (should (> (buffer-size) 0))
              (dolist (heading (cdr menu))
                (should (string-match-p (regexp-quote heading)
                                        (buffer-string))))))
        (ignore-errors (transient-quit-all)))
      ;; Leave no transient state behind for the next menu.
      (beads-test--clear-transient-state))

    ;; Leave no view buffers behind either: the later `:unit' test
    ;; `beads-remote-open-test-dashboard-scopes-to-root' scans
    ;; `buffer-list' for a dashboard buffer and must not find this
    ;; throwaway store's.  Every buffer whose `beads-store-directory' is
    ;; this temporary repo is safe to kill here.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (bound-and-true-p beads-store-directory)
                   (equal beads-store-directory default-directory))
          (kill-buffer buffer))))))

(provide 'beads-manual-walkthrough-test)
;;; beads-manual-walkthrough-test.el ends here
