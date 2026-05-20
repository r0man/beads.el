;;; beads-agent-phase-1a-ii-test.el --- Phase 1a-ii acceptance gates -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Acceptance gates for Phase 1a-ii (bde-xle9.2): the behavioural
;; break — role-only system defaults + per-type user-envelope
;; defconsts + the two-region prompt editor.  Placeholder
;; substitution is asserted over EVALUATED values, never a source
;; grep (a source grep would red-flag the NEWS old-defaults block and
;; docstrings).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent)
(require 'beads-agent-type)
(require 'beads-agent-types)
(require 'beads-agent-prompt-edit)
(require 'beads-types)

(defun beads-agent-phase-1a-ii-test--issue ()
  "Return a synthetic issue with a distinctive id."
  (beads-issue :id "bde-TEST"
               :title "Synthetic split issue"
               :description "Split description body."
               :acceptance-criteria "Split acceptance."))

;;; Substitution preserved for template types

(ert-deftest beads-agent-phase-1a-ii-test-substitution-preserved ()
  "Template types substitute the issue id into the user prompt, and
the system prompt is non-nil role text."
  (beads-agent-types-register-builtin)
  (let ((issue (beads-agent-phase-1a-ii-test--issue)))
    (dolist (name '("Task" "Review" "Plan" "QA"))
      (let* ((type (beads-agent-type-get name))
             (sys (beads-agent-type-system-prompt type issue))
             (user (beads-agent-type-build-user-prompt type issue)))
        (should (stringp sys))
        (should (not (string-empty-p sys)))
        (should (string-match-p "bde-TEST" user))))))

;;; No <ISSUE-...> literal survives in any rendered region

(ert-deftest beads-agent-phase-1a-ii-test-no-placeholder-in-rendered ()
  "No `<ISSUE-...>' literal remains in the rendered system or user
region for template types (checked over evaluated values)."
  (beads-agent-types-register-builtin)
  (let ((issue (beads-agent-phase-1a-ii-test--issue))
        (rx "<ISSUE-\\(ID\\|TITLE\\|DESCRIPTION\\)>"))
    (dolist (name '("Task" "Review" "Plan" "QA"))
      (let* ((type (beads-agent-type-get name))
             (sys (beads-agent-type-system-prompt type issue))
             (user (beads-agent-type-build-user-prompt type issue)))
        (should-not (string-match-p rx (or sys "")))
        (should-not (string-match-p rx (or user "")))))
    ;; Role-only defaults themselves carry no placeholders (evaluated
    ;; defcustom/defconst values, not a source grep).
    (should-not (string-match-p rx beads-agent-review-prompt))
    (should-not (string-match-p rx beads-agent-qa-prompt))
    (should-not (string-match-p rx beads-agent-plan-prompt))
    (should-not (string-match-p rx beads-agent-type-task--system-prompt))))

;;; Builder carve-out: Custom + fallback excluded, asserted via builder

(ert-deftest beads-agent-phase-1a-ii-test-builder-carve-out ()
  "Custom and the orchestration fallback are builders: system-prompt
nil, but the issue id is present via the builder path."
  (beads-agent-types-register-builtin)
  (let* ((issue (beads-agent-phase-1a-ii-test--issue))
         (custom (beads-agent-type-get "Custom")))
    (should (null (beads-agent-type-system-prompt custom issue)))
    (should (string-match-p
             "bde-TEST"
             (beads-agent-type-build-user-prompt custom issue)))
    ;; Orchestration fallback (single-string builder, unchanged).
    (should (string-match-p "bde-TEST"
                            (beads-agent--build-prompt issue)))))

;;; NEWS-recipe smoke: customised role defcustom is the SYSTEM prompt

(ert-deftest beads-agent-phase-1a-ii-test-news-recipe-smoke ()
  "A user `setq' of `beads-agent-review-prompt' to role + <ISSUE-ID>
text is delivered as the system prompt with the placeholder
substituted."
  (beads-agent-types-register-builtin)
  (let ((issue (beads-agent-phase-1a-ii-test--issue))
        (beads-agent-review-prompt
         "You are my reviewer for <ISSUE-ID>."))
    (let ((sys (beads-agent-type-system-prompt
                (beads-agent-type-get "Review") issue)))
      (should (equal sys "You are my reviewer for bde-TEST.")))))

;;; Empty/cancel disambiguation (combine path)

(ert-deftest beads-agent-phase-1a-ii-test-empty-sys-no-stray-newline ()
  "`sys=\"\"' is treated as absent on the combine path (no `\\n\\n')."
  (should (equal (beads-agent-backend--combine-prompt "" "u") "u"))
  (should (equal (beads-agent-backend--combine-prompt nil "u") "u"))
  (should (equal (beads-agent-backend--combine-prompt "S" "u") "S\n\nu")))

;;; Two-region prompt editor

(defmacro beads-agent-phase-1a-ii-test--with-git (&rest body)
  "Run BODY with git project helpers stubbed."
  `(cl-letf (((symbol-function 'beads-git-get-project-name)
              (lambda () "proj"))
             ((symbol-function 'beads-git-get-branch)
              (lambda () "main"))
             ((symbol-function 'beads-buffer-is-main-branch-p)
              (lambda (&optional _b) t)))
     ,@body))

(ert-deftest beads-agent-phase-1a-ii-test-confirm-splits-regions ()
  "Confirm returns the system region and the user region separately."
  (beads-agent-phase-1a-ii-test--with-git
   (let (got)
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "bde-1" "ROLE TEXT" "TASK TEXT" "Task"
        (lambda (sys user) (setq got (list sys user))))
       (beads-agent-prompt-edit-confirm))
     (should (equal got '("ROLE TEXT" "TASK TEXT"))))))

(ert-deftest beads-agent-phase-1a-ii-test-blank-system-is-nil ()
  "A blank system region yields SYS=nil (use backend identity)."
  (beads-agent-phase-1a-ii-test--with-git
   (let (got)
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "bde-1" "" "just the task" "Task"
        (lambda (sys user) (setq got (list sys user))))
       (beads-agent-prompt-edit-confirm))
     (should (equal got '(nil "just the task"))))))

(ert-deftest beads-agent-phase-1a-ii-test-cancel-sentinel ()
  "Cancel calls the callback with the (nil nil) sentinel."
  (beads-agent-phase-1a-ii-test--with-git
   (let ((got 'unset))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "bde-1" "ROLE" "TASK" "Task"
        (lambda (sys user) (setq got (list sys user))))
       (beads-agent-prompt-edit-cancel))
     (should (equal got '(nil nil))))))

(ert-deftest beads-agent-phase-1a-ii-test-marker-survives-pasted-heading ()
  "Pasting `## User prompt' into the system region does not desync
the marker-based parser."
  (beads-agent-phase-1a-ii-test--with-git
   (let (got)
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "bde-1" "role line\n## User prompt\nstill system" "real user" "Task"
        (lambda (sys user) (setq got (list sys user))))
       (beads-agent-prompt-edit-confirm))
     ;; The literal "## User prompt" inside the system region is part
     ;; of the system text, NOT a region boundary.
     (should (equal (car got) "role line\n## User prompt\nstill system"))
     (should (equal (cadr got) "real user")))))

(ert-deftest beads-agent-phase-1a-ii-test-internal-whitespace-preserved ()
  "Leading/trailing blank lines are trimmed; internal indented code
blocks round-trip verbatim."
  (beads-agent-phase-1a-ii-test--with-git
   (let (got
         (user "\n\nline 1\n\n    indented code\n\nline 2\n\n"))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "bde-1" "ROLE" user "Task"
        (lambda (_s u) (setq got u)))
       (beads-agent-prompt-edit-confirm))
     (should (equal got "line 1\n\n    indented code\n\nline 2")))))

(ert-deftest beads-agent-phase-1a-ii-test-heading-read-only ()
  "The heading lines are read-only (yank/edit across them is blocked)."
  (beads-agent-phase-1a-ii-test--with-git
   (save-window-excursion
     (beads-agent-prompt-edit-show
      "bde-1" "ROLE" "TASK" "Task" #'ignore)
     (goto-char (point-min))
     ;; Point-min is the start of the "## System prompt" heading line.
     (should (get-text-property (point) 'read-only))
     (should-error (insert "X") :type 'text-read-only)
     (beads-agent-prompt-edit-cancel))))

(ert-deftest beads-agent-phase-1a-ii-test-default-directory-at-confirm ()
  "The editor buffer's `default-directory' is the cwd bound when
`beads-agent-prompt-edit-show' was called (worktree/project dir)."
  (beads-agent-phase-1a-ii-test--with-git
   (let* ((dir (file-name-as-directory (make-temp-file "bde-pe-" t)))
          captured)
     (unwind-protect
         (save-window-excursion
           (let ((default-directory dir))
             (beads-agent-prompt-edit-show
              "bde-1" "ROLE" "TASK" "Task"
              (lambda (&rest _) nil)))
           ;; The edit buffer captured DIR even though we are now back
           ;; in the ert default-directory.
           (let ((buf (get-buffer
                       (beads-agent-prompt-edit--buffer-name "bde-1"))))
             (setq captured (buffer-local-value 'default-directory buf))
             (with-current-buffer buf
               (beads-agent-prompt-edit-cancel)))
           (should (equal (file-truename captured)
                          (file-truename dir))))
       (delete-directory dir t)))))

(provide 'beads-agent-phase-1a-ii-test)
;;; beads-agent-phase-1a-ii-test.el ends here
