;;; beads-manual-walkthrough-test.el --- Shared demo walkthrough tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; The manual's one shared `/tmp/demo-project' walkthrough
;; (implementation plan WS-6; REQ-003, REQ-009, REQ-011).
;;
;; The manual documents a single reproducible scenario both beads.el and
;; gascity.el describe: the deterministic twelve-issue fixture at
;; `/tmp/demo-project' (dataset `sha256:6504c508...'), followed as
;;   create -> list -> show -> claim/update -> dependency -> close ->
;;   epic -> maintenance
;; with the four shared demo steps in order:
;;   triage demo-3 (in progress) -> unblock demo-4 (blocked by demo-3)
;;   -> close demo-6 (closed) -> inspect the demo-10 epic.
;;
;; This file has two halves:
;;
;;   * `:unit' tests read `doc/*.texi' and assert the manual states the
;;     shared path, ids, hash, ordered steps, keys, screenshot
;;     references, cross-repo contract, and "Tested with" section.  They
;;     need neither `bd' nor the capture stack.
;;   * `:integration' tests run the documented UI entry points against a
;;     real fixture created by `docs/manual/screenshots/fixture.sh' and
;;     assert the same ordered steps and ids reach the documented
;;     results.  Only the documented entry points are used.
;;
;; The fixture assigns every issue to the `demo' actor; the walkthrough
;; says so and sets `beads-actor' to `demo' for the session, so the
;; documented `C' claim and `d' close obey bd's claim ownership rules.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'beads)
;; The dispatch and maintenance prefixes live here; a real install gets
;; them from the generated autoloads, which a plain `-L lisp' checkout
;; has not produced yet.
(require 'beads-menu)
(require 'beads-actions)
(require 'beads-compose)
(require 'beads-command-create)
(require 'beads-command-close)
(require 'beads-command-dep)
(require 'beads-command-epic)
(require 'beads-command-list)
(require 'beads-command-reopen)
(require 'beads-command-show)
(require 'beads-command-update)
(require 'beads-integration-test)

(defconst beads-manual-walkthrough-test--root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name
                         (locate-library "beads-manual-walkthrough-test"))))
  "Repository root, derived from this test file's location.")

(defconst beads-manual-walkthrough-test--fixture
  (expand-file-name "docs/manual/screenshots/fixture.sh"
                    beads-manual-walkthrough-test--root)
  "The fixture manager the manual documents.")

(defconst beads-manual-walkthrough-test--dataset
  (expand-file-name "docs/manual/screenshots/fixtures/beads-fixture.jsonl"
                    beads-manual-walkthrough-test--root)
  "The frozen twelve-issue dataset the fixture imports.")

(defconst beads-manual-walkthrough-test--dataset-hash
  "6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8"
  "The recorded SHA-256 of the frozen dataset (REQ-011).")

(defconst beads-manual-walkthrough-test--project-path "/tmp/demo-project"
  "The shared demo project path both manuals document.")

(defconst beads-manual-walkthrough-test--demo-ids
  (mapcar (lambda (n) (format "demo-%d" n)) (number-sequence 1 12))
  "The twelve frozen demo ids, in order.")

(defconst beads-manual-walkthrough-test--shared-steps
  '("demo-3" "demo-4" "demo-6" "demo-10")
  "The four shared demo steps, in the order both manuals describe them.")

(defconst beads-manual-walkthrough-test--walkthrough-file
  (expand-file-name "doc/getting-started.texi"
                    beads-manual-walkthrough-test--root)
  "The manual chapter that holds the walkthrough.")

(defconst beads-manual-walkthrough-test--installation-file
  (expand-file-name "doc/installation.texi"
                    beads-manual-walkthrough-test--root)
  "The manual chapter that holds the \"Tested with\" section.")

;;; Helpers

(defun beads-manual-walkthrough-test--read (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun beads-manual-walkthrough-test--sha256 (file)
  "Return the SHA-256 hex digest of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun beads-manual-walkthrough-test--manual-text ()
  "Return the concatenation of every hand-authored Texinfo source."
  (mapconcat #'beads-manual-walkthrough-test--read
             (directory-files (expand-file-name "doc"
                                                beads-manual-walkthrough-test--root)
                              t "\\.texi\\'")
             "\n"))

(defun beads-manual-walkthrough-test--walkthrough-section ()
  "Return the walkthrough body, including its subsections."
  (let* ((text (beads-manual-walkthrough-test--read
                beads-manual-walkthrough-test--walkthrough-file))
         (start (string-match "^@node The Shared Walkthrough" text)))
    (unless start
      (ert-fail "doc/getting-started.texi has no `@node The Shared Walkthrough'"))
    (substring text start
               (or (string-match "^@node Interface Concepts" text start)
                   (length text)))))

(defun beads-manual-walkthrough-test--fixture-command (&rest args)
  "Run fixture.sh ARGS and return its stdout.
stderr is captured separately so bd's progress messages cannot be
mistaken for the printed project path."
  (let ((stderr (make-temp-file "beads-fixture-err")))
    (unwind-protect
        ;; A buffer as the stdout destination and a file for stderr: a
        ;; file name in the first slot is read as a buffer name.
        (with-temp-buffer
          (let ((code (apply #'call-process beads-manual-walkthrough-test--fixture
                             nil (list (current-buffer) stderr) nil args)))
            (unless (eq code 0)
              (ert-fail
               (format "fixture.sh %s failed with %s: %s"
                       (mapconcat #'identity args " ") code
                       (beads-manual-walkthrough-test--read stderr))))
            (string-trim (buffer-string))))
      (ignore-errors (delete-file stderr)))))

(defmacro beads-manual-walkthrough-test--with-fixture (&rest body)
  "Run BODY against the documented demo fixture.
Creates the fixture with `fixture.sh create' (the manual's setup),
scopes every beads call to it, and removes it again afterwards.  The
fixture owns every issue as `demo', so run `bd' as that actor through
`BEADS_ACTOR', as the fixture-setup section documents."
  (declare (indent 0) (debug (body)))
  `(let* ((tmpdir (make-temp-file "beads-walkthrough-fixture-" t))
          (process-environment
           ;; A private TMPDIR keeps this run off the shared
           ;; /tmp/demo-project a concurrent gascity.el fixture may
           ;; own; the project basename (and so every buffer name) is
           ;; still the documented demo-project.
           (cons (concat "TMPDIR=" tmpdir)
                 ;; `bd' resolves its actor from BEADS_ACTOR; the fixture
                 ;; owns every bead as `demo', so the documented claim and
                 ;; close keys must run as that actor.
                 (cons "BEADS_ACTOR=demo"
                       (seq-remove (lambda (entry)
                                     (string-prefix-p "BEADS_ACTOR=" entry))
                                   (beads-test-isolated-process-environment)))))
          (project (beads-manual-walkthrough-test--fixture-command "create"))
          (default-directory (file-name-as-directory project))
          (beads--project-cache (make-hash-table :test 'equal))
          (beads--completion-cache nil))
     (unwind-protect
         (cl-letf (((symbol-function 'beads-git-find-project-root)
                    (lambda () project)))
           (progn ,@body))
       (beads-test--clear-transient-state)
       (beads-test--clear-caches)
       ;; Leave no view buffers behind: later `:unit' tests scan
       ;; `buffer-list' for a dashboard buffer and must not find this
       ;; throwaway store's.
       (dolist (buffer (buffer-list))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (bound-and-true-p beads-store-directory)
                        (string-prefix-p project beads-store-directory))
               (kill-buffer buffer)))))
       (ignore-errors
         (beads-manual-walkthrough-test--fixture-command "teardown" project))
       (ignore-errors (delete-directory tmpdir t)))))

(defun beads-manual-walkthrough-test--issue (issue-id)
  "Return ISSUE-ID as a typed issue object from the store."
  (beads-command-execute
   (beads-command-show :issue-ids (list issue-id) :json t)))

(defun beads-manual-walkthrough-test--status (issue-id)
  "Return the stored status of ISSUE-ID."
  (oref (beads-manual-walkthrough-test--issue issue-id) status))

(defun beads-manual-walkthrough-test--show (issue-id)
  "Open ISSUE-ID and wait until its data has rendered.
`beads-show' loads the issue asynchronously; the wait makes the
buffer-state assertions deterministic without changing the entry
point the manual documents."
  (let ((buffer (beads-show issue-id :directory default-directory)))
    (with-timeout (15 (ert-fail (format "timed out loading %s" issue-id)))
      (while (with-current-buffer buffer (null beads-show--issue-data))
        (accept-process-output nil 0.05)))
    buffer))

(defun beads-manual-walkthrough-test--goto-issue (buffer issue-id)
  "Move point to ISSUE-ID's row in list BUFFER.
Pages through the list because the pager shows one window-sized page
at a time; the closed section is on a later page."
  (with-current-buffer buffer
    (with-timeout (15 (ert-fail (format "list never showed %s in %s"
                                       issue-id (buffer-name))))
      (catch 'found
        (dotimes (_ 20)
          (goto-char (point-min))
          ;; Anchor to the whole id: a created id like `demo-6ab' must not
          ;; satisfy a search for `demo-6'.  The row's ID cell is the
          ;; second check, so a column change cannot reintroduce the false
          ;; match (RF-4 / AC-009).
          (when (re-search-forward (concat "\\_<" (regexp-quote issue-id) "\\_>")
                                   nil t)
            (beginning-of-line)
            (when (equal (tabulated-list-get-id) issue-id)
              (throw 'found t)))
          (beads-pager-next-page))
        (ert-fail (format "list never showed %s in %s"
                          issue-id (buffer-name)))))))

(defun beads-manual-walkthrough-test--issue-by-title (title)
  "Return the live issue titled TITLE, or nil."
  (seq-find (lambda (issue) (equal title (oref issue title)))
            (beads-list-execute :limit nil :all t)))

(defun beads-manual-walkthrough-test--with-prompt (answer thunk)
  "Call THUNK with `completing-read' and `read-string' answering ANSWER.
ANSWER is a string.  Both prompt readers ignore their arguments and
return ANSWER, so the documented interactive key path runs unchanged."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) answer))
            ((symbol-function 'read-string)
             (lambda (&rest _) answer)))
    (funcall thunk)))

;;; Unit: the manual states the one shared scenario

(ert-deftest beads-manual-walkthrough-test-dataset-hash-recorded ()
  "The manual records the frozen dataset's actual SHA-256 (REQ-011)."
  :tags '(:unit)
  (should (equal (beads-manual-walkthrough-test--sha256
                  beads-manual-walkthrough-test--dataset)
                 beads-manual-walkthrough-test--dataset-hash))
  (let ((text (beads-manual-walkthrough-test--manual-text)))
    (should (string-match-p
             (regexp-quote beads-manual-walkthrough-test--dataset-hash) text))))

(ert-deftest beads-manual-walkthrough-test-fixture-path-and-ids ()
  "The manual names the shared path, the twelve ids, and the isolation rule."
  :tags '(:unit)
  (let ((text (beads-manual-walkthrough-test--manual-text)))
    (should (string-match-p
             (regexp-quote beads-manual-walkthrough-test--project-path) text))
    (dolist (id beads-manual-walkthrough-test--demo-ids)
      (should (string-match-p (regexp-quote id) text)))
    ;; The fixture-setup section must state why the store is isolated.
    (should (string-match-p "BEADS_DIR" text))
    (should (string-match-p "BEADS_DOLT_PORT" text))
    ;; The cross-repo contract: same path, city, rig, vocabulary.
    (should (string-match-p "gascity-fixture" text))
    (should (string-match-p "demo-rig" text))))

(ert-deftest beads-manual-walkthrough-test-shared-steps-in-order ()
  "The four shared demo steps appear in order in the walkthrough (REQ-003)."
  :tags '(:unit)
  (let ((body (beads-manual-walkthrough-test--walkthrough-section))
        (headings '("Triage demo-3 and Update It"
                    "Unblock demo-4"
                    "Close and Reopen demo-6"
                    "Inspect the demo-10 Epic"))
        (last -1))
    (dolist (heading headings)
      (let ((pos (string-match-p (regexp-quote heading) body)))
        (should pos)
        (should (> pos last))
        (setq last pos)))
    ;; The shared contract is stated explicitly, ids in the same order.
    (let ((mention (string-match-p "triage @code{demo-3}, unblock" body)))
      (should mention))))

(ert-deftest beads-manual-walkthrough-test-required-arc ()
  "The walkthrough covers create -> list -> show -> claim/update ->
dependency -> close -> epic -> maintenance, in that order (REQ-009)."
  :tags '(:unit)
  (let ((body (beads-manual-walkthrough-test--walkthrough-section))
        (headings '("Create an Issue"
                    "List the Issues"
                    "Show an Issue"
                    "Triage demo-3 and Update It"
                    "Unblock demo-4"
                    "Close and Reopen demo-6"
                    "Inspect the demo-10 Epic"
                    "Open the Dispatch and Maintenance Menus"))
        (last -1))
    (dolist (heading headings)
      (let ((pos (string-match-p (regexp-quote heading) body)))
        (should pos)
        (should (> pos last))
        (setq last pos)))))

(ert-deftest beads-manual-walkthrough-test-keys-and-screenshots ()
  "Each step names its exact keys and screenshot reference (REQ-009)."
  :tags '(:unit)
  (let ((body (beads-manual-walkthrough-test--walkthrough-section))
        (keys '("C-c C-c" "RET" "C" "s" "#" "d" "o" "?" "l" "c" "!"
                "M-x beads-epic-menu" "M-x beads-maintenance"))
        (figures '("fig-dashboard-dark"
                   "fig-compose-dark"
                   "fig-list-dark"
                   "fig-show-dark"
                   "fig-blocked-dark"
                   "fig-ready-dark"
                   "fig-epic-dark"
                   "fig-maintenance-dark")))
    (dolist (key keys)
      (should (string-match-p (regexp-quote (format "@kbd{%s}" key)) body)))
    (dolist (figure figures)
      ;; `@pxref' and `@ref' are both valid cross-reference forms.
      (should (string-match-p (concat "@[px]*ref{" (regexp-quote figure) "}")
                              body)))))

(ert-deftest beads-manual-walkthrough-test-tested-with ()
  "Installation has a \"Tested with\" section naming versions and the CI matrix."
  :tags '(:unit)
  (let ((text (beads-manual-walkthrough-test--read
               beads-manual-walkthrough-test--installation-file)))
    (should (string-match-p "^@node Tested with" text))
    ;; The observed capture/QA environment.
    (should (string-match-p "31\\.1" text))
    (should (string-match-p "guix describe" text))
    (should (string-match-p "f546638fed68114b67e5bf1c681c5c62876a4018" text))
    ;; The fixture dataset and its recorded hash.
    (should (string-match-p "beads-fixture\\.jsonl" text))
    (should (string-match-p
             (regexp-quote beads-manual-walkthrough-test--dataset-hash) text))
    ;; The CI matrix is cited separately, with 29.4 non-blocking.
    (dolist (version '("29\\.4" "30\\.2" "31\\.1" "snapshot"))
      (should (string-match-p version text)))
    (should (string-match-p "continue-on-error" text))))

;;; Integration: the documented walkthrough against the real fixture

(ert-deftest beads-manual-walkthrough-test-shared-fixture-walkthrough ()
  "The documented keys complete the shared demo walkthrough (REQ-003/009).
Follows the manual's ordered steps against `/tmp/demo-project' (the
fixture prints that path) and asserts each documented result.  Uses
only entry points the manual names."
  :tags '(:integration)
  (beads-test-skip-unless-bd)
  (beads-manual-walkthrough-test--with-fixture
    ;; ---------------------------------------------------------------
    ;; Fixture setup: the documented `fixture.sh create'.
    ;; ---------------------------------------------------------------
    (should (file-directory-p default-directory))
    (should (file-exists-p (expand-file-name ".beads-fixture-project"
                                             default-directory)))

    ;; ---------------------------------------------------------------
    ;; Open the board: `M-x beads'.
    ;; ---------------------------------------------------------------
    (let ((board (beads-dashboard :directory default-directory)))
      (should (buffer-live-p board))
      (with-current-buffer board
        (should (derived-mode-p 'beads-dashboard-mode))
        (should (string-match-p "\\`\\*beads-dashboard<" (buffer-name)))
        (should (equal beads-store-directory default-directory))))

    ;; ---------------------------------------------------------------
    ;; Create an issue: `? c', then `C-c C-c'.
    ;; ---------------------------------------------------------------
    (let* ((title "Walkthrough: document the shared demo")
           (buffer (beads-compose-create)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert title "\n\nCreated from the shared demo walkthrough.\n")
        (beads-compose-submit))
      (let ((created (beads-manual-walkthrough-test--issue-by-title title)))
        (should created)
        (should (string-prefix-p "demo-" (oref created id)))))

    ;; ---------------------------------------------------------------
    ;; List the issues: `? l'.
    ;; ---------------------------------------------------------------
    (beads-list-all)
    (let ((list-buffer (get-buffer "*beads-list[demo-project]*")))
      (should (buffer-live-p list-buffer))
      (with-current-buffer list-buffer
        (should (derived-mode-p 'beads-list-mode))
        ;; The frozen twelve ids are all listed, including closed demo-6.
        (dolist (id beads-manual-walkthrough-test--demo-ids)
          (should (string-match-p (regexp-quote id) (buffer-string))))
        (should (string-match-p "Walkthrough: document the shared demo"
                                (buffer-string)))))

    ;; ---------------------------------------------------------------
    ;; Show an issue: `RET' on demo-3.
    ;; ---------------------------------------------------------------
    (let ((show (beads-manual-walkthrough-test--show "demo-3")))
      (should (buffer-live-p show))
      (with-current-buffer show
        (should (derived-mode-p 'beads-show-mode))
        (should (string-match-p "demo-3" (buffer-string)))
        ;; The detail header renders the human-readable status label.
        (should (string-match-p "In Progress" (buffer-string)))))

    ;; ---------------------------------------------------------------
    ;; Triage demo-3: `C' claim, `s' status, `#' priority.
    ;; The fixture owns every issue as `demo', and the fixture-setup
    ;; section sets `beads-actor' to `demo', so the documented keys
    ;; satisfy bd's ownership check.
    ;; ---------------------------------------------------------------
    (let ((show (beads-show "demo-3" :directory default-directory)))
      (with-current-buffer show
        (call-interactively #'beads-actions-claim)
        (beads-manual-walkthrough-test--with-prompt
            "in_progress"
          (lambda () (call-interactively #'beads-actions-set-status)))
        (beads-manual-walkthrough-test--with-prompt
            "1 - High"
          (lambda () (call-interactively #'beads-actions-set-priority)))))
    (let ((triage (beads-manual-walkthrough-test--issue "demo-3")))
      (should (equal "in_progress" (oref triage status)))
      (should (equal "demo" (oref triage assignee)))
      (should (equal 1 (oref triage priority))))

    ;; ---------------------------------------------------------------
    ;; Inspect dependencies and the demo-10 epic.
    ;; demo-4 is blocked by demo-3; demo-10 is an epic whose children
    ;; are demo-11 and demo-12.
    ;; ---------------------------------------------------------------
    (let ((deps (beads-command-execute
                 (beads-command-dep-list :issue-id "demo-4" :json t))))
      (should (seq-some (lambda (dep)
                          (equal "demo-3" (oref dep depends-on-id)))
                        deps)))
    (should (equal "blocked"
                   (beads-manual-walkthrough-test--status "demo-4")))
    (let ((show (beads-manual-walkthrough-test--show "demo-4")))
      (with-current-buffer show
        (should (string-match-p "demo-3" (buffer-string)))
        (should (string-match-p "Blocked" (buffer-string)))))
    (let ((epic (beads-manual-walkthrough-test--issue "demo-10")))
      (should (equal "epic" (oref epic issue-type))))
    (let ((show (beads-manual-walkthrough-test--show "demo-10")))
      (with-current-buffer show
        (should (string-match-p "demo-11" (buffer-string)))
        (should (string-match-p "demo-12" (buffer-string)))))
    (let ((status (beads-epic)))
      (should (buffer-live-p status))
      (with-current-buffer status
        (should (string-match-p "demo-10" (buffer-string)))))

    ;; ---------------------------------------------------------------
    ;; Unblock demo-4: close its blocker demo-3 (`d'), then `s' demo-4
    ;; to open, and the ready list contains demo-4.
    ;; ---------------------------------------------------------------
    (let ((show (beads-show "demo-3" :directory default-directory)))
      (with-current-buffer show
        (beads-manual-walkthrough-test--with-prompt
            "blocker resolved by the walkthrough"
          (lambda () (call-interactively #'beads-actions-close)))))
    (should (equal "closed"
                   (beads-manual-walkthrough-test--status "demo-3")))
    (let ((show (beads-show "demo-4" :directory default-directory)))
      (with-current-buffer show
        (beads-manual-walkthrough-test--with-prompt
            "open"
          (lambda () (call-interactively #'beads-actions-set-status)))))
    (should (equal "open"
                   (beads-manual-walkthrough-test--status "demo-4")))
    (let ((ready (beads-ready :directory default-directory)))
      (with-current-buffer ready
        (should (string-match-p "demo-4" (buffer-string)))
        ;; Word boundaries: the created bead is demo-<hash>, which may
        ;; start with the same characters as demo-3.
        (should-not (string-match-p "\\_<demo-3\\_>" (buffer-string)))))

    ;; ---------------------------------------------------------------
    ;; Close and reopen demo-6 from the list, where `o' and `d' are
    ;; bound: `o' reopens the closed issue, `d' closes it again.
    ;; ---------------------------------------------------------------
    (should (equal "closed"
                   (beads-manual-walkthrough-test--status "demo-6")))
    (beads-list-all)
    (let ((list-buffer (get-buffer "*beads-list[demo-project]*")))
      (beads-manual-walkthrough-test--goto-issue list-buffer "demo-6")
      (with-current-buffer list-buffer
        (beads-manual-walkthrough-test--with-prompt
            ""
          (lambda () (call-interactively (key-binding (kbd "o")))))))
    (should (equal "open"
                   (beads-manual-walkthrough-test--status "demo-6")))
    (beads-list-all)
    (let ((list-buffer (get-buffer "*beads-list[demo-project]*")))
      (beads-manual-walkthrough-test--goto-issue list-buffer "demo-6")
      (with-current-buffer list-buffer
        (beads-manual-walkthrough-test--with-prompt
            "walkthrough close"
          (lambda () (call-interactively (key-binding (kbd "d")))))))
    (should (equal "closed"
                   (beads-manual-walkthrough-test--status "demo-6")))

    ;; ---------------------------------------------------------------
    ;; Open the dispatch (`?') and maintenance (`!') menus.
    ;; ---------------------------------------------------------------
    (dolist (menu '((beads-dispatch . ("Issues" "Workflow" "Views" "Manage"))
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
      (beads-test--clear-transient-state))))

(provide 'beads-manual-walkthrough-test)
;;; beads-manual-walkthrough-test.el ends here
