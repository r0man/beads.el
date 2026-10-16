;;; beads-manual-test.el --- Tests for the user manual + screenshot pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Guards for W7 (implementation plan WI-6; REQ-007, REQ-008, REQ-010;
;; plan-review P2, P3): the deterministic fixture, the capture scope
;; parser, the manifest/view/page/image contract, and the isolation list
;; shared with `beads-test-isolation-env-vars'.
;;
;; The `:unit' tests read files and run the resolution-only paths of
;; `capture.sh --list-views' and `fixture.sh print-isolation-vars', none
;; of which need `bd', sway, or a graphical Emacs.  The `:integration'
;; tests drive `capture.sh' with a fake capture engine
;; (`BEADS_SHOT_ENGINE') so the verify and subset-isolation paths are
;; exercised without the GUI capture stack.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'json)
;; `beads-test-isolation-env-vars' lives with the integration fixtures.
(require 'beads-integration-test)

(defconst beads-manual-test--root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name
                         (locate-library "beads-manual-test"))))
  "Repository root, derived from this test file's location.")

(defconst beads-manual-test--manual-dir
  (expand-file-name "doc" beads-manual-test--root)
  "The hand-authored Texinfo manual source directory.")

(defconst beads-manual-test--images-dir
  (expand-file-name "images" beads-manual-test--manual-dir)
  "The committed screenshot directory.")

(defconst beads-manual-test--capture
  (expand-file-name "docs/manual/screenshots/capture.sh" beads-manual-test--root)
  "The screenshot pipeline orchestrator.")

(defconst beads-manual-test--fixture
  (expand-file-name "docs/manual/screenshots/fixture.sh" beads-manual-test--root)
  "The deterministic fixture project manager.")

(defconst beads-manual-test--shots
  (expand-file-name "docs/manual/screenshots/beads-shots.el" beads-manual-test--root)
  "The beads.el shot list.")

(defconst beads-manual-test--fixture-jsonl
  (expand-file-name "docs/manual/screenshots/fixtures/beads-fixture.jsonl"
                    beads-manual-test--root)
  "The deterministic fixture data.")

(defconst beads-manual-test--expected-views
  '("dashboard" "list" "ready" "blocked" "show" "epic"
    "compose" "molecule" "swarm" "agents" "dispatch"
    "maintenance" "agent-menu" "formula")
  "The complete view set of the capture manifest, in manifest order.")

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun beads-manual-test--run (program args &optional env)
  "Run PROGRAM with ARGS synchronously and return (EXIT-CODE . OUTPUT).
ENV is a list of \"NAME=VALUE\" strings added to the inherited
environment.  OUTPUT concatenates stdout and stderr."
  (let ((out (generate-new-buffer " *beads-manual-stdout*"))
        (err (make-temp-file "beads-manual-stderr"))
        (process-environment (append env process-environment)))
    (unwind-protect
        (cons (apply #'call-process program nil (list out err) nil args)
              (concat (with-current-buffer out (buffer-string))
                      (with-temp-buffer
                        (insert-file-contents err)
                        (buffer-string))))
      (kill-buffer out)
      (delete-file err))))

(defun beads-manual-test--slurp (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun beads-manual-test--views (scope &optional env)
  "Return the view names `capture.sh --list-views --scope SCOPE' selects.
ENV is passed through to `beads-manual-test--run'."
  (let* ((result (beads-manual-test--run
                  beads-manual-test--capture
                  (list "--list-views" "--scope" scope)
                  env))
         (code (car result))
         (output (cdr result)))
    (should (equal code 0))
    (split-string output "[\n\r]+" t)))

(defun beads-manual-test--fixture-issues ()
  "Parse the fixture JSONL into a list of alists."
  (let (issues)
    (with-temp-buffer
      (insert-file-contents beads-manual-test--fixture-jsonl)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (unless (string-empty-p line)
            (push (json-parse-string line :object-type 'alist :array-type 'list)
                  issues)))
        (forward-line 1)))
    (nreverse issues)))

(defun beads-manual-test--export-ids (project)
  "Return the issue ids `bd export' reports from seeded PROJECT.
The isolation variables are unset for the subprocess so an inherited
agent-shell value cannot reroute bd to a live store."
  (let ((default-directory (file-name-as-directory project)))
    (let* ((result (beads-manual-test--run
                    "bd" '("export") beads-test-isolation-env-vars))
           (code (car result))
           (output (cdr result)))
      (should (equal code 0))
      (delq nil
            (mapcar (lambda (line)
                      (when (string-prefix-p "{" line)
                        (alist-get 'id (json-parse-string
                                        line :object-type 'alist))))
                    (split-string output "[\n\r]+" t))))))

(defun beads-manual-test--manual-pages ()
  "Return (FILE . CONTENTS) for every Texinfo chapter in the manual dir."
  (mapcar (lambda (file) (cons file (beads-manual-test--slurp file)))
          (directory-files beads-manual-test--manual-dir t "\\.texi\\'")))

(defun beads-manual-test--image-references ()
  "Return every distinct `images/...png' path a manual form references.
Resolves both the `@shot{<name>, ...}' macro (to `images/<name>.png' and
its `images/<name>-thumb.png' preview) and any remaining
`@image{images/<file>, ...}' form, so
`beads-manual-image-references-exist' proves every referenced PNG exists."
  (let (refs)
    (dolist (page (beads-manual-test--manual-pages))
      (with-temp-buffer
        (insert (cdr page))
        ;; `@shot{<name>, <caption>}' expands to the full-size image (the
        ;; lightbox href) plus its bounded `-thumb' preview.  Anchor to the
        ;; line start so the `@c' comment and `@macro shot' definition do
        ;; not contribute a placeholder such as `NAME'.
        (goto-char (point-min))
        (while (re-search-forward
                "^@shot{\\([A-Za-z0-9._-]+\\)," nil t)
          (let ((name (match-string 1)))
            (push (concat "images/" name ".png") refs)
            (push (concat "images/" name "-thumb.png") refs)))
        ;; Any remaining literal `@image{images/<file>, ...}' form.
        (goto-char (point-min))
        (while (re-search-forward
                "@image{\\(images/[A-Za-z0-9._-]+\\)\\(?:,[^}]*\\)?}" nil t)
          (push (concat (match-string 1) ".png") refs))))
    (delete-dups (nreverse refs))))

(defun beads-manual-test--unbounded-image-references ()
  "Return full-size image paths embedded by a bare `@image' form.
REQ-003 forbids an unbounded full-size `@image' in an illustrated HTML
chapter; the bounded `@shot' macro emits only `-thumb' previews, so any
`@image{images/<file>, ...}' whose file does not end in `-thumb' is a
violation."
  (let (refs)
    (dolist (page (beads-manual-test--manual-pages))
      (with-temp-buffer
        (insert (cdr page))
        (goto-char (point-min))
        (while (re-search-forward
                "@image{\\(images/[A-Za-z0-9._-]+\\)\\(?:,[^}]*\\)?}" nil t)
          (let ((path (match-string 1)))
            (unless (string-suffix-p "-thumb" path)
              (push (concat path ".png") refs))))))
    (nreverse refs)))

(defun beads-manual-test--write-fake-engine (file body)
  "Write an executable fake capture engine to FILE with shell BODY."
  (write-region (concat "#!/usr/bin/env bash\nset -euo pipefail\n" body)
                nil file)
  (set-file-modes file #o755)
  file)

;;; ============================================================
;;; Fixture (REQ-005, REQ-006)
;;; ============================================================

(ert-deftest beads-manual-fixture-jsonl-parses ()
  "The fixture JSONL is a non-empty stream of issue objects."
  :tags '(:unit)
  (let ((issues (beads-manual-test--fixture-issues)))
    (should issues)
    (dolist (issue issues)
      (should (stringp (alist-get 'id issue)))
      (should (stringp (alist-get 'title issue)))
      (should (stringp (alist-get 'status issue))))))

(ert-deftest beads-manual-fixture-role-ids-unique ()
  "Fixture ids are unique and use the stable `demo-<n>' role form."
  :tags '(:unit)
  (let* ((issues (beads-manual-test--fixture-issues))
         (ids (mapcar (lambda (issue) (alist-get 'id issue)) issues)))
    (should (= (length ids) (length (seq-uniq ids))))
    (dolist (id ids)
      (should (string-match-p "\\`demo-[0-9]+\\'" id)))))

(ert-deftest beads-manual-fixture-dependency-targets-exist ()
  "Every dependency points at another fixture issue."
  :tags '(:unit)
  (let* ((issues (beads-manual-test--fixture-issues))
         (ids (seq-uniq (mapcar (lambda (issue) (alist-get 'id issue)) issues))))
    (dolist (issue issues)
      (dolist (dependency (alist-get 'dependencies issue))
        (should (member (alist-get 'depends_on_id dependency) ids))))))

(ert-deftest beads-manual-fixture-export-has-no-generated-ids ()
  "A seeded fixture exports only the stable `demo-<n>' role ids (AC-6).
This inspects a real seeded store, complementing the JSONL-only checks:
bd must not stamp a run-generated hash onto any imported issue."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--fixture))
  (skip-unless (executable-find "bd"))
  (let* ((result (beads-manual-test--run beads-manual-test--fixture '("create")))
         (code (car result))
         (project (seq-find
                   (lambda (line)
                     (and (file-directory-p line)
                          (file-exists-p
                           (expand-file-name ".beads-fixture-project" line))))
                   (split-string (cdr result) "[\n\r]+" t))))
    (should (equal code 0))
    (should project)
    (unwind-protect
        (let ((ids (beads-manual-test--export-ids project)))
          (should (= (length ids) (length (beads-manual-test--fixture-issues))))
          (dolist (id ids)
            (should (string-match-p "\\`demo-[0-9]+\\'" id))))
      (beads-manual-test--run
       beads-manual-test--fixture (list "teardown" project)))))

;;; ============================================================
;;; Scope parser (REQ-004)
;;; ============================================================

(ert-deftest beads-manual-scope-all-maps-views ()
  "Scope `all' selects every manifest view exactly once, in order."
  :tags '(:unit)
  (should (equal (beads-manual-test--views "all")
                 beads-manual-test--expected-views)))

(ert-deftest beads-manual-scope-subset-maps-views ()
  "A comma-separated scope selects exactly those views."
  :tags '(:unit)
  (should (equal (beads-manual-test--views "list,show") '("list" "show")))
  (should (equal (beads-manual-test--views "dashboard") '("dashboard"))))

(ert-deftest beads-manual-scope-changed-maps-views ()
  "Scope `changed' selects the views whose manual source changed."
  :tags '(:unit)
  (let* ((source (expand-file-name "views.texi" beads-manual-test--manual-dir))
         (original (beads-manual-test--slurp source)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert original)
            (insert "\n"))
          (let ((views (beads-manual-test--views
                        "changed" (list "BEADS_SHOT_BASE=HEAD"))))
            (should (member "dashboard" views))
            (should (seq-every-p
                     (lambda (view)
                       (member view (beads-manual-test--views "all")))
                     views))))
      (with-temp-file source
        (insert original)))))

;;; ============================================================
;;; Manifest / shot list / manual page / image contract
;;; ============================================================

(ert-deftest beads-manual-manifest-views-have-shots ()
  "Every manifest view has a thunk in `beads-shots.el'."
  :tags '(:unit)
  (let ((shots (beads-manual-test--slurp beads-manual-test--shots)))
    (dolist (view (beads-manual-test--views "all"))
      (should (string-match-p (format "(list \"%s\"" (regexp-quote view))
                              shots)))))

(ert-deftest beads-manual-manifest-views-have-pages ()
  "Every manifest view is embedded by a Texinfo chapter.
A view is embedded through the bounded `@shot' macro, or through a bare
`@image' in a chapter not yet converted.  This is the REQ-004
image-coverage gate: dropping a view's illustration must fail it."
  :tags '(:unit)
  (let ((pages (beads-manual-test--manual-pages)))
    (dolist (view (beads-manual-test--views "all"))
      (let ((needle (format
                     "\\(?:@shot{%s-\\|@image{images/%s-\\)\\(?:dark\\|light\\)"
                     (regexp-quote view) (regexp-quote view))))
        (should (seq-some
                 (lambda (page) (string-match-p needle (cdr page)))
                 pages))))))

(ert-deftest beads-manual-texinfo-has-no-org-image-links ()
  "The Texinfo chapters keep no Org `[[file:images/...]]' references."
  :tags '(:unit)
  (dolist (page (beads-manual-test--manual-pages))
    (should-not (string-match-p "\\[\\[file:images/" (cdr page)))))

(ert-deftest beads-manual-image-references-exist ()
  "Every `images/*.png' referenced by the manual exists on disk."
  :tags '(:unit)
  (let ((references (beads-manual-test--image-references)))
    (should references)
    (dolist (reference references)
      (should (file-exists-p
               (expand-file-name reference beads-manual-test--manual-dir))))))

(ert-deftest beads-manual-no-unbounded-full-size-image ()
  "No illustrated chapter embeds an unbounded full-size `@image' (REQ-003)."
  :tags '(:unit)
  (should-not (beads-manual-test--unbounded-image-references)))

(ert-deftest beads-manual-manifest-theme-consistency ()
  "The manifest dark theme and the shot engine default agree (REQ-004).
The concrete theme is switched by the theme work item; this gate keeps
`capture.sh' and `beads-shots.el' pinned to one standard dark theme."
  :tags '(:unit)
  (let* ((capture (beads-manual-test--slurp beads-manual-test--capture))
         (shots (beads-manual-test--slurp beads-manual-test--shots))
         (manifest-theme
          (with-temp-buffer
            (insert capture)
            (goto-char (point-min))
            (when (re-search-forward
                   "^[ \t]*\\([A-Za-z0-9._-]+\\)[ \t]+-dark" nil t)
              (match-string 1))))
         (engine-theme
          (and (string-match
                "BEADS_SHOT_THEME\"[ \t]*\"\\([A-Za-z0-9._-]+\\)\""
                shots)
               (match-string 1 shots))))
    (should manifest-theme)
    (should engine-theme)
    (should (equal manifest-theme engine-theme))))

;;; ============================================================
;;; Isolation list drift (plan-review P2)
;;; ============================================================

(ert-deftest beads-manual-isolation-list-matches-fixture ()
  "`fixture.sh' hardcodes the same variables as the test fixtures."
  :tags '(:unit)
  (let* ((result (beads-manual-test--run
                  beads-manual-test--fixture '("print-isolation-vars")))
         (code (car result))
         (output (cdr result)))
    (should (equal code 0))
    (should (equal (sort (split-string output "[\n\r]+" t) #'string<)
                   (sort (copy-sequence beads-test-isolation-env-vars)
                         #'string<)))))

;;; ============================================================
;;; Shell-level checks (plan-review P3)
;;; ============================================================

(ert-deftest beads-manual-scope-invalid-exits-nonzero ()
  "An unknown view makes `capture.sh' fail before it captures anything."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--capture))
  (let ((result (beads-manual-test--run
                 beads-manual-test--capture
                 (list "--scope" "definitely-not-a-view"))))
    (should (/= 0 (car result)))
    (should (string-match-p "unknown view" (cdr result)))))

(ert-deftest beads-manual-verify-detects-drift ()
  "`--verify' fails, naming a drifting image, when two runs differ."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--capture))
  (let* ((dir (make-temp-file "beads-manual-verify-" t))
         (engine (expand-file-name "engine.sh" dir))
         (project (expand-file-name "project" dir))
         (staging (expand-file-name "staging" dir))
         (counter (expand-file-name "counter" dir)))
    (unwind-protect
        (progn
          (make-directory project)
          (beads-manual-test--write-fake-engine
           engine
           (concat
            "n=0\n"
            "if [ -f \"$BEADS_SHOT_COUNTER\" ]; then n=$(cat \"$BEADS_SHOT_COUNTER\"); fi\n"
            "n=$((n + 1))\n"
            "printf '%s' \"$n\" > \"$BEADS_SHOT_COUNTER\"\n"
            "mkdir -p \"$BEADS_SHOT_OUTDIR\"\n"
            "for v in $BEADS_SHOT_VIEWS; do\n"
            "  printf 'run-%s-%s' \"$v\" \"$n\""
            " > \"$BEADS_SHOT_OUTDIR/$v$BEADS_SHOT_SUFFIX.png\"\n"
            "done\n"))
          (let ((result (beads-manual-test--run
                         beads-manual-test--capture
                         (list "--scope" "list" "--verify")
                         (list (concat "BEADS_SHOT_PROJECT=" project)
                               (concat "BEADS_SHOT_ENGINE=" engine)
                               (concat "BEADS_SHOT_STAGING=" staging)
                               (concat "BEADS_SHOT_COUNTER=" counter)))))
            (should (/= 0 (car result)))
            (should (string-match-p "verify FAILED" (cdr result)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest beads-manual-verify-scope-leaves-non-selected ()
  "`--verify-scope' passes when only out-of-scope raws are produced."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--capture))
  (let* ((source (expand-file-name "list-dark.png" beads-manual-test--images-dir))
         (dir (make-temp-file "beads-manual-scope-" t))
         (engine (expand-file-name "engine.sh" dir))
         (project (expand-file-name "project" dir))
         (staging (expand-file-name "staging" dir)))
    (skip-unless (file-exists-p source))
    (unwind-protect
        (progn
          (make-directory project)
          (beads-manual-test--write-fake-engine
           engine
           (concat
            "mkdir -p \"$BEADS_SHOT_OUTDIR\"\n"
            "# Deliberately write a raw for a view outside the selected scope.\n"
            "cp \"$BEADS_SHOT_FIXTURE_IMAGE\""
            " \"$BEADS_SHOT_OUTDIR/list$BEADS_SHOT_SUFFIX.png\"\n"))
          (let ((result (beads-manual-test--run
                         beads-manual-test--capture
                         (list "--verify-scope" "show")
                         (list (concat "BEADS_SHOT_PROJECT=" project)
                               (concat "BEADS_SHOT_ENGINE=" engine)
                               (concat "BEADS_SHOT_STAGING=" staging)
                               (concat "BEADS_SHOT_FIXTURE_IMAGE=" source)))))
            (should (equal 0 (car result)))
            (should (string-match-p "verify-scope:" (cdr result)))))
      (ignore-errors (delete-directory dir t)))))

(provide 'beads-manual-test)
;;; beads-manual-test.el ends here
