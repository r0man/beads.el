;;; beads-manual-test.el --- Tests for the user manual + screenshot pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Guards for W7 and W8 (implementation plan WS-8; REQ-014, REQ-015;
;; AC-004, AC-005; plus the W7 fixture/scope/manifest guards): the
;; deterministic fixture, the capture scope parser, the
;; manifest/view/page/image contract, the isolation list shared with
;; `beads-test-isolation-env-vars', the no-third-party-theme policy, and
;; image-orphan coverage.
;;
;; The `:unit' tests read files and run the resolution-only paths of
;; `capture.sh --list-views' and `fixture.sh print-isolation-vars', none
;; of which need `bd', sway, or a graphical Emacs.  The `:integration'
;; tests drive `capture.sh' with a fake capture engine
;; (`BEADS_SHOT_ENGINE') so the verify and subset-isolation paths are
;; exercised without the GUI capture stack.

;;; Code:

(require 'cl-lib)
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

(defconst beads-manual-test--forbidden
  (expand-file-name "scripts/check-manual-forbidden" beads-manual-test--root)
  "The offline forbidden-host-name gate (REQ-015, REQ-016).")

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

(defconst beads-manual-test--theme-path-files
  (mapcar (lambda (file) (expand-file-name file beads-manual-test--root))
          '("docs/manual/screenshots/capture.sh"
            "docs/manual/screenshots/fixture.sh"
            "docs/manual/screenshots/formula-verify.sh"
            "docs/manual/screenshots/beads-shots.el"
            "docs/manual/screenshots/screenshot.el"
            "scripts/emacs-minimal"
            "share/emacs.d/init.el"
            "Makefile.am"
            "guix.scm"))
  "The capture/test/build path scanned for third-party theme use (AC-004).
Prose (the screenshot README) is excluded on purpose: it documents the
removed requirement, it does not load a theme.")

(defconst beads-manual-test--third-party-theme-patterns
  '(("\\buse-package[ \t\n]+['\"]?[a-z0-9-]*themes?\\b"
     . "use-package of a theme")
    ("\\bemacs-[a-z0-9-]*themes?\\b"
     . "theme package requirement")
    ("\\(?:require\\|load\\|load-file\\)[ \t]+['\"]ef-"
     . "require/load of an ef- theme")
    ("screenshot-theme-library[ \t\n]+['\"]"
     . "literal screenshot-theme-library")
    ("\\bBEADS_SHOT_THEME_LIBRARY=['\"]?[^'\" \t$]"
     . "literal BEADS_SHOT_THEME_LIBRARY"))
  "Requirement/load forms that would introduce a third-party theme.
Each entry is (REGEXP . DESCRIPTION).  A docstring that merely cites
`ef-themes' as an example matches none of these.")

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

(defun beads-manual-test--run-stdout (program args &optional env)
  "Run PROGRAM with ARGS and return (EXIT-CODE . STDOUT).
Standard error is discarded, so a caller can assert a command's reserved
stdout contract (the pipeline sends all progress chatter to stderr)."
  (let ((out (generate-new-buffer " *beads-manual-stdout-only*"))
        (process-environment (append env process-environment)))
    (unwind-protect
        (cons (apply #'call-process program nil (list out nil) nil args)
              (with-current-buffer out (buffer-string)))
      (kill-buffer out))))

(defun beads-manual-test--theme-violations (&optional files)
  "Return \"FILE:LINE: DESCRIPTION\" for third-party theme use in FILES.
FILES defaults to the capture/test/build path.  Only requirement/load
forms are matched, so the verbatim engine docstring -- which merely cites
`ef-themes' as an example of an external theme library -- cannot trip the
gate.  A missing file is itself reported, because the gate must not pass
vacuously after a rename."
  (let (violations)
    (dolist (file (or files beads-manual-test--theme-path-files))
      (cond
       ((not (file-readable-p file))
        (push (format "%s: missing from the capture path"
                      (file-relative-name file beads-manual-test--root))
              violations))
       (t
        (let ((text (beads-manual-test--slurp file)))
          (dolist (pattern beads-manual-test--third-party-theme-patterns)
            (let ((pos 0))
              (while (string-match (car pattern) text pos)
                (with-temp-buffer
                  (insert text)
                  (push (format "%s:%d: %s"
                                (file-relative-name file
                                                   beads-manual-test--root)
                                (line-number-at-pos (match-beginning 0))
                                (cdr pattern))
                        violations))
                (setq pos (match-end 0)))))))))
    (nreverse violations)))

(defun beads-manual-test--manifest-themes ()
  "Return the THEME field of every slot line in the capture manifest."
  (let (themes)
    (with-temp-buffer
      (insert (beads-manual-test--slurp beads-manual-test--capture))
      (goto-char (point-min))
      (when (search-forward "read -r -d '' MANIFEST" nil t)
        (forward-line 1)
        (while (and (not (eobp)) (not (looking-at-p "EOF")))
          (let ((line (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))
            (unless (string-empty-p line)
              (push (car (split-string line "[\t ]+")) themes)))
          (forward-line 1))))
    (nreverse themes)))

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

(defun beads-manual-test--referenced-images (&optional pages)
  "Return the set of `images/...' references PAGES make.
Resolves the bounded `@shot{<name>, ...}' macro to `images/<name>.png'
and its `images/<name>-thumb.png' preview, plus any literal
`images/<file>.png'.  PAGES defaults to the handwritten chapters."
  (let ((referenced (make-hash-table :test #'equal)))
    (dolist (page (or pages (beads-manual-test--manual-pages)))
      (dolist (line (split-string (cdr page) "\n" t))
        (cond
         ((string-prefix-p "@shot{" line)
          (let ((name (string-trim
                       (car (split-string (substring line 6) ",")))))
            (puthash (concat "images/" name ".png") t referenced)
            (puthash (concat "images/" name "-thumb.png") t referenced)))
         (t
          (let ((pos 0))
            (while (string-match "images/[A-Za-z0-9._-]+[.]png" line pos)
              (puthash (substring line (match-beginning 0) (match-end 0))
                       t referenced)
              (setq pos (match-end 0))))))))
    referenced))

(defun beads-manual-test--orphan-images (&optional images-dir pages)
  "Return the `images/...' references no page embeds.
IMAGES-DIR defaults to the manual's images directory and PAGES to the
handwritten chapters.  Both the full image and its `-thumb' must be
referenced; any other committed PNG is an orphan (AC-005)."
  (let ((referenced (beads-manual-test--referenced-images pages))
        (dir (or images-dir beads-manual-test--images-dir)))
    (seq-remove
     (lambda (reference) (gethash reference referenced))
     (mapcar (lambda (file) (concat "images/" file))
             (seq-filter (lambda (file) (string-suffix-p ".png" file))
                         (directory-files dir nil nil t))))))

(defun beads-manual-test--write-fake-engine (file body)
  "Write an executable fake capture engine to FILE with shell BODY."
  (write-region (concat "#!/usr/bin/env bash\nset -euo pipefail\n" body)
                nil file)
  (set-file-modes file #o755)
  file)

(defun beads-manual-test--hold-lock (lockfile ready)
  "Hold an exclusive flock on LOCKFILE, touching READY once acquired.
Return the holder process; kill it in an `unwind-protect' cleanup.  The
ready file makes acquisition observable so the concurrency assertions do
not depend on a fixed sleep."
  (let ((proc (make-process
               :name "beads-manual-lock-holder"
               :command (list "flock" "-x" lockfile
                              "sh" "-c"
                              (concat "touch "
                                      (shell-quote-argument ready)
                                      "; sleep 60"))
               :connection-type 'pipe
               :noquery t))
        (deadline (+ (float-time) 10)))
    (while (and (not (file-exists-p ready)) (< (float-time) deadline))
      (sleep-for 0.05))
    (should (file-exists-p ready))
    proc))

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

(ert-deftest beads-manual-fixture-shared-path-contract ()
  "The fixture uses the fixed shared project path and a next-to lock (REQ-001).
The sibling gascity.el manual uses the same `${TMPDIR:-/tmp}/demo-project'
path, so the run parent must no longer be a `mktemp -d' directory."
  :tags '(:unit)
  (let ((script (beads-manual-test--slurp beads-manual-test--fixture)))
    (should (string-match-p "FIXTURE_PROJECT_DIR=\"\\${FIXTURE_TMPDIR}/demo-project\""
                            script))
    (should (string-match-p "FIXTURE_LOCK_FILE=\"\\${FIXTURE_TMPDIR}/demo-project\\.lock\""
                            script))
    (should (string-match-p "FIXTURE_DATA_SHA256=\"[0-9a-f]\\{64\\}\"" script))
    (should (string-match-p "FIXTURE_LOCK_HELD" script))
    (should-not (string-match-p "\\bmktemp\\b" script))))

(ert-deftest beads-manual-fixture-dataset-hash-recorded ()
  "The frozen dataset hash is pinned once and recorded in both docs (REQ-002)."
  :tags '(:unit)
  (let* ((script (beads-manual-test--slurp beads-manual-test--fixture))
         (hash (and (string-match
                     "FIXTURE_DATA_SHA256=\"\\([0-9a-f]+\\)\"" script)
                    (match-string 1 script)))
         (actual (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally beads-manual-test--fixture-jsonl)
                   (secure-hash 'sha256 (current-buffer))))
         (readme (beads-manual-test--slurp
                  (expand-file-name "README.md"
                                    (file-name-directory beads-manual-test--fixture)))))
    (should hash)
    (should (equal hash actual))
    (should (string-match-p (regexp-quote hash) readme))
    (should (seq-some
             (lambda (page) (string-match-p (regexp-quote hash) (cdr page)))
             (beads-manual-test--manual-pages)))))

(ert-deftest beads-manual-fixture-refuses-markerless-path ()
  "`create' refuses a directory at the shared path without the marker (REQ-001).
The refusal must not delete anything; it is the guard that lets the fixed
path be recreated safely."
  :tags '(:unit)
  (skip-unless (file-executable-p beads-manual-test--fixture))
  (skip-unless (executable-find "flock"))
  (let* ((tmp (make-temp-file "beads-manual-fixture-path-" t))
         (project (expand-file-name "demo-project" tmp))
         (keep (expand-file-name "keep.txt" project)))
    (unwind-protect
        (progn
          (make-directory project)
          (write-region "not ours\n" nil keep)
          (let ((result (beads-manual-test--run
                         beads-manual-test--fixture '("create")
                         (list (concat "TMPDIR=" tmp)))))
            (should (/= 0 (car result)))
            (should (string-match-p "refusing" (cdr result)))
            (should (file-exists-p keep))))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest beads-manual-fixture-teardown-is-path-guarded ()
  "`teardown' refuses a foreign path and is idempotent at the shared path."
  :tags '(:unit)
  (skip-unless (file-executable-p beads-manual-test--fixture))
  (skip-unless (executable-find "flock"))
  (let* ((tmp (make-temp-file "beads-manual-fixture-teardown-" t))
         (project (expand-file-name "demo-project" tmp))
         (other (expand-file-name "other" tmp)))
    (unwind-protect
        (progn
          (make-directory other)
          (let ((result (beads-manual-test--run
                         beads-manual-test--fixture (list "teardown" other)
                         (list (concat "TMPDIR=" tmp)))))
            (should (/= 0 (car result)))
            (should (string-match-p "refusing" (cdr result)))
            (should (file-directory-p other)))
          ;; The shared path is already gone: teardown is a no-op success.
          (let ((result (beads-manual-test--run
                         beads-manual-test--fixture (list "teardown" project)
                         (list (concat "TMPDIR=" tmp)))))
            (should (equal 0 (car result)))
            (should (string-match-p "already gone" (cdr result)))))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest beads-manual-fixture-recreates-stale-marker-project ()
  "`create' replaces a stale marker project at the shared path (REQ-001).
The fixed path is reused on every run, so a leftover fixture carrying the
marker must be recreated instead of refused -- and the command still
prints exactly one stdout line, the project path."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--fixture))
  (skip-unless (executable-find "bd"))
  (skip-unless (executable-find "flock"))
  (let* ((tmp (make-temp-file "beads-manual-fixture-stale-" t))
         (project (expand-file-name "demo-project" tmp))
         (env (list (concat "TMPDIR=" tmp)))
         (first (beads-manual-test--run-stdout
                 beads-manual-test--fixture '("create") env)))
    (unwind-protect
        (progn
          (should (equal 0 (car first)))
          (should (equal (string-trim (cdr first)) project))
          (should (file-exists-p
                   (expand-file-name ".beads-fixture-project" project)))
          (let ((second (beads-manual-test--run-stdout
                         beads-manual-test--fixture '("create") env)))
            (should (equal 0 (car second)))
            (should (equal (string-trim (cdr second)) project))
            (should (file-exists-p
                     (expand-file-name ".beads-fixture-project" project)))))
      (beads-manual-test--run beads-manual-test--fixture
                              (list "teardown" project) env)
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest beads-manual-fixture-serializes-on-shared-lock ()
  "Concurrent fixture runs serialize on the shared lock (REQ-001).
A nested caller that sets `FIXTURE_LOCK_HELD=1' (as `capture.sh' does)
must skip acquisition instead of deadlocking; an independent caller must
wait and then fail rather than share the store."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--fixture))
  (skip-unless (executable-find "flock"))
  (let* ((tmp (make-temp-file "beads-manual-fixture-lock-" t))
         (project (expand-file-name "demo-project" tmp))
         (ready (expand-file-name "lock-ready" tmp))
         (env (list (concat "TMPDIR=" tmp) "FIXTURE_LOCK_TIMEOUT=1"))
         (proc (beads-manual-test--hold-lock (concat project ".lock") ready)))
    (unwind-protect
        (progn
          (let ((result (beads-manual-test--run
                         beads-manual-test--fixture (list "teardown" project)
                         (cons "FIXTURE_LOCK_HELD=1" env))))
            (should (equal 0 (car result)))
            (should (string-match-p "already gone" (cdr result))))
          (let ((result (beads-manual-test--run
                         beads-manual-test--fixture (list "teardown" project)
                         env)))
            (should (/= 0 (car result)))
            (should (string-match-p "timed out" (cdr result)))))
      (ignore-errors (delete-process proc))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest beads-manual-capture-waits-on-shared-lock ()
  "A second capture waits for the shared lock, then fails (REQ-001).
It must never run concurrently against the one fixed store."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--capture))
  (skip-unless (executable-find "flock"))
  (let* ((tmp (make-temp-file "beads-manual-capture-lock-" t))
         (project (expand-file-name "demo-project" tmp))
         (ready (expand-file-name "lock-ready" tmp))
         (proc (beads-manual-test--hold-lock (concat project ".lock") ready)))
    (unwind-protect
        (let ((result (beads-manual-test--run
                       beads-manual-test--capture
                       (list "--scope" "list")
                       (list (concat "TMPDIR=" tmp)
                             "BEADS_SHOT_LOCK_TIMEOUT=1"
                             (concat "BEADS_SHOT_STAGING="
                                     (expand-file-name "staging" tmp))))))
          (should (/= 0 (car result)))
          (should (string-match-p "timed out" (cdr result)))
          (should-not (file-directory-p project)))
      (ignore-errors (delete-process proc))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest beads-manual-fixture-export-has-no-generated-ids ()
  "A seeded fixture exports only the stable `demo-<n>' role ids (AC-6).
This inspects a real seeded store, complementing the JSONL-only checks:
bd must not stamp a run-generated hash onto any imported issue.  `TMPDIR'
is scoped to a private directory: the default shared path may legitimately
hold a live sibling fixture (the gascity.el manual uses the same path with
its own marker), and `create' refuses rather than deletes a foreign one."
  :tags '(:integration)
  (skip-unless (file-executable-p beads-manual-test--fixture))
  (skip-unless (executable-find "bd"))
  (let* ((tmp (make-temp-file "beads-manual-fixture-export-" t))
         (env (list (concat "TMPDIR=" tmp)))
         (result (beads-manual-test--run beads-manual-test--fixture '("create") env))
         (code (car result))
         (project (seq-find
                   (lambda (line)
                     (and (file-directory-p line)
                          (file-exists-p
                           (expand-file-name ".beads-fixture-project" line))))
                   (split-string (cdr result) "[\n\r]+" t))))
    (unwind-protect
        (progn
          (should (equal code 0))
          (should (equal project (expand-file-name "demo-project" tmp)))
          (when project
            (let ((ids (beads-manual-test--export-ids project)))
              (should (= (length ids)
                         (length (beads-manual-test--fixture-issues))))
              (dolist (id ids)
                (should (string-match-p "\\`demo-[0-9]+\\'" id))))
            (beads-manual-test--run
             beads-manual-test--fixture (list "teardown" project) env)))
      (ignore-errors (delete-directory tmp t)))))

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
;;; Shot-list id normalization
;;; ============================================================

(ert-deftest beads-manual-normalize-ids-keeps-fixture-project-name ()
  "Normalization rewrites generated ids but never the fixture's name.
The shared project basename (`demo-project') matches the id syntax, so
without this the dashboard header and the show view's project row would
be rewritten -- and both are read-only, which aborted the shot."
  :tags '(:unit)
  (let ((process-environment
         (cons "BEADS_SHOTS_NO_RUN=1"
               (cons "BEADS_SHOT_DIR=/tmp/demo-project"
                     process-environment))))
    ;; Load the shot list without starting a graphical capture.
    (load beads-manual-test--shots t t)
    (cl-letf (((symbol-function 'beads-shots--progress) #'ignore))
      (with-temp-buffer
        (insert "Beads-Dashboard - demo-project\n"
                "role demo-10\n"
                "generated demo-abc123\n"
                "frozen demo-q9z2\n")
        ;; A generated id inside read-only text is left alone rather than
        ;; aborting the shot.
        (goto-char (point-min))
        (search-forward "frozen ")
        (put-text-property (point) (line-end-position) 'read-only t)
        (beads-shots--normalize-ids (current-buffer))
        (should (equal (buffer-string)
                       (concat "Beads-Dashboard - demo-project\n"
                               "role demo-10\n"
                               "generated demo-role\n"
                               "frozen demo-q9z2\n")))))))

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

(ert-deftest beads-manual-image-orphans ()
  "Every committed image, full size and thumbnail, is referenced (AC-005).
This is the reverse of `beads-manual-image-references-exist': a committed
PNG that no `@shot' or `@image' embeds is an orphan the manual no longer
shows."
  :tags '(:unit)
  (let ((committed
         (mapcar (lambda (file) (concat "images/" file))
                 (seq-filter (lambda (file) (string-suffix-p ".png" file))
                             (directory-files beads-manual-test--images-dir
                                              nil nil t)))))
    (should committed)
    (should-not (beads-manual-test--orphan-images))))

(ert-deftest beads-manual-image-orphans-detects-injected ()
  "The orphan gate reports an unreferenced full image or thumbnail."
  :tags '(:unit)
  (let* ((dir (make-temp-file "beads-manual-orphans-" t))
         (page (expand-file-name "page.texi" dir))
         (images (expand-file-name "images" dir)))
    (unwind-protect
        (progn
          (make-directory images)
          (write-region "@shot{list, A list view}\n" nil page)
          (dolist (name '("list.png" "list-thumb.png" "stale.png"))
            (write-region name nil (expand-file-name name images)))
          (should (equal (beads-manual-test--orphan-images
                          images
                          (list (cons page (beads-manual-test--slurp page))))
                         '("images/stale.png"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest beads-manual-manifest-theme-consistency ()
  "The manifest slots and the shot engine resolve to the same default (REQ-004).
Both slots name the `default' sentinel, which `beads-shots-theme' resolves
to nil (load no theme); the manifest must not claim a third-party theme
that the capture path does not load."
  :tags '(:unit)
  (let ((themes (beads-manual-test--manifest-themes)))
    (should themes)
    (should (equal themes (make-list (length themes) "default")))
    (let ((process-environment
           (cons "BEADS_SHOTS_NO_RUN=1"
                 (cons "BEADS_SHOT_THEME=default"
                       (cons "BEADS_SHOT_THEME_LIBRARY="
                             process-environment)))))
      (load beads-manual-test--shots t t)
      (should-not (beads-shots-theme))
      (should-not (beads-shots-theme-library)))))

(ert-deftest beads-manual-no-third-party-theme ()
  "The capture, test, and build path loads no third-party theme (REQ-004).
`beads-shots.el' is the only theme switch: the `default' sentinel means
no theme, and a library is consulted only from an explicit
`BEADS_SHOT_THEME_LIBRARY' value.  The verbatim engine docstring's
`ef-themes' example is a citation, not a requirement or a load, so only
requirement/load forms are matched."
  :tags '(:unit)
  (let ((violations (beads-manual-test--theme-violations)))
    (should-not violations))
  ;; The engine keeps a nil default, so a library can only reach it through
  ;; the explicit environment value (an operator's opt-in).
  (should (string-match-p
           "screenshot-theme-library nil"
           (beads-manual-test--slurp
            (expand-file-name "docs/manual/screenshots/screenshot.el"
                              beads-manual-test--root)))))

(ert-deftest beads-manual-no-third-party-theme-detects-injected ()
  "The theme gate reports a requirement/load but ignores a citation."
  :tags '(:unit)
  (let ((file (make-temp-file "beads-manual-theme-" nil ".el")))
    (unwind-protect
        (progn
          (write-region (concat "(use-package ef-themes)\n"
                                "(require 'ef-cyprus)\n"
                                "(setq screenshot-theme-library \"ef-themes\")\n")
                        nil file)
          (should (= 3 (length (beads-manual-test--theme-violations
                                (list file)))))
          ;; The verbatim engine docstring cites an example only; a
          ;; citation must not be flagged, nor must the nil default.
          (write-region (concat "(defun x () \"(e.g. ef-themes from a Guix profile).\")\n"
                                "(defvar screenshot-theme-library nil)\n")
                        nil file)
          (should-not (beads-manual-test--theme-violations (list file))))
      (ignore-errors (delete-file file)))))

;;; ============================================================
;;; Forbidden host-name gate (REQ-015, REQ-016)
;;; ============================================================

(ert-deftest beads-manual-forbidden-gate-passes-tree ()
  "The checked-in manual sources carry no forbidden host name."
  :tags '(:unit)
  (skip-unless (file-executable-p beads-manual-test--forbidden))
  (let ((result (beads-manual-test--run beads-manual-test--forbidden nil)))
    (should (equal 0 (car result)))
    (should (string-empty-p (string-trim (cdr result))))))

(ert-deftest beads-manual-forbidden-gate-detects-injected ()
  "The gate prints `path:line' and fails on an injected token."
  :tags '(:unit)
  (skip-unless (file-executable-p beads-manual-test--forbidden))
  (let* ((root (make-temp-file "beads-manual-forbidden-" t))
         (script (expand-file-name "scripts/check-manual-forbidden" root))
         (texi (expand-file-name "doc/beads.texi" root))
         (html (expand-file-name "doc/beads.html" root))
         (notes (expand-file-name "docs/manual/screenshots/fixtures/x.jsonl"
                                  root))
         (image (expand-file-name "docs/manual/images/x.png" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory script) t)
          (make-directory (file-name-directory texi) t)
          (make-directory (file-name-directory notes) t)
          (make-directory (file-name-directory image) t)
          (copy-file beads-manual-test--forbidden script)
          (set-file-modes script #o755)
          ;; A clean source plus a case-variant token and a generated-HTML
          ;; token; the binary image hit must be ignored.
          (write-region "clean line\n" nil texi)
          (write-region "visit Burningswell.example\n" nil html)
          (write-region "clean\n" nil notes)
          (write-region "burningswell in an image path\n" nil image)
          (let ((result (beads-manual-test--run script nil)))
            (should (/= 0 (car result)))
            (should (string-match-p "doc/beads\\.html:1:.*[Bb]urningswell"
                                    (cdr result)))
            (should-not (string-match-p "images/x\\.png" (cdr result)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest beads-manual-forbidden-gate-detects-build-input ()
  "The gate scans the manual build inputs under `docs/manual/'."
  :tags '(:unit)
  (skip-unless (file-executable-p beads-manual-test--forbidden))
  (let* ((root (make-temp-file "beads-manual-forbidden-" t))
         (script (expand-file-name "scripts/check-manual-forbidden" root))
         (texi (expand-file-name "doc/beads.texi" root))
         (notes (expand-file-name "docs/manual/screenshots/fixture.sh" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory script) t)
          (make-directory (file-name-directory texi) t)
          (make-directory (file-name-directory notes) t)
          (copy-file beads-manual-test--forbidden script)
          (set-file-modes script #o755)
          (write-region "clean line\n" nil texi)
          (write-region "# burningswell.com\n" nil notes)
          (let ((result (beads-manual-test--run script nil)))
            (should (/= 0 (car result)))
            (should (string-match-p
                     "docs/manual/screenshots/fixture\\.sh:1:" (cdr result)))))
      (ignore-errors (delete-directory root t)))))

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
