;;; beads-util.el --- Low-level utilities for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; Low-level utility functions used throughout beads.el modules.
;; This module exists so that ~30 modules can require just the
;; utilities without pulling in the full beads.el entry point,
;; breaking the circular dependency where beads.el requires
;; beads-command which needs beads utilities.

;;; Code:

(require 'beads-custom)
(require 'beads-git)
(require 'beads-remote)

;; Forward declarations for optional dependencies
(declare-function beads-from-json "beads-types")
(declare-function beads-completion-invalidate-cache "beads-completion")
(declare-function beads-completion-issue-table "beads-completion")
(declare-function beads-completion--get-cached-issues "beads-completion")

;;; Variables

(defvar beads--project-cache (make-hash-table :test 'equal)
  "Cache of project roots to .beads directories.")

;; Forward declarations for global option variables (defined in beads-option.el)
(defvar beads-global-actor nil)
(defvar beads-global-db nil)
(defvar beads-global-directory nil)
(defvar beads-global-global nil)
(defvar beads-global-json nil)
(defvar beads-global-sandbox nil)

;;; Constants

(defconst beads-display-value-max-length 40
  "Maximum length for displaying values in transient menus.
Values longer than this will be truncated with \"...\" appended.")

(defconst beads-separator-line-width 40
  "Width of separator lines in error and debug buffers.")

(defconst beads-stats-separator-width 50
  "Width of separator lines in statistics display.")

(defconst beads-graph-label-max-length 30
  "Maximum length for issue titles in dependency graphs.
Longer titles will be truncated for graph display.")

;;; Issue Identifiers

(defconst beads-issue-id-regexp
  "\\([a-zA-Z][a-zA-Z0-9._-]*-[0-9a-z]+\\(?:\\.[0-9]+\\)*\\)"
  "Regexp matching a beads issue id; group 1 is the id.
An id is PREFIX-HASH[.CHILD...]: PREFIX starts with a letter and may
contain letters, digits, dots, underscores and hyphens (\"bd\",
\"beads.el\", \"my_project\"); HASH is a lowercase base-36 string --
NOT hexadecimal, real ids look like bs-lc1lb, gce-hck or bde-dww, which
a hex class never matched; CHILD suffixes are numeric (bd-a1b2.1.3).
The regexp carries no \\b anchors because hyphens and dots are word
boundaries to Emacs, so \\b would accept \"agent-abc\" inside
\"gc-agent-abc\"; use `beads-issue-id-search-forward' or
`beads-issue-id-at-point', which apply `beads-issue-id--boundary-p'.")

(defvaralias 'beads-eldoc-issue-prefixes 'beads-issue-id-prefixes)

(defcustom beads-issue-id-prefixes nil
  "Issue id prefixes to recognise in text, or nil for any.
A list of strings such as (\"bde\" \"gce\").  The base-36 id syntax
also matches ordinary hyphenated words (\"post-command\"), so a caller
that knows which stores are in play -- gascity.el knows every rig's
prefix -- sets this buffer-locally and eldoc/reference detection then
ignores everything else.  Nil accepts any syntactically valid id."
  :type '(repeat string)
  :local t
  :group 'beads)


(defun beads-issue-id--allowed-p (id prefixes)
  "Return non-nil when ID's prefix is in PREFIXES, or PREFIXES is nil."
  (or (null prefixes)
      (seq-some (lambda (prefix) (string-prefix-p (concat prefix "-") id))
                prefixes)))

(defun beads-issue-id--boundary-p (beg end)
  "Return non-nil when the id between BEG and END stands alone.
The character before BEG must not be part of an id prefix, and the
character after END must not extend the id, so \"agent-abc\" inside
\"gc-agent-abc\" is rejected while a trailing dot or comma is fine."
  (and (or (= beg (point-min))
           (not (string-match-p "[[:alnum:]._-]" (string (char-before beg)))))
       (or (= end (point-max))
           (not (string-match-p "[[:alnum:]_-]" (string (char-after end)))))))

(defun beads-issue-id-search-forward (&optional bound regexp prefixes)
  "Search forward for the next issue id ending before BOUND.
REGEXP defaults to `beads-issue-id-regexp' (group 1 must be the id);
PREFIXES defaults to nil, accept any -- callers normally pass
`beads-issue-id-prefixes'.  Matches that fail `beads-issue-id--boundary-p'
or the prefix allowlist are skipped.  Returns the id with the match
data set (group 1) and point after it, or nil with point at BOUND."
  (let ((regexp (or regexp beads-issue-id-regexp))
        (case-fold-search nil)
        (found nil))
    (while (and (not found) (re-search-forward regexp bound t))
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (if (and (beads-issue-id--boundary-p beg end)
                 (beads-issue-id--allowed-p (match-string-no-properties 1)
                                            prefixes))
            (setq found (match-string-no-properties 1))
          ;; Retry one character in: the greedy prefix may have swallowed
          ;; a leading token, or this is a non-id word to step past.
          (goto-char (1+ beg)))))
    found))

(defun beads-issue-id-search-backward (&optional bound regexp prefixes)
  "Search backward for the closest issue id ending at or before point.
A plain `re-search-backward' will not do: it stops at the nearest
start position, which for \"bd-1\" is the tail \"d-1\", and once that is
rejected the real id is excluded because it extends past point.  So
scan forward from BOUND (default `point-min') with
`beads-issue-id-search-forward' -- REGEXP and PREFIXES as there -- and
keep the last id ending before point.  On success point is at the id's
start, the match data (group 1) is set and the id returned; otherwise
nil with point unchanged."
  (let ((origin (point))
        (last nil))
    (save-excursion
      (goto-char (or bound (point-min)))
      (while (beads-issue-id-search-forward origin regexp prefixes)
        (setq last (cons (match-beginning 1) (match-end 1)))))
    (when last
      (goto-char (car last))
      (set-match-data (list (car last) (cdr last) (car last) (cdr last)))
      (buffer-substring-no-properties (car last) (cdr last)))))

(defun beads-issue-id-at-point (&optional prefixes regexp)
  "Return the issue id at point, or nil.
A button carrying an `issue-id' property wins (beads-show references);
otherwise the current line is scanned with `beads-issue-id-search-forward'
for an id overlapping point.  PREFIXES and REGEXP are passed through."
  (or (when-let* ((button (button-at (point))))
        (button-get button 'issue-id))
      (save-excursion
        (let ((pos (point))
              (end (line-end-position))
              (found nil))
          (goto-char (line-beginning-position))
          (while (and (not found)
                      (beads-issue-id-search-forward end regexp prefixes))
            (when (and (<= (match-beginning 1) pos)
                       (<= pos (match-end 1)))
              (setq found (match-string-no-properties 1))))
          found))))

;;; Utilities

(defun beads--log (level format-string &rest args)
  "Log message to *beads-debug* buffer if debug is enabled.
LEVEL is one of `error', `info', or `verbose'.
FORMAT-STRING and ARGS are passed to `format'.

The log format is compatible with `log-view-mode':
  TIMESTAMP [LEVEL] message"
  (when beads-enable-debug
    ;; Check if this message should be logged based on level
    (when (or (eq level 'error)
              (and (eq beads-debug-level 'info)
                   (memq level '(error info)))
              (eq beads-debug-level 'verbose))
      (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
             ;; Map verbose -> DEBUG for log-view-mode compatibility
             (level-str (if (eq level 'verbose) "DEBUG" (upcase (symbol-name level))))
             (msg (apply #'format format-string args))
             (log-line (format "%s [%-5s] %s\n" timestamp level-str msg))
             (buf (get-buffer-create "*beads-debug*")))
        ;; Log to buffer
        (with-current-buffer buf
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert log-line)))
        ;; Auto-scroll if buffer is visible in a window
        (when-let* ((win (get-buffer-window buf)))
          (with-selected-window win
            (goto-char (point-max))
            (recenter -1)))))))

(defun beads--error (format-string &rest args)
  "Display error message to user.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((msg (apply #'format format-string args)))
    (apply #'beads--log 'error "ERROR: %s" (list msg))
    (user-error "Beads: %s" msg)))

(defun beads--string-blank-p (value)
  "Return non-nil if VALUE is blank.
A value is considered blank if it is nil, not a string, or an empty string.
This function safely handles non-string values without signaling an error,
which is useful when validating transient arguments that may return a
non-string truthy value instead of a string in some transient versions."
  (or (null value)
      (not (stringp value))
      (string-empty-p (string-trim value))))

(defun beads--sanitize-string (value)
  "Return VALUE if it is a non-blank string, otherwise nil.
This ensures that non-string values (like t) and empty strings are
converted to nil, which is useful for processing transient arguments."
  (when (and (stringp value)
             (not (string-empty-p (string-trim value))))
    value))

;;; Beads Directory Discovery

(defun beads--find-beads-dir (&optional directory)
  "Find .beads directory starting from DIRECTORY.
If DIRECTORY is nil, uses `default-directory'.
Returns the path to .beads directory or nil if not found.

Search order:
1. Walk up from DIRECTORY/`default-directory' looking for .beads
   (ensures worktree-local .beads is found before main repo's)
2. If not found and in a git worktree, check the main repository"
  (let* ((start-dir (or directory default-directory))
         (cached (gethash start-dir beads--project-cache)))
    (if cached
        cached
      ;; Try local discovery first
      (let ((beads-dir (locate-dominating-file start-dir ".beads")))
        ;; If not found locally, check if we're in a worktree
        (unless beads-dir
          (when-let* ((main-repo (beads-git-find-main-repo)))
            (let ((main-beads (expand-file-name ".beads" main-repo)))
              (when (file-directory-p main-beads)
                (setq beads-dir main-repo)))))
        (when beads-dir
          (let ((full-path (expand-file-name ".beads" beads-dir)))
            (puthash start-dir full-path beads--project-cache)
            full-path))))))

(defcustom beads-project-root-markers
  '(".beads" "city.toml" ".gc" "pack.toml")
  "Filenames marking a non-git beads project root.
Each entry is matched with `file-exists-p', so files and
directories both qualify.  The markers are:

  - \".beads\" — a plain beads project (the data directory bd
    itself creates); the authoritative marker.
  - \"city.toml\", \".gc\", \"pack.toml\" — Gas City workspace
    manifests.  A Gas City city/pack root keeps its \".beads/\" at
    the city root rather than inside a \".git\"-bearing repo, so we
    recognize the city/pack manifests directly.

None of these require a \".git\" directory at the project root.  The
Gas City entries are deliberately specific manifest names, not
generic config files; \"pack.toml\" in particular is the Gas City
pack manifest.  If one of them ever collides with an unrelated
tool's config and mis-detects a non-beads directory, remove that
entry from this list rather than widening it — \".beads\" alone
covers every real beads project."
  :type '(repeat string)
  :group 'beads)

(defun beads--find-project-root (&optional directory)
  "Return the project root at or above DIRECTORY, or nil.
If DIRECTORY is nil, uses `default-directory'.

Walks up looking for the nearest ancestor that holds any marker in
`beads-project-root-markers'.  Recognizes plain beads projects and
non-git Gas City workspaces.  Git repositories are intentionally not
probed here — callers that prefer VC detection should try
`beads-git-find-project-root' first and fall back to this."
  (let* ((start (or directory default-directory))
         (root (locate-dominating-file
                start
                (lambda (dir)
                  (seq-some (lambda (marker)
                              (file-exists-p (expand-file-name marker dir)))
                            beads-project-root-markers)))))
    (when root
      (file-name-as-directory (expand-file-name root)))))

(defvar beads--remote-root-cache (make-hash-table :test 'equal)
  "Remote start directory -> project root, or :none.
Root discovery on a remote directory costs one TRAMP round trip per
marker and level; its answers, negative ones included, are remembered
until `beads-forget-project-roots'.")

(defun beads-forget-project-roots ()
  "Forget the remembered project roots of remote directories.
Call after creating a project (bd init) under a directory already
looked up, or after moving one."
  (interactive)
  (clrhash beads--remote-root-cache))

(defun beads--walk-up-for-markers (start markers)
  "Return the nearest directory at or above START holding one of MARKERS.
A pure-name walk (`file-name-directory' of `directory-file-name'):
unlike `locate-dominating-file' it never calls `abbreviate-file-name',
which stats the file system, so the only I/O is one `file-exists-p'
per marker and level."
  (let ((dir (file-name-as-directory start))
        (found nil))
    (while (and dir (not found))
      (when (seq-some (lambda (m) (file-exists-p (concat dir m))) markers)
        (setq found dir))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (and parent (not (equal parent dir)) parent))))
    found))

(defun beads--remote-project-root (dir)
  "Return the project root of remote directory DIR, or nil; remembered.
No VC detection (`project-current' walks and runs git over TRAMP): the
markers of `beads-project-root-markers' plus \".git\", nearest wins.
On an ssh-transport host the walk is one shell command over the ssh
pipe (`beads-remote-ssh-find-up'), no TRAMP at all; elsewhere a
pure-name walk with one `file-exists-p' per marker and level."
  (let ((cached (gethash dir beads--remote-root-cache)))
    (cond ((eq cached :none) nil)
          (cached cached)
          (t (let* ((markers (append beads-project-root-markers '(".git")))
                    (root (if (beads-remote-ssh-pipe-p dir)
                              ;; One command over the ssh pipe: no TRAMP.
                              (beads-remote-ssh-find-up dir markers)
                            (beads--walk-up-for-markers dir markers))))
               (puthash dir (or root :none) beads--remote-root-cache)
               root)))))

(defun beads--project-root ()
  "Return the canonical project root, or nil if not in a project.
Tries VC/git detection first via `beads-git-find-project-root'
\(correct for normal repos and Gas City rigs), then falls back to
`beads--find-project-root' so non-git beads projects and Gas City
workspaces are recognized.

Whichever branch wins, the result is normalized to an absolute path
with a trailing slash, so callers get a canonical directory name
regardless of source.  This makes the resolver a safe drop-in for a
bare `beads-git-find-project-root' call (the previous dashboard helper
normalized the git result itself; that work now lives here).

This is the package-wide resolver: prefer it over calling
`beads-git-find-project-root' directly, except where an operation
genuinely requires git (worktrees, branches, sesman sessions).

On a remote `default-directory' no VC detection runs: the markers of
`beads-project-root-markers' and \".git\" are looked up with a
pure-name walk, and the answer is remembered per directory
\(`beads--remote-project-root', `beads-forget-project-roots')."
  (if (file-remote-p default-directory)
      ;; Remote: a bounded, remembered marker walk (first contact only).
      (beads--remote-project-root (file-name-as-directory default-directory))
    (when-let* ((root (or (ignore-errors (beads-git-find-project-root))
                         (beads--find-project-root))))
      (file-name-as-directory (expand-file-name root)))))

(defun beads-store-resolve (directory)
  "Return DIRECTORY as a store directory name for this Emacs, or nil.
Nil or empty DIRECTORY yields nil.  A host-local absolute DIRECTORY
given while `default-directory' is remote is re-prefixed with that
remote, so a caller passing bd's view of the path (as for --directory)
still scopes the store on the right host.  Pure string operations, no
file I/O."
  (when (and (stringp directory) (not (string-empty-p directory)))
    (file-name-as-directory
     (if (and (not (file-remote-p directory))
              (file-name-absolute-p directory)
              (file-remote-p default-directory))
         (concat (file-remote-p default-directory) directory)
       directory))))

(defun beads-store-project-root (store)
  "Return the project root for the explicit store directory STORE.
A remote STORE is its own root: finding a root walks the VC tree and
markers, synchronous TRAMP I/O at open time (dashboard-v3 §12 B3), and
bd resolves the store from --directory anyway.  A local STORE resolves
as usual from STORE, falling back to STORE itself."
  (if (file-remote-p store)
      store
    (let ((default-directory store))
      (or (beads--project-root) store))))

(defun beads--project-name-for-root (root)
  "Return the project name (basename) for ROOT directory.
ROOT is a directory name (a trailing slash, as produced by
`beads--project-root', is stripped first).  Prefer this when the
root is already in hand so the root is not resolved a second time."
  (file-name-nondirectory (directory-file-name root)))

(defun beads--project-name ()
  "Return the basename of the canonical project root, or nil.
Resolves the root via `beads--project-root' (git first, then the
non-git marker walk), so Gas City and other non-git beads projects get
a real name instead of \"unknown\"."
  (when-let* ((root (beads--project-root)))
    (beads--project-name-for-root root)))

(defun beads--resolve-beads-dir (beads-dir)
  "Resolve BEADS-DIR, following a redirect file if present.
If BEADS-DIR contains a `redirect' file, reads its content as a
path relative to the project root (parent of BEADS-DIR) and
returns the resolved target directory.  If the target does not
exist or there is no redirect file, returns BEADS-DIR unchanged."
  (let ((redirect-file (expand-file-name "redirect" beads-dir)))
    (if (file-readable-p redirect-file)
        (let* ((project-root
                (file-name-directory (directory-file-name beads-dir)))
               (target (string-trim
                        (with-temp-buffer
                          (insert-file-contents redirect-file)
                          (buffer-string))))
               (resolved (expand-file-name target project-root)))
          (if (file-directory-p resolved)
              resolved
            beads-dir))
      beads-dir)))

(defun beads--get-database-path ()
  "Get the database path for bd commands.
Returns nil if auto-discovery should be used.
Follows .beads/redirect files to find the actual database
directory, then looks for a SQLite .db file or a Dolt database
subdirectory."
  (or beads-database-path
      (when-let* ((beads-dir (beads--find-beads-dir)))
        (let* ((resolved-dir (beads--resolve-beads-dir beads-dir)))
          (or
           ;; Legacy: SQLite .db file
           (car (directory-files resolved-dir t "\\.db\\'"))
           ;; Modern: Dolt database directory
           (let ((dolt-dir (expand-file-name "dolt" resolved-dir)))
             (when (file-directory-p dolt-dir)
               dolt-dir)))))))

;;; Process Execution

(defun beads--build-command (subcommand &rest args)
  "Build bd command with SUBCOMMAND and ARGS.
Automatically adds global flags based on customization and
global transient variables (beads-global-*).

Global transient variables (set via beads-option-global options)
take precedence over defcustom settings."
  ;; Use push/nreverse for O(n) performance instead of repeated append (O(n^2))
  (let ((parts nil))
    ;; Build arguments by pushing in desired final order, then reverse at end
    ;; Final: (executable [--actor actor] [--db db] [flags...] subcommand args... --json)

    ;; Push executable first (will be first after nreverse)
    (push beads-executable parts)

    ;; Actor: beads-global-actor > beads-actor > $USER
    (when-let* ((actor (or beads-global-actor beads-actor)))
      ;; Convert to string in case it's a symbol
      (let ((actor-str (if (stringp actor) actor (format "%s" actor))))
        (unless (string-empty-p (string-trim actor-str))
          (push "--actor" parts)
          (push actor-str parts))))

    ;; Database: beads-global-db > beads--get-database-path
    (when-let* ((db (or beads-global-db (beads--get-database-path))))
      ;; Convert to string in case it's a symbol
      (let ((db-str (if (stringp db) db (format "%s" db))))
        (unless (string-empty-p (string-trim db-str))
          ;; Strip Tramp prefix for remote paths so bd can understand the path
          (push "--db" parts)
          (push (file-local-name db-str) parts))))

    ;; Working directory (like git -C)
    (when-let* ((dir beads-global-directory))
      (let ((dir-str (if (stringp dir) dir (format "%s" dir))))
        (unless (string-empty-p (string-trim dir-str))
          (push "--directory" parts)
          (push (file-local-name dir-str) parts))))

    ;; Boolean global flags (only if set via transient)
    (when beads-global-global
      (push "--global" parts))
    (when beads-global-sandbox
      (push "--sandbox" parts))

    ;; Add subcommand
    (push subcommand parts)

    ;; Add command-specific args
    (dolist (arg args)
      (push arg parts))

    ;; --json flag goes at the end
    (push "--json" parts)

    ;; Reverse to get correct order
    (nreverse parts)))

;;; Public API

(defun beads-check-executable ()
  "Check if bd executable is available.
Returns t if found, signals error otherwise.  On a remote store
reached over the ssh pipe (`beads-remote-ssh-pipe-p') no probe runs:
bd is found on the host's PATH when a command runs."
  (interactive)
  (if (or (beads-remote-ssh-pipe-p)       ; found on the host's PATH, no probe
          (executable-find beads-executable))
      (progn
        (when (called-interactively-p 'interactive)
          (message "Found bd executable: %s" beads-executable))
        t)
    (beads--error "Cannot find bd executable '%s'.
Install bd CLI from https://github.com/steveyegge/beads
or set `beads-executable' to the full path" beads-executable)))

;;; Completion Support (aliases for backward compatibility)

;; The completion implementation is in beads-completion.el.
;; These aliases maintain backward compatibility with existing code.

(defalias 'beads--issue-completion-table #'beads-completion-issue-table
  "Return completion table for issue IDs with title-aware matching.")

(defalias 'beads--invalidate-completion-cache #'beads-completion-invalidate-cache
  "Invalidate the completion cache.")

(defalias 'beads--get-cached-issues #'beads-completion--get-cached-issues
  "Get cached issue list, refreshing if stale.")

;;; History Variables

(defvar beads--issue-id-history nil
  "History list for issue ID completion.")

(defvar beads--dependency-type-history nil
  "History list for dependency type completion.")

(defvar beads--worktree-name-history nil
  "History list for worktree name completion.")

(defvar beads--worktree-branch-history nil
  "History list for worktree branch completion.")

(defvar beads--worktree-existing-history nil
  "History list for existing worktree completion.")

;;; JSON Parsing

(defun beads--parse-issue (json)
  "Parse issue from JSON object.
Returns a beads-issue EIEIO instance."
  (let ((issue (if (vectorp json) (aref json 0) json)))
    (beads-from-json 'beads-issue issue)))

(defun beads--parse-issues (json)
  "Parse list of issues from JSON array.
Returns a list of beads-issue EIEIO instances."
  (when (and json (vectorp json))
    (mapcar (lambda (j) (beads-from-json 'beads-issue j)) (append json nil))))

;;; Footer

(provide 'beads-util)
;;; beads-util.el ends here
