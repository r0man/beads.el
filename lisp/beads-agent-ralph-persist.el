;;; beads-agent-ralph-persist.el --- Ralph iteration persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; Persist Ralph iteration records and raw stream events so a kill
;; mid-run is recoverable.  Two artefacts per controller root, both
;; under `.beads/scratch/ralph/' (mirrors the `.beads/scripts/'
;; precedent for tooling-adjacent files; `.beads/' proper is the Dolt-
;; backed workspace and bd's sync tooling treats it as authoritative).
;;
;;   1. Iteration summary log
;;      .beads/scratch/ralph/<root-id>.jsonl
;;      Append-only NDJSON, one iteration record per line.
;;      Written after every iteration AND on status transitions.
;;      Drives `beads-agent-ralph-show-history' and resume.
;;
;;   2. Per-iter event stream
;;      .beads/scratch/ralph/<root-id>.iter-<N>.ndjson
;;      Raw NDJSON received from the claude binary for iteration N.
;;      Compressed in-place to `.iter-<N>.ndjson.gz' once the iter
;;      closes.  Replay via RET on a history row.
;;
;; Retention: `beads-agent-ralph-event-retention' (default 50) prunes
;; the oldest per-iter event files when count exceeds the threshold.
;; The summary log is never auto-pruned -- it's the durable timeline.
;;
;; The capture is hooked into the stream module via a dedicated
;; subscriber registered by the controller at spawn time.  This file
;; deliberately knows nothing about the controller state machine; it
;; reads from a stream + controller pair to write records.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'subr-x)

;; This module is a leaf: the controller (`beads-agent-ralph') and the
;; stream (`beads-agent-ralph-stream') require us, not vice versa.  We
;; only use generic `oref' on caller-supplied objects at runtime, with
;; the slot accesses on the controller/iteration classes wrapped in
;; `with-no-warnings' since the byte-compiler cannot see those classes
;; from this file (same precedent as `beads-command.el').

(declare-function beads-agent-ralph--stream-subscribe
                  "beads-agent-ralph-stream" (stream label callback))
(declare-function beads-agent-ralph--stream-unsubscribe
                  "beads-agent-ralph-stream" (stream label))

;;; Customization

(defgroup beads-agent-ralph-persist nil
  "Persistence for the Ralph iteration loop."
  :group 'beads-agent-ralph
  :prefix "beads-agent-ralph-")

(defcustom beads-agent-ralph-event-retention 50
  "Maximum number of per-iter NDJSON event files retained per root.
When a new iteration's event file is closed, older event files for
the same root are pruned newest-first until the count is below this
threshold.  Summary log (`.jsonl') is never pruned automatically.

Set to nil to disable retention pruning entirely."
  :type '(choice (const :tag "Keep all" nil) integer)
  :group 'beads-agent-ralph-persist)

(defcustom beads-agent-ralph-persist-gzip-program "gzip"
  "Program used to compress per-iter NDJSON files on iteration close.
Compression is best-effort: a failed exit silently leaves the
uncompressed `.ndjson' in place.  Set to nil to disable compression."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'beads-agent-ralph-persist)

;;; Path helpers

(defun beads-agent-ralph-persist-root-dir (project-dir)
  "Return the `.beads/scratch/ralph/' directory under PROJECT-DIR.
Does not create the directory; the writer functions create on first
write so a read-only consumer (history view) can probe its
existence."
  (expand-file-name
   "scratch/ralph"
   (expand-file-name ".beads" (file-name-as-directory project-dir))))

(defun beads-agent-ralph-persist-jsonl-path (project-dir root-id)
  "Return the summary log path for ROOT-ID under PROJECT-DIR."
  (expand-file-name
   (format "%s.jsonl" root-id)
   (beads-agent-ralph-persist-root-dir project-dir)))

(defun beads-agent-ralph-persist-iter-event-path
    (project-dir root-id iter &optional compressed)
  "Return the per-iter NDJSON event file path under PROJECT-DIR.
The path is for ROOT-ID iteration ITER.  When COMPRESSED is
non-nil, returns the gzipped path (.ndjson.gz)."
  (expand-file-name
   (format "%s.iter-%d.ndjson%s"
           root-id iter (if compressed ".gz" ""))
   (beads-agent-ralph-persist-root-dir project-dir)))

(defun beads-agent-ralph-persist--ensure-dir (project-dir)
  "Create the scratch dir under PROJECT-DIR if missing.
Returns the absolute path on success, nil if creation fails."
  (let ((dir (beads-agent-ralph-persist-root-dir project-dir)))
    (condition-case _err
        (progn (make-directory dir t) dir)
      (error nil))))

;;; JSON helpers

(defun beads-agent-ralph-persist--encode (value)
  "Encode VALUE (an alist or plist) to a JSON string.
Uses lexical bindings that round-trip with the parser's keyword
choices so reading back via `json-read-from-string' with the same
bindings produces the original structure (up to keyword/string
duality)."
  (let ((json-encoding-default-indentation "")
        (json-encoding-pretty-print nil)
        (json-encoding-separator ",")
        (json-encoding-object-sort-predicate nil)
        (json-false :json-false))
    (json-encode value)))

(defun beads-agent-ralph-persist--iteration-to-alist (iter root-id)
  "Convert an `beads-agent-ralph--iteration' ITER into an alist for JSON.
ROOT-ID is recorded on each summary record so the JSONL is self-
describing even when copied out of context.  The shape mirrors the
iteration object slots plus a stable `kind' tag."
  ;; `with-no-warnings' here silences spurious \"Unknown slot\" warnings:
  ;; this module deliberately does not require `beads-agent-ralph' (the
  ;; dep direction is the other way), so the byte-compiler cannot see
  ;; the iteration class.  Slots are real -- see the iteration defclass
  ;; in `beads-agent-ralph.el'.  Same precedent as `beads-command.el'.
  (with-no-warnings
    (list
     (cons "kind" "iteration")
     (cons "root_id" (or root-id ""))
     (cons "issue_id" (or (oref iter issue-id) ""))
     (cons "status" (and (oref iter status) (symbol-name (oref iter status))))
     (cons "duration_ms" (or (oref iter duration-ms) 0))
     (cons "cost_usd" (or (oref iter cost-usd) 0))
     (cons "tool_call_count" (oref iter tool-call-count))
     (cons "bd_updates_count" (oref iter bd-updates-count))
     (cons "sentinel_hit" (if (oref iter sentinel-hit) t :json-false))
     (cons "summary" (or (oref iter summary) ""))
     (cons "files_touched" (oref iter files-touched))
     (cons "stderr_tail" (oref iter stderr-tail))
     (cons "verify_result" (oref iter verify-result))
     (cons "timestamp" (format-time-string "%FT%T%z")))))

(defun beads-agent-ralph-persist--status-to-alist (controller status)
  "Convert a CONTROLLER transition to STATUS into an alist for JSON.
Captures terminal-state crumbs (status, done-reason, totals) so a
post-mortem reader can reconstruct the loop's final disposition
without consulting the live controller."
  ;; See sibling helper above: leaf-module slot accesses on the
  ;; controller class require `with-no-warnings'.
  (with-no-warnings
    (list
     (cons "kind" "status")
     (cons "root_id" (or (oref controller root-id) ""))
     (cons "status" (and (oref controller status) (symbol-name status)))
     (cons "done_reason" (and (oref controller done-reason)
                              (symbol-name (oref controller done-reason))))
     (cons "iteration" (oref controller iteration))
     (cons "cumulative_cost_usd" (oref controller cumulative-cost-usd))
     (cons "false_claim_count" (oref controller false-claim-count))
     (cons "consecutive_stalls" (oref controller consecutive-stalls))
     (cons "timestamp" (format-time-string "%FT%T%z")))))

;;; Writers

(defun beads-agent-ralph-persist-append-line (path line)
  "Append LINE plus newline to PATH.
Ensures PATH's parent directory exists.  Best-effort: a failure to
open or write logs to *Messages* and returns nil so the caller
continues without blocking iteration."
  (let ((dir (file-name-directory path)))
    (condition-case err
        (progn
          (unless (file-directory-p dir)
            (make-directory dir t))
          (with-temp-buffer
            (insert line "\n")
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) path t 'quiet)))
          t)
      (error
       (message "beads-agent-ralph-persist: append to %s failed: %S"
                path err)
       nil))))

(defun beads-agent-ralph-persist-record-iteration (project-dir root-id iter)
  "Append an iteration record for ITER (root ROOT-ID) under PROJECT-DIR."
  (when (and project-dir root-id iter)
    (let ((alist (beads-agent-ralph-persist--iteration-to-alist iter root-id))
          (path (beads-agent-ralph-persist-jsonl-path project-dir root-id)))
      (beads-agent-ralph-persist-append-line
       path
       (beads-agent-ralph-persist--encode alist)))))

(defun beads-agent-ralph-persist-record-status (project-dir controller status)
  "Append a status transition record for CONTROLLER to STATUS under PROJECT-DIR."
  (when (and project-dir (oref controller root-id))
    (let ((alist (beads-agent-ralph-persist--status-to-alist
                  controller status))
          (path (beads-agent-ralph-persist-jsonl-path
                 project-dir (oref controller root-id))))
      (beads-agent-ralph-persist-append-line
       path
       (beads-agent-ralph-persist--encode alist)))))

;;; Per-iter event capture
;;
;; The capture subscriber tracks the previously-seen head cell of the
;; stream's `events' list and walks only the new prefix on each flush.
;; `events' is push-style (newest at head) so walking head-first and
;; pushing into a fresh list yields chronological order without ever
;; reversing the whole history.  O(new-events-per-flush) instead of
;; O(total-events).

(defun beads-agent-ralph-persist-make-event-subscriber (project-dir root-id iter)
  "Return a stream subscriber that captures events for one iteration.
PROJECT-DIR, ROOT-ID, ITER are bound into the closure.  Returns a
function suitable for
`beads-agent-ralph--stream-subscribe' (CALLBACK signature: one
argument, the stream).

The subscriber writes new events to the per-iter `.ndjson' file each
flush.  On terminal status it compresses the file (best-effort) and
prunes older event files per `beads-agent-ralph-event-retention'.

A second invocation after compression is a no-op."
  (let ((seen-head nil)
        (compressed nil)
        (path (beads-agent-ralph-persist-iter-event-path
               project-dir root-id iter)))
    (lambda (stream)
      (unless compressed
        (let* ((head (oref stream events))
               (new-chron nil)
               (cur head))
          (while (and cur (not (eq cur seen-head)))
            (push (car cur) new-chron)
            (setq cur (cdr cur)))
          (when new-chron
            (dolist (event new-chron)
              (beads-agent-ralph-persist-append-line
               path
               (beads-agent-ralph-persist--encode
                (beads-agent-ralph-persist--event-to-alist event))))
            (setq seen-head head)))
        (when (memq (oref stream status) '(finished failed stopped))
          (setq compressed t)
          (beads-agent-ralph-persist--maybe-compress path)
          (beads-agent-ralph-persist--prune-event-files
           project-dir root-id))))))

(defun beads-agent-ralph-persist--event-to-alist (event)
  "Coerce EVENT (plist or alist) into a JSON-encodable alist.
The stream parses NDJSON into plists with keyword keys; round-tripping
those back through `json-encode' produces nested arrays unless we
unwrap.  This helper walks the plist and emits an alist of (STRING
. VALUE) pairs."
  (cond
   ((and (listp event) (keywordp (car-safe event)))
    (let (result)
      (while event
        (let ((k (car event))
              (v (cadr event)))
          (push (cons (substring (symbol-name k) 1)
                      (beads-agent-ralph-persist--event-to-alist v))
                result))
        (setq event (cddr event)))
      (nreverse result)))
   ((and (listp event) (or (null event) (not (keywordp (car-safe event)))))
    (mapcar #'beads-agent-ralph-persist--event-to-alist event))
   (t event)))

(defun beads-agent-ralph-persist--maybe-compress (path)
  "Compress PATH in place with `beads-agent-ralph-persist-gzip-program'.
Best-effort: a missing program or non-zero exit leaves PATH alone.
A successful run deletes the original."
  (when (and beads-agent-ralph-persist-gzip-program
             (file-exists-p path))
    (let ((default-directory (file-name-directory path)))
      (condition-case _err
          (call-process
           beads-agent-ralph-persist-gzip-program
           nil nil nil
           "-f" path)
        (error nil)))))

(defun beads-agent-ralph-persist--iter-number-from-name (name)
  "Return the iter integer parsed from event file NAME, or nil.
NAME is a basename like `bde-foo.iter-7.ndjson' or `.iter-3.ndjson.gz'."
  (when (string-match "\\.iter-\\([0-9]+\\)\\.ndjson\\(\\.gz\\)?\\'" name)
    (string-to-number (match-string 1 name))))

(defun beads-agent-ralph-persist--list-event-files (project-dir root-id)
  "Return per-iter event files for ROOT-ID under PROJECT-DIR.
Returns a list of (ITER . FULL-PATH) sorted by ITER ascending.
Both `.ndjson' and `.ndjson.gz' files are included; if both exist
for the same iter, the gzipped one wins."
  (let* ((dir (beads-agent-ralph-persist-root-dir project-dir))
         (prefix (concat root-id ".iter-"))
         (entries (and (file-directory-p dir)
                       (directory-files dir nil
                                        (regexp-quote prefix))))
         table)
    (dolist (name entries)
      (let ((iter (beads-agent-ralph-persist--iter-number-from-name name)))
        (when iter
          (let* ((compressed (string-suffix-p ".gz" name))
                 (existing (assq iter table)))
            (cond
             ((null existing)
              (push (cons iter (cons compressed
                                     (expand-file-name name dir)))
                    table))
             ;; Prefer compressed copy when both exist.
             ((and compressed (not (car (cdr existing))))
              (setcdr existing (cons t (expand-file-name name dir)))))))))
    (sort (mapcar (lambda (cell)
                    (cons (car cell) (cdr (cdr cell))))
                  table)
          (lambda (a b) (< (car a) (car b))))))

(defun beads-agent-ralph-persist--prune-event-files (project-dir root-id)
  "Drop oldest per-iter event files for ROOT-ID under PROJECT-DIR.
Trims any files beyond the retention threshold.  Bounded by
`beads-agent-ralph-event-retention'; nil disables pruning."
  (when (and (numberp beads-agent-ralph-event-retention)
             (> beads-agent-ralph-event-retention 0))
    (let* ((files (beads-agent-ralph-persist--list-event-files
                   project-dir root-id))
           (excess (- (length files) beads-agent-ralph-event-retention)))
      (when (> excess 0)
        (dolist (entry (cl-subseq files 0 excess))
          (let ((path (cdr entry)))
            (when (file-exists-p path)
              (condition-case err
                  (delete-file path)
                (error
                 (message "beads-agent-ralph-persist: prune %s failed: %S"
                          path err))))))))))

;;; Readers

(defun beads-agent-ralph-persist-read-jsonl (project-dir root-id)
  "Read all records for ROOT-ID's summary log under PROJECT-DIR.
Returns a list of alists in chronological order.  Returns nil when
the log does not exist or cannot be parsed (any individual unparsable
line is replaced with a placeholder alist)."
  (let ((path (beads-agent-ralph-persist-jsonl-path project-dir root-id)))
    (when (file-readable-p path)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents path))
        (let (records)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (point) (line-end-position))))
              (unless (string-empty-p (string-trim line))
                (push
                 (condition-case _err
                     (let ((json-object-type 'alist)
                           (json-array-type 'list)
                           (json-key-type 'string)
                           (json-false nil)
                           (json-null nil))
                       (json-read-from-string line))
                   (error (list (cons "kind" "malformed") (cons "raw" line))))
                 records))
              (forward-line 1)))
          (nreverse records))))))

(defun beads-agent-ralph-persist-iter-events-path (project-dir root-id iter)
  "Return readable event-file path under PROJECT-DIR for ROOT-ID iter ITER.
Prefers the gzipped path if it exists, falling back to the raw
`.ndjson'.  Returns nil when neither is on disk."
  (let ((compressed (beads-agent-ralph-persist-iter-event-path
                     project-dir root-id iter t))
        (plain (beads-agent-ralph-persist-iter-event-path
                project-dir root-id iter nil)))
    (cond
     ((file-readable-p compressed) compressed)
     ((file-readable-p plain) plain)
     (t nil))))

(defun beads-agent-ralph-persist-read-iter-events (project-dir root-id iter)
  "Read all events under PROJECT-DIR captured for ROOT-ID iteration ITER.
Transparently decompresses `.ndjson.gz' via
`auto-compression-mode' (Emacs's `jka-compr' handles this when the
mode is enabled, which is the default).  Returns a list of alists
in receive order; an unparsable line is preserved as
`((\"kind\" . \"malformed\") (\"raw\" . LINE))'."
  (let ((path (beads-agent-ralph-persist-iter-events-path
               project-dir root-id iter)))
    (when path
      (with-temp-buffer
        (insert-file-contents path)
        (let (records)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (point) (line-end-position))))
              (unless (string-empty-p (string-trim line))
                (push
                 (condition-case _err
                     (let ((json-object-type 'alist)
                           (json-array-type 'list)
                           (json-key-type 'string)
                           (json-false nil)
                           (json-null nil))
                       (json-read-from-string line))
                   (error (list (cons "kind" "malformed") (cons "raw" line))))
                 records))
              (forward-line 1)))
          (nreverse records))))))

;;; Resume detection

(defun beads-agent-ralph-persist-resume-summary (project-dir root-id)
  "Return a plist summarising prior state for ROOT-ID under PROJECT-DIR.

Plist keys:
  :iterations        — count of iteration records on disk.
  :last-iteration    — most recent iteration index, or nil.
  :cumulative-cost   — sum of cost_usd across iteration records.
  :last-timestamp    — ISO-8601 string of the most recent record.
  :path              — path to the summary log.

Returns nil when no summary log exists or it is empty."
  (let* ((path (beads-agent-ralph-persist-jsonl-path project-dir root-id))
         (records (beads-agent-ralph-persist-read-jsonl project-dir root-id))
         (iters (cl-remove-if-not
                 (lambda (r) (equal (cdr (assoc "kind" r)) "iteration"))
                 records)))
    (when iters
      (let ((cost 0.0)
            (last-time nil))
        (dolist (rec iters)
          (let ((c (cdr (assoc "cost_usd" rec)))
                (t- (cdr (assoc "timestamp" rec))))
            (when (numberp c) (cl-incf cost c))
            (when (stringp t-) (setq last-time t-))))
        (list :iterations (length iters)
              :last-iteration (length iters)
              :cumulative-cost cost
              :last-timestamp last-time
              :path path)))))

(defun beads-agent-ralph-persist-archive-jsonl (project-dir root-id)
  "Rename the existing JSONL log under PROJECT-DIR for ROOT-ID with a timestamp.
Returns the archive path on success, nil if there was no log to
archive or the rename failed.  The archive name follows the pattern
`<root-id>.<timestamp>.jsonl' under the same scratch directory."
  (let* ((src (beads-agent-ralph-persist-jsonl-path project-dir root-id))
         (ts (format-time-string "%Y%m%dT%H%M%S"))
         (dst (expand-file-name
               (format "%s.%s.jsonl" root-id ts)
               (beads-agent-ralph-persist-root-dir project-dir))))
    (when (file-exists-p src)
      (condition-case _err
          (progn (rename-file src dst t) dst)
        (error nil)))))

(defun beads-agent-ralph-persist-resume-options (worktree-dirty)
  "Return the list of resume option chars for the prompt.
WORKTREE-DIRTY non-nil includes the [s]tash-and-resume option per
spec.  Always includes [r]esume, [f]resh, [F]ull-reset, [c]ancel."
  (if worktree-dirty
      (list ?r ?s ?f ?F ?c)
    (list ?r ?f ?F ?c)))

(defun beads-agent-ralph-persist-resume-prompt-text (summary worktree-dirty)
  "Return the two-line legend text for the resume `read-char' prompt.
SUMMARY is the plist from `beads-agent-ralph-persist-resume-summary'.
WORKTREE-DIRTY adds the dirty-tree header line and [s]tash option."
  (let* ((iters (or (plist-get summary :iterations) 0))
         (cost (or (plist-get summary :cumulative-cost) 0))
         (ts (or (plist-get summary :last-timestamp) "?"))
         (header (if worktree-dirty
                     (format "Worktree DIRTY: see git status\n")
                   ""))
         (state (format "Prior state: %d iters · $%.4f cumulative · last %s\n"
                        iters cost ts))
         (legend (if worktree-dirty
                     "[r]esume  [s]tash-and-resume  [f]resh  [F]ull-reset  [c]ancel"
                   "[r]esume  [f]resh  [F]ull-reset  [c]ancel")))
    (concat header state legend)))

(defun beads-agent-ralph-persist--worktree-dirty-p (project-dir)
  "Return non-nil when PROJECT-DIR's git tree is dirty.
Runs `git status --porcelain'; an empty result is clean.  Errors are
treated as clean so a non-git directory doesn't trigger the dirty
path."
  (when (and project-dir (file-directory-p project-dir))
    (let* ((default-directory (file-name-as-directory project-dir))
             (out
              (condition-case _err
                  (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "git" nil (current-buffer) nil
                                    "status" "--porcelain")))
                (error nil))))
        (and (stringp out) (not (string-empty-p (string-trim out)))))))

(defun beads-agent-ralph-persist-stash-name (root-id iter)
  "Return the git-stash message for a stash-and-resume on ROOT-ID at ITER."
  (format "ralph-resume %s @iter%d %s"
          (or root-id "?") (or iter 0)
          (format-time-string "%FT%T%z")))

(defun beads-agent-ralph-persist-stash-worktree (project-dir root-id iter)
  "Run `git stash push' in PROJECT-DIR with a Ralph message for ROOT-ID/ITER.
Returns the stash output on success, nil on failure.  The caller
displays the result in the dashboard banner."
  (when (file-directory-p project-dir)
    (let ((default-directory (file-name-as-directory project-dir))
          (msg (beads-agent-ralph-persist-stash-name root-id iter)))
      (condition-case _err
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "git" nil (current-buffer) nil
                            "stash" "push" "-u" "-m" msg)))
        (error nil)))))

;;; Full-reset destructive support

(defun beads-agent-ralph-persist-record-paths (project-dir root-id)
  "Return all on-disk paths owned by PROJECT-DIR/ROOT-ID.
Includes the summary log and every per-iter event file (compressed
or raw).  Used by `Full-reset' to enumerate before deletion."
  (let ((paths nil)
        (jsonl (beads-agent-ralph-persist-jsonl-path project-dir root-id)))
    (when (file-exists-p jsonl) (push jsonl paths))
    (dolist (entry (beads-agent-ralph-persist--list-event-files
                    project-dir root-id))
      (let ((p (cdr entry)))
        (when (file-exists-p p) (push p paths))))
    paths))

(provide 'beads-agent-ralph-persist)

;;; beads-agent-ralph-persist.el ends here
