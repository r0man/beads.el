;;; beads-live.el --- Live bd events stream supervisor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of beads.el.

;;; Commentary:

;; The stream supervisor behind the `beads-live' feature set (design of
;; record: `plans/beads-events-live/design.md' §3.1).  One supervised
;; `bd events tail --follow' process per canonical store feeds the
;; in-memory model that the dashboard, list, show and events views
;; render from, instead of every view polling `bd' on a timer.
;;
;; Wave 1 owns this module: capability/keying (WI-LIVE-03), spawn and
;; transport (WI-LIVE-04), parse and deliver (WI-LIVE-05), checkpoint,
;; start/resume and re-baseline (WI-LIVE-06), classification and backoff
;; (WI-LIVE-07), poll fallback (WI-LIVE-08), and the attach/subscribe/
;; status/invalidation surface (WI-LIVE-09, this slice).
;;
;; Capability.  The journal can be switched off per workspace, and
;; `bd events tail --follow' against a journal-off store still exits 0
;; and stays silent (experiments.md §3.5).  Readiness must therefore
;; never be inferred from stream liveness: the one authoritative probe
;; is `bd config get events-journal' (`true'/`false', exit 0), read
;; once per store and cached here.  A `bd' too old to know the key is
;; treated the same as the journal being off — poll mode, no error
;; (design.md §4.1, AC-2).
;;
;; Keying.  Streams are keyed by (canonical store root, journal kind),
;; so that every spelling of one store collapses to one key and a
;; gascity `gc events' stream for the same directory is a different key
;; from a beads `bd events' stream (design.md §4.1, §8, HC-3).
;;
;; Transport.  Local stores use a local `make-process' pipe; a
;; single-hop ssh-family host a LOCAL `ssh -T' pipe
;; (`beads-remote-ssh-command'), never a `tramp-sh' `make-process'
;; whose pty-backed multiplexer can deadlock the shared ssh master.
;; Any other remote method is poll-only.  All process creation goes
;; through `beads-live--spawn', the scripted-emitter test seam.
;;
;; Parse and deliver.  `beads-live-parse-chunk' splits the stream's
;; stdout into complete JSONL lines, buffers a trailing partial, skips
;; lines that are not JSON objects (ssh banners, stray text) and never
;; signals — unlike the whole-text
;; `beads-events-records-from-json-lines', which signals on the first
;; non-blank non-JSON line (plan-review advisory A2).  The journal's
;; `issue' object is a sparse `omitempty' partial: present fields in
;; `beads-live--issue-subset' are authoritative (an absent optional
;; field means its zero value), fields outside it keep their baseline
;; value.  Each applied record runs the raw subscribers and the public
;; `beads-event-hooks', and queues its op for one debounced batch.
;;
;; Checkpoint.  The journal is per-branch and per-replica, so a
;; checkpoint is keyed by `(root, branch, replica)' and discarded on a
;; mismatch.  A pruned `--since', a branch/replica change, or the
;; periodic reconcile re-baselines rather than stalling.
;;
;; WI-LIVE-09 (this slice) adds the view-facing surface:
;; `beads-live-attach'/`-detach' (refcount; the last detach stops the
;; stream and kills its stderr buffer), `beads-live-subscribe'/
;; `-unsubscribe' (raw per-record subscription), the public
;; `beads-event-hooks' per-record hook (advisory A1),
;; `beads-live-invalidate-functions' (ROOT KINDS OPS) and the public
;; `beads-live-invalidate' seam, `beads-live-status'/
;; `beads-live-header-string' (pure, redisplay-safe), `beads-live-active-p',
;; and the `beads-live-toggle' (`W'), `beads-live-reconnect' (`g') and
;; `beads-live-stop-all' controls.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'beads-custom)
(require 'beads-util)
(require 'beads-command)
(require 'beads-command-config)
(require 'beads-command-events)
(require 'beads-remote)
(require 'beads-types)

(declare-function tramp-dissect-file-name "tramp"
                  (name &optional default-user default-host))
(declare-function tramp-file-name-localname "tramp" (vec))
(declare-function tramp-file-name-hop "tramp" (vec))
(declare-function beads-command-list "beads-command-list" (&rest args))

;; `beads-store-directory' is a buffer-local `defvar-local' in
;; `beads-meta.el'; declared so a view buffer's pinned store is read
;; without requiring the whole UI.
(defvar beads-store-directory)

;; The options are owned by WI-LIVE-02 (`beads-custom'); repeat the
;; documented defaults so this module byte-compiles and unit-tests
;; before that item lands.  When `beads-custom' already defines them,
;; these `defvar' forms are no-ops and never override the defcustom.
(defvar beads-live-debounce 0.15
  "Seconds to coalesce a burst of journal records before invalidating.")
(defvar beads-live-backoff '(2 5 15 60)
  "Repeating reconnect delay schedule, in seconds.")
(defvar beads-live-stable-after 15
  "Seconds a stream must stay up before its reconnect attempt resets.")
(defvar beads-live-poll-interval 30
  "Seconds between fallback journal polls and the relaxed reconcile.")
(defvar beads-live-reconcile 600
  "Seconds between periodic full reconciles while a live stream is active.")
(defvar beads-live-ring-size 500
  "Maximum number of journal records kept in one store's ring buffer.")
(defvar beads-live-poll-limit 1000
  "Maximum records one fallback poll reads.")

;;; Journal identity

(defconst beads-live-journal-kind-bd "bd-events"
  "Journal-kind component of a beads stream key.
Every beads `bd events' stream is keyed under this string; a different
journal watching the same directory (for example gascity's
`gc events') uses a different kind and so never shares its stream.")

(defconst beads-live-journal-kind-gc "gc-events"
  "Journal-kind component of a gascity `gc events' stream key.
Declared here so the beads and gascity journals have distinct keys by
construction, even when both watch one directory.")

(defconst beads-live-events-journal-key "events-journal"
  "The `bd config' key whose value enables the events journal.
Read once per store by `beads-live-events-journal-enabled-p'; the
journal is on exactly when the value is the literal `true'.")

;;; Canonical store root and stream key

(defun beads-live-canonical-root (dir)
  "Return the canonical spelling of store directory DIR, or nil.
The canonical root is the store's identity for the live stream table
and its cache, so every spelling of one store must produce the same
value:

- a local DIR is expanded and given a trailing slash;
- a remote DIR becomes its dissected TRAMP prefix plus localname, so
  \"/ssh::/p\" and \"/ssh:HOST:/p\" (TRAMP's default host filled in by
  `tramp-dissect-file-name') are one directory — the same keying
  `beads-remote-prefix' and `beads-buffer' use, so a store opened
  either way cannot fork a second stream.

Pure: name dissection only, no `file-remote-p' (whose expansion can
ask the host for its home directory) and no other I/O."
  (when (and (stringp dir) (not (string-empty-p dir)))
    (let ((dir (file-name-as-directory dir)))
      (if-let* ((prefix (beads-remote-prefix dir))
                (vec (ignore-errors (tramp-dissect-file-name dir))))
          (concat prefix
                  (file-name-as-directory (tramp-file-name-localname vec)))
        (expand-file-name dir)))))

(defun beads-live-stream-key (dir &optional kind)
  "Return the canonical live-stream key for store directory DIR.
KIND is the journal kind, defaulting to `beads-live-journal-kind-bd'.
The key is a string of the canonical root and KIND, so equal keys mean
the same journal of the same store and a key is stable across every
spelling of that store.  A nil DIR yields nil.

Because the kind is part of the key, a beads `bd events' stream and a
gascity `gc events' stream over one directory are never conflated; and
because the root is canonical, re-opening a store as
\"/ssh:host:/p\" after \"/ssh:host:/p/\" attaches to the same stream."
  (when-let* ((root (beads-live-canonical-root dir)))
    (concat root "\0" (or kind beads-live-journal-kind-bd))))

(defun beads-live-key (dir &optional kind)
  "Return `beads-live-stream-key' for DIR and KIND.
The short, gascity-mirroring name for the same canonical table key
\(mirrors `gascity-live-key')."
  (beads-live-stream-key dir kind))

;;; Capability probe (cached per store)

(defconst beads-live--capability-unprobed 'beads-live--unprobed
  "Sentinel marking a store whose events-journal capability is unknown.
Distinguishes \"not probed yet\" from a probed store whose journal is
off (cached nil).")

(defvar beads-live--capability (make-hash-table :test 'equal)
  "Canonical store root → events-journal capability.
A value of t means the journal is enabled (`stream' mode); nil means
the journal is off or the `bd' in that store is too old to know the
key (`poll' mode).  Roots never probed are absent.")

(defun beads-live--events-journal-value (result)
  "Extract the events-journal value string from command RESULT.
RESULT is what `beads-command-execute' returns for a
`beads-command-config-get': normally an alist carrying a `value'
entry, but a bare string or boolean is accepted for robustness.
Returns nil when RESULT carries no usable value."
  (cond
   ((null result) nil)
   ((stringp result) result)
   ((eq result t) "true")
   ((and (listp result) (assq 'value result))
    (let ((value (cdr (assq 'value result))))
      (cond ((stringp value) value)
            ((eq value t) "true")
            (t nil))))
   (t nil)))

(defun beads-live--journal-value-enabled-p (value)
  "Return non-nil when config VALUE means the events journal is enabled.
`bd' writes the boolean as the literal string \"true\"/\"false\";
accept the usual truthy spellings so a representation change does not
silently disable live mode."
  (and (stringp value)
       (member (downcase (string-trim value)) '("true" "1" "yes" "on"))))

(defun beads-live--probe-events-journal (store)
  "Run `bd config get events-journal' for canonical STORE.
Return the raw configuration value string (for example \"true\" or
\"false\"), or nil when the probe fails or the `bd' does not know the
key.  This function signals nothing: an unsupported or failing `bd'
is poll mode, not an error (design.md §4.1, AC-2)."
  (let* ((default-directory (file-name-as-directory store))
         (beads-store-directory store)
         (command (beads-command-config-get
                   :key beads-live-events-journal-key
                   :directory (directory-file-name store))))
    (condition-case err
        (beads-live--events-journal-value (beads-command-execute command))
      (error
       (beads--log 'info "beads-live: events-journal probe failed for %s: %s"
                   store (error-message-string err))
       nil))))

(defun beads-live-events-journal-enabled-p (dir &optional refresh)
  "Return non-nil when DIR's store has the events journal enabled.
DIR is canonicalized and probed at most once per store; REFRESH forces
a fresh read (and refreshes the cache).  A store whose `bd' does not
know `events-journal' is treated as disabled, never as an error."
  (let ((root (beads-live-canonical-root dir)))
    (when root
      (let ((cached (gethash root beads-live--capability
                             beads-live--capability-unprobed)))
        (if (and (not refresh)
                 (not (eq cached beads-live--capability-unprobed)))
            cached
          (let ((enabled (beads-live--journal-value-enabled-p
                          (beads-live--probe-events-journal root))))
            (puthash root enabled beads-live--capability)
            enabled))))))

(defun beads-live-capability (dir &optional refresh)
  "Return DIR's live mode: `stream' when the journal is on, else `poll'.
REFRESH forces a fresh capability read.  This is the single decision a
caller makes before spawning: `stream' means start the journal
follower; `poll' means no stream is started and the existing timer
refresh is left untouched (design.md §4.1)."
  (if (beads-live-events-journal-enabled-p dir refresh) 'stream 'poll))

(defun beads-live-forget-capability (&optional dir)
  "Forget the cached events-journal capability for DIR.
With DIR nil, clear the whole cache (used when the environment or the
`bd' binary changes)."
  (if dir
      (when-let* ((root (beads-live-canonical-root dir)))
        (remhash root beads-live--capability))
    (clrhash beads-live--capability)))

;;; Hooks, view locals and stream state

(defvar beads-live-state-functions nil
  "Abnormal hook run when a store's stream changes state.
Called with (ROOT STATE REASON).  Views redraw their header here.")

(defvar beads-live-invalidate-functions nil
  "Abnormal hook run once per debounced journal batch.
Called with (ROOT KINDS OPS): ROOT the store root, KINDS the view kinds
the batch touched, OPS the journal ops in the batch.  Views and the
command-layer cache attach here (design.md §3.1).")

(defvar beads-event-hooks nil
  "Abnormal hook run once per applied journal record.
Called with (RECORD ROOT).  This is the public per-record seam named by
US-4 and AC-7 (plan-review advisory A1); `beads-live-subscribe' is the
buffer-scoped subscription over the same records.  A hook error never
breaks the stream.")

(defvar-local beads-live--refresh nil
  "Function refreshing this view on an invalidation, or nil.")

(defvar-local beads-live--kinds nil
  "View kinds this view depends on; nil means every kind.")

(defvar-local beads-live--root nil
  "Canonical store root this view is attached to.")

(defun beads-live--run-hook (hook &rest args)
  "Run abnormal HOOK with ARGS, isolating any hook error.
A misbehaving hook must never break the stream."
  (condition-case err
      (apply #'run-hook-with-args hook args)
    (error (message "beads-live: hook %s error: %s"
                    hook (error-message-string err)))))

(cl-defstruct (beads-live--stream (:constructor beads-live--stream-create)
                                  (:copier nil))
  "One store's live `bd events' stream.
Slots mirror design.md §3.1.  ACTIVITY is the bounded list of recent
delivery times behind the status rate, kept in memory only."
  root
  host
  name
  process
  (partial "")
  seq
  (state 'off)
  reason
  (attempt 0)
  retry-timer
  retry-at
  (enabled t)
  views
  subscribers
  debounce-timer
  pending
  stderr-buffer
  (mode 'stream)
  poll-timer
  poll-busy
  started
  stopping
  resume
  confirm-timer
  branch
  replica
  checkpoint
  baseline
  (activity nil))

(defun beads-live--set-state (stream state &optional reason)
  "Record STREAM's STATE and REASON, then run the state hooks.
Runs `beads-live-state-functions' with (ROOT STATE REASON) and forces a
mode-line update in every live attached view.  The state is only
propagated when it actually changes."
  (unless (and (eq state (beads-live--stream-state stream))
               (equal reason (beads-live--stream-reason stream)))
    (setf (beads-live--stream-state stream) state
          (beads-live--stream-reason stream) reason)
    (beads-live--run-hook 'beads-live-state-functions
                          (beads-live--stream-root stream) state reason)
    (dolist (buf (beads-live--stream-views stream))
      (when (buffer-live-p buf)
        (with-current-buffer buf (force-mode-line-update))))))

;;; Stream registry

(defvar beads-live--streams (make-hash-table :test 'equal)
  "Canonical stream key → `beads-live--stream'.
One entry per `(canonical root, journal kind)' so every view of a store
shares one follower (design.md §4.1).")

(defun beads-live--store-root (dir)
  "Return the canonical store root for DIR.
Uses `beads-live-canonical-root' so every spelling of one store maps to
one stream; otherwise a plain expansion."
  (cond
   ((null dir) nil)
   ((fboundp 'beads-live-canonical-root)
    (beads-live-canonical-root dir))
   ((string-empty-p dir) nil)
   (t (file-name-as-directory (expand-file-name dir)))))

(defun beads-live--stream-key (root)
  "Return the stream-table key for canonical ROOT."
  (if (fboundp 'beads-live-stream-key)
      (beads-live-stream-key root)
    root))

(defun beads-live--store-display-name (root)
  "Return a short display name for ROOT."
  (or (ignore-errors
        (file-name-nondirectory (directory-file-name (file-local-name root))))
      "store"))

(defun beads-live--stream-for (root)
  "Return the stream for ROOT, creating it on first use."
  (let ((key (beads-live--stream-key root)))
    (or (gethash key beads-live--streams)
        (puthash key
                 (beads-live--stream-create
                  :root root
                  :host (file-remote-p root 'host)
                  :name (beads-live--store-display-name root))
                 beads-live--streams))))

(defun beads-live--forget-stream (root)
  "Remove ROOT's stream from the registry (used by detach/tests)."
  (when root
    (remhash (beads-live--stream-key root) beads-live--streams)))

(defun beads-live--default-root ()
  "Return the store directory the current buffer belongs to.
Prefers the buffer-local `beads-store-directory' the views pin, then
`default-directory', resolved with `beads-store-resolve'."
  (or (and (bound-and-true-p beads-store-directory)
           (stringp beads-store-directory)
           (not (string-empty-p beads-store-directory))
           (beads-store-resolve beads-store-directory))
      (beads-store-resolve default-directory)
      default-directory))

(defun beads-live--find (&optional dir)
  "Return the stream covering DIR (default the buffer's store), or nil.
Pure: dissects names and reads the stream table only, so it is safe at
redisplay and does no file-name-handler I/O."
  (let* ((root (beads-live--store-root (or dir (beads-live--default-root))))
         (key (and root (beads-live--stream-key root))))
    (and key (gethash key beads-live--streams))))

;;; Transport selection

(defun beads-live--ssh-p (root)
  "Return non-nil when ROOT's host is reachable with a plain `ssh -T'.
ROOT is a canonical store directory.  True only for a remote,
single-hop, ssh-family TRAMP name (see `beads-remote-ssh-methods').
This deliberately ignores `beads-remote-transport': the stream always
uses the local ssh pipe for an ssh-family host, never a `tramp-sh'
`make-process', whose pty multiplexer can deadlock the shared ssh
master.  Any other remote method is poll-only.  Pure: parses the name
only, no TRAMP connection."
  (and (stringp root)
       (file-remote-p root)
       (member (file-remote-p root 'method) beads-remote-ssh-methods)
       (not (ignore-errors
              (tramp-file-name-hop (tramp-dissect-file-name root))))))

;;; argv

(defun beads-live--args (stream)
  "Return the bd argv tokens (after the program) for STREAM.
The tokens are:

  (\"events\" \"tail\" \"--follow\" [\"--since\" SEQ] [\"--directory\" DIR])

`--since' is emitted only when the stream knows a SEQ (a gap-free
resume; `bd events --since' is strictly greater-than).  `--json' is
never emitted: `tail' prints one JSON object per line either way, and
without `--json' the pruned-checkpoint truncation reason stays on
stderr (`beads-live--classify') instead of becoming a multi-line JSON
object on stdout.  DIR is the store scope, handed over host-local
exactly as `beads-meta-build-global-options' serializes the
`--directory' global."
  (let ((root (beads-live--stream-root stream)))
    (append (list "events" "tail" "--follow")
            (when-let* ((seq (beads-live--stream-seq stream)))
              (list "--since" (number-to-string seq)))
            (when (and root (not (string-empty-p root)))
              (list "--directory" (file-local-name root))))))

(defun beads-live-command (stream)
  "Return the local argv that runs STREAM's `bd events tail --follow'.
Locally the program is `beads-remote-find-executable' of
`beads-executable' (a no-op for a local store, a host-local path for a
remote one).  On a single-hop ssh-family host it is a LOCAL no-pty
`ssh -T' pipe (`beads-remote-ssh-command'): the remote command `cd's to
the store and prepends the pure PATH fragment, and no TRAMP round trip
happens, so (re)starting a stream never blocks."
  (let* ((root (beads-live--stream-root stream))
         (argv (cons beads-executable (beads-live--args stream))))
    (if (beads-live--ssh-p root)
        (beads-remote-ssh-command root argv :cd t)
      (cons (beads-remote-find-executable beads-executable root)
            (beads-live--args stream)))))

;;; stderr buffer

(defun beads-live--stderr-buffer (stream)
  "Return STREAM's dedicated `*beads-live: STORE*' stderr buffer.
Created on demand with a local `default-directory' so the reliable
stderr pipe of a remote stream never performs file I/O through TRAMP
from a process sentinel."
  (let ((buf (beads-live--stream-stderr-buffer stream)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create
                 (format "*beads-live: %s*"
                         (or (beads-live--stream-name stream) "store"))))
      (with-current-buffer buf
        (setq-local default-directory temporary-file-directory))
      (setf (beads-live--stream-stderr-buffer stream) buf))
    buf))

(defun beads-live--stderr-lines (stream start)
  "Return the non-empty stderr lines STREAM wrote after position START.
START is a buffer position captured before the process was spawned."
  (let ((buf (beads-live--stream-stderr-buffer stream)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (split-string (buffer-substring-no-properties
                       (min start (point-max)) (point-max))
                      "\n" t "[ \t\r]+")))))

;;; Spawn

(defun beads-live--spawn (stream)
  "Spawn STREAM's `bd events tail --follow' as a local pipe process.
A local store runs `bd' directly (`beads-live-command'); a single-hop
ssh-family store runs a local `ssh -T' pipe.  Stdout is parsed by
`beads-live--filter'; stderr goes to STREAM's dedicated
`*beads-live: STORE*' buffer, whose last line becomes the header reason
via `beads-live--exited'.  All process creation of the live feature
goes through this function, so tests replace it (or `make-process')
with a scripted emitter.  Returns the process, or nil when the spawn
failed (the failure is recorded on STREAM and a backoff retry is
scheduled)."
  (let* ((root (beads-live--stream-root stream))
         (stderr (beads-live--stderr-buffer stream))
         (mark (with-current-buffer stderr (point-max)))
         ;; Our own stderr pipe: the default pipe's sentinel would
         ;; append "Process … stderr finished" to the buffer, which
         ;; would then read as the stream's last stderr line.
         (stderr-pipe (make-pipe-process
                       :name (format "beads-live %s stderr"
                                     (or (beads-live--stream-name stream)
                                         "store"))
                       :buffer stderr
                       :noquery t
                       :sentinel #'ignore))
         ;; A local ssh process needs a local working directory.
         (default-directory (if (and (stringp root) (file-remote-p root))
                                temporary-file-directory
                              (or root default-directory)))
         proc)
    (setf (beads-live--stream-mode stream) 'stream
          (beads-live--stream-partial stream) ""
          (beads-live--stream-started stream) (float-time))
    (condition-case err
        (setq proc
              (make-process
               :name (format "beads-live %s"
                             (or (beads-live--stream-name stream) "store"))
               :command (beads-live-command stream)
               :connection-type 'pipe
               :coding 'utf-8-unix
               :noquery t
               :stderr stderr-pipe
               :file-handler nil
               :filter (lambda (proc chunk)
                         (ignore proc)
                         (beads-live--filter stream chunk))
               :sentinel (lambda (proc event)
                           (ignore event)
                           (unless (process-live-p proc)
                             ;; Collect the last stderr bytes first.
                             (when (process-live-p stderr-pipe)
                               (accept-process-output stderr-pipe 0.05 nil t))
                             (delete-process stderr-pipe)
                             (beads-live--exited stream proc mark)))))
      (error
       (setq proc nil)
       (delete-process stderr-pipe)
       (with-current-buffer stderr
         (goto-char (point-max))
         (insert (error-message-string err) "\n"))
       (beads-live--set-state stream 'reconnecting (error-message-string err))
       (beads-live--schedule-retry stream)))
    (when proc
      (setf (beads-live--stream-process stream) proc)
      (beads-live--set-state stream 'connecting nil))
    proc))

;;; In-memory model (WI-LIVE-05)

(defvar beads-live--models (make-hash-table :test 'equal)
  "Canonical store root → `beads-live--model'.")

(defvar beads-live--raw-issues (make-hash-table :test #'eq :weakness 'key)
  "Journal record → raw wire `issue' alist, or nil for a delete.
Populated by `beads-live-parse-chunk' so the model can merge, and views
can read, wire fields the `beads-issue' class does not carry (notably
`is_blocked' and `created_by').  Weak keys: an entry disappears with
its record once the bounded ring and the logs stop referencing it.")

(defconst beads-live--raw-missing 'beads-live--raw-missing
  "Sentinel for a record that was not parsed by `beads-live-parse-chunk'.")

(cl-defstruct (beads-live--model (:constructor beads-live--model-create)
                                 (:copier nil))
  "The in-memory journal state of one store.
ISSUES maps an issue id to its merged wire alist (or `:deleted' for a
tombstone); LOGS maps an issue id to its records newest-first; RING is
the bounded newest-first record ring used by rewind and the timeline."
  (issues (make-hash-table :test 'equal))
  (logs (make-hash-table :test 'equal))
  (ring nil)
  (ring-size beads-live-ring-size))

(defun beads-live-model (stream)
  "Return STREAM's in-memory model, creating it on first use."
  (let ((root (beads-live--stream-root stream)))
    (or (gethash root beads-live--models)
        (puthash root (beads-live--model-create) beads-live--models))))

(defun beads-live-forget-model (stream)
  "Drop STREAM's in-memory model."
  (when-let* ((root (beads-live--stream-root stream)))
    (remhash root beads-live--models)))

(defconst beads-live--issue-subset
  '(id title status priority issue_type owner created_by created_at
    updated_at is_blocked assignee labels started_at lease_expires_at
    heartbeat_at closed_at close_reason)
  "Wire fields a journal `issue' snapshot carries authoritatively.
The snapshot is the full post-mutation issue marshalled with
`omitempty': a key absent from it means the field's zero value (an
absent `is_blocked' clears a block, an absent `labels' clears labels).
Fields outside this subset — `description', dependencies, comments,
`*_count', `revision' — are never on the wire, so their baseline value
is preserved.")

(defun beads-live--beads-issue->snapshot (issue)
  "Return a wire-shaped alist for beads-issue ISSUE, or nil.
Used when a record did not come through `beads-live-parse-chunk' (the
poll path, or the baseline seed): the class cannot carry `is_blocked'
or `created_by', so those stay absent and are read as their zero value."
  (when issue
    (cl-loop for (key . slot) in
             '((id . id) (title . title) (status . status)
               (priority . priority) (issue_type . issue-type)
               (owner . owner) (assignee . assignee) (labels . labels)
               (created_at . created-at) (updated_at . updated-at)
               (started_at . started-at) (closed_at . closed-at)
               (close_reason . close-reason)
               (lease_expires_at . lease-expires-at)
               (heartbeat_at . heartbeat-at))
             collect (cons key (oref issue slot)))))

(defun beads-live--record-snapshot (record)
  "Return RECORD's raw wire issue snapshot.
The value is a raw alist, or nil for a `delete' tombstone.  Records not
produced by `beads-live-parse-chunk' fall back to the parsed
`beads-issue' object."
  (let ((entry (gethash record beads-live--raw-issues
                        beads-live--raw-missing)))
    (if (eq entry beads-live--raw-missing)
        (beads-live--beads-issue->snapshot (oref record issue))
      entry)))

(defun beads-live--merge-issue (model id snapshot)
  "Merge journal SNAPSHOT for ID into MODEL's issues hash.
SNAPSHOT is a raw wire alist; nil is a `delete' tombstone.  Every field
in `beads-live--issue-subset' is authoritative — present keys are taken
from SNAPSHOT and absent optional keys are set to their zero value —
while every field outside the subset keeps its baseline value."
  (when id
    (if (null snapshot)
        (puthash id :deleted (beads-live--model-issues model))
      (let* ((existing (gethash id (beads-live--model-issues model)))
             (base (if (and existing (consp existing)) existing nil))
             (merged (cl-remove-if
                      (lambda (pair)
                        (memq (car pair) beads-live--issue-subset))
                      (copy-sequence base))))
        (dolist (key beads-live--issue-subset)
          (let ((pair (assq key snapshot)))
            (when pair
              (push (cons key (cdr pair)) merged))))
        (puthash id (nreverse merged) (beads-live--model-issues model))))))

(defun beads-live--model-seed (stream issues)
  "Seed STREAM's model issue table from baseline ISSUES.
ISSUES is the `beads-issue' list the baseline loader returned.  The
journal deltas merge over these baselines, so fields the wire object
never carries keep their value."
  (let ((model (beads-live-model stream)))
    (dolist (issue issues)
      (when (beads-issue-p issue)
        (when-let* ((id (oref issue id)))
          (puthash id (beads-live--beads-issue->snapshot issue)
                   (beads-live--model-issues model)))))))

;;; Parsing (WI-LIVE-05, advisory A2)

(defun beads-live-parse-chunk (partial chunk)
  "Split PARTIAL + CHUNK into complete JSONL journal records.
Return (RECORDS . REST): RECORDS the parsed `beads-event-record'
objects of every complete line, in order, and REST the trailing
incomplete line to keep for the next chunk.  Lines that are not JSON
objects (ssh banners, `bd' commentary, stray text) are skipped, and a
line that fails to parse is dropped: the stream parser must never
signal the way the whole-text `beads-events-records-from-json-lines'
does.  Each record's raw wire `issue' alist is remembered in
`beads-live--raw-issues' so the sparse merge can see fields the
`beads-issue' class drops."
  (let* ((text (concat partial chunk))
         (end (string-match-p "\n[^\n]*\\'" text))
         records)
    (if (null end)
        (cons nil text)
      (dolist (line (split-string (substring text 0 end) "\n" t))
        (when (string-match-p "\\`[[:space:]]*{" line)
          (condition-case nil
              (let* ((raw (json-parse-string line :object-type 'alist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object nil))
                     (record (beads-event-record-from-json raw)))
                (puthash record (alist-get 'issue raw) beads-live--raw-issues)
                (push record records))
            (json-error nil))))
      (cons (nreverse records) (substring text (1+ end))))))

;;; Delivery (WI-LIVE-05)

(defun beads-live--notify-subscribers (stream record)
  "Call STREAM's raw subscribers with RECORD.
A subscriber is a (FUNCTION . BUFFER) cons; BUFFER nil means call in
whatever buffer is current.  A subscriber error never breaks the
stream."
  (dolist (sub (beads-live--stream-subscribers stream))
    (let ((fn (car sub))
          (buf (cdr sub)))
      (when (or (null buf) (buffer-live-p buf))
        (condition-case err
            (if buf
                (with-current-buffer buf (funcall fn record))
              (funcall fn record))
          (error (message "beads-live: subscriber error: %s"
                          (error-message-string err))))))))

(defun beads-live--append-ring (model record)
  "Prepend RECORD to MODEL's ring, trimming it to its ring size."
  (setf (beads-live--model-ring model)
        (cons record (beads-live--model-ring model)))
  (let ((size (beads-live--model-ring-size model)))
    (when (and (integerp size) (> (length (beads-live--model-ring model)) size))
      (setf (beads-live--model-ring model)
            (seq-take (beads-live--model-ring model) size)))))

(defun beads-live--append-log (model record)
  "Prepend RECORD to MODEL's per-issue log for its issue id."
  (when-let* ((id (oref record issue-id)))
    (puthash id
             (cons record (gethash id (beads-live--model-logs model)))
             (beads-live--model-logs model))))

(defun beads-live--note-activity (stream)
  "Record one delivery time on STREAM for the status rate.
Bounded so a long-lived stream does not grow without limit."
  (setf (beads-live--stream-activity stream)
        (cons (float-time)
              (seq-take (beads-live--stream-activity stream) 128))))

(defun beads-live--deliver (stream records)
  "Apply RECORDS to STREAM's model, subscribers and debounce batch.
Each record advances STREAM's seq (records at or below it are ignored
as echoes of an already-applied mutation), has its sparse issue
snapshot merged, and is appended to the bounded ring and the per-issue
log.  Raw subscribers and the public `beads-event-hooks' run
immediately; the op is queued for one debounced invalidation."
  (let ((model (beads-live-model stream)))
    (dolist (record records)
      (let ((seq (oref record seq)))
        (if (and (integerp seq)
                 (<= seq (or (beads-live--stream-seq stream) -1)))
            nil ;; an echo of a record already applied
          (when (integerp seq)
            (setf (beads-live--stream-seq stream) seq))
          (beads-live--merge-issue model
                                   (oref record issue-id)
                                   (beads-live--record-snapshot record))
          (beads-live--append-ring model record)
          (beads-live--append-log model record)
          (beads-live--note-activity stream)
          (beads-live--notify-subscribers stream record)
          (beads-live--run-hook 'beads-event-hooks record
                                (beads-live--stream-root stream))
          (beads-live--queue stream record))))))

;;; Debounced invalidation (WI-LIVE-05, WI-LIVE-09)

(defconst beads-live-op-routes
  '(("create" . (list show dashboard))
    ("update" . (list show dashboard))
    ("close" . (list show dashboard))
    ("delete" . (list show dashboard))
    ("dep_add" . (list show dashboard))
    ("dep_remove" . (list show dashboard))
    ("comment" . (show)))
  "Journal op → view kinds it invalidates.
Claim, reopen, status, assignee and label changes all arrive as
`update', so the op is the coarse key and the field delta refines it in
the views.")

(defun beads-live-route (op)
  "Return the view kinds journal OP invalidates."
  (cdr (assoc op beads-live-op-routes)))

(defun beads-live--queue (stream record)
  "Add RECORD's op to STREAM's debounce batch, arming the timer.
A burst of records therefore coalesces into one flush."
  (push record (beads-live--stream-pending stream))
  (unless (beads-live--stream-debounce-timer stream)
    (setf (beads-live--stream-debounce-timer stream)
          (run-at-time beads-live-debounce nil #'beads-live--flush stream))))

(defun beads-live--batch-ops (records)
  "Return the ordered, de-duplicated journal ops in RECORDS."
  (delete-dups (delq nil (mapcar (lambda (record) (oref record op)) records))))

(defun beads-live--batch-kinds (ops)
  "Return the de-duplicated view kinds journal OPS invalidate."
  (delete-dups (cl-mapcan (lambda (op) (copy-sequence (beads-live-route op)))
                          ops)))

(defun beads-live--invalidate (root kinds ops)
  "Route one debounced batch for the store at ROOT.
KINDS is `all' or a list of view kinds; OPS the journal ops.  Runs
`beads-live-invalidate-functions', then each attached view's REFRESH
whose kinds intersect KINDS (a nil KINDS means any)."
  (beads-live--run-hook 'beads-live-invalidate-functions root kinds ops)
  (when-let* ((stream (and root
                           (gethash (beads-live--stream-key root)
                                    beads-live--streams))))
    (dolist (buf (beads-live--stream-views stream))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((fn beads-live--refresh)
                (want beads-live--kinds))
            (when (and fn (or (eq kinds 'all) (null want)
                              (cl-intersection want kinds)))
              (condition-case err
                  (funcall fn)
                (error (message "beads-live: refresh error: %s"
                                (error-message-string err)))))))))))

(defun beads-live--flush (stream)
  "Invalidate STREAM's pending batch once and checkpoint its seq.
The batch is empty and the timer slot cleared even when no view is
attached, so a later record starts a fresh batch."
  (let* ((records (nreverse (beads-live--stream-pending stream)))
         (ops (beads-live--batch-ops records))
         (kinds (beads-live--batch-kinds ops)))
    (setf (beads-live--stream-pending stream) nil
          (beads-live--stream-debounce-timer stream) nil)
    (when records
      (beads-live--invalidate (beads-live--stream-root stream) kinds ops)
      (beads-live--checkpoint-write stream))))

;;; Process filter (WI-LIVE-05)

(defun beads-live--filter (stream chunk)
  "Feed CHUNK of STREAM's stdout to the parser and deliver the records.
A delivered record marks the stream live; the trailing partial line is
kept on the stream for the next chunk."
  (let ((parsed (beads-live-parse-chunk
                 (beads-live--stream-partial stream) chunk)))
    (setf (beads-live--stream-partial stream) (cdr parsed))
    (when (car parsed)
      (beads-live--set-state stream 'live nil)
      (beads-live--deliver stream (car parsed)))))

;;; Checkpoint identity and storage (WI-LIVE-06)

(defvar beads-live-state-directory nil
  "Directory holding per-store live checkpoints.
Defined as a user option in `beads-custom.el' (WI-LIVE-02); when unbound
or nil, `locate-user-emacs-file' supplies the default location.")

;;; Injectable seams (WI-LIVE-06)

(defvar beads-live-baseline-function nil
  "Function of STREAM that performs the full baseline read.
When nil, `beads-live-baseline-read' uses the existing list loader.
Substituted in tests so no `bd' process is needed.")

(defvar beads-live-head-function nil
  "Function of STREAM returning the store's highest journal seq.
When nil, `beads-live-head-read' reads the finite journal.
Substituted in tests so no `bd' process is needed.")

(defvar beads-live-spawn-function nil
  "Function of STREAM that starts the follower process.
When nil, the real process creator `beads-live--spawn' (WI-LIVE-04) is
used.  This is the scripted-emitter test seam.")

(defvar beads-live-branch-function nil
  "Function of ROOT returning the store's branch identity.
When nil, `beads-live--read-branch' reads it from git (local only).")

(defvar beads-live-replica-function nil
  "Function of ROOT returning the store's replica identity.
When nil, `beads-live--read-replica' reads it best-effort.")

(defvar beads-live-capability-function nil
  "Function of ROOT and REFRESH returning `stream' or `poll'.
When nil, the capability probe from WI-LIVE-03 is used.")

(defvar beads-live-replica-id nil
  "Explicit replica identity, or nil to resolve one per store.
Useful for tests and for a caller that already knows the replica.")

(defun beads-live--read-branch (root)
  "Return ROOT's git branch, or nil.
Only local roots are inspected; a remote root returns nil here so no
TRAMP round trip is made from the stream layer.  A detached HEAD or a
non-git store yields nil."
  (when (and (stringp root)
             (file-directory-p root)
             (not (file-remote-p root)))
    (ignore-errors
      (with-temp-buffer
        (let ((default-directory (file-name-as-directory root)))
          (when (zerop (process-file "git" nil t nil
                                     "rev-parse" "--abbrev-ref" "HEAD"))
            (let ((branch (string-trim (buffer-string))))
              (unless (or (string-empty-p branch)
                          (string= branch "HEAD"))
                branch))))))))

(defun beads-live--config-string (result)
  "Extract a configuration string from command RESULT, or nil."
  (cond
   ((null result) nil)
   ((stringp result) result)
   ((eq result t) "true")
   ((and (listp result) (assq 'value result))
    (let ((value (cdr (assq 'value result))))
      (and (stringp value) value)))
   (t nil)))

(defun beads-live--read-replica (root)
  "Return ROOT's replica identity, or nil.
Best-effort, in order: `beads-live-replica-id', `$BEADS_REPLICA', then
`bd config get replica-id'.  A nil replica is valid: checkpoints are
already scoped to the local state directory and the store root."
  (or beads-live-replica-id
      (getenv "BEADS_REPLICA")
      (when (fboundp 'beads-command-config-get)
        (beads-live--config-string
         (beads-command-execute
          (beads-command-config-get
           :key "replica-id"
           :directory (directory-file-name root)))))))

(defun beads-live--branch (root)
  "Return ROOT's branch identity through the configured seam."
  (ignore-errors
    (funcall (or beads-live-branch-function #'beads-live--read-branch) root)))

(defun beads-live--replica (root)
  "Return ROOT's replica identity through the configured seam."
  (ignore-errors
    (funcall (or beads-live-replica-function #'beads-live--read-replica) root)))

(defun beads-live--identity (root)
  "Return ROOT's checkpoint identity as `(:branch B :replica R)'."
  (list :branch (beads-live--branch root)
        :replica (beads-live--replica root)))

(defun beads-live--state-directory ()
  "Return the directory that holds live checkpoints."
  (file-name-as-directory
   (or (and (boundp 'beads-live-state-directory)
            (stringp beads-live-state-directory)
            (not (string-empty-p beads-live-state-directory))
            beads-live-state-directory)
       (locate-user-emacs-file "beads-live/"))))

(defun beads-live-checkpoint-file (root branch replica)
  "Return the checkpoint file for the (ROOT, BRANCH, REPLICA) identity.
The name is a hash of the three components under
`beads-live-state-directory', so two branches or replicas of one store
can never collide."
  (expand-file-name
   (format "beads-live-%s.eld"
           (secure-hash 'sha1
                        (format "%s\0%s\0%s"
                                (or root "")
                                (or branch "")
                                (or replica ""))))
   (beads-live--state-directory)))

(defun beads-live-checkpoint-read (root branch replica)
  "Return the valid checkpoint plist for ROOT/BRANCH/REPLICA, or nil.
A missing, unreadable, malformed, or identity-mismatched checkpoint is
discarded — the branch/replica check is repeated on the content, so a
checkpoint copied between identities is never carried across."
  (let ((file (beads-live-checkpoint-file root branch replica)))
    (when (file-readable-p file)
      (let ((data (condition-case nil
                      (with-temp-buffer
                        (insert-file-contents-literally file)
                        (goto-char (point-min))
                        (read (current-buffer)))
                    (error nil))))
        (when (and (listp data)
                   (equal (plist-get data :root) root)
                   (equal (plist-get data :branch) branch)
                   (equal (plist-get data :replica) replica)
                   (integerp (plist-get data :seq)))
          data)))))

(defun beads-live-checkpoint-write (root branch replica seq)
  "Write a checkpoint for ROOT/BRANCH/REPLICA at SEQ.
The write is atomic: a temporary file in the checkpoint directory is
renamed over the target.  Returns the file name."
  (let* ((file (beads-live-checkpoint-file root branch replica))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (let ((tmp (make-temp-file (expand-file-name ".beads-live-" dir) nil ".eld")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (let ((print-level nil)
                    (print-length nil))
                (prin1 (list :root root :branch branch :replica replica
                             :seq seq :updated (float-time))
                       (current-buffer))))
            (rename-file tmp file t))
        (when (file-exists-p tmp)
          (ignore-errors (delete-file tmp)))))
    file))

(defun beads-live-checkpoint-delete (root branch replica)
  "Delete ROOT/BRANCH/REPLICA's checkpoint file, if any.
Returns non-nil when a file was removed."
  (let ((file (beads-live-checkpoint-file root branch replica)))
    (when (file-exists-p file)
      (delete-file file)
      t)))

(defun beads-live-checkpoint-save (stream seq)
  "Persist SEQ as STREAM's checkpoint for its current identity.
A non-integer SEQ is ignored.  Returns the checkpoint file, or nil."
  (when (integerp seq)
    (beads-live-checkpoint-write (beads-live--stream-root stream)
                                 (beads-live--stream-branch stream)
                                 (beads-live--stream-replica stream)
                                 seq)))

(defun beads-live--checkpoint-write (stream)
  "Persist STREAM's applied seq as its checkpoint.
Coalesced with the debounced batch; a no-op when STREAM has no integer
seq (e.g. before a resume).  This is the private hook
`beads-live--flush' calls and the unit suite substitutes."
  (beads-live-checkpoint-save stream (beads-live--stream-seq stream)))

;;; Baseline and head (WI-LIVE-06)

(defun beads-live-baseline-read (stream)
  "Read every issue of STREAM's store with the existing list loader.
Returns a list of `beads-issue', or nil when the read fails."
  (require 'beads-command-list nil t)
  (let ((root (beads-live--stream-root stream)))
    (condition-case err
        (beads-command-execute
         (beads-command-list :all t
                             :directory (directory-file-name root)))
      (error
       (beads--log 'info "beads-live: baseline read failed for %s: %s"
                   root (error-message-string err))
       nil))))

(defun beads-live-head-read (stream)
  "Return the highest journal seq for STREAM's store, or nil.
Reads the finite journal (`events tail', no `--follow') and takes the
maximum seq; nil means an empty journal or a failed read."
  (require 'beads-command-events nil t)
  (let ((root (beads-live--stream-root stream)))
    (condition-case err
        (let ((records (beads-command-execute
                        (beads-command-events-tail
                         :directory (directory-file-name root)))))
          (cl-loop for record in records
                   maximize (oref record seq)))
      (error
       (beads--log 'info "beads-live: head read failed for %s: %s"
                   root (error-message-string err))
       nil))))

(defun beads-live--baseline (stream)
  "Full read of STREAM's store, seeding the model and BASELINE slot.
Returns the issue list, or nil."
  (let ((issues (ignore-errors
                  (funcall (or beads-live-baseline-function
                               #'beads-live-baseline-read)
                           stream))))
    (setf (beads-live--stream-baseline stream) issues)
    (when (and issues (fboundp 'beads-live--model-seed))
      (beads-live--model-seed stream issues))
    issues))

(defun beads-live--head (stream)
  "Return STREAM's store head seq through the configured seam."
  (ignore-errors
    (funcall (or beads-live-head-function #'beads-live-head-read) stream)))

(defun beads-live--spawn-stream (stream)
  "Start STREAM's follower through the configured spawn seam.
Prefers `beads-live-spawn-function', then the real `beads-live--spawn'
from WI-LIVE-04, and otherwise leaves the control flow intact."
  (let ((fn (or beads-live-spawn-function
                (and (fboundp 'beads-live--spawn) #'beads-live--spawn))))
    (if fn
        (funcall fn stream)
      (beads-live--set-state stream 'connecting nil)
      nil)))

(defun beads-live--capability (root &optional refresh)
  "Return `stream' or `poll' for ROOT through the configured seam.
REFRESH forces a fresh capability read.  The journal-off / `bd'-too-old
case is `poll': no stream is started and the existing timer refresh is
left untouched (AC-2)."
  (cond
   (beads-live-capability-function
    (funcall beads-live-capability-function root refresh))
   ((fboundp 'beads-live-capability)
    (beads-live-capability root refresh))
   (t 'stream)))

(defun beads-live--resume-seq (stream)
  "Resolve STREAM's identity and return a valid checkpoint seq, or nil.
A checkpoint whose branch or replica differs is discarded: the file name
is keyed by `(root, branch, replica)' and the content is validated again."
  (let* ((root (beads-live--stream-root stream))
         (identity (beads-live--identity root))
         (branch (plist-get identity :branch))
         (replica (plist-get identity :replica)))
    (setf (beads-live--stream-branch stream) branch
          (beads-live--stream-replica stream) replica)
    (let ((checkpoint (beads-live-checkpoint-read root branch replica)))
      (setf (beads-live--stream-checkpoint stream) checkpoint)
      (plist-get checkpoint :seq))))

(defun beads-live--start-stream (stream)
  "Start or resume STREAM's follower, baselining when uncheckpointed.
With a valid checkpoint the follower resumes gap-free with `--since
<seq>'.  Otherwise the store is baselined and the follower starts at the
current head, which is also persisted so a crash before the first batch
does not re-baseline."
  (beads-live--set-state stream 'connecting nil)
  (setf (beads-live--stream-stopping stream) nil)
  (if-let* ((since (beads-live--resume-seq stream)))
      (progn
        (setf (beads-live--stream-seq stream) since)
        (beads-live--spawn-stream stream))
    (let ((head (beads-live--head stream)))
      (beads-live--baseline stream)
      (setf (beads-live--stream-seq stream) head)
      (when (integerp head)
        (beads-live-checkpoint-save stream head))
      (beads-live--spawn-stream stream))))

(defun beads-live--start (stream)
  "Start or resume STREAM honoring its journal capability.
A journal-off store goes to `poll' mode and starts no follower process
\(AC-2); otherwise the follower resumes or baselines."
  (let ((root (beads-live--stream-root stream)))
    (if (eq (beads-live--capability root) 'poll)
        (progn
          (setf (beads-live--stream-mode stream) 'poll)
          (beads-live--set-state stream 'poll nil)
          (beads-live--start-poll stream))
      (setf (beads-live--stream-mode stream) 'stream)
      (beads-live--start-stream stream))))

(defun beads-live-start (dir &optional refresh)
  "Start or resume the live journal stream for store DIR.
REFRESH forces a fresh journal-capability read.  Returns the
`beads-live--stream', or nil when DIR is not a store.  When the journal
is off (or `bd' is too old) the stream is put in `poll' mode and no
follower process is spawned (AC-2)."
  (let* ((root (beads-live--store-root dir))
         (stream (and root (beads-live--stream-for root))))
    (when stream
      (when refresh
        (beads-live-forget-capability root))
      (beads-live--start stream))
    stream))

;;; Re-baseline (WI-LIVE-06)

(defconst beads-live-truncation-marker "events journal truncated:"
  "Substring of the pruned-checkpoint diagnostic on stderr.
The stream omits `--json' precisely so this reason stays on stderr; see
design.md §4.5.  The exit classifier routes it to re-baseline.")

(defconst beads-live--truncation-regexp
  (concat (regexp-quote beads-live-truncation-marker)
          " checkpoint \\([0-9]+\\) is below the retained window "
          "\\[\\([0-9]+\\)\\.\\.\\([0-9]+\\)\\]")
  "Regexp for the pruned `--since' diagnostic.
Group 1 is the requested checkpoint, 2 the retention floor, 3 the head.")

(defun beads-live-truncation-p (text)
  "Return non-nil when TEXT reports a pruned checkpoint."
  (and (stringp text)
       (string-match-p (regexp-quote beads-live-truncation-marker) text)))

(defun beads-live-truncation-info (text)
  "Return `(:checkpoint N :floor F :head H)' parsed from TEXT, or nil."
  (when (and (stringp text)
             (string-match beads-live--truncation-regexp text))
    (list :checkpoint (string-to-number (match-string 1 text))
          :floor (string-to-number (match-string 2 text))
          :head (string-to-number (match-string 3 text)))))

(defun beads-live-checkpoint (stream)
  "Return STREAM's loaded checkpoint for its current identity, or nil."
  (beads-live-checkpoint-read (beads-live--stream-root stream)
                              (beads-live--stream-branch stream)
                              (beads-live--stream-replica stream)))

(defun beads-live-checkpoint-seq (stream)
  "Return STREAM's checkpointed seq, or nil."
  (plist-get (beads-live-checkpoint stream) :seq))

(defun beads-live-rebaseline (stream &optional reason)
  "Discard STREAM's checkpoint and rebuild from a full read.
Resets the checkpoint to the current head and records REASON (default
\"re-baselined\"); used for a pruned `--since', a branch/replica change,
and the periodic reconcile (design.md §4.6, AC-3).  Returns the new head
seq, or nil."
  (let ((root (beads-live--stream-root stream)))
    (beads-live-checkpoint-delete root
                                  (beads-live--stream-branch stream)
                                  (beads-live--stream-replica stream))
    (setf (beads-live--stream-checkpoint stream) nil)
    (let ((head (beads-live--head stream)))
      (beads-live--baseline stream)
      (setf (beads-live--stream-seq stream) head)
      (when (integerp head)
        (beads-live-checkpoint-save stream head))
      (beads-live--set-state stream 'partial
                             (or reason "re-baselined"))
      head)))

(defun beads-live--rebaseline (stream &optional reason)
  "Private alias of `beads-live-rebaseline' for STREAM and REASON.
The exit classifier calls this name so tests can substitute it without
also replacing the public function."
  (beads-live-rebaseline stream reason))

(defun beads-live-handle-truncation (stream text)
  "Re-baseline STREAM when TEXT is the pruned-checkpoint diagnostic.
Returns the new head when it re-baselined, else nil.  Called by the exit
classifier (WI-LIVE-07); any other stderr tail is a no-op."
  (when (beads-live-truncation-p text)
    (beads-live--rebaseline stream
                            "events journal truncated (checkpoint pruned)")))

(defun beads-live-reconcile (stream)
  "Periodic safety-net full read for STREAM's unjournaled writes.
`bd dolt pull' and `bd sql' bypass the journal, so the reconcile
re-baselines (design.md §4.6).  The timer decides how often."
  (beads-live--rebaseline stream "periodic reconcile"))

;;; Classification and backoff (WI-LIVE-07)

(defconst beads-live--journal-disabled-regexp
  "\\(?:events journal is disabled\\|events-journal\\|journal.*disabled\\)"
  "Stderr fragment of the journal-off note (`bd events tail').")

(defconst beads-live--unknown-subcommand-regexp
  "\\(?:unknown\\|unrecognized\\|no such\\) \\(?:command\\|subcommand\\|flag\\|option\\)\\|not supported"
  "Stderr fragment of an unknown/unsupported `bd events' invocation.")

(defconst beads-live--connection-regexp
  (concat "\\(?:connection\\|connect\\|dial tcp\\|no route to host"
          "\\|network is unreachable\\|host key\\|could not resolve"
          "\\|broken pipe\\|i/o timeout\\|unreachable\\|timed out\\)")
  "Stderr fragment of a connection or host failure.")

(defun beads-live--classify (stderr-tail)
  "Classify a stream exit from STDERR-TAIL into a recovery state.
STDERR-TAIL is the stderr text as a string or a list of its lines,
exactly what `beads-live--stderr-lines' returns.  Returns one of the
design.md §4.5 states: `poll' for a disabled journal or unknown
subcommand (stop retrying the stream), `partial' for a pruned
checkpoint (re-baseline), `offline' for a connection/host failure, or
`reconnecting' otherwise."
  (let ((text (cond
               ((null stderr-tail) "")
               ((listp stderr-tail) (mapconcat #'identity stderr-tail "\n"))
               ((stringp stderr-tail) stderr-tail)
               (t (format "%s" stderr-tail)))))
    (cond
     ((or (string-match-p beads-live--journal-disabled-regexp text)
          (string-match-p beads-live--unknown-subcommand-regexp text))
      'poll)
     ((beads-live-truncation-p text)
      'partial)
     ((string-match-p beads-live--connection-regexp text)
      'offline)
     (t 'reconnecting))))

(defun beads-live--backoff-delay (attempt)
  "Return the reconnect delay in seconds for ATTEMPT (0-based).
The schedule `beads-live-backoff' repeats once ATTEMPT runs past its
end; an empty schedule yields no delay."
  (let* ((schedule (or beads-live-backoff '(2 5 15 60)))
         (count (length schedule)))
    (if (zerop count)
        0
      (nth (mod (max 0 (or attempt 0)) count) schedule))))

(defun beads-live--stream-stable-p (stream)
  "Return non-nil when STREAM stayed up for `beads-live-stable-after'.
A stream that reached this age resets its backoff attempt, so a later
failure starts the schedule over instead of continuing where the last
run of failures left off."
  (let ((started (beads-live--stream-started stream)))
    (and (numberp started)
         (>= (- (float-time) started) beads-live-stable-after))))

(defun beads-live--cancel-retry (stream)
  "Cancel STREAM's pending reconnect timer, if any."
  (when-let* ((timer (beads-live--stream-retry-timer stream)))
    (when (timerp timer) (cancel-timer timer))
    (setf (beads-live--stream-retry-timer stream) nil)))

(defun beads-live--retry (stream)
  "Retry STREAM now; called by the backoff timer."
  (setf (beads-live--stream-retry-timer stream) nil)
  (when (and (beads-live--stream-enabled stream)
             (not (beads-live--stream-stopping stream)))
    (beads-live--start stream)))

(defun beads-live--schedule-retry (stream)
  "Schedule STREAM's next reconnect on the backoff schedule.
Uses the current ATTEMPT to pick the delay, then advances ATTEMPT for
the next failure.  Any previously pending timer is cancelled first."
  (beads-live--cancel-retry stream)
  (let* ((attempt (or (beads-live--stream-attempt stream) 0))
         (delay (beads-live--backoff-delay attempt)))
    (setf (beads-live--stream-attempt stream) (1+ attempt)
          (beads-live--stream-retry-at stream) (+ (float-time) delay)
          (beads-live--stream-retry-timer stream)
          (run-at-time delay nil #'beads-live--retry stream))))

(defun beads-live--exited (stream _proc mark)
  "Handle STREAM's `bd events tail' process exit.
MARK is the stderr buffer position captured before the process was
spawned.  Classify the failure from the stderr tail (design.md §4.5)
and transition the stream: a disabled journal or unknown subcommand
switches to `poll', a pruned checkpoint becomes `partial' and
re-baselines, a connection/host failure becomes `offline', and anything
else becomes `reconnecting'.  Every case but `poll' (and a deliberate
stop) schedules the next attempt on the backoff schedule; a stream that
had been stable forgets its attempt first.  Returns the classification."
  (setf (beads-live--stream-process stream) nil)
  (when (beads-live--stream-stable-p stream)
    (setf (beads-live--stream-attempt stream) 0))
  (let* ((lines (beads-live--stderr-lines stream mark))
         (reason (car (last lines)))
         (kind (beads-live--classify lines)))
    (cond
     ((beads-live--stream-stopping stream)
      (beads-live--cancel-retry stream)
      (beads-live--set-state stream 'off nil))
     ((eq kind 'poll)
      (setf (beads-live--stream-mode stream) 'poll)
      (beads-live--set-state stream 'poll reason)
      (when (fboundp 'beads-live--start-poll)
        (beads-live--start-poll stream)))
     ((eq kind 'partial)
      (beads-live--set-state stream 'partial reason)
      (when (fboundp 'beads-live--rebaseline)
        (beads-live--rebaseline stream reason))
      (beads-live--schedule-retry stream))
     (t
      (beads-live--set-state stream kind reason)
      (beads-live--schedule-retry stream)))
    kind))

(defun beads-live--reconnect-stream (stream)
  "Retry STREAM immediately with a full refresh (the `g' control).
Cancels any pending backoff/poll timer, resets the attempt count so the
schedule starts over, marks the resume as a full `:all' refresh so
anything that went stale during the gap is re-read, and starts the
stream again (design.md §4.5)."
  (beads-live--cancel-retry stream)
  (beads-live--stop-poll stream)
  (setf (beads-live--stream-attempt stream) 0
        (beads-live--stream-retry-at stream) nil
        (beads-live--stream-stopping stream) nil
        (beads-live--stream-resume stream) :all)
  (beads-live--set-state stream 'connecting "manual reconnect")
  (beads-live--start stream))

(defun beads-live-reconnect (&optional arg)
  "Reconnect the stream for ARG at once.
ARG is a `beads-live--stream' (internal callers and tests) or a store
directory / nil (the interactive `g' control), in which case the stream
covering ARG is reconnected.  Resets the backoff attempt, marks a full
refresh, and starts the follower (or re-arms polling)."
  (interactive)
  (let ((stream (if (beads-live--stream-p arg) arg (beads-live--find arg))))
    (when stream
      (beads-live--reconnect-stream stream))))

;;; Poll fallback (WI-LIVE-08)

(defun beads-live--stop-poll (stream)
  "Cancel STREAM's fallback poll timer, if any."
  (when-let* ((timer (beads-live--stream-poll-timer stream)))
    (when (timerp timer) (cancel-timer timer))
    (setf (beads-live--stream-poll-timer stream) nil
          (beads-live--stream-poll-busy stream) nil)))

(defun beads-live--start-poll (stream)
  "Arm STREAM's fallback poll timer (no follower process is started).
Used when the journal is off, `bd' is too old, or the store's remote
transport cannot stream (AC-2)."
  (unless (timerp (beads-live--stream-poll-timer stream))
    (setf (beads-live--stream-mode stream) 'poll
          (beads-live--stream-poll-timer stream)
          (run-at-time beads-live-poll-interval beads-live-poll-interval
                       #'beads-live--poll stream))))

(defun beads-live--poll (stream)
  "Read STREAM's journal once without following and deliver new records.
Runs `beads-command-events-tail' (no `--follow') through
`beads-command-execute-async' with a per-store cache key so overlapping
polls coalesce; a pruned checkpoint re-baselines (AC-3)."
  (require 'beads-command-events nil t)
  (unless (beads-live--stream-poll-busy stream)
    (let ((root (beads-live--stream-root stream))
          (since (beads-live--stream-seq stream)))
      (setf (beads-live--stream-poll-busy stream) t)
      (condition-case err
          (beads-command-execute-async
           (beads-command-events-tail
            :since since
            :limit beads-live-poll-limit
            :directory (directory-file-name root))
           (lambda (records)
             (setf (beads-live--stream-poll-busy stream) nil)
             (beads-live--deliver stream records)
             (beads-live--set-state stream 'poll nil))
           (lambda (err)
             (setf (beads-live--stream-poll-busy stream) nil)
             (let ((message (error-message-string err)))
               (if (beads-live-truncation-p message)
                   (beads-live--rebaseline
                    stream "events journal truncated (poll)")
                 (beads-live--set-state stream 'offline message))))
           :cache-key (concat (beads-live--stream-key root) "\0poll"))
        (error
         (setf (beads-live--stream-poll-busy stream) nil)
         (beads-live--set-state stream 'offline (error-message-string err)))))))

;;; Attach / detach (WI-LIVE-09)

(defun beads-live--running-p (stream)
  "Return non-nil when STREAM has a process or poll going.
A stream still confirming its connection (`connecting', or
`reconnecting' right after a respawn) is running: restarting it would
orphan its process."
  (or (process-live-p (beads-live--stream-process stream))
      (timerp (beads-live--stream-poll-timer stream))))

(cl-defun beads-live-attach (&optional buffer &key refresh kinds)
  "Attach BUFFER (default current) to its store's live journal stream.
The store is the root of BUFFER's `beads-store-directory' (or its
`default-directory').  The first view of a store starts its stream;
killing BUFFER detaches it (`beads-live-detach').  REFRESH, when
non-nil, is called with BUFFER current after a debounced batch touching
one of KINDS (`all', or a list of view kinds; nil means any) or after a
resume.  Returns the stream, or nil when BUFFER has no store root."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((root (beads-live--store-root (beads-live--default-root)))
           (stream (and root (beads-live--stream-for root))))
      (when stream
        (setq beads-live--refresh refresh
              beads-live--kinds kinds
              beads-live--root root)
        (add-hook 'kill-buffer-hook #'beads-live-detach nil t)
        (unless (memq (current-buffer) (beads-live--stream-views stream))
          (push (current-buffer) (beads-live--stream-views stream)))
        (when (and (beads-live--stream-enabled stream)
                   (not (beads-live--running-p stream))
                   (not (timerp (beads-live--stream-retry-timer stream))))
          (beads-live--start stream)))
      stream)))

(defun beads-live-detach (&optional buffer)
  "Detach BUFFER (default current) from its store's stream.
The last view out stops the stream, forgets it from the registry and
kills its stderr buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((root beads-live--root)
                (stream (and root
                             (gethash (beads-live--stream-key root)
                                      beads-live--streams))))
      (setf (beads-live--stream-views stream)
            (delq (current-buffer) (beads-live--stream-views stream)))
      (setf (beads-live--stream-subscribers stream)
            (cl-remove-if (lambda (sub) (eq (cdr sub) (current-buffer)))
                          (beads-live--stream-subscribers stream)))
      (setq beads-live--root nil)
      (unless (cl-some #'buffer-live-p (beads-live--stream-views stream))
        (beads-live--stop stream)
        (let ((stderr (beads-live--stream-stderr-buffer stream)))
          (when (buffer-live-p stderr) (kill-buffer stderr)))
        (beads-live--forget-stream root)
        (beads-live--run-hook 'beads-live-state-functions root 'gone nil)))))

(defun beads-live--stop (stream)
  "Stop STREAM's process and timers; leave it in the stream table."
  (setf (beads-live--stream-stopping stream) t)
  (beads-live--cancel-retry stream)
  (beads-live--stop-poll stream)
  (when (process-live-p (beads-live--stream-process stream))
    (delete-process (beads-live--stream-process stream)))
  (setf (beads-live--stream-process stream) nil)
  (beads-live--set-state stream 'off nil))

;;; Subscribe (WI-LIVE-09)

(defun beads-live-subscribe (fn &optional buffer)
  "Call FN with every raw record of BUFFER's store, as it arrives.
BUFFER (default current) is attached first when it has no stream.  FN
runs with BUFFER current and stops when it is killed.  Returns a handle
for `beads-live-unsubscribe'."
  (with-current-buffer (or buffer (current-buffer))
    (let ((stream (or (and beads-live--root
                           (gethash (beads-live--stream-key beads-live--root)
                                    beads-live--streams))
                      (beads-live-attach))))
      (when stream
        (let ((sub (cons fn (current-buffer))))
          (push sub (beads-live--stream-subscribers stream))
          (cons stream sub))))))

(defun beads-live-unsubscribe (handle)
  "Remove the subscription HANDLE from `beads-live-subscribe'."
  (when (consp handle)
    (let ((stream (car handle)))
      (setf (beads-live--stream-subscribers stream)
            (delq (cdr handle) (beads-live--stream-subscribers stream))))))

(defalias 'beads-events-subscribe #'beads-live-subscribe
  "Subscribe FN to every raw record of BUFFER's store.
The requirement-level name for `beads-live-subscribe' (US-4).")

;;; Status and header (WI-LIVE-09)

(defun beads-live--rate (stream)
  "Return STREAM's recent record rate in records per second.
Pure: counts the in-memory activity ring within the last second, so it
is safe at redisplay."
  (let ((now (float-time)))
    (cl-count-if (lambda (t0) (<= (- now t0) 1.0))
                 (beads-live--stream-activity stream))))

(defun beads-live-status (&optional dir)
  "Return the live state of DIR's store as a plist, or nil.
Keys: :state, :reason, :mode, :seq, :rate, :retry-in, :root, :name,
:host.  `:state' is one of `live', `poll', `partial', `connecting',
`reconnecting', `offline', `off' or `gone'.  Pure: reads the stream
table and in-memory activity only, so it is safe at redisplay."
  (when-let* ((stream (beads-live--find dir)))
    (list :state (if (beads-live--stream-enabled stream)
                     (beads-live--stream-state stream)
                   'off)
          :reason (beads-live--stream-reason stream)
          :mode (beads-live--stream-mode stream)
          :seq (beads-live--stream-seq stream)
          :rate (beads-live--rate stream)
          :retry-in (when-let* ((at (beads-live--stream-retry-at stream)))
                      (max 0 (ceiling (- at (float-time)))))
          :root (beads-live--stream-root stream)
          :name (beads-live--stream-name stream)
          :host (beads-live--stream-host stream))))

(defun beads-live-header-string (&optional dir)
  "Return the header-line fragment for DIR's store, or nil.
The `● live ∿N/s · seq M' chip degrades to `poll', `partial',
`offline' and `reconnecting' as in design.md §7.2/§7.9.  Pure; safe at
redisplay."
  (when-let* ((status (beads-live-status dir)))
    (let ((state (plist-get status :state))
          (seq (plist-get status :seq))
          (reason (plist-get status :reason)))
      (pcase state
        ('live
         (propertize
          (format "● live ∿%d/s%s"
                  (or (plist-get status :rate) 0)
                  (if (integerp seq) (format " · seq %d" seq) ""))
          'face 'beads-events-status))
        ('poll
         (propertize (format "● poll %ss" beads-live-poll-interval)
                     'face 'beads-events-status))
        ('partial
         (propertize (if reason (format "◐ partial: %s" reason) "◐ partial")
                     'face 'warning 'help-echo reason))
        ('connecting
         (propertize "○ connecting" 'face 'shadow))
        ('offline
         (propertize (format "○ offline @%s"
                             (or (plist-get status :host) "localhost"))
                     'face 'error 'help-echo reason))
        ('off
         (propertize "○ live off" 'face 'shadow))
        (_
         (propertize (if-let* ((n (plist-get status :retry-in)))
                         (format "○ reconnecting (%ds)" n)
                       "○ reconnecting")
                     'face 'warning 'help-echo reason))))))

;;; Invalidation seam and controls (WI-LIVE-09)

(defun beads-live-active-p (&optional dir)
  "Return non-nil when a `live' or `poll' stream covers DIR.
The command-layer seam (WI-LIVE-10): while it answers non-nil, a
completed foreground write leaves invalidation to the stream."
  (when-let* ((stream (beads-live--find dir)))
    (and (beads-live--stream-enabled stream)
         (memq (beads-live--stream-state stream) '(live poll))
         t)))

(defun beads-live-invalidate (dir &optional kinds ops)
  "Run a synthetic invalidation batch for DIR's attached views.
A public, gascity-free seam (AC-8): KINDS is `all' or a list of view
kinds; OPS the journal ops.  A no-op when no stream covers DIR."
  (when-let* ((stream (beads-live--find dir)))
    (beads-live--invalidate (beads-live--stream-root stream)
                            (or kinds 'all) ops)))

(defun beads-live-toggle (&optional dir)
  "Turn DIR's store stream off, or back on (the `W' control).
DIR defaults to the current buffer's store."
  (interactive)
  (let* ((root (beads-live--store-root (or dir (beads-live--default-root))))
         (stream (and root (beads-live--stream-for root))))
    (when stream
      (if (beads-live--stream-enabled stream)
          (progn
            (setf (beads-live--stream-enabled stream) nil)
            (beads-live--stop stream)
            (message "Live refresh off for %s"
                     (beads-live--stream-name stream)))
        (setf (beads-live--stream-enabled stream) t
              (beads-live--stream-attempt stream) 0)
        (beads-live--start stream)
        (message "Live refresh on for %s"
                 (beads-live--stream-name stream))))))

(defun beads-live-stop-all ()
  "Stop every store's live stream (e.g. before unloading beads)."
  (interactive)
  (maphash (lambda (_root stream) (beads-live--stop stream))
           beads-live--streams)
  (clrhash beads-live--streams))

(provide 'beads-live)
;;; beads-live.el ends here
