;;; beads-custom.el --- Customization variables for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el

;;; Commentary:

;; This file contains all user-customizable variables for beads.el.
;; Users can configure these variables via M-x customize-group RET beads RET
;; or by setting them in their Emacs configuration.

;;; Code:

;;; Customization Group

(defgroup beads nil
  "Magit-like interface for Beads issue tracker."
  :group 'tools
  :prefix "beads-")

;;; Executable and Database

(defcustom beads-executable "bd"
  "Path to the bd executable.
This can be either a simple command name (e.g., \"bd\") if it's
in your PATH, or a full path to the executable."
  :type 'string
  :group 'beads)

(defcustom beads-remote-search-path
  '("~/.guix-home/profile/bin"
    "~/.guix-profile/bin"
    "/run/current-system/profile/bin"
    "~/.nix-profile/bin"
    "~/.local/bin"
    "~/bin")
  "Directories probed for programs on a remote host.
When a program (bd, or gc/tmux for packages built on beads.el) runs
against a remote TRAMP `default-directory' under a bare name, TRAMP
resolves it against `tramp-remote-path', which covers system
directories but not per-user profile directories, so a program
installed via Guix, Nix or pip lands in exit 127.  These directories
are probed (in order, on the remote host, cached per connection) after
`tramp-remote-path' fails (`beads-remote-find-executable'), and are
prepended to the PATH of remote commands so the program's own
children resolve too (`beads-remote-path-assignment').  A leading `~'
is the remote user's home.  Run `beads-remote-forget' after changing
it mid-session."
  :type '(repeat string)
  :group 'beads)

(defcustom beads-remote-miss-ttl 60
  "Seconds a failed remote executable lookup is remembered, 0 to disable.
Resolving a bare program name on a remote host costs one synchronous
channel round trip per `beads-remote-search-path' entry.  A definite
miss is remembered this long, then re-probed, so installing the
program on the host heals itself.  Probe errors are never cached."
  :type 'natnum
  :group 'beads)

(defcustom beads-database-path nil
  "Path to the beads database.
If nil, bd will auto-discover the database by searching for a
.beads directory in the project hierarchy."
  :type '(choice (const :tag "Auto-discover" nil)
                 (file :tag "Database path"))
  :group 'beads)

;;; Dolt Server Configuration

(defcustom beads-dolt-port nil
  "TCP port of the Dolt server for bd commands.
When set to an integer, beads.el injects BEADS_DOLT_PORT=<port>
into the process environment for every bd command.  This ensures
bd connects to the specified Dolt server instead of auto-starting
its own instance or relying on metadata.json in the .beads
directory.

Set this to the port of your shared Dolt server, e.g.:
  (setq beads-dolt-port 3307)

If nil, beads.el does not inject BEADS_DOLT_PORT, and bd uses its
normal discovery order: BEADS_DOLT_PORT env var, then metadata.json,
then auto-start."
  :type '(choice (const :tag "Let bd discover Dolt port" nil)
                 (integer :tag "Dolt server TCP port"))
  :group 'beads)

;;; Actor Configuration

(defcustom beads-actor nil
  "Actor name for audit trail.
If nil, uses the $USER environment variable.  This value is used
for tracking who performs actions in the issue tracker."
  :type '(choice (const :tag "Use $USER" nil)
                 (string :tag "Actor name"))
  :group 'beads)

;;; Debug Settings

(defcustom beads-enable-debug nil
  "Enable debug logging to *beads-debug* buffer.
When enabled, all bd commands and their output will be logged
for troubleshooting purposes."
  :type 'boolean
  :group 'beads)

(defcustom beads-debug-level 'info
  "Debug logging level.
- `error': Only log errors
- `info': Log commands and important events (default)
- `verbose': Log everything including command output"
  :type '(choice (const :tag "Error only" error)
                 (const :tag "Info (commands and events)" info)
                 (const :tag "Verbose (all output)" verbose))
  :group 'beads)

;;; UI Behavior

(defcustom beads-auto-refresh t
  "Automatically refresh buffers after mutations.
When enabled, beads list and show buffers will automatically
refresh after operations like create, update, or close."
  :type 'boolean
  :group 'beads)

(defcustom beads-list-default-limit 0
  "Default limit for issue list operations.
When 0, all issues are returned (no limit).
When set to a positive integer, limits the number of issues returned
in list operations such as `beads-command-list'.

This can be overridden per-command by explicitly setting the :limit
argument when calling list commands."
  :type 'natnum
  :group 'beads)

;;; Completion Behavior

(defcustom beads-completion-show-unavailable-backends t
  "Whether to show unavailable backends in completion lists.
When non-nil, unavailable backends are shown but cannot be selected.
They appear grayed out and are grouped separately from available backends.
When nil, only available backends appear in the completion list.

This affects `beads-agent-start' and related functions that prompt
for backend selection."
  :type 'boolean
  :group 'beads-agent)

(defcustom beads-agent-curated-backends
  '("claude-code" "agent-shell" "terminal")
  "Backends shown directly in the agent launch UI.
Backends not listed here stay registered and selectable, but the
launch menu places them behind an `... other' overflow (the backend
list is curated, not truncated).  Names are matched against the
backend `name' slot; a name that is not registered is ignored.

`mock' is a test-only backend and is never shown in the user menu."
  :type '(repeat string)
  :group 'beads-agent)

;;; Formula Variables

(defcustom beads-formula-enum-metadata-keys
  '(("drain_policy" . allowed_drain_policies)
    ("interaction_mode" . interaction_modes)
    ("review_mode" . review_modes))
  "Built-in mapping of a formula variable name to its methodology choice key.
Some formula variables declare their allowed values in the formula's
`metadata.gc.methodology' object rather than on the variable itself.  Each
entry maps a variable name to the methodology key holding its choices; a
variable with neither an explicit `enum' nor an entry here is read as plain
text.  Downstream packages that ship additional methodology var names can
add to this list (`beads-formula-var-choices' consults it)."
  :type '(alist :key-type string :value-type symbol)
  :group 'beads)

;;; Live Journal Streaming

(defgroup beads-live nil
  "Live `bd events' journal streaming.
These options configure how beads.el follows a store's event journal,
coalesces and applies records, recovers from disconnects, and degrades
to polling when the journal is unavailable."
  :group 'beads
  :prefix "beads-live-")

(defcustom beads-live-state-directory
  (locate-user-emacs-file "beads-live/")
  "Directory holding per-store live journal checkpoints.
Each stream persists its last applied sequence number in an `.eld'
file keyed by store root, branch and replica, so restarting Emacs
resumes the tail without replaying the whole journal.  A checkpoint
whose branch or replica no longer matches its store is discarded."
  :type 'directory
  :group 'beads-live)

(defcustom beads-live-debounce 0.15
  "Seconds to coalesce a burst of journal records before refreshing views.
Records arriving within this window are applied to the in-memory model
immediately but are batched into a single redisplay and a single
checkpoint write, so a factory burst does not cause one refresh per
record."
  :type 'number
  :group 'beads-live)

(defcustom beads-live-poll-interval 30
  "Seconds between fallback journal polls.
Used when live tailing is unavailable (journal disabled, a remote
method that cannot stream, or an older `bd'), and as the relaxed
reconcile cadence while a live stream is active."
  :type 'natnum
  :group 'beads-live)

(defcustom beads-live-backoff '(2 5 15 60)
  "Reconnect backoff schedule in seconds, repeated while a stream is down.
Each failed reconnect waits the next delay in this list, cycling back
to the start after the last one, until a stream stays up for
`beads-live-stable-after' seconds and the schedule resets."
  :type '(repeat natnum)
  :group 'beads-live)

(defcustom beads-live-stable-after 15
  "Seconds a stream must stay up before the reconnect backoff resets.
A stream that dies sooner is treated as part of the same outage and
the next backoff delay is used instead of restarting the schedule."
  :type 'natnum
  :group 'beads-live)

(defcustom beads-live-reconcile 600
  "Seconds between periodic full reconciles while a live stream is active.
Reconciles re-read the store to pick up writes the journal never saw,
such as those from `bd dolt pull' or direct SQL.  The UI reports a
partial state while a reconcile is in flight and never claims `live'."
  :type 'natnum
  :group 'beads-live)

(defcustom beads-live-change-window 30
  "Seconds a row stays marked as recently changed after a journal update.
The `beads-event-changed' face (the `◈' marker) fades after this window
so a burst of activity is visible without permanently recolouring rows."
  :type 'natnum
  :group 'beads-live)

(defcustom beads-events-notify-ops '("close" "dependency_added")
  "Journal ops that may raise a desktop notification.
Used by `beads-events-notify-mode' (opt-in, off by default).  The
default notifies on closures and on dependency additions that make a
bead ready; an unknown op name is ignored.  Notifications never fire
while the mode is disabled."
  :type '(repeat string)
  :group 'beads-live)

(defcustom beads-events-notify-rate-limit 60
  "Minimum seconds between notifications for the same issue.
Rate-limits a notify storm during a factory drain to at most one
message per issue per interval.  Set to 0 to disable rate limiting."
  :type 'natnum
  :group 'beads-live)

;;; Provide

(provide 'beads-custom)
;;; beads-custom.el ends here
