---
plan_slug: beads-events-live
phase: validation
rig: beads.el
rig_root: /home/roman/workspace/beads.el
artifact_root: /home/roman/workspace/beads.el/plans
schema: beads.events-live.experiments.v1
artifact: experiments
status: complete
scope: planning-only
bd_version: 1.3.1
created_at: 2026-10-09T10:22:00Z
updated_at: 2026-10-09T10:22:00Z
---

# bead-604 experiments — `bd events` journal empirical validation

Empirical validation for the `plans/beads-events-live/` design (`bead be-ca9j`).
Every claim below was produced by running the installed `bd` against a
**throwaway** temp project (`mktemp -d`, `bd init`, `bd config set
events-journal true`), never against `~/bright-lights`, the rig store, or any
production store. Raw commands and outputs are recorded verbatim.

## 0. Environment and method

- `bd version 1.3.1 (dev)` (`/home/roman/.guix-home/profile/bin/bd`).
- Temp project: `mktemp -d /tmp/be-xwwt-job1.*`, `bd init` (embedded Dolt,
  `.beads/embeddeddolt/`), `bd config set events-journal true`.
- Every `bd` invocation unset the Gas Town isolation variables
  (`BEADS_DIR`, `BEADS_DOLT_PORT`, `BEADS_DOLT_SERVER_PORT`, `GC_DOLT_PORT`,
  `BEADS_ACTOR`, `GC_SESSION_NAME`, `GC_SESSION_ID`, `GC_AGENT`,
  `GC_TEMPLATE`) so the agent shell cannot reroute `bd` to a production store.
  This is the same hazard `beads-test-isolation-env-vars` guards in the test
  suite; without it `bd init` inside `/tmp` aborted against an unrelated
  database because `$BEADS_DIR` was exported.
- Actor is fixed with `--actor probe` for the canonical sequence so the op
  mapping is unambiguous; the actor-precedence matrix (§4) deliberately varies
  the environment instead.
- Raw evidence bundle retained at `/tmp/be-xwwt-job1-evidence/`
  (`full-tail.jsonl`, `full-export.jsonl`, `sequence.txt`, `ids.txt`,
  `version.txt`).

## 1. Canonical sequence and the raw journal

Commands, in order (after `bd config set events-journal true`):

```
bd create "blocker"                                  # -> B
bd create "dependent"                                # -> D
bd --actor probe dep add    "$D" "$B"
bd --actor probe comment    "$D" "validation comment"
bd --actor probe update     "$D" --claim
bd --actor probe update     "$B" --priority 1
bd --actor probe close      "$B"
bd --actor probe reopen     "$B"
bd --actor probe label add  "$B" foo,bar
bd --actor probe label remove "$B" foo
bd --actor probe update     "$B" --status in_progress
bd --actor probe update     "$B" --assignee alice
bd --actor probe delete     "$D" --force
```

`bd events tail --since 0 --limit 100000` after the sequence (abridged issue
snapshots; the full file is the evidence bundle):

```
1  create   B       actor=Roman Scherer  issue={id,title,status,priority,issue_type,owner,created_at,created_by,updated_at}
2  create   D       actor=Roman Scherer  (same field set)
3  dep_add  D       actor=probe          issue has is_blocked:true
                                        dep={kind:"blocks",target:B,metadata:"{}"}
4  comment  D       actor=probe          comment={id,author,text,created_at,source}
5  update   D       actor=probe          status=in_progress assignee=probe started_at lease_expires_at heartbeat_at
6  update   B       actor=probe          priority 2 -> 1
7  update   D       actor=<ABSENT>       is_blocked removed (derived unblock)
8  close    B       actor=probe          status=closed closed_at close_reason
9  update   D       actor=<ABSENT>       is_blocked:true re-added (derived re-block)
10 update   B       actor=probe          status=open (this IS "reopen")
11 update   B       actor=probe          labels=["bar"]
12 update   B       actor=probe          labels=["bar","foo"]
13 update   B       actor=probe          labels=["bar"]  (label remove)
14 update   B       actor=probe          status=in_progress
15 update   B       actor=probe          assignee=alice
16 dep_remove D     actor=probe          dep={kind:"blocks",target:B,...} (cascade of delete)
17 delete   D       actor=probe          issue: null
```

Op census (`collections.Counter`):

```
{'create': 2, 'dep_add': 1, 'comment': 1, 'update': 10,
 'close': 1, 'dep_remove': 1, 'delete': 1}   # 17 records
```

`bd events tail --since 0` and `bd events export` are byte-identical
(`tail==export: True`).

## 2. Verdict table (assumption -> result)

| # | Assumption in the design | Verdict | Evidence |
|---|---|---|---|
| 1 | The journal emits exactly `create/update/close/delete/dep_add/dep_remove/comment`. | **CONFIRMED** | §1 op census: exactly those seven. |
| 1a | `claim`, `status_changed`, `reopen`, `label_add`/`label_removed` are distinct ops. | **FALSIFIED** | Claim, priority, reopen, status, assignee and **label** changes all arrive as `op:"update"`. There is no `claim`/`status_changed`/`reopen`/`label_*` op. |
| 1b | Label add of N labels is one record. | **FALSIFIED** | `label add B foo,bar` produced **two** `update` rows (seq 11 `labels:["bar"]`, seq 12 `labels:["bar","foo"]`) — one per label. |
| 1c | Only user mutations appear. | **FALSIFIED** | Closing a blocker emits a derived `update` on the dependent (seq 7), and reopening re-blocks it (seq 9). Derived cascade rows carry **no `actor`** (field omitted). |
| 2 | `record.issue` is the full post-mutation issue object. | **FALSIFIED** | It is a **sparse/`omitempty` partial**: the key set varies per record (`is_blocked` only after a dep change, `labels` only after a label op, `close_reason` only after close, lease fields only after claim). `description`, `dependencies`, `comments`, `*_count`, `revision` are **never** present. `bd show --json` has 17 keys; a journal `issue` had at most 14, and never the same set twice. |
| 2a | `record.issue` is `null` on delete. | **CONFIRMED** | seq 17: `"issue":null`. |
| 2b | `dep`/`comment` payloads are present only on their ops. | **CONFIRMED** | `dep` only on `dep_add`/`dep_remove`; `comment` only on `comment`. |
| 2c | `dep_add` is emitted for an idempotent same-type re-add. | **CONFIRMED** | Two consecutive `dep add X Y` emitted seq 20 and seq 21, both `dep_add`. |
| 2d | `dep_remove` of an already-gone edge emits nothing. | **CONFIRMED** | A second `dep remove X Y` emitted no record; the first emitted a derived `update` (unblock) then one `dep_remove`. |
| 3 | `actor` is a bd audit field with precedence `$BEADS_ACTOR > bd config > git user.name > $USER`. | **CONFIRMED** | §4 matrix; `--actor` additionally beats all four. |
| 3a | `actor` is always present. | **FALSIFIED** | Derived cascade rows omit it (no `"actor"` key at all). Empty means "no actor behind the write", not "unknown user". |
| 4 | `--since` returns records with `seq > N` (strictly greater). | **CONFIRMED** | `--since H` empty; `--since H-1` returned `[H]`. |
| 4a | `--limit` truncates. | **CONFIRMED** | `--limit 3` returned `[1,2,3]`. |
| 4b | Journal-off is detected by a failing/empty `bd events tail`. | **FALSIFIED** | With the journal off, `tail` **exits 0** and still prints historical rows; the "disabled" note goes to **stderr**. Detection must use `bd config get events-journal` (or the stderr note), not the exit status. |
| 4c | A `--since` below the pruned floor fails. | **CONFIRMED** | exit 1; human error on stderr; `--json` emits a `events_journal_truncated` object on **stdout**. |
| 5 | `tail`/`export` emit one JSON object per line in both human and `--json` mode. | **CONFIRMED** | Byte-identical output with and without `--json` for both commands. |
| 5a | `--json` only affects success output. | **FALSIFIED (detail)** | On the pruned-floor error, `--json` prints a **multi-line pretty-printed** JSON object on stdout, not JSONL. A line-oriented parser must not assume every stdout line is a complete record. |
| 6 | `bd serve` streams the journal over HTTP (design defers it). | **UNKNOWN / deferral holds** | `bd serve` refuses to start on the embedded-Dolt backend ("requires a Dolt SQL server"); its help documents **no** events route. No evidence of an HTTP journal stream; the deferral is consistent. |
| 7 | `beads-types.el`'s `beads-event-*` constants are the journal op strings. | **FALSIFIED** | Those constants are the **audit-events** vocabulary (`created`, `updated`, `claimed`, `status_changed`, `commented`, `closed`, `reopened`, `dependency_added`, `dependency_removed`, `label_added`, `label_removed`, `compacted`, `lease_reclaimed`) and do **not** equal the journal op set (`create`, `update`, `close`, `delete`, `dep_add`, `dep_remove`, `comment`). `beads-event.el` needs its own mapping. |
| 8 | `beads-event-record`'s `issue` slot is a `beads-issue`. | **CONFIRMED (with caveat)** | The class exists and parses, but because the wire object is sparse, a naive `beads-from-json` yields a `beads-issue` whose untouched slots are nil. Applying it wholesale would clobber known fields. |

## 3. Raw evidence by question

### 3.1 Op set and per-op fields

Seven ops, exactly. Per-op required/optional fields observed:

| op | keys (besides always-present `seq`, `ts`, `op`, `issue_id`) |
|---|---|
| `create` | `actor`; `issue` (sparse) |
| `update` | `actor` (omitted on derived); `issue` (sparse; the union of touched fields) |
| `close` | `actor`; `issue` (adds `closed_at`, `close_reason`) |
| `delete` | `actor`; `issue:null` |
| `dep_add` | `actor`; `issue`; `dep:{kind,target,metadata}` |
| `dep_remove` | `actor`; `issue`; `dep:{kind,target,metadata}` |
| `comment` | `actor`; `issue`; `comment:{id,author,text,created_at,source}` |

`seq` is `int64`, `ts` is RFC3339 UTC (`2026-10-09T10:21:41Z`). The snapshot is
a Go struct marshalled with `omitempty`: **absent field means zero/false**, not
"unchanged". This is the single most consequential finding for the model layer.

### 3.2 Snapshot vs `bd show --json`

```
bd show <id> --json keys:
  comment_count, comments_omitted, created_at, created_by, dependency_count,
  dependent_count, description, id, issue_type, labels, owner, priority,
  revision, status, title, updated_at

journal record.issue key sets observed (max):
  id, title, status, priority, issue_type, owner, created_by, created_at,
  updated_at [, is_blocked] [, assignee] [, labels] [, started_at]
  [, lease_expires_at] [, heartbeat_at] [, closed_at] [, close_reason]
```

Missing from every journal snapshot: `description`, `dependencies`,
`comments`, `dependency_count`, `dependent_count`, `comment_count`,
`revision`, `comments_omitted`.

### 3.3 Actor provenance matrix

With `git config user.name "Git User"`, `$USER=roman`, per invocation:

| Setup | `actor` |
|---|---|
| `--actor carol` (flag) + `BEADS_ACTOR=alice` + config `bob` | `carol` |
| `BEADS_ACTOR=alice` + config `bob` + git name | `alice` |
| no env; `bd config set actor bob`; git name | `bob` |
| no env; no config; git `user.name` | `Git User` |
| no env; no config; no git identity (`GIT_CONFIG_GLOBAL=/dev/null`, `GIT_CONFIG_SYSTEM=/dev/null`, local unset) | `roman` (`$USER`) |
| `BEADS_ACTOR=` (empty) | falls through (to git name / `$USER`) |

Conclusion: **`--actor` > `$BEADS_ACTOR` > `bd config actor` > `git user.name`
> `$USER`**; empty string is treated as unset. Gas City exports `BEADS_ACTOR`
per session (observed value in the agent shell: `ec-wisp-21mcmn`), and the
journal records exactly that string — never a session *name*, *id*, agent type,
or template.

### 3.4 `--since` / `--limit` / `--follow`

```
$ bd events tail --since 10 --limit 10          # HEAD was 10
(no output; exit 0)
$ bd events tail --since 9 --limit 10
{"seq":10,...}                                   # strictly greater than --since
$ bd events tail --since 0 --limit 3
seq 1,2,3
```

`--follow` prints the backlog from `--since` and then streams each committed
record, flushed per line. A `bd create` produced the new line on the follower's
stdout within the poll interval (<~0.2s here), with nothing on stderr.
`--follow` never exits (it kept running under `timeout`), so it cannot go
through a request/response executor.

### 3.5 Journal off: exit 0 + stderr note (detection)

```
$ bd config get events-journal
false
$ bd events tail --since 0 --limit 5  1>/dev/null   # stdout discarded
note: the events journal is disabled for this workspace (enable with
'bd config set events-journal true'); any records shown were written while it
was enabled, and new mutations are not being recorded
$ bd events tail --since 0 --limit 5  ; echo exit=$?
{...historical rows...}
exit=0
$ bd events tail --follow               # still streams nothing new, never exits
note: ... (stderr)
```

So a naive `tail --follow` against a journal-off store looks connected and
silent. **Capability detection must probe `bd config get events-journal`** (or
parse stderr), and must not infer readiness from process liveness.

### 3.6 Pruned floor: the documented failure

After `events-journal-retain-days 0`, `events-journal-retain-rows 0`, and
`bd events prune --before 5`:

```
$ bd events tail --since 0 --limit 3        # human; stderr
Error: events journal truncated: checkpoint 0 is below the retained window
[5..10]; records 1..4 were pruned
Hint: resume with --since 4 to continue from the oldest retained record
(accepting the gap), or re-import from scratch
exit=1

$ bd events tail --since 0 --limit 3 --json  # stdout, pretty-printed
{
  "code": "events_journal_truncated",
  "error": "events journal truncated: checkpoint 0 is below the retained window [5..10]; records 1..4 were pruned",
  "floor": 5,
  "head": 10,
  "schema_version": 1,
  "since": 0
}
exit=1
```

`bd events export` fails identically (human error, exit 1). `--since floor-1`
(`--since 4`, floor 5) succeeds and returns `[5,6,...]`.

### 3.7 `bd serve`

```
$ bd serve --addr 127.0.0.1:0
Error: operation "serve" not supported by the embedded-dolt backend:
bd serve requires a Dolt SQL server; this workspace uses embedded Dolt
exit=1
```

`bd serve --help` documents `/healthz`, `/v0/beads/context`,
`/v0/beads/ready`, `/v0/beads/issues:sweep`, auth and Host allowlisting; it
documents no events endpoint. The design's deferral of an HTTP transport is
consistent with this; a definitive answer needs a Dolt SQL server deployment.

## 4. Findings that must change the design

1. **Sparse snapshot.** `record.issue` must be *merged* into the model (only
   present fields), never substituted. A delete tombstone is `issue:null`.
2. **Seven ops only.** `claim`/`reopen`/`status`/`label` are `update` rows; a
   label op may fan out to several `update` rows. Signal-level mapping must key
   off `update` plus the *field delta*, not off a distinct op.
3. **Derived cascade rows.** Closing/reopening a blocker emits actor-less
   `update` rows on dependents. The activity view must label these as derived
   (system) changes and not attribute them to a user.
4. **Audit constants mismatch.** `beads-event-*` (audit vocabulary) is not the
   journal op vocabulary; `beads-event.el` owns a distinct mapping.
5. **Journal-off detection.** Probe `bd config get events-journal`; do not rely
   on `tail` failing. The stderr note is the stream-side fallback signal.
6. **Actor contract.** `actor` is the sole provenance carried by the journal.
   There is no session/agent field; session attribution is derived and
   optional.
7. **`--json` error shape.** The truncated error with `--json` is a
   multi-line pretty object on stdout; the stream parser must tolerate this
   (detect `code`, or rely on the command layer's exit-code check before
   parsing).

## 5. Reproduction

```
TMPD=$(mktemp -d); cd "$TMPD"
env -u BEADS_DIR -u BEADS_DOLT_PORT -u BEADS_DOLT_SERVER_PORT -u GC_DOLT_PORT \
    -u BEADS_ACTOR -u GC_SESSION_NAME -u GC_SESSION_ID -u GC_AGENT \
    -u GC_TEMPLATE bd init
env ... bd config set events-journal true
# then the §1 sequence, then:
env ... bd events tail --since 0 --limit 100000
env ... bd events export
```

---

*Bead `be-ca9j` (plan `beads-events-live`), validation artifact. Planning
only; no `.el` source was changed.*
