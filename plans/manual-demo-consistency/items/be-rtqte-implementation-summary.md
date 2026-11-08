---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-b9jhp
  formula: do-work
methodology:
  pack: gascity
  name: build-basic
producer:
  formula: do-work
  stage: implement
  attempt: 1
status: approved
trace:
  upstream:
    - path: beads/be-rtqte
      hash: bead:be-rtqte
      ids: [REQ-001, REQ-002, REQ-006]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: docs/manual/screenshots/fixtures/beads-fixture.jsonl
      hash: sha256:6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8
    - path: docs/manual/screenshots/fixture.sh
      hash: sha256:12c327e971f7b4cdbb7749b84b17f86a8daaf475e35f9a014a6bc0d0e8dd0bf6
    - path: docs/manual/screenshots/capture.sh
      hash: sha256:9e31517f3b45a2ce63e824b93c4df3ab3742f45e3dd00879dc3253f8a5f65def
    - path: docs/manual/screenshots/README.md
      hash: sha256:d181e5cd82faa2337e534c3ca6c4b3b6edcf624ebe04c8bb8bef302300b790d9
    - path: doc/installation.texi
      hash: sha256:c8c474f630ac982826c8745673436639e0dc5c1af4647bed8db47443b404d4a8
    - path: lisp/test/beads-manual-test.el
      hash: sha256:406d825f54a011a570b457098de0a26860d59e84a1997b58604b217cc117618e
  coverage:
    - id: REQ-001
      status: covered
    - id: REQ-002
      status: covered
    - id: REQ-006
      status: covered
---

# Implementation Summary: W1 shared `/tmp/demo-project` fixture

## Summary

Work item W1 (implementation plan WS-1) retargets the canonical screenshot
fixture from a private per-run `mktemp` parent
(`${TMPDIR:-/tmp}/beads-fixture.run.XXXXXXXX/beads-fixture.project`) to the
shared cross-repo contract project at `${TMPDIR:-/tmp}/demo-project`, with the
fixed basename `demo-project` so buffer and mode-line text stay stable.

The frozen dataset is untouched: the twelve `demo-1` … `demo-12` issues and
their ids, statuses, dependencies, labels, comments, and pinned timestamps are
exactly as committed, and the single `FIXTURE_DATA_SHA256` literal in
`fixture.sh` still equals the file digest. Because the path is now fixed and
machine-shared, the lifecycle is marker-guarded and advisory-`flock`
serialized, and the fixture path, dataset hash, and isolation contract are
recorded in the manual and the screenshot README so `fixture.sh verify` fails
if either document drifts from the data.

## Intended Behavior

- **Fixed path.** `fixture.sh create` seeds and prints
  `${TMPDIR:-/tmp}/demo-project` (default `/tmp/demo-project`).
- **Marker-guarded `create`.** A stale fixture at that path is deleted and
  recreated only when it carries `.beads-fixture-project`; any other directory
  or file there is refused with a non-zero exit and nothing is deleted. A
  seeding failure still removes the half-built project. `remove_fixture_dir`
  no longer touches a run parent, because there is none.
- **Strict `teardown`.** Only the exact shared path (a trailing slash is
  trimmed) is accepted, the marker is required, a non-directory at that path is
  refused, and an already-absent project is an idempotent success.
- **Serialization.** `fixture.sh create|teardown|verify` take an advisory
  `flock` on `${TMPDIR:-/tmp}/demo-project.lock` (next to, never inside, the
  project); `capture.sh` takes the same lock and holds it for the whole capture
  run with a bounded wait (`BEADS_SHOT_LOCK_TIMEOUT`, default 1800 s) and a log
  line. `capture.sh` exports `FIXTURE_LOCK_HELD=1` after acquiring it, so the
  nested `fixture.sh` calls it makes skip acquisition instead of deadlocking
  against their own parent; a direct CLI call without the marker still locks
  itself. A second concurrent capture waits rather than sharing the store or
  its `.beads.gate.lock`.
- **Dataset hash gate.** `FIXTURE_DATA_SHA256` is the single literal
  (`6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8`).
  `verify` hashes `fixtures/beads-fixture.jsonl` against it, then requires the
  same value in `docs/manual/screenshots/README.md` and in the manual's shared
  demo-project Texinfo section. The structural contract (role ids
  `^demo-[0-9]+$`, no auto-generated id, every dependency target present, the
  open/in-progress/blocked/closed set, an epic with ≥ 2 parent-child children,
  labels, comments) is unchanged.
- **`capture.sh`.** `ensure_project`/`teardown_project` use the fixed path and
  lock. `BEADS_SHOT_PROJECT` remains the explicit reuse escape hatch: a reused
  path is never locked and never torn down. Header comment and `--help`
  document the shared path, the lock, and the timeout.
- **Isolation unchanged.** The four-name unset list and
  `print-isolation-vars` are byte-identical in effect, so
  `beads-manual-isolation-list-matches-fixture` keeps proving the drift
  contract.
- **Manual + README.** The manual gains an `Installation` section, "The Shared
  Demo Project", naming the path, the marker/lock lifecycle, the dataset hash,
  and the isolation rules; the README replaces the per-run path text and
  records the lock and hash responsibilities. WS-6 links and expands this
  section rather than re-deriving it.

### Coverage

| ID | Status |
| --- | --- |
| REQ-001 | covered |
| REQ-002 | covered |
| REQ-006 | covered |

## Changed Files

| File | sha256 | Change |
| --- | --- | --- |
| `docs/manual/screenshots/fixture.sh` | `12c327e971f7b4cdbb7749b84b17f86a8daaf475e35f9a014a6bc0d0e8dd0bf6` | fixed project path, marker-guarded recreate/teardown, advisory lock with nested-lock guard, dataset-hash gate in `verify`, docs/hash checks, updated usage. |
| `docs/manual/screenshots/capture.sh` | `9e31517f3b45a2ce63e824b93c4df3ab3742f45e3dd00879dc3253f8a5f65def` | shared path constants, whole-run lock with bounded wait and `FIXTURE_LOCK_HELD` export, fixed-path assertions in `ensure_project`, `fatal` helper, header/`--help` text. |
| `docs/manual/screenshots/README.md` | `d181e5cd82faa2337e534c3ca6c4b3b6edcf624ebe04c8bb8bef302300b790d9` | shared path and lock contract, dataset hash + single-literal rule, `flock` requirement, env table, reuse escape hatch. |
| `doc/installation.texi` | `c8c474f630ac982826c8745673436639e0dc5c1af4647bed8db47443b404d4a8` | new "The Shared Demo Project" section: path, marker/lock lifecycle, dataset hash, isolation, cross-repo contract pointer. |
| `lisp/test/beads-manual-test.el` | `406d825f54a011a570b457098de0a26860d59e84a1997b58604b217cc117618e` | shared-path/hash contract tests, markerless-refusal and teardown path-guard tests, lock serialization + nested-guard tests, capture lock-wait test, hermetic `TMPDIR` scoping for the seeded-store integration test. |

## Verification

First verification command (targeted unit gate, run from the worktree):

```
guix shell -D -f guix.scm -- emacs --batch -Q -L lisp -L lisp/test \
  -l lisp/test/beads-manual-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
```

Observed: **PASS** — `Ran 20 tests, 20 results as expected, 0 unexpected`.

Targeted shell/integration gate for this file (same load path, selector
`(tag :integration)`): **PASS** —
`Ran 6 tests, 6 results as expected, 0 unexpected`, including
`beads-manual-fixture-serializes-on-shared-lock`,
`beads-manual-capture-waits-on-shared-lock`,
`beads-manual-fixture-export-has-no-generated-ids`.

Bead proof commands (from the worktree, `guix shell -D -f guix.scm`):

| Proof | Command | Observed |
| --- | --- | --- |
| fixed path | `docs/manual/screenshots/fixture.sh create` | exit 0, prints `/tmp/demo-project`, marker `.beads-fixture-project` present |
| structural + hash verify | `docs/manual/screenshots/fixture.sh verify` | exit 0, `fixture.sh: verify: 12 role-keyed issues OK`, `fixture.sh: PASS` |
| stale marked path recreated | create, add `STALE`, create again | exit 0, stale file gone, project re-seeded |
| markerless path refused | `mkdir /tmp/demo-project; fixture.sh create` | exit 1, "refusing to remove … exists without .beads-fixture-project", `keep.txt` still present |
| idempotent teardown | `fixture.sh teardown /tmp/demo-project/` twice | exit 0, "already gone" |
| repo store untouched | `stat -c %Y .beads` before/after a create+verify+teardown | unchanged (`1794153401` both) |
| isolation vars unset | create with `BEADS_DIR=<hostile dir>`, `BEADS_DOLT_PORT=9`, `BEADS_DOLT_SERVER_PORT=9`, `GC_DOLT_PORT=9` | exit 0, hostile dir still empty, fixture store under `$TMPDIR` |
| concurrent waits | hold `flock` on the lock file; `fixture.sh teardown` (1 s timeout) then with `FIXTURE_LOCK_HELD=1` | independent caller exits non-zero with "timed out"; nested caller exits 0 immediately |
| second capture waits | hold `flock`; `capture.sh --scope list` with `BEADS_SHOT_LOCK_TIMEOUT=1` | exits non-zero with "timed out … waiting for another capture", no project created |
| formula contract | `formula-verify.sh list` with a deterministic fake engine and a private `TMPDIR` | exit 0, byte-identical, project created and torn down, `formula-verify.sh` unchanged |

Project gates (all from the worktree, `guix shell -D -f guix.scm`):

| Gate | Result |
| --- | --- |
| `make` (byte-compile, warnings-as-errors) | **PASS** — `Byte-compiled 129 file(s)`; only pre-existing third-party `sly` autoload warnings |
| `make lint` | **PASS** (exit 0) |
| `make html` | **PASS** (exit 0, no Texinfo warnings) |
| `makeinfo --no-split -o /tmp/beads-check.info doc/beads.texi` | **PASS** (exit 0, no warnings; new node resolves) |
| `scripts/check-manual-forbidden` | **PASS** (exit 0) |
| `make check` (`check-manual` + full ERT) | **PASS** — `Ran 6691 tests, 6665 results as expected, 0 unexpected, 26 skipped` (the 26 are pre-existing `:live` skips) |

Final proof command (the workflow gate, re-run after the last source edit):

```
guix shell -D -f guix.scm -- make check
```

Observed: **PASS** — exit 0, `Ran 6691 tests, … 0 unexpected`. The run happened
while the sibling `gascity.el` fixture was live at the real `/tmp/demo-project`,
which is why the seeded-store integration test scopes `TMPDIR` to a private
directory: `create` correctly refuses a foreign marker instead of deleting it.

## Remaining Risks

- **`flock` is now a hard requirement** for `fixture.sh create|teardown|verify`
  and for `capture.sh` (Linux/util-linux `flock`). macOS and other hosts
  without util-linux `flock` will fail rather than run unserialized. That is
  the intended trade-off for a single fixed path; the README lists the tool.
- **Cross-repo marker naming.** `beads.el` uses `.beads-fixture-project`; the
  sibling `gascity.el` fixture currently at the shared path uses
  `.gascity-fixture-project`. Sequential captures interoperate (each run tears
  its own fixture down while holding the lock, so the next run finds the path
  gone), but a *leftover* sibling fixture makes `fixture.sh create` refuse with
  a clear error instead of deleting it. That refusal is the specified safety
  behavior; docs state it. If the two manuals want a single marker name, that
  is a cross-repo decision outside this item.
- **Shared machine state.** Integration tests now exercise the fixed path
  through a private `TMPDIR`; the literal `/tmp/demo-project` proof was
  observed on this host before the sibling fixture appeared. Two `make check`
  runs on one host serialize on the same lock (bounded by 600 s), so a very
  long concurrent capture can make the test suite wait rather than fail.
- **Theme work is not in this item.** `capture.sh`'s header and the README
  requirements table still name `ef-themes`, and the manifest still uses the
  `ef-*` theme names; WS-2/WS-3 replace those. The lock/`-light` slot
  interaction is unaffected by this item.
- **`AGENTS.md` still describes the fixture as `mktemp`-based** (line ~266).
  It is outside WS-1's file boundary; WS-8 (tests/gates/NEWS/docs sweep) should
  update it.
- **`verify`'s hash-to-doc check scans all `doc/*.texi`** rather than one fixed
  file, so moving the section between chapters keeps it green. It cannot tell
  a hash quote in the wrong chapter from the right one; the WS-6 walkthrough
  edit should keep the quote in the shared demo-project section.
