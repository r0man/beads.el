---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-v63z
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
    - path: beads/be-1sab
      hash: bead:be-1sab
      ids: [REQ-005, REQ-006]
    - path: docs/manual/screenshots/fixture.sh
      hash: sha256:6ddaf8ba6fc6b27269a49f6b30d201f5eb13849d2c8c2fa50f948a53d7318ca7
    - path: docs/manual/screenshots/fixtures/beads-fixture.jsonl
      hash: sha256:6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8
  coverage:
    - id: REQ-005
      status: covered
    - id: REQ-006
      status: covered
---

# Implementation Summary: W1 deterministic screenshot fixture and store isolation

## Summary

Implements work item W1 (implementation plan section WI-3) for the
`user-manual-screenshots` pipeline: a deterministic, isolated throwaway beads
project and the role-keyed fixture that seeds it. The two artifacts are
`docs/manual/screenshots/fixture.sh` and
`docs/manual/screenshots/fixtures/beads-fixture.jsonl`.

The fixture never touches the repository `.beads` store or any live/agent
store. `fixture.sh` hardcodes the four isolation variables and unsets them
before any `bd` call, then creates a fresh `mktemp -d` git project, runs
`bd init --prefix demo`, and imports the fixture. Every id, owner, dependency
timestamp, and comment timestamp in the fixture is pinned, so two `create`
runs export byte-identical JSONL. Role ids are `demo-1` through `demo-12` and
no auto-generated hash appears anywhere in the fixture file.

| ID | Status |
| --- | --- |
| REQ-005 | covered |
| REQ-006 | covered |

## Intended Behavior

- `fixture.sh create` (aliases `up`, and the no-argument default) creates a
  throwaway project under `${TMPDIR:-/tmp}/beads-fixture.XXXXXX`, initializes
  git and an embedded-Dolt beads store with `bd init --prefix demo`, imports
  `fixtures/beads-fixture.jsonl`, and prints only the absolute project path on
  stdout. All `bd` output goes to stderr so the path can be captured with a
  command substitution.
- Store isolation (plan-review P2): `BEADS_DIR`, `BEADS_DOLT_PORT`,
  `BEADS_DOLT_SERVER_PORT`, and `GC_DOLT_PORT` are unset before any `bd` call
  and are never re-exported, so a Gas Town shell that exports them cannot
  reroute `bd` at the production rig store. `print-isolation-vars` prints the
  hardcoded list, one name per line, so the W7 test can assert it equals
  `beads-test-isolation-env-vars` without scraping shell source.
- Determinism (REQ-006): the JSONL pins every issue id (`demo-1`..`demo-12`),
  `owner`, `created_by`, `created_at`, `updated_at`, `started_at`,
  `closed_at`, dependency `created_at`/`created_by`, and comment
  `created_at`. Roles cover ready, in-progress, blocked (explicit status), a
  `blocks` dependency chain, closed, an epic with two `parent-child` children,
  labeled issues, and a commented issue. No run-generated id appears in the
  fixture or in the exported store.
- `fixture.sh teardown <dir>` removes a created project. It refuses any path
  outside the temp prefix and any directory that lacks the
  `.beads-fixture-project` marker that `create` writes, so a stray argument
  cannot `rm -rf` a real checkout.
- `fixture.sh verify` creates a project, asserts unique role ids, that every
  dependency target exists, that all four statuses are present, that an epic
  has at least two parent-child children, and that a labeled and a commented
  issue exist, then tears the project down and prints `PASS`.

## Changed Files

- `docs/manual/screenshots/fixture.sh` — create/teardown/verify plus the
  hardcoded isolation list and `print-isolation-vars`. Mode 100755.
- `docs/manual/screenshots/fixtures/beads-fixture.jsonl` — 12 role-keyed
  issues, keyed by stable `demo-N` ids, with pinned metadata, labels,
  dependencies, and comments.

## Verification

First verification command (run from the worktree):

```bash
docs/manual/screenshots/fixture.sh verify
```

Observed: `fixture.sh: verify: 12 role-keyed issues OK` and
`fixture.sh: PASS` (exit 0).

Final proof command — two independent creates under a leaked Gas Town
environment, then a byte comparison of the exported stores:

```bash
export BEADS_DIR=/home/roman/workspace/beads.el/.beads \
       BEADS_DOLT_PORT=9999 BEADS_DOLT_SERVER_PORT=9998 GC_DOLT_PORT=9997
D1=$(docs/manual/screenshots/fixture.sh create)
D2=$(docs/manual/screenshots/fixture.sh create)
(cd "$D1" && bd export | sort) > /tmp/fixture-1.jsonl
(cd "$D2" && bd export | sort) > /tmp/fixture-2.jsonl
diff /tmp/fixture-1.jsonl /tmp/fixture-2.jsonl
docs/manual/screenshots/fixture.sh teardown "$D1"
docs/manual/screenshots/fixture.sh teardown "$D2"
```

Observed: `diff` reported no differences (`EXPORTS IDENTICAL`); both exports
contained 12 role-keyed issues; `bd ready` listed 7 issues and `bd blocked`
listed the 3 dependency-blocked issues; `bd comments demo-1` returned the two
pinned comments; and both throwaway directories were removed. The
`print-isolation-vars` output is exactly `BEADS_DIR`, `BEADS_DOLT_PORT`,
`BEADS_DOLT_SERVER_PORT`, `GC_DOLT_PORT` in that order.

## Remaining Risks

- W1 owns only the fixture and its isolation; the screenshot capture itself
  (WI-2) is out of scope, so the fixture is validated by `bd export` and
  `bd list` rather than by a rendered image.
- The fixture assumes `bd import` continues to express labels, comments, and
  `blocks`/`parent-child` dependencies. `fixture.sh` documents the annotation
  hook to use if a future `bd` release drops one of those fields; `verify`
  would fail loudly in that case.
- `verify` depends on `python3` for the JSONL assertions; the create/teardown
  paths do not.
- The committed fixture pins absolute timestamps (2025-01-01 and later) so
  relative-time rendering is stable; if the UI starts rendering absolute local
  times, the timezone of the capture host could still introduce drift and
  would need to be pinned by the capture stage.
