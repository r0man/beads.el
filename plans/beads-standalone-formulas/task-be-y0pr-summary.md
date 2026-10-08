---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-uptc
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
    - path: beads/be-y0pr
      hash: bead:be-y0pr
      ids:
        - REQ-SF-099
        - REQ-SF-098
    - path: lisp/beads-types.el
      hash: sha256:cccfd7d62c314103037d59eac1729adf14843ee2201fb63c322bfe98e9cc5658
    - path: lisp/beads-command-swarm.el
      hash: sha256:8731751fd131ec8c532b992960a99a141c8b0a8ecfbfde7222b40b1cf46f01f5
    - path: lisp/test/beads-command-swarm-test.el
      hash: sha256:2461a5f4aafeb0783bc2fe862f6267c8b0c5a45178d2e2ef6db1a0eb4a95da07
  coverage:
    - id: REQ-SF-099
      status: covered
    - id: REQ-SF-098
      status: covered
---

## Summary

WI-SF-15 (REQ-SF-099, REQ-SF-098 domain states). The four
`beads-command-swarm-*` classes previously returned raw JSON strings because
they had no `:result`; the swarm porcelain would have needed a second
parser. This change adds typed swarm result classes to `beads-types.el`,
declares `:result` on the four command classes, unwraps the
`bd swarm list --json` `swarms` envelope in the command's parse method, and
adds `beads-swarm-domain-error-p`, the single detector for the exit-0
`{error: ...}` domain payloads of `bd swarm create` / `bd swarm validate`.

Coverage:

| ID | Status |
| --- | --- |
| REQ-SF-099 | covered |
| REQ-SF-098 | covered |

## Intended Behavior

- `beads-types.el` gains six swarm result classes with documented
  slots and `beads-from-json` support: `beads-swarm-list-item` (fleet list
  rows), `beads-swarm-status-issue` (one status-board row),
  `beads-swarm-status` (parsed `bd swarm status --json` payload with its
  four groups and reported counts), `beads-ready-front` (one ready
  front/wave), `beads-swarm-issue-node` (one `--verbose` validate graph
  node), and `beads-swarm-analysis` (the validate payload and create's
  embedded analysis). `beads-swarm-analysis` has a custom
  `beads-from-json` method because its `issues` key is a map of
  issue-id to node, not an array; it is converted to a list of
  `beads-swarm-issue-node` objects. A `beads-swarm-create-result` envelope
  carries the create-specific `swarm_id`/`coordinator`/`epic_id` plus the
  domain-error fields (`error`, `existing_id`, `existing_title`).
- `beads-command-swarm.el` declares `:result`: create ->
  `beads-swarm-create-result`, list -> `(list-of beads-swarm-list-item)`,
  status -> `beads-swarm-status`, validate -> `beads-swarm-analysis`. The
  list command overrides `beads-command-parse` only to unwrap the CLI's
  `swarms` envelope and coerce each entry; it does not re-parse JSON.
- `beads-swarm-domain-error-p` accepts the raw JSON alist or the typed
  result and returns the domain-error string for `swarm already exists`,
  `epic is not swarmable`, and the `swarmable=false` validate state
  (REQ-SF-098), else nil. It is the only place a domain payload becomes a
  user-facing error.
- No command slot changed. The existing `beads-swarm` transient and the
  four classes' CLI serialization/validation are untouched.

## Changed Files

- `lisp/beads-types.el` - new Swarm Types section: six typed result
  classes, a `beads-swarm-create-result` envelope, their
  `-from-json` wrappers, and the map-aware `beads-from-json` method for
  `beads-swarm-analysis`.
- `lisp/beads-command-swarm.el` - `:result` declarations on the four
  swarm command classes, the `bd swarm list` envelope-unwrapping
  `beads-command-parse` override, and `beads-swarm-domain-error-p`.
- `lisp/test/beads-command-swarm-test.el` - 14 new unit tests: result
  declarations, list/status/validate/create parsing fixtures (including
  ready-front wave math and the verbose graph), and the domain-error
  detector (alist and typed, match and non-match).
- `NEWS.md` - Unreleased entry for the typed swarm command results.

## Verification

First verification command (the bead's expectation):

```
eldev test -f beads-command-swarm-test.el
```

Result: 22/22 tests passed, 0 unexpected (the file retains its eight
original command-line/validation tests plus the 14 added here).

Intermediate gates, all from the worktree:

```
eldev compile
eldev -p -dtT lint
eldev test '(not (tag :integration))'
```

Result: `eldev compile` exit 0 with no warnings; `eldev -p -dtT lint` exit 0
with no warnings (checkdoc initially flagged one docstring, fixed); the unit
suite ran 5832 tests, 5808 expected, 0 unexpected, 24 skipped (live).

Final proof command (run from the launcher rig root
`/home/roman/workspace/beads.el`):

```
GC_BEAD_ID=be-zt1t .gc/scripts/checks/build-artifact-valid.sh
```

Result: `build artifact valid: schema=gc.build.implementation-summary.v1`
(exit 0).

## Remaining Risks

- REQ-SF-098 is covered here only for the payload/domain-state layer
  (the domain-error detector and `swarmable=false`). The swarm fleet list,
  status board, worker lanes and validate/waves rendering are WI-SF-16 and
  WI-SF-17; live evidence for those renderings is gated there, so this
  work item ships unit-level parse/detector evidence only.
- The generated transient for `beads-command-swarm-status` is named
  `beads-swarm-status`, the same symbol as the EIEIO constructor for the
  `beads-swarm-status` result type. The transient is the intended
  definition; the class constructor is shadowed. The `beads-defcommand`
  form is wrapped in `with-no-warnings` with an explanatory comment so
  `eldev compile` stays warning-free. Objects are still created via
  `make-instance`/`beads-from-json`, so no code path needs the constructor.
- `beads-swarm-domain-error-p` lives in `beads-command-swarm.el`
  (per this work item's file list and the implementation-plan's
  "additive to `beads-command-swarm.el`" note) rather than
  `beads-swarm.el` (design.md §5.4). `beads-swarm.el` does not exist until
  WI-SF-16; WI-SF-16 can require `beads-command-swarm` and reuse the
  detector unchanged.
- The `issues` map in create's embedded analysis is parsed even when
  `--verbose` was not passed, because `bd swarm create` includes it in
  v1.3.x; a future bd that omits it parses to a nil node list.
