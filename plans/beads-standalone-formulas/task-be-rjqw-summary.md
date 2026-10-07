---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-fmog
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
    - path: beads/be-rjqw
      hash: bead:be-rjqw
      ids:
        - REQ-SF-070
        - REQ-SF-071
        - REQ-SF-072
    - path: lisp/beads-handoff.el
      hash: sha256:41934d88e25482e980e87f2bc4434f5177ddf0dd736cb8c860d620974fe68d60
    - path: lisp/beads-command-prime.el
      hash: sha256:b6b9874a5bc42ead394540378273670e317bce8ff875e84f809f113b68e45f51
    - path: lisp/beads-command-misc.el
      hash: sha256:9997e8214858f62171520958fab7ca235fd7b726f8a54cf6f72b04c61a04021c
    - path: lisp/test/beads-handoff-test.el
      hash: sha256:cab494a37540685fb9d6e69cba43fe801407b78ec655a78219dfaca86092c423
    - path: lisp/test/beads-cli-sync-test.el
      hash: sha256:2c30f2dc79752243533248895dbbe76cbf8d1b689d939c902a8c23f57c183df9
  coverage:
    - id: REQ-SF-070
      status: covered
    - id: REQ-SF-071
      status: covered
    - id: REQ-SF-072
      status: covered
---

## Summary

WI-SF-11 adds the agent hand-off and operational-context surfaces. The new
module `lisp/beads-handoff.el` owns three things: the
`beads-handoff-envelope` cl-defgeneric seam that renders a molecule,
formula or issue into the issue-envelope user prompt and returns the
`(ISSUE SYSTEM-PROMPT USER-PROMPT)` triple accepted by the 4-arity
`beads-agent-backend-start`; the `beads-handoff` hand-off transient; and
the `beads-context` sectioned buffer that renders `bd prime`, persistent
memories, `bd setup` recipe status and the active `agent.profile`
policy.

The `beads-command-prime` class was split out of
`beads-command-misc.el` into `lisp/beads-command-prime.el` (one file per
subcommand, per the Wave 0 relocation). `bd prime` and `bd setup` emit
human-readable text (neither has a `--json` flag), so both classes are
now declared `:json nil` and `beads-command-execute` returns their raw
stdout. Because `beads-context` is the new view, the `bd context`
transient was renamed to `beads-bd-context` so the two symbols do not
collide; the command class and its CLI serialization are unchanged.

Implemented in the isolated worktree
`/home/roman/workspace/beads.el/worktrees/be-rjqw` (source anchor
`be-rjqw`) detached at `origin/main` `917280a`, committed as `f4d99d2`.

## Intended Behavior

- `beads-handoff-envelope` is a `cl-defgeneric`; its default method
  returns `(ISSUE SYSTEM-PROMPT USER-PROMPT)` where USER-PROMPT is the
  issue-envelope text naming the work (molecule/formula/issue plus the
  exact `bd` commands to continue) followed by the agent type's task
  template and an optional prime/memories context excerpt. Extensions
  (gascity) can override it (REQ-SF-070).
- `beads-handoff-start` resolves the named backend and agent type
  (default `Task`), builds the prompts and calls the 4-arity
  `beads-agent-backend-start`, so the hand-off reuses the existing
  backend registry. `beads-handoff-agent` is the interactive entry point
  and `beads-handoff` is the transient (issue, backend, agent type,
  worktree, context toggle, prompt preview, start).
- `beads-context` (plus `beads-context-prime` and `beads-setup-status`)
  opens `beads-context-mode` and shows the Prime section
  (`bd prime --full`), the Memories section
  (`bd prime --memories-only`), the Setup status section (the
  `bd setup --list` text plus a per-recipe `--check` classification) and
  the Policy section. `BD_AGENT_PROFILE` wins over the `agent.profile`
  config key; the view never writes policy (REQ-SF-071, REQ-SF-072).
- The context view is store-scoped: an explicit `directory` becomes a
  buffer-local `beads-store-directory`, which
  `beads-meta-build-global-options` turns into `--directory` for every
  read. `g` refreshes, `c` copies the prime text, `q` buries.
- `bd setup <recipe> --check` failures that exit non-zero are caught per
  recipe so one missing recipe cannot abort the whole context view.

## Changed Files

- `lisp/beads-handoff.el` (new): work plists, the
  `beads-handoff-envelope` generic, `beads-handoff-start`,
  `beads-handoff-agent`, the `beads-handoff` transient, and the
  `beads-context` view/formatting (`beads-context-mode`,
  `beads-context--collect`, `beads-context--setup-recipe-names`,
  `beads-context--setup-status-line`, `beads-context--policy`).
- `lisp/beads-command-prime.el` (new): the `beads-command-prime` class,
  relocated from `beads-command-misc.el` and declared `:json nil`.
- `lisp/beads-command-misc.el` (modified): removed the prime class and
  its autoload; requires `beads-command-prime`; declared
  `beads-command-setup` `:json nil`; renamed the `bd context` transient
  to `beads-bd-context` via `:transient :manual`.
- `lisp/test/beads-handoff-test.el` (new): 14 unit tests (envelope,
  work constructors, mock-backend hand-off, prime/setup formatting
  fixtures, policy, render/refresh) plus one `:integration` test that
  drives `beads-context` against a real `bd` temp repo.
- `lisp/test/beads-cli-sync-test.el` (modified): the `bd context`
  transient assertion now checks `beads-bd-context`.

## Verification

First verification command (unit suite for the new module):

```
eldev test -f beads-handoff-test.el
```

Observed: `Ran 15 tests, 15 results as expected, 0 unexpected`
(the 15th is the `:integration` context test against real `bd`).

Supporting runs, all from inside the worktree:

- `eldev test -f beads-command-misc-test.el -f beads-cli-sync-test.el -f beads-menu-test.el`
  -> `Ran 247 tests, 247 results as expected, 0 unexpected`.
- `eldev -p -dtT test '(not (tag :integration))'`
  -> `Ran 5832 tests, 5808 results as expected, 0 unexpected, 24 skipped`.
- `eldev -p -dtT compile` -> finished successfully, no warnings from
  the new files.
- `eldev -p -dtT lint` -> no warnings in `beads-handoff.el`,
  `beads-command-prime.el` or `beads-command-misc.el`.
- Live non-graphical acceptance: `beads-context "/home/roman/bright-lights"`
  run in a fresh `emacs -nw -Q` inside tmux (local rig, worktree `lisp/`
  on the load path) rendered `▾ Prime (bd prime)`, `▾ Setup status` and
  `▾ Policy`, policy=`conservative`, prime-length=5537, 14 recipes.

Final proof command:

```
GC_BEAD_ID=be-5wm1 /home/roman/workspace/beads.el/.gc/scripts/checks/build-artifact-valid.sh
```

Observed pass after recording this summary path on workflow root
`be-fmog` (`build artifact valid: schema=gc.build.implementation-summary.v1`).

Known unrelated failures in this environment: the live `bd` is 1.3.1
rather than the CI-pinned 1.0.5, so `beads-audit-gate-no-new-slot-drift`
reports pre-existing drift (`list --include-ephemeral`, `purge --limit`,
`purge --wisps-plane`); none of those paths are touched by this change.
`beads-main-test` has seven pre-existing environment failures, confirmed
identical on the base commit.

| ID | Status |
| --- | --- |
| REQ-SF-070 | covered |
| REQ-SF-071 | covered |
| REQ-SF-072 | covered |

## Remaining Risks

- Ambiguity resolved by decision: the implementation plan names
  `M-x beads-context` as the context view, but `beads-context` was
  already the auto-generated `bd context` transient. To keep both
  surfaces, the new view keeps `beads-context` and the CLI transient was
  renamed `beads-bd-context` (class and command line unchanged). The
  menu's "Context" entry now opens the richer view; `bd context` output
  is not yet rendered as a section of it.
- `beads-context--collect` is synchronous. The design's async table lists
  the context view on `beads-command-execute-async` with a
  `:cache-key`; this implementation reads synchronously so the
  per-recipe `bd setup --check` fan-out is easy to mock and fixture-test.
  Moving it to the async aggregator is deferred to the integration/wiring
  stage (WI-SF-12) together with the molecule view.
- `bd setup <recipe> --check` output is human text (plan-review F9), so
  the status classification is heuristic (`installed`/`stale`/`missing`,
  falling back to the first non-empty line); it upgrades automatically if
  `bd setup --json` appears.
- The hand-off starts the backend directly (the documented 4-arity
  seam). Session registration/buffer renaming through
  `beads-agent--start-backend-async` and the prompt editor are not part
  of this focused path; the molecule/formula views (WI-SF-01/WI-SF-05)
  will wire `a` to `beads-handoff` with their work objects.
