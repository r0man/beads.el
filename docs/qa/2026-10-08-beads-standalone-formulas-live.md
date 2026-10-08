# beads-standalone-formulas — live acceptance (Emacs-in-tmux, bright-lights)

- Date: 2026-10-08
- Plan: `plans/beads-standalone-formulas/` (WI-SF-01..19), branch
  `standalone-formulas-integration` @ `7422bbc` (draft PR #68)
- Build under test: integration worktree `beads-sf`, Emacs 31.1, loaded from
  the worktree `lisp/` + the eldev `vui-1.4.0` / `sesman-0.3.2` package dirs.
- Method: `emacs -nw -Q` in a dedicated tmux session (`beads-sf`) with a
  `beads-sf` server; driven with `emacsclient --eval`. Cities: local
  `/home/roman/bright-lights` and TRAMP
  `/ssh:localhost:/home/roman/bright-lights`.

## Results

| Surface | Result | Evidence |
|---|---|---|
| Load / no gascity | **pass** | `(featurep 'gascity)` nil; all new entry points fbound. |
| Formula browser (WI-SF-05) | **pass** | `*beads-formula-list[bright-lights]*` renders with the **Phase** column and **Source** path (`build-base … workflow 11 18 … ~/bright-lights/.beads/formulas/build-base.formula.toml`). |
| Molecule view (WI-SF-01) | **pass** | `*beads-molecule[bright-lights]/bl-mol-p1m*`: header (phase, assignee, 0/1, rate/ETA), Ready frontier/Blocked/Gated, step tree, Gates/Output sections; live step `ready`. |
| Molecule claim (WI-SF-02) | **pass** | `beads-molecule-claim` on `bl-mol-53b`: view flips `ready` → `current`, assignee `controller`; `bd show` confirms `status in_progress`. |
| Molecule close (WI-SF-02) | **pass** | `beads-molecule-close` with reason: view `current` → `done`, 1/1 (100%); `bd show` confirms `status closed`, `close_reason "live acceptance"`. |
| Gate list (WI-SF-06) | **pass (empty state)** | `*beads-gates[bright-lights]*` opens and renders the empty store. |
| Wisp list (WI-SF-07) | **pass (empty state)** | `*beads-wisps[bright-lights]*` opens and renders the empty store. |
| Swarm validate / waves (WI-SF-17) | **pass** | `*beads-swarm-waves[/home/roman/bright-lights/]/bl-732y*`: `Swarmable: yes`, `Max parallelism: 1`, `Estimated sessions: 2`, `Wave 0: bl-732y.1`, `Wave 1: bl-732y.2`, Warnings/Errors sections. |
| Swarm fleet list (WI-SF-16) | **pass (empty state)** | `*beads-swarm-list[bright-lights]*` opens empty (no swarms). |
| Context view (WI-SF-11) | **pass** | `*beads-context[bright-lights]*`: `Context — bright-lights (policy: conservative)` + Prime output. |
| Formula authoring (WI-SF-09) | **pass** | `beads-formula-new "sf-acceptance" "workflow" "project"` scaffolds `.beads/formulas/sf-acceptance.formula.toml` and opens it in `conf-unix-mode` (no `toml-mode` installed; expected fallback). |
| TRAMP parity (local ↔ `/ssh:localhost:`) | **pass** | `*beads-molecule[/ssh:localhost:\|bright-lights]/bl-mol-p1m*` renders the same view as local; TRAMP formula browser also renders. |

## Notes

- **`bd swarm create`/`list` are unsupported in bright-lights** (its `bd` runs
  in proxied-server mode: `proxy.swarm.unsupported`), so the create/list/
  coordinator round-trip cannot be exercised there. `swarm validate` works.
  The swarm create→status→claim→close round-trip is covered by the
  `:integration` test `beads-swarm-test-integration-roundtrip` against a
  temporary `bd init` repo (passing).
- Acceptance artifacts (`bl-mol-p1m`, `bl-732y` + children, the scaffolded
  formula) were deleted afterwards.
- Lint (`eldev -p lint`) has no complaints on this tip.

## Outstanding

- UI gateway surfaces (the `beads-gate`/`beads-wisp` transient keys, `beads-bond`
  transient, `beads-cook` transient) were exercised by their unit tests, not by
  live keypresses; the populated list states were not produced in bright-lights.
- `beads-audit-gate-no-new-slot-drift` fails locally against the installed
  `bd 1.3.1-dev` (unrelated to this branch; passes in CI on `bd v1.3.0`).
