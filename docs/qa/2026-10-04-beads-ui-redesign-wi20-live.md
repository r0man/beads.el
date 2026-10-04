# beads.el UI redesign — WI-20 live acceptance (Emacs-in-tmux, bright-lights)

- Date: 2026-10-04
- Bead: `be-1iu7` (build-from-plan) / plan `plans/beads-ui-redesign/` (WI-20)
- PR: #67 (`feat(ui): beads.el UI redesign`)
- Build under test: `wi18-integration` @ `5fd1aa9` (worktree
  `worktrees/be-73vg`), Emacs 31.1, transient 0.13.8, `emacs -nw -Q` in tmux
  (`beads-e2e`), beads loaded from the worktree `lisp/` + the eldev vui/sesman
  package dirs.
- Cities: local `/home/roman/bright-lights` and TRAMP
  `/ssh:localhost:~/bright-lights`.

## Results

| Surface | Result | Evidence |
|---|---|---|
| Status buffer (local) | **pass** | `*beads-status<bright-lights>*` renders `Beads — bright-lights`, sections In progress (0) / Ready (34) / Blocked (14). |
| Status buffer (TRAMP) | **pass** | `*beads-status</ssh:localhost:\|bright-lights>*` renders the same sections over `/ssh:localhost:~/bright-lights` (REQ-019 parity). |
| Sling — cold entry | **pass (after fix)** | Initially crashed `transient-setup` (see Defects). Now renders the What/Who/Actions groups + live footer. |
| Sling — full preview | **pass** | `*beads-sling: preview*` with `Validation`, `Recipe — none picked (plain dispatch)`, `Routing plan (dry run)`. |
| Dispatch menu (`?`) | **pass** | Opens with Maintenance/refresh/quit groups. |
| List prefix | **pass** | Opens; `l All issues`, `-n`/`-r` options. |
| Detail (`beads-show`) | **pass** | `*beads-show[bright-lights]/bl-pa2 hello*`: sections, action bar (`d/C/s/#/e/S/c/w/?`), nav hints `q bury · g refresh · ? dispatch`. |
| Formula browse | **pass** | `*beads-formula-list[bright-lights]*`: `Found 45 formulas`. |
| Terminal attach | **pass** | `beads-terminal-attach` (moved into beads.el) opened `*beads-agent-beads-scroll*` in `ghostel-mode` against a disposable tmux session on the bright-lights socket. |
| Agent launch | **partial** | `beads-agent-start` and the backend registry exist and list the post-slimming backends (`claude-code-ide agent-shell eca pi claude claudemacs claude-code mock`). A full session launch was not exercised to completion: the harness invocation ran outside a list/detail store context and failed to fetch the issue (`Cannot start agent: failed to fetch issue bl-pa2`). Needs an in-buffer pass. |
| Formula launch / follow | **partial** | Browse + the sling formula shape/preview verified; an actual formula launch/follow was not exercised in this pass. |

## Defects found and fixed during the pass

1. **`be-d9ht` (P1, fixed `c628c20`)** — `M-x beads-sling` cold entry crashed
   `transient-setup` ("Need command …; got #[…]"): `beads-sling--children-specs`
   put one-arg lambda descriptions in the What/Who suffix slots. Fixed by moving
   the dynamic descriptions onto the pick commands; an ERT that actually parses
   the transient via `transient-setup` was added. Re-verified live.
2. **`be-ka4s` (P1, fixed `5fd1aa9`)** — the full `eldev -p -dtT test` suite
   hung indefinitely on `beads-live-test-list-bulk-close`:
   `beads-list-mark-all` looped forever on section headers in the grouped list.
   Fixed by advancing one entry per iteration and skipping headers. The suite
   now completes (~7 min) instead of hanging 6h.

## Notes / observations

- First status-buffer open showed a one-time ~2-min CPU spike (90%) that did not
  reproduce on subsequent opens; not filed.
- CI: on `5fd1aa9` the Test workflow concludes **success** (30.2/31.1/snapshot
  pass; 29.4 fails with the known EIEIO/transient `void-function closure`
  issue — `continue-on-error` per `test.yml`). Lint, coverage, claude-review,
  codecov pass. GitHub still reports `mergeStateStatus=BLOCKED` (29.4 shows as a
  failed check in the rollup).

## Outstanding for a full WI-20 sign-off

- Agent launch: start + attach a real agent session from list/detail (in-buffer),
  backend selection, prompt preview, session lifecycle.
- Formula launch + follow: pick, vars, launch, follow the created root.
