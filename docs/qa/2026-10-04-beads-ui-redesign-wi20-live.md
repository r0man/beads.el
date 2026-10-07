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
| Agent launch | **pass (after fix)** | From the list in non-git bright-lights, `beads-agent-start` (mock backend) reached the prompt editor and, confirmed with `C-c C-c`, started the session: `Started Task agent session bl-rpq#1 on bl-rpq` (mock `sessions=1, start-calls=1`). Initially failed — see `be-kw9o`. |
| Formula browse + launch | **pass** | `beads-formula-list` (45) and `beads-formula-show e2e-demo` (`*beads-formula[bright-lights]/e2e-demo*`, Description/Variables/Launch inputs). `beads-formula-launch-standalone e2e-demo` (var `note=wi20-live`) logged `Formula launched` and created a real wisp in bright-lights: root `bl-mol-aob` + latch `bl-mol-kfh` (disposable; deleted after the pass). |

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
3. **`be-kw9o` (P1, fixed `288973c`+`5cb1527`)** — agent launch failed in
   non-git beads projects/cities: `beads-git-find-project-root` is nil in the
   bright-lights city, so `project-dir` reached `default-directory` as nil
   (`stringp nil`), mislabelled by the fetch wrapper as "Failed to parse
   issue". Fixed with a `beads-agent--project-root` fallback (git → non-git
   `.beads` marker walk → `default-directory`), a nil guard, tightened
   error-scoping, and a worktree skip outside git. Re-verified live above.

## Notes / observations

- First status-buffer open showed a one-time ~2-min CPU spike (90%) that did not
  reproduce on subsequent opens; not filed.
- CI: on `5fd1aa9` the Test workflow concludes **success** (30.2/31.1/snapshot
  pass; 29.4 fails with the known EIEIO/transient `void-function closure`
  issue — `continue-on-error` per `test.yml`). Lint, coverage, claude-review,
  codecov pass. GitHub still reports `mergeStateStatus=BLOCKED` (29.4 shows as a
  failed check in the rollup).

## Outstanding

- None blocking. Agent launch was exercised with the **mock** backend (no real
  external CLI spawned); the real claude-code/agent-shell backends follow the
  same verified start path. Formula "follow the created root" is covered by the
  normal list/detail drill-in, which is verified.
