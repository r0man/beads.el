---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-kn1v
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
    - path: beads/be-z028
      hash: bead:be-z028
      ids: [REQ-002, REQ-004, REQ-007]
    - path: plans/user-manual/implementation-plan.md
      hash: sha256:c9ab161d8c4f9d6737b885fad4b4f565fdf634ac3c805d248696439c209d920a
    - path: plans/user-manual/requirements.md
      hash: sha256:937abb5961549361d93d4ff428066c36a12b7a1062bdc407b1d47cf8c03ce1e5
    - path: docs/manual/screenshots/capture.sh
      hash: sha256:6c87e9c0109ca3117c508e2b7a65990a3f0edc33a88bc2c0e5e57020c00e8729
    - path: docs/manual/screenshots/README.md
      hash: sha256:bc427cab468bd187698bd96fcadec745c26ecff65c853d386916c209a733e519
    - path: docs/manual/Makefile
      hash: sha256:d7ab4a362f1575c792c89e7b7473513d2d453fb203d0bb3ab5e82c9ec074f69c
    - path: docs/manual/images/dashboard-dark.png
      hash: sha256:c11f1d6bc585794d1ddec790e1a73cced5a5a3c55cf728882528c65d27493347
  coverage:
    - id: REQ-002
      status: covered
    - id: REQ-004
      status: covered
    - id: REQ-007
      status: covered
---

# Implementation Summary — be-z028 (W6: full capture, verify, and commit manual images)

## Summary

W6 runs the real screenshot pipeline in this environment and commits the
manual images, then adds the plan-review P3 subset-isolation proof
(implementation-plan step 4 and Verification; REQ-002, REQ-004, REQ-007;
AC-2, AC-4, AC-7). The four owned files changed in commit
`4c19d9bfb8d6fbe23c8f0320b95fe68668929bdc` in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-z028`:

- `docs/manual/images/*.png` — 36 new committed images (18 full captures and
  18 thumbnails), produced by a real headless `sway` + graphical Emacs +
  ImageMagick capture against the deterministic fixture store.
- `docs/manual/screenshots/capture.sh` — new `--verify-scope SCOPE` mode that
  snapshots `sha256` + `mtime` of every committed image, runs the subset
  through the real capture and compress path, and fails, naming each image,
  if any **non-selected** image changed byte-for-byte or in mtime.
- `docs/manual/Makefile` — `verify-scope` target (`SCOPE=...`).
- `docs/manual/screenshots/README.md` — documents `--verify-scope` under the
  verify contract.

The W6 worktree was created at `origin/main` (`c551853`), which does not carry
the pipeline it must drive. To make the pipeline self-contained, the W6 branch
merges the completed dependency worktrees before capturing: W1 (`be-1sab`,
fixture), W2 (`be-ec91`, engine/orchestrator) + W5 (`be-hedp`, contract
README), W3 (`be-0n84`, manual pages), and W4 (`be-rf87`, formula). Those
commits are ancestors of the W6 commit, so the integration stage can merge W6
without re-deriving them.

## Intended Behavior

- **REQ-002 (manual embeds real screenshots).** `capture.sh --scope all`
  rendered every manifest view against the throwaway fixture store and wrote
  the full + thumbnail PNG pairs the manual references. The capture is a
  genuine GUI run: a private headless `sway` compositor, a pgtk Emacs, and
  ImageMagick palette compression; nothing is faked by file existence.
- **REQ-004 (one scope variable, subset-safe).** `--verify-scope` turns the
  documented subset guarantee into an executable proof: a subset run must not
  rewrite or delete any out-of-scope image. The check compares digest and
  mtime of every non-selected image before and after the subset capture.
- **REQ-007 (reproducible, verified).** `capture.sh --verify all` captures the
  scope twice against one fixture and asserts byte-identical raw output. The
  fixture pins role ids `demo-N` and the shot list normalizes any residual
  store-generated id, so two back-to-back runs agree.
- **AC-4 not weakened.** The subset proof uses the real capture+compress path,
  not a file-existence shortcut.

## Changed Files

- `docs/manual/images/` — new; 18 full images
  (`<view>-<theme>.png`) and 18 thumbnails
  (`<view>-<theme>-thumb.png`) across the `ef-elea-dark` and `ef-cyprus`
  themes, for the dashboard, list, ready, blocked, show, epic, compose,
  molecule, swarm, agents, dispatch, maintenance, agent-menu, and formula
  views.
- `docs/manual/screenshots/capture.sh` — `snapshot_images`,
  `view_name_selected`, and `verify_scope_isolation` plus the
  `--verify-scope`/`--verify-scope=` argument handling and dispatch
  (+85/-2).
- `docs/manual/Makefile` — `verify-scope` target and `SCOPE` wiring (+9/-1).
- `docs/manual/screenshots/README.md` — a "Subset isolation / verify-scope
  (AC-4)" section under the verify contract (+15).
- `plans/user-manual/items/be-z028-implementation-summary.md` — this artifact.

## Verification

Run from the worktree `/home/roman/workspace/beads.el/worktrees/be-z028`:

1. **First verification command** —
   `docs/manual/screenshots/capture.sh --scope all` — result **pass**
   (`EXIT=0`); 36 images written, log line
   `docs/manual/images total: 732.5 KiB`.
2. `docs/manual/screenshots/capture.sh --verify all` — result **pass**
   (`EXIT=0`); `verify: byte-identical for 18 image(s) (scope all)`.
3. `docs/manual/screenshots/capture.sh --verify-scope show` — result
   **pass** (`EXIT=0`); `verify-scope: 32 non-selected image(s) unchanged
   (digest + mtime) for scope show`.
4. The same `--verify-scope` check **fails on real change**: an earlier run
   whose selection predicate misclassified the rewritten `show-*` files as
   non-selected reported `verify-scope: DRIFT non-selected image changed`
   for all four and exited `1`. After the predicate was fixed it passed,
   showing the check exercises the drift path rather than passing
   unconditionally.
5. `bash -n docs/manual/screenshots/capture.sh` — **pass** (no syntax
   errors); `--help` lists `--verify-scope`.
6. `docs/manual/screenshots/fixture.sh verify` — **pass**
   (`fixture.sh: verify: 12 role-keyed issues OK`).
7. **Final proof command**, from the launcher rig root
   `/home/roman/workspace/beads.el` after recording this artifact at
   `gc.implementation.summary_path` on the workflow root:
   `GC_BEAD_ID=be-51jl .gc/scripts/checks/build-artifact-valid.sh` — result
   **pass** (`build artifact valid:
   schema=gc.build.implementation-summary.v1`).

| ID | Status |
| --- | --- |
| REQ-002 | covered |
| REQ-004 | covered |
| REQ-007 | covered |

## Remaining Risks

- **The fixture directory name is captured in buffer names.** The dashboard
  and show screenshots include the fixture project name (e.g.
  `beads-fixture.vK34SG`) because the buffer name derives from the store
  root. `--verify` passes because both captured runs share one fixture, but
  two separate `--scope all` invocations against fresh `mktemp` projects
  produce different header text. This is a property of the W1 fixture and the
  W2 shot list, outside the W6-owned boundary; if cross-invocation
  determinism of the committed images is required, the fixture should expose
  a stable project name and the README "Allowed differences" section should
  record it.
- **GUI capture depends on the host stack.** The run requires pgtk Emacs,
  headless `sway`, and ImageMagick `convert`. All were present and exercised
  here; a host without them cannot regenerate the images.
- **Transient (whole-frame) shots are wider than their content.** The
  `dispatch`/`formula`/`agent-menu`/`maintenance` shots export the whole
  frame, so they contain a large empty region; the thumbnails for the
  smallest ones upscale and can exceed the full-size palette PNG. Cosmetic,
  not a correctness issue.
- **Merged dependency branches.** The W6 commit sits on top of W1/W2/W3/W4/W5
  merge commits; the publish/integration stage should treat W6 as the
  integrating node rather than merging those worktrees again.
