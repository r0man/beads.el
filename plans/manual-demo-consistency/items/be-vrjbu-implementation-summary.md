---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-wm8j0
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
    - path: beads/be-vrjbu
      hash: bead:be-vrjbu
      ids: [REQ-005]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: docs/manual/screenshots/capture.sh
      hash: sha256:0d67bc9d45c9f6d3123cb7bf8d4df62e69096d0aa5d2a5fcdc4b4ba68b91d8c0
    - path: docs/manual/screenshots/beads-shots.el
      hash: sha256:c00c50fa93feeae63f711ecad09758845fffd3c678e87944106ca465c975ab32
    - path: docs/manual/screenshots/README.md
      hash: sha256:b38c0aa9c7c205eb9e80fae7fdb5698652b2e534d4e53032e0efc09a74300615
    - path: lisp/test/beads-manual-test.el
      hash: sha256:deafa320f493f629e4c01730183dd70518cd4dd28c2eec7ea7c30d6016389bfe
  coverage:
    - id: REQ-005
      status: covered
---

# Implementation Summary: W3 regenerate every screenshot under default faces

## Summary

Work item W3 (implementation plan WS-3) regenerates every committed image
under `docs/manual/images/` from the shared `/tmp/demo-project` fixture with
Emacs's built-in default faces and no theme loaded, and proves the pipeline is
reproducible.

The first full run exposed a real capture defect rather than a theme problem:
the post-render id normalizer in `beads-shots.el` matched the fixture project's
own name (`demo-project`), which is syntactically a beads id (`demo-<hash>`)
and is rendered by the dashboard header and the show view's project row. Both
of those rows live in read-only render output, so `replace-match` aborted the
`dashboard`, `show`, and `epic` shots with `text-read-only` / `buffer-read-only`
(the dark slot had no images for them, and the light slot lost `list` as well).
Normalization now skips the fixture project's basename and any match inside
read-only text (logging it) instead of aborting the shot.

The full manifest then captured cleanly: 18 full images and 18 thumbnails
(fourteen `-dark` views plus four `-light` link-stability slots), with no
`FAILED` shot and no normalization rewrite. `--verify all` captured every scope
twice and found all 18 raw PNGs byte-identical; `--verify-scope show` left all
32 non-selected images byte- and mtime-identical.

A later re-run of `--verify all` exposed a second, pre-existing timing defect:
`beads-shots--await-frame-size` can return as soon as `frame-pixel-*` reports
the requested size, before the headless compositor has painted it. The export
then read the previous frame size, so `dispatch` laid its transient out for the
tiny preceding compose frame (a clipped menu) and an empty list could keep the
220x88 baseline instead of being fitted down. The capture-only
`x-export-frames` advice now pumps the event loop for one bounded beat after
the size check (and the transient thunk does the same before
`transient-setup`), so every shot exports the frame it actually laid out.
`dispatch` is now the complete menu in every run, `agents` is the compact
fitted list in every run, and repeated `--verify` invocations are stable.

The capture-time advice (frame-settle wait, id normalization, fixed dashboard
relative time) is now installed by `beads-shots-install-capture-hooks` from
`beads-shots-run` instead of at file load, so the ERT suite can load the shot
list for the pure helpers without mutating a live session; `BEADS_SHOTS_NO_RUN`
suppresses the entry point for that load. A new `:unit` test pins the
normalization behaviour.

No `lisp/` runtime code changed; only the documentation capture pipeline, its
test, and the committed images.

### Coverage

| ID | Status |
| --- | --- |
| REQ-005 | covered |

## Intended Behavior

- **Every committed image is regenerated under default faces (REQ-005).**
  `docs/manual/screenshots/capture.sh --scope all` creates the shared fixture
  at `${TMPDIR:-/tmp}/demo-project` (here `/tmp/demo-project`), captures the
  manifest's fourteen views in the `-dark` slot and the four link-stability
  views (`dashboard`, `list`, `show`, `epic`) in the `-light` slot with no
  theme loaded, and writes a full 8-bit-palette PNG plus a 520 px thumbnail
  for each of the 18 images. The `-dark` and `-light` pairs are
  byte-identical by design; the suffixes are link-stability slots, not theme
  claims.
- **The fixture project name is not an issue id.** `beads-shots--normalize-ids`
  still rewrites a fixture-prefixed id that is not `demo-<digits>` to
  `demo-role`, but it derives the fixture project's basename from
  `BEADS_SHOT_DIR` and leaves that string alone. It also skips a match inside
  a read-only buffer or read-only text and records
  `normalize: skipped <id> in read-only <buffer>` in `capture.log` instead of
  aborting the shot; a run-generated id can therefore never reach a committed
  image, and the dashboard/show headers render `demo-project` unchanged.
- **Capture-only advice is installed when a capture runs.** The frame-settle
  `x-export-frames` advice, the post-render normalization advice on
  `screenshot-export` / `screenshot-export-frame`, and the fixed
  `beads-dashboard--format-relative-time` override moved into
  `beads-shots-install-capture-hooks`, called first by `beads-shots-run`.
  Loading the file with `BEADS_SHOTS_NO_RUN` set (the ERT path) installs
  nothing and does not capture.
- **The README records only facts the regeneration proves.** It now states the
  committed image count (18 full + 18 thumbnails, fourteen dark + four light
  slots) and that every committed image is embedded by at least one `@shot`,
  and it documents the project-name/read-only exclusions of the normalizer.
  Slot identity and the "`default` means load no theme" manifest were already
  documented and are unchanged.
- **Captions and `@shot` references are unchanged.** All 36 committed files
  (18 full, 18 thumbnails) are referenced by an existing `@shot`; every
  manifest view still has a manual page, and `dashboard-dark` is referenced by
  both `getting-started.texi` and `views.texi` (allowed by AC-005).

## Changed Files

| File | sha256 | Change |
| --- | --- | --- |
| `docs/manual/screenshots/beads-shots.el` | `c00c50fa93feeae63f711ecad09758845fffd3c678e87944106ca465c975ab32` | adds `beads-shots--fixture-non-ids` (the fixture project basename) and skips it plus read-only matches in `beads-shots--normalize-ids`; moves the three capture-only advice installations into `beads-shots-install-capture-hooks`, called from `beads-shots-run`; guards the entry point with `BEADS_SHOTS_NO_RUN` for ERT loads; adds a bounded event-loop pump after the frame-size check (and before `transient-setup`) so exports read the painted frame size instead of a stale one. |
| `docs/manual/screenshots/README.md` | `b38c0aa9c7c205eb9e80fae7fdb5698652b2e534d4e53032e0efc09a74300615` | records the proven 18 full + 18 thumbnail inventory and the per-slot split; documents the normalizer's project-name and read-only exclusions. |
| `lisp/test/beads-manual-test.el` | `deafa320f493f629e4c01730183dd70518cd4dd28c2eec7ea7c30d6016389bfe` | new `:unit` test `beads-manual-normalize-ids-keeps-fixture-project-name`: loads the shot list without capturing, keeps `demo-project` and `demo-10`, rewrites `demo-abc123` to `demo-role`, and leaves a read-only `demo-q9z2` alone. |
| `docs/manual/images/*.png` (36 files) | regenerated; per-file hashes in the commit | every full image and thumbnail, including the four `-light` slots, re-captured from the shared fixture under default faces. `dashboard`/`list`/`show`/`epic` dark and light files are byte-identical within each pair. |

`docs/manual/screenshots/capture.sh` is unchanged by this item
(`sha256:0d67bc9d45c9f6d3123cb7bf8d4df62e69096d0aa5d2a5fcdc4b4ba68b91d8c0`);
its manifest already named the `default` sentinel and it already unsets the
four isolation variables before launching the capture Emacs, so the earlier
`/tmp/demo-project` content was the fixture, not the ambient rig store.

## Verification

Preflight (recorded as the task requires): `docs/manual/screenshots/capture.sh
--list-views` printed the fourteen manifest views
(`dashboard list ready blocked show epic compose molecule swarm agents
dispatch maintenance agent-menu formula`). Observed toolchain: GNU Emacs 31.1
(pgtk GUI, `emacs -Q -l docs/manual/screenshots/beads-shots.el`), sway 1.12
(wlroots headless), ImageMagick 6.9.13, `flock` from util-linux, `bd` present,
and a writable `XDG_RUNTIME_DIR=/run/user/1001`.

First verification command (targeted manual unit gate, run from the worktree
`/home/roman/workspace/beads.el/worktrees/be-vrjbu`):

```
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
```

Observed: **PASS** — `Ran 21 tests, 21 results as expected, 0 unexpected`,
including the new `beads-manual-normalize-ids-keeps-fixture-project-name`.

Bead proof commands (all from the worktree):

| Proof | Command | Observed |
| --- | --- | --- |
| full capture regenerates every image | `docs/manual/screenshots/capture.sh --scope all` against `/tmp/demo-project` | **PASS** — exit 0; 18 raw PNGs, no `FAILED` line in `capture.log`, 18 full + 18 thumbnails written (773.7 KiB total) |
| reproducibility (AC-4) | `docs/manual/screenshots/capture.sh --verify all` | **PASS** — exit 0, `verify: byte-identical for 18 image(s) (scope all)`; re-run after the frame-paint fix and a second `--verify dispatch`/`--verify agents`, all byte-identical |
| subset isolation (AC-4) | `docs/manual/screenshots/capture.sh --verify-scope show` | **PASS** — exit 0, `32 non-selected image(s) unchanged (digest + mtime) for scope show` |
| canonical set restored after the subset proof | `docs/manual/screenshots/capture.sh --scope all` | **PASS** — exit 0; re-ran so the committed set is the full-run output (the isolated `show` run legitimately omits the dashboard breadcrumb row) |
| image inventory and references | script over `docs/*.texi` + `docs/manual/images/*.png` | 36 committed files = 18 full + 18 thumbnails; 18 unique `@shot` names; **0 unreferenced images**; dark/light pairs `dashboard`/`list`/`show`/`epic` sha256-identical |
| every manifest view has a page | `beads-manual-manifest-views-have-pages` (in the suite below) | **PASS** |
| no third-party theme or wall-clock text | visual inspection of the 18 raws; manifest is `default` for both slots; `capture.log` has no `FAILED` and no `normalize:` line | default faces only, fixture ids `demo-1`…`demo-12` (plus the deterministic `demo-project` project name), dates are the pinned 2025 fixture values, dashboard relative time pinned to `0s ago` |
| project gate | `make check` (runs `check-manual` first) | **PASS** — exit 0, `Ran 6692 tests, 6666 results as expected, 0 unexpected, 26 skipped` (the 26 skips are the pre-existing `:live` suite) |
| lint | `make lint` | **PASS** — exit 0, `Byte-compiled 130 file(s)` (only pre-existing third-party `sly` autoload warnings) |
| docs gate | `make check-manual` | **PASS** — exit 0 |

Final proof command (the item's proof, after the last pipeline edit):

```
docs/manual/screenshots/capture.sh --verify all
```

Observed: **PASS** — exit 0, `verify: byte-identical for 18 image(s) (scope
all)`. The README edit and the summary are non-executable documentation edits;
`make check` and `make lint` were re-run after them and stayed green.

## Remaining Risks

- **The shared fixture path is not fully serialized yet.** `/tmp/demo-project`
  is shared with the sibling gascity.el manual by contract, and both pipelines
  use marker files and an advisory `flock` on `/tmp/demo-project.lock`. A
  crashed sibling run left a `.gascity-fixture-project` directory at that path
  when this item ran; beads' `fixture.sh create` correctly refuses to delete a
  foreign fixture, so the stale directory was removed by hand before the run.
  The sibling capture stack in this convoy does not yet take the lock, so a
  concurrent sibling capture could still remove/recreate the path mid-run; the
  lock is the intended contract and the cross-repo work must finish it.
- **The `show` image depends on the dashboard buffer existing.** When the
  dashboard has been opened in the same Emacs, the show view renders a
  `↳ Back to dashboard` breadcrumb row; an isolated `--scope show` capture
  omits it. `--verify all` is deterministic (fixed order), and
  `--verify-scope` only asserts the non-selected images, so both proofs pass,
  but the committed `show` images are the full-run output by design. Re-run
  `--scope all` after any `--verify-scope show` before committing.
- **Whole-frame transient shots still show the previous buffer.** `dispatch`,
  `maintenance`, `agent-menu`, and `formula` export the whole baseline frame,
  so the compose buffer from the preceding shot shows behind the popup (the
  old `ef-themes` images have the same shape). The menu layout itself is now
  deterministic and complete; tightening the frame would need per-menu frame
  fitting and is out of scope for this item.
- **`-dark` and `-light` images are byte-identical by design.** REQ-004
  prefers link stability, so each duplicated image costs a little repository
  size. Changing that requires changing the naming contract in both manuals
  first.
- **Read-only normalization skip is a safety valve.** A run-generated id that
  appears inside read-only render output would survive as-is (logged as
  `normalize: skipped …` in `capture.log`) rather than failing the shot. The
  fixture store makes that case unreachable today, but a future view that
  embeds a run-time id in immutable text would need a rendering fix, not a
  normalizer override.
- **`NEWS.md` has no entry.** The plan assigns the changelog/publish sweep to
  WS-8; this item stayed inside its WS-3 file boundary.
- **Build artifacts.** The autotools bootstrap/configure/make used for the
  gates left `.elc`, `doc/beads.info`, and other build outputs in the worktree;
  they are gitignored and not part of the commit.
