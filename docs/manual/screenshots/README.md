# Documentation screenshot pipeline

This directory captures the screenshots embedded in the beads.el user
manual. It is written in two halves so the capture *engine* can be reused
verbatim by the sibling `gascity.el` manual: a package-agnostic engine and a
beads.el-specific shot list.

```
screenshots/
├── screenshot.el      reusable engine — knows nothing about beads.el
├── beads-shots.el     beads.el load-path + one thunk per manifest view
├── capture.sh         orchestrator — fixture, compositor, run, compress, verify
├── fixture.sh         create/seed/teardown the deterministic temp project (WI-3)
├── fixtures/
│   └── beads-fixture.jsonl
└── README.md          this file
```

## What it does

1. Creates (or reuses) the **shared demo project** — a git repo at
   `${TMPDIR:-/tmp}/demo-project` (default `/tmp/demo-project`) with its own
   `bd` store seeded from `fixtures/beads-fixture.jsonl`. This is the same path
   and the same fixed basename the sibling `gascity.el` manual promises, so
   both manuals describe one demo project and the buffer/mode-line text is
   identical across invocations. It never reads or writes the repository's own
   `.beads` store or a live/agent store (REQ-005). Pass
   `BEADS_SHOT_PROJECT=/path` to reuse an existing fixture instead (a reused
   path is never torn down).
2. Starts a **private, headless Wayland compositor** (`sway` with the
   wlroots headless backend) so capture frames never appear on the user's
   desktop. Windows are floated so Emacs's requested frame size is honored
   (sway tiles by default, which would force every frame to fill the output).
3. Runs a **graphical** Emacs once per manifest slot (`-dark` / `-light`).
   It loads beads.el, the engine, and the shot list; renders each selected
   view against the fixture store; waits for the view's asynchronous `bd`
   data to settle; fits the frame tightly to the rendered content; and
   exports a raw PNG.
4. **Compresses** each raw PNG to an 8-bit-palette full image plus a small
   thumbnail under `docs/manual/images/` (committed). The raw PNGs are
   throwaway.

`x-export-frames` needs a real frame, so this runs a **graphical** Emacs,
never `--batch`.

## Requirements

| Tool | Why | Verified |
|------|-----|----------|
| Emacs (pgtk/GUI) | renders the views; `x-export-frames` | 30.2 |
| `sway` (wlroots headless) | private capture display | 1.11 |
| ImageMagick `convert` | compress + thumbnail | 6.9 |
| `bd` + `git` | create the fixture project | 1.3.1 |
| `flock` (util-linux) | serialize runs on the shared fixture | 2.42 |
| `python3` | only `fixture.sh verify` (the structural check) | — |

No third-party theme is a requirement: every image is captured from
Emacs's built-in default faces (REQ-004). No `ef-`, `modus`, or other
third-party theme may be loaded by the capture or manual-build path; the
only exception tolerated below is the verbatim engine's docstring example.

## Running it

```sh
./capture.sh                       # full pipeline for scope `all`
./capture.sh --scope all           # same, explicit
./capture.sh --scope dashboard,show
./capture.sh --scope changed       # views touched by the working diff
./capture.sh --verify all          # capture twice and assert byte-identity
./capture.sh --verify-scope show   # a subset must leave other images untouched
./capture.sh --compress            # recompress the existing raws only
```

The committed image footprint is small (palette PNGs, full + thumbnail).
Capture is read-only with respect to the fixture store: it only opens views.

### Scope (REQ-004)

One variable, `scope`, selects what to regenerate:

| Value | Meaning |
|-------|---------|
| `all` | every view in the manifest |
| `changed` | views whose source files differ from `BEADS_SHOT_BASE` (default: merge-base with the origin default branch) |
| `a,b,c` | a comma-separated view-name subset |

A subset run passes only the selected views to the engine and compresses only
those names into `docs/manual/images/`, so it never rewrites or deletes an
out-of-scope image. `--scope <unknown>` exits non-zero (status 2) before
anything runs.

### Shared fixture, lock, and dataset hash (REQ-001, REQ-002)

The project path is fixed, not per-run, because it is a cross-repo contract
with the `gascity.el` manual. Two consequences are part of the pipeline:

- **Marker-guarded lifecycle.** `fixture.sh create` recreates a stale project
  only when it carries the `.beads-fixture-project` marker; anything else at
  that path is refused (non-zero, nothing deleted). `teardown` accepts only the
  exact `${TMPDIR:-/tmp}/demo-project` path (a trailing slash is trimmed),
  requires the marker, and succeeds idempotently when the directory is already
  gone. `remove_fixture_dir` never removes a run parent, because there is none.
- **Serialized runs.** `fixture.sh create|teardown|verify` hold an advisory
  `flock` on `${TMPDIR:-/tmp}/demo-project.lock` briefly; `capture.sh` takes
  the same lock and holds it for the whole capture run (bounded by
  `BEADS_SHOT_LOCK_TIMEOUT`, default 1800 s). A second concurrent capture waits
  at the log line instead of sharing the store and its `.beads.gate.lock`. The
  lock file lives next to, never inside, the project. `capture.sh` exports
  `FIXTURE_LOCK_HELD=1` after acquiring it, so the nested `fixture.sh` calls
  it makes skip acquisition and cannot deadlock against their own parent; a
  direct CLI invocation without the marker still locks itself.

The dataset is frozen at `demo-1` … `demo-12` with `sha256`
`6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8`. That
literal lives once, as `FIXTURE_DATA_SHA256` in `fixture.sh`; `fixture.sh
verify` hashes `fixtures/beads-fixture.jsonl` against it, then checks that
this README and the manual's shared demo-project section record the same
value. A data edit without the documentation update therefore fails `verify`.

Isolation is unchanged: `capture.sh` unsets the four variables that can
reroute `bd` (`BEADS_DIR`, `BEADS_DOLT_PORT`, `BEADS_DOLT_SERVER_PORT`,
`GC_DOLT_PORT`) before spawning Emacs, and `fixture.sh` unsets them before
every `bd` call. `fixture.sh print-isolation-vars` prints the hardcoded list
that `beads-manual-isolation-list-matches-fixture` compares against the
integration tests' `beads-test-isolation-env-vars`.

### Reproducibility / verify (REQ-007)

`capture.sh --verify <scope>` captures the scope twice into throwaway staging
directories and compares every produced raw PNG byte-for-byte. It reports
each drifting, missing, or extra image by name and exits non-zero. It never
mutates the committed images, so it is safe to run at any time.

The target is **byte-identical output** for two runs of the same scope:

- The fixture pins stable role ids `demo-N` via `bd init --prefix demo` and
  explicit-id import, so no run-generated hash appears.
- A post-render normalization hook (`beads-shots--normalize-ids`, installed
  with `advice-add` on `screenshot-export` and `screenshot-export-frame`)
  replaces any `beads-issue-id-regexp` match that shares the fixture prefix
  but is not already `demo-N` with `demo-role`, as a backstop.
- The dashboard status line shows a wall-clock relative timestamp
  (`refreshed Ns ago`). For capture only, `beads-shots.el` overrides
  `beads-dashboard--format-relative-time` to a fixed `0s ago`, so the
  settle duration cannot leak into the image.
- The samplers are fixed: the same private compositor resolution, the same
  frame geometry, the same default faces (no theme loaded), and the same font.

### Subset isolation / verify-scope (AC-4)

`capture.sh --verify-scope <subset>` proves a subset run cannot disturb the
rest of the manual: it snapshots the `sha256` digest and `mtime` of every
`docs/manual/images/*.png`, runs the subset through the real capture and
compress path, and fails, naming each offending image, if any **non-selected**
image changed byte-for-byte or in mtime. It requires a prior `--scope all`
run to have populated `docs/manual/images/`.

```sh
./capture.sh --scope all            # populate images/
./capture.sh --verify-scope show    # show/subset; all other images must not move
```

## Configuration (environment variables)

`capture.sh` sets or honours these.

| Variable | Default | Meaning |
|----------|---------|---------|
| `BEADS_REPO` | this checkout | beads.el checkout (load path) |
| `BEADS_SHOT_DIR` | created by `fixture.sh` | fixture project the views open against |
| `BEADS_SHOT_PROJECT` | unset | reuse an existing fixture project instead of creating one; a reused path is never torn down and does not take the shared lock |
| `BEADS_SHOT_LOCK_TIMEOUT` | `1800` | seconds `capture.sh` waits for another run's fixture lock |
| `BEADS_SHOT_STAGING` | `/tmp/beads-shots` | parent for per-run staging dirs |
| `BEADS_SHOT_BASE` | origin default branch | git ref for `--scope changed` |
| `BEADS_SHOT_WATCHDOG` | `400` | seconds before a capture Emacs kills itself |
| `BEADS_SHOT_ENGINE` | unset | capture-executable override used by the shell tests (skips the compositor) |
| `BEADS_SHOT_THEME` | `default` | set by `capture.sh` per manifest slot; `default` means load no theme |
| `BEADS_SHOT_THEME_LIBRARY` | unset | optional theme library for a non-default `BEADS_SHOT_THEME`; never set by the shipped manifest |

Isolation: `capture.sh` unsets `BEADS_DIR`, `BEADS_DOLT_PORT`,
`BEADS_DOLT_SERVER_PORT`, and `GC_DOLT_PORT` before spawning Emacs, so a
value inherited from an agent shell cannot reroute the capture. The same four
names are hardcoded in `fixture.sh`.

`fixture.sh` also honours `TMPDIR` (the shared project moves to
`$TMPDIR/demo-project`) and `FIXTURE_LOCK_TIMEOUT` (its own bounded lock wait,
default 600 s). The `FIXTURE_LOCK_HELD=1` marker is set by `capture.sh` only
after it owns the lock; do not set it by hand.

The capture **manifest** — which views are shot in which slot — is a short
here-doc near the top of `capture.sh`:

```
default       -dark   dashboard list ready blocked show epic compose molecule swarm agents dispatch maintenance agent-menu formula
default       -light  dashboard list show epic
```

Each line is `THEME  SUFFIX  view view …`. `default` means "load no theme":
those images are captured with Emacs's built-in default faces and its
`defface` defaults, so no third-party theme colours or fonts can appear
(REQ-004). The `THEME` field still selects the slot suffix; any other value
would name a theme symbol for a future re-theming, and
`BEADS_SHOT_THEME_LIBRARY` would point `custom-theme-load-path` at that
theme's library.

The `-dark` and `-light` suffixes are **link-stability slots, not theme
claims**: both come from the same default faces, so the dashboard, list,
show, and epic images are captured twice into two slots that are byte-identical
by design. Keeping both preserves every `@shot` reference and the
beads.el/gascity.el `<view>-<theme>.png` naming contract (plan resolution 1,
REQ-004). Captions describe the view or the default faces, never an `ef-`
variant.

Image geometry (widths, palette sizes) is set just below the manifest. The
view names must match the shots defined in `beads-shots.el`.

## Views

| View | Surface | Kind |
|------|---------|------|
| `dashboard` | `beads-dashboard` project pulse | vui, async |
| `list` | full issue list | tabulated list |
| `ready` | ready-work list | tabulated list |
| `blocked` | blocked-issues list | tabulated list |
| `show` | issue detail (`demo-4`, blocked with dependencies) | section view |
| `epic` | epic detail (`demo-10` with children) | section view |
| `compose` | `beads-compose-create` buffer | buffer |
| `molecule` | molecule view of `demo-10` | vui, async |
| `swarm` | swarm fleet list | vui, async |
| `agents` | agent session list | tabulated list |
| `dispatch` | `beads-dispatch` transient | whole frame |
| `maintenance` | `beads-maintenance` transient | whole frame |
| `agent-menu` | `beads-agent` transient | whole frame |
| `formula` | `beads-formula-menu` transient | whole frame |

`beads-shots.el` requires each view module explicitly because it runs from a
bare `emacs -Q -l`, where the package's generated autoloads file is absent.
It also sets `screenshot-async-process-regexp` to
`beads-async\|beads-policy-probe`, the two process names `beads-command.el`
uses, so the engine's `screenshot-settle` waits for dashboard and eldoc data.

## Provenance and reuse

`screenshot.el` is copied **verbatim** from the sibling pipeline:

- Upstream: `gascity.el/doc/screenshots/screenshot.el`
- Upstream file revision:
  `7c23f47fca77da4f44ee017b921f9f6afea61085` (last commit touching the file;
  gascity.el `HEAD` at copy time was
  `3f439488d6df3c842e048fccc2ad026f3f6d94a8`)
- Copy hash (`sha256`):
  `dd8b3313a1f440d04bd48986aa0c02e6bcbc4fa73fbe26d8e31bd3f3775459b1`

Do not fork the engine's logic. To refresh the copy, replace the file with the
upstream version and update the revision/hash above; the engine is
package-agnostic and should need no beads.el knowledge. Its docstring names
`ef-themes` only as an example of an external theme library for
`custom-theme-load-path`; that string is part of the verbatim upstream file,
not a requirement, and nothing in this repository loads it (REQ-004,
REQ-006).

The engine API (see the docstrings in `screenshot.el`):

| Function / variable | Purpose |
|---------------------|---------|
| `screenshot-init` | strip chrome, hide cursor, load theme + font |
| `screenshot-settle` | pump the event loop until async data loads |
| `screenshot-fit-frame` | size the frame to the rendered content |
| `screenshot-export` | display a buffer full-frame and write `NAME.png` |
| `screenshot-export-frame` | write the whole frame (overlay UIs, transients) |
| `screenshot-run` | capture a list of `(NAME THUNK . PROPS)` shots |
| `screenshot-theme` / `-theme-library` | theme symbol + library for the load path |
| `screenshot-output-dir` / `-font` / `-max-*` | output dir, font, frame bounds |

Shot props: `:settle SECONDS`, `:fixed-rows N` (paginated lists),
`:row-pad N`, `:whole-frame t` (capture the frame as arranged, for transient
popups).

## Cross-repo contract (REQ-009)

This section is the single authoritative statement of the interface the
beads.el and gascity.el manuals share (plan-review P4). The sibling
`gascity.el` build (`gascity.el/doc/screenshots/`) mirrors it, so the two
manuals read as one system. The *contract* is the formula interface and the
image layout; the per-package file names and shot lists differ by design.

The contract fixes exactly these values:

- **Formula name**: `user-manual-screenshots`. A package that grows a user
  manual declares this same formula name.
- **Scope variable**: exactly one, `scope` — `all` captures every view,
  `changed` selects the views touched by the working diff, and a
  comma-separated list selects that exact subset. A subset run must not
  rewrite or delete any out-of-scope image.
- **Fixture**: referenced by role, never by a stored id; the generated ids
  have the form `demo-<role>`. It is a deterministic, isolated temporary
  project, never the repository or a live store.
- **Layout**: `screenshots/` holds the pipeline and `images/` holds the
  committed images.  beads.el keeps both under `docs/manual/` even though
  its manual source moved to `doc/`; gascity.el keeps them under `doc/`.
- **Naming**: `<view>-<theme>.png` for the full image and
  `<view>-<theme>-thumb.png` for the thumbnail, where `theme` is the link-stability
  slot `dark` or `light`; both slots show the default faces.
- **Verify**: re-run a scope twice and fail, naming each drifting image,
  unless the output is byte-identical.

Concrete alignment with the sibling build:

| Contract term | beads.el | gascity.el (mirrors this contract) |
| --- | --- | --- |
| Manual directory | `doc/` | `doc/` |
| Pipeline directory | `docs/manual/screenshots/` | `doc/screenshots/` |
| Committed images | `docs/manual/images/` | `doc/images/` |
| Capture engine | `screenshots/screenshot.el` | `screenshots/screenshot.el` (the same file, copied verbatim) |
| Shot list | `screenshots/beads-shots.el` | `screenshots/gascity-shots.el` |
| Orchestrator | `screenshots/capture.sh` | `screenshots/capture.sh` |
| Image naming | `<view>-<theme>.png`, `-thumb.png` | identical |
| Formula | `user-manual-screenshots` | the same name when its build adopts it |
| Scope | `scope` = `all` \| `changed` \| subset | the same semantics |

The engine is the one file shared **verbatim**; everything package-specific
is a shot list plus the manifest/geometry table near the top of
`capture.sh`. Do not fork the engine's logic — to refresh the copy, replace
it with the upstream file and update the provenance hash above.

## Allowed differences

None by default: the target is byte-identical output. If a view inevitably
renders a value that changes between runs (e.g. a wall-clock relative
timestamp), it must be enumerated here and the verify path must assert that
specific allowed variation instead of passing unconditionally. The current
manifest has no such entry; the fixture pins timestamps so relative labels
only change across long spans, not between two back-to-back runs.

## Notes & gotchas

- **sway tiles by default.** The config floats all windows
  (`for_window [app_id=".*"] floating enable`) so `set-frame-size` is
  honored; without it, every frame is forced to the output size.
- **Frame size persists between shots.** The engine resets the frame to a
  large baseline before each shot so a paginated list opens at full size
  before being fitted down.
- **Theme load path (docstring example only).** `screenshot.el`'s docstring
  mentions `"ef-themes"` solely as an example of how to point
  `custom-theme-load-path` at an externally installed theme library (for
  example from a Guix profile). It is not a requirement and nothing in the
  capture or manual-build path loads a third-party theme: the engine's
  `screenshot-theme-library` is nil unless a future capture sets
  `BEADS_SHOT_THEME_LIBRARY`. The engine stays a verbatim copy of the
  gascity.el file, so the example string is deliberately not deleted.
- **A failing view is skipped, not fatal.** `screenshot-run` logs a failed
  thunk and continues, so one broken view never aborts the run; check
  `$BEADS_SHOT_STAGING/run/capture.log`.
- **Empty views are not faked.** A view with nothing to show is left out of
  the manifest rather than committed as an empty frame.
