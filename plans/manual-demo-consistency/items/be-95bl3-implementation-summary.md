---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-ybleu
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
    - path: beads/be-95bl3
      hash: bead:be-95bl3
      ids: [REQ-004, REQ-006]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: docs/manual/screenshots/capture.sh
      hash: sha256:aa60d7c2b655da01593da16aa9f1276fb78b42a86db63b6819dec9cfb500b1df
    - path: docs/manual/screenshots/beads-shots.el
      hash: sha256:d48edbb0d7f63f51446a6b7c652b7610443ffc34e812e9438c1f9ccbc45f8514
    - path: docs/manual/screenshots/README.md
      hash: sha256:28354cbe864a171729d3b9e7dccb5e3c5cc86eef73ac6e161ff3a3443fe92853
    - path: docs/manual/screenshots/screenshot.el
      hash: sha256:dd8b3313a1f440d04bd48986aa0c02e6bcbc4fa73fbe26d8e31bd3f3775459b1
    - path: scripts/emacs-minimal
      hash: sha256:1e3667c23896515a2c5296b3a03cbc54f095e1df9cdfcc56316305d23a880396
    - path: share/emacs.d/init.el
      hash: sha256:c99e17b8a0ac0513de81e3083759063598d61544bad49de2276c000a4132b07f
    - path: doc/formulas.texi
      hash: sha256:61674fa222d64345fc122705152a383b6c4de492e2fdfb178d8c8038eb8c06f0
    - path: doc/issue-workflow.texi
      hash: sha256:bbfb38a9a1076f6767b47a69b69f7745f64c2d560ef334dfaef502b5ab003b8a
    - path: doc/views.texi
      hash: sha256:5f66ca46bbad0093b5fe26dfa8b3bf2a0c454d40ef9e21118d394551b420a038
  coverage:
    - id: REQ-004
      status: covered
    - id: REQ-006
      status: covered
---

# Implementation Summary: W2 default-theme-only capture and test path

## Summary

Work item W2 (implementation plan WS-2) removes every third-party theme from
the capture, test, and manual-build path and makes Emacs's built-in default
faces the only thing the committed screenshots can show. The capture manifest
now names the `default` sentinel instead of `ef-deuteranopia-dark` /
`ef-cyprus`; `beads-shots.el` resolves an unset or `default`
`BEADS_SHOT_THEME` to a nil theme (and a nil theme library), so
`screenshot-init` loads nothing; `scripts/emacs-minimal` and
`share/emacs.d/init.el` no longer pull in or rotate `ef-themes`; and the
screenshot README, its configuration/provenance/notes sections, and the four
`-light` captions describe default faces instead of a light theme.

The `-dark` / `-light` filename suffixes are kept as link-stability slots, not
theme claims: both are captured from the same default faces, so every `@shot`
reference, every `@anchor`, and the beads.el/gascity.el `<view>-<theme>.png`
naming contract stay valid (plan resolution 1, REQ-004). `screenshot.el` stays
a verbatim copy of the gascity.el engine; its docstring's `ef-themes` string is
kept and documented as an example of `custom-theme-load-path`, never a
requirement or a load (REQ-006). No beads.el runtime code changes.

## Intended Behavior

- **Manifest (`capture.sh`).** The two manifest lines become
  `default       -dark   dashboard list ready blocked show epic compose
  molecule swarm agents dispatch maintenance agent-menu formula` and
  `default       -light  dashboard list show epic`. The 14-view set and the
  four `-light` slots are unchanged. The `THEME` field still selects the slot
  suffix; `default` means "load no theme". Header requirements no longer name
  a theme and state that every image comes from the built-in default faces.
- **Engine configuration (`beads-shots.el`).** `BEADS_SHOT_THEME` unset,
  empty, or `default` resolves through the new `beads-shots-theme` to nil, so
  `screenshot-theme` is nil and `screenshot-init` skips `load-theme`. The new
  `beads-shots-theme-library` reads the optional `BEADS_SHOT_THEME_LIBRARY`
  (default nil), so `screenshot-theme-library` is nil too. Any other
  `BEADS_SHOT_THEME` value still names a theme symbol and keeps the engine's
  existing `screenshot-theme` / `screenshot-theme-library` behaviour for a
  future re-theming. The `ef-` name is gone from the file's commentary. Id
  normalization and the frame-settle advice are untouched.
- **Test environment.** `share/emacs.d/init.el` drops the `use-package
  ef-themes` block (and its post-init rotation); the rest of the init
  (magit, beads, vertico) is unchanged. `scripts/emacs-minimal` drops
  `emacs-ef-themes` from its `guix shell` list and keeps the rest.
- **README (`docs/manual/screenshots/README.md`).** The `ef-themes`
  requirement row is deleted and replaced with a statement that no
  third-party theme (`ef-`, `modus`, or other) may be loaded by the capture or
  manual-build path. The manifest block, the "once per theme" pipeline step,
  the reproducibility sampler line, and the naming-contract bullet now
  describe default faces and the two link-stability slots. The standalone
  "ef-themes load path" note is rewritten as "Theme load path (docstring
  example only)": `screenshot.el` keeps the string solely as a docstring
  example of `custom-theme-load-path`, and the engine's
  `screenshot-theme-library` is nil unless a future capture sets
  `BEADS_SHOT_THEME_LIBRARY`. The configuration table documents both new
  theme variables. The provenance section repeats the docstring-example
  reading.
- **Captions (`doc/*.texi`).** The four `-light` captions
  (`dashboard-light`, `list-light`, `show-light`, `epic-light`) now describe
  the view or the default faces ("… with Emacs's default faces and no theme
  loaded"); no `@shot` name or `@anchor` changed, so link stability holds.
- **Build inputs.** `guix.scm` and `.github/workflows/*` contain no theme
  input; the only remaining `ef-themes` occurrence in the repository is the
  verbatim engine docstring plus the two README passages that document it as
  an example.

### Coverage

| ID | Status |
| --- | --- |
| REQ-004 | covered |
| REQ-006 | covered |

## Changed Files

| File | sha256 | Change |
| --- | --- | --- |
| `docs/manual/screenshots/capture.sh` | `aa60d7c2b655da01593da16aa9f1276fb78b42a86db63b6819dec9cfb500b1df` | manifest now names the `default` sentinel for both slots (14 views + four `-light`), manifest/header comments explain "load no theme" and the link-stability slots, requirements line drops the theme. |
| `docs/manual/screenshots/beads-shots.el` | `d48edbb0d7f63f51446a6b7c652b7610443ffc34e812e9438c1f9ccbc45f8514` | new `beads-shots-theme` / `beads-shots-theme-library` resolvers (`default`/unset ⇒ nil/nil), setq uses them, commentary documents the theme contract and drops the `ef-` example. |
| `docs/manual/screenshots/README.md` | `28354cbe864a171729d3b9e7dccb5e3c5cc86eef73ac6e161ff3a3443fe92853` | deletes the `ef-themes` requirement row, default-face manifest + "no third-party theme" policy, link-stability slot explanation, rewritten theme-load-path note and provenance sentence, theme env table rows. |
| `scripts/emacs-minimal` | `1e3667c23896515a2c5296b3a03cbc54f095e1df9cdfcc56316305d23a880396` | drops `emacs-ef-themes` from the `guix shell` package list. |
| `share/emacs.d/init.el` | `c99e17b8a0ac0513de81e3083759063598d61544bad49de2276c000a4132b07f` | drops the `use-package ef-themes` block and its post-init theme rotation; rest unchanged. |
| `doc/views.texi` | `5f66ca46bbad0093b5fe26dfa8b3bf2a0c454d40ef9e21118d394551b420a038` | `dashboard-light` caption rewritten to default faces. |
| `doc/issue-workflow.texi` | `bbfb38a9a1076f6767b47a69b69f7745f64c2d560ef334dfaef502b5ab003b8a` | `list-light` and `show-light` captions rewritten to default faces. |
| `doc/formulas.texi` | `61674fa222d64345fc122705152a383b6c4de492e2fdfb178d8c8038eb8c06f0` | `epic-light` caption rewritten to default faces. |

`docs/manual/screenshots/screenshot.el` is unchanged
(`sha256:dd8b3313a1f440d04bd48986aa0c02e6bcbc4fa73fbe26d8e31bd3f3775459b1`),
still the verbatim gascity.el engine.

## Verification

First verification command (targeted manual unit gate, run from the worktree
`/home/roman/workspace/beads.el/worktrees/be-95bl3`):

```
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
```

Observed: **PASS** — `Ran 16 tests, 16 results as expected, 0 unexpected`,
including `beads-manual-manifest-theme-consistency`, which now compares the
manifest's first `-dark` theme word (`default`) with the
`BEADS_SHOT_THEME` default in `beads-shots.el` (`default`) and finds them
equal.

Bead proof commands (all from the worktree):

| Proof | Command | Observed |
| --- | --- | --- |
| no theme requirement or load in the capture/test/build path | `grep -rn "use-package ef-themes\|emacs-ef-themes\|ef-deuteranopia\|ef-cyprus" --exclude-dir=.git --exclude-dir=plans .` | exit 1, **no matches** |
| only occurrences of `ef-themes` left | `grep -rn "ef-themes" --exclude-dir=.git --exclude-dir=plans .` | 3 lines: verbatim `screenshot.el` docstring (line 46) + the two README passages documenting it as an example; no `require`/`load`/`use-package` |
| theme sentinel resolves to no theme | load `beads-shots.el` under `BEADS_SHOT_DIR=/tmp` with `BEADS_SHOT_THEME` unset and with `=default` | progress line reports `theme=nil` in both cases |
| theme resolver shape | extract the three resolver defuns and eval them | unset ⇒ `theme=nil library=nil`; `default` ⇒ `theme=nil library=nil`; `some-theme` + `BEADS_SHOT_THEME_LIBRARY=some-lib` ⇒ `theme=some-theme library="some-lib"` |
| `init.el` starts without `ef-themes` | `emacs -Q --batch -l share/emacs.d/init.el --eval '…(featurep (quote ef-themes))…'` | exit 0, `ef-themes-feature=nil enabled-themes=nil` |
| same under the dev shell (`scripts/emacs-minimal` equivalent) | `guix shell -D -f guix.scm --pure -- emacs --batch -L $PWD/lisp -l share/emacs.d/init.el --eval '…'` | exit 0, `emacs=31.1 ef-themes-feature=nil enabled-themes=nil` |
| `scripts/emacs-minimal` has no theme package | `grep -q ef-themes scripts/emacs-minimal share/emacs.d/init.el`; `bash -n scripts/emacs-minimal` | no match, syntax OK |
| shell syntax | `bash -n docs/manual/screenshots/capture.sh` | OK |
| manifest still parses to the 14 views | `docs/manual/screenshots/capture.sh --list-views` via the manual test suite (`beads-manual-scope-all-maps-views`) | PASS |
| manual theme-consistency test with the `default` sentinel | `make check` (includes `beads-manual-manifest-theme-consistency`) | PASS |
| build inputs carry no theme | `grep -in "ef-\|modus" guix.scm .github/workflows/*` | no matches |
| `make info` with rewritten captions | `make info` | exit 0, no warnings; `doc/beads.info` rebuilt |
| `make html` with rewritten captions | `make html` | exit 0, no warnings; the four affected pages carry the new captions and **zero** pages contain "under the light theme" |
| lint | `make lint` | exit 0, `Byte-compiled 130 file(s)` (only pre-existing third-party `sly` autoload warnings) |

Final proof command (the project gate, run after the last source edit):

```
make check
```

Observed: **PASS** — exit 0,
`Ran 6685 tests, 6659 results as expected, 0 unexpected, 26 skipped`
(the 26 skips are the pre-existing `:live` suite). `make check` runs
`check-manual` (the forbidden-name gate) first, so that gate is green too.

## Remaining Risks

- **Committed images are still WS-3's job.** This item makes it impossible for
  the capture path to load a theme and rewrites the captions, but the PNGs
  currently committed under `docs/manual/images/` were rendered with
  `ef-themes` until WS-3 re-runs `capture.sh --scope all` under the `default`
  sentinel. Do not ship the manual with the old images and the new captions.
- **Branch base vs WS-1.** This worktree is based on `origin/main` d38a8726,
  which does not yet contain WS-1 (`be-rtqte`, the shared `/tmp/demo-project`
  fixture). WS-1 also edits `capture.sh`; this item edits the manifest block
  and header, WS-1 edits `ensure_project`/`teardown_project` and the lock, so
  the refinery merge must combine both edit sets rather than take one side.
  Nothing in this item depends on WS-1 for correctness.
- **`-dark` and `-light` images are byte-identical by design.** REQ-004
  prefers link stability; the duplicated dashboard/list/show/epic images cost
  a little repository size and `--verify-scope` treats both slots as one view.
  If a future design wants genuinely distinct images, it must change the
  naming contract in both manuals first.
- **New optional env var.** `BEADS_SHOT_THEME_LIBRARY` is new; the shipped
  manifest never sets it, so the default path cannot load a third-party
  theme. A future re-theming must set both `BEADS_SHOT_THEME` and this
  variable, which the README documents.
- **Verbatim engine docstring.** `screenshot.el` still contains `ef-themes` as
  an example string. That is intentional (REQ-006 "copied verbatim") and
  documented in two places; a naive repository-wide grep will keep finding it.
  If gascity.el refreshes the engine without the example, re-copy and update
  the recorded provenance hash.
- **`NEWS.md`** has no entry for this change; the plan assigns the NEWS/publish
  sweep to WS-8, and this item stayed inside its WS-2 file boundary.
- **Unresolved ambiguity (recorded, not asked).** The plan text says to
  "replace the theme table/paragraph with the default-face description"; the
  README had a theme-requirement table row and a manifest paragraph rather
  than a standalone theme table, so both were rewritten. No other
  theme-claiming prose was found in `doc/*.texi` or the README beyond the four
  captions.
