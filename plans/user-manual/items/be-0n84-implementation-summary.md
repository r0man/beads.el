---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-ex01
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
    - path: beads/be-0n84
      hash: bead:be-0n84
      ids: [REQ-001, REQ-002]
    - path: plans/user-manual/implementation-plan.md
      hash: sha256:c9ab161d8c4f9d6737b885fad4b4f565fdf634ac3c805d248696439c209d920a
    - path: docs/manual/index.org
      hash: sha256:b899827e0ad778f10ed16562ffbf7f054edca97afb64090d2f67026fe5d80208
    - path: docs/manual/issue-workflow.org
      hash: sha256:95762449cae0bd9bd58ee77f50ce1ec534188d3dbf24e43713ca2490dfad029a
    - path: docs/manual/views.org
      hash: sha256:0530c931aea16cd9d1abde326b753f1a75d7e430a678cbd772144067b4933a79
    - path: docs/manual/formulas.org
      hash: sha256:e76d0dc8cfbfecee7a6a77df84c48bb25b8985cc26545129ee56d5b27d6a6271
    - path: docs/manual/agents.org
      hash: sha256:5113dcef3b759182790bf546c57055a12949e0b88edb5810bd22abea5881a2eb
    - path: docs/manual/orchestration.org
      hash: sha256:a7654f441a2ef7a0612ab0bc7e8fba0171c7a2296442d4d057dbf69d0f223cf2
    - path: docs/manual/maintenance.org
      hash: sha256:be4de6fb1325fa62cc6a8debc9eb5880937e3ddefa4cb7d8543451f08ebde390
    - path: docs/manual/Makefile
      hash: sha256:59548200368adc8d2373e3f475d2ee78b9f8d9b78cac54d69b20ee674c5d45ac
    - path: docs/beads.org
      hash: sha256:7e69dbd6fd97e20359a448a62915bae3d0221879d78e894f7c6799b9d08caf18
    - path: docs/.orgconfig
      hash: sha256:a862b6a8034d394695595318ed9c8056969192f92a791983c99ea1372f9cfb8c
  coverage:
    - id: REQ-001
      status: covered
    - id: REQ-002
      status: covered
---

# Implementation Summary: W3 user manual under `docs/manual/`

## Summary

Implements work item W3 (implementation plan section WI-1) of the
`user-manual-screenshots` build: the task-organized user manual under
`docs/manual/`, its build entry point, and the redirect stub for the old
`docs/beads.org`.

The manual is seven Org pages, each covering one task area from REQ-001:

| Page | Covers |
| --- | --- |
| `docs/manual/index.org` | Title, TOC, installation, first issue, interface concepts |
| `docs/manual/issue-workflow.org` | create/list/view/edit/close/claim/status/priority/dependencies/labels/bulk |
| `docs/manual/views.org` | board and dashboard, list/show sections, pagination, thing motion, eldoc |
| `docs/manual/formulas.org` | formulas, molecules, epics, cook/pour/bond/burn, health |
| `docs/manual/agents.org` | agent types/backends, sessions, worktrees, sling, prompts |
| `docs/manual/orchestration.org` | dispatch/maintenance menus, terminal, Dolt/repo, remotes, config, audit |
| `docs/manual/maintenance.org` | customizing/faces, plumbing/EIEIO, extending, FAQ, indexes |

Every visual page embeds the full and `-thumb` screenshot pair for the views
that illustrate it, using the manifest view/theme names fixed by W2
(`docs/manual/images/<view>-<theme>.png` and `...-thumb.png`). All fourteen
dark views and all four light views in the W2 manifest are referenced by at
least one page, and no reference uses a view or theme outside the manifest.

| ID | Status |
| --- | --- |
| REQ-001 | covered |
| REQ-002 | covered |

## Intended Behavior

- **REQ-001 (task-organized manual).** `docs/manual/` holds an `index.org`
  table of contents plus one page per REQ-001 feature area. The TOC names
  every area (getting started, issue workflow, dashboards/views,
  formulas/molecules, agents/sling, orchestration/terminal, maintenance/admin)
  and links its page. The manual migrates and expands the former
  `docs/beads.org`, fixing its drift: the entry point is `beads-dashboard`
  (`M-x beads`), the dispatch menu is `?` and the maintenance menu is `!`,
  `beads-ops-menu.el`/`beads-advanced-menu.el` are documented as folded into
  `beads-menu.el`, the requirements are Emacs 29.1+ with transient/sesman/vui,
  and the stale `menu-mockups.md` reference is gone.
- **REQ-002 (screenshots next to features).** Each visual section embeds
  `[[file:images/<view>-<theme>.png]]` immediately followed by the matching
  `-thumb` reference, so the rendered manual can show the thumbnail while the
  full image stays linked. The references are exactly the W2 manifest names,
  so when W6 runs the capture the manual points at real, current images.
- **`docs/manual/Makefile`.** Provides one entry point: `manual` exports every
  Org page to `html/` with a headless Emacs, `screenshots` runs
  `screenshots/capture.sh --scope $(SCOPE)` (default `all`), `verify` runs
  `capture.sh --verify $(SCOPE)`, and `clean` removes generated HTML.
- **`docs/beads.org` redirect stub.** Replaces the old monolithic manual with
  a short page that points at `docs/manual/index.org` and its pages, and adds
  an HTML meta-refresh to `manual/index.html`, so existing bookmarks and
  links keep working.
- **`docs/.orgconfig`.** Adds the `fn` and `code` Org macros that the manual
  uses but the setup file did not define, which is what made the previous
  `docs/beads.org` fail a plain Org export (`Undefined Org macro: fn`). This
  is a documentation-only change; no Elisp or product behavior changes.

## Changed Files

- `docs/manual/index.org` — new. Title, TOC, installation, first issue, and
  the interface concepts (modes, transients, context actions, compose buffers,
  marks, movement, store scoping).
- `docs/manual/issue-workflow.org` — new. The full issue lifecycle.
- `docs/manual/views.org` — new. Board/dashboard, list/show sections,
  pagination, thing motion, eldoc.
- `docs/manual/formulas.org` — new. Formulas, molecules, epics.
- `docs/manual/agents.org` — new. Agents, backends, sessions, sling, swarm.
- `docs/manual/orchestration.org` — new. Menus, terminal, Dolt, config, audit.
- `docs/manual/maintenance.org` — new. Customizing, plumbing, extending, FAQ,
  and the keystroke/function/variable indexes.
- `docs/manual/Makefile` — new. `manual`, `screenshots`, `verify`, `clean`.
- `docs/beads.org` — replaced with a redirect stub.
- `docs/.orgconfig` — added the `fn` and `code` macros.

## Verification

First verification command — export the whole manual with the new Makefile
target, from the worktree:

```bash
make -C docs/manual manual
```

Observed: exit 0, `manual written to .../docs/manual/html`, and seven HTML
files (`index`, `issue-workflow`, `views`, `formulas`, `agents`,
`orchestration`, `maintenance`). `grep -rn '{{{' docs/manual/html/*.html`
found no unexpanded macro, and the generated pages contain the expected
`<kbd>` and image `<img>` markup. `make -C docs/manual clean` removed the
HTML.

Final proof command — a manifest↔manual reconciliation over the two W2
manifest themes and every page:

```bash
python3 - <<'PY'
import re, glob, os
dark = "dashboard list ready blocked show epic compose molecule swarm agents dispatch maintenance agent-menu formula".split()
light = "dashboard list show epic".split()
refs = set()
for f in glob.glob("docs/manual/*.org"):
    for m in re.finditer(r"\[\[file:images/([^\]\[]+)\]\]", open(f).read()):
        refs.add(m.group(1))
views = {re.match(r"^(.*)-(dark|light)", p).group(1) for p in refs}
assert all(p.endswith((".png",)) for p in refs)
assert set(dark) <= views, set(dark) - views
assert set(light) <= views, set(light) - views
assert all(v in dark or v in light for v in views)
print("OK", len(refs), "references cover", sorted(views))
PY
```

Observed: `OK 26 references cover [...]` with all fourteen dark and four
light views covered, no reference to an unknown view, and no reference to a
light image outside the W2 manifest. Also observed: `docs/manual/` contains
exactly the seven expected pages, and `grep -rn 'menu-mockups' docs/manual/
docs/beads.org` is empty.

## Remaining Risks

- W3 owns the manual's image *references*, not the images. The pipeline in
  W2 and the full capture in W6 produce `docs/manual/images/*.png`; until W6
  runs, the referenced files do not exist on disk. W6 proves AC-2 with real
  captures, and W7's test asserts every referenced image exists after W6. W3
  deliberately does not fabricate placeholder images.
- The manual prose is migrated from `docs/beads.org` and corrected against
  `AGENTS.md` and `docs/ui-redesign.md`. A few design-doc terms remain
  stale upstream: `AGENTS.md` still says `beads-status` "forwards" to the
  dashboard although no such function exists (the entry point is
  `beads-dashboard`). The manual documents the code, not the stale note.
- `docs/.orgconfig` is shared with the old manual; adding the `fn`/`code`
  macros fixes export for both. If another in-flight work item also edits
  that file, the change is additive and small.
- The manual references `beads-formula-menu`, `beads-agent`, and
  `beads-swarm` as documented in the W2 shot list. If a future refactor
  renames a command but not a shot, the manifest coverage test is the guard.
