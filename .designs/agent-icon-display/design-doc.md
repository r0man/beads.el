# Design: Agent visual identity — icons across all UI surfaces

## Status: shipped (bde-npte) — icon set superseded by bde-e9nt

This document is the original design that landed under epic
[bde-npte](https://github.com/) (closed 2026-05-21, 9/9 children
complete). All mechanism — `:icon` slot on `beads-agent-type`,
`beads-agent-type-icon-or-letter` accessor, GUI/TTY auto-detection via
`beads-agent-display-use-icons`, `beads-agent-display-type-icons` user
overrides, propagation across the seven UI surfaces (issue list,
dashboard, `*beads-agents*` list, show buffer Agent Sessions, mode-line,
per-issue transient header, prompt-edit header line) — is as designed
below.

**The built-in icon set was swapped under [bde-e9nt] from the
human/tool metaphor in this doc to an animal set.** A second swap
under PR #63 review (see [bde-f573]) aligned the shipped icons with
the PR description's eagle/deer/raccoon/chipmunk/fox set. The rest of
the design (slot mechanics, accessor contract, fallback rules, outcome
glyphs, override surface) is unchanged.

| Type   | Letter | Original design | **As shipped**   | Codepoint        |
|--------|--------|-----------------|------------------|------------------|
| Task   | `T`    | 👷              | 🦅 (eagle)       | U+1F985          |
| Review | `R`    | 🕵️              | 🦌 (deer)        | U+1F98C          |
| Plan   | `P`    | 🧭              | 🦝 (raccoon)     | U+1F99D          |
| QA     | `Q`    | 🧪              | 🐿️ (chipmunk)    | U+1F43F U+FE0F   |
| Custom | `C`    | 🪄              | 🦊 (fox)         | U+1F98A          |

Note: the QA chipmunk uses VS16 (U+FE0F) to force emoji presentation;
without it, U+1F43F renders as a text-style black-and-white glyph on
several common emoji fonts.

Source of truth for the live values: `lisp/beads-agent-types.el`
(`:icon` initforms on each `beads-agent-type-<x>` subclass). User
overrides via `beads-agent-display-type-icons` still take precedence over the
shipped initforms — see the resolution order in
`beads-agent-type-icon-or-letter` (`lisp/beads-agent-type.el`).

The remainder of this document preserves the original design context
and rationale (human/tool metaphor, glyph age analysis, etc.) so the
design intent — high-discriminability single-grapheme identity per
agent role, with shape-disambiguated outcome glyphs and per-type
override — is not lost. Read it as the *contract* the implementation
satisfies, not as the literal icon list.

---

## Executive Summary

Today every running agent on an issue row is identified by a single uppercase
letter pulled from the `beads-agent-type` class's `letter` slot
(`T`/`R`/`P`/`Q`/`C` for Task/Review/Plan/QA/Custom), optionally suffixed with
an instance number (`T#1`). The letter is rendered via
`(substring type-name 0 1)` inline at the callsites, not through the existing
`beads-agent-type-letter-display` generic. The result is dense, color-only
status encoding (colorblind-hostile), high visual ambiguity between QA and
Custom, and zero presence on the dashboard.

This design adds an `:icon` slot to `beads-agent-type`, defaults built-in types
to descriptive **tool/role emoji** (🛠 🔍 🗺 🧪 ✨), centralizes the display
through a new `beads-agent-type-icon-or-letter` accessor that auto-degrades to
the existing letter under TTY Emacs, and threads the icon through **seven UI
surfaces** — the issue list column, the dashboard (currently shows no agent
presence at all), the `*beads-agents*` list, the show buffer's Agent Sessions
section, the mode-line, the per-issue agent transient header, and the prompt
edit header line.

The change is additive (the `letter` slot stays — it remains the keyboard
mnemonic for `a t`/`a r`/etc.), feature-flagged via
`beads-agent-display-use-icons`, and lets the user override icons per-type via
a defcustom without subclassing. Drops the `#N` instance suffix from the
common cases and moves it to help-echo, freeing column width.

## Problem Statement

The current agent display has three concrete weaknesses:

1. **Low discriminability at a glance.** `T` `R` `P` `Q` `C` are visually
   similar uppercase glyphs; status (running / touched / finished / failed) is
   encoded only by face color, which collapses for colorblind users and
   anywhere emoji bypass face foreground.
2. **Not integrated where users look.** The dashboard — the user's home
   screen — shows zero indication of which issues have running agents. The
   `*beads-agents*` list buffer omits the agent type entirely (shows only
   backend name). The show buffer's Agent Sessions section likewise lacks the
   type indicator.
3. **Letter contract is honored in registration but ignored in rendering.**
   `beads-agent-type--validate-letter` enforces letter uniqueness, but no UI
   code calls `beads-agent-type-letter-display`; callsites inline
   `(substring type-name 0 1)`. A custom type named `"Test"` registers
   successfully with letter `T` but then collides at render time with `Task`
   (both show `T` regardless of the registered letter).

Goal: one **emoji** per running agent class, propagated to every UI surface
that shows agent presence, with a clean fallback to letters on TTY Emacs and
a defcustom override path for users who want different icons.

## Proposed Design

### Overview

Add a new `icon` slot on `beads-agent-type` and a layered lookup
(`defcustom override → class slot → letter fallback`). Build one accessor —
`beads-agent-type-icon-or-letter` — and refactor every rendering callsite to
use it. Auto-detect terminal support via `display-graphic-p`. Drop the
default `#N` instance suffix in cramped contexts (issue list column,
dashboard); preserve full type+instance string in mode-line, agent list, and
help-echo.

### Key Components

```
┌─────────────────────────────────────────────────────────────────┐
│  beads-agent-type   (lisp/beads-agent-type.el)                  │
│  ────────────────                                                │
│  slots: name, letter, description, prompt-template,             │
│         system-prompt, +icon  ◄── NEW                           │
│  api:   beads-agent-type-letter-display  (existing, unchanged)  │
│         beads-agent-type-icon            (existing-style gfn)   │
│         beads-agent-type-icon-or-letter  ◄── NEW (the one       │
│                                              UI code calls)    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  beads-agent-types.el  (built-in classes)                       │
│  Adds :icon initforms: 🛠 🔍 🗺 🧪 ✨                            │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  beads-agent-display.el  ◄── NEW small module (or stay in       │
│                              beads-agent.el if preferred)       │
│  Concentrates: icon lookup, supported-p, format-for-session,    │
│                format-for-issue (focused/touched/outcome).      │
└─────────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────┬───────┴───────┬─────────────┬──────────┐
        ▼             ▼               ▼             ▼          ▼
   issue-list    dashboard      agent-list     show buffer  mode-line
   column        sections       buffer         section      indicator
   (Agents)      (issue rows)   (Type col +    (Agent       (project/
                                Status col)    Sessions)    agent ctx)
        │                                                       │
        └───── per-issue transient header, prompt-edit ◄────────┘
              header — same accessor, smaller surfaces
```

### Interface

#### New EIEIO slot

```elisp
(defclass beads-agent-type ()
  (... existing slots ...
   (icon
    :initarg :icon
    :initform nil
    :documentation "Display icon string for this agent type, or nil.
A short string (typically a single emoji, two display columns wide) used
as the visual identifier across all UIs.  When nil, the letter slot is
used as fallback.  Users may override via `beads-agent-display-type-icons'.")))
```

#### New defcustoms

```elisp
(defcustom beads-agent-display-use-icons 'auto
  "Whether to use emoji icons for agent type indicators.
- `auto' (default): icons under GUI Emacs, letters in TTY frames
- t: always use icons
- nil: always use single letters (T/R/P/Q/C)"
  :type '(choice (const :tag "Auto-detect by frame" auto)
                 (const :tag "Always icons" t)
                 (const :tag "Always letters" nil))
  :group 'beads-agent)

(defcustom beads-agent-display-type-icons nil
  "Per-type icon overrides for agent display.
Alist mapping lowercase type names to display strings.  Overrides the
`:icon' slot of the corresponding `beads-agent-type'.  Set the whole
variable to nil to disable overrides and use only class slots."
  :type '(alist :key-type (string :tag "Type name (lowercase)")
                :value-type (string :tag "Icon"))
  :group 'beads-agent)

(defcustom beads-agent-display-show-instance nil
  "When non-nil, append #N to icons in narrow display contexts.
By default the instance number is shown only in mode-line, agent list,
and help-echo tooltips, freeing space in the issue list column."
  :type 'boolean
  :group 'beads-agent)
```

#### New accessor (the single touch-point for renderers)

```elisp
(cl-defgeneric beads-agent-type-icon (type)
  "Return the icon string for TYPE, or nil if no icon is configured.
Resolution order: `beads-agent-display-type-icons' override → `icon' slot.
Does not apply the terminal-supported-p gate; callers should use
`beads-agent-type-icon-or-letter' for the user-visible string.")

(cl-defmethod beads-agent-type-icon ((type beads-agent-type))
  (or (cdr (assoc (downcase (oref type name)) beads-agent-display-type-icons))
      (and (slot-boundp type 'icon) (oref type icon))))

(defun beads-agent--icons-supported-p ()
  "Return non-nil when the selected frame should render emoji icons."
  (pcase beads-agent-display-use-icons
    ('auto (display-graphic-p))
    (val val)))

(defun beads-agent-type-icon-or-letter (type)
  "Return the user-visible identifier string for TYPE.
Returns the configured icon when icons are enabled and supported,
otherwise the type's `letter' slot."
  (or (and (beads-agent--icons-supported-p)
           (beads-agent-type-icon type))
      (oref type letter)))

(defun beads-agent-display-format-session (session &optional brief)
  "Format SESSION's identifier as icon (or letter) +/- instance number.
With BRIEF non-nil, omits the #N suffix even when
`beads-agent-display-show-instance' is set."
  ...)
```

#### Built-in icon assignments (Phase 1) — **decided**

| Type | Letter (kept) | Icon | Codepoint(s) | Unicode | Mnemonic |
|------|---------------|------|--------------|---------|----------|
| Task | `T` | `👷` | U+1F477 | 6.0 (2010) | construction worker — builds the thing |
| Review | `R` | `🕵️` | U+1F575 U+FE0F | 7.0 (2014) | detective — investigates the code |
| Plan | `P` | `🧭` | U+1F9ED | 11.0 (2018) | compass — charts the course |
| QA | `Q` | `🧪` | U+1F9EA | 11.0 (2018) | test tube — runs experiments |
| Custom | `C` | `🪄` | U+1FA84 | 13.0 (2020) | magic wand — open-ended / freeform |

**Icon notes:**
- The Review icon **must** include the U+FE0F variation selector — without
  it, U+1F575 renders as text-style (b/w box) on Apple Color Emoji and Noto
  Color Emoji. Store the icon as `"🕵️"` (two codepoints) in the slot.
- Plan/QA/Custom use Unicode 11.0+ glyphs — fine on Emacs 29+ with Noto
  Color Emoji 2018+, Apple Color Emoji 12.0+ (macOS 10.15+). Older systems
  fall back to tofu; users override via `beads-agent-display-type-icons`.
- `🪄` (Custom) is the youngest glyph (2020). Risk-mitigation: per-type
  override defcustom lets users swap to e.g. `✨` or `🎨` without subclassing.
- The metaphor is **"the role's tool"** — a navigator is identified by the
  compass they carry, a researcher by the test tube. Mixed humans+objects
  works because each glyph reads as "a worker doing X."
- `string-width` is `2` for each; 👷 is single-codepoint, 🕵️ is two
  codepoints but renders as one 2-cell grapheme.

### Data Model

One new optional slot on `beads-agent-type`. No new files (could live in a
small `beads-agent-display.el` for clarity, or in `beads-agent.el` to avoid a
new file). No persistence, no migration, no schema change. Existing
`letter`-only types (e.g. 3rd-party custom types) continue to display their
letter unchanged.

## Trade-offs and Decisions

### Decisions Made

1. **Reuse the existing `letter` slot for keyboard mnemonics; add a separate
   `icon` slot for visual identity.** Conflating them breaks the documented
   single-character contract, the letter uniqueness registry, and the
   `a t / a r / a p / a q / a c` keybindings.
2. **Default to tool/role emoji (🛠 🔍 🗺 🧪 ✨), not human emoji.** Tool
   glyphs offer higher shape discrimination in a 1-character slot, render
   uniformly (single codepoint, no ZWJ skin-tone variants), and read as
   "kinds of work" — the agent identity *is* the work it does.
3. **Auto-detect TTY vs GUI via `display-graphic-p`; fall back to letters.**
   Conservative; users can override either way via the defcustom.
4. **Drop the `#N` instance suffix from the issue list column and dashboard
   by default; keep it in mode-line, agent list, and help-echo.** Frees
   width for multiple concurrent agents; preserves disambiguation where
   space allows.
5. **Centralize rendering through one accessor
   (`beads-agent-type-icon-or-letter`), refactor all callsites.** Today
   `(substring type-name 0 1)` is inlined four times — a custom type
   `"Test"` with letter `T` registers but collides with `Task` at render
   time. The refactor fixes the latent bug.
6. **Issue list column shows focused agents only; touched moves to
   help-echo.** Today the column shows `T#1/~R` mixing focused (yellow) and
   touched (dim). Both is rare and busy. Focused-only is the dominant
   signal; touched is preserved through the existing
   `beads-agent--get-sessions-touching-issue` query and surfaced in
   tooltip + show buffer.

### Decisions confirmed (2026-05-20)

- **Q1. Icon set → Humans (the role's tool).** 👷 🕵️ 🧭 🧪 🪄 — see Built-in
  icon assignments table above.
- **Q2. TTY fallback → `'auto`.** Icons in GUI, letters in TTY by default;
  users can force either way.
- **Q3. Instance `#N` → off by default in tight surfaces.** `🛠` not `🛠1` in
  the issue list and dashboard. `#N` still shows in mode-line, the agent
  list buffer's Title column, and help-echo tooltips.
- **Q4. Dashboard placement → trailing badge on each issue row.** Reuses
  the issue-list formatter; one change updates both. Example:
  ```
  ● beads.el-123  Implement foo                  👷
  ● beads.el-124  Review API design              🕵️ 👷
  ● beads.el-125  Plan migration
  ```

### Q5. Outcome glyphs (finished / failed) → **status prefix + faded role icon**

Decided: `✓👷` for finished, `✗👷` for failed. Width 4 cells; preserves
agent identity through outcome (you can still see "the Task agent
finished" vs "the Review agent finished"); status carries its own
shape so colorblind users get a non-color signal.

**Concretely:**
- **Running / focused:** role icon at full color → `👷`
- **Touched only (focused elsewhere):** role icon with `shadow` face → dim `👷`
  (and help-echo "touched but focused elsewhere"). Today's `~` prefix
  retired — face inheritance does the work.
- **Finished:** `✓` (U+2713, single cell) + role icon, both with
  `beads-list-agent-finished` face → `✓👷`
- **Failed:** `✗` (U+2717, single cell) + role icon, both with
  `beads-list-agent-failed` face → `✗👷`

**Width budget impact:** outcome glyphs take 3 cells (1 + 2), running
agents take 2. The issue list column default rises from the originally
proposed 9 to **11** to fit 2 outcomed agents + separator + 1 running
agent, or 3 running agents + 2 separators. The current default of 15
covers everything; we can ship at 11 and revisit if anyone complains.

**`✓` and `✗` are single-cell ASCII-adjacent (U+2713 / U+2717), present in
every monospace font and renderable in TTY** — so the outcome status
remains shape-distinguishable even when the GUI fallback gates icons off.
In TTY mode finished/failed agents render as `✓T` / `✗R` rather than the
single tinted `●` legacy fallback, which is itself a strict improvement.

### Trade-offs

- **Column width:** emoji are 2 cells wide; current 15-cell budget fits
  ~3 emoji + separators (`🛠 / 🔍`) vs. ~5 letters today. We propose
  reducing `beads-list-agent-width` default to **9** (3 icons + 2 separators)
  and trusting overflow to tooltip + show buffer. Users with concurrent
  4+-agent issues can bump it.
- **Emoji font dependence:** users without emoji fonts see tofu boxes. The
  `'auto` default plus the `nil` escape hatch cover this; we don't promise
  rendering correctness, only graceful degradation.
- **Terminal multiplexer width bugs:** tmux/screen sometimes mis-report
  `string-width` for emoji. Mitigation: feature-flagged off by default in
  TTY; users opt in if their stack works.
- **Loss of letter-as-visual-key in GUI:** Some users have learned `T`/`R`/
  `P` and may resent emoji. Fully reversible via `:use-icons nil`.

## Risks and Mitigations

| Risk | Severity | Mitigation |
|------|----------|------------|
| Emoji renders as `?` / tofu on some Emacs builds | Medium | `display-graphic-p` gate + `nil` defcustom + letter fallback |
| `string-width` miscalculation in tabulated-list-mode | Low | Emacs ≥27 handles it; pin Emacs 29.1+ already in package reqs |
| Custom agent types lack an icon | Low | Slot default is nil → falls back to letter automatically |
| User dislikes the icon set | Low | `beads-agent-display-type-icons` defcustom for per-type override |
| Tmux/screen width bugs cause column misalignment | Low | Off by default in TTY (`'auto`); known-bad combos opt out |
| Loss of `:letter` semantic in user-customized classes | None | `:letter` slot retained, fully backwards compatible |

## Implementation Plan

### Phase 1: MVP — slot + accessor + issue list column

Single small change set:

1. Add `icon` slot to `beads-agent-type` in `beads-agent-type.el`.
2. Add `:icon` initforms to the five built-in classes in
   `beads-agent-types.el`.
3. Add `beads-agent-display-use-icons`, `beads-agent-display-type-icons`,
   `beads-agent-display-show-instance` defcustoms.
4. Add `beads-agent-type-icon`, `beads-agent--icons-supported-p`,
   `beads-agent-type-icon-or-letter`, `beads-agent-display-format-session`.
5. Refactor `beads-list--format-agent` and `beads-list--format-agent-indicator`
   in `beads-command-list.el` to call `beads-agent-type-icon-or-letter`
   (looking up the type object via `beads-agent-type-get`).
6. Drop `#N` from the issue list column by default; preserve in help-echo.
7. Drop touched-only branch from the column (keep focused only); surface
   touched count in help-echo.
8. Reduce `beads-list-agent-width` default to 11 (fits 2 outcomed + 1 running
   or 3 running with separators).
9. Tests: `lisp/test/beads-agent-display-test.el` covering:
   - icon resolution order (override → slot → letter fallback)
   - `'auto` / `t` / `nil` gating
   - format-session output for each type
   - regression: existing letter-only behavior with `:use-icons nil`

### Phase 2: Polish — propagate to all surfaces

10. `beads-agent-list.el`: add a "Type" column with the icon (or convert
    the existing "Backend" column header to "Type — Backend").
11. `beads-command-show.el:807-830`: prefix Agent Sessions lines with the
    icon.
12. `beads-agent.el:1600-1693`: use icon in mode-line `default` and
    `compact` formats; full keeps name. Cache `icons-supported-p` per frame.
13. `beads-agent.el:805-815` and similar transient headers: prefix display
    name with icon.
14. `beads-agent-prompt-edit.el:79-83`: prefix header with icon.
15. **Dashboard integration (the headline new feature):** add a trailing
    agent badge group to issue rows in Ready/In Flight/Blocked dashboard
    sections via `beads-dashboard-sections.el`. Reuse the issue-list
    formatter so a single change updates both.

### Phase 3: Future

16. Nerd Font alternative icon set as a built-in preset (some users prefer
    `nf-fa-wrench` / `nf-md-magnify` / `nf-md-map` etc).
17. Custom (`✨`) type currently has no role-specific icon; let the
    user-provided runtime prompt set a one-off icon via prompt-edit.
18. Screen reader: emit `'help-echo` plus `'aria-label`-equivalent
    (Emacs `'speech` property if available) carrying the type name in words.
19. Theme-aware icons: light/dark variants if requested.

## Appendix: Dimension Analyses (inline summary)

- **API:** new slot vs reuse `letter` vs defcustom-only → recommended
  layered approach (slot + defcustom override + accessor). One refactor
  fixes a latent bug where custom types' `:letter` is ignored by renderers.
- **UX:** tool/role emoji beat human emoji for shape discrimination,
  rendering uniformity, and semantic clarity. Status stays in face;
  touched moves to tooltip; instance moves to tooltip + mode-line.
- **Data:** one optional slot; no migration; fully backwards compatible.
- **Scale:** display-only, O(agents-on-screen) `string-width` calls per
  redraw — negligible.
- **Security:** new user-controlled icon strings present no surface
  beyond what defcustoms already allow.
- **Integration:** seven UI surfaces, two phases. Phase 1 ships the slot,
  accessor, and issue list. Phase 2 propagates to dashboard, agent list,
  show buffer, mode-line, transient headers, and prompt edit. Backwards
  compatible via the `nil` escape hatch.
