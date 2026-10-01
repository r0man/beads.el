# beads.el UI Redesign — Plan (planning only)

**Status: plan for review (refinement 2). Implementation is out of scope until
the plan and the ASCII mockups are reviewed and signed off.**

This directory holds the complete plan for re-thinking beads.el into a
world-class, Magit-style Emacs porcelain for the `bd` bead store. It mirrors
the process and artifact shape of the gascity.el redesign
(`gascity.el/plans/sling-command/`) and now clears the "deeper design +
multi-state mockups" bar of bead `be-mv8d`.

**Refinement 2 folds two user decisions:**

- **F2** — `M-x beads` opens the **status buffer**; the old transient is
  renamed **`beads-dispatch`** and bound to **`?`**; `beads-dashboard` stays
  the full board.
- **F3** — **remove QA and Custom entirely** (classes, prompts, defcustoms,
  start commands, keybindings, registration calls, tests). QA folds into
  **Review as a QA mode** (its testing prompt is kept); Custom's freeform
  prompt moves to **sling's freeform path**; the freed `a q` / `a c` keys are
  released.

## Read in this order

1. [`requirements.md`](requirements.md) — the numbered requirements
   (REQ-001…REQ-033) covering the eight redesign areas, the hard
   constraints, and the acceptance criteria for this planning task.
2. [`design.md`](design.md) — the thesis, non-goals, the concrete
   magit/forge **extension seam list with signatures**, the target **module
   map**, the **navigation + keymap contract**, the **data/async model**, the
   **faces/design language**, the standalone sling abstraction, the
   agent-launch and formula designs, and the `gascity-terminal` →
   `beads-terminal-tmux` code-movement plan.
3. [`menu-mockups.md`](menu-mockups.md) — ASCII renderings of **every**
   redesigned surface in **every relevant state** (loading/empty/populated/
   folded/error, narrow windows, long titles, no matches, the sling and
   agent-launch shapes, formula browse/launch/follow, terminal scroll) with
   the real rendering rules (groups, collapse, keys) and key-flow traces.
4. [`slimming.md`](slimming.md) — the surface-area reduction audit:
   every removed/collapsed menu, command and agent role, with its
   justification and replacement (including the role roster).
5. [`implementation-plan.md`](implementation-plan.md) — the ordered,
   **pruning-first** implementation plan (WI-1…WI-20).
6. [`decomposition.md`](decomposition.md) — the work items with
   dependencies and REQ traceability (the later implementation turns these
   into beads).
7. [`plan-review.md`](plan-review.md) — the round-1 critique, its findings,
   and the sign-off checklist.

## The shape of the change (one paragraph)

`beads.el` becomes a deliberately designed, keyboard-driven porcelain:
`M-x beads` opens a **status buffer** (not a transient); list, detail,
formula and session views are sectioned and obey one navigation contract
(`q`/`g`/`TAB`/`SPC`/`RET`/`?`); the `beads-meta` command classes remain the
execution + parse layer only; a standalone **sling** abstraction dispatches a
bead to a local agent target and extends to gascity.el (`gc sling`) through
documented seams; **terminal handling moves** from gascity.el into
beads.el; and the surface is **slimmed** — deprecated and duplicate menus go,
generated transients are demoted to a dispatch backend, and the exposed agent
role roster shrinks from five roles to three (QA is removed and folds into
Review as a QA mode; Custom is removed and becomes the sling freeform path)
with the registries left open.

## Non-negotiables

- Hand-built UI only; generated transients are a dispatch backend, never the
  porcelain.
- beads.el must **not** depend on gascity.el; gascity.el is a thin extension.
- gascity.el integration is optional at runtime.
- No `.el` source file is modified by this planning task.
