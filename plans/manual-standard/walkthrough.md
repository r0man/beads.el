# Fresh-`emacs -Q` walkthrough — evidence and gap fixes

Work item: WI-12 (`be-pdiac`).  Requirements: REQ-011, REQ-012.

This artifact records the fresh-`emacs -Q` walkthrough of the manual and the
gaps it surfaced.  It accompanies the automated half of the same walkthrough,
`lisp/test/beads-manual-walkthrough-test.el`.

## What proves "passes using only the manual"

"Passes using only the manual" is proven by **two** artifacts, not by a
literal fresh-daemon manual run inside ERT:

1. The `:integration` test
   `lisp/test/beads-manual-walkthrough-test.el` drives the documented path
   in a throwaway embedded-Dolt store with only the setup the manual
   documents (the package loaded from `lisp/` plus `bd` on `PATH`).  It
   asserts every documented entry point exists and that the documented
   result is reached.
2. This file records the **interactive** fresh-`emacs -Q` run -- the
   keystrokes, the screens reached, the gaps found, and the fixes applied.

ERT cannot run a literal interactive `emacs -Q` daemon session, so the
literal interactive half is captured here rather than re-derived inside the
test.  This is stated explicitly as the plan requires.

## Method

* Throwaway repository under `mktemp` (`/tmp/manual-walkthrough-14682`),
  `git init` + `bd init` (`bd` 1.3.1, embedded Dolt).  No real project or
  host store was touched; every isolation variable from
  `beads-test-isolation-env-vars` was unset before `bd` and before Emacs.
* `emacs -Q -nw` inside a tmux session, with only:

  ```sh
  (add-to-list 'load-path ".../beads.el/worktrees/be-pdiac/lisp")
  (require 'beads)
  ```

  This is exactly the manual's "Installing from Source" setup.  The explicit
  `autoload` forms in `beads.el` are what a plain `-L lisp` checkout relies
  on (the `;;;###autoload` comments need a generated loaddefs file, the
  executable forms do not), so the documented `M-x beads`,
  `M-x beads-dispatch`, `M-x beads-maintenance`, `M-x beads-epic-menu`,
  and `M-x beads-show-actions` are all reachable.
* Steps were driven with tmux `send-keys` and read back with
  `capture-pane`; the observed screens are summarised under each step.

## Steps observed

| # | Step | Manual location | Result |
| - | ---- | --------------- | ------ |
| 1 | Load the package (`load-path` + `require`) | Installation and Requirements | OK; `featurep 'beads` |
| 2 | `M-x beads` | Getting Started / The Board | Board buffer opened, `beads-dashboard-mode`, header names the store |
| 3 | `?` `c`, type title + description, `C-c C-c` | Your First Issue / Creating Issues | Compose buffer `*beads-create[...]*`; issue created and message shown |
| 4 | `?` `l` | Listing Issues | `*beads-list[...]*` in `beads-list-mode`, issue listed |
| 5 | `RET` on the row | Viewing Issues | `*beads-show[...]/ISSUE*` in `beads-show-mode` |
| 6 | `d`, type a reason, `RET` | Closing and Reopening | Prompts `Close <id> reason:`; issue leaves Open, `g` refreshes |
| 7 | `?` `d` `a`, set target + `-t blocks`, `x` | Dependencies | "Dependency added: <src> blocks <dst>" |
| 8 | `?` `F` | Formulas | Formula menu: `l` list, `s` show, `c` convert |
| 9 | `?` `m` | Molecules | Molecule menu matches the documented `p w b d s c P S B r t e l` tables |
| 10 | `?` `!` | The Dispatch and Maintenance Menus | Maintenance menu: Issue Lifecycle / Views / Details / Events / Workflow / Database |
| 11 | `?` from the board | Transients | Groups Issues / Workflow / Views / Manage + Maintenance row; no `E` key |

The cook → pour → mol-show flow (formula file under
`.beads/formulas/`, `beads-command-cook`, `beads-mol-pour`,
`beads-mol-show`) is exercised start to finish by the `:integration` test;
the interactive run confirmed the documented menu entry points for it.

## Gaps found and fixes

Each gap is a place where the prose told the reader to do something that
does not work, or failed to explain what they must do.

1. **Epic menu key did not exist.**  `doc/formulas.texi` said "Press `E`
   from the dispatch menu" for the epic menu, but `beads-dispatch` binds no
   `E` (`E` is unbound in the Views group); a reader following the manual
   gets an error.  `beads-epic-menu` exists and is autoloaded, it is simply
   not on a menu.
   *Fix:* `beads-epic-menu` now documents `M-x beads-epic-menu` and states
   that it is not bound to a dispatch key.  (Runtime change deliberately
   not made: REQ-017 forbids editing `lisp/` runtime files, and the manual
   is made truthful instead.)

2. **Board buffer name used the wrong brackets.**  `doc/getting-started.texi`
   listed `*beads-dashboard[PROJECT]*`, but the board -- uniquely -- uses
   angle brackets: `*beads-dashboard<PROJECT>*` (remote:
   `*beads-dashboard<REMOTE|PROJECT>*`).  The list/ready/blocked/show/agent
   views do use square brackets.
   *Fix:* the buffer-name list now shows the board with angle brackets and
   notes the exception and the remote form.

3. **`?` in a show buffer.**  `doc/issue-workflow.texi` said `?` opens a
   persistent actions transient; it actually opens the shared dispatch
   menu.  The stay-open prefix `beads-show-actions` is defined and
   autoloaded but bound to no key.
   *Fix:* the paragraph now says `?` opens the dispatch menu and points at
   `M-x beads-show-actions` for the compact stay-open menu.

4. **`!` presented as a direct key.**  `doc/getting-started.texi`,
   `doc/formulas.texi`, and `doc/orchestration.texi` referred to the
   maintenance menu as `(!)`.  `!` is only bound inside the dispatch menu;
   pressing it in a view signals "undefined".
   *Fix:* all three now say the maintenance menu is reached with `? !`
   (or `M-x beads-maintenance`).

5. **Phantom "context group" in the dispatch menu.**  `doc/getting-started.texi`
   (twice) and `doc/orchestration.texi` claimed the dispatch menu "builds a
   context group from the view at point".  The menu is a fixed
   Issues/Workflow/Views/Manage set plus a Maintenance row plus
   `beads-menu-providers` groups; no context group is built, from any view.
   *Fix:* the claim is removed and replaced with the Maintenance row and the
   provider hook.

6. **No concrete formula → molecule flow.**  The chapter listed the formula
   and molecule menus but never explained how to get from a formula file to
   a poured, inspected molecule.
   *Fix:* `doc/formulas.texi` (Instantiate) now shows a minimal
   `.beads/formulas/<name>.formula.toml` and the `? ! K` (cook),
   `? m p` (pour), `? m s` (show) path to inspect it.

### Observations deliberately not changed

* The show view's `q` is `quit-window` and its footer says "bury" while the
  manual's key table says "Quit".  "Quit"/"bury" describe the same
  window-closing action and the manual is not misleading; left as is.
* The formula menu also has `S` (Schema index) and the molecule menu extra
  `o`/`a`/`=` verbs that the manual's short tables do not list.  Omission,
  not error; the full list lives in the Command and Variable Reference.

## Screenshots

No committed image needed regeneration.  Gaps 1–5 are prose/key-binding
errors and gap 6 is missing prose; the screens they describe
(dispatch, maintenance, board, list, show, compose, formula, molecule) are
unchanged, and the committed screenshots still illustrate exactly the
screens this walkthrough saw.  The deterministic capture pipeline and its
`--verify`/isolation contract are untouched.

## Verification

* `lisp/test/beads-manual-walkthrough-test.el` (`:integration`) passes:
  `1 tests, 1 results as expected, 0 unexpected`.
* `makeinfo doc/beads.texi` exits 0 with no warnings after the edits.
* `scripts/dispatch-docs --check` reports `dispatch docs ok` (the edits are
  outside the generated regions).
* Only `doc/*.texi`, `NEWS.md`, `plans/manual-standard/`, and test/support
  files changed; no `lisp/` runtime file was touched (REQ-017).
