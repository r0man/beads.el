# beads.el key registry

This file is the **beads.el half** of the shared, cross-package key
registry agreed with gascity.el (see gascity.el's `docs/key-registry.md`;
the tables are kept identical).  It ends the silent meaning-swap of
`d`, `s`, `w`, `i`, `t`, `D`, `C`, `#` between beads and gascity
buffers (plan F1) and gives `docs/defcommand-redesign.md` and the
README a single place to point at.

`lisp/test/beads-key-registry-test.el` enforces the table: it asserts
the **effective** (post-inheritance) binding of every key below in the
mode's keymap, so a parent redefinition can no longer re-point a
contract key without failing CI (plan F11).  The gascity.el registry
test asserts the same cross-package rule from its side.

## Universal navigation contract

These keys mean the same thing in **every** beads mode.

| Key | Meaning | list | show | dashboard | section |
|---|---|---|---|---|---|
| `q` | quit/bury | `beads-list-quit` | `quit-window` | `quit-window` | `beads-section-quit` |
| `g` | refresh | `beads-list-refresh` | `beads-refresh-show` | `beads-dashboard-refresh-dispatch` | `beads-section-refresh` |
| `TAB` | next thing | `beads-thing-forward` | `beads-thing-forward` | `beads-thing-forward` | `beads-thing-forward` |
| `S-TAB` | prev thing | `beads-thing-backward` | `beads-thing-backward` | `beads-thing-backward` | `beads-thing-backward` |
| `SPC` | fold/toggle | `beads-thing-toggle` | `beads-thing-toggle` | `beads-thing-toggle` | `beads-thing-toggle` |
| `RET` | visit/activate | `beads-list-show` | `beads-show-follow-reference` | `beads-dashboard-visit-at-point` | `beads-section-visit-issue` |
| `n` / `p` | next/prev item | `beads-list-next` / `beads-list-previous` | `beads-show-next-section` / `beads-show-previous-section` | `beads-dashboard-next-item` / `beads-dashboard-previous-item` | — |
| `?` | dispatch | `beads-dispatch` | `beads-dispatch` | `beads-dispatch` | `beads-dispatch` |

`N` / `P` move between sections in `beads-list` and
`beads-dashboard`; `beads-show` deliberately binds `N` elsewhere and
is recorded as a declared exception below.

The `?` dispatch binding is installed through the universal navigation
contract (`beads-mode--install-navigation-keys`); every primary view
binds it, including the formula list/show and swarm waves views added
during the discoverability pass.  `beads-key-registry-test.el` asserts
the effective binding for the four mode columns above and
`beads-discoverability-test.el` covers the remaining primary views by
invoking each mode and inspecting its local map.

## At-point / scope keys

Keys are keyed by the **type at point**, not by the mode.  A bead row
and a session row may share a key only when the verb is the same.

| Key | Bead scope | Agent/session scope | City/rig scope | List scope |
|---|---|---|---|---|
| `RET` | visit bead | detail | drill in | drill in |
| `SPC` | toggle/fold | toggle drawer | fold | row detail |
| `d` | **close bead** | — | — | close bead |
| `D` | delete (menu) | **drain** | — | — |
| `s` | set status | suspend | suspend rig | — |
| `S` | sort list *(beads exception)* | sling work | sling | sort *(beads exception)* |
| `w` | copy id | wake | resume | copy |
| `C` | claim | — | — | claim |
| `#` | priority | — | — | priority |
| `x` | — | — | — | clear filter |
| `?` | `beads-dispatch` | `gascity-dispatch` | `gascity-dispatch` | per package |

### beads.el bindings enforced by the test

| Key | list | show | dashboard |
|---|---|---|---|
| `d` | `beads-actions-close` | `beads-actions-close` | — |
| `s` | `beads-actions-set-status` | `beads-actions-set-status` | — |
| `C` | `beads-actions-claim` | `beads-actions-claim` | — |
| `#` | `beads-actions-set-priority` | `beads-actions-set-priority` | — |
| `w` | `beads-list-copy-id` | `beads-show-copy-id` | — |
| `x` | `beads-list-clear-filter` | — | — |

### Declared exceptions

* `S` is **sort**, not sling, in `beads-list-mode-map` and
  `beads-show-mode-map`.  Sling is a gascity verb; beads list keeps
  the long-standing Magit-style sort binding, so gascity may not reuse
  `S` for a non-sort verb in a beads list.
* `beads-show-mode-map` binds `N` to `beads-show-compose-comment` and
  `D` to `beads-formula-distill`; `beads-section-mode-map` carries no
  `n`/`p`/`N`/`P` of its own.
