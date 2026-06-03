# Plan: shape beads.el's public API so gascity.el builds on it cleanly

## Status: PLAN (not implemented) — `bde-9c5z`

This is a **design/plan document only**. It proposes changes to
**beads.el**'s public API so that **gascity.el** (a separate rig, the
`gc`/Gas City porcelain) can build on beads.el with fewer private-API
workarounds, less duplicated structure, and more declarative command
definitions. **No code is changed by this bead.** Each phase below is
independently shippable behind a backward-compatible alias.

> All `bde-*` / `gce-*` identifiers are local beads issue IDs in their
> respective `.beads/` Dolt databases; resolve with `bd show <id>`.
> They are not addressable via a public URL and are intentionally not
> hyperlinked.

### Repos analyzed (read-only)

| Repo | Role | Path | Size (lisp/) |
|------|------|------|--------------|
| beads.el | library (this rig) | `/home/roman/workspace/beads.el/lisp/` | ~90 modules |
| gascity.el | consumer (other rig) | `/home/roman/workspace/gascity.el/lisp/` | 15 modules, ~4,885 lines |

`gascity.el` already builds on `beads.el`: it `require`s `beads-meta`,
`beads-command`, `beads-section`, `beads-terminal`, and soft-requires
`beads-command-show` and `beads-dashboard`. The coupling works today —
the goal is to convert each *implicit / private* coupling into an
*explicit / public* contract, and to remove the duplicated structure
that both libraries currently carry.

---

## Executive summary

There are **seven coupling sites**. Ranked by leverage:

1. **`gascity-defcommand` is a hand-maintained fork of `beads-defcommand`**
   (gascity-command.el:50–108 ↔ beads-command.el:92–280). The fork
   exists only because the macro's reusable skeleton was never
   extracted. It drags in four `beads--` (private) helper calls.
2. **Four private (`beads--`) macro-helpers leak** into that fork:
   `beads--extract-option`, `beads--derive-transient-name`,
   `beads--extract-first-sentence`, `beads--current-feature-name`.
3. **The transient extension contract is undocumented.** gascity must
   know (by reading source) that `beads-meta-define-transient` emits
   suffixes calling three `beads-command-*` generics, then delegate
   them back (gascity-command.el:223–233).
4. **Store scoping is a monkeypatch.** gascity rebinds `beads-execute`
   with `cl-letf` to inject `:directory` (gascity-section.el:366–374)
   because `beads-show`/`beads-dashboard` don't expose a store
   argument — even though the underlying `beads-command-show` class
   **already has a `:directory` slot** (beads-command.el:318).
5. **Pagination is duplicated** ~50 lines: `beads-pager--*`
   (beads-pager.el:51–91, private, unused by the dashboard) ↔
   `gascity-tabulated--*-page-*` (gascity-tabulated.el:238–262).
6. **gascity has no typed domain model.** It juggles raw alists/plists
   for rig/session/agent/convoy/mail/order, while beads.el has a
   reflective JSON→EIEIO engine (`beads-from-json`, beads-meta.el:311–338)
   that is already library-neutral and could decode `gc --json` too.
7. **The section/mode reuse is healthy** and is the template the rest
   should follow: `gascity-section-mode` derives from
   `beads-section-mode` (gascity-section.el:134) with a `:parent`
   keymap (gascity-section.el:113). Keep this; document it as the
   blessed extension pattern.

Net effect of the full rollout: **−1 forked macro, −4 private-symbol
references, −~50 duplicated pagination lines**, a documented extension
API, and an optional typed-domain-object layer that removes scattered
`alist-get`/`plist-get` juggling across three gascity view modules.

```
                         TODAY                                  TARGET
   gascity-command.el ──fork──▶ beads-defcommand     gascity-defcommand ─wraps─▶ beads-meta-defcommand
        │  calls beads--extract-option (priv)              │  calls beads-meta-extract-option (pub)
        │  calls beads--derive-transient-name (priv)       │  calls beads-meta-derive-transient-name (pub)
        │  calls beads--extract-first-sentence (priv)      │  calls beads-meta-first-sentence (pub)
        │  calls beads--current-feature-name (priv)        │  calls beads-meta-current-feature-name (pub)
   gascity-section.el ─cl-letf rebind▶ beads-execute  gascity ─▶ (beads-show id :directory store)  [pub arg]
   gascity-tabulated.el ─copy──▶ beads-pager (priv)   gascity ─require─▶ beads-pager (pub)
   gascity raw alists/plists                          gascity-rig / -session / -agent  via beads-from-json
```

---

## 1. Current-coupling inventory (the workaround census)

Every cross-repo reference, with file:line in **both** repos. Privacy
is by the doubledash convention: `beads--foo` = private,
`beads-foo` = public.

### 1A. Private-function leaks (`beads--*` called from gascity) — **HIGH risk**

All four are called at macro-expansion time inside `gascity-defcommand`.

| beads.el private fn | defined at | called from gascity at | what it does |
|---|---|---|---|
| `beads--extract-option` | beads-command.el:79 | gascity-command.el:75, :78 | pull `(VALUE . REST)` for a keyword out of a `defclass` options plist |
| `beads--derive-transient-name` | beads-command.el:72 | gascity-command.el:90 | `gascity-command-status` → `gascity-status` (strip `-command-`) |
| `beads--extract-first-sentence` | beads-meta.el:113 | gascity-command.el:95 | first sentence of a docstring → transient short-doc |
| `beads--current-feature-name` | beads-meta.el:102 | gascity-command.el:96 | current file's feature name → `autoload` form target |

These are stable, library-neutral text/plist utilities, but the `--`
convention licenses beads.el to rename or relocate them without notice;
gascity would break silently at macroexpansion.

### 1B. Bridge generics (gascity specializes beads.el generics) — **HIGH risk**

`beads-meta-define-transient` (invoked at gascity-command.el:107)
generates transient suffixes that call three `beads-command-*`
generics. gascity therefore specializes them on its own
`gascity-command` base and forwards each to a parallel
`gascity-command-*` generic:

| beads.el generic | defined at | gascity `cl-defmethod` at | forwards to |
|---|---|---|---|
| `beads-command-validate` | beads-command.el:466 | gascity-command.el:223 | `gascity-command-validate` |
| `beads-command-execute-interactive` | beads-command.el:643 | gascity-command.el:227 | `gascity-command-execute-interactive` |
| `beads-command-preview` | beads-command.el:691 | gascity-command.el:231 | `gascity-command-preview` |

This is load-bearing and correct, but it is an **undocumented
extension contract** — gascity discovered it by reading beads.el
source (see the comment block at gascity-command.el:211–217).

### 1C. Public API consumed (works today; promote to *documented stable contract*)

| beads.el public symbol | defined at | used from gascity at | notes |
|---|---|---|---|
| `beads-meta-build-command-line` | beads-meta.el:965 | gascity-command.el:252 | argv from slot metadata — the core reuse |
| `beads-meta-slot-property` | beads-meta.el:733 | gascity-command.el:375, :377 | read `:long-option` / `:option-type` |
| `beads-meta-define-transient` | beads-meta.el:1510 | gascity-command.el:107 | generate the transient menu |
| `beads-section-mode` | beads-section.el:146 | gascity-section.el:134 | parent major mode (healthy reuse) |
| `beads-section-mode-map` | beads-section.el:142 | gascity-section.el:113 | `:parent` keymap (healthy reuse) |
| `beads-terminal-spawn` + backend classes | beads-terminal.el:117 / :161,:266,:330,:386 | gascity-terminal.el:94 / :51–54 | terminal abstraction reuse |
| `beads-show` | beads-command-show.el:1564 | gascity-section.el:374 | issue detail view delegation |
| `beads-dashboard` | beads-dashboard.el:1040 | gascity-section.el:434, :453 | project board delegation |
| `beads-execute` | beads-command.el:1314 | gascity-section.el:366–374 | **monkeypatched** — see 1D |

### 1D. Store-scoping monkeypatch — **MEDIUM risk**

`gascity-beads--show-in-store` (gascity-section.el:350–375) needs
beads.el to act on a *specific* rig's `.beads/` store, but `beads-show`
takes only `issue-id` (beads-command-show.el:1564) and resolves the
store from `default-directory`. The shared Dolt server can misroute
that working dir to a different rig's database, so gascity forces
`bd --directory` by **rebinding `beads-execute`** to splice
`:directory` into the `beads-command-show` argument list:

```elisp
;; gascity-section.el:366–374 (the workaround)
(let ((base (symbol-function 'beads-execute)))
  (cl-letf (((symbol-function 'beads-execute)
             (lambda (class &rest args)
               (apply base class
                      (if (eq class 'beads-command-show)
                          (append args (list :directory store))
                        args)))))
    (beads-show id)))
```

The `beads-command-show` class **already accepts** `:directory` (it is
a slot on `beads-command-global-options`, beads-command.el:318,
serializing to `--directory DIR`). The only thing missing is a way to
pass it through the high-level `beads-show` entry point.

### 1E. Duplicated pagination — **LOW risk, easy win**

| concern | beads.el (private, unused by dashboard) | gascity.el (reimplemented) |
|---|---|---|
| compute page size from window | beads-pager.el:51 (`beads-pager--compute-page-size`) | gascity-tabulated.el:238 (`gascity-tabulated--compute-page-size`) |
| effective page size | beads-pager.el:57 | gascity-tabulated.el:243 |
| total pages | beads-pager.el:63 | gascity-tabulated.el:247 |
| page slice / start / end | beads-pager.el:69, :73, :91 | gascity-tabulated.el:253 (`--page-slice`) |

~95% identical window-height/clamp/slice logic, ~50 lines, two copies.
beads-pager's *internals* are private (`beads-pager--*`); only its
buffer-stateful wrappers (`beads-pager-set-entries` etc.,
beads-pager.el:108+) are public, and those assume beads-pager's own
buffer-local variables, so gascity could not reuse them and rolled its
own.

### 1F. The `gascity-defcommand` fork — **the root cause of 1A**

`gascity-defcommand` (gascity-command.el:50–108) duplicates the
structure of `beads-defcommand` (beads-command.el:92–280):

| step | beads-defcommand | gascity-defcommand |
|---|---|---|
| extract custom options | `beads--extract-option` ×4 (`:cli-command`,`:result`,`:json`,`:transient`) | `beads--extract-option` ×2 (`:global-section`,`:cli-command`) |
| class-allocated `cli-command` slot | beads-command.el:171–177 | gascity-command.el:81–87 |
| derive transient name | `beads--derive-transient-name` | `beads--derive-transient-name` |
| short-doc from docstring | `beads--extract-first-sentence` | `beads--extract-first-sentence` |
| autoload feature name | (via `beads--current-feature-name`) | `beads--current-feature-name` |
| emit `defclass` (eval-and-compile) | yes | yes |
| emit transient | `beads-option-global-section` (fixed) | caller's `:global-section` (parameterized) |

The fork's **deltas** from the original are small and clean:
gascity adds a `NAME!` bang-function (gascity-command.el:102–105),
parameterizes the global-section, and **omits** beads-specific
behavior (`beads--normalize-slot`, the `:result`/`:json` symbol
properties, `:transient :manual` mode). That small delta is exactly
what a parameterized skeleton macro should capture.

### 1G. EIEIO gap: gascity has no typed domain objects — **conciseness debt**

gascity decodes `gc --json` to raw alists/vectors
(`gascity-reader-parse-json`, gascity-reader.el:83) and threads them
untyped through every view. Representative juggling:

- `(alist-get 'name r)`, `(alist-get 'path alist)`, `(alist-get 'rigs …)` — gascity-section.el:327–329, :346–347, :422–423
- `(alist-get 'prefix r)` for id→store routing — gascity-section.el:346
- agents as plists: `(plist-get agent :work-dir)`, `(plist-get agent :name)` — gascity-section.el:447–448
- per-list `--entry` row mappers re-reading the same alist keys — gascity-tabulated.el:457–488, :572–594

beads.el solved the identical problem with typed classes + a reflective
decoder: `beads-from-json` (beads-meta.el:311–338) walks a class's
slot metadata, maps each `:json-key` to an initarg, coerces by `:type`,
and recurses into nested EIEIO classes. It is **already
library-neutral** — nothing in the default method is beads-specific.
gascity could define `gascity-rig` / `gascity-session` /
`gascity-agent` / … classes and decode straight into them.

---

## 2. Proposed public-API changes (signatures + before/after)

### 2.1 Promote the four macro-helpers to public `beads-meta-*` names

All four are generic; the natural home is the `beads-meta` namespace
(the slot-metadata + codegen engine gascity already depends on). Define
public names, keep the `beads--` names as obsolete aliases so nothing
breaks.

```elisp
;; In beads-meta.el — public names (move the two command.el helpers here
;; or re-export; both are pure functions with no beads-command deps):

(defalias 'beads-meta-extract-option        #'beads--extract-option)
(defalias 'beads-meta-derive-transient-name #'beads--derive-transient-name)
(defalias 'beads-meta-first-sentence        #'beads--extract-first-sentence)
(defalias 'beads-meta-current-feature-name  #'beads--current-feature-name)

;; Backward-compat (so existing beads.el internal callers are unaffected,
;; and any third party on the old names gets a deprecation warning):
(define-obsolete-function-alias 'beads--extract-option
  'beads-meta-extract-option "beads.el 0.N")
;; …same for the other three.
```

**gascity migration** (gascity-command.el):

```elisp
;; before
(beads--extract-option :global-section options)      ; :75
(beads--derive-transient-name name)                  ; :90
(beads--extract-first-sentence docstring)            ; :95
(beads--current-feature-name)                         ; :96
;; after
(beads-meta-extract-option :global-section options)
(beads-meta-derive-transient-name name)
(beads-meta-first-sentence docstring)
(beads-meta-current-feature-name)
```

Trivial, mechanical, fully backward-compatible. Resolves 1A.

### 2.2 A parameterized command-definition skeleton: `beads-meta-defcommand`

Factor the shared macro body so `beads-defcommand` and
`gascity-defcommand` both become thin wrappers. The library-specific
bits become parameters.

```elisp
(cl-defmacro beads-meta-defcommand (name superclasses slots
                                    &rest options
                                    &key global-section
                                         slot-normalizer       ; fn or nil
                                         symbol-properties      ; alist SYM→VALUE, or nil
                                         extra-forms            ; list of forms to splice (e.g. a bang fn)
                                    &allow-other-keys)
  "Engine behind `beads-defcommand' and `gascity-defcommand'.
Extracts :cli-command, derives the transient name/short-doc/feature,
normalizes SLOTS via SLOT-NORMALIZER (identity when nil), emits the
`defclass', applies SYMBOL-PROPERTIES, optionally emits a
`beads-meta-define-transient' against GLOBAL-SECTION, and splices
EXTRA-FORMS. Returns the full `progn'.")
```

`beads-defcommand` becomes (sketch):

```elisp
(defmacro beads-defcommand (name superclasses slots &rest options)
  `(beads-meta-defcommand ,name ,superclasses ,slots
     :global-section beads-option-global-section
     :slot-normalizer #'beads--normalize-slot
     :symbol-properties ,(beads--collect-result/json-props options)
     ,@options))
```

`gascity-defcommand` becomes (sketch):

```elisp
(defmacro gascity-defcommand (name superclasses slots &rest options)
  (let ((bang (intern (concat (symbol-name name) "!"))))
    `(beads-meta-defcommand ,name ,superclasses ,slots
       :global-section ,(plist-get options :global-section) ; caller-supplied
       :extra-forms ((defun ,bang (&rest args)
                       (oref (gascity-command-execute (apply #',name args)) result)))
       ,@options)))
```

This removes the structural duplication of 1F **and** every private
helper call of 1A in one move (the helper calls move inside
`beads-meta-defcommand`). It is the highest-leverage change, and the
most invasive — hence scheduled late (Phase 4) behind the cheaper
Phase 1 alias that already unblocks gascity.

> **Decision point for the maintainer:** if the extraction in 2.2 is
> judged too large to land safely, Phase 1 (2.1 aliases) alone fully
> removes the *private-API* risk; gascity keeps its own thin fork but
> calls only public names. 2.2 is the DRY purist's win, not a
> correctness requirement.

### 2.3 Document + stabilize the transient extension contract

No code change required for correctness — gascity's delegation already
works. The deliverable is a **published contract** so consumers don't
reverse-engineer it:

- In `beads-meta-define-transient`'s docstring (beads-meta.el:1510) and
  a new "Extending beads commands" section in the manual, state that
  generated suffixes invoke exactly these three generics, that they are
  the supported extension points, and that their signatures are stable:
  `beads-command-validate (command) → error-string|nil`,
  `beads-command-execute-interactive (command) → side effect`,
  `beads-command-preview (command) → string`.
- Optional convenience: ship a `beads-command-protocol` abstract mixin
  documenting the three generics as `cl-defgeneric` stubs, so a
  consumer `(defclass gascity-command (beads-command-protocol) …)`
  signals intent. (Low value; documentation is the real fix.)

Resolves 1B.

### 2.4 Thread a store argument through `beads-show` / `beads-dashboard`

The class already supports `:directory`; expose it on the entry points
so gascity can stop monkeypatching `beads-execute`.

```elisp
;; beads-command-show.el:1564 — add an optional keyword:
(cl-defun beads-show (issue-id &key directory)
  "Show detailed view of ISSUE-ID.
With DIRECTORY non-nil, act on the bead store at DIRECTORY (passed to
bd as --directory / -C) instead of resolving from `default-directory'."
  ...
  (beads-execute 'beads-command-show :issue-ids (list issue-id)
                 ;; only when non-nil, so existing callers are unchanged:
                 (when directory :directory) (when directory directory)))

;; beads-dashboard.el:1040 — formalize the directory it already honors:
(cl-defun beads-dashboard (&key directory)
  "Open the project board, optionally scoped to DIRECTORY's store.")
```

Both signatures stay backward-compatible (existing zero-arg / single-arg
calls are unaffected). **gascity migration** collapses
gascity-section.el:350–375 from the 25-line `cl-letf` wrapper to:

```elisp
;; before: gascity-beads--show-in-store rebinds beads-execute (…:366-374)
;; after:
(defun gascity-beads--show-in-store (id store)
  (if store (beads-show id :directory store) (beads-show id)))
```

and gascity-rig-beads / gascity-agent-beads (gascity-section.el:411–454)
drop the `(let ((default-directory dir)) (beads-dashboard))` dance for
`(beads-dashboard :directory dir)`. Resolves 1D.

> Optional sugar, if the maintainer prefers it over per-entry keywords:
> a `beads-with-store` macro —
> `(defmacro beads-with-store (dir &rest body) …)` — that binds the
> resolution once for any nested high-level call. The keyword approach
> is recommended (explicit, discoverable, no dynamic-scope surprises).

### 2.5 Promote pagination to a reusable, buffer-agnostic public API

Extract the pure arithmetic (no buffer-locals) into public functions
both libraries call; keep the stateful wrappers as a thin layer on top.

```elisp
;; beads-pager.el — promote the pure core (currently beads-pager--*):
(defun beads-pager-page-count (total page-size) …)   ; was --total-pages
(defun beads-pager-page-bounds (page page-size total) ; → (START . END)
  …)                                                   ; was --page-start/--page-end
(defun beads-pager-window-page-size (&optional window) ; was --compute-page-size
  …)
(defun beads-pager-slice (entries page page-size) …)  ; pure list slice
```

The existing buffer-stateful `beads-pager-set-entries` /
`-next-page` / `-prev-page` (beads-pager.el:108+) re-implement on top of
these pure functions — no behavior change for beads.el's own list
buffers. **gascity migration** deletes
gascity-tabulated.el:238–262 and calls `beads-pager-window-page-size`,
`beads-pager-page-count`, `beads-pager-slice` directly. Net ≈ −50 lines
in gascity, single source of truth. Resolves 1E.

---

## 3. EIEIO recommendations

### 3.1 Adopt typed domain objects in gascity via the reflective decoder

**Recommendation:** publish `beads-from-json` (already public at
beads-meta.el:311) explicitly as a **library-neutral** decoder, and
have gascity define EIEIO classes for its domain payloads decoded by it.

`beads-from-json`'s default method (beads-meta.el:320–338) is already
generic: it walks `beads-meta-command-slots`, maps each slot's
`:json-key` (beads-meta.el:833, default = slot-name with `-`→`_`) to an
initarg, coerces via `beads-coerce-json-value` by `:type`, and recurses
into nested EIEIO classes. Nothing in it is beads-specific.

**Proposed gascity classes** (new `gascity-domain.el`), e.g.:

```elisp
(defclass gascity-rig ()
  ((name   :initarg :name   :type (or null string) :json-key name)
   (path   :initarg :path   :type (or null string) :json-key path)
   (prefix :initarg :prefix :type (or null string) :json-key prefix)
   (status :initarg :status :type (or null string) :json-key status))
  :documentation "A rig as reported by `gc rig list'.")

(defclass gascity-session ()
  ((name      :initarg :name      :type (or null string) :json-key session_name)
   (rig       :initarg :rig       :type (or null string) :json-key rig)
   (state     :initarg :state     :type (or null string) :json-key state)
   (work-dir  :initarg :work-dir  :type (or null string) :json-key work_dir))
  :documentation "An agent session as reported by `gc session list'.")
;; …and gascity-agent / gascity-convoy / gascity-mail / gascity-order.
```

**Before** (scattered, untyped — gascity-section.el:327–348):

```elisp
(seq-find (lambda (r) (equal (alist-get 'name r) rig))
          (append (alist-get 'rigs (gascity-command-rig-list!)) nil))
(alist-get 'path alist)
(alist-get 'prefix r)
```

**After** (typed accessors, one decode site):

```elisp
(seq-find (lambda (r) (equal (gascity-rig-name r) rig))
          (gascity-rigs))           ; decodes the vector once via beads-from-json
(gascity-rig-path rig-obj)
(gascity-rig-prefix r)
```

**Rationale.** This removes the `alist-get`/`plist-get` juggling
spread across `gascity-section.el`, `gascity-tabulated.el`, and
`gascity-status.el`; it gives the agent (currently a bare plist with
`:work-dir`/`:name`) a real type; and the `:json-key` slot metadata
becomes the single contract for "what shape does `gc` return," replacing
the ad-hoc symbol literals re-typed at each call site. Polymorphism wins
where the views currently `pcase`/`cond` on payload kind (rig vs agent
vs convoy at-point): a `cl-defgeneric gascity-at-point-visit (obj)` with
one method per class replaces the precedence ladder in
`gascity-beads-at-point` (gascity-section.el:464+).

**Scope caution (where EIEIO does *not* pay).** Do **not** convert the
command-execution plumbing or the `gc` global flags into more classes —
that layer (`gascity-command*`) is already EIEIO and already shares
beads.el's engine. Keep the *mutating* command classes as-is. The win
is strictly the **read payloads** (rig/session/agent/convoy/mail/order),
which are today untyped. Converting only those is a self-contained
change with no effect on the command layer.

### 3.2 Do not over-classify

The `gascity-command-action` / streaming split (gascity-types.el:140–159)
and the three bridge generics are already the right amount of EIEIO.
This plan adds classes only for the untyped read payloads (3.1) and
the optional `beads-command-protocol` mixin (2.3) — nothing else.

---

## 4. DRY / dedup proposals (summary)

| # | Duplication / workaround | Fix | Removes |
|---|---|---|---|
| D1 | 4 private-helper calls (1A) | public `beads-meta-*` aliases (2.1) | private coupling |
| D2 | forked `gascity-defcommand` (1F) | `beads-meta-defcommand` skeleton (2.2) | ~1 forked macro |
| D3 | undocumented bridge contract (1B) | publish the 3-generic protocol (2.3) | reverse-engineering |
| D4 | `cl-letf` store monkeypatch (1D) | `:directory` on `beads-show`/`-dashboard` (2.4) | ~25 fragile lines |
| D5 | duplicated pagination (1E) | public `beads-pager-*` pure fns (2.5) | ~50 lines in gascity |
| D6 | untyped alist/plist juggling (1G) | typed classes + `beads-from-json` (3.1) | scattered `alist-get` |

---

## 5. Section / dashboard duplication (detail for D5 + context)

- **Section mode — already shared, keep as the template.**
  `gascity-section-mode` derives from `beads-section-mode`
  (gascity-section.el:134) and parents its keymap
  (gascity-section.el:113). This is the *good* pattern; the only action
  is to **document** the inheritance chain
  (`vui-mode → beads-section-mode → consumer-mode`) and the
  `:parent` keymap idiom in `beads-section.el`'s commentary so future
  consumers copy it deliberately rather than by archaeology.

- **Dashboard vs tabulated — genuinely different, do not force-merge.**
  beads-dashboard is a vui component tree with async per-section
  loaders and collapse state (beads-dashboard.el:181–218); gascity's
  status board is also vui (gascity-status.el:208–238) but its
  homogeneous lists are `tabulated-list-mode`
  (gascity-tabulated.el:525–550). These are different enough that the
  only shared substrate worth extracting is **pagination** (D5) and the
  **at-point text-property stamping** pattern. A full shared dashboard
  base is *not* recommended — the cost (generalizing beads-dashboard's
  issue-centric loaders to arbitrary entities) exceeds the benefit.

- **At-point resolution — pattern, not code, is shared.**
  beads stamps a `beads-section` text property and reads it back
  (beads-section.el:51–65); gascity stamps `gascity-agent` /
  `gascity-rig`. Rather than a generic resolver (low payoff), fold this
  into 3.1: once payloads are typed objects, "object at point" returns a
  typed instance and `cl-defgeneric` dispatch replaces the per-property
  readers.

---

## 6. Phased, low-risk rollout

Each phase is independently shippable, lands in **beads.el first**, and
is backward-compatible. gascity migrates in the *following* step, so the
two repos never have to land atomically.

| Phase | beads.el change | gascity.el follow-up | Risk | Backward-compat |
|---|---|---|---|---|
| **0. Docs** | Publish the extension API: bridge-generic protocol (2.3), mode-inheritance pattern (§5), store-scoping intent | none | none | n/a — docs only |
| **1. Promote helpers** | 4 public `beads-meta-*` aliases + obsolete aliases (2.1) | swap 4 call sites to public names | very low | obsolete aliases keep old names working |
| **2. Store argument** | `:directory` keyword on `beads-show` + `beads-dashboard` (2.4) | delete `cl-letf` wrapper; pass `:directory` | low | new optional keyword; old calls unchanged |
| **3. Pagination** | promote `beads-pager-*` pure fns (2.5) | delete `gascity-tabulated--*-page-*`; call public fns | low | beads internals re-implemented on the pure fns; no UX change |
| **4. defcommand skeleton** | extract `beads-meta-defcommand`; make `beads-defcommand` a wrapper (2.2) | make `gascity-defcommand` a wrapper | medium | beads-defcommand behavior unchanged (covered by its tests) |
| **5. Typed domain** | (none — `beads-from-json` already public; just document neutrality) | add `gascity-domain.el`; decode payloads; migrate views | medium | gascity-internal; no beads.el change |

**Gating per phase:** `eldev compile`, `eldev lint`, `eldev test` stay
green in beads.el after Phases 1–4; gascity's own suite stays green
after each follow-up. Phases 0–3 are mechanical and low-risk and could
land in one PR; Phases 4–5 each warrant their own PR with tests.

**Recommended first PR:** Phases 0–2 together. They remove the two
*highest-risk* couplings (private-helper leak, store monkeypatch) with
near-zero blast radius and unblock the rest.

---

## 7. Risks, non-goals, and open questions

**Risks**
- Phase 4 (`beads-meta-defcommand` extraction) touches the most-used
  macro in beads.el. Mitigate by extracting behind the existing
  `beads-defcommand` API and leaning on its current test suite; the
  macro's *output* must be byte-for-byte equivalent for beads' own
  commands.
- Phase 5 changes how gascity reads every payload; land it list-by-list
  (rig, then session, …) so a regression is isolated to one view.

**Non-goals**
- No change to the `bd`/`gc` CLIs or their JSON.
- No merge of the two dashboards (§5).
- No change to beads.el's own command set or behavior — only additive
  public surface + one internal refactor (Phase 4) that preserves
  output.

**Open questions for the maintainer**
1. Home for the promoted helpers — `beads-meta` (recommended, gascity
   already requires it) vs a new `beads-util`/`beads-macro` module?
2. Store scoping — per-entry `:directory` keyword (recommended) vs a
   `beads-with-store` dynamic macro (§2.4)?
3. Appetite for Phase 4 — ship the skeleton extraction, or stop at
   Phase 1 (aliases) and accept gascity's thin fork as permanent?

---

## 8. Acceptance mapping

| Acceptance criterion (`bde-9c5z`) | Section |
|---|---|
| Coupling / workaround inventory with file:line (both repos) | §1 (1A–1G) |
| Specific public-API proposals (signatures + before/after) | §2 (2.1–2.5) |
| EIEIO recommendations + rationale | §3 |
| DRY / dedup proposals | §4 (D1–D6), §5 |
| Phased rollout with backward-compat | §6 |
| Doc-only; `eldev`/lint stays green | no source changed by this bead |
