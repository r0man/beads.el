# beads-defcommand Redesign Plan

## Goal

Make `beads-defcommand` work like a regular `defclass` with enriched
slot metadata. Auto-generate transient menus and validation. Simplify
the execution contract: programmatic calls always return domain
objects, interactive calls always run in a terminal.

**The core insight**: by defining domain types (in `beads-types.el`)
with proper `:type` on every slot, and command classes (via
`beads-defcommand`) with `:result`, we get a **complete typed
Elisp API for the entire beads CLI** — for free. No hand-written
parsing, no hand-written validation. Define the types and the
commands; everything else is derived.

```
beads-types.el          beads-command-*.el
─────────────           ──────────────────
domain classes    +     command classes        =   full typed API
  (beads-issue,           (beads-command-close,
   beads-label,            beads-command-list,
   beads-stats-data)       ...)
       │                         │
       │  :type on slots         │  :result
       │  (introspection)        │  (auto-parse JSON)
       ▼                         ▼
  ┌─────────────────────────────────────────┐
  │  beads-command-execute                  │
  │    → clones cmd, sets json=t            │
  │    → runs bd CLI with --json            │
  │    → beads-from-json (introspection)    │
  │    → returns typed domain objects       │
  ├─────────────────────────────────────────┤
  │  beads-command-execute-interactive      │
  │    → json stays nil (default)           │
  │    → runs bd CLI in terminal            │
  │    → human-readable colored output      │
  ├─────────────────────────────────────────┤
  │  transient menu (auto-generated)        │
  │    → from slot :group/:short-option/etc │
  ├─────────────────────────────────────────┤
  │  validation (auto-generated)            │
  │    → from slot :required/:choices/:type │
  └─────────────────────────────────────────┘
```

## Design Decisions

### D1: Command immutability

**Decision: Clone in execution methods.**

The `json` slot defaults to `nil`. `beads-command-execute` clones
the command and sets `json` to `t` on the clone.
`beads-command-execute-interactive` uses the command as-is (json
is already nil). The original command object is never mutated.

The clone is shallow (EIEIO `clone` uses `copy-sequence` on the
internal vector). This is safe because execution methods only
mutate the `json` boolean on the clone. List-valued slots share
structure — do not mutate them on the clone.

### D2: Commands that don't return issues

**Decision: Every command declares `:result` (superseded by D10).**

Commands declare `:result` with the same type syntax as `:type`
on slots — including `(list-of T)`:

- Single object: `:result beads-issue`
- List of objects: `:result (list-of beads-issue)`
- Other domain type: `:result beads-stats-data`
- No structured result: omit `:result` (or `:result nil`) →
  raw parsed JSON as fallback

This eliminates the vectorp heuristic in `beads-command-parse` —
the declared type drives parsing through the same
`beads-coerce-json-value` pipeline used for slot types.
See D10 for full details.

### D3: Commands that don't support --json

**Decision: json defaults to nil; execute sets it to t when supported.**

The `json` slot on `beads-command-global-options` defaults to `nil`.
`beads-command-execute` clones and sets `json` to `t` only when
the class-level `:json` property is true (the default).

Commands that don't support `--json` declare it explicitly:

```elisp
(beads-defcommand beads-command-doctor (beads-command-global-options)
  ()
  :documentation "Run bd doctor."
  :json nil
  :transient nil)
```

The macro stores `:json` on a symbol property:
`(put 'beads-command-doctor 'beads-json nil)`. The default is `t`
— most commands support `--json`. `beads-command-execute` checks
this before setting the slot on the clone.

### D4: Transient generation

**Decision: Generate by default; three modes via `:transient`.**

Every `beads-defcommand` gets a transient menu by default. The
transient name is derived from the class name
(`beads-command-close` -> `beads-close`). The docstring comes
from `:documentation`. The global options section is always
appended when a transient is generated.

Three values for `:transient`:

- omitted / `t` — auto-generate (default)
- `nil` — no transient at all (e.g., `doctor`, `init`)
- `:manual` — skip auto-generation; a hand-written
  `transient-define-prefix` follows in the same file

Commands with `:transient :manual` define their own transient
immediately after the `beads-defcommand` form:

```elisp
(beads-defcommand beads-command-list (beads-command-global-options)
  ((status ...))
  :documentation "List issues."
  :result (list-of beads-issue)
  :transient :manual)

;; Hand-written transient for magit-log-style UX
(transient-define-prefix beads-list ()
  "List issues with filters."
  ["Filters"
   ("-s" "Status" "--status=" :reader beads-read-status)]
  ["List"
   ("l" "All issues" beads-list-all)
   ("r" "Ready"      beads-list-ready)])
```

### D5: Bang functions

**Decision: Remove entirely. Replace with `beads-execute` /
`beads-execute-async` convenience functions.**

No more per-command `beads-command-close!`, `beads-command-create!`,
etc. Two generic shorthands replace all ~40 bang functions:

```elisp
;; Sync shorthand:
(beads-execute 'beads-command-close :issue-ids '("bd-1") :reason "done")
;; => beads-issue object

;; Async shorthand:
(beads-execute-async 'beads-command-close
  #'on-success #'on-error
  :issue-ids '("bd-1") :reason "done")

;; Full form (when you need the command object):
(beads-command-execute (beads-command-close :issue-ids '("bd-1") :reason "done"))
```

Implementation:

```elisp
(defun beads-execute (class &rest args)
  "Construct CLASS with ARGS and execute it.
Returns parsed result (domain objects, JSON, or raw stdout)."
  (unless (find-class class nil)
    (error "Not a beads command class: %S" class))
  (beads-command-execute (apply #'make-instance class args)))

(defun beads-execute-async (class on-success &optional on-error
                            &rest args)
  "Construct CLASS with ARGS and execute asynchronously.
ON-SUCCESS receives the parsed result.
ON-ERROR receives the condition; nil means display via `beads--error'."
  (unless (find-class class nil)
    (error "Not a beads command class: %S" class))
  (beads-command-execute-async
   (apply #'make-instance class args) on-success on-error))
```

No deprecation period. No backward compatibility. Pre-1.0 project.

### D6: Execution object

**Decision: Remove entirely.**

`beads-command-execution` class is deleted. `beads-command-execute`
returns the parsed result directly (domain objects, raw JSON, or
raw stdout). Error conditions (`beads-command-error`,
`beads-json-parse-error`, `beads-validation-error`) carry all
diagnostic info (exit-code, stdout, stderr) in the condition data.

Nil return on success is valid (e.g., empty list from `bd list`).
Errors are always signaled — if `beads-command-execute` returns
without signaling, the command succeeded.

### D7: Async signature

**Decision: Positional args, on-error optional.**

```elisp
(beads-command-execute-async command on-success &optional on-error)
```

- `on-success` receives the parsed result (same as sync return)
- `on-error` receives the condition; nil (default) means display
  error via `beads--error` — never signal from a sentinel

### D8: Three-level validation via generic methods

**Decision: No generated validation code. Base methods handle
everything via slot metadata introspection.**

Three generic methods form a validation hierarchy:

**Level 1: Per-slot** — `beads-command-validate-slot` checks a
single slot using metadata. The base method handles `:required`
and `:choices` generically. Override with `(eql SLOT-NAME)` for
custom constraints on specific fields.

**Level 2: All-slots** — `beads-command-validate-slots` loops
over slots, calling `validate-slot` for each, collecting all
errors. Override for custom iteration logic.

**Level 3: Full command** — `beads-command-validate` delegates
to `validate-slots` by default. Override for cross-field rules.

```elisp
;; Base method: handles :required and :choices for ANY slot
;; on ANY command via metadata introspection. No code generation.
(cl-defmethod beads-command-validate-slot ((cmd beads-command)
                                           slot-name value)
  "Validate VALUE for SLOT-NAME on CMD using slot metadata."
  (let ((required (beads-meta-slot-property cmd slot-name :required))
        (choices  (beads-meta-slot-property cmd slot-name :choices)))
    (cond
     ((and required (or (null value)
                        (and (stringp value) (string-empty-p value))
                        (and (listp value) (null value))))
      (format "%s is required" slot-name))
     ((and choices value (not (member value choices)))
      (format "%s must be one of: %s" slot-name
              (string-join choices ", "))))))

;; Base method: loops over all slots, collects errors
(cl-defmethod beads-command-validate-slots ((cmd beads-command))
  "Validate all slots using metadata. Returns list of error strings."
  (let (errors)
    (dolist (slot (beads-meta-command-slots
                   (eieio-object-class cmd)))
      (when-let ((err (beads-command-validate-slot
                       cmd
                       (beads-meta-slot-name slot)
                       (slot-value cmd (beads-meta-slot-name slot)))))
        (push err errors)))
    (nreverse errors)))

;; Base validate delegates:
(cl-defmethod beads-command-validate ((cmd beads-command))
  (beads-command-validate-slots cmd))

;; Per-slot override: custom constraint on one field
(cl-defmethod beads-command-validate-slot
    ((cmd beads-command-create) (_slot (eql title)) value)
  (when (and value (> (length value) 200))
    "title must be 200 characters or fewer"))

;; Full command override: cross-field rules
(cl-defmethod beads-command-validate ((cmd beads-command-create))
  (with-slots (title file) cmd
    (cond
     ((and (not title) (not file))
      "Must provide either title or --file")
     ((and title file)
      "Cannot use both title and --file"))))
```

The macro generates **zero** validation code. All standard
validation (`:required`, `:choices`) is handled by the base
`beads-command-validate-slot` method reading slot metadata at
runtime.

Note: `(list-of T)` on slot `:type` is used for introspection
and documentation, not runtime element validation. EIEIO's
element-level type checking is version-dependent and potentially
O(n). The actual enforcement points are `beads-from-json` (which
constructs values correctly by type) and
`beads-command-validate-slot` (which checks constraints at
submission time).

### D9: Execute-interactive

**Decision: Default is terminal, override for message-style.**

The default `beads-command-execute-interactive` on `beads-command`
runs the command in a terminal buffer (vterm > eat > term). The
`json` slot is nil by default, so no clone needed.

Terminal-by-default is intentional: the `bd` CLI produces colored,
formatted output designed for human consumption. Process buffers
strip or mangle ANSI codes. The terminal preserves the exact
experience `bd` was designed for.

Commands that want the "execute -> message -> refresh" pattern
(close, reopen, update, etc.) override with a `cl-defmethod` that
calls `beads-command-execute` programmatically.

A shared utility `beads-execute-with-message` is provided for
the common message-style pattern:

```elisp
(defun beads-execute-with-message (cmd format-fn)
  "Execute CMD programmatically, call FORMAT-FN with result.
Handles errors gracefully with user-facing messages."
  (condition-case err
      (let ((result (beads-command-execute cmd)))
        (funcall format-fn result))
    (beads-validation-error
     (user-error "%s" (cadr err)))
    (beads-command-error
     (user-error "Command failed: %s" (plist-get (cddr err) :stderr)))))
```

### D10: Automatic JSON-to-domain-object parsing via `:result`

**Decision: Declare result type on command, auto-parse via EIEIO
introspection.**

Each command declares a `:result` macro keyword specifying the
return type — either a domain class symbol (e.g. `beads-issue`)
or `(list-of CLASS)` for commands returning multiple objects.
The macro stores it on a symbol property (not a slot — avoids
collision with command arguments and class-allocated mutation
risks). The base `beads-command-parse` delegates to
`beads-coerce-json-value` with the declared type, using the same
coercion pipeline as slot types — no special-case vectorp
heuristics.

This eliminates all hand-written `*-from-json` functions and all
per-command `beads-command-parse` overrides. Users can still
customize parsing by implementing a `cl-defmethod` on
`beads-from-json` for their domain class, or on
`beads-command-parse` for their command class.

#### How it works

**1. Command declares result type:**

```elisp
(beads-defcommand beads-command-close (beads-command-global-options)
  ((issue-ids ...)
   (reason ...))
  :documentation "Close one or more issues."
  :result (list-of beads-issue))
```

The macro stores it:

```elisp
(put 'beads-command-close 'beads-result '(list-of beads-issue))
```

**2. Generic `beads-from-json` constructs any EIEIO object:**

```elisp
(cl-defgeneric beads-from-json (class json-alist)
  "Construct an instance of CLASS from JSON-ALIST.
Walks CLASS slots via EIEIO introspection (through beads-meta
abstraction layer), maps JSON keys to slot initargs, and
recursively coerces values based on :type.

Override this method for classes with non-standard JSON shapes.
Unknown JSON fields are silently dropped.")
```

The default method is unspecialized — works for ANY EIEIO class.
Only passes initargs for keys actually present in the JSON alist,
so absent fields use the slot's `:initform` default while explicit
nulls get nil:

```elisp
;; Default: generic introspection for all classes
(cl-defmethod beads-from-json (class json-alist)
  (let (initargs)
    (dolist (slot (beads-meta-command-slots class))
      (let* ((json-key (beads-meta--slot-json-key slot))
             (pair (assq json-key json-alist)))
        (when pair  ;; key exists in JSON, even if value is nil
          (push (beads-meta-slot-initarg slot) initargs)
          (push (beads-coerce-json-value
                 (cdr pair) (beads-meta-slot-type slot))
                initargs))))
    (apply #'make-instance class (nreverse initargs))))

;; Override for non-standard JSON shape:
(cl-defmethod beads-from-json ((class (subclass beads-stats-data)) json)
  ;; custom parsing for stats' nested structure
  ...)
```

**3. Recursive type coercion via extensible generic:**

```elisp
(cl-defgeneric beads-coerce-json-value (value type)
  "Coerce JSON VALUE to match EIEIO TYPE spec.
Extensible — add methods for custom types.")

(cl-defmethod beads-coerce-json-value (value (_type (eql boolean)))
  (not (eq value :json-false)))

(cl-defmethod beads-coerce-json-value (value (_type (eql integer)))
  (if (stringp value) (string-to-number value) value))

(cl-defmethod beads-coerce-json-value (value (_type (eql string)))
  value)

;; list-of and nullable types handled by pcase in a primary
;; method since compound types are not dispatchable via eql:
(cl-defmethod beads-coerce-json-value (value type)
  "Default: handle nullable, (list-of X), and EIEIO class types."
  (let ((core (beads-meta--unwrap-nullable type)))
    (if (not (eq core type))
        ;; Was nullable — nil passes through, non-nil recurses
        (when value (beads-coerce-json-value value core))
      (pcase type
        (`(list-of ,elem)
         (mapcar (lambda (v) (beads-coerce-json-value v elem))
                 (append value nil)))
        ;; If type is an EIEIO class, recurse
        (_ (if (and (symbolp type) (find-class type nil))
               (beads-from-json type value)
             value))))))
```

The `beads-meta--unwrap-nullable` utility handles arbitrary
`(or ...)` forms, not just two-element `(or null X)`:

```elisp
(defun beads-meta--unwrap-nullable (type)
  "Extract core type from (or ...) forms containing null.
Strips null, returns the remaining type. If multiple non-null
types remain, returns the most specific per priority:
list-of > integer > string."
  (pcase type
    (`(or . ,members)
     (let ((non-null (remq 'null members)))
       (pcase non-null
         ('() nil)
         (`(,single) single)
         (_ (beads-meta--most-specific-type non-null)))))
    (_ type)))
```

**4. Default `beads-command-parse` uses result type:**

```elisp
(cl-defmethod beads-command-parse ((cmd beads-command) stdout)
  (if (not (oref cmd json))
      stdout
    (let* ((json-null nil)
           (json-object-type 'alist)
           (json-array-type 'vector)
           (json-key-type 'symbol)
           (json (json-read-from-string stdout))
           (result-type (get (eieio-object-class cmd) 'beads-result)))
      (if (null result-type)
          json
        ;; Delegate to beads-coerce-json-value — handles single
        ;; objects, (list-of T), (or null T), and nested types
        ;; using the same pipeline as slot coercion.
        (beads-coerce-json-value json result-type)))))
```

Note: `json-null` is bound to `nil` so `:json-null` never appears
in parsed data. The `beads-from-json` default method preserves the
distinction between absent fields (slot uses `:initform` default)
and explicit JSON null (slot gets nil) by only passing initargs
for keys present in the JSON alist via `assq`.

**5. Users override at two levels:**

```elisp
;; Level 1: Override how a domain class parses from JSON
;; (affects ALL commands that return this type).
;; Use when the JSON shape is non-standard for the domain class.
(cl-defmethod beads-from-json ((class (subclass beads-stats-data)) json)
  ...)

;; Level 2: Override how a specific command parses its output
;; (affects only this command).
;; Use when the command's stdout is not standard JSON or when
;; the response wrapping differs from the vector/object convention.
(cl-defmethod beads-command-parse ((cmd beads-command-stats) stdout)
  ...)
```

When `beads-command-parse` is overridden and calls
`beads-from-json`, the Level 1 override still applies. When
`beads-command-parse` is overridden and does NOT call
`beads-from-json`, Level 1 is bypassed — this is explicit opt-out.

#### Known `beads-from-json` overrides needed

These domain classes have non-standard JSON shapes and will need
explicit overrides:

- `beads-stats-data` — nested wrapper with `summary` and
  `recent_activity` sub-objects
- `beads-dependency` — polymorphic JSON input (`type` vs
  `dependency_type` key depending on context)
- `beads-epic-status` — nested `epic` key containing an issue

#### Result type contract

`:result CLASS` means the JSON response is a single object.
`:result (list-of CLASS)` means the JSON response is an array.
Commands must match the actual bd CLI output shape. If bd returns
an array (even for single-element results like `bd close`), use
`(list-of T)` — never rely on automatic unwrapping.

#### What this eliminates

- All hand-written `*-from-json` functions in beads-types.el (~10)
- All per-command `beads-command-parse` overrides (~20)
- The `:parse-as` macro keyword

#### What's needed

- All domain classes in beads-types.el need proper `:type` on
  every slot, using `(list-of T)` for typed lists
- JSON key-to-slot mapping: hyphen-to-underscore convention
  (slot `issue-type` → JSON key `"issue_type"`)
- `:json-key` custom slot property for exceptions where the
  slot name doesn't map to the JSON key via convention
  (e.g., `beads-dependency` slot `dep-type` → JSON key
  `"dependency_type"`)
- Domain types for commands that currently return raw JSON
  (audit, config, count, compact, etc.)
- All introspection of EIEIO slots goes through the beads-meta
  abstraction layer (not `cl--` internal APIs directly)

### D11: Slot shorthand

**Decision: Deferred to follow-up (option B).**

Get the macro redesign landed first. Slot templates
(`:slot/issue-ids` etc.) are an independent feature.

### D12: Use standard `:type` instead of `:option-type`

**Decision: Replace `:option-type` with standard EIEIO `:type`.**

The CLI serialization behavior is inferred from the standard EIEIO
`:type` slot property. This eliminates a custom property, gives us
real runtime type validation from EIEIO for free, and uses the
built-in `(list-of ELEM)` type specifier (available since Emacs
27.1, defined in `eieio-core.el`).

#### Type inference algorithm

1. Normalize: unwrap `(or null X)` to extract the core type X
2. Match X against known types:

| `:type`                      | Inferred serialization | CLI output           |
|------------------------------|------------------------|----------------------|
| `boolean`                    | boolean                | `--flag`             |
| `integer`                    | integer                | `--flag N`           |
| `(or null integer)`          | integer                | `--flag N`           |
| `string`                     | string                 | `--flag value`       |
| `(or null string)`           | string                 | `--flag value`       |
| `(or null string integer)`   | string                 | `--flag value`       |
| `(list-of string)`           | list of strings        | `--flag a,b,c`       |
| `(list-of integer)`          | list of integers       | `--flag 1,2,3`       |
| `(or null (list-of string))` | list of strings        | `--flag a,b,c`       |
| `list`                       | list of strings        | `--flag a,b,c`       |
| omitted / `t`                | string                 | default fallback     |

3. Priority when multiple types appear in `or`: `list-of` >
   `integer` > `string`. The most specific non-null type wins.

#### Benefits

- **Introspection**: the type drives serialization, parsing, and
  transient generation automatically.
- **One less custom property**: `:option-type` is eliminated.
- **Standard Emacs Lisp**: no custom type system, uses what EIEIO
  already provides.
- **Element-typed lists**: `(list-of integer)` vs `(list-of string)`
  enables type-aware serialization and parsing.

Note: EIEIO's element-level type checking for `(list-of T)` is
version-dependent and potentially O(n) at set-time. Do not rely
on it for runtime safety. The actual enforcement points are
`beads-from-json` (constructs values correctly by type) and
`beads-command-validate-slot` (checks constraints at submission).

### D13: Hierarchical subcommand derivation via class hierarchy

**Decision: Derive CLI subcommands by walking the class hierarchy;
keep `:cli-command` as escape hatch.**

The `beads-command-subcommand` generic method builds the CLI
subcommand string by walking up the class hierarchy. Each command
class between `beads-command` (exclusive) and the concrete class
(inclusive) contributes a name segment, stripped of the
`beads-command-` prefix. This maps class inheritance to CLI
namespacing naturally.

#### How it works

```elisp
;; Base subcommand group — no slots of its own, just a prefix
(beads-defcommand beads-command-admin (beads-command-global-options)
  ()
  :documentation "Admin commands."
  :abstract t)

;; Concrete subcommand — inherits prefix from parent
(beads-defcommand beads-command-admin-compact (beads-command-admin)
  ((database :type (or null string) :short-option "d"))
  :documentation "Compact database."
  :result nil)
;; Derived subcommand: "admin compact"
```

The default `beads-command-subcommand` method:

```elisp
(cl-defmethod beads-command-subcommand ((command beads-command))
  "Derive subcommand by walking class hierarchy.
Each class between beads-command and the concrete class contributes
a segment. beads-command-admin-compact with parent beads-command-admin
produces \"admin compact\"."
  (let* ((class (eieio-object-class command))
         (name (symbol-name class))
         ;; Walk parents to find subcommand-contributing classes
         (segments (beads--collect-subcommand-segments class)))
    (when segments
      (string-join segments " "))))
```

For commands where the class hierarchy doesn't match the CLI
path, `:cli-command` overrides the derivation:

```elisp
(beads-defcommand beads-command-compact (beads-command-global-options)
  (...)
  :documentation "Compact database."
  :cli-command "admin compact")
```

#### Benefits

- **Structural**: class hierarchy mirrors CLI namespace hierarchy
- **DRY**: parent class defines the prefix once, all children
  inherit it
- **Discoverable**: `M-x describe-class` shows the hierarchy,
  which maps directly to the CLI subcommand tree
- **Escape hatch**: `:cli-command` handles legacy names and
  non-standard CLI paths without class hierarchy contortion

### D14: Auto-generated transient menu hierarchy from class hierarchy

**Decision: The transient menu tree is derived from the command
class hierarchy. Every command is reachable automatically.
High-frequency commands use hand-written transients
(`:transient :manual`) for carefully designed UX.**

Since D13 maps class inheritance to CLI namespacing, and D4
generates transients from slot metadata, we can derive the
complete menu tree automatically:

- **Abstract parent classes** → transient prefixes (sub-menus)
- **Concrete leaf classes** → transient suffixes (actions)
- **Root transient** → auto-populated from all top-level commands

#### How it works

**1. Registration at load time:**

Each `beads-defcommand` registers itself in a class hierarchy
table via symbol properties at load time (not macro expansion
time). All command files are `require`d from `beads.el`:

```elisp
;; Macro emits these at load time:
;; Parent→children mapping (appended incrementally)
(put 'beads-command-admin 'beads-children
     (cons 'beads-command-admin-compact
           (get 'beads-command-admin 'beads-children)))

;; Child→parent for reverse lookup
(put 'beads-command-admin-compact 'beads-parent
     'beads-command-admin)
```

**2. Abstract parents become transient prefixes:**

```elisp
(beads-defcommand beads-command-admin (beads-command-global-options)
  ()
  :documentation "Admin commands."
  :abstract t)
```

Generates:

```elisp
(transient-define-prefix beads-admin ()
  "Admin commands."
  ["Admin"
   ("c" "Compact" beads-admin-compact)
   ("g" "GC"      beads-admin-gc)])
```

All command files are `require`d from `beads.el`.
`beads-meta-rebuild-transients` runs once at the end of
`beads.el` after all requires, building parent menus from
registered children.

**3. Concrete leaves become suffixes:**

A concrete command with slots (like `beads-command-close`)
generates its own options transient as before (per D4). It
appears as a suffix in its parent's menu.

A concrete command with no slots just executes directly.

**4. Key binding derivation:**

Suffix keys are derived with a simple chain:

1. Explicit `:transient-key "k"` on the class — manual override
2. `:short-option` value (first char) — matches the CLI flag
3. First letter of the leaf name segment — `compact` → `"c"`

Collisions produce a **load-time warning** via
`display-warning`. No automatic fallback algorithm — the
developer must resolve collisions with explicit `:transient-key`.
This keeps keys stable: adding a new sibling command never
silently reassigns existing keys.

```elisp
;; Explicit key override
(beads-defcommand beads-command-admin-compact (beads-command-admin)
  (...)
  :transient-key "c")

;; Or auto-derived: "compact" → "c", "gc" → "g"
```

**5. Group headers:**

Groups within a transient prefix are derived from the parent
class `:documentation`. Multiple groups can be specified via
`:transient-group` on child classes:

```elisp
(beads-defcommand beads-command-admin-compact (beads-command-admin)
  (...)
  :documentation "Compact database."
  :transient-group "Maintenance")

(beads-defcommand beads-command-admin-gc (beads-command-admin)
  (...)
  :documentation "Garbage collect."
  :transient-group "Maintenance")

(beads-defcommand beads-command-admin-users (beads-command-admin)
  (...)
  :documentation "Manage users."
  :transient-group "Access Control")
```

Generates:

```elisp
(transient-define-prefix beads-admin ()
  "Admin commands."
  ["Maintenance"
   ("c" "Compact"  beads-admin-compact)
   ("g" "GC"       beads-admin-gc)]
  ["Access Control"
   ("u" "Users"    beads-admin-users)])
```

Children without `:transient-group` are placed in a default
group named after the parent.

**6. Root transient:**

The root `beads` transient is the same mechanism — it collects
all direct children of `beads-command-global-options` (or a
designated root class). Top-level concrete commands and abstract
subcommand groups both appear:

```elisp
;; Auto-generated root transient
(transient-define-prefix beads ()
  "Beads issue tracker."
  ["Issues"
   ("c" "Close"      beads-close)
   ("r" "Create"     beads-create)
   ("l" "List"       beads-list)]
  ["Subcommands"
   ("a" "Admin"      beads-admin)
   ("f" "Federation" beads-federation)
   ("m" "Merge slot" beads-merge-slot)])
```

**7. Hand-written transient overrides:**

Commands that need carefully designed UX use
`:transient :manual` (see D4). The `beads-defcommand` skips
auto-generation and the hand-written `transient-define-prefix`
follows immediately in the same file. No load-order concerns.

For partial overrides — auto-generated infixes with hand-written
suffix layout — use `beads-meta-infix-group`:

```elisp
(transient-define-prefix beads-list ()
  "List issues with filters."
  ;; Auto-generated from slot metadata
  (beads-meta-infix-group 'beads-command-list "Filters")
  ;; Hand-written suffixes
  ["List"
   ("l" "All issues" beads-list-all)
   ("r" "Ready"      beads-list-ready)
   ("b" "Blocked"    beads-list-blocked)])
```

`beads-meta-infix-group` reads slot metadata from the class and
returns a transient group vector. New slots added to the class
automatically appear in the hand-written transient.

Typical candidates for `:transient :manual`:

- `beads-list` — magit-log-style filter infixes (Pattern 2)
- `beads-create` — buffer-based editing flow (Pattern 3)
- `beads` (root) — carefully curated top-level layout with
  section groupings that don't map to class hierarchy
- Any command where the UX requires infix switches, multi-step
  flows, or non-standard suffix arrangement

Commands that are fine with auto-generated transients:

- Admin/maintenance commands (`beads-admin-compact`, etc.)
- Simple one-shot commands (`beads-close`, `beads-reopen`)
- Subcommand group prefixes (`beads-admin`, `beads-federation`)

**8. Other customization points:**

- `:transient nil` on a class — exclude from menu entirely
- `:transient :manual` — skip auto-generation; hand-written
  transient follows in the same file
- `:transient-key "k"` — explicit key override
- `:transient-group "Name"` — control grouping within parent
- `beads-meta-infix-group` — compose auto-generated infixes
  into hand-written transients
- Override `beads-meta-transient-suffixes` method on a parent
  class for full manual control of its child list

#### Eager generation

All command files are `require`d from `beads.el`.
`beads-meta-rebuild-transients` runs once at the end of
`beads.el` after all requires:

```elisp
(defun beads-meta-rebuild-transients ()
  "Rebuild all transient menus from the command class hierarchy.
Called once at startup after all command files are required."
  (beads-meta--walk-hierarchy 'beads-command-global-options
    (lambda (class children)
      (when (and children
                 (not (eq (get class 'beads-transient) :manual)))
        (beads-meta--define-prefix-transient class children)))))
```

#### Benefits

- **Complete**: every command is reachable from the menu tree
  automatically; high-frequency commands use `:transient :manual`
  for carefully designed UX
- **Consistent**: menu structure always matches the CLI namespace
- **Extensible**: third-party packages add commands by defining
  new subclasses — they appear in menus automatically
- **Discoverable**: `M-x beads` shows the full command tree,
  navigable with standard transient keys
- **Groupable**: `:transient-group` gives fine-grained control
  over visual layout without breaking the hierarchy
- **Composable**: `beads-meta-infix-group` lets hand-written
  transients reuse auto-generated infix groups

## Macro API

### Invocation

```elisp
(beads-defcommand beads-command-close (beads-command-global-options)
  ((issue-ids
    :type (list-of string)
    :positional 1
    :separator " "
    :short-option "i"
    :reader beads--read-issue-at-point-or-prompt
    :group "Close Issue"
    :required t)
   (reason
    :type (or null string)
    :short-option "r"
    :transient beads-transient-multiline
    :documentation "Close Reason"
    :group "Close Issue"
    :required t))
  :documentation "Close one or more issues with a required reason."
  :result (list-of beads-issue))
```

Command returning a list of domain objects:

```elisp
(beads-defcommand beads-command-list (beads-command-global-options)
  ((status
    :type (or null string)
    :short-option "s"
    :reader beads-read-status
    :group "Filters")
   (type
    :type (or null string)
    :short-option "t"
    :reader beads-read-type
    :group "Filters"))
  :documentation "List issues with optional filters."
  :result (list-of beads-issue))
```

Minimal command (no slots, no transient, no JSON):

```elisp
(beads-defcommand beads-command-doctor (beads-command-global-options)
  ()
  :documentation "Run bd doctor."
  :json nil
  :transient nil)
```

No `:global-section`, no `:parse-as`, no `:cli-command`,
no `:option-type`. The macro generates everything from the class
name, standard EIEIO `:type`, `:result`, and slot metadata.

### Slot property changes

| Old name             | New name          | Rationale                                      |
|----------------------|-------------------|-------------------------------------------------|
| `:option-type`       | `:type`           | Use standard EIEIO type; infer serialization    |
| `:key`               | (removed)         | `:short-option` serves as both CLI flag and transient key |
| `:field-name`        | `:documentation`  | Reuse standard EIEIO slot property              |
| `:option-separator`  | `:separator`      | Shorter, unambiguous in context                 |

The `:documentation` slot property serves double duty: EIEIO uses
it for docstrings, and the transient system uses it as the display
label for multiline editors (replacing `:field-name`).

### Remaining custom slot properties

These are specific to beads and have no EIEIO equivalent.
Properties marked "override only" have sensible defaults and
only need explicit values when the default is wrong.

**Always specified:**
- `:short-option "x"` — single-letter CLI flag AND transient key
- `:group "name"` — transient group name
- `:required t` — required for validation
- `:reader FN` — transient reader function for interactive input
- `:choices LIST` — valid values for validation and completion
- `:positional N` — positional argument position (1, 2, ...)
- `:transient CLASS` — transient infix class override
- `:level N` — transient visibility level

**Override only (have defaults):**
- `:long-option "name"` — default: slot name (hyphens preserved)
- `:argument "text"` — default: derived from `:long-option` →
  `"--long-option="`
- `:prompt "text"` — default: humanized slot name → `"Slot name: "`
- `:separator ","` — default: `","` for `(list-of ...)` types
- `:order N` — default: slot definition order
- `:json-key "key"` — default: slot name with hyphens → underscores

### Class-level macro keywords

- `:documentation "text"` — standard EIEIO; also used for transient
  docstring
- `:result CLASS` — domain class symbol for automatic JSON-to-object
  parsing (e.g., `beads-issue`, `beads-stats-data`). Stored on a
  symbol property via `(put CLASS 'beads-result TYPE)`. When nil
  or omitted, parse returns raw JSON. `:result CLASS` means JSON
  is a single object; `:result (list-of CLASS)` means JSON is an
  array.
- `:transient` — controls transient generation:
  - omitted / `t` — auto-generate (default)
  - `nil` — no transient at all
  - `:manual` — skip auto-generation; hand-written
    `transient-define-prefix` follows in the same file
- `:json` — whether the command supports `--json`. Default `t`.
  Set to `nil` for commands that don't support it (e.g., doctor).
  Stored on symbol property; checked by `beads-command-execute`
  before setting `json` on the clone.

### What the macro generates

1. **EIEIO class** via `defclass` (wrapped in `eval-and-compile`)
2. **Symbol properties** for `:result`, `:json`, hierarchy
   registration (`beads-parent`, `beads-children`)
3. **Transient menu** via `beads-meta-define-transient` (unless
   `:transient nil` or `:transient :manual`)
4. **Autoload cookie** for the transient command (when generated)
5. **Defensive check** that all superclasses are defined at
   macro-expansion time

### What the macro does NOT generate

- Validation methods (base methods handle `:required` / `:choices`
  via slot metadata introspection at runtime — zero generated code)
- Bang functions (removed; use `beads-execute` / `beads-execute-async`)
- Parse methods (handled by base class using `:result` +
  `beads-from-json`; override with `cl-defmethod`)
- Execute-interactive methods (default on base class, override
  per command; use `beads-execute-with-message` helper for
  message-style)

### Naming conventions

- Class: `beads-command-close`
- Transient: `beads-close` (strip `-command-` from class name)
- CLI subcommand: derived by walking the class hierarchy.  Each
  command class contributes its own name segment (strip
  `beads-command-` prefix from each level).  Examples:
  - `beads-command-close` → `"close"`
  - `beads-command-admin-compact` inheriting `beads-command-admin`
    → `"admin compact"`
  - `beads-command-federation-add-peer` inheriting
    `beads-command-federation` → `"federation add-peer"`
  - `:cli-command "migrate hooks"` — explicit override when the
    class hierarchy doesn't match the CLI path

## Execution Contract

### Method signatures

```elisp
;; Sync: clones cmd, sets json=t, returns parsed result
(beads-command-execute command)
;; => domain objects, raw JSON alist, or raw stdout string

;; Async: clones cmd, sets json=t, callbacks for result/error
(beads-command-execute-async command on-success &optional on-error)
;; on-success: (lambda (result) ...)
;; on-error:   (lambda (condition) ...) — nil means beads--error

;; Interactive: json stays nil (default), runs in terminal
(beads-command-execute-interactive command)
;; => terminal buffer

;; Shorthand:
(beads-execute 'class &rest constructor-args)
(beads-execute-async 'class on-success &optional on-error
                     &rest constructor-args)
```

### JSON handling

The `json` slot stays on `beads-command-global-options` with
default `nil`. Execution methods handle it:

| Entry point                        | json handling         | Returns              |
|------------------------------------|-----------------------|----------------------|
| `beads-command-execute`            | clone, set json=t     | parsed result        |
| `beads-command-execute-async`      | clone, set json=t     | via on-success       |
| `beads-command-execute-interactive`| as-is (json=nil)      | terminal buffer      |

Commands with `:json t` (default) get `--json` from
execute/execute-async. Commands with `:json nil` skip it.

### Parse chain

```
beads-command-execute
  -> clone cmd, set json=t
  -> run process (with --json on command line)
  -> beads-command-parse (generic method, on beads-command)
     -> json=nil: returns raw stdout string
     -> json=t, no :result: json-read-from-string (alist/vector)
     -> json=t, :result set: beads-coerce-json-value with declared type
        -> :result beads-issue: constructs single domain object
        -> :result (list-of beads-issue): maps over JSON array
        -> beads-from-json called for each EIEIO class type
        -> override: cl-defmethod on beads-from-json per class
  -> returns parsed result
```

Parse method signature: `(beads-command-parse command stdout)`
— stderr is not passed (errors are signaled before parse runs).

JSON null handling: `json-null` is bound to `nil` during parsing,
so `:json-null` never appears in the data. `beads-from-json`
preserves absent-vs-null distinction by only passing initargs for
keys present in the JSON alist (see D10).

JSON key-to-slot mapping: hyphen-to-underscore convention. Slots
with `:json-key` property override the default mapping.

Unknown JSON fields are silently dropped — if bd CLI adds a new
field, add a slot to the domain class to capture it.

Tramp compatibility: `beads-command-execute` uses `process-file`,
which respects `default-directory` for remote hosts. The `--db`
path is stripped via `file-local-name`. The clone operation does
not affect Tramp behavior.

### Error handling

Errors are signaled as conditions with full diagnostic data:

- `beads-validation-error`: command validation failed
  - data: `:command`, `:error`
- `beads-command-error`: non-zero exit code
  - data: `:command`, `:exit-code`, `:stdout`, `:stderr`
- `beads-json-parse-error`: JSON parsing failed
  - data: `:exit-code`, `:stdout`, `:stderr`, `:parse-error`

No execution object needed — the condition data carries everything.
Accessor functions for ergonomic condition data access:

```elisp
(beads-command-error-stderr err)    ;; extract stderr string
(beads-command-error-stdout err)    ;; extract stdout string
(beads-command-error-exit-code err) ;; extract exit code
```

If `beads-command-execute` returns without signaling, the command
succeeded. Nil return is valid (e.g., empty list results).

### Validation

No generated validation code. Three levels of generic methods
handle validation via slot metadata introspection at runtime.
See D8 for the full design and examples.

## Classes Affected

### Deleted

- `beads-command-execution` — replaced by direct return + conditions

### Modified

- `beads-command-global-options` — `json` slot default stays `nil`
  (execute methods set it via clone)
- `beads-command` (base) — `beads-command-execute` clones and sets
  json, returns result directly; `beads-command-parse` takes stdout
  only
- All domain classes in `beads-types.el` — bare `list` slots updated
  to `(list-of T)` for proper introspection

### Parse method signature change

Current: `(beads-command-parse command execution)`
- `execution` is a `beads-command-execution` object

New: `(beads-command-parse command stdout)`
- `stdout` is a string

This is a breaking change for all existing parse method overrides.

## Testing Strategy

**Prerequisite for all migration phases:**

1. **Fixture capture**: for every command with a parse override,
   capture real `bd --json` output as test fixtures (JSON files)
2. **Parity tests**: for each fixture, verify that
   `beads-from-json` produces identical domain objects as the
   current hand-written `*-from-json` functions
3. **Run parity tests before deleting any hand-written code**
4. **CI gate**: parity tests run in CI to catch regressions between
   phases

## Migration Plan

### Phase 1: Simplify execution and remove bang functions

This is a single atomic "big break" phase. Pre-1.0 project — no
backward compatibility, no intermediate half-migrated states.

1. Delete `beads-command-execution` class
2. Change `beads-command-execute` to clone cmd, set json=t, return
   parsed result directly
3. Change `beads-command-parse` signature to `(command stdout)`
4. Change `beads-command-execute-async` signature to
   `(command on-success &optional on-error)`
5. Update all callers of `(oref execution result)` to use return
   value directly
6. Update all `beads-command-parse` overrides for new signature
7. Add `beads-execute` and `beads-execute-async` convenience
   functions (using `make-instance` + `find-class` guard)
8. Add `beads-execute-with-message` utility for interactive
   overrides
9. Add condition accessor functions (`beads-command-error-stderr`,
   etc.)
10. Remove bang function generation from macro
11. Replace all ~372 bang function call sites with `beads-execute`
    via mechanical codemod (`query-replace-regexp`:
    `(beads-command-\([a-z-]+\)!` → `(beads-execute 'beads-command-\1`)
12. CI gate: `rg 'beads-command-\w+!' lisp/` must return 0 hits
13. CI gate: `rg 'oref.*execution.*result' lisp/` must return 0 hits

### Phase 2: Simplify the macro

1. Remove `:global-section` keyword handling — generate transient
   by default (opt-out with `:transient nil` or `:transient :manual`)
2. Remove `:parse-as` keyword handling — replaced by `:result`
3. Keep `:cli-command` keyword — use class hierarchy for
   subcommand derivation. Base classes contribute a prefix
   (e.g. `beads-command-admin` → `"admin"`), subclasses append
   their suffix (e.g. `beads-command-admin-compact` → `"compact"`
   → full subcommand `"admin compact"`). `:cli-command` remains
   as escape hatch for non-standard names
4. Add `:result` keyword — stored on symbol property
5. Add `:json` keyword — stored on symbol property (default `t`)
6. Add `:transient nil` / `:transient :manual` support
7. Add three-level validation base methods (D8) — no generated
   validation code
8. Replace `:option-type` inference with `:type`-based inference
9. Rename `:key` → (use `:short-option`), `:field-name` →
   `:documentation`, `:option-separator` → `:separator`
10. Add `:json-key` custom slot property support
11. Add slot property defaults (`:long-option`, `:argument`,
    `:prompt`, `:separator`, `:order` auto-derived)
12. Add defensive superclass existence check at macro-expansion time
13. Add class hierarchy registration at load time (parent→children,
    child→parent symbol properties)
14. Add `:transient-key` and `:transient-group` macro keywords
15. Add key collision detection with `display-warning` at rebuild
16. Introduce abstract parent classes for subcommand groups
    (`beads-command-admin`, `beads-command-federation`,
    `beads-command-merge-slot`, etc.)
17. Implement `beads-meta-rebuild-transients` — eager generation
    of the full transient menu tree from class hierarchy, called
    once at end of `beads.el` after all `require`s
18. Implement `beads-meta-infix-group` for composable partial
    overrides
19. Implement `beads-meta--unwrap-nullable` for robust `(or ...)`
    type normalization

### Phase 3: Domain types and generic parsing

**Prerequisite**: capture test fixtures, write parity tests.

1. Update all bare `list` slots in `beads-types.el` to
   `(list-of T)` (e.g., `(list-of beads-dependency)`,
   `(list-of beads-comment)`, `(list-of string)`)
2. Ensure all domain class slots have proper `:type`
3. Add `:json-key` to slots with non-standard JSON key mapping
4. Add missing domain types for commands that currently return
   raw JSON
5. Implement `beads-from-json` generic function with default
   EIEIO-introspection method (unspecialized primary method);
   only pass initargs for keys present in JSON alist
6. Implement `beads-coerce-json-value` as extensible generic,
   using `beads-meta--unwrap-nullable` for `(or ...)` handling
7. Implement slot-name-to-JSON-key mapping (hyphen → underscore)
   via beads-meta abstraction layer
8. Update base `beads-command-parse` to use `:result` +
   `beads-from-json`
9. Add `beads-from-json` overrides for known non-standard classes:
   `beads-stats-data`, `beads-dependency`, `beads-epic-status`
10. Run parity tests — verify identical output
11. Delete all hand-written `*-from-json` functions
12. Delete all hand-written `beads-command-parse` overrides

### Phase 4: Clean up command files

1. Add `:result` to all command definitions (`:result CLASS` for
   single objects, `:result (list-of CLASS)` for arrays)
2. Add `:json nil` to commands that don't support `--json`
3. Add `:transient :manual` to commands with hand-written transients
4. Delete `beads-command-execute-interactive` overrides that match
   the default terminal behavior
5. Update all slot definitions:
   - `:option-type :string` → `:type (or null string)`
   - `:option-type :boolean` → `:type boolean`
   - `:option-type :integer` → `:type (or null integer)`
   - `:option-type :list` → `:type (list-of string)` or
     `(list-of integer)` as appropriate
   - `:key "x"` → `:short-option "x"` (where not already set)
   - `:field-name "Foo"` → `:documentation "Foo"`
   - `:option-separator ","` → `:separator ","`
   - Remove properties that match new defaults

### Phase 5: Follow-up (separate effort)

1. Slot shorthand / slot templates
2. Async progress UX pattern (long-running operations)

## Risks

- **Phase 1 blast radius** — parse signature change (~20 overrides),
  bang function removal (~372 call sites), and execution object
  removal all happen atomically. Mitigated by mechanical codemod
  and CI grep gates.
- **`:type` inference edge cases** — complex or unusual type specs
  may not map cleanly. The inference function needs a clear
  fallback (default to string). `beads-meta--unwrap-nullable`
  handles arbitrary `(or ...)` forms robustly.
- **Internal `cl--` APIs** — slot introspection uses internal Emacs
  APIs that could change between versions. All access is
  centralized through beads-meta abstraction layer to contain
  breakage to one file. Hardening strategy:
  - CI regression tests exercising every `beads-meta-*` accessor
    on all supported Emacs versions (28.2, 29.4, 30.2, snapshot)
  - Version-conditional code paths where public APIs exist
  - Replace `define-advice` on `eieio--slot-override` with
    post-`defclass` fixup for custom property preservation
- **`beads-from-json` override scope** — ~3-5 domain classes need
  overrides for non-standard JSON shapes. These must be identified
  and tested before deleting hand-written parsers.
- **EIEIO `clone` is shallow** — list-valued slots share structure
  between original and clone. Safe because only `json` (boolean)
  is mutated on the clone. Invariant must be preserved.
- **`(list-of T)` element validation** — EIEIO's element-level
  type checking is version-dependent and O(n). Do not rely on it
  for runtime safety. Enforcement via `beads-from-json` and
  `beads-command-validate-slot` instead.

## Non-Goals

- Changing how transient infixes are generated from metadata
- Changing the terminal backend system
- Adding new command classes
- Backward compatibility or deprecation periods
