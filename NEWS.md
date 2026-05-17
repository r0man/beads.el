# beads.el NEWS

User-visible and API-breaking changes, newest first.

## Unreleased

### Breaking: `beads-agent-backend-start` is now 4-arity

The backend protocol generic changed signature:

```elisp
;; before
(beads-agent-backend-start backend issue prompt)
;; after
(beads-agent-backend-start backend issue system-prompt user-prompt)
```

There is **no backwards-compatibility shim and no deprecated alias**.
Every in-tree backend migrated atomically. An out-of-tree backend that
still defines a 3-arity `cl-defmethod` will, on the first start
attempt, hit a default method on the abstract base
`beads-agent-backend` that signals a plain `error`:

> Backend `<class>` must implement 4-arity beads-agent-backend-start
> (backend issue system-prompt user-prompt).  See NEWS

(This replaces what would otherwise be a cryptic
`cl-no-applicable-method`.)

**Migration recipe** for an out-of-tree backend:

```
;; arglist
s/(backend issue prompt)/(backend issue system-prompt user-prompt)/

;; body, if your backend has no dedicated system-prompt channel:
(let ((prompt (beads-agent-backend--combine-prompt
               system-prompt user-prompt)))
  ...existing body unchanged...)
```

`beads-agent-backend--combine-prompt` prepends a non-empty
SYSTEM-PROMPT to USER-PROMPT separated by a blank line, and returns
USER-PROMPT unchanged when SYSTEM-PROMPT is nil or empty. An empty
system prompt is treated as absent (no stray blank line).

`issue` may be `nil` (project-level agents call with no issue); methods
must tolerate that in slot 2 (this was already true pre-change).

In this phase (1a-i) the rendered prompt is **byte-identical** to the
prior release: the new system-prompt channel exists but every built-in
agent type still yields `nil` for it. The default-value rewrites that
make the split observable land in a separate, separately-revertable
change.

### Breaking: `beads-agent-type-build-prompt` renamed

`beads-agent-type-build-prompt` is now
`beads-agent-type-build-user-prompt` (same arity, same behaviour, same
return value). No alias.

**Migration recipe:**

```
s/beads-agent-type-build-prompt/beads-agent-type-build-user-prompt/
```

### New: `beads-agent-type-system-prompt` generic

`(beads-agent-type-system-prompt type issue)` returns the agent
role/identity string (with `<ISSUE-...>` placeholders substituted), or
`nil` when the type has no distinct system prompt. Builder types (such
as Custom) and — in this phase — every built-in type return `nil`. A
new `system-prompt` slot was added to `beads-agent-type`
(string/symbol, default `nil`), mirroring `prompt-template`.

### Breaking: prompt-edit callback signature + cancel sentinel

`beads-agent-prompt-edit-show`'s CALLBACK is now invoked with **two**
arguments, `(SYSTEM USER)`, instead of one:

- Confirm: `(funcall callback SYSTEM USER)`. In this phase the buffer
  is still single-region, so `SYSTEM` is always `nil` and `USER`
  carries the edited text. (The two-region editing UI lands in a
  later phase.)
- Cancel: `(funcall callback nil nil)`.

The **cancel sentinel is `(nil nil)`**. "No system override, real user
prompt" is `(nil "the user text")` and proceeds to launch. The
orchestrator distinguishes cancel — `(and (null sys) (null user))` —
from "use the default system prompt" — `(null sys)` with non-nil
`user`. Any out-of-tree code installing a prompt-edit callback must
accept two arguments and treat `(nil nil)` as cancel.
