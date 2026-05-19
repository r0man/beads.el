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

### Behavioural break: default prompts split into system + user

The built-in agent-type default prompts were **rewritten** (not
relabelled). The role/identity preamble is now a **role-only** system
prompt; the issue envelope (`<ISSUE-ID>: <ISSUE-TITLE>` +
`<ISSUE-DESCRIPTION>`) and the type-specific `bd close`/`bd update`
shell blocks moved to a new **user-prompt** defconst per type:

- `beads-agent-type-task--prompt` → split into
  `beads-agent-type-task--system-prompt` (role) +
  `beads-agent-type-task--user-prompt` (envelope/output).
- `beads-agent-{review,plan,qa}-prompt` defcustom **default values**
  rewritten to role-only; new
  `beads-agent-type-{review,plan,qa}--user-prompt` defconsts carry the
  envelope/output. Defcustom *names* are unchanged.
- Custom and the orchestration fallback are builders (no
  `<ISSUE-...>`); `system-prompt` stays nil; their builder is
  unchanged (renamed only, in 1a-i).

**Consequence for users who `setq`/`customize`d the role defcustoms:**
your value is now delivered as the **system** prompt. Embedded
`<ISSUE-...>` placeholders still substitute. Embedded `bd close`/`bd
update` instructions now arrive in the *role* channel — move them into
the matching `beads-agent-type-*--user-prompt` if you relied on them.

The prompt editor now shows **two editable regions** (`## System
prompt` / `## User prompt`) with read-only marked headings; a blank
system region means "use the backend's built-in identity".

#### Old default values (verbatim, for diffing)

`beads-agent-type-task--prompt` (removed):

```
You are a task-completion agent for beads. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>.

# Constraints

- Stay focused on the assigned task
- Don't make unrelated changes
- If blocked, explain clearly what's needed
- Communicate progress and decisions

# Agent Workflow

1. **Claim the Task**
   - Update issue status to in_progress: `bd update <ISSUE-ID> --status in_progress`
   - Read the task description carefully
   - Check acceptance criteria if available

2. **Execute the Task**
   - Use available tools to complete the work
   - Follow best practices from project documentation
   - Run tests if applicable
   - Keep changes focused on the task

3. **Track Discoveries**
   - If you find bugs, TODOs, or related work:
     - File new issues using bd create
     - Link them with discovered-from dependencies: `bd dep add <new-id> --type discovered-from --target <ISSUE-ID>`
   - This maintains context for future work

4. **Verify Completion**
   - Check that all acceptance criteria are met
   - Ensure tests pass
   - Review your changes for quality

# Output

When work is complete, close the issue with a clear summary:

    bd close <ISSUE-ID> --reason "$(cat <<'EOF'
    <Summary of what was accomplished, any important decisions made, and verification performed>
    EOF
    )"

If blocked, update the issue status and explain:

    bd update <ISSUE-ID> --status blocked --notes "$(cat <<'EOF'
    <Clear explanation of what is blocking progress and what is needed to proceed>
    EOF
    )"
```

`beads-agent-review-prompt` old default began:
`"You are a code review agent. Please work on beads issue <ISSUE-ID>:
<ISSUE-TITLE>."` followed by the Constraints/Review Focus sections and
an `# Output` block with `bd update <ISSUE-ID> --notes …`.

`beads-agent-qa-prompt` old default began: `"You are a QA agent.
Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>."` followed by
the Constraints/QA Workflow sections and an `# Output` block with `bd
update <ISSUE-ID> --acceptance … --notes …`.

`beads-agent-plan-prompt` old default began: `"You are a planning
agent. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>."` then
`"Create a detailed implementation plan WITHOUT making any code
changes."`, the Constraints/Planning Steps/Plan Review sections, and
an `# Output` block with `bd update <ISSUE-ID> --description … --design
… --acceptance … --notes …`.

The combined rendered prompt (system + blank line + user) for the
built-in template types still contains the same substituted issue id
and the same instruction content as before — only the delivery
channel split.

### Terminal backend registered (opt-in); efrit removed

`beads-agent-backend-claude` (the `claude` CLI spawned directly into a
terminal — collision-free by construction) is now **registered and
selectable**. It is **opt-in**: the per-type backend defcustoms
(`beads-agent-{task,review,plan,qa}-backend`) are **not** flipped.

> Net effect for default users in this release: `beads-agent.el`'s
> orchestrator `rename-buffer` is **not** patched and the per-type
> backend defcustom defaults are **not** flipped. A user who never
> customised their backend gets *exactly the bde-h93r behaviour after
> this PR as before it*. Only users who explicitly opt in via
> `(setq beads-agent-task-backend "claude")` are protected. The
> originating bug is *displaced*, not fixed, this release.

The `efrit` backend was **removed** (`beads-agent-efrit.el` and its
test deleted; the `require`, header comment, and `"efrit"` test
fixtures dropped). No deprecation alias.

`beads-reader-terminal` was added (completes over registered
terminals, returns the class symbol for `beads-agent-default-terminal`).
It resolves the class from the registered terminal *instance* rather
than reconstructing `beads-terminal-<name>`, so a third-party terminal
whose registered name differs from its class symbol resolves
correctly.

#### Two terminal knobs coexist (time-boxed)

For one release the `beads-terminal` group holds **two** knobs:

- `beads-terminal-backend` — *symbol* (`nil`/`vterm`/`eat`/`term`),
  governs one-shot `bd` command execution
  (`beads-command--run-in-terminal`).
- `beads-agent-default-terminal` — *class symbol* (default
  `beads-terminal-auto`), governs agent terminal spawning.

`beads-terminal--symbol->class` bridges the old vocabulary so the
Phase 3 unification (collapse onto one knob) is mechanical.

#### Per-backend system-prompt seam status

The Phase 2 spike requires reading the upstream source of each wrapper
package to confirm its system-prompt seam. In this build environment
**none of `claude-code-ide`, `claude-code`, `claudemacs`, `eca`, or
`agent-shell` is installed**, so no seam could be verified. Per the
plan's spike-gating rule, every wrapper backend therefore **holds the
Phase 1a-i concat shim** (system + blank line + user combined via
`beads-agent-backend--combine-prompt`) and ships unchanged. Wiring a
dedicated seam for any wrapper is deferred until its upstream source
can be verified. Only the terminal `beads-agent-backend-claude`
delivers the system prompt through a dedicated channel
(`--append-system-prompt`).

### New: `pi` terminal backend; ghostel `ghostel-exec` fix

`beads-agent-backend-pi` (the `pi` CLI spawned directly into a
terminal) is now **registered and selectable**, configuration-identical
to `beads-agent-backend-claude` (`--append-system-prompt` + positional
message). Like `claude` it is **opt-in** — the per-type backend
defcustoms are **not** flipped.

`beads-terminal-ghostel` now spawns through ghostel's public
`ghostel-exec` (PROGRAM + ARGS) instead of the single-shell
`ghostel-shell` defcustom, fixing a bug where ghostel tried to exec a
program literally named `"claude --append-system-prompt …"`. Its
priority dropped 15 → 5 so `auto` prefers ghostel → vterm → eat →
ansi-term → term.

> vterm trade-off: vterm has no argv-direct entry point, so
> `beads-terminal-vterm` joins the argv through
> `shell-quote-argument` and feeds it to `/bin/sh -c`. This is safe
> for shell metacharacters (including single quotes in a system
> prompt, which become correct POSIX `'…'"'"'…'` quoting), but the
> quoted form may display surprisingly inside the vterm buffer. Use
> ghostel/eat/term for argv-direct spawning if that matters.
