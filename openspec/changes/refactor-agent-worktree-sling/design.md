# Design: Agent Worktree Sling Refactoring

## Context

beads.el provides AI agent integration that can spawn agents in git worktrees
for isolation. Currently, worktree creation uses direct `git worktree`
commands, bypassing the beads database redirect setup that `bd worktree create`
provides.

The bd CLI recently added `bd worktree` subcommands that:
1. Create worktrees with proper `.beads/redirect` files
2. Add worktree paths to `.gitignore`
3. Provide safety checks before removal
4. Support custom branch names separate from worktree names

Gastown is an external AI agent orchestration system that also uses git
worktrees (called "hooks") for agent persistence. Supporting Gastown
integration requires a flexible "sling" workflow where work can be
assigned to agents running in pre-created or dynamically-created worktrees.

## Goals

1. Use `bd worktree` commands instead of raw git commands
2. Provide rich transient UI for worktree management
3. Support "sling" workflow for flexible agent assignment
4. Enable Gastown integration without requiring Gastown
5. Maintain backward compatibility with existing agent sessions

## Non-Goals

- Full Gastown feature parity (that's gastown.el's job)
- Multi-repository support (future work)
- Automatic worktree cleanup policies

## Decisions

### Decision 1: EIEIO Command Classes for Worktree Operations

**What:** Create `beads-command-worktree-*` classes following existing patterns.

**Why:** Consistent with project architecture. EIEIO classes provide:
- Automatic --json flag handling
- Database path resolution
- Actor/db global flags
- Async execution support

**Alternatives considered:**
- Direct shell calls: Rejected - breaks consistency, loses JSON parsing
- Single function per command: Rejected - doesn't leverage EIEIO benefits

### Decision 2: Worktree Transient Menu

**What:** Add `beads-worktree` transient prefix accessible from main menu.

**Why:** Follows Magit-style interface. Users expect keyboard-driven
worktree management similar to `magit-worktree`.

**Menu structure:**
```
Worktree Management
├── c  Create worktree
├── l  List worktrees
├── r  Remove worktree
└── i  Info (current worktree)
```

### Decision 3: Completion with Intelligent Suggestions

**What:** Use grouped completing-read with annotations for worktree selection.

**Why:** Project convention uses rich completion tables (see `beads-completion.el`).
Worktree selection should match this pattern.

**Completion suggestions for worktree name:**
- Open issues (common use case: one worktree per issue)
- Existing branch names
- Free-form input allowed

**Annotations show:**
- Branch name (if different from worktree name)
- Beads state: redirect, shared, none

### Decision 4: Sling Workflow for Agent Assignment

**What:** Add `beads-agent-sling` command that:
1. Prompts for target (issue or project)
2. Prompts for worktree (create new or select existing)
3. Prompts for backend
4. Starts agent in selected worktree

**Why:** "Slinging" work to agents is the Gastown paradigm. Adopting this
terminology and workflow enables:
- Standalone beads.el usage with same mental model
- Smooth transition to Gastown when adopted
- Clear separation: worktree setup vs agent spawning

**Workflow options:**
```
Sling Work
├── Issue + New Worktree    (create worktree named after issue)
├── Issue + Existing Worktree (use pre-created worktree)
└── Project + Worktree      (no specific issue focus)
```

### Decision 5: Optional Gastown Backend

**What:** Implement `beads-agent-gastown` backend that delegates to gastown.el.

**Why:** Gastown provides advanced orchestration (Mayor/Polecat hierarchy,
Convoys, etc.) that some users want. Making it an optional backend:
- Works seamlessly when gastown.el is installed
- Falls back to built-in backends otherwise
- Shares common worktree infrastructure

**Integration points:**
- Gastown uses "hooks" (worktrees) for persistence
- beads.el worktree commands align with Gastown's hook concept
- Backend translates between beads sessions and Gastown convoys

### Decision 6: Preserve Backward Compatibility

**What:** Keep existing `beads-agent-use-worktrees` behavior working.

**Why:** Users have workflows depending on current behavior. Migration
should be seamless.

**Compatibility approach:**
- `beads-agent-use-worktrees = t` uses `bd worktree create` (new)
- `beads-agent-use-worktrees = nil` works in main repo (unchanged)
- `beads-agent-use-worktrees = 'ask` prompts as before
- New `beads-agent-sling` provides explicit control

## Risks / Trade-offs

### Risk: bd CLI worktree command availability

**Risk:** Users with older bd versions lack worktree subcommand.

**Mitigation:** Check bd version at startup, fall back to git commands
if worktree subcommand unavailable. Log deprecation warning.

### Risk: Gastown API instability

**Risk:** Gastown is evolving; API may change.

**Mitigation:** Gastown backend is optional and isolated. Changes to
gastown.el don't affect core beads.el functionality.

### Trade-off: Worktree naming convention

**Trade-off:** Should worktree name default to issue ID or be flexible?

**Decision:** Default to issue ID when slinging work on an issue,
but allow override via --branch flag. Matches bd worktree semantics.

## Migration Plan

### Phase 1: Add bd worktree support (non-breaking)
1. Add EIEIO command classes
2. Add transient menu
3. Add completion support
4. All new code, no changes to existing

### Phase 2: Migrate agent worktree creation
1. Modify `beads-agent--ensure-worktree-async` to use bd commands
2. Deprecate direct git worktree functions
3. Test with existing workflows

### Phase 3: Add sling workflow
1. Add `beads-agent-sling` command
2. Document new workflow
3. Optional Gastown backend

### Rollback

Each phase is independently revertible:
- Phase 1: Remove new files
- Phase 2: Restore git worktree functions
- Phase 3: Remove sling command and Gastown backend

## Open Questions

1. **Worktree cleanup policy:** Should we provide automated cleanup of
   stale worktrees? Defer to future enhancement.

2. **Multi-agent per worktree:** Should multiple agents share a worktree?
   Current model is 1:1. Gastown supports this; consider for future.

3. **Remote worktrees:** Should we support worktrees over TRAMP? The
   existing Tramp-compatible process execution should work, but needs
   testing with bd worktree commands.
