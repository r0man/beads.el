---
name: OpenSpec: Apply
description: Convert an approved OpenSpec change into beads epics and sub-issues.
category: OpenSpec
tags: [openspec, apply, beads]
---
<!-- OPENSPEC:START -->
**Guardrails**
- Only run this command after the proposal has been reviewed and approved.
- Refer to `openspec/AGENTS.md` (located inside the `openspec/` directory—run `ls openspec` or `openspec update` if you don't see it) if you need additional OpenSpec conventions or clarifications.
- Do not write implementation code in this stage. This command creates beads issues for tracking work. Implementation happens by working on beads issues.

**Steps**
1. Determine the change ID to apply:
   - If this prompt already includes a specific change ID (for example inside a `<ChangeId>` block populated by slash-command arguments), use that value after trimming whitespace.
   - If the conversation references a change loosely (for example by title or summary), run `openspec list` to surface likely IDs, share the relevant candidates, and confirm which one the user intends.
   - Otherwise, review the conversation, run `openspec list`, and ask the user which change to apply; wait for a confirmed change ID before proceeding.

2. Read the proposal documents:
   - Read `changes/<id>/proposal.md` to understand scope and goals
   - Read `changes/<id>/design.md` (if present) to understand technical decisions
   - Read `changes/<id>/tasks.md` to get the task breakdown

3. Create beads epic for the change:
   ```bash
   bd create "Epic: [Brief description from proposal]" --type epic --priority 2 \
     --notes "OpenSpec change: openspec/changes/[change-id]/

   Proposal: [1-2 sentence summary from proposal.md]"
   ```

4. Create beads sub-issues from tasks.md:
   - Parse each major task item from tasks.md
   - Create a beads issue for each task with appropriate type (task, feature, bug)
   - Set dependencies between issues to reflect task ordering from tasks.md
   - Link all sub-issues to the epic in notes
   ```bash
   # Example for each task:
   bd create "[Task description from tasks.md]" --type task --priority 2 \
     --notes "Epic: [epic-id]. OpenSpec change: [change-id]. Task: [task number]"

   # Set dependencies if task depends on previous task:
   bd update [current-issue-id] --blocked-by [previous-issue-id]
   ```

5. Update tasks.md with cross-references:
   - Add beads issue IDs next to each task for traceability
   - Add epic ID at the top of tasks.md
   - Format: `- [ ] 1.1 Task description → beads.el-X`

6. Verify issue structure:
   ```bash
   bd show [epic-id]      # Review epic and verify structure
   bd ready               # Check which issues are ready to work on
   bd list --status open  # See all created issues
   ```

7. Summarize to user:
   - Report epic ID and all sub-issue IDs created
   - Show which issues are immediately ready for work
   - Remind that implementation happens via beads workflow (`bd ready`, work, `bd close`)

**Next Steps**
After issues are created, implementation proceeds via the normal beads workflow:
- `bd ready` to find unblocked issues
- `bd update [id] --status in_progress` to start work
- Implement the task
- `bd close [id] --reason "Completed"` when done
- Repeat until all issues and epic are closed
- Then run `/openspec:archive` to archive the change

**Reference**
- Use `openspec show <id>` to review the proposal at any time
- Use `bd show [issue-id]` to see issue details and dependencies
- Use `bd graph` to visualize issue dependencies
<!-- OPENSPEC:END -->
