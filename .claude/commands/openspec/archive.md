---
name: OpenSpec: Archive
description: Archive a completed OpenSpec change after all beads issues are closed.
category: OpenSpec
tags: [openspec, archive, beads]
---
<!-- OPENSPEC:START -->
**Guardrails**
- Only archive after ALL beads issues for the change are closed.
- Keep changes tightly scoped to the requested outcome.
- Refer to `openspec/AGENTS.md` (located inside the `openspec/` directory—run `ls openspec` or `openspec update` if you don't see it) if you need additional OpenSpec conventions or clarifications.

**Steps**
1. Determine the change ID to archive:
   - If this prompt already includes a specific change ID (for example inside a `<ChangeId>` block populated by slash-command arguments), use that value after trimming whitespace.
   - If the conversation references a change loosely (for example by title or summary), run `openspec list` to surface likely IDs, share the relevant candidates, and confirm which one the user intends.
   - Otherwise, review the conversation, run `openspec list`, and ask the user which change to archive; wait for a confirmed change ID before proceeding.
   - If you still cannot identify a single change ID, stop and tell the user you cannot archive anything yet.

2. Validate the change ID by running `openspec list` (or `openspec show <id>`) and stop if the change is missing, already archived, or otherwise not ready to archive.

3. Verify beads issues are complete:
   - Read `openspec/changes/<id>/tasks.md` to find linked beads issue IDs
   - Run `bd show [epic-id]` to verify the epic status
   - Run `bd list --status open` and check if any issues related to this change are still open
   - If any linked issues are still open, report them and stop—do not archive until all work is complete
   ```bash
   # Check for open issues related to this change
   bd list --status open | grep -i "[change-id]"
   ```

4. Run `openspec archive <id> --yes` so the CLI moves the change and applies spec updates without prompts (use `--skip-specs` only for tooling-only work).

5. Review the command output to confirm the target specs were updated and the change landed in `changes/archive/`.

6. Validate with `openspec validate --strict` and inspect with `openspec show <id>` if anything looks off.

7. Sync beads with git:
   ```bash
   bd sync
   ```

**Reference**
- Use `openspec list` to confirm change IDs before archiving.
- Use `bd list --status closed` to verify all related issues are closed.
- Inspect refreshed specs with `openspec list --specs` and address any validation issues before handing off.
<!-- OPENSPEC:END -->
