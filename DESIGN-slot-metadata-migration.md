# Design: Migrate Remaining Command Classes to Use Slot Metadata

**Task**: bde-wisp-m0e (Design phase of bde-x0om)
**Date**: 2026-01-08
**Status**: In Progress

## Executive Summary

This document outlines the design for completing the migration of all beads.el command classes to use EIEIO slot metadata for automatic CLI argument and transient menu generation. The slot metadata system (implemented in `beads-meta.el`) enables automatic generation of command-line arguments and transient menus from slot definitions, eliminating the need for manual `beads-command-line` methods and reducing boilerplate significantly.

## Background

### What is Slot Metadata?

The `beads-meta.el` module provides infrastructure for adding custom properties to EIEIO slots that enable:
- **Auto-generation of CLI arguments** from slot metadata
- **Auto-generation of transient infixes** from slot metadata
- **Consistency** between CLI and transient interfaces

Metadata properties include:

**CLI Properties:**
- `:long-option` - Long CLI option (e.g., `"--title"`)
- `:short-option` - Short CLI option (e.g., `"-t"`)
- `:option-type` - Serialization type (`:string`, `:boolean`, `:integer`, `:list`)
- `:positional` - Position for positional args (integer or nil)
- `:option-separator` - Separator for `:list` type (default `","`)

**Transient Properties:**
- `:transient-key` - Key binding in transient menu
- `:transient-description` - Description in transient
- `:transient-class` - Transient class (e.g., `transient-option`)
- `:transient-argument` - Argument string for transient
- `:transient-prompt` - Input prompt string
- `:transient-choices` - Valid choices list
- `:transient-group` - Group name for organization
- `:transient-level` - Menu visibility level (1-7)
- `:transient-order` - Order within group

### How Automatic Command-Line Generation Works

The `beads-command-json` base class provides a default `beads-command-line` method that:

1. Calls `beads-command-subcommand` to get the subcommand name
2. Calls `beads-meta-build-command-line` to automatically generate CLI arguments from slot metadata
3. Appends global flags and `--json` flag

This means that once a command class has:
- Complete slot metadata (CLI and transient properties)
- A `beads-command-subcommand` method implementation

The manual `beads-command-line` method can be **deleted** and the default implementation will automatically generate the correct command line!

## Current State Analysis

### Fully Migrated Pattern

**Example**: `beads-command-doctor`, `beads-command-blocked`

Characteristics:
1. ✅ All slots have complete metadata (both CLI and transient properties)
2. ✅ NO manual `beads-command-line` method (relies on default from `beads-command-json`)
3. ✅ Has `beads-command-subcommand` method returning subcommand name
4. ✅ MAY have custom `beads-command-parse` for domain object conversion
5. ✅ MAY have `beads-command-validate` for custom validation

### Command Classes by Migration Status

#### Category A: Fully Migrated (Complete ✓)
- `beads-command-doctor` (in `beads-command-doctor.el`)
- `beads-command-blocked` (in `beads-command-blocked.el`)

#### Category B: Partially Migrated - Has Metadata But Needs Cleanup
These classes have slot metadata but still have manual `beads-command-line` methods that should be removed:

**In separate files:**
- `beads-command-ready` (in `beads-command-ready.el`) - Has complete metadata, has manual method

**In beads-command.el:**
- `beads-command-list` - Has metadata, has manual method
- `beads-command-create` - Has metadata, has manual method
- `beads-command-show` - Has metadata, has manual method
- `beads-command-update` - Has metadata, has manual method
- `beads-command-close` - Has metadata, has manual method

#### Category C: Partially Migrated - Incomplete Metadata
These classes have SOME slot metadata (usually transient properties only) but lack complete CLI properties AND still have manual methods:

- `beads-command-reopen` - 2 slots have transient metadata, lacks CLI properties
- `beads-command-dep-add` - 1 slot has some metadata
- `beads-command-dep-remove` - 2 slots have some metadata
- `beads-command-dep-tree` - 4 slots have some metadata
- `beads-command-label-add` - 1 slot has some metadata
- `beads-command-label-remove` - 2 slots have some metadata
- `beads-command-epic-status` - 1 slot has some metadata

#### Category D: Not Started - No Metadata
These classes have NO slot metadata and manual `beads-command-line` methods:

- `beads-command-init`
- `beads-command-quickstart`
- `beads-command-export`
- `beads-command-import`
- `beads-command-delete`
- `beads-command-stats`
- `beads-command-dep-cycles`
- `beads-command-label-list-all`
- `beads-command-label-list`
- `beads-command-epic-close-eligible`

## Migration Approach

### Phase 1: Cleanup Partially Migrated Classes (Category B)

**Priority**: P1 (High) - Quick wins with significant impact

For classes that already have complete slot metadata:

1. **Verify metadata completeness**: Ensure all slots have both CLI and transient properties
2. **Test manual method**: Run existing tests to capture expected command-line output
3. **Remove manual method**: Delete the `cl-defmethod beads-command-line` implementation
4. **Verify tests still pass**: Ensure default metadata-based generation produces identical output
5. **Update child task**: Close the corresponding bde-* child task

**Affected classes** (5):
- `beads-command-ready` (separate file)
- `beads-command-list`
- `beads-command-create`
- `beads-command-show`
- `beads-command-update`
- `beads-command-close`

**Estimated complexity**: Low - metadata already exists, just need to remove manual methods and verify tests pass.

### Phase 2: Complete Metadata for Partially Migrated Classes (Category C)

**Priority**: P2 (Medium)

For classes with incomplete metadata:

1. **Analyze current manual method**: Understand what CLI arguments are generated
2. **Add missing CLI properties**: Add `:positional`, `:long-option`, `:short-option`, `:option-type` to slots
3. **Verify/complete transient properties**: Ensure transient properties are complete
4. **Add `beads-command-subcommand` method** if missing
5. **Test metadata-based generation**: Verify output matches manual method
6. **Remove manual method**: Delete the `cl-defmethod beads-command-line` implementation
7. **Update tests**: Ensure all tests pass
8. **Close child task**: Close the corresponding bde-* child task

**Affected classes** (7):
- `beads-command-reopen`
- `beads-command-dep-add`
- `beads-command-dep-remove`
- `beads-command-dep-tree`
- `beads-command-label-add`
- `beads-command-label-remove`
- `beads-command-epic-status`

**Estimated complexity**: Medium - need to analyze existing implementation and add missing metadata properties.

### Phase 3: Full Migration for Unmigrated Classes (Category D)

**Priority**: P3 (Lower) - More effort, less frequently used commands

For classes with no metadata:

1. **Analyze current manual method**: Document CLI argument structure
2. **Design slot metadata**: Plan CLI and transient properties for each slot
3. **Add complete slot metadata**: Add both CLI and transient properties to all slots
4. **Add `beads-command-subcommand` method**: Implement to return subcommand name
5. **Test metadata-based generation**: Verify output matches manual method
6. **Remove manual method**: Delete the `cl-defmethod beads-command-line` implementation
7. **Create/update transient menu**: Generate or update transient definitions
8. **Update tests**: Ensure all tests pass
9. **Close child task**: Close the corresponding bde-* child task

**Affected classes** (10):
- `beads-command-init` (P3)
- `beads-command-quickstart` (P3)
- `beads-command-export` (P3)
- `beads-command-import` (P3)
- `beads-command-delete` (P2)
- `beads-command-stats` (P3)
- `beads-command-dep-cycles` (P2)
- `beads-command-label-list-all` (P2)
- `beads-command-label-list` (P2)
- `beads-command-epic-close-eligible` (P3)

**Estimated complexity**: High - full metadata design and implementation from scratch.

## Migration Pattern by Example

### Example: beads-command-delete (Category D - Not Started)

**Current State** (in beads-command.el ~line 2876):
```elisp
(defclass beads-command-delete (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to delete (positional argument).")
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force deletion without preview (--force flag)."))
  :documentation "Represents bd delete command.")

(cl-defmethod beads-command-line ((command beads-command-delete))
  "Build command arguments for delete COMMAND."
  (with-slots (issue-id force) command
    (let ((args (list "delete"))
          (global-args (cl-call-next-method)))
      (setq args (append args global-args))
      (when issue-id
        (setq args (append args (list issue-id))))
      (when force
        (setq args (append args (list "--force"))))
      args)))
```

**After Migration**:
```elisp
(defclass beads-command-delete (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to delete (positional argument)."
    ;; CLI properties
    :positional 1
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID (required)"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Delete Issue"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force deletion without preview (--force flag)."
    ;; CLI properties
    :long-option "--force"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Delete Issue"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd delete command.")

(cl-defmethod beads-command-subcommand ((_command beads-command-delete))
  "Return subcommand name for delete command."
  "delete")

;; Manual beads-command-line method is DELETED - default implementation
;; from beads-command-json now handles it automatically!
```

### Example: beads-command-ready (Category B - Has Metadata, Needs Cleanup)

**Current State** (in beads-command-ready.el):
- ✅ Has complete slot metadata (lines 39-275)
- ❌ Still has manual `beads-command-line` method (lines 284-339)
- ✅ Has `beads-command-subcommand` method (lines 280-282)

**Migration Steps**:
1. Run existing tests to capture expected command-line output
2. Delete lines 284-339 (the manual `beads-command-line` method)
3. Run tests again - they should still pass!
4. Close task bde-1ae6

**Result**: ~55 lines of boilerplate removed, functionality unchanged.

## Edge Cases and Considerations

### 1. Positional Arguments

**Challenge**: Some commands have positional arguments (e.g., issue IDs) that can be single or multiple values.

**Solution**: Use `:positional` property with appropriate value:
- `:positional 1` for first positional argument
- For lists of positional args, use `:option-type :list`
- The metadata system handles multiple positional args correctly

**Example** (from beads-command-reopen):
```elisp
(issue-ids
  :initarg :issue-ids
  :type (or null list)
  :initform nil
  :documentation "One or more issue IDs to reopen."
  :positional 1
  :option-type :list)
```

### 2. List Options with Multiple Flags

**Challenge**: Some options like `--label` need to be repeated for each value (NOT comma-separated).

**Example**: `bd ready --label backend --label security`

**Solution**: The `beads-meta-build-command-line` function handles `:option-type :list` correctly:
- For positional args: expands to multiple values
- For named options: repeats the flag for each value

**Verification needed**: Test that list options are handled correctly by metadata system.

### 3. Commands with Special Behavior

**Special Cases**:
- `beads-command-init`: No JSON support, very simple
- `beads-command-quickstart`: Interactive command, no JSON
- `beads-command-export`: JSON output goes to stderr (not stdout)

**Approach**: These can still use metadata for transient menus, but may need custom `beads-command-parse` or `beads-command-execute-interactive` methods.

### 4. Commands Already in Separate Files

Commands that already have separate files (`beads-command-ready.el`, `beads-command-blocked.el`, `beads-command-doctor.el`) should stay in their files. Only the manual methods need removal.

### 5. Transient Menu Updates

After migrating command classes, corresponding transient menus might need updates:
- Some transients are manually defined (e.g., in `beads-create.el`)
- Future work: Use `beads-meta-define-transient` to auto-generate transients from metadata
- For this task: Focus on CLI migration, transient updates can follow

### 6. Testing Strategy

For each migrated class:
1. **Before migration**: Capture command-line output from tests
2. **After migration**: Verify command-line output is identical
3. **Regression tests**: Ensure all existing tests still pass
4. **Integration tests**: Test actual CLI execution (if tests exist)

### 7. Validation Methods

Custom `beads-command-validate` methods should be preserved - metadata doesn't replace custom validation logic.

## Acceptance Criteria

For each command class to be considered "fully migrated":

- [ ] All slots have complete metadata (CLI and transient properties)
- [ ] Has `beads-command-subcommand` method implementation
- [ ] NO manual `beads-command-line` method (relies on default from `beads-command-json`)
- [ ] All existing tests pass
- [ ] Command-line output verified to match original implementation
- [ ] Corresponding bde-* child task is closed

For the overall epic (bde-x0om):

- [ ] All 15 remaining command classes migrated (Categories B, C, D)
- [ ] All manual `beads-command-line` methods removed from target classes
- [ ] Significant code reduction achieved (~500+ lines removed)
- [ ] All tests pass (lint, compile, test suite)
- [ ] Documentation updated (if needed)

## Implementation Order

### Recommended Sequence:

**Week 1**: Category B cleanup (Quick wins)
1. beads-command-ready
2. beads-command-list
3. beads-command-show
4. beads-command-create
5. beads-command-update
6. beads-command-close

**Week 2**: Category C completion (Medium effort)
7. beads-command-reopen
8. beads-command-dep-add
9. beads-command-dep-remove
10. beads-command-dep-tree
11. beads-command-label-add
12. beads-command-label-remove
13. beads-command-epic-status

**Week 3**: Category D full migration (Higher effort)
14. beads-command-delete
15. beads-command-init
16. beads-command-export
17. beads-command-import
18. beads-command-stats
19. beads-command-dep-cycles
20. beads-command-label-list-all
21. beads-command-label-list
22. beads-command-epic-close-eligible

## Risks and Mitigations

### Risk 1: Metadata System Bugs

**Risk**: The `beads-meta-build-command-line` function might not handle all edge cases correctly.

**Mitigation**:
- Test thoroughly with existing test suite
- Compare generated command-lines with manual implementations
- Fix metadata system if issues found (separate task)

### Risk 2: Transient Menu Breakage

**Risk**: Removing manual methods might break existing transient menus.

**Mitigation**:
- Review transient definitions before migration
- Test transient menus after migration
- Keep custom transient infixes where needed

### Risk 3: Time Estimation

**Risk**: Migration takes longer than expected (3 weeks estimated).

**Mitigation**:
- Prioritize high-use commands first (Category B)
- Lower-use commands (Category D) can be deferred
- Track progress per command class

## Success Metrics

- **Lines of code removed**: Target ~500+ lines (manual command-line methods)
- **Test coverage**: Maintain or improve (currently >75%)
- **Command classes migrated**: 15+ (out of ~22 total)
- **Bug reports**: Zero regressions from migration
- **Performance**: No degradation (metadata generation is fast)

## Next Steps (Implementation Phase)

After design approval:

1. **Create implementation plan**: Break down into per-class tasks
2. **Set up testing infrastructure**: Ensure we can verify command-line output
3. **Start with Category B**: Quick wins with `beads-command-ready`
4. **Iterate through categories**: Work through B → C → D
5. **Track progress**: Update parent task bde-x0om with progress
6. **Close child tasks**: Mark bde-* tasks as closed as each class completes

## Open Questions

1. **Should we close child tasks in bde-x0om that are marked "closed" but still have manual methods?**
   → Decision: Leave closed, those refer to initial metadata addition. This task is about cleanup.

2. **Should we update transient menus as part of this task?**
   → Decision: Focus on CLI migration only. Transient updates are future work.

3. **What about beads-command-quickstart and beads-command-init (no JSON)?**
   → Decision: Still add metadata for consistency. They can override methods as needed.

## References

- **Parent task**: bde-x0om (Migrate remaining command classes to use slot metadata)
- **Proof of concept**: bde-es27 (Migrate beads-command-create to use slot metadata)
- **Infrastructure**: bde-exg9 (EIEIO Meta-Programming: Reduce boilerplate with slot metadata)
- **Slot metadata documentation**: lisp/beads-meta.el (lines 1-100)
- **Example fully migrated class**: lisp/beads-command-doctor.el
- **Example partial migration**: lisp/beads-command-ready.el

---

**Design Status**: ✅ Complete
**Ready for Implementation**: Yes
**Next Phase**: Implementation (bde-wisp-32o)
