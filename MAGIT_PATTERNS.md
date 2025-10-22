# Magit and Forge Design Patterns for beads.el

This document captures design patterns from Magit and Forge that beads.el
should follow to provide a consistent, polished Emacs interface.

## References

- **Magit Repository**: https://github.com/magit/magit
  - Local reference: `magit-reference/`
- **Forge Repository**: https://github.com/magit/forge
  - Local reference: `magit-reference/forge-reference/`

## Core Principles

1. **Consistency**: All transient menus should follow the same patterns
2. **Discoverability**: Key bindings and options should be self-explanatory
3. **Context-awareness**: Commands adapt based on current context
4. **Polish**: Every detail matters - naming, grouping, visual presentation

## Transient Menu Structure

### Basic Menu Anatomy

Magit transient menus follow this structure:

```elisp
(transient-define-prefix magit-commit ()
  "Create a new commit or replace an existing commit."
  :info-manual "(magit)Initiating a Commit"  ; Optional: link to manual
  :man-page "git-commit"                      ; Optional: man page
  ["Arguments"                                 ; Arguments section first
   ("-a" "Stage all modified and deleted files" ("-a" "--all"))
   ("-e" "Allow empty commit" "--allow-empty")
   ("-v" "Show diff of changes to be committed" ("-v" "--verbose"))
   (magit:--author :description "Override the author")]
  [["Create"                                   ; Action sections grouped
    ("c" "Commit" magit-commit-create)]
   ["Edit HEAD"
    ("e" "Extend" magit-commit-extend)
    ("a" "Amend" magit-commit-amend)
    ("w" "Reword" magit-commit-reword)]
   ["Edit"
    ("f" "Fixup" magit-commit-fixup)
    ("s" "Squash" magit-commit-squash)]])
```

**Key observations:**
- Arguments come before actions
- Related actions are grouped into columns
- Short descriptions explain what each command does
- Hierarchical grouping: `[["Column 1"] ["Column 2"]]`

### Forge Menu Structure

Forge extends this pattern for issue tracking:

```elisp
(transient-define-prefix forge-dispatch ()
  "Dispatch a forge command."
  :transient-non-suffix #'transient--do-call
  :refresh-suffixes t
  :environment #'forge--menu-environment
  [["Fetch"
    ("f f" "all topics" forge-pull)
    ("f t" "one topic" forge-pull-topic)]
   ["Create"
    :if (##forge-get-repository :tracked?)
    ("c i" "issue" forge-create-issue)
    ("c p" "pull-request" forge-create-pullreq)]
   ["Visit"
    ("v t" "topic" forge-visit-topic)
    ("v i" "issue" forge-visit-issue)]])
```

**Additional patterns:**
- `:refresh-suffixes t` - Update menu after commands run
- `:transient-non-suffix #'transient--do-call` - How to handle non-suffix
  keys
- `:if` predicate - Conditionally show/hide groups
- Multi-character bindings like "f f", "c i" for namespacing

## Argument/Flag Patterns

### Flag Format

Magit uses consistent flag formatting:

1. **Boolean flags** (no value required):
   ```elisp
   ("-a" "Stage all modified and deleted files" ("-a" "--all"))
   ("-e" "Allow empty commit" "--allow-empty")
   ("-v" "Show diff of changes to be committed" ("-v" "--verbose"))
   ("-n" "Disable hooks" ("-n" "--no-verify"))
   ```

   Format: `(KEY DESCRIPTION FLAGS)`
   - KEY: Single dash + letter (e.g., "-a")
   - DESCRIPTION: Clear explanation of what the flag does
   - FLAGS: Either a single string or a list of short and long forms

2. **Value flags** (require input):
   ```elisp
   (transient-define-argument magit-commit:--date ()
     :description "Override the author date"
     :class 'transient-option
     :shortarg "-D"
     :argument "--date="
     :reader #'transient-read-date)
   ```

   Format: Define custom argument with `transient-define-argument`
   - `:shortarg` - Single-letter shortcut shown in menu
   - `:argument` - Full CLI argument ending in "="
   - `:reader` - Custom function to read the value
   - `:class 'transient-option` - Indicates it takes a value

### Argument Naming Conventions

- **Short args**: Single dash + letter (e.g., `-a`, `-D`, `-n`)
- **Long args**: Double dash + full word (e.g., `--all`, `--date=`,
  `--no-verify`)
- **Descriptions**: Active voice, concise, explain the effect
  (not just repeat the flag name)

### Argument Grouping

Arguments are grouped logically in the "Arguments" section:

```elisp
["Arguments"
 ;; Most common flags first
 ("-a" "Stage all modified and deleted files" ("-a" "--all"))
 ("-e" "Allow empty commit" "--allow-empty")
 ("-v" "Show diff of changes to be committed" ("-v" "--verbose"))
 ("-n" "Disable hooks" ("-n" "--no-verify"))

 ;; Less common flags with level
 (magit-commit:--date :level 7)
 (magit:--gpg-sign :level 5)]
```

## Action/Command Patterns

### Command Format

Actions follow this pattern:

```elisp
("KEY" "DESCRIPTION" COMMAND)
```

Examples from Magit:
```elisp
("c" "Commit" magit-commit-create)
("e" "Extend" magit-commit-extend)
("a" "Amend" magit-commit-amend)
```

### Command Grouping

Actions are organized into logical columns with headers:

```elisp
[["Create"                   ; Header for column
  ("c" "Commit" magit-commit-create)]
 ["Edit HEAD"                ; Second column
  ("e" "Extend" magit-commit-extend)
  ("a" "Amend" magit-commit-amend)
  ("w" "Reword" magit-commit-reword)]
 ["Edit"                     ; Third column
  ("f" "Fixup" magit-commit-fixup)
  ("s" "Squash" magit-commit-squash)]]
```

**Grouping principles:**
- Group related commands together
- Use clear, concise column headers
- Put most common commands first
- Use empty strings `""` for spacing when needed

### Context-Aware Commands

Forge uses context-aware command naming:

```elisp
("f" "fetch" forge-pull-this-topic)        ; "this" = at point
("b" "browse" forge-browse-this-topic)     ; "this" = at point
("v" "visit" forge-visit-topic)            ; no "this" = prompt user
```

**Pattern:**
- Commands with "this" in name operate on item at point
- Commands without "this" prompt for target
- Descriptions use lowercase for actions

## Key Binding Conventions

### Single-Letter Bindings

Common patterns across Magit:

- `c` - Create, Commit, Checkout
- `e` - Edit, Extend
- `a` - Amend, Add, Apply
- `f` - Fetch, Fixup, Fork
- `s` - Squash, Stage, Show
- `w` - Reword, Write
- `d` - Delete, Discard, Draft
- `b` - Browse, Branch
- `v` - Visit, View
- `r` - Reset, Reword, Rebase, Respond
- `p` - Push, Pull, Preview
- `n` - Note, Next
- `m` - Merge, Milestone, Marks
- `l` - Labels, List
- `t` - Title, Topic, Type

### Multi-Character Bindings

Forge uses multi-character bindings for namespacing:

```elisp
("f f" "all topics" forge-pull)            ; f = fetch namespace
("f t" "one topic" forge-pull-topic)
("f n" "notifications" forge-pull-notifications)

("c i" "issue" forge-create-issue)         ; c = create namespace
("c p" "pull-request" forge-create-pullreq)
("c d" "discussion" forge-create-discussion)

("v t" "topic" forge-visit-topic)          ; v = visit namespace
("v i" "issue" forge-visit-issue)
("v p" "pull-request" forge-visit-pullreq)
```

**Pattern for beads.el:**
- Use multi-character bindings when you have many related commands
- First character is the namespace (category)
- Second character identifies the specific command
- Keeps the menu organized and discoverable

## Visual Conventions

### Transient Suffix Attributes

Forge uses these attributes for visual feedback:

```elisp
:transient #'transient--do-replace   ; Replace current menu
:inapt-if <predicate>                ; When command is not applicable
:inapt-face 'forge-suffix-active     ; Face for inapt items
:if <predicate>                      ; Show/hide based on condition
:level N                             ; Visibility level (0=hidden, 7=advanced)
```

### Faces for State

Forge defines custom faces for different states:

```elisp
(defface forge-suffix-active
  '((t :inherit transient-value :weight bold))
  "Face for suffixes whose effect is currently active.")

(defface forge-suffix-implied
  '((t :inherit transient-value :weight normal))
  "Face for suffixes whose effect is currently implied.")
```

**Application in beads.el:**
- Use `:inapt-if` to gray out unavailable commands
- Use custom faces to show active filters or states
- Use `:level` to hide advanced options by default

## Global Flags Handling

### Git Global Flags in Magit

Magit handles Git's global flags (like `--git-dir`, `--work-tree`)
differently from command-specific options:

1. **Not exposed in transient menus** - Global flags are handled
   automatically
2. **Set via variables** - Configuration through `magit-git-global-arguments`
3. **Added automatically** - The `magit-git-command` function adds them

### Beads Global Flags

Beads has similar global flags: `--actor`, `--db`, `--no-daemon`, etc.

**Recommended approach** (following Magit):
1. **Don't expose in every transient menu** - Would clutter the UI
2. **Configure via variables**:
   ```elisp
   (defcustom beads-actor nil
     "Default actor for bd commands.")

   (defcustom beads-db-path nil
     "Explicit database path (overrides .beads discovery).")
   ```
3. **Add automatically in `beads--build-command`** - Already implemented
4. **Optionally expose in settings menu** - Create `beads-configure`
   menu similar to `forge-configure`

**Example settings menu** (for later implementation):

```elisp
(transient-define-prefix beads-configure ()
  "Configure beads.el settings."
  ["Global Options"
   (beads:--actor :description "Default actor")
   (beads:--db :description "Database path")
   ("-d" "Disable daemon" "--no-daemon")])
```

## Buffer Management Patterns

### Buffer Naming

Magit and Forge use consistent buffer naming:

- `*magit-status: PROJECT*` - Project-specific status
- `*forge-topics: OWNER/REPO*` - Repository-specific list
- `*forge-topics*` - Global list (all repositories)

**Pattern for beads.el:**
- `*beads-list: PROJECT*` - Project-specific issue list (already implemented)
- `*beads-show: bd-123*` - Issue detail view (already implemented)
- Follow `*name: context*` or `*name*` pattern

### Buffer Modes

Both use `special-mode` or `magit-mode` as base:

```elisp
(define-derived-mode forge-topics-mode magit-mode "Topics"
  "Major mode for browsing a list of topics.")
```

**beads.el already follows this:**
- `beads-list-mode` derives from `tabulated-list-mode`
- `beads-show-mode` derives from `special-mode`
- Both are read-only with custom keymaps

## State Management

### Forge Filter State

Forge uses a spec object to manage list filters:

```elisp
(defcustom forge-list-buffer-default-topic-filters
  (forge--topics-spec :type 'topic :active t :state 'open :order 'newest)
  "Filters initially used to limit topics listed in list buffers.")
```

The spec is stored in a buffer-local variable and updated by filter commands.

**Pattern for beads.el:**
- Consider using a similar spec-based approach for list filters
- Store filter state in buffer-local variables
- Allow users to customize default filters
- Update filters via transient menu commands

### Transient State

Magit/Forge use buffer-local variables for transient state:

```elisp
(defvar-local forge--buffer-topics-spec nil
  "The filter spec for the current buffer.")
```

**beads.el consideration:**
- Already uses buffer-local `default-directory` for project context
- Could add buffer-local filter state for list views
- Use `defvar-local` for buffer-specific state

## Menu Organization Patterns

### Hierarchical Menus

Forge uses hierarchical menu organization:

1. **Top-level dispatch** - `forge-dispatch` (shows all categories)
2. **Category menus** - `forge-topic-menu`, `forge-topics-menu`
3. **Specific action menus** - `forge-topic-state-menu`

**Pattern for beads.el:**
- `beads-main` - Top-level dispatch (already exists)
- Category menus - `beads-create`, `beads-update`, `beads-close` (already
  exist)
- Add more specific menus as needed (e.g., `beads-label-menu`)

### Menu Reusability

Forge defines reusable menu groups:

```elisp
(defvar forge--topic-menus-group
  '[:description forge--topic-menus-group-description
    ("N m" forge-topics-menu)
    ("N t" forge-topic-menu)])
```

These groups are inserted into multiple menus using the variable name.

**Not needed for beads.el yet**, but could be useful if menus become
very complex.

## Command Implementation Patterns

### Suffix Commands

Magit defines commands as transient suffixes:

```elisp
(transient-define-suffix magit-commit-create (&optional args)
  "Create a new commit."
  (interactive (list (magit-commit-arguments)))
  ;; Implementation...
)
```

**Key points:**
- Use `transient-define-suffix` for commands in menus
- Accept `args` parameter populated by `(magit-commit-arguments)`
- The arguments function retrieves current transient state

### Argument Retrieval

```elisp
(defun magit-commit-arguments ()
  (transient-args 'magit-commit))
```

This retrieves all currently selected arguments from the transient menu.

**beads.el already does this** in modules like `beads-create.el`:
- Commands retrieve arguments from module-specific variables
- Similar pattern, but uses module variables instead of `transient-args`
- Both approaches work well

## Recommendations for beads.el

### 1. Transient Menu Format

**Update all transient menus to follow Magit patterns:**

- Add "Arguments" section before actions
- Use consistent flag format: `("-X" "Description" "--flag")`
- Group actions into logical columns with headers
- Use multi-character bindings for related commands

### 2. Flag Presentation

**For `bd` command flags:**

```elisp
["Arguments"
 ("-p" "Priority (0-4, 0=highest)" "--priority=")
 ("-t" "Issue type" "--type=")
 ("-P" "Parent issue" "--parent=")
 ("-T" "Tags (comma-separated)" "--tags=")]
```

**Not:**
```elisp
["Options"
 ("title" "Issue title" beads-create--set-title)]  ; Too verbose
```

### 3. Global Flags

**Don't expose `--actor`, `--db`, `--no-daemon` in every menu.**

Instead:
- Handle automatically in `beads--build-command` (already done)
- Configure via variables (already done with `beads-actor`)
- Optionally create `beads-configure` menu for power users

### 4. Context-Aware Commands

**Use "this" in command names** when operating on item at point:

```elisp
beads-show-this-issue      ; Show issue at point
beads-close-this-issue     ; Close issue at point
beads-show-issue           ; Prompt for issue ID
```

### 5. Key Bindings

**Follow Magit conventions:**

- `c` - Create
- `u` - Update
- `k` - Close (kill)
- `s` - Show
- `l` - List
- `r` - Ready (show ready issues)
- `b` - Blocked (show blocked issues)
- `g` - Refresh (already standard in Emacs)
- `p` - Preview
- `RET` - Visit/show item at point

### 6. Validation and Preview

**Magit/Forge provide preview and validation:**

```elisp
;; Preview what will be executed
("p" "Preview command" beads-create--preview :transient t)

;; Reset form
("r" "Reset" beads-create--reset :transient t)

;; Execute
("c" "Create issue" beads-create--execute)
```

**beads.el already has this!** Keep this pattern.

### 7. Level System

**Use `:level` for advanced options:**

```elisp
("-n" "Disable hooks" "--no-verify" :level 5)
("-D" beads:--date :level 7)
```

- Level 1-4: Always visible
- Level 5: Shown with `C-x l` once
- Level 7: Rarely shown, for advanced users

## Text-Based Menu Preview Format

When creating text previews for transient menus in issue descriptions,
use this format:

```
Transient: Create Issue
────────────────────────────────────────────────────────────

Arguments
 -p, --priority=<n>  Priority (0-4, 0=highest)
 -t, --type=<type>   Issue type (bug|feature|task|epic|chore)
 -T, --tags=<list>   Tags (comma-separated)
 -P, --parent=<id>   Parent issue ID

Required
 t  Title         Issue title

Optional
 d  Description   Multi-line description
 D  Design        Design notes

Actions
 c  Create issue
 p  Preview command
 r  Reset
 q  Quit
```

**Format notes:**
- Use box-drawing characters or simple lines for separation
- Group arguments, required fields, optional fields, and actions
- Show short flag, long flag, and description
- Show key binding and command name for actions
- Keep it under 80 columns wide

## Examples from Magit/Forge

### Example 1: magit-commit

```
Committing
────────────────────────────────────────────────────────────

Arguments
 -a, --all              Stage all modified and deleted files
 -e, --allow-empty      Allow empty commit
 -v, --verbose          Show diff of changes to be committed
 -n, --no-verify        Disable hooks
 -R, --reset-author     Claim authorship and reset author date
 -A, --author=<author>  Override the author
 -D, --date=<date>      Override the author date

Create          Edit HEAD       Edit
 c  Commit       e  Extend       f  Fixup
                 a  Amend        s  Squash
                 w  Reword       A  Alter
```

### Example 2: forge-dispatch

```
Forge
────────────────────────────────────────────────────────────

Fetch           Create              Visit
 f f  all       c i  issue           v t  topic
 f t  one       c p  pull-request    v i  issue
 f n  notify    c d  discussion      v p  pull-request

Browse          Display
 b i  issues    -S  Toggle in status buffer
 b p  PRs       -H  Toggle legend
 b r  remote
```

## Summary Checklist

When implementing or updating a beads.el transient menu:

- [ ] Arguments section comes before actions
- [ ] Flags use `-X` short form and `--long-form`
- [ ] Descriptions are concise and explain the effect
- [ ] Actions are grouped into logical columns with headers
- [ ] Key bindings follow Magit conventions
- [ ] Context-aware commands use "this" in names
- [ ] Global flags (`--actor`, `--db`) are not exposed
- [ ] Preview and reset commands have `:transient t`
- [ ] Advanced options use `:level 5` or `:level 7`
- [ ] Text-based preview is included in the issue description

## Further Reading

- Magit manual: https://magit.vc/manual/
- Transient manual: https://magit.vc/manual/transient/
- Forge manual: https://magit.vc/manual/forge/

---

*Generated for beads.el-65: Epic for analyzing Magit and Forge design
patterns*
