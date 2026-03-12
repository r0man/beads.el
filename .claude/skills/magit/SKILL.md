---
name: magit
description: Expert guide for Magit architecture, section API, buffer management, process handling, and mode patterns. Use when implementing or debugging magit-section, magit-mode, magit-insert-section, magit-process, or git porcelain UI.
---

# Magit Expert

This skill provides comprehensive guidance for working with Magit — the
Emacs Git porcelain — based on Magit 4.x source code and real-world
usage in packages like beads.el and Forge.

## When to Use This Skill

Invoke this skill when:
- Working with `magit-section`, `magit-insert-section`, section navigation
- Deriving from `magit-mode` or `magit-section-mode`
- Implementing buffer refresh patterns (`magit-refresh`)
- Using `magit-process` / process-file for Git execution
- Integrating with the Magit status buffer via hooks
- Building keyboard-driven UI in the Magit style
- Adding sections to `magit-status-sections-hook`

## Core Architecture

```
special-mode
  └── magit-section-mode   (read-only, section navigation)
        └── magit-mode     (abstract, buffer lifecycle)
              ├── magit-status-mode
              ├── magit-log-mode
              ├── magit-diff-mode
              ├── magit-refs-mode
              └── (your-derived-mode)
```

Key files: `magit-section.el`, `magit-mode.el`, `magit-process.el`,
`magit-status.el`, `magit-git.el`

## Section API (magit-section.el)

### Core Section Class

`magit-section` is an EIEIO class with key slots:

| Slot | Type | Purpose |
|------|------|---------|
| `type` | symbol | Section type identifier |
| `value` | any | Section data (unique per instance) |
| `keymap` | keymap | Section-specific keybindings |
| `start` | marker | Buffer position of section start |
| `content` | marker | Buffer position of content start |
| `end` | marker | Buffer position of section end |
| `hidden` | bool | Whether section is collapsed |
| `parent` | section | Parent section |
| `children` | list | Child sections |

Sections are stored as the `magit-section` text property on buffer text.

### Creating Sections

```elisp
;; The primary macro for creating sections
(magit-insert-section (TYPE &optional VALUE HIDE)
  ;; Insert heading text here
  (magit-insert-heading "Section Title")
  ;; Insert child sections or content
  (dolist (item items)
    (magit-insert-section (item-type item)
      (insert (format "%s\n" item)))))
```

**Parameters:**
- `TYPE` — symbol naming this section type (e.g., `'file`, `'hunk`, `'commit`)
- `VALUE` — arbitrary data stored in section's `value` slot
- `HIDE` — non-nil to start collapsed

**Key pattern: always call `magit-insert-heading` before child content:**

```elisp
(magit-insert-section (mytype myvalue)
  (magit-insert-heading
    (propertize "Title" 'face 'magit-section-heading)
    ": "
    (propertize myvalue 'face 'magit-hash))
  ;; Child sections follow here
  (insert "child content\n"))
```

### Section Retrieval

```elisp
(magit-section-at)               ; Section at point (via text property)
(magit-current-section)          ; Section at point (context-menu aware)
(oref (magit-current-section) type)   ; Get section type
(oref (magit-current-section) value)  ; Get section value

;; Type checking
(magit-section-match 'file)           ; Is current section of type file?
(magit-section-match '(file hunk))    ; Match type hierarchy
```

### Section Navigation Keys

| Key | Command | Action |
|-----|---------|--------|
| `TAB` | `magit-section-toggle` | Expand/collapse |
| `C-<tab>` | `magit-section-cycle` | Cycle visibility |
| `<backtab>` | `magit-section-cycle-global` | Show all |
| `n` / `p` | `magit-section-forward/backward` | Next/prev section |
| `M-n` / `M-p` | `magit-section-forward/backward-sibling` | Sibling jump |
| `^` | `magit-section-up` | Go to parent |
| `1`–`4` | `magit-section-show-level-N` | Show N levels deep |

### Visibility Customization

```elisp
;; Set initial visibility per section type
(setq magit-section-initial-visibility-alist
      '((stashes . hide)
        (upstream . show)))

;; Cache visibility across refreshes
(setq magit-section-cache-visibility t)

;; Custom visibility hook
(add-hook 'magit-section-set-visibility-hook #'my-visibility-fn)
```

### Section Highlighting

Faces for styling:
- `magit-section-highlight` — Background of current section
- `magit-section-heading` — Section headings
- `magit-section-heading-selection` — Selected heading
- `magit-section-secondary-heading` — Secondary headings

## Buffer Management (magit-mode.el)

### Mode Setup

`magit-section-mode` sets up:
- `buffer-read-only: t` — No accidental edits
- `buffer-disable-undo` — Performance
- `truncate-lines: t` — No line wrapping
- Custom `magit-section-mode-map`

### Creating Magit Buffers

```elisp
(defvar-local my-buffer-var nil
  "Buffer-local context variable.")

(define-derived-mode my-magit-mode magit-section-mode "My Mode"
  "Description."
  :group 'my-group
  (setq-local revert-buffer-function #'my-refresh-buffer))

(defun my-display ()
  "Open my buffer."
  (interactive)
  (let ((buf (get-buffer-create (format "*my-buffer[%s]*"
                                        (project-root (project-current))))))
    (with-current-buffer buf
      (my-magit-mode)
      (my-refresh-buffer))
    (pop-to-buffer buf)))

(defun my-refresh-buffer (&rest _)
  "Refresh my buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (my-insert-sections)))
```

### Buffer Naming

Magit uses format strings for buffer names:

```elisp
(setq magit-buffer-name-format "*magit: %M%V*")
;; %M = mode name (e.g., "magit-status")
;; %V = value context (e.g., repo path)
;; %t = top-level directory
```

### Display Strategies

```elisp
;; Configurable display function
(setq magit-display-buffer-function
      #'magit-display-buffer-traditional)

;; Alternatives:
;; magit-display-buffer-same-window-except-diff-v1
;; magit-display-buffer-fullframe-status-v1

;; Low-level display
(magit-display-buffer buffer)
```

### Refresh Mechanism

```elisp
;; Trigger refresh
(magit-refresh)              ; Refresh current magit buffer
(magit-refresh-buffer buf)   ; Refresh specific buffer

;; Hooks (in order of execution)
magit-pre-refresh-hook       ; Before refresh (e.g., save buffers)
magit-refresh-buffer-hook    ; Per-buffer on each refresh
magit-post-refresh-hook      ; After refresh (e.g., auto-revert)
magit-create-buffer-hook     ; On first creation only
```

### Status Buffer Sections Hook

```elisp
;; Add your section to magit-status
(magit-add-section-hook 'magit-status-sections-hook
                        #'my-insert-section
                        nil   ; Append (vs position)
                        t)    ; Local to this buffer

;; Hook functions insert sections into status buffer
(defun my-insert-section ()
  "Insert my section into magit-status."
  (when (my-relevant-p)
    (magit-insert-section (my-type)
      (magit-insert-heading "My Section")
      (dolist (item (my-items))
        (magit-insert-section (my-item item)
          (insert (format "  %s\n" item)))))))
```

### Built-in Status Sections

The `magit-status-sections-hook` runs these by default (in order):
- `magit-insert-status-headers` — Branch, remote, tracking info
- `magit-insert-merge-log` — Active merge state
- `magit-insert-rebase-sequence` — Active rebase state
- `magit-insert-am-sequence` — Active `git am` state
- `magit-insert-sequencer-sequence` — Cherry-pick/revert
- `magit-insert-bisect-output` — Bisect state
- `magit-insert-untracked-files` — Untracked files section
- `magit-insert-unstaged-changes` — Unstaged diffs
- `magit-insert-staged-changes` — Staged diffs
- `magit-insert-stashes` — Stash list
- `magit-insert-unpushed-to-pushremote` — Unpushed commits
- `magit-insert-unpushed-to-upstream` — Upstream unpushed

## Process Handling (magit-process.el)

### Tramp-Compatible Git Execution

**Critical rule: Always use `process-file`, never `call-process`.**
This enables remote execution via Tramp.

```elisp
;; Low-level: synchronous, captures output
(magit-call-git "commit" "-m" "message")

;; Higher-level: synchronous with error handling
(magit-git "log" "--oneline" "-10")

;; Async: runs in background, logs to process buffer
(magit-run-git-async "fetch" "--prune")

;; With environment
(let ((process-environment (cons "GIT_EDITOR=true"
                                 process-environment)))
  (magit-call-git "rebase" "--continue"))
```

### Process Output Buffer

```elisp
;; Get the process log buffer
(magit-process-buffer)

;; Control logging
(setq magit-process-log-max 32)          ; Keep last N sections
(setq magit-process-popup-time -1)        ; Never auto-show (-1)
                                          ; Or: seconds to wait

;; ANSI colors in process output
(setq magit-process-apply-ansi-colors t)
```

### Error Handling

```elisp
;; git exit code → signal error
(magit-call-git "commit") ; Raises error on non-zero exit

;; Custom error display
(setq magit-process-display-mode-line-error t)
(setq magit-process-error-tooltip-max-lines 20)
```

### process-file vs call-process

```elisp
;; For Tramp compatibility in your own code:
(process-file "git" nil t nil "log" "--oneline")
;;  ^program   ^stdin ^output ^display ^args...

;; NOT this (breaks on remote files):
(call-process "git" nil t nil "log" "--oneline")
```

When working with Tramp-hosted files, strip the Tramp prefix for
paths passed to git:

```elisp
(file-local-name default-directory)  ; "/ssh:host:/path" → "/path"
```

## Transient Integration

Magit uses the `transient` library for all interactive menus.
See the `emacs-transient` skill for full documentation.

### Key Patterns from Magit

```elisp
;; Magit's main dispatch
(transient-define-prefix magit-dispatch ()
  "Invoke a Magit command from a list of available commands."
  :man-page "git"
  ["Transient and dwim commands"
   [("A" "Apply"          magit-cherry-pick)
    ("b" "Branch"         magit-branch)
    ("c" "Commit"         magit-commit)]])

;; Context-aware suffix: checks section type
(transient-define-suffix magit-stage-file (file)
  "Stage FILE."
  (interactive
   (list (magit-read-file "Stage file")))
  (magit-run-git "add" "--" file))
```

### Augmenting Existing Magit Transients

```elisp
;; Add to magit-dispatch
(transient-append-suffix 'magit-dispatch "!"
  '("X" "My command" my-magit-command))

;; Add to magit-fetch
(transient-append-suffix 'magit-fetch "-p"
  '("-t" "Fetch tags" "--tags"))
```

## Keymap Patterns

```elisp
;; Derive from magit-section-mode-map
(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'my-visit-section)
    (define-key map (kbd "o")   #'my-browse-section)
    (define-key map (kbd "g")   #'my-refresh-buffer)
    map)
  "Keymap for `my-mode'.")

;; Section-type-specific keymaps
(defvar my-item-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "d") #'my-delete-item)
    map))

;; Register section-type keymap
(magit-insert-section (my-item value nil my-item-section-map)
  (insert (format "%s\n" value)))
```

## Common Patterns

### Context-Aware Commands

```elisp
(defun my-visit-thing ()
  "Visit the thing at point."
  (interactive)
  (let ((section (magit-current-section)))
    (pcase (oref section type)
      ('my-issue (my-visit-issue (oref section value)))
      ('my-pr    (my-visit-pr    (oref section value)))
      (_         (user-error "No thing at point")))))
```

### Inserting Headers

```elisp
(defun my-insert-headers ()
  "Insert header information."
  (magit-insert-section (my-headers)
    (magit-insert-heading "Status")
    (insert (format "%-10s %s\n"
                    (propertize "Branch:" 'face 'magit-header-line-key)
                    (propertize (magit-get-current-branch)
                                'face 'magit-branch-local)))))
```

### Section with Clickable Items

```elisp
(defun my-insert-items (items)
  "Insert ITEMS as clickable sections."
  (magit-insert-section (my-list)
    (magit-insert-heading
      (format "Items (%d)" (length items)))
    (dolist (item items)
      (magit-insert-section (my-item item)
        (insert
         (concat
          (propertize (my-item-id item)   'face 'magit-hash)
          " "
          (propertize (my-item-title item) 'face 'magit-section-secondary-heading)
          "\n"))))))
```

### Integration with project.el

```elisp
;; Discover .mydir from project root
(defun my-find-config ()
  "Find config starting from project root."
  (when-let ((root (magit-toplevel)))
    (locate-dominating-file root ".mydir")))
```

## Important Notes

1. **Read-only buffers**: Use `(let ((inhibit-read-only t)) ...)` when inserting
2. **Section text properties**: Magit stores sections via `magit-section` text property
3. **Buffer-local state**: Use `defvar-local` for per-buffer context
4. **Refresh pattern**: `erase-buffer` + re-insert sections (don't patch in place)
5. **Section hierarchy**: `magit-insert-heading` must come before child `magit-insert-section` calls
6. **process-file**: Always for git commands — never `call-process` or `shell-command`

## Common Gotchas

1. **Missing magit-insert-heading**: Content before it is part of the heading, not body
2. **Wrong section keymap**: Set the keymap as 4th arg to `magit-insert-section`
3. **Forgetting inhibit-read-only**: Buffer is read-only — wrap inserts
4. **Not erasing before refresh**: Old content persists underneath new
5. **Calling call-process for git**: Breaks Tramp — use `process-file` or `magit-call-git`

## Version Notes

- Magit 4.x: Requires Emacs 26.1+, transient 0.7.0+
- Section API stable since Magit 3.x
- `magit-section-mode` is the new base (was `magit-mode` in older versions)
