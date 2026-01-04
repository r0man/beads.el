# Quickstart: Adding a New BD Command to Beads.el

**Date**: 2026-01-04
**Feature**: 001-eieio-command-system

## Overview

This guide walks you through adding a new bd CLI command to beads.el using the EIEIO command infrastructure.

## Prerequisites

- Understanding of EIEIO (Emacs Lisp Object System)
- Familiarity with transient library
- Access to `bd <command> --help` output

## Step 1: Research the Command

Run `bd <command> --help` to understand:
- Positional arguments
- Named options (flags and values)
- Command description

Example for `bd ready`:
```
$ bd ready --help
Show ready work (no blockers, open or in_progress)

Usage:
  bd ready [flags]

Flags:
  -n, --limit int              Maximum issues to show
      --format string          Output format (table, json, compact)
      --include-deferred       Include deferred issues
  -h, --help                   help for ready
```

## Step 2: Create Command Class File

Create `lisp/beads-command-<name>.el`:

```elisp
;;; beads-command-ready.el --- Ready command class -*- lexical-binding: t; -*-

;;; Commentary:

;; EIEIO class for `bd ready' command.

;;; Code:

(require 'beads-command)

(defclass beads-command-ready (beads-command-json)
  ;; Define slots for each option
  ((limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Maximum issues to show (--limit)."
    ;; CLI properties
    :long-option "--limit"
    :short-option "-n"
    :option-type :integer
    ;; Transient properties
    :transient-key "n"
    :transient-description "Limit results"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-reader transient-read-number-N+
    :transient-group "Options"
    :transient-level 2
    :transient-order 1)

   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format)."
    :long-option "--format"
    :option-type :string
    :transient-key "f"
    :transient-description "Output format"
    :transient-class transient-option
    :transient-argument "--format="
    :transient-choices ("table" "json" "compact")
    :transient-group "Options"
    :transient-level 2
    :transient-order 2)

   (include-deferred
    :initarg :include-deferred
    :type boolean
    :initform nil
    :documentation "Include deferred issues (--include-deferred)."
    :long-option "--include-deferred"
    :option-type :boolean
    :transient-key "d"
    :transient-description "Include deferred"
    :transient-class transient-switch
    :transient-argument "--include-deferred"
    :transient-group "Filters"
    :transient-level 3
    :transient-order 1))
  :documentation "Represents bd ready command.")

(cl-defmethod beads-command-subcommand ((_command beads-command-ready))
  "Return subcommand name for ready command."
  "ready")

(cl-defmethod beads-command-validate ((_command beads-command-ready))
  "Validate ready command. No required fields."
  nil)

(provide 'beads-command-ready)
;;; beads-command-ready.el ends here
```

## Step 3: Create Transient UI File

Create `lisp/beads-ready.el`:

```elisp
;;; beads-ready.el --- Ready command interface -*- lexical-binding: t; -*-

;;; Commentary:

;; Transient interface for `bd ready' command.

;;; Code:

(require 'beads)
(require 'beads-command-ready)
(require 'beads-meta)
(require 'beads-option)

;;;###autoload (autoload 'beads-ready "beads-ready" nil t)
(beads-meta-define-transient beads-command-ready "beads-ready"
  "Show ready work (no blockers, open or in_progress).

Lists issues that are ready to work on - either open or in_progress
with no blocking dependencies."
  beads-option-global-section)

(provide 'beads-ready)
;;; beads-ready.el ends here
```

## Step 4: Add to Main Menu

Update `lisp/beads-main.el` to include the new command:

```elisp
;; In the appropriate group
["Views"
 ("r" "Ready" beads-ready)
 ("b" "Blocked" beads-blocked)
 ...]
```

## Step 5: Write Tests

Create `lisp/test/beads-ready-test.el`:

```elisp
;;; beads-ready-test.el --- Tests for beads-ready -*- lexical-binding: t; -*-

(require 'ert)
(require 'beads-command-ready)

(ert-deftest beads-ready-subcommand ()
  "Test subcommand returns correct value."
  (let ((cmd (beads-command-ready)))
    (should (equal "ready" (beads-command-subcommand cmd)))))

(ert-deftest beads-ready-validate ()
  "Test validation passes with no args."
  (let ((cmd (beads-command-ready)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-ready-command-line ()
  "Test command line building."
  (let ((cmd (beads-command-ready :limit 10 :include-deferred t)))
    (let ((args (beads-command-line cmd)))
      (should (member "ready" args))
      (should (member "--limit" args))
      (should (member "10" args))
      (should (member "--include-deferred" args)))))

(provide 'beads-ready-test)
;;; beads-ready-test.el ends here
```

## Step 6: Verify

1. **Byte compile**:
   ```bash
   guix shell -D -f guix.scm -- eldev -p -dtT compile
   ```

2. **Run tests**:
   ```bash
   BD_NO_DAEMON=1 guix shell -D -f guix.scm -- eldev -p -dtT test
   ```

3. **Lint**:
   ```bash
   guix shell -D -f guix.scm -- eldev -p -dtT lint
   ```

4. **Manual test**:
   - Start Emacs
   - Load beads.el
   - Run `M-x beads-ready`
   - Verify transient menu appears with correct options

## Slot Property Reference

### CLI Properties

| Property | Required | Description |
|----------|----------|-------------|
| `:long-option` | Yes* | Long option name (--foo) |
| `:short-option` | No | Short option name (-f) |
| `:option-type` | Yes | :string, :boolean, :integer, :list |
| `:positional` | No | Position number for positional args |
| `:option-separator` | No | Separator for :list type |

*Required for non-positional arguments

### Transient Properties

| Property | Required | Description |
|----------|----------|-------------|
| `:transient-key` | Yes | Keybinding (single char) |
| `:transient-description` | Yes | Display text |
| `:transient-class` | No | transient-option, transient-switch |
| `:transient-argument` | Yes* | Transient arg format (--foo=) |
| `:transient-reader` | No | Input reader function |
| `:transient-choices` | No | Valid choices for completion |
| `:transient-prompt` | No | Prompt string |
| `:transient-group` | No | Group name (default: ungrouped) |
| `:transient-level` | No | Visibility level 1-7 (default: 4) |
| `:transient-order` | No | Order within group |

*Required for options (not switches)

## Common Patterns

### Required Positional Argument

```elisp
(title
 :initarg :title
 :type (or null string)
 :initform nil
 :documentation "Issue title (required, positional)."
 :positional 1
 :required t
 :transient-key "t"
 :transient-description "Title (required)"
 :transient-class transient-option
 :transient-reader beads-reader-issue-title
 :transient-group "Required"
 :transient-level 1
 :transient-order 1)
```

### Boolean Switch

```elisp
(verbose
 :initarg :verbose
 :type boolean
 :initform nil
 :documentation "Enable verbose output (--verbose)."
 :long-option "--verbose"
 :short-option "-v"
 :option-type :boolean
 :transient-key "v"
 :transient-description "Verbose"
 :transient-class transient-switch
 :transient-argument "--verbose"
 :transient-group "Output"
 :transient-level 3
 :transient-order 1)
```

### Option with Choices

```elisp
(status
 :initarg :status
 :type (or null string)
 :initform nil
 :documentation "Filter by status (--status)."
 :long-option "--status"
 :option-type :string
 :transient-key "s"
 :transient-description "Status filter"
 :transient-class transient-option
 :transient-argument "--status="
 :transient-choices ("open" "closed" "in_progress" "blocked")
 :transient-group "Filters"
 :transient-level 2
 :transient-order 1)
```

### List Option

```elisp
(labels
 :initarg :labels
 :type (or null list)
 :initform nil
 :documentation "Filter by labels (--label, repeatable)."
 :long-option "--label"
 :option-type :list
 :option-separator ","
 :transient-key "l"
 :transient-description "Labels"
 :transient-class transient-option
 :transient-argument "--label="
 :transient-reader beads-reader-labels
 :transient-group "Filters"
 :transient-level 2
 :transient-order 2)
```

## Custom Rendering

To override default terminal rendering:

```elisp
(cl-defmethod beads-command-execute-interactive ((cmd beads-command-list))
  "Display list results in tabulated-list buffer."
  (let ((executed (beads-command-execute cmd)))
    (if (zerop (oref executed exit-code))
        (beads-list--display (oref executed result))
      (beads-error "List failed: %s" (oref executed stderr)))))
```

**Note on result slot**:
- For non-JSON commands: `result` contains raw `stdout` string
- For JSON commands: `result` contains parsed elisp values/objects
- Subclasses may further transform `result` (e.g., convert to `beads-issue` objects)
