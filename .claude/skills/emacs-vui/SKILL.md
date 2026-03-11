---
name: emacs-vui
description: Expert guide for building Emacs UIs with vui.el (React-like declarative components). Use when implementing vui-defcomponent, vui-mount, vui-use-async, or any vui primitive (vui-text, vui-button, vui-table, vui-hstack, vui-vstack).
---

# vui.el Expert

This skill provides comprehensive guidance for building declarative,
component-based UIs in Emacs using vui.el. vui.el is "React for Emacs
buffers" - declarative, component-based, and reactive.

## When to Use This Skill

Invoke this skill when:
- Implementing new `vui-defcomponent` components
- Working with vui primitives (text, button, field, table, etc.)
- Using hooks (use-effect, use-async, use-ref, use-memo)
- Building async-loading UIs with progressive rendering
- Replacing magit-section or manual buffer rendering with vui
- Debugging vui component state or rendering issues

## Core Concepts

- **Declarative**: Describe what your UI looks like, not how to update it
- **Component-based**: Build UIs from small, reusable pieces
- **Reactive**: When state changes, the UI updates automatically
- **No external deps**: Only requires built-in cl-lib and wid-edit

## Component Definition

```elisp
(vui-defcomponent name (prop-list)
  :state ((state-var initial-value) ...)
  :render body
  :on-mount body        ; Runs once after first render
  :on-update body       ; Runs after every re-render (not first)
  :on-unmount body      ; Runs before removal
  :should-update body)  ; Return nil to skip re-render
```

### Props (Inputs)

```elisp
(vui-defcomponent greeting (name)
  :render (vui-text (format "Hello, %s!" name)))

;; Use with keyword args:
(vui-component 'greeting :name "Alice")
```

Optional props:

```elisp
(vui-defcomponent user-badge (name role &optional avatar)
  :render
  (vui-hstack
   (when avatar (vui-text avatar))
   (vui-text name)
   (vui-text (format "[%s]" role) :face 'shadow)))
```

### State

```elisp
(vui-defcomponent counter ()
  :state ((count 0))
  :render
  (vui-fragment
   (vui-text (format "Count: %d" count))
   (vui-newline)
   (vui-button "Increment"
               :on-click (lambda ()
                           (vui-set-state :count (1+ count))))
   (vui-button "Reset"
               :on-click (lambda ()
                           (vui-set-state :count 0)))))
```

### Children

Components can receive children:

```elisp
(vui-defcomponent card (title)
  :render
  (vui-fragment
   (vui-text (format "=== %s ===" title))
   (vui-newline)
   children    ; <-- built-in variable
   (vui-newline)
   (vui-text "============")))

(vui-component 'card :title "My Card"
               (vui-text "Card content"))
```

### Mounting

```elisp
(vui-mount (vui-component 'my-app) "*my-app*")
```

## Primitives

### Text Display

```elisp
(vui-text "Hello")
(vui-text "Error!" :face 'error)
(vui-text "Bold" :face '(:weight bold :foreground "red"))
(vui-text "Keyed" :key "status")
(vui-newline)
(vui-space)      ; 1 space
(vui-space 5)    ; 5 spaces
```

### Buttons

```elisp
(vui-button "Click me"
            :on-click (lambda () (message "Clicked!")))

;; All properties:
(vui-button "Label"
            :on-click #'handler
            :face 'warning
            :disabled (not valid)
            :max-width 15           ; Truncate long labels
            :no-decoration t        ; No [ ] brackets
            :help-echo "Tooltip"    ; nil disables (2x faster)
            :tab-order -1           ; Skip in TAB navigation
            :keymap my-keymap
            :key "btn-id")
```

### Fields (Text Input)

```elisp
(vui-field :value current-value
           :on-change (lambda (v) (vui-set-state :name v))
           :on-submit (lambda () (do-search))
           :size 30
           :secret t           ; Password mode
           :placeholder "Type..."
           :face 'default
           :key "search-input")

;; Read field value programmatically:
(vui-field-value "search-input")
```

### Checkbox

```elisp
(vui-checkbox :checked enabled
              :label "Enable feature"
              :on-change (lambda (v) (vui-set-state :enabled v)))
```

### Select (Dropdown)

```elisp
(vui-select :value priority
            :options '("Low" "Medium" "High")
            :prompt "Priority: "
            :on-change (lambda (v) (vui-set-state :priority v)))

;; Display/value pairs:
(vui-select :value code
            :options '(("United States" . "US")
                       ("Germany" . "DE")))
```

### Fragment (Invisible Grouping)

```elisp
(vui-fragment
 (vui-text "Line 1")
 (vui-newline)
 (vui-text "Line 2"))

;; Nil children are silently ignored:
(vui-fragment
 (when show-header (vui-text "Header"))
 (vui-text "Body"))
```

## Layout

### Horizontal - vui-hstack

```elisp
(vui-hstack
 (vui-text "One")
 (vui-text "Two")
 (vui-text "Three"))
;; Output: One Two Three

(vui-hstack :spacing 0 (vui-text "A") (vui-text "B"))
;; Output: AB

(vui-hstack :spacing 3 (vui-text "One") (vui-text "Two"))
;; Output: One   Two
```

### Vertical - vui-vstack

```elisp
(vui-vstack
 (vui-text "Line 1")
 (vui-text "Line 2"))

(vui-vstack :spacing 1    ; blank line between items
            (vui-text "Para 1")
            (vui-text "Para 2"))

(vui-vstack :indent 4     ; indent all children
            (vui-text "- Item 1")
            (vui-text "- Item 2"))

(vui-vstack :spacing 1 :indent 2   ; combine both
            (vui-text "- Task 1")
            (vui-text "- Task 2"))
```

### Fixed-Width Box - vui-box

```elisp
(vui-box (vui-text "Centered") :width 20 :align :center)
(vui-box (vui-text "Right") :width 20 :align :right)
(vui-box (vui-text "Padded") :width 20 :padding-left 2 :padding-right 2)
```

### Table - vui-table

```elisp
;; Basic table
(vui-table
 :columns '((:min-width 8) (:min-width 10) (:min-width 8))
 :rows '(("Alice" "Developer" "NYC")
         ("Bob" "Designer" "LA")))

;; With headers and borders
(vui-table
 :columns '((:header "Name" :width 8)
            (:header "Role" :width 10)
            (:header "Location" :width 10))
 :rows '(("Alice" "Developer" "NYC"))
 :border :ascii)   ; or :unicode

;; Column alignment
(vui-table
 :columns '((:header "ID" :width 5 :align :right)
            (:header "Product" :width 12 :align :left)
            (:header "Price" :width 8 :align :right))
 :rows '(("1" "Widget" "$9.99")))
```

**Column properties:**
- `:header` - Column header text
- `:width` - Target width for content
- `:min-width` - Minimum width, expands as needed
- `:grow` - If t, pad short content and expand for long
- `:truncate` - If t, truncate with "..."
- `:align` - `:left`, `:center`, or `:right`

**Width behavior:**

| :width | :grow | :truncate | Content vs Width | Result |
|--------|-------|-----------|------------------|--------|
| W | nil | nil | content < W | Shrinks to content |
| W | nil | nil | content > W | Overflow with broken bar |
| W | t | nil | content < W | Column = W, padded |
| W | t | nil | content > W | Expands to fit |
| W | t | t | content < W | Column = W, padded |
| W | t | t | content > W | Column = W, truncated |

**Interactive cells:**

```elisp
(vui-table
 :columns '((:header "Item" :width 12)
            (:header "Action" :width 10))
 :rows `(("Apples" ,(vui-button "[Edit]" :on-click edit-fn))
         ("Oranges" ,(vui-button "[Edit]" :on-click edit-fn))))
```

### Dynamic Lists - vui-list

```elisp
(vui-list '("Apple" "Banana" "Cherry")
          (lambda (item) (vui-text (format "- %s" item))))

;; With key function for efficient updates:
(vui-list todos
          (lambda (todo) (vui-component 'todo-item :todo todo))
          (lambda (todo) (plist-get todo :id)))

;; With indentation:
(vui-list items #'vui-text nil :indent 2)
```

## Hooks

### Side Effects - vui-use-effect

```elisp
;; Run only once (on mount):
(vui-use-effect ()
  (message "Mounted"))

;; Run when dependency changes:
(vui-use-effect (user-id)
  (fetch-user user-id callback))

;; With cleanup (returned lambda runs on unmount or before next effect):
(vui-use-effect ()
  (let ((timer (run-with-timer 1 1
                 (vui-with-async-context
                   (vui-set-state :elapsed #'1+)))))
    (lambda () (cancel-timer timer))))
```

### Async Data Loading - vui-use-async

```elisp
(vui-defcomponent user-data (user-id)
  :render
  (let ((result (vui-use-async user-id
                  (lambda (resolve _reject)
                    (funcall resolve (fetch-user user-id))))))
    (pcase (plist-get result :status)
      ('pending (vui-text "Loading..."))
      ('error (vui-text (format "Error: %s" (plist-get result :error))))
      ('ready (vui-text (plist-get result :data))))))
```

**Return value plist:**
- `:status` - `pending`, `ready`, or `error`
- `:data` - Loaded data (when ready)
- `:error` - Error message (when error)

**Key-based caching:** Same key = cached result. New key = new load.

**Truly non-blocking with make-process:**

```elisp
(vui-use-async 'balance
  (lambda (resolve reject)
    (make-process
     :name "cmd"
     :command '("hledger" "balance" "-O" "csv")
     :buffer (generate-new-buffer " *async*")
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (eq 0 (process-exit-status proc))
             (funcall resolve (with-current-buffer (process-buffer proc)
                                (buffer-string)))
           (funcall reject "Command failed")))))))
```

**Multiple async calls:**

```elisp
(let ((users (vui-use-async 'users loader1))
      (stats (vui-use-async 'stats loader2)))
  (if (seq-some (lambda (r) (eq (plist-get r :status) 'pending))
                (list users stats))
      (vui-text "Loading...")
    (render-dashboard (plist-get users :data)
                      (plist-get stats :data))))
```

### Mutable References - vui-use-ref

Persists across renders without causing re-renders:

```elisp
(let ((render-count (vui-use-ref 0)))
  (setcar render-count (1+ (car render-count)))
  (vui-text (format "Rendered %d times" (car render-count))))
```

### Memoization - vui-use-memo

```elisp
(let ((filtered (vui-use-memo (items threshold)
                  (seq-filter (lambda (x) (> x threshold)) items))))
  (vui-list filtered render-fn))
```

### Stable Callbacks - vui-use-callback

```elisp
(let ((handle-click (vui-use-callback (count)
                      (lambda ()
                        (vui-set-state :count (1+ count))))))
  (vui-component 'child :on-click handle-click))
```

## Async Context

**CRITICAL:** When calling `vui-set-state` from timers, process sentinels,
or hooks (anything outside render), you MUST restore component context.

### vui-with-async-context (no arguments)

```elisp
;; For timers, hooks — callback receives no data
(run-with-timer 1 1
  (vui-with-async-context
    (vui-set-state :seconds #'1+)))  ; Use functional update!
```

### vui-async-callback (with arguments)

```elisp
;; For process sentinels, API callbacks — callback receives data
(make-process
 :sentinel
 (vui-async-callback (proc event)
   (vui-set-state :output (process-output proc))))
```

**When NOT needed:**
- Widget callbacks (buttons, fields) - context automatic
- `vui-use-async` loaders - resolve/reject handle it
- Code directly in `:render` - already in context

## Context (Avoid Prop Drilling)

```elisp
;; Define context with default:
(vui-defcontext theme 'light "Current UI theme")

;; Provide to subtree:
(vui-component 'theme-provider :value 'dark
               (vui-component 'my-app))

;; Consume in any descendant:
(vui-defcomponent themed-button (label)
  :render
  (let ((current-theme (use-theme)))  ; Auto-generated hook
    (vui-button label
                :face (if (eq current-theme 'dark)
                          '(:background "gray20")
                        '(:background "white")))))
```

## Lifecycle Hooks

```elisp
(vui-defcomponent editor (file-path)
  :state ((content "") (saved t))

  :on-mount
  (progn
    (vui-set-state :content (read-file file-path))
    ;; Return cleanup function (runs on unmount):
    (lambda () (cleanup-resources)))

  :on-update
  ;; Has access to prev-props, prev-state, props, state
  (when (not (equal (plist-get prev-props :file-path)
                    (plist-get props :file-path)))
    (vui-set-state :content (read-file file-path)))

  :on-unmount
  (save-if-needed)

  :render ...)
```

**Execution order:** Children mount/unmount before parents (bottom-up).

## Error Handling

### Error Boundaries

```elisp
(vui-error-boundary
 :id "main-content"
 :fallback (lambda (err)
             (vui-text (format "Something broke: %s" err)))
 :on-error (lambda (err) (log-error err))
 :children
 (vui-component 'potentially-failing-component))
```

### Configuration

```elisp
;; For lifecycle errors:
(setq vui-lifecycle-error-handler 'warn)   ; 'ignore, 'signal, 'warn, or function

;; For event handler errors:
(setq vui-event-error-handler 'warn)

;; Inspect last error:
vui-last-error  ; => (TYPE ERROR CONTEXT)

;; Reset error boundary to retry:
(vui-reset-error-boundary "main-content")
```

## Performance

### Batching State Updates

```elisp
(vui-batch
  (vui-set-state :name "Alice")
  (vui-set-state :email "alice@example.com")
  (vui-set-state :role "admin"))
;; Single re-render instead of three
```

Event handlers are automatically batched.

### Skip Re-renders

```elisp
(vui-defcomponent optimized (data)
  :should-update
  (not (equal (plist-get props :data)
              (plist-get prev-props :data)))
  :render ...)
```

### Keys for Lists

Always provide stable keys for dynamic lists:

```elisp
(cl-loop for todo in todos
         collect (vui-component 'todo-item
                                :key (plist-get todo :id)
                                :todo todo))
```

### Render Timing

```elisp
(setq vui-render-delay 0.05)    ; Debounce renders
(vui-flush-sync)                ; Force immediate render
```

### Profiling

```elisp
(setq vui-timing-enabled t)
(vui-report-timing)             ; Show performance breakdown
```

## Dev Tools

```elisp
(vui-inspect)                   ; Component tree with props/state
(vui-inspect-state)             ; Only stateful components
(vui-get-instance-by-id ID)     ; Find specific instance
(vui-get-component-instances 'my-comp)  ; All instances of type

;; Debug logging:
(setq vui-debug-enabled t)
(setq vui-debug-log-phases '(render mount update))
```

## Hook Rules

1. **Call hooks at top level** of render - never inside conditionals
2. **Same order every render** - hooks are identified by call order
3. **Complete dependencies** - list ALL values used in effect/callback

```elisp
;; GOOD:
(let ((ref (vui-use-ref nil))
      (memo (vui-use-memo (data) (sort data))))
  ...)

;; BAD - hook inside conditional:
(when condition
  (let ((ref (vui-use-ref nil)))  ; DON'T!
    ...))
```

## Complete Example: Status Dashboard

```elisp
(vui-defcomponent status-dashboard ()
  :state ((expanded-rigs (make-hash-table :test 'equal)))
  :render
  (let ((result (vui-use-async 'status
                  (lambda (resolve _reject)
                    (funcall resolve
                             (json-parse-string
                              (shell-command-to-string
                               "gt status --json")))))))
    (pcase (plist-get result :status)
      ('pending
       (vui-text "Loading status..."))
      ('error
       (vui-text (format "Error: %s" (plist-get result :error))
                 :face 'error))
      ('ready
       (let ((data (plist-get result :data)))
         (vui-vstack :spacing 1
           (vui-text "Gas Town Status" :face 'bold)
           (vui-newline)
           (vui-list (plist-get data :rigs)
                     (lambda (rig)
                       (let* ((name (plist-get rig :name))
                              (expanded (gethash name expanded-rigs)))
                         (vui-fragment
                          (vui-button (format "%s %s"
                                             (if expanded "v" ">")
                                             name)
                                     :no-decoration t
                                     :face 'bold
                                     :on-click
                                     (lambda ()
                                       (puthash name (not expanded)
                                                expanded-rigs)
                                       (vui-set-state :expanded-rigs
                                                      expanded-rigs)))
                          (vui-newline)
                          (when expanded
                            (vui-vstack :indent 2
                              (vui-list (plist-get rig :agents)
                                        (lambda (agent)
                                          (vui-hstack
                                           (vui-text
                                            (if (plist-get agent :running)
                                                "* " "  ")
                                            :face (if (plist-get agent :running)
                                                      'success 'shadow))
                                           (vui-text
                                            (plist-get agent :name))))))))))
                     (lambda (rig)
                       (plist-get rig :name)))))))))

(vui-mount (vui-component 'status-dashboard) "*gt-status*")
```

## Quick Reference

| Function | Purpose |
|----------|---------|
| `vui-defcomponent` | Define component type |
| `vui-component` | Create component vnode |
| `vui-mount` | Render to buffer |
| `vui-set-state` | Update state (triggers re-render) |
| `vui-text` | Text display |
| `vui-button` | Clickable button |
| `vui-field` | Text input |
| `vui-checkbox` | Toggle checkbox |
| `vui-select` | Dropdown selection |
| `vui-fragment` | Invisible grouping |
| `vui-newline` | Line break |
| `vui-space` | Horizontal space |
| `vui-hstack` | Horizontal layout |
| `vui-vstack` | Vertical layout |
| `vui-box` | Fixed-width container |
| `vui-table` | Tabular data |
| `vui-list` | Dynamic list rendering |
| `vui-use-effect` | Side effects |
| `vui-use-async` | Async data loading |
| `vui-use-ref` | Mutable reference |
| `vui-use-memo` | Memoized computation |
| `vui-use-callback` | Stable callback |
| `vui-with-async-context` | Async context (no args) |
| `vui-async-callback` | Async context (with args) |
| `vui-defcontext` | Define context |
| `vui-batch` | Batch state updates |
| `vui-error-boundary` | Error boundary |
| `vui-inspect` | Dev tools inspector |

## Reference

- Source: https://github.com/d12frosted/vui.el
- Guides: https://github.com/d12frosted/vui.el/tree/master/docs/guide
- Real-world usage: gastown-status-buffer.el in gastown.el
