---
name: forge
description: Expert guide for Forge (magit-forge) architecture, database, topic/pullreq/issue types, notification system, and API patterns. Use when implementing or debugging forge database queries, GitHub/GitLab integration, pull request UI, or forge topic/issue commands.
---

# Forge Expert

This skill provides comprehensive guidance for working with Forge — the
Magit extension for GitHub, GitLab, Gitea, and other Git forges — based
on Forge source code and real-world usage patterns.

## When to Use This Skill

Invoke this skill when:
- Working with `forge-topic`, `forge-issue`, `forge-pullreq` objects
- Querying or mutating the Forge closql database
- Implementing PR/issue UI (`forge-topics-mode`, `forge-topic-mode`)
- Working with `forge-notification` and notifications display
- Adding forge sections to the Magit status buffer
- Implementing API interactions (`forge-get-repository`, `forge-fetch`)
- Building post/comment editing with `forge-post-mode`
- Handling multi-forge backends (GitHub, GitLab, Gitea, etc.)

## Core Architecture

Forge extends Magit with a local SQLite database (via closql + emacsql)
that mirrors forge data for offline use and fast queries.

```
forge-object (abstract, closql-object)
├── forge-repository (abstract)
│   ├── forge-github-repository
│   ├── forge-gitlab-repository
│   ├── forge-gitea-repository
│   ├── forge-forgejo-repository
│   └── forge-{gogs,bitbucket,semi}-repository
├── forge-post (abstract)         ← Body text, author, timestamps
│   ├── forge-issue-post          ← Issue comment
│   └── forge-pullreq-post        ← PR comment/review
├── forge-topic (abstract, extends forge-post)
│   ├── forge-issue
│   └── forge-pullreq
└── forge-notification
```

Key files: `forge-db.el`, `forge-core.el`, `forge-topic.el`,
`forge-issue.el`, `forge-pullreq.el`, `forge-notify.el`,
`forge-commands.el`, `forge-post.el`

## Database Architecture (forge-db.el)

### Database Connection

```elisp
;; Get database connection
(forge-db)               ; Current connection
(forge-db t)             ; Force live connection (no cache)

;; Execute raw SQL
(forge-sql [:select [id title] :from issue :where (= state 'open)])
(forge-sql-cdr [:select [id] :from issue])  ; Returns column values only
```

### Database Location

```elisp
;; Default: ~/.emacs.d/forge-database.sqlite
(setq forge-database-file "~/.emacs.d/forge-database.sqlite")
```

### Schema Tables

| Table | Purpose |
|-------|---------|
| `repository` | Git forge repos (owner, name, apihost, githost) |
| `issue` | Issues (number, state, author, title, body, ...) |
| `pullreq` | PRs (+ head-ref, base-ref, merged, draft-p, ...) |
| `issue-post` | Issue comments |
| `pullreq-post` | PR comments/reviews |
| `assignee` | User assignments |
| `label` | Labels/tags |
| `mark` | Custom topic markers |
| `milestone` | Release milestones |
| `notification` | Notifications (thread-id, type, topic) |
| `fork` | Repository forks |
| `revnote` | Inline code review notes |

### closql ORM Operations

```elisp
;; Fetch by primary key
(closql-get (forge-db) id 'forge-issue)
(closql-get (forge-db) id 'forge-pullreq)

;; Insert/update/delete
(closql-insert (forge-db) obj)
(closql-update (forge-db) obj)
(closql-delete (forge-db) obj)

;; Transactions
(closql-with-transaction (forge-db)
  (closql-insert (forge-db) issue)
  (closql-insert (forge-db) post))

;; Slot access (standard EIEIO)
(oref issue title)
(oref pullreq draft-p)
(oset issue status 'done)

;; Lazy-loaded relationships
(closql-dref issue 'assignees)   ; Load assignees on demand
(closql-dref issue 'labels)      ; Load labels on demand
(closql-dref issue 'posts)       ; Load comments on demand
```

## Topic Types

### forge-issue

Key slots:
- `number` — Issue number (int, unique per repo)
- `state` — `open`, `completed`, `unplanned`
- `status` — `unread`, `pending`, `done`
- `author` — Username string
- `title` — Issue title
- `body` — Markdown body text
- `created`, `updated`, `closed` — Timestamps
- `milestone` — Milestone title
- `saved-p` — Bookmarked/saved
- `locked-p` — Editing locked

Relations (lazy-loaded via `closql-dref`):
- `assignees` — List of assignee usernames
- `labels` — List of label names
- `marks` — Custom user markers
- `posts` — Comments (list of `forge-issue-post`)
- `reactions` — Emoji reactions

State transitions:
- `open` → `completed` (closed as fixed/resolved)
- `open` → `unplanned` (closed as won't-fix/not-planned)

### forge-pullreq

All `forge-issue` slots plus:
- `base-ref` — Target branch (e.g., `main`)
- `head-ref` — Source branch
- `base-repo` — Target repo (for cross-repo PRs)
- `head-repo` — Source repo
- `merged` — Merge commit hash (nil if not merged)
- `draft-p` — Draft PR flag
- `editable-p` — Whether current user can edit
- `cross-repo-p` — Fork PR flag

State values:
- `open` — Active PR
- `merged` (stored as `completed`) — Merged PR
- `rejected` — Closed without merging

### forge-post / forge-issue-post / forge-pullreq-post

- `number` — Comment number
- `author` — Username
- `body` — Markdown body
- `created`, `updated` — Timestamps

## Repository Detection

```elisp
;; Get repository object for current Git repo
;; `demand` controls behavior when repo isn't in database:
(forge-get-repository :tracked?)  ; nil if not tracked
(forge-get-repository :known?)    ; nil if completely unknown
(forge-get-repository :insert!)   ; Insert into DB if needed
(forge-get-repository :stub?)     ; Derive from git config only
(forge-get-repository :valid?)    ; Verify via API (no store)

;; Explicit lookup
(forge-get-repository '("github.com" "owner/repo"))

;; From URL
(forge--split-forge-url "https://github.com/owner/repo")
;; → ("github.com" "owner" "repo")
```

### forge-alist Configuration

```elisp
;; Default entries (GITHOST APIHOST WEBHOST CLASS):
("github.com" "api.github.com" "github.com" forge-github-repository)
("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
("codeberg.org" "codeberg.org/api/v1" "codeberg.org" forge-forgejo-repository)

;; Add self-hosted instance:
(add-to-list 'forge-alist
             '("git.company.com"
               "git.company.com/api/v4"
               "git.company.com"
               forge-gitlab-repository))
```

## Topic Retrieval

Generic functions dispatch on argument type:

```elisp
;; Get topic by object
(forge-get-topic topic)

;; Get issue or PR by repo + number
(forge-get-topic repo 42)

;; Get by number alone (checks both issue and PR tables)
(forge-get-topic 42)

;; Specific lookups
(forge-get-issue repo 42)
(forge-get-pullreq repo 17)

;; At point (in topic/list buffers)
(forge-topic-at-point)
(forge-issue-at-point)
(forge-pullreq-at-point)
```

## Filter Specs (forge--topics-spec)

```elisp
;; Topic filter object
(forge--topics-spec
 :type 'topic        ; 'topic (both), 'issue, 'pullreq
 :active t           ; Only non-done/non-saved
 :state 'open        ; 'open, 'completed, 'merged, nil (any)
 :status 'unread     ; 'unread, 'pending, 'done, nil (any)
 :order 'newest      ; 'newest, 'oldest, 'recently-updated
 :limit 20)          ; Max items to fetch

;; Customize defaults
(setq forge-list-buffer-default-topic-filters
      (forge--topics-spec :state 'open :limit 30))
(setq forge-status-buffer-default-topic-filters
      (forge--topics-spec :type 'issue :state 'open :limit 5))
```

## Listing Topics

```elisp
;; Database queries
(forge--ls-topics repo)              ; All topics for repo
(forge--ls-issues repo)              ; Issues only
(forge--ls-pullreqs repo)            ; PRs only

;; With filters
(forge--ls-topics repo spec)         ; Use forge--topics-spec

;; Buffer-level (interactive)
(forge-list-issues)
(forge-list-pullreqs)
(forge-list-topics)
(forge-list-notifications)
```

## UI Modes

### forge-topics-mode (List View)

Derived from `magit-mode`. Buffer: `*forge-topics*`

Key bindings:
- `RET` — Visit topic in detail view
- `o` — Browse topic in web browser
- `g` — Refresh buffer
- `'` — Forge dispatch menu

### forge-topic-mode (Detail View)

Displays full topic with title, metadata, body, and comments.
Uses Magit sections for navigation.

Buffer-local variables:
- `forge-buffer-repository` — Current repo object
- `forge-buffer-topic` — Current topic object
- `forge--buffer-topics-spec` — Filter spec for list buffers

### forge-post-mode (Editing)

Derived from `gfm-mode` (GitHub Flavored Markdown).

Key bindings:
- `C-c C-c` — Submit post
- `C-c C-k` — Cancel/discard
- `C-c C-e` — Forge post dispatch menu

```elisp
;; Prepare a post editing buffer
(forge--prepare-post-buffer "filename.md" "Optional header")
;; Creates at: ~/.emacs.d/magit/posts/filename.md
;; Resumes existing draft if file exists

;; Buffer-local state in post buffers
forge--buffer-post-object      ; The forge-post being edited
forge--submit-post-function    ; Called on C-c C-c
forge--cancel-post-function    ; Called on C-c C-k
```

## Notifications (forge-notify.el)

### forge-notification Slots

- `thread-id` — Unique notification thread ID
- `repository` — Repo this notification belongs to
- `type` — `Issue`, `PullRequest`, `Commit`, etc.
- `topic` — Topic number (links to issue/PR)
- `url` — Notification URL
- `title` — Notification title
- `reason` — Why notified: `subscribed`, `mentioned`, `assign`, etc.
- `last-read` — Timestamp of last read
- `updated` — Last activity timestamp

### Notification Queries

```elisp
;; List by status
(forge--ls-notifications 'unread)    ; New, unseen
(forge--ls-notifications 'pending)   ; Seen, not resolved
(forge--ls-notifications 'done)      ; Resolved
(forge--ls-notifications 'saved)     ; Bookmarked

;; Get notification for topic
(forge-get-notification topic)

;; At point
(forge-notification-at-point)
```

### Notification Status Transitions

- `unread` → New notification
- `pending` → Seen but needs action
- `done` → Resolved/dismissed
- `saved` → Explicitly bookmarked (independent of read state)

### forge-notifications-mode

Buffer: `*forge-notifications*`

Display style:
```elisp
(setq forge-notifications-display-style 'flat)    ; All in one list
(setq forge-notifications-display-style 'nested)  ; Grouped by repo/type
```

## Magit Status Integration

```elisp
;; Forge adds these to magit-status-sections-hook:
(forge-insert-pullreqs)   ; "Pull requests" section
(forge-insert-issues)     ; "Issues" section

;; Add forge sections to status (after loading forge):
(with-eval-after-load 'forge
  (magit-add-section-hook 'magit-status-sections-hook
                          #'forge-insert-pullreqs
                          #'magit-insert-unpushed-to-pushremote
                          t))

;; Keybindings added to magit-mode-map:
;; "'"  → forge-dispatch
;; "N"  → forge-dispatch  (alternative)
;; Also augments: magit-fetch, magit-pull, magit-branch,
;;                magit-worktree, magit-merge
```

## Key Commands

### Creating Topics

```elisp
(forge-create-issue)            ; Open editor, create via API
(forge-create-pullreq)          ; Create PR from current branch
(forge-create-pullreq-from-issue issue)  ; Convert issue to PR
```

### Visiting Topics

```elisp
(forge-visit-topic)             ; Visit topic at point
(forge-visit-issue)             ; Visit specific issue
(forge-visit-pullreq)           ; Visit specific PR
(forge-browse-topic)            ; Open in web browser
(forge-browse-pullreq)          ; Open PR in browser
(forge-browse-issue)            ; Open issue in browser
```

### PR Workflow

```elisp
;; Branch management
(forge-checkout-pullreq)        ; Checkout PR branch locally
(forge-branch-pullreq)          ; Create local branch from PR
(forge-checkout-worktree)       ; Create git worktree for PR

;; Review
(forge-merge)                   ; Merge PR via API

;; Editing
(forge-edit-topic-title)        ; Edit title
(forge-edit-topic-body)         ; Edit body in post-mode
(forge-edit-topic-labels)       ; Add/remove labels
(forge-edit-topic-assignees)    ; Add/remove assignees
(forge-edit-topic-milestone)    ; Set milestone
(forge-toggle-topic-state)      ; Open ↔ Closed
```

### Notification Commands

```elisp
(forge-list-notifications)              ; List all notifications
(forge-mark-notification-as-read)       ; Mark at-point as read
(forge-toggle-notification-saved)       ; Toggle saved status
```

## Transient Dispatch Menu

```elisp
;; Main forge dispatch (bound to "'" in magit-mode)
(transient-define-prefix forge-dispatch ()
  [["Create"
    ("c i" "issue"        forge-create-issue)
    ("c p" "pull-request" forge-create-pullreq)]
   ["Visit"
    ("v i" "issue"        forge-visit-issue)
    ("v p" "pull-request" forge-visit-pullreq)]
   ["Browse"
    ("b i" "issue"        forge-browse-issue)
    ("b p" "pull-request" forge-browse-pullreq)]])

;; Augmenting forge-dispatch with your own commands:
(transient-append-suffix 'forge-dispatch "v p"
  '("v m" "my-thing" my-forge-command))
```

## Backend Implementations

Each forge backend implements the same generic functions:

```elisp
;; URL format slots per class (forge-github-repository):
issues-url-format        ; → "https://github.com/%s/%s/issues"
issue-url-format         ; → "https://github.com/%s/%s/issues/%s"
pullreq-url-format       ; → "https://github.com/%s/%s/pull/%s"
create-issue-url-format  ; → "https://github.com/%s/%s/issues/new"
create-pullreq-url-format ; → "https://github.com/%s/%s/compare/%s"
```

### Authentication

```elisp
;; Tokens stored via auth-source (e.g., ~/.authinfo.gpg):
;; machine api.github.com login USERNAME^forge password TOKEN
;; machine gitlab.com/api/v4 login USERNAME^forge password TOKEN

;; Check auth setup
(forge-auth-source-search "api.github.com" "username^forge")
```

## Common Patterns

### Context-Aware Access

```elisp
(defun my-forge-command ()
  "Act on topic at point."
  (interactive)
  (if-let ((topic (forge-topic-at-point)))
      (pcase (eieio-object-class topic)
        ('forge-issue   (my-handle-issue topic))
        ('forge-pullreq (my-handle-pullreq topic)))
    (user-error "No topic at point")))
```

### Working with Topics

```elisp
(defun my-show-topic-info (topic)
  "Display TOPIC information."
  (let* ((repo (forge-get-repository :tracked?))
         (issue (forge-get-issue repo (oref topic number)))
         (title  (oref issue title))
         (state  (oref issue state))
         (labels (closql-dref issue 'labels))
         (posts  (closql-dref issue 'posts)))
    (message "#%d [%s] %s (%d comments)"
             (oref issue number)
             state title
             (length posts))))
```

### Inserting a Forge Section

```elisp
(defun my-forge-insert-section ()
  "Insert custom forge section into Magit status."
  (when-let ((repo (ignore-errors (forge-get-repository :tracked?))))
    (let ((topics (forge--ls-issues
                   repo
                   (forge--topics-spec :state 'open :limit 3))))
      (when topics
        (magit-insert-section (my-forge-section)
          (magit-insert-heading "My Issues")
          (dolist (topic topics)
            (magit-insert-section (my-topic (oref topic number))
              (insert
               (format "  #%-5d %s\n"
                       (oref topic number)
                       (oref topic title))))))))))
```

### Fetching from API

```elisp
;; Fetch all data for current repo
(forge-fetch)              ; Pull latest from forge API

;; Fetch specific resource
(forge-pull)               ; Fetch and refresh display
```

## Important Notes

1. **Database is local**: Forge stores a local copy — always `forge-fetch`
   before assuming data is current
2. **Lazy loading**: Relations (assignees, labels, posts) load on first
   `closql-dref` access — this may be slow for bulk operations
3. **Generic functions**: `forge-get-topic`, `forge-get-repository` etc.
   dispatch on argument type — check cl-defgeneric signatures
4. **Buffer context**: Always check `forge-buffer-repository` before
   assuming which repo you're in
5. **Draft PRs**: Check `draft-p` slot — affects display and API submission
6. **Cross-repo PRs**: Check `cross-repo-p` and `head-repo` for fork PRs

## Common Gotchas

1. **Repository not tracked**: `forge-get-repository :tracked?` returns nil
   if repo not in database — use `:insert!` to add it
2. **Missing auth**: Token not configured → API calls fail silently or
   prompt; check `~/.authinfo.gpg`
3. **Stale data**: Forge data is a local cache — must `forge-fetch` to
   get current state
4. **Nil closql-dref**: Lazy relations return nil if not yet fetched;
   check `forge-fetch` was called
5. **State vs status**: `state` = open/closed/merged (issue state);
   `status` = unread/pending/done (notification/read state)
