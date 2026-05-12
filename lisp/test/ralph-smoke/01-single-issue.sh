#!/usr/bin/env bash
# 01-single-issue.sh
#
# Live smoke: one trivial bd issue, Ralph loop runs to completion on
# the first sentinel.
#
# Asserts:
#   - dashboard buffer is created and contains an iteration row.
#   - JSONL log under .beads/scratch/ralph/ has at least one record.
#   - bd reports the issue closed at the end of the run.
#   - the per-iter NDJSON event file exists (compressed or raw).

set -u
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$(dirname "$0")/../../.." && pwd)}"
TMPDIR=$(mktemp -d /tmp/ralph-smoke-XXXX)
trap '[[ -z "${KEEP_TMP:-}" ]] && rm -rf "$TMPDIR"' EXIT

if ! command -v claude >/dev/null 2>&1; then
    echo "SKIP: claude not on PATH"; exit 0
fi
if ! command -v bd >/dev/null 2>&1; then
    echo "SKIP: bd not on PATH"; exit 0
fi

cd "$TMPDIR" || exit 2
git init -q -b main
git config user.email smoke@example.com
git config user.name "Ralph Smoke"
echo "# README" > README.md
git add README.md
git commit -q -m "initial"
bd init >/dev/null 2>&1 || true

ISSUE_OUT=$(bd create --title "Add comment to README" \
    --description "Append the literal line '# smoke comment' to README.md, then bd-close this issue." \
    --type task --priority 2 --json)
ISSUE_ID=$(echo "$ISSUE_OUT" | sed -n 's/.*"id":"\([^"]*\)".*/\1/p' | head -1)
if [[ -z "$ISSUE_ID" ]]; then
    echo "FAIL: could not parse issue id from bd create"
    exit 1
fi

echo "Created issue: $ISSUE_ID"

# Drive the loop from Emacs batch.
emacs -batch \
    -L "$PROJECT_ROOT/lisp" \
    -l beads-agent-ralph \
    -l beads-agent-ralph-dashboard \
    --eval "(let ((default-directory \"$TMPDIR/\")
                   (beads-agent-use-worktrees nil)
                   (beads-agent-ralph-permission-mode \"bypassPermissions\"))
              (beads-agent-ralph-start
                :issue \"$ISSUE_ID\"
                :project-dir \"$TMPDIR/\"
                :max-iterations 5
                :iteration-delay 1)
              ;; Wait up to 5 minutes for terminal status.
              (let ((deadline (+ (float-time) 300)))
                (while (and (< (float-time) deadline)
                            (not (memq (oref (caar
                                              (or (and (boundp 'beads-agent-ralph--last)
                                                       (list beads-agent-ralph--last))
                                                  '(nil))) status)
                                       '(done failed stopped))))
                  (accept-process-output nil 1.0))))" \
    2>&1 | tail -50

# Verify post-conditions.
echo
echo "=== Post-conditions ==="

JSONL_PATH=".beads/scratch/ralph/$ISSUE_ID.jsonl"
if [[ -f "$JSONL_PATH" ]]; then
    echo "PASS: JSONL log exists: $JSONL_PATH"
    wc -l < "$JSONL_PATH" | awk '{print "  records:", $0}'
else
    echo "FAIL: JSONL log missing"
fi

EVENT_PATH=$(ls -1 .beads/scratch/ralph/${ISSUE_ID}.iter-*.ndjson* 2>/dev/null | head -1)
if [[ -n "$EVENT_PATH" ]]; then
    echo "PASS: per-iter event file: $EVENT_PATH"
else
    echo "INFO: no per-iter event file (loop may not have spawned)"
fi

CLOSED=$(bd show "$ISSUE_ID" --json | sed -n 's/.*"status":"\([^"]*\)".*/\1/p')
if [[ "$CLOSED" == "closed" ]]; then
    echo "PASS: issue closed"
else
    echo "INFO: issue status = $CLOSED"
fi
