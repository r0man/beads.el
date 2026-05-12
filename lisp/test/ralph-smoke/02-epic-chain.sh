#!/usr/bin/env bash
# 02-epic-chain.sh
#
# Live smoke: 3-child epic with deps in chain order; verify each iter
# renders with a summary populated from bd notes; bury the dashboard;
# mode-line updates with Ralph N/M $X.XX format; notification fires
# on success per default 'on-stop policy.
#
# This script is a template -- the actual loop drive logic is
# scenario-specific.  Adapt the canned issue setup below to match.

set -u
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$(dirname "$0")/../../.." && pwd)}"
TMPDIR=$(mktemp -d /tmp/ralph-smoke-epic-XXXX)
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

# Create epic and 3 chained children.
EPIC=$(bd create --title "Smoke epic" --type epic --priority 2 --json | sed -n 's/.*"id":"\([^"]*\)".*/\1/p' | head -1)
echo "Created epic: $EPIC"

A=$(bd create --title "Add line A" --description "Append 'A' to README.md" --parent "$EPIC" --type task --priority 2 --json | sed -n 's/.*"id":"\([^"]*\)".*/\1/p' | head -1)
B=$(bd create --title "Add line B" --description "Append 'B' to README.md (after A)" --parent "$EPIC" --type task --priority 2 --json | sed -n 's/.*"id":"\([^"]*\)".*/\1/p' | head -1)
C=$(bd create --title "Add line C" --description "Append 'C' to README.md (after B)" --parent "$EPIC" --type task --priority 2 --json | sed -n 's/.*"id":"\([^"]*\)".*/\1/p' | head -1)
bd dep add "$B" "$A"
bd dep add "$C" "$B"

emacs -batch \
    -L "$PROJECT_ROOT/lisp" \
    -l beads-agent-ralph \
    -l beads-agent-ralph-dashboard \
    --eval "(let ((default-directory \"$TMPDIR/\")
                   (beads-agent-use-worktrees nil))
              (beads-agent-ralph-start
                :issue \"$EPIC\"
                :kind 'epic
                :project-dir \"$TMPDIR/\"
                :max-iterations 6
                :iteration-delay 1))" \
    2>&1 | tail -30

echo
echo "Post: bd list --parent $EPIC"
bd list --parent "$EPIC"
echo
echo "JSONL log:"
wc -l ".beads/scratch/ralph/$EPIC.jsonl" 2>/dev/null || echo "  (none)"
