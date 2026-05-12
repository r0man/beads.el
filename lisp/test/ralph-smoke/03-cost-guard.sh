#!/usr/bin/env bash
# 03-cost-guard.sh
#
# Live smoke: per-iter budget = $0.01 and max-turns = 2.  Both paths
# must terminate cleanly: budget exhaustion emits a final `result`
# event and produces done(budget); the max-turns path exits non-zero
# but is treated as informational (status iter), not failed-iter.

set -u
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$(dirname "$0")/../../.." && pwd)}"
TMPDIR=$(mktemp -d /tmp/ralph-smoke-cost-XXXX)
trap '[[ -z "${KEEP_TMP:-}" ]] && rm -rf "$TMPDIR"' EXIT

if ! command -v claude >/dev/null 2>&1; then
    echo "SKIP: claude not on PATH"; exit 0
fi

cd "$TMPDIR" || exit 2
git init -q -b main
git config user.email smoke@example.com
git config user.name "Ralph Smoke"
echo "x" > file.txt
git add file.txt
git commit -q -m "init"
bd init >/dev/null 2>&1 || true
ISSUE=$(bd create --title "Big task" --description "Do something elaborate." --type task --priority 2 --json | sed -n 's/.*"id":"\([^"]*\)".*/\1/p' | head -1)

emacs -batch \
    -L "$PROJECT_ROOT/lisp" \
    -l beads-agent-ralph \
    -l beads-agent-ralph-dashboard \
    --eval "(let* ((default-directory \"$TMPDIR/\")
                    (beads-agent-use-worktrees nil)
                    (beads-agent-ralph-max-budget-usd-per-iter 0.01)
                    (beads-agent-ralph-max-turns 2)
                    (result (beads-agent-ralph-start
                             :issue \"$ISSUE\"
                             :project-dir \"$TMPDIR/\"
                             :max-iterations 2
                             :resume-action 'fresh-no-prompt))
                    (controller (car result)))
              ;; Wait up to 3 minutes for terminal status; budget should
              ;; trigger quickly.
              (let ((deadline (+ (float-time) 180)))
                (while (and (< (float-time) deadline)
                            (not (memq (oref controller status)
                                       '(done failed stopped auto-paused))))
                  (accept-process-output nil 1.0))))" \
    2>&1 | tail -20

echo
echo "JSONL log:"
cat ".beads/scratch/ralph/$ISSUE.jsonl" 2>/dev/null | sed -n '1,5p'
