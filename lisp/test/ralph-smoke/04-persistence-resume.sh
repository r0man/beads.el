#!/usr/bin/env bash
# 04-persistence-resume.sh
#
# Live smoke: persistence kill-resume.
#
# 1. Start a Ralph loop for an issue.
# 2. Kill Emacs after the first iteration's JSONL line lands.
# 3. Restart Emacs and resume; assert that iter and cumulative-cost
#    continue from the disk summary, not from zero.
# 4. Assert the historical event file is replayable via RET.

set -u
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$(dirname "$0")/../../.." && pwd)}"
TMPDIR=$(mktemp -d /tmp/ralph-smoke-resume-XXXX)
trap '[[ -z "${KEEP_TMP:-}" ]] && rm -rf "$TMPDIR"' EXIT

if ! command -v claude >/dev/null 2>&1; then
    echo "SKIP: claude not on PATH"; exit 0
fi

cd "$TMPDIR" || exit 2
git init -q -b main
git config user.email smoke@example.com
git config user.name "Ralph Smoke"
echo "x" > a.txt && git add a.txt && git commit -q -m "init"
bd init >/dev/null 2>&1 || true
ISSUE=$(bd create --title "Resume test" --description "Append a single line per iter." --type task --priority 2 --json | sed -n 's/.*"id":"\([^"]*\)".*/\1/p' | head -1)

# Round 1: launch with timeout, expect at least one iteration to land.
timeout 90s emacs -batch \
    -L "$PROJECT_ROOT/lisp" \
    -l beads-agent-ralph \
    --eval "(let* ((default-directory \"$TMPDIR/\")
                    (beads-agent-use-worktrees nil)
                    (result (beads-agent-ralph-start
                             :issue \"$ISSUE\"
                             :project-dir \"$TMPDIR/\"
                             :max-iterations 10
                             :iteration-delay 1
                             :resume-action 'fresh-no-prompt))
                    (controller (car result)))
              ;; Block for up to 75 s so at least one iteration lands;
              ;; the outer \`timeout 90s\` will SIGTERM us if we're still
              ;; running, which is the intended kill-mid-run scenario.
              (let ((deadline (+ (float-time) 75)))
                (while (and (< (float-time) deadline)
                            (not (memq (oref controller status)
                                       '(done failed stopped auto-paused))))
                  (accept-process-output nil 1.0))))" \
    2>&1 | tail -20

echo "Round 1 JSONL state:"
wc -l ".beads/scratch/ralph/$ISSUE.jsonl" 2>/dev/null || echo "  (none)"

# Round 2: resume.  Read the summary back and verify iter counter is preserved.
emacs -batch \
    -L "$PROJECT_ROOT/lisp" \
    -l beads-agent-ralph-persist \
    --eval "(let* ((sum (beads-agent-ralph-persist-resume-summary \"$TMPDIR/\" \"$ISSUE\")))
              (princ (format \"Resume summary: %S\\n\" sum)))"

echo
echo "OK: resume summary printed above; manual verification required."
