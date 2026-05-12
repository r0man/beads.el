#!/usr/bin/env bash
# 04-bd-epic-blocker-payload.sh
#
# Question: Does `bd list --parent <epic-id> --status=open --json`
# include blocker (dependency) data inline, or does the controller
# need a second `bd dep list` call to compute the local "ready"
# predicate?
#
# NOTE: The Ralph plan referred to `--epic <id>`; the actual `bd list`
# flag is `--parent <id>`. This harness uses the correct flag and
# records the discrepancy in its output so the controller code path
# (`beads-agent-ralph.el` resolve-target step) uses the real flag.
#
# The controller's resolve-target step picks the next ready sub-issue
# of an epic. Doing it locally avoids racing with another `bd ready`
# caller; doing it with two RPCs adds latency and a race window.

set -u

if ! command -v bd >/dev/null 2>&1; then
    echo "FAIL: bd not on PATH"; exit 2
fi
if ! command -v jq >/dev/null 2>&1; then
    echo "FAIL: jq not on PATH"; exit 2
fi

# Pick the first open epic in the current beads workspace.
EPIC=$(bd list --type=epic --status=open --json 2>/dev/null \
       | jq -r '.[0].id // empty')

if [ -z "$EPIC" ]; then
    echo "FAIL: no open epic in this workspace to probe"
    echo "      run from a beads.el checkout with at least one open epic"
    exit 2
fi

echo "probing epic: $EPIC"
OUT=$(mktemp /tmp/ralph-empirical-04.XXXXXX.json)
trap 'rm -f "$OUT"' EXIT

bd list --parent "$EPIC" --status=open --json > "$OUT" 2>/dev/null
echo "rows: $(jq 'length' < "$OUT")"
echo "keys present on first row:"
jq -r '.[0] | keys[]' < "$OUT" | sort

# Look for any blocker-related field.
echo
echo "blocker-related keys (regex match):"
jq -r '.[0] | keys[] | select(test("block|dep|depends"; "i"))' < "$OUT" || true

# Heuristic: if any item carries a non-empty "dependencies" / "blockers"
# / "blocked_by" array, the local "ready" predicate is feasible.
if jq -e '.[] | (.dependencies // .blockers // .blocked_by) | length > 0' \
       < "$OUT" >/dev/null 2>&1; then
    echo "PASS: blocker data IS inline in --epic --json payload"
    echo "      local ready-predicate is feasible without a second call"
else
    echo "OBSERVED: no inline blocker arrays in --epic --json"
    echo "implication: controller needs a second 'bd ready --epic' call,"
    echo "             or must call 'bd dep list' per sub-issue."
fi

cp "$OUT" "${OUT%.json}.kept.json"
echo "kept: ${OUT%.json}.kept.json"
exit 0
