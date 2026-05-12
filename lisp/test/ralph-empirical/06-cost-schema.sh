#!/usr/bin/env bash
# 06-cost-schema.sh
#
# Question: What are the actual field names on the `result` event for
# cost and duration?
#
# The plan currently writes cost-usd and duration-ms on the stream
# slots, taken from the (presumed) result event keys. The empirical
# truth (per Claude Code CLI docs and observed runs) appears to be
# total_cost_usd and duration_ms. This harness confirms.

set -u
OUT=$(mktemp /tmp/ralph-empirical-06.XXXXXX.ndjson)
trap 'rm -f "$OUT"' EXIT

if ! command -v claude >/dev/null 2>&1; then
    echo "FAIL: claude not on PATH"; exit 2
fi
if ! command -v jq >/dev/null 2>&1; then
    echo "FAIL: jq not on PATH"; exit 2
fi

claude --print --output-format stream-json --verbose \
       --no-session-persistence \
       --permission-mode bypassPermissions \
       --max-turns 1 \
       -- "Say hi in one word." \
       > "$OUT" 2>/dev/null
EXIT=$?

echo "exit=$EXIT"
RESULT=$(jq -c 'select(.type == "result")' < "$OUT" | head -n 1)

if [ -z "$RESULT" ]; then
    echo "FAIL: no result event observed; rerun harnesses 1/2/5 to triage"
    cp "$OUT" "${OUT%.ndjson}.kept.ndjson"
    echo "kept: ${OUT%.ndjson}.kept.ndjson"
    exit 1
fi

echo "result event keys:"
echo "$RESULT" | jq -r 'keys[]' | sort
echo
echo "Likely cost / duration fields (regex):"
echo "$RESULT" | jq -r 'to_entries[] | select(.key | test("cost|duration|turn|usage|token"; "i")) | "\(.key) = \(.value)"'

echo
echo "implication: update beads-agent-ralph--stream slot names to match"
echo "the fields above before bde-2qha (parser hardening) commits a"
echo "slot-mapping pass."

cp "$OUT" "${OUT%.ndjson}.kept.ndjson"
echo "kept: ${OUT%.ndjson}.kept.ndjson"
exit 0
