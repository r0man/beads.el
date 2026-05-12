#!/usr/bin/env bash
# 02-max-turns-exit.sh
#
# Question: When --max-turns N is hit, what is the exit code, and is a
# final 'result' event emitted?
#
# The controller heuristic for distinguishing "agent ran out of turns"
# from "process crashed" is: non-zero exit + last-text present + no
# protocol error = informational, not failed. That heuristic is only
# safe if we know which exit code claude uses and whether a result
# event lands first.

set -u
OUT=$(mktemp /tmp/ralph-empirical-02.XXXXXX.ndjson)
trap 'rm -f "$OUT"' EXIT

if ! command -v claude >/dev/null 2>&1; then
    echo "FAIL: claude not on PATH"; exit 2
fi
if ! command -v jq >/dev/null 2>&1; then
    echo "FAIL: jq not on PATH"; exit 2
fi

claude --print --output-format stream-json --verbose \
       --include-partial-messages --no-session-persistence \
       --permission-mode bypassPermissions \
       --max-turns 1 \
       -- "Plan a 10-step solution then implement it." \
       > "$OUT" 2>/dev/null
EXIT=$?

echo "exit=$EXIT"
echo "ndjson lines: $(wc -l < "$OUT")"
echo "event types observed:"
jq -r 'select(.type) | .type' < "$OUT" 2>/dev/null | sort | uniq -c

if jq -e 'select(.type == "result")' < "$OUT" >/dev/null 2>&1; then
    echo "OBSERVED: --max-turns produced a 'result' event"
    echo "result event (subtype + cost fields):"
    jq 'select(.type == "result") | {subtype, num_turns, total_cost_usd, duration_ms, duration_api_ms, is_error}' \
       < "$OUT"
else
    echo "OBSERVED: --max-turns did NOT emit a 'result' event"
fi

cp "$OUT" "${OUT%.ndjson}.kept.ndjson"
echo "kept: ${OUT%.ndjson}.kept.ndjson"
echo "implication: controller maps exit=$EXIT + last-text + no protocol error -> informational"
exit 0
