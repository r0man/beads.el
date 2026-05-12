#!/usr/bin/env bash
# 05-max-budget-exit.sh
#
# Question: On --max-budget-usd exhaustion, is a clean `result` event
# emitted before the process exits?
#
# The controller's per-iteration cost guard reads cost from the result
# event. If budget exhaustion doesn't produce a result event, the
# controller must fall back to a wall-clock duration estimate and a
# "budget exhausted, exact cost unknown" banner.

set -u
OUT=$(mktemp /tmp/ralph-empirical-05.XXXXXX.ndjson)
trap 'rm -f "$OUT"' EXIT

if ! command -v claude >/dev/null 2>&1; then
    echo "FAIL: claude not on PATH"; exit 2
fi
if ! command -v jq >/dev/null 2>&1; then
    echo "FAIL: jq not on PATH"; exit 2
fi

# 1 cent ceiling -- low enough to trip on a verbose prompt.
claude --print --output-format stream-json --verbose \
       --include-partial-messages --no-session-persistence \
       --permission-mode bypassPermissions \
       --max-budget-usd 0.01 \
       -- "Write a detailed essay on operating-system schedulers covering CFS, BFS, MuQSS, and EEVDF, with examples." \
       > "$OUT" 2>/dev/null
EXIT=$?

echo "exit=$EXIT"
echo "ndjson lines: $(wc -l < "$OUT")"
echo "event types observed:"
jq -r 'select(.type) | .type' < "$OUT" 2>/dev/null | sort | uniq -c

if jq -e 'select(.type == "result")' < "$OUT" >/dev/null 2>&1; then
    echo "PASS: budget exhaustion produced a 'result' event"
    echo "result event (subtype + cost fields):"
    jq 'select(.type == "result") | {subtype, total_cost_usd, duration_ms, is_error}' \
       < "$OUT"
else
    echo "OBSERVED: budget exhaustion did NOT emit a 'result' event"
    echo "implication: controller per-iter cost guard must fall back to wall-clock"
fi

cp "$OUT" "${OUT%.ndjson}.kept.ndjson"
echo "kept: ${OUT%.ndjson}.kept.ndjson"
exit 0
