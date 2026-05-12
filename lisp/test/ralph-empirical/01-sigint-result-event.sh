#!/usr/bin/env bash
# 01-sigint-result-event.sh
#
# Question: Does `claude --print --output-format stream-json` emit a final
# `result` event when interrupted with SIGINT?
#
# The Ralph stream parser has a drain-on-sentinel path that assumes the
# result event has been observed before terminal status is set. If SIGINT
# doesn't emit a result, the controller must NOT block waiting for cost
# data on user-initiated stops; status `stopped` must carry whatever cost
# it has.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
OUT=$(mktemp /tmp/ralph-empirical-01.XXXXXX.ndjson)
trap 'rm -f "$OUT"' EXIT

if ! command -v claude >/dev/null 2>&1; then
    echo "FAIL: claude not on PATH"; exit 2
fi
if ! command -v jq >/dev/null 2>&1; then
    echo "FAIL: jq not on PATH"; exit 2
fi

# Start a deliberately slow-ish prompt and SIGINT after 2s.
claude --print --output-format stream-json --verbose \
       --include-partial-messages --no-session-persistence \
       --permission-mode bypassPermissions \
       -- "count slowly from 1 to 100, one number per line" \
       > "$OUT" 2>/dev/null &
PID=$!

sleep 2
kill -INT "$PID" 2>/dev/null
wait "$PID"
EXIT=$?

echo "exit=$EXIT"
echo "ndjson lines: $(wc -l < "$OUT")"
echo "event types observed:"
jq -r 'select(.type) | .type' < "$OUT" 2>/dev/null | sort | uniq -c

if jq -e 'select(.type == "result")' < "$OUT" >/dev/null 2>&1; then
    echo "PASS: SIGINT produced a final 'result' event"
    echo "result event:"
    jq 'select(.type == "result")' < "$OUT"
    cp "$OUT" "${OUT%.ndjson}.kept.ndjson"
    echo "kept: ${OUT%.ndjson}.kept.ndjson"
    exit 0
else
    echo "OBSERVED: SIGINT did NOT emit a 'result' event"
    echo "implication: controller stop must record status=stopped with"
    echo "             nil cost-usd / duration-ms (no final event to read)."
    cp "$OUT" "${OUT%.ndjson}.kept.ndjson"
    echo "kept: ${OUT%.ndjson}.kept.ndjson"
    # This is informational, not a test failure.
    exit 0
fi
