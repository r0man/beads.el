#!/usr/bin/env bash
# 03-partial-messages-shape.sh
#
# Question: With --include-partial-messages, what is the exact `type`
# field of partial events, how do they reference their parent message
# id, and what is the ordering of content blocks within a single
# assistant turn?
#
# The Ralph stream parser's partial-messages hash table needs to know:
# - which event types belong to which logical message
# - whether ordering is strictly stream order or whether blocks can
#   arrive interleaved
# - whether the parent message id is on the event itself or only on a
#   prior `message_start` shaped event

set -u
OUT=$(mktemp /tmp/ralph-empirical-03.XXXXXX.ndjson)
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
       --max-turns 2 \
       -- "Say a one-sentence greeting, then say goodbye." \
       > "$OUT" 2>/dev/null
EXIT=$?

echo "exit=$EXIT"
echo "ndjson lines: $(wc -l < "$OUT")"
echo
echo "All unique event 'type' values:"
jq -r 'select(.type) | .type' < "$OUT" | sort -u
echo
echo "Distinct type+subtype combos:"
jq -r 'select(.type) | "\(.type)\t\(.subtype // "-")"' < "$OUT" | sort -u
echo
echo "First 5 events (compact):"
head -n 5 "$OUT" | jq -c .
echo
echo "Sample partial-message event (first one with content_block_delta or similar):"
jq -c 'select(.type | test("delta|partial|content_block"))' < "$OUT" | head -n 3
echo
echo "All keys appearing at top level of any event (frequency):"
jq -r 'keys[]' < "$OUT" | sort | uniq -c | sort -rn

cp "$OUT" "${OUT%.ndjson}.kept.ndjson"
echo "kept: ${OUT%.ndjson}.kept.ndjson"
exit 0
