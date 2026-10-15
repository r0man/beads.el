#!/usr/bin/env bash
# large-store.sh --- reproducible large-store benchmark for beads.el.
#
# Builds a throwaway git + beads project seeded with N issues (default
# 1200, matching the real stores the feature was motivated by), then runs
# `benchmarks/large-store.el' under a batch Emacs to time the hot paths
# and count `bd' reads per operation.
#
# Usage:
#   benchmarks/large-store.sh [N] [--repeat K] [--label TEXT] [--keep]
#
#   N            Issue count to seed (default 1200).
#   --repeat K   Repetitions per measurement, best-of (default 3).
#   --label TEXT Free-form label shown in the report header.
#   --keep       Keep the seeded store and print its path instead of
#                deleting it on exit.
#
# The store is created under `mktemp -d' and is always torn down unless
# --keep.  Every isolation variable that could reroute `bd' to a shared
# production store is unset before any `bd' call (the same list the test
# suite uses; see `beads-test-isolation-env-vars').
#
# Requires: bd, dolt, git, an Emacs with beads.el's deps (transient,
# vui) on the load path, and a POSIX shell with arithmetic.

set -euo pipefail

readonly SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
readonly ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)

# Hardcoded isolation list; keep in sync with `beads-test-isolation-env-vars'
# in lisp/test/beads-integration-test.el and the screenshot fixture.
readonly ISOLATION_ENV_VARS=(
  BEADS_DIR
  BEADS_DOLT_PORT
  BEADS_DOLT_SERVER_PORT
  GC_DOLT_PORT
)
for var in "${ISOLATION_ENV_VARS[@]}"; do
  unset "$var" 2>/dev/null || true
done

N=1200
REPEAT=3
LABEL=""
KEEP=0

while [ $# -gt 0 ]; do
  case "$1" in
    --repeat) REPEAT=$2; shift 2 ;;
    --label)  LABEL=$2; shift 2 ;;
    --keep)   KEEP=1; shift ;;
    -h|--help)
      sed -n '2,30p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    *)
      case "$1" in
        ''|*[!0-9]*) printf 'large-store.sh: unknown argument: %s\n' "$1" >&2; exit 2 ;;
        *) N=$1; shift ;;
      esac ;;
  esac
done

command -v bd >/dev/null 2>&1 || { echo "large-store.sh: bd not found" >&2; exit 1; }
command -v git >/dev/null 2>&1 || { echo "large-store.sh: git not found" >&2; exit 1; }

readonly EMACS=${EMACS:-emacs}
command -v "$EMACS" >/dev/null 2>&1 || { echo "large-store.sh: emacs not found" >&2; exit 1; }

BENCH_DIR=$(mktemp -d "${TMPDIR:-/tmp}/beads-large-store.XXXXXX")
readonly BENCH_DIR

cleanup() {
  if [ "$KEEP" -eq 0 ]; then
    rm -rf "$BENCH_DIR"
  else
    printf 'large-store.sh: kept store at %s\n' "$BENCH_DIR" >&2
  fi
}
trap cleanup EXIT

gen_issues() {
  local n=$1 i st ty
  local statuses=(open in_progress blocked closed open open in_progress open deferred open)
  local types=(task bug feature chore task task)
  for ((i = 0; i < n; i++)); do
    st=${statuses[i % ${#statuses[@]}]}
    ty=${types[i % ${#types[@]}]}
    printf '{"title":"Perf issue %05d with a reasonably descriptive title for rendering","description":"Body line one for issue %d.\\n\\nBody line two with more text to parse and render.","issue_type":"%s","priority":%d,"status":"%s","assignee":"worker-%d","labels":["performance","area-%d"]}\n' \
      "$i" "$i" "$ty" "$((i % 5))" "$st" "$((i % 7))" "$((i % 13))"
  done
}

printf 'large-store.sh: seeding %d issues in %s\n' "$N" "$BENCH_DIR" >&2
git -C "$BENCH_DIR" init -q
(
  cd "$BENCH_DIR"
  bd init --prefix perf >/dev/null 2>&1 || bd init >/dev/null 2>&1
)
gen_issues "$N" > "$BENCH_DIR/issues.jsonl"
(
  cd "$BENCH_DIR"
  bd import issues.jsonl >/dev/null 2>&1
)
printf 'large-store.sh: seeded; running benchmark\n' >&2

BEADS_BENCH_DIR="$BENCH_DIR" \
BEADS_BENCH_N="$N" \
BEADS_BENCH_REPEAT="$REPEAT" \
BEADS_BENCH_LABEL="$LABEL" \
  "$EMACS" --batch -Q -L "$ROOT/lisp" -l "$ROOT/benchmarks/large-store.el"
