#!/usr/bin/env bash
# fixture.sh --- deterministic, isolated fixture for the screenshot pipeline.
#
# Creates a throwaway git + beads project seeded from
# fixtures/beads-fixture.jsonl, prints the absolute project directory, and
# removes it again on teardown.  This is the base of REQ-005 (never touch the
# repository's own .beads store or a live/agent store) and REQ-006 (the same
# run produces the same store, independent of run-generated ids).
#
# Usage:
#   docs/manual/screenshots/fixture.sh create              # create + seed; print path
#   docs/manual/screenshots/fixture.sh teardown <dir>      # remove a created project
#   docs/manual/screenshots/fixture.sh verify              # create, check, tear down
#   docs/manual/screenshots/fixture.sh print-isolation-vars
#   docs/manual/screenshots/fixture.sh --help
#
# `create' (also `up', and the no-argument default) prints exactly one line:
# the absolute path of the temporary project.  Every bd message goes to stderr,
# so callers can capture the path with a plain `$(fixture.sh create)'.
#
# Isolation (plan-review P2): the four environment variables below can reroute
# `bd' to the shared production store.  They are unset before any bd call and
# hardcoded here because a POSIX shell cannot source the Emacs Lisp constant
# `beads-test-isolation-env-vars' (lisp/test/beads-integration-test.el).  Keep
# the two lists in sync; lisp/test/beads-manual-test.el asserts the equality by
# reading `fixture.sh print-isolation-vars'.
#
# The fixture is byte-reproducible: every issue, dependency, and comment in
# fixtures/beads-fixture.jsonl pins its id, owner, created_by, and timestamps,
# so two `create' runs export identical JSONL and render identical screenshots.

set -euo pipefail

readonly FIXTURE_PREFIX="demo"

# Hardcoded isolation list.  One name per array entry, in the same order as
# `beads-test-isolation-env-vars' in lisp/test/beads-integration-test.el.
readonly ISOLATION_ENV_VARS=(
  BEADS_DIR
  BEADS_DOLT_PORT
  BEADS_DOLT_SERVER_PORT
  GC_DOLT_PORT
)

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
readonly SCRIPT_DIR
readonly FIXTURE_FILE="$SCRIPT_DIR/fixtures/beads-fixture.jsonl"
readonly FIXTURE_TMPDIR="${TMPDIR:-/tmp}"
readonly FIXTURE_DIR_PREFIX="beads-fixture."

# Marker written into every created project.  teardown refuses to delete a
# directory without it, so a stray argument can never rm -rf a real checkout.
readonly FIXTURE_MARKER=".beads-fixture-project"

die() {
  printf 'fixture.sh: %s\n' "$*" >&2
  exit 1
}

require_tool() {
  command -v "$1" >/dev/null 2>&1 || die "required tool not found on PATH: $1"
}

usage() {
  cat <<'EOF'
Usage: fixture.sh <command>

Commands:
  create, up          Create and seed a throwaway project; print its path.
                      This is the default when no command is given.
  teardown <dir>      Remove a project created by `create'.
  verify              Create a project, validate its role ids, dependencies,
                      states, labels, and comments, then tear it down.
  print-isolation-vars
                      Print the hardcoded isolation variable names, one per
                      line, for the drift test.
  help, --help, -h    Show this help.

Environment:
  The four isolation variables (BEADS_DIR, BEADS_DOLT_PORT,
  BEADS_DOLT_SERVER_PORT, GC_DOLT_PORT) are unset before every bd call, so the
  temp project is always the only store bd can reach.
EOF
}

unset_isolation_vars() {
  local var
  for var in "${ISOLATION_ENV_VARS[@]}"; do
    unset "$var"
  done
}

print_isolation_vars() {
  printf '%s\n' "${ISOLATION_ENV_VARS[@]}"
}

# ---------------------------------------------------------------------------
# create
# ---------------------------------------------------------------------------

seed_project() {
  local project="$1"

  (
    set -euo pipefail
    cd "$project"

    # An isolated repo needs a deterministic, local git identity; the fixture
    # never pushes anywhere.
    git init -q
    git config user.email "fixture@beads.invalid"
    git config user.name "Beads Fixture"

    # --skip-hooks/--skip-agents keep the throwaway project free of tool
    # integrations the pipeline does not use.  All bd chatter goes to stderr
    # so `create' can reserve stdout for the project path.
    bd init --prefix "$FIXTURE_PREFIX" --quiet --non-interactive \
      --skip-hooks --skip-agents 1>&2

    # The JSONL already carries labels, parent-child and blocks dependencies,
    # and comments, so the importer expresses all of them and no post-import bd
    # writes are required.  If a future bd import drops one of those fields,
    # apply it here with `bd label add' / `bd comments add' before returning.
    bd import "$FIXTURE_FILE" --quiet 1>&2
  )
}

create_fixture() {
  [ -r "$FIXTURE_FILE" ] || die "fixture data not found: $FIXTURE_FILE"
  require_tool bd
  require_tool git
  require_tool mktemp

  local project
  project=$(mktemp -d "$FIXTURE_TMPDIR/$FIXTURE_DIR_PREFIX"XXXXXX) \
    || die "mktemp -d failed under $FIXTURE_TMPDIR"

  # Clean up a half-built project if seeding fails partway.
  if ! seed_project "$project"; then
    rm -rf -- "$project"
    die "failed to seed fixture project in $project"
  fi

  [ -d "$project/.beads" ] || {
    rm -rf -- "$project"
    die "fixture project has no .beads store: $project"
  }

  : >"$project/$FIXTURE_MARKER"
  printf '%s\n' "$project"
}

# ---------------------------------------------------------------------------
# teardown
# ---------------------------------------------------------------------------

teardown_fixture() {
  local project="${1:-}"
  [ -n "$project" ] || die "teardown needs the project directory"

  case "$project" in
    "$FIXTURE_TMPDIR/$FIXTURE_DIR_PREFIX"*) ;;
    *)
      die "refusing to remove unexpected path: $project"
      ;;
  esac

  [ -d "$project" ] || {
    printf 'fixture.sh: already gone: %s\n' "$project" >&2
    return 0
  }
  [ -f "$project/$FIXTURE_MARKER" ] \
    || die "refusing to remove $project: missing $FIXTURE_MARKER (not a fixture project)"

  rm -rf -- "$project"
  printf 'fixture.sh: removed %s\n' "$project" >&2
}

# ---------------------------------------------------------------------------
# verify
# ---------------------------------------------------------------------------

verify_project() {
  local project="$1"
  require_tool python3

  (
    cd "$project"
    bd export
  ) | python3 -c '
import json
import re
import sys

fixture_path = sys.argv[1]
prefix = sys.argv[2]

issues = []
for line in sys.stdin:
    line = line.strip()
    if line:
        issues.append(json.loads(line))

expected = []
with open(fixture_path, encoding="utf-8") as handle:
    for line in handle:
        line = line.strip()
        if line:
            expected.append(json.loads(line)["id"])

errors = []
by_id = {}
for issue in issues:
    ident = issue.get("id", "")
    if ident in by_id:
        errors.append(f"duplicate id: {ident}")
    by_id[ident] = issue

role_re = re.compile(rf"^{re.escape(prefix)}-[0-9]+(\.[0-9]+)*$")
for ident in by_id:
    if not role_re.match(ident):
        errors.append(f"non role id (auto-generated hash?): {ident}")

missing = [ident for ident in expected if ident not in by_id]
if missing:
    errors.append(f"missing role ids: {sorted(missing)}")

for ident, issue in by_id.items():
    for dep in issue.get("dependencies") or []:
        target = dep.get("depends_on_id")
        if target and target not in by_id:
            errors.append(f"{ident} depends on missing {target}")

statuses = {issue.get("status") for issue in by_id.values()}
for required in ("open", "in_progress", "blocked", "closed"):
    if required not in statuses:
        errors.append(f"missing status: {required}")

children = {}
for ident, issue in by_id.items():
    for dep in issue.get("dependencies") or []:
        if dep.get("type") == "parent-child":
            children.setdefault(dep.get("depends_on_id"), []).append(ident)
if not any(
    issue.get("issue_type") == "epic" and len(children.get(ident, [])) >= 2
    for ident, issue in by_id.items()
):
    errors.append("no epic with two parent-child children")

if not any(issue.get("labels") for issue in by_id.values()):
    errors.append("no labeled issue")
if not any(
    issue.get("comments") or issue.get("comment_count")
    for issue in by_id.values()
):
    errors.append("no commented issue")

if errors:
    for error in errors:
        print(f"fixture.sh: verify: {error}", file=sys.stderr)
    sys.exit(1)
print(f"fixture.sh: verify: {len(by_id)} role-keyed issues OK")
' "$FIXTURE_FILE" "$FIXTURE_PREFIX"
}

verify_fixture() {
  require_tool bd
  require_tool git
  require_tool mktemp

  local project
  if ! project=$(create_fixture); then
    die "failed to create fixture project"
  fi
  trap 'rm -rf -- "${project:-}"' EXIT

  verify_project "$project"
  rm -rf -- "$project"
  trap - EXIT
  printf 'fixture.sh: PASS\n'
}

# ---------------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------------

main() {
  unset_isolation_vars
  case "${1:-create}" in
    create | up) create_fixture ;;
    teardown | destroy | remove) teardown_fixture "${2:-}" ;;
    verify) verify_fixture ;;
    print-isolation-vars) print_isolation_vars ;;
    help | --help | -h) usage ;;
    *)
      printf 'fixture.sh: unknown command: %s\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
}

main "$@"
