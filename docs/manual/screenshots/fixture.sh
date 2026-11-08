#!/usr/bin/env bash
# fixture.sh --- deterministic, isolated fixture for the screenshot pipeline.
#
# Creates the shared demo project at ${TMPDIR:-/tmp}/demo-project, seeded from
# fixtures/beads-fixture.jsonl, prints its absolute path, and removes it again
# on teardown.  The fixed, shared path is the contract the sibling gascity.el
# manual uses too, so both manuals describe one demo project with one stable
# basename (REQ-001, REQ-003).  This is the base of REQ-005 (never touch the
# repository's own .beads store or a live/agent store) and REQ-006 (the same
# run produces the same store, independent of run-generated ids).
#
# Usage:
#   docs/manual/screenshots/fixture.sh create              # create + seed; print path
#   docs/manual/screenshots/fixture.sh teardown <dir>      # remove the shared project
#   docs/manual/screenshots/fixture.sh verify              # create, check, tear down
#   docs/manual/screenshots/fixture.sh print-isolation-vars
#   docs/manual/screenshots/fixture.sh --help
#
# `create' (also `up', and the no-argument default) prints exactly one line:
# the absolute project path.  Every bd message goes to stderr, so callers can
# capture the path with a plain `$(fixture.sh create)'.
#
# Because the path is fixed rather than per-run, `create' and `teardown' hold
# an advisory flock on ${TMPDIR:-/tmp}/demo-project.lock for their duration;
# `capture.sh' holds the same lock for a whole capture run.  A caller that
# already holds it (capture.sh) exports FIXTURE_LOCK_HELD=1 and the nested
# invocation skips acquisition, so it cannot deadlock against its own parent.
#
# The project path, the frozen dataset hash, and the isolation contract are
# documented in docs/manual/screenshots/README.md and in the manual's shared
# demo-project section; `verify' checks that both still record the hash.
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
readonly FIXTURE_README="$SCRIPT_DIR/README.md"
# One shared, fixed project path for both manuals (REQ-001).  Normalize a
# trailing slash in TMPDIR so capture.sh derives the identical lock path.
FIXTURE_TMPDIR="${TMPDIR:-/tmp}"
while [ "${FIXTURE_TMPDIR%/}" != "$FIXTURE_TMPDIR" ]; do
  FIXTURE_TMPDIR="${FIXTURE_TMPDIR%/}"
done
[ -n "$FIXTURE_TMPDIR" ] || FIXTURE_TMPDIR="/"
readonly FIXTURE_TMPDIR
readonly FIXTURE_PROJECT_DIR="${FIXTURE_TMPDIR}/demo-project"
# The lock lives next to, never inside, the project (teardown deletes the
# project).  capture.sh computes the same path.
readonly FIXTURE_LOCK_FILE="${FIXTURE_TMPDIR}/demo-project.lock"
readonly FIXTURE_LOCK_TIMEOUT="${FIXTURE_LOCK_TIMEOUT:-600}"

# The frozen dataset digest.  This is the single literal; `verify' checks the
# JSONL against it and checks that both the screenshot README and the manual's
# shared demo-project section record it, so a data edit cannot land without its
# documentation (REQ-002).
readonly FIXTURE_DATA_SHA256="6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8"

# Marker written into every created project.  teardown refuses to delete a
# directory without it, and `create' refuses to replace one, so a stray
# argument or an unrelated directory at the shared path can never be rm -rf'd.
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
  create, up          Create and seed the shared demo project at
                      ${TMPDIR:-/tmp}/demo-project and print its path.  A
                      stale fixture there is recreated; any other directory
                      is refused.  This is the default when no command is
                      given.
  teardown <dir>      Remove the shared demo project.  Only the exact
                      ${TMPDIR:-/tmp}/demo-project path is accepted, and it
                      must carry the .beads-fixture-project marker.
  verify              Create the project, validate its role ids,
                      dependencies, states, labels, and comments, check the
                      frozen dataset hash recorded in the README and the
                      manual, then tear it down.
  print-isolation-vars
                      Print the hardcoded isolation variable names, one per
                      line, for the drift test.
  help, --help, -h    Show this help.

Environment:
  TMPDIR                Parent of the shared demo-project and its lock file
                        (default: /tmp).
  FIXTURE_LOCK_TIMEOUT  Seconds to wait for the fixture lock (default: 600).
  FIXTURE_LOCK_HELD     Set by capture.sh, which already holds the lock; a
                        nested create/teardown then skips acquisition.

Isolation:
  The four isolation variables (BEADS_DIR, BEADS_DOLT_PORT,
  BEADS_DOLT_SERVER_PORT, GC_DOLT_PORT) are unset before every bd call, so the
  shared demo project is always the only store bd can reach.

The fixture project lives at ${TMPDIR:-/tmp}/demo-project, next to its
advisory lock file ${TMPDIR:-/tmp}/demo-project.lock.  create and teardown
hold the lock briefly; concurrent runs serialize on it instead of sharing the
store.
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
# Serialization
# ---------------------------------------------------------------------------

# Run a command under the shared advisory lock.  A nested invocation made by
# capture.sh already inherits the lock through FIXTURE_LOCK_HELD=1, so it must
# not open a second flock on the same file and block against its own parent.
with_lock() {
  if [ -n "${FIXTURE_LOCK_HELD:-}" ]; then
    "$@"
    return
  fi
  require_tool flock
  mkdir -p -- "$FIXTURE_TMPDIR" \
    || die "cannot create the fixture parent: $FIXTURE_TMPDIR"
  exec 9>"$FIXTURE_LOCK_FILE" \
    || die "cannot open the lock file: $FIXTURE_LOCK_FILE"
  if ! flock -w "$FIXTURE_LOCK_TIMEOUT" 9; then
    exec 9>&-
    die "timed out after ${FIXTURE_LOCK_TIMEOUT}s waiting for $FIXTURE_LOCK_FILE"
  fi
  printf 'fixture.sh: holding %s\n' "$FIXTURE_LOCK_FILE" >&2
  local status=0
  "$@" || status=$?
  flock -u 9 2>/dev/null || true
  exec 9>&-
  return "$status"
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

remove_fixture_dir() {
  local project="${1:-}"
  [ -n "$project" ] || return 0
  rm -rf -- "$project"
}

create_fixture() {
  [ -r "$FIXTURE_FILE" ] || die "fixture data not found: $FIXTURE_FILE"
  # Check the tools before touching the shared path: a shell without `bd'
  # must not destroy a working fixture and then fail (R3).
  require_tool bd
  require_tool git
  local project="$FIXTURE_PROJECT_DIR"

  # The path is shared and fixed, so a stale fixture is expected and is
  # recreated.  Anything else at that path is never deleted: only a directory
  # carrying our marker is ours (REQ-001).
  if [ -e "$project" ] || [ -L "$project" ]; then
    [ -f "$project/$FIXTURE_MARKER" ] \
      || die "refusing to remove $project: exists without $FIXTURE_MARKER (not a fixture project)"
    rm -rf -- "$project" || die "cannot remove the stale fixture: $project"
  fi

  mkdir -p -- "$project" \
    || { remove_fixture_dir "$project"; die "mkdir failed: $project"; }

  # Clean up a half-built project if seeding fails partway.
  if ! seed_project "$project"; then
    remove_fixture_dir "$project"
    die "failed to seed fixture project in $project"
  fi

  [ -d "$project/.beads" ] || {
    remove_fixture_dir "$project"
    die "fixture project has no .beads store: $project"
  }

  : >"$project/$FIXTURE_MARKER"
  printf '%s\n' "$project"
}

# ---------------------------------------------------------------------------
# teardown
# ---------------------------------------------------------------------------

trim_trailing_slashes() {
  local path="${1:-}"
  while [ "${path%/}" != "$path" ]; do
    path="${path%/}"
  done
  printf '%s\n' "$path"
}

teardown_fixture() {
  local project="${1:-}"
  [ -n "$project" ] || die "teardown needs the project directory"
  project="$(trim_trailing_slashes "$project")"

  # Only the one shared path is removable, so a stray argument can never point
  # the marker-guarded rm -rf somewhere else.
  [ "$project" = "$FIXTURE_PROJECT_DIR" ] \
    || die "refusing to remove unexpected path: $project (only $FIXTURE_PROJECT_DIR is allowed)"

  if [ ! -d "$project" ]; then
    # Idempotent when the project is already gone, but never silently accept a
    # file or symlink squatting on the shared path.
    if [ -e "$project" ] || [ -L "$project" ]; then
      die "refusing to remove $project: not a directory"
    fi
    printf 'fixture.sh: already gone: %s\n' "$project" >&2
    return 0
  fi
  [ -f "$project/$FIXTURE_MARKER" ] \
    || die "refusing to remove $project: missing $FIXTURE_MARKER (not a fixture project)"

  remove_fixture_dir "$project"
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

# The dataset hash is pinned once above; `verify' proves the JSONL still
# matches it and that both documents that promise it (the screenshot README
# and the manual's shared demo-project section) record the same value.
check_dataset_hash() {
  require_tool sha256sum
  local actual expected="$FIXTURE_DATA_SHA256" file found=0
  actual="$(sha256sum "$FIXTURE_FILE" | cut -d' ' -f1)"
  [ "$actual" = "$expected" ] \
    || die "dataset hash mismatch: $actual, expected $expected in FIXTURE_DATA_SHA256"
  grep -qF "$expected" "$FIXTURE_README" \
    || die "$FIXTURE_README does not record the dataset hash $expected"
  for file in "$SCRIPT_DIR"/../../../doc/*.texi; do
    [ -f "$file" ] || continue
    if grep -qF "$expected" "$file"; then found=1; break; fi
  done
  [ "$found" -eq 1 ] \
    || die "the manual Texinfo sources do not record the dataset hash $expected"
}

verify_fixture() {
  require_tool bd
  require_tool git
  check_dataset_hash

  local project
  if ! project=$(create_fixture); then
    die "failed to create fixture project"
  fi
  trap 'remove_fixture_dir "${project:-}"' EXIT

  verify_project "$project"
  remove_fixture_dir "$project"
  trap - EXIT
  printf 'fixture.sh: PASS\n'
}

# ---------------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------------

main() {
  unset_isolation_vars
  case "${1:-create}" in
    create | up) with_lock create_fixture ;;
    teardown | destroy | remove) with_lock teardown_fixture "${2:-}" ;;
    verify) with_lock verify_fixture ;;
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
