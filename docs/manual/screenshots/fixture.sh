#!/usr/bin/env bash
#
# Deterministic, throwaway beads project for the manual screenshot pipeline.
#
# The screenshot pipeline must render the same views on every machine without
# ever touching a real store.  This script builds an isolated git project in a
# fresh mktemp directory, seeds it from the role-keyed fixture beside it, and
# prints the project directory on stdout.  Tear it down with `teardown DIR`.
#
# Commands:
#   fixture.sh create              create + seed a project, print its path
#   fixture.sh teardown DIR        remove a project created by `create`
#   fixture.sh verify              create, check the fixture, tear down, PASS
#
# The fixture project never reads or writes the repository's own .beads store
# nor any live/agent store: every bd call runs with the isolation variables
# below unset, inside the mktemp directory.
set -euo pipefail

# ---------------------------------------------------------------------------
# Store isolation
# ---------------------------------------------------------------------------
#
# A Gas Town agent shell exports all four of these variables and points bd at
# the shared production rig store.  BEADS_DIR is the load-bearing one: it names
# an absolute store path that overrides repo-local discovery, so `bd init`
# would resolve the production store, see it already initialized, and exit
# before the throwaway project is ever created.  The two Dolt port variables
# and GC_DOLT_PORT are defence in depth.
#
# This list is deliberately hardcoded rather than sourced from Emacs Lisp; the
# POSIX shell cannot read `beads-test-isolation-env-vars'.  W7's test asserts
# this hardcoded list equals that Lisp constant so the two cannot drift.
BEADS_FIXTURE_ISOLATION_VARS="BEADS_DIR BEADS_DOLT_PORT BEADS_DOLT_SERVER_PORT GC_DOLT_PORT"

unset BEADS_DIR
unset BEADS_DOLT_PORT
unset BEADS_DOLT_SERVER_PORT
unset GC_DOLT_PORT

# bd must never prompt for input while the pipeline runs unattended.
export BD_NON_INTERACTIVE=1

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FIXTURE_FILE="$SCRIPT_DIR/fixtures/beads-fixture.jsonl"

# Marker written into every created project.  teardown refuses to delete a
# directory without it, so a stray argument can never rm -rf a real checkout.
FIXTURE_MARKER=".beads-fixture-project"

# The role ids the fixture promises.  `verify` enforces these.
FIXTURE_PREFIX="demo"
FIXTURE_ROLE_COUNT=8

die() {
  printf 'fixture.sh: %s\n' "$*" >&2
  exit 1
}

require_tool() {
  command -v "$1" >/dev/null 2>&1 || die "required tool not found on PATH: $1"
}

# ---------------------------------------------------------------------------
# create
# ---------------------------------------------------------------------------

seed_project() {
  local project="$1"

  [ -f "$FIXTURE_FILE" ] || die "fixture file not found: $FIXTURE_FILE"

  (
    cd "$project"

    # An isolated repo needs a deterministic, local git identity; the fixture
    # never pushes anywhere.
    git init -q
    git config user.email "fixture@beads.local"
    git config user.name "Beads Fixture"

    # Initialize beads with the fixture prefix.  --skip-hooks/--skip-agents
    # keep the throwaway project free of tool integrations the pipeline does
    # not use, and keep creation fast and deterministic.  All bd chatter goes
    # to stderr so `create' can reserve stdout for the project path.
    bd init --prefix "$FIXTURE_PREFIX" --quiet --skip-hooks --skip-agents --non-interactive >&2 || exit 1

    # Import the role-keyed fixture.  The JSONL already carries labels,
    # parent-child and blocks dependencies, and comments, so the importer
    # expresses all of them and no post-import bd writes are required.  If a
    # future bd import drops one of those fields, apply it here with
    # `bd label add' / `bd comment' before returning.
    bd import "$FIXTURE_FILE" --quiet >&2 || exit 1
  )
}

cmd_create() {
  require_tool bd
  require_tool git
  require_tool mktemp

  local project
  project="$(mktemp -d "${TMPDIR:-/tmp}/beads-screenshot-fixture.XXXXXX")" \
    || die "could not create temp directory"

  # Clean up a half-built project if seeding fails partway.
  if ! seed_project "$project"; then
    rm -rf "$project"
    die "failed to seed fixture project"
  fi

  : >"$project/$FIXTURE_MARKER"
  [ -d "$project/.beads" ] || die "seed did not create $project/.beads"

  # stdout is the contract: only the project directory goes here.
  printf '%s\n' "$project"
}

# ---------------------------------------------------------------------------
# teardown
# ---------------------------------------------------------------------------

cmd_teardown() {
  local project="${1:-}"
  [ -n "$project" ] || die "usage: fixture.sh teardown DIR"

  case "$project" in
    */../*|*/..) die "refusing suspicious path: $project" ;;
  esac

  [ -d "$project" ] || die "not a directory: $project"
  [ -f "$project/$FIXTURE_MARKER" ] \
    || die "refusing to remove $project: missing $FIXTURE_MARKER (not a fixture project)"

  rm -rf "$project"
}

# ---------------------------------------------------------------------------
# verify
# ---------------------------------------------------------------------------

verify_project() {
  local project="$1"
  require_tool python3

  (
    cd "$project"
    bd list --all --json
  ) | python3 -c '
import json
import re
import sys

prefix, role_count = sys.argv[1], int(sys.argv[2])
issues = json.load(sys.stdin)
if isinstance(issues, dict):
    issues = [issues]

errors = []
by_id = {}
for issue in issues:
    ident = issue.get("id", "")
    if ident in by_id:
        errors.append(f"duplicate id: {ident}")
    by_id[ident] = issue

pattern = re.compile(rf"^{re.escape(prefix)}-\d+$")
for ident in by_id:
    if not pattern.match(ident):
        errors.append(f"non role id (auto-generated hash?): {ident}")

expected = {f"{prefix}-{n}" for n in range(1, role_count + 1)}
missing = expected - set(by_id)
if missing:
    errors.append(f"missing role ids: {sorted(missing)}")

# Every dependency target must exist.
for ident, issue in by_id.items():
    for dep in issue.get("dependencies") or []:
        target = dep.get("depends_on_id")
        if target and target not in by_id:
            errors.append(f"{ident} depends on missing {target}")

statuses = {issue.get("status") for issue in by_id.values()}
for required in ("open", "in_progress", "blocked", "closed"):
    if required not in statuses:
        errors.append(f"missing status: {required}")

# An epic with at least two parent-child children.
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
' "$FIXTURE_PREFIX" "$FIXTURE_ROLE_COUNT"
}

cmd_verify() {
  require_tool bd
  require_tool git
  require_tool mktemp

  local project=""
  # `create` prints only the path; its diagnostics go to stderr.
  if ! project="$(cmd_create)"; then
    die "failed to create fixture project"
  fi
  trap 'rm -rf "${project:-}"' EXIT

  verify_project "$project"
  printf 'fixture.sh: PASS\n'
}

# ---------------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------------

case "${1:-}" in
  create)   cmd_create ;;
  teardown) shift; cmd_teardown "${1:-}" ;;
  verify)   cmd_verify ;;
  *)
    cat >&2 <<'USAGE'
usage: fixture.sh create
       fixture.sh teardown DIR
       fixture.sh verify

create    build an isolated project from fixtures/beads-fixture.jsonl and
          print its absolute path on stdout
teardown  remove a project previously created by `create`
verify    create, validate the fixture, tear down, and print PASS
USAGE
    exit 2
    ;;
esac
