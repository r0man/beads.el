#!/usr/bin/env bash
# formula-verify.sh --- exec-mode check for the user-manual-screenshots formula.
#
# The Gas City check runner executes gc.check_path with no arguments, so this
# thin wrapper re-runs the reproducibility comparison the formula's `verify`
# step documents: capture the scope twice and fail unless every image is
# byte-identical.  The scope is taken from BEADS_SHOT_SCOPE when set and
# otherwise verifies the full manifest, which is the strongest check.
#
# Usage: formula-verify.sh [scope]
#
# See capture.sh and README.md in this directory for the pipeline and scope
# semantics.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCOPE="${1:-${BEADS_SHOT_SCOPE:-all}}"

exec "$HERE/capture.sh" --verify "$SCOPE"
