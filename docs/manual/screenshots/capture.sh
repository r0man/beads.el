#!/usr/bin/env bash
#
# capture.sh --- Regenerate the beads.el documentation screenshots.
#
# Pipeline, end to end:
#   1. Start a *private* headless Wayland compositor (sway) so capture
#      frames never appear on the user's desktop.
#   2. Run a graphical Emacs once per theme, loading beads.el + the
#      reusable engine (screenshot.el) + the shot list (beads-shots.el),
#      which renders each view and exports a raw PNG.
#   3. Compress each raw PNG to an 8-bit-palette full image and a small
#      thumbnail under docs/manual/images/ (committed; the raws are
#      throwaway).
#
# The capture is scoped: `--scope all` regenerates every image, a named
# subset regenerates only those views, and `--verify` runs the same scope
# twice and fails, naming any image that is not byte-identical.  A subset
# run never rewrites or deletes an image outside the selected scope.
#
# Requirements: emacs (pgtk/GUI), sway (wlroots headless backend),
# ImageMagick `convert`, the `bd` CLI, and ef-themes.  The views run
# against a throwaway fixture project created by fixture.sh (REQ-005).
#
# The engine is copied verbatim from the sibling gascity.el pipeline; see
# README.md in this directory for provenance and the cross-repo contract.
#
# Usage:
#   ./capture.sh                       # full pipeline, scope all
#   ./capture.sh --scope views          # only the named views
#   ./capture.sh --scope all --verify   # capture twice, assert byte-identical
#   ./capture.sh --compress             # recompress existing raws only
#
# Scope is `all`, `changed`, or a comma-separated list of view names.
# `changed` selects the views whose sources changed since BEADS_SHOT_BASE
# (default: the merge-base with the origin default branch).
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../../.." && pwd)"
IMAGES_DIR="$REPO/docs/manual/images"
STAGING="${BEADS_SHOT_STAGING:-/tmp/beads-shots}"
SHOTS_EL="$HERE/beads-shots.el"
FIXTURE_SH="$HERE/fixture.sh"
WATCHDOG="${BEADS_SHOT_WATCHDOG:-400}"

export BEADS_REPO="${BEADS_REPO:-$REPO}"

# Output image geometry.  UI screenshots are flat-colour, so 8-bit palette
# PNGs with dithering disabled compress small and stay crisp.
FULL_W=1500       # full image max width (px); narrower shots keep native size
FULL_COLORS=256   # palette size for the full images
THUMB_W=520       # thumbnail max width (px)
THUMB_COLORS=128  # a preview needs fewer colours

# ---- Capture manifest --------------------------------------------------
# One line per theme: "THEME  SUFFIX  view view view ...".  A view named
# in two themes is captured twice (e.g. the dashboard, to show theming).
# The suffix is the "-<theme>" half of the committed name, so the output is
# exactly <view>-<theme>.png and <view>-<theme>-thumb.png.
read -r -d '' MANIFEST <<'EOF' || true
ef-elea-dark  -dark   dashboard list ready blocked show epic compose molecule swarm agents dispatch maintenance agent-menu formula
ef-cyprus     -light  dashboard list show epic
EOF

# Source files per view, for `--scope changed`.  A view is selected when
# any of its sources changed relative to BEADS_SHOT_BASE.  The manual
# pages land in WI-1/WI-3; the product modules already exist.
declare -A VIEW_SOURCES=(
  [dashboard]="docs/manual/views.org lisp/beads-dashboard.el lisp/beads-dashboard-sections.el lisp/beads-section.el"
  [list]="docs/manual/issue-workflow.org lisp/beads-command-list.el lisp/beads-spec.el lisp/beads-pager.el"
  [ready]="docs/manual/issue-workflow.org lisp/beads-command-list.el lisp/beads-spec.el"
  [blocked]="docs/manual/issue-workflow.org lisp/beads-command-list.el lisp/beads-spec.el"
  [show]="docs/manual/issue-workflow.org lisp/beads-command-show.el lisp/beads-section.el"
  [epic]="docs/manual/formulas.org lisp/beads-command-show.el lisp/beads-command-epic.el"
  [compose]="docs/manual/issue-workflow.org lisp/beads-compose.el"
  [molecule]="docs/manual/formulas.org lisp/beads-molecule.el lisp/beads-command-mol.el"
  [swarm]="docs/manual/formulas.org lisp/beads-swarm.el"
  [agents]="docs/manual/agents.org lisp/beads-agent.el lisp/beads-agent-list.el"
  [dispatch]="docs/manual/index.org lisp/beads-menu.el"
  [maintenance]="docs/manual/orchestration.org lisp/beads-menu.el"
  [agent-menu]="docs/manual/agents.org lisp/beads-agent.el"
  [formula]="docs/manual/formulas.org lisp/beads-command-formula.el"
)

log() { printf '\033[1;36m[capture]\033[0m %s\n' "$*" >&2; }

usage_error() {
  printf 'capture.sh: %s\n' "$*" >&2
  printf 'Try --help for usage.\n' >&2
  exit 2
}

usage() {
  cat <<'EOF'
Usage: capture.sh [--scope SCOPE] [--verify [SCOPE]] [--compress] [--help]

  --scope SCOPE   all (default), changed, or a comma-separated view list.
  --verify [SCOPE]
                  Capture SCOPE twice and fail, naming the drifting image,
                  unless every output is byte-identical.
  --compress      Recompress the raw PNGs already in BEADS_SHOT_STAGING.
  --help          Show this help.

Environment:
  BEADS_REPO            beads.el checkout to capture (default: this repo)
  BEADS_SHOT_STAGING    raw PNG staging dir (default: /tmp/beads-shots)
  BEADS_SHOT_PROJECT    existing fixture project; when unset, fixture.sh
                        creates and tears down a throwaway one
  BEADS_SHOT_BASE       git ref for `--scope changed`
  BEADS_SHOT_ENGINE     capture executable override (used by the shell
                        tests; skips the compositor)
  BEADS_SHOT_WATCHDOG   seconds before a capture Emacs kills itself (400)
EOF
}

# ---- Isolation ---------------------------------------------------------
# Unset every variable in beads-test-isolation-env-vars so a value inherited
# from an agent shell cannot reroute the capture into the repo or a live
# store.  fixture.sh hardcodes the same four names; keep them in sync.
for _var in BEADS_DIR BEADS_DOLT_PORT BEADS_DOLT_SERVER_PORT GC_DOLT_PORT; do
  unset "$_var"
done
unset _var

# ---- Scope resolution --------------------------------------------------

all_views() {
  local line theme suffix
  local IFS=$' \t\n'
  while IFS= read -r line; do
    [ -z "${line// }" ] && continue
    # shellcheck disable=SC2086
    set -- $line
    theme="$1"; suffix="$2"; shift 2
    printf '%s\n' "$@"
  done <<< "$MANIFEST" | awk '!seen[$0]++'
}

view_known() { all_views | grep -qxF "$1"; }

changed_views() {
  local base="${BEADS_SHOT_BASE:-}" default_branch changed view src hit
  if [ -z "$base" ]; then
    default_branch="$(git -C "$REPO" symbolic-ref --short refs/remotes/origin/HEAD \
      2>/dev/null | sed 's|^origin/||' || true)"
    [ -n "$default_branch" ] && base="origin/$default_branch"
  fi
  [ -n "$base" ] || usage_error \
    "--scope changed: set BEADS_SHOT_BASE (cannot resolve the origin default branch)"
  changed="$(
    { git -C "$REPO" diff --name-only "$base"... 2>/dev/null || true
      git -C "$REPO" status --porcelain 2>/dev/null | awk '{print $NF}'
    } | sort -u
  )"
  SELECTED_VIEWS=()
  while read -r view; do
    [ -n "$view" ] || continue
    hit=0
    # shellcheck disable=SC2086
    for src in ${VIEW_SOURCES[$view]}; do
      if printf '%s\n' "$changed" | grep -qxF "$src"; then hit=1; break; fi
    done
    [ "$hit" = 1 ] && SELECTED_VIEWS+=("$view")
  done < <(all_views)
}

resolve_scope() {
  local scope="$1" v
  SELECTED_VIEWS=()
  case "$scope" in
    all)
      while IFS= read -r v; do SELECTED_VIEWS+=("$v"); done < <(all_views) ;;
    changed)
      changed_views ;;
    *)
      local IFS=','
      for v in $scope; do
        [ -n "$v" ] || continue
        view_known "$v" || usage_error "unknown view in --scope: $v"
        SELECTED_VIEWS+=("$v")
      done
      [ "${#SELECTED_VIEWS[@]}" -gt 0 ] || usage_error "empty --scope" ;;
  esac
  [ "${#SELECTED_VIEWS[@]}" -gt 0 ] || log "no views selected for scope $scope"
}

view_selected() {
  local v
  for v in "${SELECTED_VIEWS[@]}"; do [ "$v" = "$1" ] && return 0; done
  return 1
}

# ---- Fixture project lifecycle ----------------------------------------

PROJECT="${BEADS_SHOT_PROJECT:-}"
CREATED_PROJECT=""

ensure_project() {
  if [ -n "$PROJECT" ]; then
    [ -d "$PROJECT" ] || usage_error "BEADS_SHOT_PROJECT is not a directory: $PROJECT"
    export BEADS_SHOT_DIR="$PROJECT"
    return
  fi
  [ -r "$FIXTURE_SH" ] || usage_error "fixture.sh not found: $FIXTURE_SH"
  PROJECT="$("$FIXTURE_SH" create)"
  [ -n "$PROJECT" ] && [ -d "$PROJECT" ] || usage_error "fixture.sh create returned no project"
  CREATED_PROJECT="$PROJECT"
  export BEADS_SHOT_DIR="$PROJECT"
  log "fixture project: $PROJECT"
}

teardown_project() {
  if [ -n "$CREATED_PROJECT" ] && [ -r "$FIXTURE_SH" ]; then
    "$FIXTURE_SH" teardown "$CREATED_PROJECT" >/dev/null 2>&1 || true
  fi
}

# ---- Headless compositor lifecycle ------------------------------------
SWAY_PID=""
start_sway() {
  export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
  local conf="$STAGING/sway.conf" logf="$STAGING/sway.log"
  # sway is a *tiling* compositor: by default it forces every window to
  # fill the output, overriding the frame size we set.  Float (and
  # de-chrome) all windows so Emacs's `set-frame-size' is honored and the
  # exported PNG is fitted tightly to the rendered content.
  cat > "$conf" <<'CONF'
xwayland disable
default_border none
default_floating_border none
for_window [app_id=".*"] floating enable, border none
output HEADLESS-1 resolution 2880x1800 position 0,0
CONF
  local before after
  before=$(ls -1 "$XDG_RUNTIME_DIR"/wayland-[0-9]* 2>/dev/null | grep -v '\.lock$' | sort || true)
  WLR_BACKENDS=headless WLR_LIBINPUT_NO_DEVICES=1 sway -c "$conf" >"$logf" 2>&1 &
  SWAY_PID=$!
  local i=0
  WAYLAND_DISPLAY=""
  while [ $i -lt 50 ]; do
    kill -0 "$SWAY_PID" 2>/dev/null || { log "sway died:"; cat "$logf" >&2; exit 1; }
    after=$(ls -1 "$XDG_RUNTIME_DIR"/wayland-[0-9]* 2>/dev/null | grep -v '\.lock$' | sort || true)
    WAYLAND_DISPLAY=$(comm -13 <(printf '%s\n' "$before") <(printf '%s\n' "$after") \
                        | head -1 | xargs -r basename || true)
    [ -n "$WAYLAND_DISPLAY" ] && break
    sleep 0.2; i=$((i+1))
  done
  [ -z "$WAYLAND_DISPLAY" ] && { log "no headless wayland socket appeared"; cat "$logf" >&2; exit 1; }
  export WAYLAND_DISPLAY GDK_BACKEND=wayland
  log "headless sway on WAYLAND_DISPLAY=$WAYLAND_DISPLAY (pid $SWAY_PID)"
}
stop_sway() { [ -n "$SWAY_PID" ] && kill "$SWAY_PID" 2>/dev/null || true; }

cleanup() { stop_sway; teardown_project; }
trap cleanup EXIT INT TERM

# ---- Capture ----------------------------------------------------------

run_engine() {
  local theme="$1" suffix="$2" views="$3" outdir="$4"
  if [ -n "${BEADS_SHOT_ENGINE:-}" ]; then
    # The override is responsible for its own display; this is the seam the
    # shell tests use so `--verify` is exercisable without sway/Emacs.
    BEADS_SHOT_THEME="$theme" BEADS_SHOT_SUFFIX="$suffix" \
    BEADS_SHOT_VIEWS="$views" BEADS_SHOT_OUTDIR="$outdir" \
      timeout "$WATCHDOG" "$BEADS_SHOT_ENGINE" \
      || log "engine exited non-zero for theme $theme"
  else
    BEADS_SHOT_THEME="$theme" BEADS_SHOT_SUFFIX="$suffix" \
    BEADS_SHOT_VIEWS="$views" BEADS_SHOT_OUTDIR="$outdir" \
      timeout "$WATCHDOG" emacs -Q -l "$SHOTS_EL" 2>>"$outdir/emacs.log" \
      || log "emacs exited non-zero for theme $theme (see $outdir/emacs.log)"
  fi
}

capture_into() {
  local outdir="$1"
  mkdir -p "$outdir"
  : > "$outdir/capture.log"
  local line theme suffix views filtered v
  local IFS=$' \t\n'
  while IFS= read -r line; do
    [ -z "${line// }" ] && continue
    # shellcheck disable=SC2086
    set -- $line
    theme="$1"; suffix="$2"; shift 2
    views="$*"
    filtered=""
    for v in $views; do
      if view_selected "$v"; then filtered="$filtered $v"; fi
    done
    [ -z "${filtered// }" ] && continue
    log "theme=$theme suffix=$suffix views=[$filtered]"
    run_engine "$theme" "$suffix" "$filtered" "$outdir"
  done <<< "$MANIFEST"
  log "raw captures:"; ls -l "$outdir"/*.png 2>/dev/null \
    | awk '{print "  "$5, $NF}' >&2 || true
}

# ---- Compress ---------------------------------------------------------
compress() {
  local srcdir="$1" total=0 raw name full thumb fs ts
  mkdir -p "$IMAGES_DIR"
  shopt -s nullglob
  for raw in "$srcdir"/*.png; do
    name="$(basename "$raw" .png)"
    # A subset run must not rewrite an image outside the selected scope.
    local selected=0 v
    for v in "${SELECTED_VIEWS[@]}"; do
      case "$name" in "$v"-dark|"$v"-light) selected=1; break ;; esac
    done
    [ "$selected" = 1 ] || continue
    full="$IMAGES_DIR/$name.png" thumb="$IMAGES_DIR/$name-thumb.png"
    convert "$raw" -strip -resize "${FULL_W}x>" -dither None -colors "$FULL_COLORS" \
            -define png:compression-level=9 "$full"
    convert "$raw" -strip -resize "${THUMB_W}x>" -dither None -colors "$THUMB_COLORS" \
            -define png:compression-level=9 "$thumb"
    fs=$(stat -c%s "$full"); ts=$(stat -c%s "$thumb")
    total=$((total + fs + ts))
    printf '  %-26s full=%6d  thumb=%6d\n' "$name" "$fs" "$ts" >&2
  done
  shopt -u nullglob
  log "docs/manual/images total: $(awk "BEGIN{printf \"%.1f\", $total/1024}") KiB"
}

# ---- Verify -----------------------------------------------------------
verify_runs() {
  local a="$1" b="$2" name fa fb drift=0 count=0
  local afiles bfiles
  shopt -s nullglob
  afiles=("$a"/*.png)
  bfiles=("$b"/*.png)
  shopt -u nullglob
  if [ "${#afiles[@]}" -ne "${#bfiles[@]}" ]; then
    log "verify: DRIFT image count differs: run1=${#afiles[@]} run2=${#bfiles[@]}"
    drift=1
  fi
  for fa in "${afiles[@]:-}"; do
    [ -e "$fa" ] || continue
    name="$(basename "$fa")"
    fb="$b/$name"
    if [ ! -f "$fb" ]; then
      log "verify: DRIFT missing in second run: $name"; drift=1; continue
    fi
    if cmp -s "$fa" "$fb"; then count=$((count+1)); else log "verify: DRIFT $name"; drift=1; fi
  done
  for fb in "${bfiles[@]:-}"; do
    [ -e "$fb" ] || continue
    name="$(basename "$fb")"
    [ -f "$a/$name" ] || { log "verify: DRIFT extra in second run: $name"; drift=1; }
  done
  if [ "$drift" -ne 0 ]; then
    printf 'capture.sh: verify FAILED for scope %s; drifting image(s) above\n' "$SCOPE" >&2
    exit 1
  fi
  [ "$count" -gt 0 ] || { printf 'capture.sh: verify: no images captured for scope %s\n' "$SCOPE" >&2; exit 1; }
  log "verify: byte-identical for $count image(s) (scope $SCOPE)"
}

# ---- Argument parsing -------------------------------------------------
SCOPE="all"
VERIFY=0
COMPRESS_ONLY=0
POSITIONAL=""

while [ $# -gt 0 ]; do
  case "$1" in
    --scope)
      [ $# -ge 2 ] || usage_error "--scope requires an argument"
      SCOPE="$2"; shift 2 ;;
    --scope=*)
      SCOPE="${1#--scope=}"; shift ;;
    --verify)
      VERIFY=1; shift ;;
    --compress)
      COMPRESS_ONLY=1; shift ;;
    --help|-h)
      usage; exit 0 ;;
    -*)
      usage_error "unknown argument: $1" ;;
    *)
      [ -z "$POSITIONAL" ] || usage_error "unexpected extra argument: $1"
      POSITIONAL="$1"; shift ;;
  esac
done

# `capture.sh --verify all` and `capture.sh --verify --scope all` both work.
if [ -n "$POSITIONAL" ]; then
  [ "$SCOPE" = all ] || usage_error "scope given twice: $SCOPE and $POSITIONAL"
  SCOPE="$POSITIONAL"
fi

resolve_scope "$SCOPE"

mkdir -p "$STAGING"

if [ "$COMPRESS_ONLY" = 1 ]; then
  compress "$STAGING"
  log "done."
  exit 0
fi

ensure_project

if [ -n "${BEADS_SHOT_ENGINE:-}" ]; then
  log "using BEADS_SHOT_ENGINE=$BEADS_SHOT_ENGINE; skipping the compositor"
else
  start_sway
fi

if [ "$VERIFY" = 1 ]; then
  rm -rf "$STAGING/verify-a" "$STAGING/verify-b"
  capture_into "$STAGING/verify-a"
  capture_into "$STAGING/verify-b"
  verify_runs "$STAGING/verify-a" "$STAGING/verify-b"
else
  rm -rf "$STAGING"/run-*.png 2>/dev/null || true
  mkdir -p "$STAGING/run"
  capture_into "$STAGING/run"
  compress "$STAGING/run"
fi

log "done."
