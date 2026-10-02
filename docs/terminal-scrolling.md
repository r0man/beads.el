# beads.el — Agent buffers: transparent mouse + Emacs scrolling

Status: **implemented** in `lisp/beads-terminal-tmux.el` (moved into
beads.el from `gascity.el`, WI-14/WI-15).  Every load-bearing assumption
below was validated by the live experiments E1–E9 recorded in
`gascity.el/docs/qa/2026-09-29-agent-scroll-mouse-experiments.md`; those
references survive the move because they are historical evidence, not
beads.el code.

## 1. Problem

An agent buffer embeds the agent's session through a tmux client
(`beads-terminal-tmux-attach`).  To read the transcript the user would
otherwise have to press `C-b [`, drive tmux copy-mode with tmux keys, and
exit with `q`.  That is not Emacs, and the mouse does nothing on most
backends.

## 2. How it works

- **Renderers.** An attach buffer is a terminal-emulator buffer created by
  `beads-terminal-spawn` through `beads-terminal-tmux-run`.  Backends:
  ghostel (priority 5), vterm (10), eat (20), ansi-term (40), term (50);
  `beads-terminal-tmux--backend-class` maps `beads-terminal-tmux-backend`,
  `auto` → first available.
- **Embedding.** The argv is `env -u TMUX tmux [-L SOCKET] attach-session
  -t SESSION` (`beads-terminal-tmux--attach-argv`), wrapped into a local
  `ssh -t HOST …` for remote stores (`beads-remote-ssh-argv`).  The tmux
  client is a local process in every case; keys and mouse bytes travel
  over the local pty, so there is no TRAMP in the input path at all.
- **Keymaps.** `beads-terminal-tmux-attach-map` adds only `C-c b` (bead at
  point) and `C-c s` (scroll toggle) via an `emulation-mode-map-alists`
  entry keyed on `beads-terminal-tmux--attach-keys` — deliberately above
  local/minor maps so it survives vterm/ghostel copy modes.  Everything
  else belongs to the pty.  `beads-terminal-tmux--unshadow-keys` +
  `beads-terminal-tmux-unshadow-minor-modes` (default
  `(pixel-scroll-precision-mode)`) neutralise global minor-mode keymaps
  (e.g. pixel-scroll's PageUp/PageDown) in terminal buffers, because they
  would scroll a screen-only buffer for nothing (E2).
- **tmux configuration beads touches.** Only the session-scoped options in
  the attach pre-step `beads-terminal-tmux--attach-script`: `status off`
  (mirrored into the mode line by `beads-terminal-tmux--status-install`),
  and, when `beads-terminal-tmux-ensure-mouse` is on, `mouse on` plus the
  D3 wheel-exit binding.  beads sets **no** global option; `mouse on`
  comes from the user's `~/.tmux.conf` otherwise (E4), `mode-keys emacs`
  is the tmux default.  Sessions live on per-store sockets passed in via
  the attach `SOCKET` argument (`gascity.el` supplies a per-city socket).

## 3. Option analysis

| # | Option | How it works | What breaks | Effort |
|---|---|---|---|---|
| a | tmux `mouse on` + copy-mode integration | Wheel over the pane → tmux `WheelUpPane/Down` bindings scroll scrollback (copy mode entered/left automatically at tmux's discretion) | Needs a mouse-reporting backend (ghostel/eat); vterm/term users get nothing (E3). Needs `mouse on` (E4). When the agent TUI claims mouse the wheel is a pass-through — correct, not a break (E5) | None–small (ensure the option) |
| b | Terminal mouse passthrough vs Emacs intercept | Same as (a) viewed from Emacs: forward mouse bytes vs intercept wheel events | Intercepting wheel in Emacs and translating to tmux keys is the only way to help vterm/term users; per-backend wheel handling must not break eat/ghostel's own passthrough (E5) | Small (one wheel translator) |
| c | Alternate-screen handling (copy-mode vs scrollback) | The tmux client is always on the alt screen (E2); the Emacs buffer has no scrollback worth scrolling | Any design that scrolls the *Emacs buffer* is dead for attach buffers. Scrolling must happen *inside* tmux (copy mode) | — (rules out alternatives) |
| d | Dedicated scroll sub-mode with Emacs keys | A buffer-local minor mode: first `C-b [` enters copy mode, then Emacs keys are translated to tmux copy-mode bytes; `q`/Esc leave both | Desync when the user leaves copy mode another way (mitigated: the toggle key re-syncs; `q`/Esc are also translated). Collides with nothing while active — the agent sees no keys (E9) | Medium |
| e | What the backend libraries already offer | ghostel: real scrollback + wheel intercept — useless for attach (E2, alt screen); eat: `eat-enable-mouse` passthrough; vterm: none; term: none. Raw-key APIs exist everywhere: `vterm-send-string`, `term-send-raw-string`, `eat-self-input`, `ghostel-send-key`/`-send-string` (control bytes need `-send-key` in semi-char mode, E6) | — | — |

Rejected: driving tmux scrollback via side-channel commands
(`tmux send-keys -X scroll-up` per wheel notch through
`beads-terminal-tmux--run-async`) — one process per notch, latency and
process churn; also rejected: `cat | less`-style re-attach through
non-alt-screen clients — fights the tmux client contract (E2) and the
status mirror.

## 4. Design

Two layers, both inside the attach buffer, no global rebinding.

### D1 — `beads-terminal-tmux-scroll-mode` (the keyboard layer, all backends)

A buffer-local minor mode for attach buffers, toggled per buffer:

- **Enter:** `C-c s` (in `beads-terminal-tmux-attach-map`; no §10
  collision — the attach map owns only `C-c` keys).  It sends
  `C-b` `[` (copy mode on) and activates.
- **Active keys → byte translations** (`beads-terminal-tmux--scroll-sequence`,
  E6/E7):

  | Emacs key | Sent bytes | tmux effect |
  |---|---|---|
  | `C-p` / `C-n` | `\e[1;5A` / `\e[1;5B` (C-Up/C-Down) | scroll ±1 line (viewport) |
  | `C-v` / `M-v` | `\e[6~` / `\e[5~` | page down/up |
  | `PageDown` / `PageUp` | `\e[6~` / `\e[5~` | page down/up |
  | `M-<` / `M->` | `\e<` / `\e>` | jump to top/bottom (tmux `history-top`/`history-bottom`) |
  | `q`, `Esc` | `q` / `\e` | leave copy mode; mode deactivates itself |

  The mode keys are exactly the keys a Magit-style user expects; inside
  the mode the agent's `C-p`/`C-n` are not reachable (E9) — that is the
  collision resolution: **scrolling is an explicit mode, not a shadowing
  of live keys.**
- **Raw-key adapter** (one function per backend, selected from the
  buffer's major mode — E6, `beads-terminal-tmux--scroll-backend`):
  - vterm → `vterm-send-string`
  - term/ansi-term → `term-send-raw-string`
  - eat → `eat-self-input`
  - ghostel → `ghostel-send-key` for control bytes,
    `ghostel-send-string` for escape sequences
  - a backend with no adapter deactivates the mode with an echo-area
    message instead of erroring.
- **State sync (optimistic + self-healing).** Mode activation assumes
  copy mode on.  Each translation is a plain byte send; if the user left
  copy mode out-of-band (e.g. mouse wheel-down after the D3 binding), the
  next translated key shows up in the agent's editor — recoverable by
  re-toggling (`C-c s` sends `q` first when it thinks it is active).
  No async `pane_in_mode` resync is needed for v1.
- **Indication.** The mode line already carries the status mirror; the
  mirrored string gains a `[scroll]` marker while the mode is active.

### D2 — transparent mouse (the wheel layer)

- **Mouse-reporting backends (ghostel/eat):** nothing to implement —
  wheel already scrolls the transcript through tmux's own bindings (E4),
  and is correctly a pass-through when the agent claims mouse (E5).
  beads only **ensures the precondition**: the attach pre-step
  (`beads-terminal-tmux--attach-script`, one host round trip it already
  makes) additionally runs `set-option -t SESSION mouse on` when
  `show-options -g mouse` reports `off` — session-scoped, undone by the
  same teardown that restores `status` (kill-buffer hook,
  `beads-terminal-tmux--status-teardown`).  Gate:
  `beads-terminal-tmux-ensure-mouse` (default t).
- **Non-reporting backends (vterm/term):** `beads-terminal-tmux-wheel-mode`
  is armed on attach (`beads-terminal-tmux--arm-wheel`), so a wheel notch
  works out of the box with no `C-c s`: it injects the SGR mouse event
  tmux would have received (`beads-terminal-tmux--wheel-mouse-sequence`),
  and tmux's own copy-mode handling runs.  When
  `beads-terminal-tmux-scroll-mode` is active the wheel instead keeps the
  D2 key translation — the first notch after (re-)entry re-sends the
  copy-mode entry bytes, then a run of
  `beads-terminal-tmux--scroll-wheel-notch` C-Up/C-Downs (≈10 lines,
  matching tmux's `-N 5` feel).  These bindings live in the scroll/wheel
  maps, so `C-c s` upgrades a vterm attach to full mouse + key scrolling
  with no tmux-side dependencies.
- ghostel/eat never get the wheel mode: their native tmux passthrough must
  not be double-driven (E5).

### D3 — wheel-down exits copy mode (tmux binding patch, all backends)

tmux 3.7c does not leave copy mode when wheeling to the bottom (E8) — the
transcript would never "snap back to live".  The attach pre-step installs
one session-scoped binding alongside `status off`:

```
bind -T copy-mode WheelDownPane select-pane \
  \; if -F '#{==:#{scroll_position},0}' 'send -X cancel' 'send -X -N 5 scroll-down'
```

and teardown unbinds it (like the `set-option -u status` restore), gated
by the same `beads-terminal-tmux-ensure-mouse` switch.

### Interaction with existing conventions

- New keys: `C-c b` and `C-c s` in `beads-terminal-tmux-attach-map` only
  (an attach buffer's keys belong to the pty; the `C-c` prefix is the
  established exception).  No design §10 conflicts; dashboards
  (`t`/`RET` attach) are unchanged.  `S` remains sling.
- Non-blocking: every tmux change rides the pre-step's existing single
  host round trip; no new sync gc/tmux call.
- Remote stores: zero extra work — the terminal is a local client in both
  shapes (§2); bytes travel over the local ssh pty.  The pre-step changes
  run through `beads-terminal-tmux--run-async` for both local and remote,
  already the case.
- The status mirror, project pinning, eldoc wiring are untouched.
- beads.el stays free of gascity: no `gascity-` reference exists in the
  moved code; the only gascity-specific parameter is the tmux **socket**,
  passed to `beads-terminal-tmux-attach`.

## 5. Acceptance criteria

1. **Mouse for everyone who can have it.** Pre-step ensures `mouse on` +
   installs the D3 wheel-exit binding; teardown restores.  *Accept:*
   attach to bright-lights agent; wheel scrolls; wheeling to bottom
   returns to live tail; external `tmux attach` sees default bindings
   after buffer kill; all over TRAMP.
2. **Scroll sub-mode.** `beads-terminal-tmux-scroll-mode` with the D1
   translation table + per-backend raw-key adapter + mode-line marker.
   *Accept:* on ghostel and vterm (and term), `C-c s` then
   `C-p`/`C-n`/`C-v`/`M-v`/`PageUp`/`q` drive copy mode exactly as the E6
   table; the agent never receives a key while the mode is active;
   `C-c s` re-syncs after an out-of-band copy-mode exit.
3. **Wheel translation for non-reporting backends.** The wheel mode and
   the scroll-mode wheel extension.  *Accept:* vterm attach, mouse
   scrolls the transcript without any toggle; eat/ghostel behaviour
   unchanged (their native passthrough wins).

## 6. Test plan

- **ERT (pure, `cl-letf` stubs), `lisp/test/beads-terminal-tmux-test.el`:**
  - translation table: `beads-terminal-tmux--scroll-sequence` is a pure
    function of an Emacs key — table-driven tests;
  - raw-key adapter dispatch per backend major mode;
  - `beads-terminal-tmux--attach-script` emits the `mouse on` /
    `bind WheelDownPane` fragments when the option is on (string
    assertions), and teardown restores them (`-u` fragments);
  - the wheel-mode arm/no-arm rule per backend
    (`beads-terminal-tmux--backend-reports-mouse-p`);
  - no new store/async verbs, so the non-blocking verb guard needs no
    additions (the modes send raw bytes, spawn nothing).
- **Live e2e (the acceptance gate; harness rules: timeouts, no unbounded
  loops):** fresh GUI Emacs in tmux, attach a bright-lights agent over
  `/ssh:localhost:…`, drive `C-c s`, wheel events (synthetic or real),
  assert host-side `pane_in_mode`/`scroll_position` after each step —
  exactly the E4–E8 probes; record under `docs/qa/`.
