# Global Options Inheritance Audit

A `bd <cmd> --help` showing a `Global Flags:` section ⇒ the class should inherit `beads-command-global-options`.

## `beads-command-global-options` slot drift

The class itself defines the slots inherited by every command. Comparing against `bd close --help` Global Flags (representative sample):

**Missing slots (CLI advertises, class lacks):**

- `--directory` (string) — Change to this directory before running the command (like git -C)
- `--global` (boolean) — Use the global shared-server database (beads_global)

**Extra slots (class has, CLI no longer advertises):**

- `--no-db` (slot present, not in current bd Global Flags)
- `--no-daemon` (slot present, not in current bd Global Flags)
- `--no-auto-import` (slot present, not in current bd Global Flags)
- `--no-auto-flush` (slot present, not in current bd Global Flags)
- `--lock-timeout` (slot present, not in current bd Global Flags)
- `--allow-stale` (slot present, not in current bd Global Flags)

## Should inherit, doesn't

_None._


## Inherits but shouldn't

_None._


---

## Counts
- Total classes: 216
- Should inherit but doesn't: 0
- Inherits but shouldn't: 0
