# Global Options Inheritance Audit

A `bd <cmd> --help` showing a `Global Flags:` section ⇒ the class should inherit `beads-command-global-options`.

## `beads-command-global-options` slot drift

The class itself defines the slots inherited by every command. Comparing against `bd close --help` Global Flags (representative sample):

**Missing slots (CLI advertises, class lacks):**

- `--ignore-schema-skew` (boolean) — Proceed despite forward schema drift (some queries may fail)

**Extra slots (class has, CLI no longer advertises):**

_None._

## Should inherit, doesn't

_None._


## Inherits but shouldn't

_None._


---

## Counts
- Total classes: 238
- Should inherit but doesn't: 0
- Inherits but shouldn't: 0
