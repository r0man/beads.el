---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-5nvl
  formula: do-work
methodology:
  pack: gascity
  name: build-basic
producer:
  formula: do-work
  stage: implement
  attempt: 1
status: approved
trace:
  upstream:
    - path: beads/be-zy8r
      hash: bead:be-zy8r
      ids:
        - REQ-SF-050
        - REQ-SF-051
        - REQ-SF-052
    - path: lisp/beads-bond.el
      hash: sha256:5783a0e8ae050199f1035991f369ec42a0cdbb6b89f206341d7f9417d7e43b16
    - path: lisp/test/beads-bond-test.el
      hash: sha256:48c47f0ccf9e74ee19bb85a446e8517c7b40cabe3d08a613bf78b0995a1c84ef
    - path: lisp/beads-command-mol.el
      hash: sha256:780fc257325b7ac484a59c399d1aa6e0cae14216a0c6ff8967175ef7b7c74b1f
  coverage:
    - id: REQ-SF-050
      status: covered
    - id: REQ-SF-051
      status: covered
    - id: REQ-SF-052
      status: covered
---

## Summary

Implemented WI-SF-08, the bond flow (design.md §4.1/§4.2/§5.3/§7.2/§9).
`lisp/beads-bond.el` adds the `beads-bond` transient, the
`beads-bond-operand-reader` completion seam, the `beads-bond-command`
builder over `beads-command-mol-bond`, a read-only dry-run preview, and
the bond-point attachment seam. `lisp/test/beads-bond-test.el` covers the
flow with 27 unit tests plus one end-to-end integration test that dry-runs
and then applies a real `bd mol bond`.

While wiring the flow I found that `beads-command-mol-bond` advertised
`:choices ("seq" "par" "gate")`, values the live `bd` binary rejects
("invalid bond type 'seq', must be: sequential, parallel, or
conditional"); the base command validator enforced those choices, so a
valid bond command could never be built. Corrected the metadata to the
real `sequential`/`parallel`/`conditional` values (REQ-SF-050).

Coverage:

| ID | Status |
| --- | --- |
| REQ-SF-050 | covered |
| REQ-SF-051 | covered |
| REQ-SF-052 | covered |

## Intended Behavior

- `M-x beads-bond` (and `beads-bond-for` for callers) opens an adaptive
  `beads-prefix` transient. `A`/`B` pick the two operands over a
  provider hook (`beads-bond-operand-functions`) whose default lists
  every formula name and every molecule id; unmatched ids are accepted
  free-text so uncookable protos stay reachable.
- `t` cycles sequential/parallel/conditional, `p` cycles
  follow/pour/ephemeral, `a` sets the proto+proto result name, `r` sets
  the dynamic ref, `v` accumulates `key=value` vars, and `o` targets a
  declared `compose.bond_points` attachment site.
- `P` opens a read-only preview that explains the operand relation and
  shows the real `bd mol bond --dry-run` output; the preview never gates
  `s`. `s` applies the JSON bond and reports `result_id`/`result_type`.
- A named bond point is recorded and shown, and its `parallel` flag
  seeds the bond type. `bd mol bond` has no attachment-site flag, so the
  client-side targeting is the maximum the CLI allows; this is stated in
  the module commentary and the preview.
- The flow reads bond points from the typed `beads-formula` slot when
  WI-SF-05 provides it, and degrades to nil (no signal) before then, so
  the module is independently loadable.

## Changed Files

- `lisp/beads-bond.el` (new): the bond flow, preview and bond-point
  attachment seam.
- `lisp/test/beads-bond-test.el` (new): 27 unit tests (command assembly,
  phase/ref/vars, validation, operand completion hook, bond points,
  labels/preview, cycling, entry scope, run, transient layout) plus one
  `:integration` test that cooks two formulas then dry-runs and applies
  the real `bd mol bond`.
- `lisp/beads-command-mol.el` (2-line metadata fix): correct the
  `beads-command-mol-bond` `:choices`/prompt from the invalid
  `seq`/`par`/`gate` to the live `sequential`/`parallel`/`conditional`,
  unblocking REQ-SF-050. This is a required integration fix, not a slot
  or serialization change.

## Verification

First verification command — the source anchor's stated expectation:

```
eldev test -f beads-bond-test.el
```

Result: pass. `Ran 28 tests, 28 results as expected, 0 unexpected` (27
unit + 1 integration against a real `bd mol bond`). The integration test
writes two `.formula.toml` files, cooks both protos, asserts the dry-run
text, then applies the bond and asserts `result_type=compound_proto` and
a `result_id`.

Supplementary checks (all pass):

```
eldev -p -dtT compile lisp/beads-bond.el      # no warnings
eldev -p -dtT lint                            # beads-bond.el clean
eldev test -f beads-command-mol-test.el       # 24/24, choices fix safe
```

Final proof command — the build-artifact gate, run from the launcher rig
root (`gc.work_dir` absent on the root, so the installed check under
`/home/roman/workspace/beads.el/.gc/scripts/checks` is the durable root):

```
GC_BEAD_ID=be-wrrc /home/roman/workspace/beads.el/.gc/scripts/checks/build-artifact-valid.sh
```

Result: pass, `build artifact valid: schema=gc.build.implementation-summary.v1
path=/home/roman/workspace/beads.el/worktrees/be-zy8r/plans/beads-standalone-formulas/task-be-wrrc-summary.md`.

## Remaining Risks

- **Bond-point targeting is advisory.** `bd` 1.3.1 exposes no
  attachment-site flag on `mol bond`; the flow records the target bond
  point and seeds the bond type from its `parallel` flag, but cannot
  place the bond at the named step server-side. If upstream `bd` adds a
  flag, `beads-bond-command` is the single place to extend.
- **Inline formula cooking.** On `bd` 1.3.1 `bd mol bond formulaA
  formulaB` errors unless both protos are already cooked
  (`linking proto A: ... not found`). The flow passes operands through
  verbatim; the integration test cooks first to stay version-robust.
  This is a `bd` behavior, not a beads.el defect.
- **WI-SF-05 dependency.** Bond-point enumeration depends on the typed
  `bond-points` slot added to `beads-formula` by WI-SF-05. Until it
  lands the picker falls back to free text; once it lands the accessor
  picks it up without further changes.
- **Formula-detail/molecule wiring.** `beads-bond-for` is the seam for
  the formula-detail attachment key and the molecule-view `b` prefill;
  those key bindings are owned by WI-SF-05/WI-SF-02/WI-SF-12. Live tmux
  acceptance of the full interactive flow is aggregated by WI-SF-13; the
  transient has been parse-verified here (`:transient` test) and the
  command path end-to-end verified against real `bd`.
