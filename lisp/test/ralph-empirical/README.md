# Ralph empirical-TODO harnesses

These six scripts probe `claude` and `bd` CLI behaviours that the
Ralph loop's parser and controller depend on. They are **not** part
of the `eldev test` suite: each one spawns a real `claude` (or `bd`)
process, captures NDJSON, and prints PASS / FAIL with the observed
shape so the implementer can match the parser to reality before
coding the corresponding code path.

Run them by hand when working on the matching parser/controller task.
Each script exits 0 on PASS, non-zero on FAIL. Stdout is the verdict;
captured NDJSON is left in `/tmp/ralph-empirical-*` for inspection.

| Script                          | Tracking task / question                                                                |
|---------------------------------|-----------------------------------------------------------------------------------------|
| `01-sigint-result-event.sh`     | Does `claude --print` emit a final `result` event when SIGINT'd?                        |
| `02-max-turns-exit.sh`          | What exit code / events does `--max-turns N` produce when the limit is hit?            |
| `03-partial-messages-shape.sh`  | Exact `type` field, parent-message id, and block ordering with `--include-partial-messages`. |
| `04-bd-epic-blocker-payload.sh` | Does `bd list --epic <id> --status=open --json` include blocker data inline?            |
| `05-max-budget-exit.sh`         | On `--max-budget-usd` exhaustion, is a clean `result` event emitted?                    |
| `06-cost-schema.sh`             | Field names for cost and duration in the `result` event (plan reads `cost-usd`).        |

Requirements:

- `claude` on PATH (authenticated to anthropic.com).
- `bd` on PATH for harness 4.
- `jq` for JSON inspection.
- A few cents of API budget for harnesses 1, 2, 3, 5, 6.

The scripts use trivial prompts and short turn caps to keep cost low.

## Reporting empirical findings

When you run a harness, paste its output into the matching beads issue
(or this directory's `findings/` if you create one). The controller's
schema decisions in `lisp/beads-agent-ralph.el` should cite the
harness output that justifies each field name and exit-code branch.
