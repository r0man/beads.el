# Ralph live smoke tests

These four scenarios exercise the full Ralph loop against a real
`claude` binary and a real bd repo.  They are **not** part of the
`eldev test` suite: each one spawns `claude --print` (which charges
API credits) and writes to disk.

Run them by hand when validating an integration after a refactor.
Each script exits 0 on PASS, non-zero on FAIL.

## Pre-flight

Before running:

```bash
# Compile clean.
eldev compile

# All non-live tests pass.
eldev test
```

You also need:

- `claude` on PATH, authenticated against an anthropic.com account.
- `bd` on PATH for the test repos.
- A few cents of API budget.

## Scenarios

| Script                        | What it tests                                                      |
|-------------------------------|--------------------------------------------------------------------|
| `01-single-issue.sh`          | One issue, trivial change, sentinel fires, dashboard updates.      |
| `02-epic-chain.sh`            | 3-child epic with deps in chain order; iter rows + mode-line.      |
| `03-cost-guard.sh`            | per-iter budget = $0.01; max-turns = 2; budget and turn caps both. |
| `04-persistence-resume.sh`    | Kill mid-run; reopen; resume; verify counters + replay.            |

Each script:

1. Creates a temp bd repo in `/tmp/ralph-smoke-XXXX/`.
2. Adds canonical issue(s) for the scenario.
3. Launches Emacs in batch with the Ralph loop wired up.
4. Asserts post-conditions against the repo and the JSONL log.
5. Cleans up unless `KEEP_TMP=1` is set.

## Reporting results

When you run a scenario, paste the output into the matching beads issue
(bde-ckby) or attach to a follow-up bug.  The smoke runs cite empirical
TODO outcomes from `lisp/test/ralph-empirical/` -- specifically TODO
#1 (SIGINT result emission) which the `01-single-issue.sh` `stop` path
exercises.
