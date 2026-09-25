# Requirements: project-switch-scope

- **Schema:** `gc.build.requirements.v1`
- **Scope bead:** `gc-8kg0` (project-switch-scope)
- **Workflow root:** `gc-8kgz` (beads.el CI Phase 1: scope-based orchestration)
- **Author:** gc.requirements-planner-1 (auto-planned)
- **Date:** 2026-09-25
- **Problem statement:** The beads.el Gas City workflow templates currently hardcode an implicit
  "one repository / one plan directory" assumption. Every template role (requirements planning,
  decomposition, implementation, review, verification) reads and writes plan documents under a
  fixed root (`plans/<workflow-root>/…`), and the repository checkout itself is shared across
  scopes. This works for a single workflow at a time but breaks down when:

  1. Multiple workflow roots (scopes) run concurrently against the same repository — their plan
     directories and metadata keys can collide.
  2. A scope's work must happen in a *different* repository or sub-checkout than the one where
     `gc` orchestrates (e.g. the `beads.el` repo orchestrating CI work in another clone).

  The **project-switch-scope** scope introduces explicit, scope-scoped project targeting so each
  workflow can declare *which project (repository + plan directory root)* its roles operate on,
  with safe defaults preserving today's behavior.

- **W6H:**
  - *Who:* Maintainers and contributor-tooling users of beads.el (elisp) and its Gas City
    `graph.v2` role workers; automated workflow sessions that execute template roles.
  - *What:* Add a `gc.var.project` (and per-bead `gc.project_id` metadata) mechanism that role
    workers resolve at claim time to (a) a repository working directory and (b) a plan directory
    root, then use for all reads/writes during that bead's execution. Include an explicit
    "switch project" step pattern that restores the orchestrator's own project afterwards.
  - *When:* Phase 1 of the beads.el CI rollout, immediately after workflow-root/session plumbing
    lands and before templates rely on it for multi-repo CI runs.
  - *Where:* Elisp code under `lisp/` (role-worker helpers, template expansion), template
    definitions under `templates/` (or the equivalent distributed template store), validation
    scripts under `.gc/scripts/checks/`, and documentation under `docs/`.
  - *Why:* Without scope-scoped project targeting, concurrent scopes corrupt each other's plan
    directories, and any cross-repository workflow requires manual, error-prone coordination.
  - *How:* Backward-compatible opt-in: unset/empty `gc.project` falls back to the current single-
    project behavior; set `gc.project` causes claim-time resolution, path derivation, and
    metadata stamping, with validation and clear failure messages.

- **User stories:**
  - As a *workflow author*, I define `gc.var.project=ci-cd` on my scope root so every downstream
    role automatically reads/writes `plans/ci-cd/**` without template edits.
  - As a *role worker session*, when I claim a bead carrying `gc.project_id`, I resolve the
    project's repository directory and plan root once at claim time and record them in my session
    environment, so I never guess paths mid-task.
  - As a *workflow author orchestrating across repositories*, I mark a step bead as a project
    switch (e.g. `gc.project_switch=checkout-a`) so implementation work runs in that checkout, and
    a paired restore step returns the orchestrator to its home project before teardown.
  - As a *maintainer reviewing a failed run*, I read `gc.failure_class=project_unresolved` with a
    message naming the offending bead and missing project definition, so I can fix the scope in
    one edit.
  - As a *contributor with an old workflow*, I run an existing single-project scope unchanged
    (no `gc.project` set) and observe identical behavior to today.

- **Technical stories:**
  - *T1 — Project definition & resolution:* A project is defined by scope metadata
    (`gc.project.<id>.repo_path`, `gc.project.<id>.plans_path`, optional
    `gc.project.<id>.display_name`). Resolution is deterministic: home project (when unset) →
    orchestrator checkout + `plans/`; named project → the scope's declared repo_path/plans_path.
    Unresolvable (missing declaration, non-existent path) is an error, not a silent fallback.
  - *T2 — Claim-time stamping:* When a claimed bead (or its workflow root) specifies a project,
    the claim path exports `GC_PROJECT_ID`, `GC_PROJECT_REPO_PATH`, and `GC_PROJECT_PLANS_PATH`
    and stamps `gc.project_id` on the claimed bead if not already present, so later audit tooling
    can reconstruct which project served which bead.
  - *T3 — Plan path derivation:* All template-generated document paths are derived via the
    resolved plans path (`<plans_path>/<workflow-root>/…`) instead of a hardcoded `plans/`.
    Existing writers/tests switch to the derived root.
  - *T4 — Project-switch step pattern:* A switch step updates the *downstream* beads' effective
    project (via metadata on the step or scope), and a restore step re-asserts the home project.
    Switch/restore steps are ordinary beads with `gc.kind=workflow-control` semantics so they run
    even when earlier work failed.
  - *T5 — Validation:* `.gc/scripts/checks/build-artifact-valid.sh` and any template validators
    accept and verify `gc.project.*` declarations (paths exist-or-planned, ids are slugs), and a
    new check catches two scopes in the same run declaring conflicting definitions for one
    project id.
  - *T6 — Concurrency isolation:* Two scopes may reference the same repository but must own
    distinct plan roots (`plans/<scope-specific-id>/…`); deriving plan root per workflow root
    preserves this. Any shared, mutable cross-scope state is enumerated and guarded.

- **Behavior requirements:**
  - *B1:* Setting `gc.project` (string id) on a workflow root or bead selects the named project
    for all beads in that workflow subtree; per-bead `gc.project_id` overrides the root value.
  - *B2:* With no `gc.project` set anywhere in the chain, behavior is identical to the current
    single-project behavior (home repo + `plans/<workflow-root>/`).
  - *B3:* If a named project cannot be resolved (not declared on the scope, or declared path does
    not exist), the claiming worker fails the bead with `gc.outcome=fail`,
    `gc.failure_class=project_unresolved`, and a reason that names the bead id, project id, and
    the missing declaration or path.
  - *B4:* All plan-document reads and writes by role workers use the resolved
    `GC_PROJECT_PLANS_PATH`; no role worker may hardcode `plans/` after this scope lands.
  - *B5:* A project-switch step changes the effective project only for beads downstream of the
    switch (and within the same workflow root); the restore step returns the effective project to
    the scope's home project before any teardown/finalize bead runs.
  - *B6:* The claimed bead records `gc.project_id` metadata at claim time when a project is
    active, enabling post-hoc audit queries by project.
  - *B7:* Validation scripts fail with a clear, machine-readable error when a project
    declaration is malformed (non-slug id, relative path escaping the repo, duplicate id with
    conflicting paths) and pass when declarations are well-formed.

- **Example mapping:**
  - *Ex.1 (happy path, single named project):* Scope declares `gc.project.ci-cd.repo_path=.`
    and `gc.project.ci-cd.plans_path=plans/`, root sets `gc.project=ci-cd`. A decomposition bead
    claims, resolves `plans_path=plans/`, writes `plans/ci-cd/…`, stamps `gc.project_id=ci-cd`,
    passes. → covers B1, B4, B6.
  - *Ex.2 (default behavior preserved):* Legacy scope with no project metadata claims a role
    bead; it behaves exactly as today (home repo, `plans/<root-id>/`). → covers B2.
  - *Ex.3 (cross-repo switch/restore):* Step A (switch) sets effective project `external-ci`
    (repo_path=`../beads-external-clone`); steps B–C write plan docs and run commands in that
    repo; step D (restore) re-asserts home project; teardown then runs in the home repo. → covers
    B1, B4, B5.
  - *Ex.4 (unresolved project):* Root sets `gc.project=typo-id`; no declaration exists. Claiming
    worker marks the bead failed with `gc.failure_class=project_unresolved` and stops before any
    file writes. → covers B3.
  - *Ex.5 (validation):* `.gc/scripts/checks/build-artifact-valid.sh` is extended (or a sibling
    check added) to validate project declarations in the requirements/decomposition artifacts;
    malformed declarations block the pipeline with a named error. → covers B7.
  - *Edge (concurrent scopes):* Two workflow roots, each with `gc.project=ci-cd` but distinct
    workflow-root ids, both derive plans roots `<plans_path>/<their-root-id>/` and never write
    into each other's directories. → covers B4, T6.

- **Acceptance criteria:**
  - *AC1:* A bead with an active named project exports `GC_PROJECT_ID`, `GC_PROJECT_REPO_PATH`,
    and `GC_PROJECT_PLANS_PATH` at claim time (B1, B6).
  - *AC2:* Beads with no project metadata resolve to the home project and behave identically to
    pre-change runs (B2) — verified by existing tests passing unmodified or with only mechanical
    path-derivation updates.
  - *AC3:* An unresolvable project fails the bead with `gc.outcome=fail`,
    `gc.failure_class=project_unresolved`, and a reason naming bead id, project id, and the
    missing declaration/path (B3).
  - *AC4:* No role worker or template helper hardcodes the literal `plans/` plan root after this
    scope; a grep/lint check (or unit test) enforces derivation from the resolved plans path
    (B4).
  - *AC5:* A switch/restore pair demonstrably changes and restores the effective project within
    one workflow run, including a teardown bead that runs in the home project (B5).
  - *AC6:* Validation tooling rejects malformed project declarations with a machine-readable
    error naming the offending id/field, and accepts well-formed declarations (B7).
  - *AC7:* All new behavior ships with unit tests (elisp, ERT) covering resolution, default
    fallback, failure paths, switch/restore, and plan-path derivation.

- **Out of scope:**
  - Creating or cloning the target repository checkout if it does not exist (declaration must
    point at an existing path; auto-provisioning is future work).
  - Remote/CI-specific runner configuration, secrets management, or matrix expansion.
  - Migrating historical beads/plan directories to a new layout (only future runs are affected).
  - Changes to bead storage format or `bd` CLI semantics beyond metadata conventions used here.
  - Multi-project fanout topologies (one bead fanning out to several projects) — single
    effective project per bead only.

- **Open questions:**
  - *Q1:* Should `gc.project.<id>.*` declarations live on the scope root only, or be allowed on
    intermediate beads? (Default assumption: scope root only; per-bead `gc.project_id` selects.)
  - *Q2:* Exact environment variable names (`GC_PROJECT_ID` / `GC_PROJECT_REPO_PATH` /
    `GC_PROJECT_PLANS_PATH`) — confirm against existing `GC_*` conventions used by the claim
    hook.
  - *Q3:* Whether the switch step should stamp downstream beads eagerly (at decomposition time)
    or lazily (at claim time via workflow-root inheritance). Default assumption: lazy via
    inheritance, matching the claim-time stamping design.
  - *Q4:* Is there an existing shared-state registry that T6 must coordinate with (e.g. locks on
    `plans/`)? To be confirmed during decomposition; none assumed in this artifact.
