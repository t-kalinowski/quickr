# Refactor Plan (quickr)

## Setup / ordering (keep implicit alphabetical collation)
- [x] **Ensure early handler registry initialization in `R/aaa-utils.R`.** Because we are adding new handler files (`r2f-assign`, `r2f-matrix-*`) that sort before `r2f.R` in alphabetical load order, the `r2f_handlers` environment (and `register_r2f_handler`) must exist before those files load. I will move just the handler registry initialization and `register_r2f_handler()` into `R/r2f-aaa-registry.R.` Also, keep `R/aaa-utils.R` (which you want sourced first), so files can safely register handlers without introducing an explicit Collate field. This preserves implicit alphabetical loading while preventing load-time errors.

## Quick wins (low risk, clarity)
- [x] **Deduplicate `%error%` into `R/aaa-utils.R`.** This removes the duplicate definition from `R/classes.R`, leaving a single utility implementation in `aaa-utils.R` as requested. The goal is to reduce duplicated behavior and future drift while keeping all shared utils in the first-loaded file.
- [x] **Make handler registration table-driven.** I’ll extend `register_r2f_handler()` to accept metadata (`dest_supported`, `dest_infer`, `match_fun`) and update call sites to pass these explicitly instead of setting attributes manually. This centralizes handler configuration in one place and makes handler capabilities easier to scan and change consistently.
- [x] **Collapse repetitive unary intrinsic registrations.** I’ll introduce a small helper (e.g., `register_unary_intrinsic`) to register the common “one-arg, same-dims” handlers (sin/cos/exp/etc.), and use it for the complex intrinsics too. This keeps the list of supported intrinsics readable while removing boilerplate that currently obscures behavior.

## Medium restructure (behavior preserved, easier to reason about)
- [x] **Split assignment handling into `R/r2f-assign.R`.** The monolithic `<-` handler will be broken into focused helpers (dispatch to `<-`/`<<-` targets, fall-through assignment, local closure assignments, sapply assignments, and the generic dest-inference path). The handler itself will become a short orchestration function. This reduces branching and makes each path easier to test and reason about without changing semantics.
- [x] **Unify local-closure lowering.** `compile_closure_call()` and `compile_closure_call_assignment()` share match-call and argument-prep logic. I’ll extract a shared `match_closure_call_args()` and a `compile_local_closure_proc()` wrapper so both call sites go through the same path. This removes duplication and ensures any future fixes to closure matching or argument conversion apply consistently.
- [x] **Split matrix helpers into `R/r2f-matrix-parse.R`, `R/r2f-matrix-infer.R`, `R/r2f-matrix-blas.R`.** I’ll move parsing helpers (dims, transpose handling, conformability checking), inference helpers (dest-shape inference), and BLAS emission helpers into separate files with the requested prefix. `R/sub-r2f-matrix.R` will keep only the public matrix r2f handlers and the wiring between them. This makes the matrix pipeline more modular and reduces cognitive load when changing either parsing or BLAS emission.
- [x] **Consolidate conformability checks into `check_conformable()`.** I’ll replace `assert_conformable()` with a two-phase helper that returns `{ok, unknown}` and a separate warning helper. Each matrix op will then decide whether to throw or warn based on that result. This avoids diverging warning/error behavior and makes the rationale for warnings explicit while preserving existing user-visible error messages.

## Bloat / dead-ends (cleanup only)
- [x] **Remove unimplemented stubs.** I’ll delete `r2f_slice()` (empty), `substitute_unique_case_insensitive_symbols()` (hard stop), and the partial `cbind` handler. These are currently dead ends with no usage and no tests, and keeping them in core files adds noise without functional value.

## Validation (per project instructions)
- [ ] **Format:** run `air format .` to keep formatting consistent after moves and edits.
- [ ] **Tests:** run full test suite `R -q -e 'devtools::test()'` (no skips, no disables).
- [ ] **Checks:** run `R -q -e 'rcmdcheck::rcmdcheck(error_on = "warning")'` before finishing.
