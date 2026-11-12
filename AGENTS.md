# Working on quickr

quickr compiles annotated R functions to Fortran. Users spend time compiling and declare some facts about their data so calls to the resulting function can be fast.

## Design

Keep recursive AST translation understandable one handler at a time. Prefer the existing R AST, `eval()` with controlled environments, and small helpers that name real responsibilities. Add an analysis pass only when a concrete correctness or performance problem needs one.

Preserve convenient declaration syntax: `NA` axes, fixed dimensions, shared dimension names, and supported size expressions. Unknown extent does not mean unknown rank or element type. `NA` in a shape declaration is distinct from missing values in the data.

Within the supported subset and for inputs satisfying declarations, compiled functions should agree with R. Document intentional differences; do not silently turn a compiler limitation into a new public restriction.

Do work at compilation time when possible. Validate input contracts once at the C boundary. Emit checks inside the function only for facts that can change there or cannot be established earlier. Reject contradictions known during translation. Generated code may rely on documented input and indexing preconditions; do not add general runtime bounds checks or data scans without an explicit behavior decision.

When changing emitted code, distinguish compilation cost, wrapper cost per call, and work per loop iteration or array element. Check allocation, copying, and temporary lifetime as well as execution time.

## Implementation

Each handler owns its syntax and translates its operands through the common recursive path. Keep statement emission and temporary lifetime in the statement context. Make changes to enclosing scopes explicit.

Preserve evaluation count and order, stored array extents, binding identity across closures, and copy-on-modify behavior. A dimension expression and an already allocated array's extent are different facts.

Keep straightforward single-use code inline. Extract helpers for a shared invariant or a meaningful responsibility. Keep invariant comments with the implementation when moving code.

Prefer explicit S7 properties over unchecked attributes. Keep compatibility workarounds small and link the upstream issue that makes them necessary.

## Change discipline

Keep a behavior-changing PR to one coherent observable change. Separate mechanical file moves and unrelated formatting from behavior changes.

Start a behavior fix with a failing public regression. For an internal refactor, establish a passing public baseline before editing.

Describe accepted and rejected inputs that change, with a short example. Update `NEWS.md` and the owning user documentation. Record intentional limitations beside the code; distinguish them from bugs awaiting a fix.

Keep source changes proportional to the problem. A review finding should lead to a focused fix or a recorded follow-up, rather than an unrelated expansion of the PR.

## Tests and performance

Prefer tests that call `quick()` and execute the generated function, such as `expect_quick_identical()`. Test output values, dimensions, modes, errors, and relevant side effects. R coverage measures the compiler; it does not establish correctness of generated C or Fortran.

Use narrow generated-code assertions when they protect a stated compilation or performance contract that output-value tests cannot observe, such as avoiding a full-array copy or a guard inside a loop. Avoid incidental temporary names, whitespace, and complete snapshots for these checks.

For performance claims, benchmark the same inputs and toolchain before and after, with compilation separated from warmed execution. Report what was measured and what is only inferred from emitted code.

Do not disable tests to get a pass. Keep compiler and platform requirements in shared test support. Preserve CRAN-enabled compile-and-call smoke coverage.

Regenerate snapshots through testthat, inspect their diffs, and rerun without accepting changes. Do not hand-edit generated snapshots or `README.md`.

## Validation

During development, run the relevant subset:

```sh
R -q -e 'devtools::test(filter = "regex")'
```

After a large refactor, run the full suite:

```sh
R -q -e 'devtools::test()'
```

Before opening a PR:

```sh
air format .
R -q -e 'rcmdcheck::rcmdcheck(error_on = "warning")'
```

Regenerate README changes with:

```sh
R -q -e 'devtools::build_readme()'
```

Report commands actually run and any unavailable compiler or platform coverage. Respect the user's requested stopping point; review does not authorize merge or release.
