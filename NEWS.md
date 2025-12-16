# quickr (development version)

- On macOS, quickr will use LLVM flang (`flang-new`) for compilation when
  available (e.g. `brew install flang`). This is optional and can be disabled
  with `options(quickr.prefer_flang = FALSE)`.

- Added nested compilation scopes with local declaration emission, enabling
  block-scoped temporaries (Fortran 2008 `block ... end block`) and local
  closures (lowered to internal procedures under `contains`), including `sapply()`
  lowering with host association for captures and support for `simplify = "array"` for
  higher-rank outputs. Examples:

  - Block-scoped temporary for expression subscripting:

    ```r
    out[1] <- ifelse((x > 0.0)[2, 3], 1.0, 0.0)
    ```

  - Local closure + `sapply()` lowering:

    ```r
    f <- function(i) x[i] + 1.0
    out <- sapply(seq_along(out), f)
    ```

  This lowering matches R’s “copy-on-modify" semantics: modifications to the variables from the parent scope trigger a copy in the local closure scope.

- Added support for superassignment (`<<-`) inside local closures (lowered to internal procedures) to explicitly mutate variables in the enclosing quick function scope, including `x[i] <<- ...` subset targets. Example:

  ```r
  apply_boundary_conditions <- function() {
    temp[1, ] <<- 0
    temp[nx, ] <<- 0
    temp[, 1] <<- 0
    temp[, ny] <<- 0
    NULL
  }
  apply_boundary_conditions()
  ```

- Added support for `array(data=..., dim=...)` for shape construction in
  user code and tests.

- Added extensive tests + snapshots for local closures, `sapply()`,
  block-scoped temporaries, and package compilation behavior (internal procedure
  name collisions).

- Lowered the minimum supported R version to 4.3.0 and added a backport of
  `declare()` for R < 4.4.0 (#67).

- Fixed `[` subsetting scalars with both `drop=TRUE` and `drop=FALSE`, simplifying
  to Fortran scalars so subsetted scalars in reductions (e.g., `min(m[1, 1], m[2, 1])`)
  no longer emit `minval` on scalars (#64).

- Fixed an issue where subsetting logical arrays could fail when compiling quick
  functions, e.g. `(x > 0)[2, 3]` (#68).

# quickr 0.2.1

- Added support for `!` and unary `-` and `+` (#49, @mns-nordicals)

- Functions can now return multiple arrays in a `list()`, optionally
  named (#49, @mns-nordicals).

- Updates for changes in the R-devel C API (#61)

# quickr 0.2.0

- Internal utility `r2f()` print method now shows the generated `c_bridge`
  for translated subroutines.

- Added support for `nrow()`, `ncol()` and `dim()` (#21, @mikmart).

- Added support for `runif()` with integration to R's RNG (#22, #45).

- Added support for `while`, `repeat`, `break`, `next`.

- Added support for `%%` and `%/%`.

- Added support for expression return values.

- Fix passing a scalar (rank-0) arg to reduction intrinsics
  (min, max, prod, sum).

- Fixed an issue where `/` might perform integer division if one of the operands
  is an integer type (#33, #41).

- Fixed an issue with dll symbol registration when
  `quick()` is used in a package (#19).

- Fixed segfault encountered on Windows with variable sized arrays.

- Added workaround for cases where the compiler error message might not
  display correctly in RStudio.

- Improved error message when using case-sensitive variable names (#18, #36, #39)

- Added `AGENTS.md` and `scripts/setup_codex.sh` to enable the ChatGPT/Codex agent
  to run tests in a docker container configured without internet access.


# quickr 0.1.0

* Initial CRAN submission.
