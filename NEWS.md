# quickr (development version)

- Added support for `as.integer()` coercion from `double`, `logical`, and
  `integer` values (with `double` truncating towards 0).

- Added initial matrix/linear algebra handler support from base R, using the
  same BLAS/LAPACK as R: `%*%`, `t()`, `crossprod()`, `tcrossprod()`,
  `outer()` (with `FUN="*"`), `%o%`, `forwardsolve()`, `backsolve()`, `diag()`,
  `t()`, `chol()`, `chol2inv()`, `solve()`, and `qr.solve()`. This includes
  support for:

  - `drop()` for rank 0-2 inputs (including singleton matrices).
  - `svd()` results via `$d`, `$u`, and `$v` (either from `s <- svd(x)` or
    directly from `svd(x)$d`/`$u`/`$v`).
  - `.Machine$double.eps`.
  - `qr.solve()` using the LINPACK QR path (for better compatibility with
    base R behavior).

  The plan is to add more functions in the future (#77, #79 @mns-nordicals)

- Added support for `cbind()` and `rbind()` for rank-0/1/2 inputs, with scalar
  recycling only and strict length checks for non-scalar inputs.

- On macOS, quickr will use LLVM flang (`flang-new`) for compilation when
  available (e.g. `brew install flang`). This is optional and can be disabled
  with `options(quickr.prefer_flang = FALSE)`.

- Added OpenMP parallelization via `declare(parallel())`/`declare(omp())` for
  `for` loops and `sapply()` calls.

- Added support for throwing errors with `stop()`. (#86)

- Added support for `abs()` in size expressions used by `declare(type(...))`
  (e.g. `declare(type(x = integer(abs(end - start) + 1L)))`).

- Improved `for (... in <iterable>)` lowering, including value iteration
  (`for (v in x)`) and support for `rev()` wrappers on supported iterables.

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

- Fixed nested scalar `min()`/`max()` in reductions, so clamp-style expressions
  like `min(max(x[i], lo), hi)` work reliably.

- Fixed an issue where subsetting logical arrays could fail when compiling quick
  functions, e.g. `(x > 0)[2, 3]` (#68).

- Fixed a crash when compiling chained / fall-through assignments like
  `a <- b <- 1` (#60).

- `quick()` now supports dotted symbols (e.g. `foo.bar`) for arguments, locals,
  and loop variables. Conflicting names that map to the same Fortran symbol now
  error (Fortran is case-insensitive).

- Fixed C bridge size checks for dotted argument names used in
  `declare(type(...))` size expressions (e.g. `type(x = double(foo.bar))`).

- `quick()` now gives a helpful error message when a function argument is used
  without being declared
  (i.e. missing `declare(type(arg = ...))`).

- Fixed an error in invalid subscript arity reporting, e.g. `x[1, 2, 3]` on a
  matrix now errors cleanly instead of failing while formatting the message.

- Vector-matrix recycling in arithmetic is now restricted to recycling
  along the first axes only.

- Local closures now support optional arguments with `NULL` defaults, with
  validation to ensure optional arguments are initialized (via `is.null()`)
  before use.

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
