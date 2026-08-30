# quickr (development version)

* Repeated `quick()` compilations now reuse successful `R CMD config` results
  for the rest of the R session. Restart R after changing the compiler or
  Makevars configuration.

* Successful flang availability checks are now reused for the rest of the R
  session. Restart R after changing the flang toolchain.
- `c()` now accepts matrix and array arguments, flattening them in
  column-major order like R (previously `c(m)` for a matrix `m` errored
  with "all args passed to c() must be scalars or 1-d arrays"). `as.vector()`
  is now supported: it drops dimensions (mode `"any"`, the default,
  preserves the type; `mode = "double"`/`"integer"`/`"numeric"` coerce).

- Reassigning a variable now requires the new value's shape to be
  compatible with the declared shape, extending the existing rank check
  to every dimension: quickr cannot re-declare a Fortran variable the
  way R rebinds a symbol. A statically known mismatch (e.g.
  `x <- numeric(2); x <- numeric(3)`) is a compile-time error;
  dimensions that cannot be compared at compile time are checked at run
  time. Previously such reassignments silently kept the old shape or
  produced invalid Fortran. This also covers reassignment between a scalar
  and an array in either direction: `x <- numeric(n); x <- 0` used to
  broadcast the scalar across every element, and assigning an array to a
  scalar variable used to keep only its first element, where R rebinds the
  symbol in both cases. Locals declared with unknown (`NA`) dims still
  reallocate on assignment, like R.

- `diag(x)` with a length-1 `x` and no `nrow`/`ncol` now builds the
  identity matrix of size `x`, matching R. Previously only a *literal*
  size took that path, so `diag(n)` with `n` declared `integer(1)`
  returned a 1x1 matrix containing `n` instead of the n-by-n identity —
  a silent divergence in both shape and values. As in R the size is
  `as.integer(x)`, so a `double` or `logical` `x` works and truncates
  toward zero (`diag(3.7)` is the 3x3 identity, as in R). Use
  `diag(x, nrow)` for a 1x1 matrix holding `x`.

- `declare()` size expressions now accept `as.integer()`, which lowers to
  Fortran's `INT()` and to an integer cast in the generated C bridge.

- Elementwise operations (arithmetic, comparisons, `&`, `|`) now require
  operand lengths to match, unless one operand is a scalar or a vector is
  combined column-wise with a matrix whose rows it spans. R-style partial
  recycling was never implemented: expressions like `x + y` with
  `length(x) == 4`, `length(y) == 2` previously compiled to code that read
  out of bounds. Statically unequal lengths are now a compile-time error;
  lengths that cannot be verified at compile time are checked at run time.

- `1x1`-matrix operands now follow R's rules: in arithmetic against a
  vector of statically known length other than 1 they are treated as
  scalars and the result is a plain vector (which R allows, with a
  deprecation warning), while in comparisons and `&`/`|` they are treated
  as one-row matrices, so mismatched shapes are rejected — matching R,
  which raises an error. When the vector's length is only known at run
  time, the result's shape would depend on that value (R keeps the `1x1`
  dims for a length-1 vector and drops them otherwise), so arithmetic
  also takes the one-row-matrix rule: a runtime check requires length 1
  and the result is a `1x1` matrix; longer vectors raise an error where
  R would recycle. Previously a `1x1` matrix was scalarized in arithmetic
  and not shape-checked at all in comparisons and `&`/`|`, so e.g.
  `x < m` with `m` a `1x1` matrix failed to build with a Fortran rank
  mismatch instead of a quickr error, an operand needing a cast failed to
  build even in arithmetic, and a symbolic-length `x + m` returned a plain
  vector where R returns a `1x1` matrix.

- `&&` and `||` now behave like R's scalar control operators. They
  require length-1 logical operands (longer operands are a compile-time
  error, as they are a runtime error in R; use `&`/`|` for elementwise
  logic), and they short-circuit: the right operand is evaluated only when
  the left side does not decide the answer, so idioms like
  `while (i <= n && x[i] > 0)` are safe. Previously they compiled exactly
  like `&`/`|` — elementwise over vectors (returning answers where R
  errors) and with both sides always evaluated.

- `solve(a, b)` now requires a square `a`, matching R. A rectangular `a`
  previously fell through to a least-squares solve (dgels), returning an
  answer where R raises `'a' (m x n) must be square`. Statically
  rectangular systems are now a compile-time error; when squareness is not
  known at compile time it is checked at run time. Use `qr.solve()` for
  least-squares solutions of rectangular systems (unchanged).

- Complex operands in linear algebra (`%*%`, `crossprod()`, `solve()`,
  `chol()`, ...) are now a compile-time error. quickr's lowerings use the
  real (double-only) BLAS/LAPACK routines, which previously read complex
  storage as reals and returned a plausible but wrong real result where R
  returns a complex one. Elementwise complex arithmetic and the
  mode-preserving `t()`/`diag()` are unaffected.

# quickr 0.3.0

This release adds major new support for linear algebra, local functions,
`sapply()`, and OpenMP parallelism, along with a broad round of reliability,
compatibility, and compiler/runtime improvements.

## Major new features

### Matrix and linear algebra support

- Added initial matrix / linear algebra support from base R, using the same
  BLAS/LAPACK as R. Supported operations now include `%*%`, `t()`,
  `crossprod()`, `tcrossprod()`, `outer()` with `FUN = "*"`, `%o%`,
  `forwardsolve()`, `backsolve()`, `diag()`, `chol()`, `chol2inv()`,
  `solve()`, and `qr.solve()`. (#77, #79, @mns-nordicals)

- This also adds support for:

  - `drop()` for rank 0-2 inputs, including singleton matrices.
  - `svd()` results via `$d`, `$u`, and `$v`, whether accessed from
    `s <- svd(x)` or directly as `svd(x)$d`, `svd(x)$u`, or `svd(x)$v`.
  - `.Machine$double.eps`.

### Local closures and `sapply()`

- Added support for local closures inside compiled functions, including
  `sapply()` lowering with host association for captures and support for
  `simplify = "array"` for higher-rank outputs.

- Example:

  ```r
  f <- function(i) x[i] + 1.0
  out <- sapply(seq_along(out), f)
  ```

* This lowering matches R’s copy-on-modify semantics: modifying variables from
  the parent scope triggers a copy in the local closure scope.

* Added support for superassignment (`<<-`) inside local closures, including
  subset targets such as `x[i] <<- ...`, to explicitly mutate variables in the
  enclosing quick function scope. For example:

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

* Local closures also support optional arguments with `NULL` defaults,
  with validation to ensure optional arguments are initialized via `is.null()`
  before use.

### Parallel execution

* Added OpenMP parallelization via `declare(parallel())` / `declare(omp())`
  for supported `for` loops and `sapply()` calls.

### Binding and iteration

* Added support for `cbind()` and `rbind()` for rank-0/1/2 inputs, with scalar
  recycling only and strict length checks for non-scalar inputs.

* Improved lowering for `for (... in <iterable>)`, including value iteration
  such as `for (v in x)`, and support for `rev()` wrappers on supported
  iterables.

## Language and runtime additions

* Added support for throwing errors with `stop()`. (#86)

* Added support for `as.integer()` coercion from `double`, `logical`, and
  `integer` values. `double` inputs are truncated toward 0.

* Added support for `tanh()` as a unary intrinsic.

* Added support for `rev()` for reversing rank-0/1 vectors, including bind(c)
  logicals, preserving `NA` storage where possible.

* Added support for `abs()` in size expressions used by `declare(type(...))`,
  for example:
  `declare(type(x = integer(abs(end - start) + 1L)))`.

* Improved support for `array(data = ..., dim = ...)` for shape construction in
  user code.

## Compiler and lowering improvements

- quickr now supports nested compilation scopes with local declaration
  emission, enabling block-scoped temporaries and local closures lowered to
  internal procedures under `contains`.

  - This helps in two ways. First, in large compiled functions, declared intermediate
    results no longer need to stay live for the full duration of the function,
    which can reduce peak memory use.

  - Second, it improves handling of large temporaries across compilers. In
    particular, LLVM flang may place temporary arrays on the stack by default,
    which can cause problems when those temporaries are large. This work also
    led to better control over when temporaries need to be emitted as
    heap allocatable.

* Example of a block-scoped temporary used for expression subscripting:

  ```r
  out[1] <- ifelse((x > 0.0)[2, 3], 1.0, 0.0)
  ```

## Reliability, compatibility, and diagnostics

* Added extensive tests and snapshots covering local closures, `sapply()`,
  block-scoped temporaries, and package compilation behavior, including
  internal procedure name collisions.

* Fixed `[` subsetting on scalars with both `drop = TRUE` and `drop = FALSE`,
  simplifying to Fortran scalars so subsetted scalars in reductions such as
  `min(m[1, 1], m[2, 1])` no longer emit `minval` on scalars. (#64)

* Fixed nested scalar `min()` / `max()` in reductions, so clamp-style
  expressions like `min(max(x[i], lo), hi)` work reliably.

* Fixed an issue where subsetting logical arrays could fail when compiling
  quick functions, for example `(x > 0)[2, 3]`. (#68)

* Fixed a crash when compiling chained or fall-through assignments such as
  `a <- b <- 1`. (#60)

* `quick()` now supports dotted symbols such as `foo.bar` for arguments,
  locals, and loop variables. Conflicting names that map to the same Fortran
  symbol now error, since Fortran is case-insensitive.

* Fixed C bridge size checks for dotted argument names used in
  `declare(type(...))` size expressions, for example
  `type(x = double(foo.bar))`.

* `quick()` now gives a helpful error when a function argument is used without
  being declared through `declare(type(arg = ...))`.

* Fixed an error in invalid subscript arity reporting, for example
  `x[1, 2, 3]` on a matrix now errors cleanly instead of failing while
  formatting the message.

## Breaking changes

* Vector-matrix recycling in arithmetic is now restricted to recycling along
  the first axes only.

## Platform and toolchain

* Lowered the minimum supported R version to 4.3.0 and added a backport of
  `declare()` for R < 4.4.0. (#67)

* On macOS, quickr will use LLVM flang (`flang-new`) for compilation when
  available, for example after `brew install flang`. This is optional and can
  be disabled with `options(quickr.prefer_flang = FALSE)`.

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

- Improved error message when using case-sensitive variable names
  (#18, #36, #39)

- Added `AGENTS.md` and `scripts/setup_codex.sh` to enable the ChatGPT/Codex
  agent to run tests in a docker container configured without internet access.


# quickr 0.1.0

* Initial CRAN submission.
