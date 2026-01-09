# Matrix LAPACK Handlers: Implementation Walkthrough

This document explains the changes made to support `solve()`, `chol()`,
`chol2inv()`, and `diag()` along with the linear-model example.

## 1) Add dimension expression support

- `R/manifest.R`: added `min()` and `max()` to the dimension expression
  evaluator so Fortran declarations can emit `min(a, b)` and `max(a, b)`
  instead of failing on those calls.

## 2) New BLAS/LAPACK emitters

- `R/r2f-matrix-blas.R`: introduced LAPACK helpers and diag utilities:
  - `lapack_solve()` uses `dgesv` to solve `A %*% x = B` and respects
    destination inference while avoiding destructive overwrite of inputs.
  - `lapack_inverse()` uses `dgetrf` + `dgetri` for `solve(A)`.
  - `lapack_chol()` uses `dpotrf` and zeroes the lower triangle to match
    base R `chol()` output.
  - `lapack_chol2inv()` uses `dpotri` and mirrors the upper triangle to
    produce a full symmetric inverse.
  - `diag_extract()` and `diag_matrix()` cover extraction and construction
    forms for `diag()`.
  - `diag_length_expr()` computes `min(nrow, ncol)` while keeping the
    expression symbolic when needed.
  - `zero_lower_triangle()` mirrors base R output by clearing the lower
    triangle after `dpotrf`.
- Updated `blas_int()` to safely deparse language inputs so expressions like
  `int(min(a, b), kind=c_int)` render correctly.

## 3) Destination inference for new handlers

- `R/r2f-matrix-infer.R`: added inference helpers for:
  - `solve()` (vector, matrix, and inverse cases)
  - `chol()` and `chol2inv()`

This allows the assignment handler to pass a destination and avoid extra
allocations when possible.

## 4) Register new matrix handlers

- `R/r2f-matrix.R`:
  - Added handlers for `solve()`, `chol()`, `chol2inv()`, and `diag()`.
  - Implemented base-R-like argument rules and error behavior.
  - Ensured incompatible options (e.g., `pivot=TRUE` for `chol`) are rejected
    with clear messages.

## 5) Vector and scalar reshape behavior

- `R/r2f.R`:
  - Added `maybe_reshape_vector_matrix()` and `scalarize_matrix()` so
    elementwise operations can handle vector vs 1x1 matrix and vector vs
    matrix with singleton dimensions.
    - 1x1 matrices are scalarized via `scalarize_matrix()` (they become a
      scalar designator like `x(1, 1)`), which keeps Fortran ranks consistent.
    - Vectors can be reshaped into 1xN or Nx1 matrices to match singleton
      dimensions when needed.
  - This removes rank mismatches in cases like `diag(XtX_inv) * s2` when `s2`
    is a 1x1 matrix from `crossprod(res)` and `diag()` returns a vector.
  - Also passed `hoist` through `[` lowering so temporary arrays created by
    BLAS/LAPACK helpers (e.g., `crossprod(res)` emitting a temp in `syrk()`)
    are declared and in scope when immediately subscripted like
    `crossprod(res)[1]`.

## 6) Tests added

- `tests/testthat/test-matrix-lapack.R`:
  - `solve()` vector, matrix, and inverse.
  - `chol()` and `chol2inv()`.
  - `diag()` for vector input, matrix extraction, and size-based construction.
  - Full linear-model example based on the provided code.

## 7) Commands executed

- `air format .`
- `R -q -e 'devtools::test()'`
- `R -q -e 'rcmdcheck::rcmdcheck(error_on = "warning")'`

All tests and `R CMD check` completed without errors or warnings.
