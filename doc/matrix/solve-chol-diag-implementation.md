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

- Note: LAPACK status (`info`) is not propagated yet, so singular or
  non-PD inputs will currently produce undefined numeric results instead of
  R errors. This will be revisited once we have a reliable mechanism to
  bubble Fortran failures back to R.

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
    This matters because Fortran cannot subscript arbitrary expressions, so
    hoisting ensures the temp has a name and valid storage before `[...]`.

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


## Ekstra notes

### On maybe_reshape_vector_matrix
Here’s what that helper is really doing, and why it was added.

The immediate problem it solved

After adding LAPACK-backed matrix ops, we started producing 1x1 matrices (e.g., crossprod(res)), then doing elementwise ops with vectors like diag(XtX_inv) * s2. In Fortran, rank‑1 + rank‑2 is illegal unless one side is a scalar.
maybe_reshape_vector_matrix() fixes those rank mismatches by either:
reshaping a vector to 1xN or Nx1 to match a singleton-dimension matrix, or
scalarizing a 1x1 matrix into x(1,1) so it behaves as a scalar in elementwise ops.
What base R actually does here

In R, vector vs 1x1 matrix does recycle and returns a vector, but it emits a warning:
“Recycling array of length 1 in vector‑array arithmetic is deprecated…”
So the numeric result aligns with treating 1x1 as scalar, but R warns. We don’t currently model those warnings, so the Fortran path silently accepts it.
Why it didn’t show up before

Before the new matrix handlers, we rarely produced 1x1 matrices as intermediates (e.g., crossprod()/chol()/solve() results), so vector/matrix rank clashes weren’t common.
The linear-model example added a realistic path where they show up (diag of a matrix + a 1x1 matrix scalar from crossprod).

### On reshape_vector_for_matrix
Why reshape_vector_for_matrix() still matters

It’s needed when R would recycle a vector across a singleton matrix dimension (e.g., vector + N×1 or 1×N matrix). Fortran requires same-rank arrays for elementwise ops, so we reshape the vector to 1×N or N×1 to make the operation legal and preserve R’s result shape.
It also handles the vector‑length‑1 case so a 1×1 matrix stays a 1×1 matrix (vector becomes 1×1 matrix) instead of forcing scalarization.

#### Example
f <- (vec, mat) {
  declare(
    type(vec = double(n)),
    type(mat = double(1, n))
  )
  a <- vec + mat
  b <- mat + vec
  out <- a + b
  out
}

qf <- quick(f)

x = runif(3)
a = matrix(runif(3), nrow = 1)

qf(x, a)

## On changes to [ - handler
In order to silence a warning form R that made tests fail codex add hoist to "[" handler. This creates a new tmp that we then acesss with []. 

### Example

fn <- function(x) {
  declare(type(x = double(5,5)))
  first_element <- t(x)[1]
  second_row <- t(x)[2, ]
  third_col <- t(x)[, 3]
  sub_matrix <- t(x)[c(1,2), c(3,4)]
  out <- list(
    first_element = first_element,
    second_row = second_row,
    third_col = third_col,
    sub_matrix = sub_matrix
  )
  out
}

x <- matrix(runif(5*5), 5, 5)

qfn <- quick(fn)
fn(x)
qfn(x)
