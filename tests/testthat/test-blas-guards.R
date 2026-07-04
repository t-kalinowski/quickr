# Runtime conformability guards in BLAS/LAPACK lowerings: dims that cannot
# be verified at compile time get a size() check before the BLAS call
# (never a compile-time warning, never an unchecked call).

skip_on_cran()

test_that("matrix-vector %*% guards an unknown vector length", {
  fn <- function(m, x) {
    declare(type(m = double(3, 3)), type(x = double(NA)))
    m %*% x
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(diag(3), as.double(1:3)), diag(3) %*% 1:3)
  # was: dgemv read past the end of x, returning garbage
  expect_error(qfn(diag(3), as.double(1:2)), "non-conformable arguments in %*%", fixed = TRUE)
})

test_that("vector-matrix %*% guards an unknown vector length", {
  fn <- function(x, m) {
    declare(type(x = double(NA)), type(m = double(3, 3)))
    t(x) %*% m
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(as.double(1:3), diag(3)), t(as.double(1:3)) %*% diag(3))
  expect_error(qfn(as.double(1:5), diag(3)), "non-conformable arguments in %*%", fixed = TRUE)
})

test_that("triangular solve guards squareness and RHS length", {
  fn <- function(l, x) {
    declare(type(l = double(n, k)), type(x = double(NA)))
    forwardsolve(l, x)
  }
  qfn <- expect_no_warning(quick(fn))
  l <- matrix(c(1, 2, 0, 3), 2, 2)
  expect_equal(qfn(l, c(1, 5)), forwardsolve(l, c(1, 5)))
  expect_error(
    qfn(l, c(1, 5, 9)),
    "non-conformable arguments in triangular solve"
  )
  expect_error(
    qfn(matrix(as.double(1:6), 2, 3), c(1, 5)),
    "triangular solve requires a square matrix"
  )
})

test_that("solve() guards an unknown RHS length", {
  fn <- function(a, b) {
    declare(type(a = double(2, 2)), type(b = double(NA)))
    solve(a, b)
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(diag(2), c(1, 2)), c(1, 2))
  expect_error(qfn(diag(2), c(1, 2, 3)), "non-conformable arguments in solve")
})

test_that("solve(a) and chol() guard squareness", {
  inv <- function(a) {
    declare(type(a = double(n, k)))
    solve(a)
  }
  qinv <- expect_no_warning(quick(inv))
  expect_equal(qinv(diag(2)), diag(2))
  expect_error(
    qinv(matrix(as.double(1:6), 2, 3)),
    "solve requires a square matrix"
  )

  chol_fn <- function(a) {
    declare(type(a = double(n, k)))
    chol(a)
  }
  qchol <- expect_no_warning(quick(chol_fn))
  expect_equal(qchol(diag(2)), diag(2))
  expect_error(
    qchol(matrix(as.double(1:6), 2, 3)),
    "chol requires a square matrix"
  )
})

test_that("NA dims are never treated as equal", {
  fn <- function(a, b) {
    declare(type(a = double(NA, NA)), type(b = double(NA, NA)))
    a %*% b
  }
  qfn <- expect_no_warning(quick(fn))
  m <- matrix(as.double(1:4), 2, 2)
  expect_equal(qfn(m, m), m %*% m)
  # was: identical(NA, NA) blessed the pair with no check at all
  expect_error(
    qfn(m, matrix(as.double(1:6), 3, 2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})

test_that("guard text is pinned (one snapshot per mechanism)", {
  fn <- function(m, x) {
    declare(type(m = double(3, 3)), type(x = double(NA)))
    m %*% x
  }
  expect_translation_snapshots(
    fn,
    note = "Unverifiable BLAS dims emit one size guard before the call."
  )
})
