skip_on_cran()

# Full translations complement the native regressions in test-matrix-mul.R,
# test-blas-guards.R, and test-matrix-lapack.R. Keep the kernel, shape-check,
# allocation, and destination-copy strategies visible together.

test_that("GEMM checks independent dimensions before allocating a local", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m, p)))
    out <- a %*% b
    sum(out)
  }
  expect_translation_snapshots(fn)
})

test_that("GEMV checks its vector length before allocating a temporary", {
  fn <- function(a, x) {
    declare(type(a = double(m, k)), type(x = double(n)))
    sum(a %*% x)
  }
  expect_translation_snapshots(fn)
})

test_that("an explicit vector transpose materializes a row matrix for GEMM", {
  fn <- function(x, m) {
    declare(type(x = double(NA)), type(m = double(3, 3)))
    t(x) %*% m
  }
  expect_translation_snapshots(fn)
})

test_that("a vector-matrix product uses transposed GEMV", {
  fn <- function(x, m) {
    declare(type(x = double(NA)), type(m = double(3, 3)))
    x %*% m
  }
  expect_translation_snapshots(fn)
})

test_that("a vector inner product checks whole vector sizes", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    x %*% y
  }
  expect_translation_snapshots(fn)
})

test_that("GEMM consumes transpose flags without materializing transposes", {
  fn <- function(x, y) {
    declare(type(x = double(4, 3)), type(y = double(5, 4)))
    t(x) %*% t(y)
  }
  expect_translation_snapshots(fn)
})

test_that("BLAS operands are promoted from integer and logical storage", {
  fn <- function(x, y) {
    declare(type(x = integer(2, 3)), type(y = logical(3, 2)))
    x %*% y
  }
  expect_translation_snapshots(fn)
})

test_that("repeated SYRK calls reuse a symbolic destination", {
  fn <- function(x, steps) {
    declare(type(x = double(n, k)), type(steps = integer(1)))
    out <- matrix(0, ncol(x), ncol(x))
    for (i in seq_len(steps)) {
      out <- crossprod(x)
    }
    out
  }
  expect_translation_snapshots(fn)
})

test_that("tcrossprod uses untransposed SYRK and fills both triangles", {
  fn <- function(x) {
    declare(type(x = double(3, 2)))
    tcrossprod(x)
  }
  expect_translation_snapshots(fn)
})

test_that("two-argument crossprod uses GEMM with a column-vector operand", {
  fn <- function(x, y) {
    declare(type(x = double(n, k)), type(y = double(n)))
    crossprod(x, y)
  }
  expect_translation_snapshots(fn)
})

test_that("outer multiplication initializes a GER destination", {
  fn <- function(x, y) {
    declare(type(x = double(n)), type(y = double(m)))
    outer(x, y)
  }
  expect_translation_snapshots(fn)
})

test_that("BLAS destination checks use stored extents after rebinding", {
  fn <- function(x, m) {
    declare(type(x = double(n, k)), type(m = integer(1)))
    out <- matrix(0, m, m)
    m <- m + 1L
    out <- crossprod(x)
    sum(out)
  }
  expect_translation_snapshots(fn)
})

test_that("BLAS destinations check literal singleton result axes", {
  fn <- function(x, m) {
    declare(type(x = double(n, 1)), type(m = integer(1)))
    out <- matrix(0, m, m)
    out <- crossprod(x)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("aliased BLAS input is protected by a separate result", {
  fn <- function(x) {
    declare(type(x = double(n, n)))
    x <- crossprod(x)
    x
  }
  expect_translation_snapshots(fn)
})

test_that("a deferred BLAS destination can change its shape", {
  fn <- function(x) {
    declare(type(x = double(n, k)), type(out = double(NA, NA)))
    out <- x
    out <- crossprod(x)
    sum(out)
  }
  expect_translation_snapshots(fn)
})

test_that("a known empty GEMM contraction fills a nonempty result", {
  fn <- function(a, b) {
    declare(type(a = double(2, 0)), type(b = double(0, 3)))
    a %*% b
  }
  expect_translation_snapshots(fn)
})

test_that("a known empty GEMV contraction fills a nonempty result", {
  fn <- function(a, x) {
    declare(type(a = double(3, 0)), type(x = double(0)))
    a %*% x
  }
  expect_translation_snapshots(fn)
})

test_that("a known empty output omits the BLAS call", {
  fn <- function(a, b) {
    declare(type(a = double(0, 3)), type(b = double(3, 2)))
    a %*% b
  }
  expect_translation_snapshots(fn)
})

test_that("TRSV uses RHS storage extents after rebinding", {
  fn <- function(a, b, m) {
    declare(
      type(a = double(2, 2)),
      type(b = double(m)),
      type(m = integer(1))
    )
    out <- numeric(2)
    m <- m + 1L
    out <- forwardsolve(a, b)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("TRSM uses the stored RHS width and checks its destination", {
  fn <- function(a, b, m, columns) {
    declare(
      type(a = double(2, 2)),
      type(b = double(2, m)),
      type(m = integer(1)),
      type(columns = integer(1))
    )
    out <- matrix(0, 2, columns)
    m <- m + 1L
    out <- backsolve(a, b)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("a transposed triangular solve passes its operation flag to BLAS", {
  fn <- function(a, b) {
    declare(type(a = double(4, 4)), type(b = double(4)))
    backsolve(a, b, transpose = TRUE)
  }
  expect_translation_snapshots(fn)
})

test_that("triangular solve checks unproven square and RHS dimensions", {
  fn <- function(a, x) {
    declare(type(a = double(n, k)), type(x = double(NA)))
    forwardsolve(a, x)
  }
  expect_translation_snapshots(fn)
})

test_that("GESV solves a vector with proven square dimensions", {
  fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n)))
    solve(a, b)
  }
  expect_translation_snapshots(fn)
})

test_that("GESV checks squareness before solving a matrix RHS", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(n, p)))
    solve(a, b)
  }
  expect_translation_snapshots(fn)
})

test_that("GESV handles a scalar RHS for a one-by-one matrix", {
  fn <- function(a, b) {
    declare(type(a = double(1, 1)), type(b = double(1)))
    solve(a, b)
  }
  expect_translation_snapshots(fn)
})

test_that("matrix inversion uses GETRF and GETRI workspace and info checks", {
  fn <- function(a) {
    declare(type(a = double(n, n)))
    solve(a)
  }
  expect_translation_snapshots(fn)
})

test_that("Cholesky factorization and inversion initialize both triangles", {
  fn <- function(a) {
    declare(type(a = double(n, n)))
    upper <- chol(a)
    chol2inv(upper)
  }
  expect_translation_snapshots(fn)
})

test_that("QR evaluates an effectful tolerance before checking RHS shape", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m)))
    qr.solve(a, b, tol = runif(1))
  }
  expect_translation_snapshots(fn)
})
