# Compact matrix/LAPACK coverage kept on CRAN.

test_that("matrix smoke: matmul inference compiles and runs", {
  fn <- function(A, B) {
    declare(type(A = double(2, 3)), type(B = double(3, 2)))
    out <- A %*% B
    out
  }

  set.seed(1)
  A <- matrix(rnorm(6), nrow = 2)
  B <- matrix(rnorm(6), nrow = 3)
  expect_quick_equal(fn, list(A = A, B = B))
})

test_that("matrix smoke: solve, qr.solve, and chol run end-to-end", {
  solve_vec <- function(A, b) {
    declare(type(A = double(n, n)), type(b = double(n)))
    solve(A, b)
  }

  qr_solve_vec <- function(X, y) {
    declare(type(X = double(n, k)), type(y = double(n)))
    qr.solve(X, y)
  }

  chol_fn <- function(A) {
    declare(type(A = double(n, n)))
    chol(A)
  }

  set.seed(2)
  n <- 4
  k <- 2

  base <- matrix(rnorm(n * n), nrow = n)
  A <- crossprod(base) + diag(n)
  b <- rnorm(n)
  expect_quick_equal(solve_vec, list(A = A, b = b))
  expect_quick_equal(chol_fn, list(A = A))

  X <- matrix(rnorm(6 * k), nrow = 6)
  y <- rnorm(6)
  q_qr_solve_vec <- expect_warning(quick(qr_solve_vec), NA)
  expect_equal(q_qr_solve_vec(X, y), qr.solve(X, y))
})
