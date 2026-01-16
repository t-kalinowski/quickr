# Public-API tests that exercise internal inference/hoisting code paths.
# These replace several direct `quickr:::` unit tests while keeping coverage.

test_that("matrix destination inference gracefully declines non-symbol inputs", {
  matmul_expr_arg <- function(A, B) {
    declare(type(A = double(2, 3)), type(B = double(3, 2)))
    out <- (A + 0.0) %*% B
    out
  }

  crossprod_expr_arg <- function(x) {
    declare(type(x = double(4, 3)))
    out <- crossprod((x + 0.0))
    out
  }

  tcrossprod_expr_arg <- function(x) {
    declare(type(x = double(4, 3)))
    out <- tcrossprod((x + 0.0))
    out
  }

  outer_expr_arg <- function(x, y) {
    declare(type(x = double(2)), type(y = double(3)))
    out <- outer((x + 0.0), y)
    out
  }

  backsolve_expr_arg <- function(U, b) {
    declare(type(U = double(2, 2)), type(b = double(2)))
    out <- backsolve((U + 0.0), b)
    out
  }

  solve_expr_arg <- function(A, b) {
    declare(type(A = double(3, 3)), type(b = double(3)))
    out <- solve((A + 0.0), b)
    out
  }

  diag_expr_arg <- function(x) {
    declare(type(x = double(3)))
    out <- diag((x + 0.0))
    out
  }

  set.seed(1)

  A <- matrix(rnorm(6), nrow = 2)
  B <- matrix(rnorm(6), nrow = 3)
  expect_quick_equal(matmul_expr_arg, list(A = A, B = B))

  x <- matrix(rnorm(12), nrow = 4)
  expect_quick_equal(crossprod_expr_arg, list(x = x))
  expect_quick_equal(tcrossprod_expr_arg, list(x = x))

  v2 <- rnorm(2)
  v3 <- rnorm(3)
  expect_quick_equal(outer_expr_arg, list(x = v2, y = v3))

  U <- matrix(c(2, 1, 0, 3), nrow = 2, byrow = TRUE)
  b2 <- c(1.25, -0.5)
  expect_quick_equal(backsolve_expr_arg, list(U = U, b = b2))

  base <- matrix(rnorm(9), 3, 3)
  A_pd <- crossprod(base) + diag(3)
  b3 <- rnorm(3)
  expect_quick_equal(solve_expr_arg, list(A = A_pd, b = b3))

  x3 <- rnorm(3)
  expect_quick_equal(diag_expr_arg, list(x = x3))
})

test_that("matrix destination inference works for common symbol-based shapes", {
  # These cases exercise the `dest_infer` helpers without reaching for any
  # `quickr:::` internals. The key property is that `out` is NOT declared, so
  # `quickr` must infer its shape from the symbol-based operands.

  matmul_mm <- function(A, B) {
    declare(type(A = double(2, 3)), type(B = double(3, 4)))
    out <- A %*% B
    out
  }

  matmul_mv <- function(A, x) {
    declare(type(A = double(2, 3)), type(x = double(3)))
    out <- A %*% x
    out
  }

  matmul_vm <- function(x, A) {
    declare(type(x = double(3)), type(A = double(3, 4)))
    out <- x %*% A
    out
  }

  matmul_t_left <- function(A, B) {
    declare(type(A = double(4, 3)), type(B = double(4, 2)))
    out <- t(A) %*% B
    out
  }

  matmul_t_right <- function(A, B) {
    declare(type(A = double(4, 3)), type(B = double(2, 3)))
    out <- A %*% t(B)
    out
  }

  matmul_t_vector <- function(v, B) {
    declare(type(v = double(3)), type(B = double(3, 4)))
    out <- t(v) %*% B
    out
  }

  crossprod_1 <- function(x) {
    declare(type(x = double(5, 3)))
    out <- crossprod(x)
    out
  }

  crossprod_2 <- function(x, y) {
    declare(type(x = double(5, 3)), type(y = double(5, 2)))
    out <- crossprod(x, y)
    out
  }

  tcrossprod_1 <- function(x) {
    declare(type(x = double(5, 3)))
    out <- tcrossprod(x)
    out
  }

  tcrossprod_2 <- function(x, y) {
    declare(type(x = double(5, 3)), type(y = double(2, 3)))
    out <- tcrossprod(x, y)
    out
  }

  outer_vv <- function(x, y) {
    declare(type(x = double(2)), type(y = double(3)))
    out <- outer(x, y)
    out
  }

  outer_sv <- function(x, y) {
    declare(type(x = double(1)), type(y = double(3)))
    out <- outer(x, y)
    out
  }

  triangular_vec <- function(U, b) {
    declare(type(U = double(2, 2)), type(b = double(2)))
    out <- backsolve(U, b)
    out
  }

  triangular_mat <- function(U, B) {
    declare(type(U = double(2, 2)), type(B = double(2, 3)))
    out <- backsolve(U, B)
    out
  }

  solve_inv <- function(A) {
    declare(type(A = double(3, 3)))
    out <- solve(A)
    out
  }

  solve_vec <- function(A, b) {
    declare(type(A = double(3, 3)), type(b = double(3)))
    out <- solve(A, b)
    out
  }

  solve_mat <- function(A, B) {
    declare(type(A = double(3, 3)), type(B = double(3, 2)))
    out <- solve(A, B)
    out
  }

  chol_out <- function(A) {
    declare(type(A = double(3, 3)))
    out <- chol(A)
    out
  }

  chol2inv_out <- function(A) {
    declare(type(A = double(3, 3)))
    out <- chol2inv(chol(A))
    out
  }

  diag_vec <- function(x) {
    declare(type(x = double(3)))
    out <- diag(x)
    out
  }

  diag_mat <- function(A) {
    declare(type(A = double(2, 3)))
    out <- diag(A)
    out
  }

  diag_missing_x <- function(n) {
    declare(type(n = integer(1)))
    out <- diag(nrow = n)
    out
  }

  diag_scalar_symbol_no_nrow <- function() {
    # Cover the scalar-symbol branch in `infer_dest_diag()` (no nrow/ncol).
    n <- 3L
    out <- diag(n)
    out
  }

  diag_vec_with_nrow_ncol <- function(x, n, m) {
    declare(type(x = double(3)), type(n = integer(1)), type(m = integer(1)))
    out <- diag(x, nrow = n, ncol = m)
    out
  }

  set.seed(123)

  A <- matrix(rnorm(2 * 3), nrow = 2)
  B <- matrix(rnorm(3 * 4), nrow = 3)
  expect_quick_equal(matmul_mm, list(A = A, B = B))

  x3 <- rnorm(3)
  expect_quick_equal(matmul_mv, list(A = A, x = x3))
  expect_quick_equal(matmul_vm, list(x = x3, A = matrix(rnorm(3 * 4), 3, 4)))

  A43 <- matrix(rnorm(4 * 3), nrow = 4)
  B42 <- matrix(rnorm(4 * 2), nrow = 4)
  expect_quick_equal(matmul_t_left, list(A = A43, B = B42))

  B23 <- matrix(rnorm(2 * 3), nrow = 2)
  expect_quick_equal(matmul_t_right, list(A = A43, B = B23))

  expect_quick_equal(
    matmul_t_vector,
    list(v = x3, B = matrix(rnorm(3 * 4), 3, 4))
  )

  X <- matrix(rnorm(5 * 3), nrow = 5)
  Yc <- matrix(rnorm(5 * 2), nrow = 5)
  Yt <- matrix(rnorm(2 * 3), nrow = 2)
  expect_quick_equal(crossprod_1, list(x = X))
  expect_quick_equal(crossprod_2, list(x = X, y = Yc))
  expect_quick_equal(tcrossprod_1, list(x = X))
  expect_quick_equal(tcrossprod_2, list(x = X, y = Yt))

  v2 <- rnorm(2)
  v3 <- rnorm(3)
  expect_quick_equal(outer_vv, list(x = v2, y = v3))
  expect_quick_equal(outer_sv, list(x = 1.25, y = v3))

  U <- matrix(c(2, 1, 0, 3), nrow = 2, byrow = TRUE)
  b <- rnorm(2)
  B_rhs <- matrix(rnorm(2 * 3), nrow = 2)
  expect_quick_equal(triangular_vec, list(U = U, b = b))
  expect_quick_equal(triangular_mat, list(U = U, B = B_rhs))

  base <- matrix(rnorm(9), 3, 3)
  A_pd <- crossprod(base) + diag(3)
  b3 <- rnorm(3)
  B32 <- matrix(rnorm(3 * 2), nrow = 3)
  expect_quick_equal(solve_inv, list(A = A_pd))
  expect_quick_equal(solve_vec, list(A = A_pd, b = b3))
  expect_quick_equal(solve_mat, list(A = A_pd, B = B32))
  expect_quick_equal(chol_out, list(A = A_pd))
  expect_quick_equal(chol2inv_out, list(A = A_pd))

  expect_quick_equal(diag_vec, list(x = rnorm(3)))
  expect_quick_equal(diag_mat, list(A = matrix(rnorm(6), nrow = 2)))
  expect_quick_equal(diag_missing_x, list(n = 3L))
  expect_quick_equal(diag_scalar_symbol_no_nrow, list())
  expect_quick_equal(
    diag_vec_with_nrow_ncol,
    list(x = rnorm(3), n = 4L, m = 2L)
  )
})

test_that("destination inference declines unsupported triangular/solve forms", {
  # These are public-facing errors, but they also exercise inference decline
  # paths (e.g., scalar RHS) without directly calling internal helpers.

  backsolve_scalar_rhs <- function(U, b) {
    declare(type(U = double(2, 2)), type(b = double(1)))
    out <- backsolve(U, b)
    out
  }

  solve_scalar_rhs <- function(A, b) {
    declare(type(A = double(2, 2)), type(b = double(1)))
    out <- solve(A, b)
    out
  }

  expect_error(
    quick(backsolve_scalar_rhs),
    regexp = "non-conformable arguments in triangular solve|expects a vector or matrix"
  )
  expect_error(
    quick(solve_scalar_rhs),
    regexp = "non-conformable arguments in solve|expects a vector or matrix"
  )
})
