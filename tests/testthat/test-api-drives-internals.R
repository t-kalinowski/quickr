# Public-API tests that exercise internal inference/hoisting code paths.
# These replace several direct `quickr:::` unit tests while keeping coverage.

skip_on_cran()

test_that("matrix destinations use the shapes of translated expressions", {
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

test_that("matrix assignments reject unsupported triangular and solve forms", {
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
