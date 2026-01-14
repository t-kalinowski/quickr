test_that("matrix ops infer destination sizes for assignments", {
  matmul_infer <- function(A, B) {
    declare(type(A = double(2, 3)), type(B = double(3, 2)))
    out <- A %*% B
    out
  }

  matvec_infer <- function(A, x) {
    declare(type(A = double(2, 3)), type(x = double(3)))
    out <- A %*% x
    out
  }

  vecmat_infer <- function(x, A) {
    declare(type(x = double(2)), type(A = double(2, 3)))
    out <- x %*% A
    out
  }

  cross_infer <- function(x) {
    declare(type(x = double(4, 3)))
    out <- crossprod(x)
    out
  }

  cross_infer2 <- function(x, y) {
    declare(type(x = double(4, 3)), type(y = double(4, 2)))
    out <- crossprod(x, y)
    out
  }

  tcross_infer <- function(x) {
    declare(type(x = double(4, 3)))
    out <- tcrossprod(x)
    out
  }

  tcross_infer2 <- function(x, y) {
    declare(type(x = double(4, 3)), type(y = double(2, 3)))
    out <- tcrossprod(x, y)
    out
  }

  outer_infer <- function(x, y) {
    declare(type(x = double(2)), type(y = double(3)))
    out <- outer(x, y)
    out
  }

  outer_op_infer <- function(x, y) {
    declare(type(x = double(2)), type(y = double(3)))
    out <- x %o% y
    out
  }

  forward_infer <- function(L, b) {
    declare(type(L = double(2, 2)), type(b = double(2, 2)))
    out <- forwardsolve(L, b)
    out
  }

  back_infer <- function(U, b) {
    declare(type(U = double(2, 2)), type(b = double(2)))
    out <- backsolve(U, b)
    out
  }

  solve_vec_infer <- function(A, b) {
    declare(type(A = double(n, n)), type(b = double(n)))
    out <- solve(A, b)
    out
  }

  solve_mat_infer <- function(A, B) {
    declare(type(A = double(n, n)), type(B = double(n, k)))
    out <- solve(A, B)
    out
  }

  solve_inv_infer <- function(A) {
    declare(type(A = double(n, n)))
    out <- solve(A)
    out
  }

  chol_infer <- function(A) {
    declare(type(A = double(n, n)))
    out <- chol(A)
    out
  }

  chol2inv_infer <- function(A) {
    declare(type(A = double(n, n)))
    out <- chol2inv(chol(A))
    out
  }

  set.seed(99)
  n <- 4
  k <- 2
  A <- matrix(rnorm(6), nrow = 2)
  B <- matrix(rnorm(6), nrow = 3)
  x2 <- rnorm(2)
  x3 <- rnorm(3)
  X <- matrix(rnorm(12), nrow = 4)
  Yc <- matrix(rnorm(8), nrow = 4)
  Yt <- matrix(rnorm(6), nrow = 2)
  v2 <- rnorm(2)
  v3 <- rnorm(3)

  L <- matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE)
  U <- matrix(c(2, 1, 0, 3), nrow = 2, byrow = TRUE)
  b_mat <- matrix(rnorm(4), nrow = 2)
  b_vec <- rnorm(2)
  base <- matrix(rnorm(n * n), n, n)
  A_pd <- crossprod(base) + diag(n)
  b_solve <- rnorm(n)
  B_solve <- matrix(rnorm(n * k), n, k)

  expect_quick_equal(matmul_infer, list(A = A, B = B))
  expect_quick_equal(matvec_infer, list(A = A, x = x3))
  expect_quick_equal(vecmat_infer, list(x = x2, A = A))
  expect_quick_equal(cross_infer, list(x = X))
  expect_quick_equal(cross_infer2, list(x = X, y = Yc))
  expect_quick_equal(tcross_infer, list(x = X))
  expect_quick_equal(tcross_infer2, list(x = X, y = Yt))
  expect_quick_equal(outer_infer, list(x = v2, y = v3))
  expect_quick_equal(outer_op_infer, list(x = v2, y = v3))
  expect_quick_equal(forward_infer, list(L = L, b = b_mat))
  expect_quick_equal(back_infer, list(U = U, b = b_vec))
  expect_quick_equal(solve_vec_infer, list(A = A_pd, b = b_solve))
  expect_quick_equal(solve_mat_infer, list(A = A_pd, B = B_solve))
  expect_quick_equal(solve_inv_infer, list(A = A_pd))
  expect_quick_equal(chol_infer, list(A = A_pd))
  expect_quick_equal(chol2inv_infer, list(A = A_pd))
})

test_that("matrix helpers report unsupported inputs", {
  matmul_bad_rank <- function(a, b) {
    declare(type(a = double(2, 2, 2)), type(b = double(2, 2)))
    a %*% b
  }

  transpose_bad_rank <- function(x) {
    declare(type(x = double(2, 2, 2)))
    t(x)
  }

  outer_bad_rank <- function(x, y) {
    declare(type(x = double(2, 2)), type(y = double(2)))
    outer(x, y)
  }

  outer_missing <- function(x) {
    declare(type(x = double(2)))
    outer(x)
  }

  forward_k <- function(L, b) {
    declare(type(L = double(2, 2)), type(b = double(2)))
    forwardsolve(L, b, k = 1)
  }

  back_bad_upper <- function(U, b, flag) {
    declare(
      type(U = double(2, 2)),
      type(b = double(2)),
      type(flag = logical(1))
    )
    backsolve(U, b, upper.tri = flag)
  }

  back_bad_A <- function(A, b) {
    declare(type(A = double(2)), type(b = double(2)))
    backsolve(A, b)
  }

  back_bad_B <- function(U, b) {
    declare(type(U = double(2, 2)), type(b = double(2, 2, 2)))
    backsolve(U, b)
  }

  expect_error(quick(matmul_bad_rank), "%\\*% only supports vectors/matrices")
  expect_error(quick(transpose_bad_rank), "t\\(\\) only supports rank 0-2")
  expect_error(quick(outer_bad_rank), "outer\\(\\) only supports vectors")
  expect_error(quick(outer_missing), "outer\\(\\) expects X and Y")
  expect_error(quick(forward_k), "forwardsolve\\(\\) does not support k")
  expect_error(quick(back_bad_upper), "only supports literal upper\\.tri")
  expect_error(quick(back_bad_A), "triangular solve expects a matrix")
  expect_error(quick(back_bad_B), "triangular solve only supports vector")
})

test_that("matrix conformability warnings are surfaced", {
  matmul_warn <- function(A, B, n, m, k) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(k = integer(1)),
      type(A = double(n, m)),
      type(B = double(k, n))
    )
    A %*% B
  }

  expect_warning(
    quick(matmul_warn),
    "cannot verify conformability in %\\*%"
  )
})

test_that("infer_dest_crossprod returns NULL for non-symbol arg", {
  scope <- quickr:::new_scope(function() NULL)
  # Pass a literal instead of a symbol
  args <- list(1L)
  result <- quickr:::infer_dest_crossprod(args, scope)
  expect_null(result)
})

test_that("infer_dest_tcrossprod returns NULL for non-symbol arg", {
  scope <- quickr:::new_scope(function() NULL)
  # Pass a literal instead of a symbol
  args <- list(1L)
  result <- quickr:::infer_dest_tcrossprod(args, scope)
  expect_null(result)
})

test_that("infer_dest_outer returns NULL when x or y not resolved", {
  scope <- quickr:::new_scope(function() NULL)
  # Pass symbols that don't resolve
  args <- list(quote(x), quote(y))
  result <- quickr:::infer_dest_outer(args, scope)
  expect_null(result)
})

test_that("infer_dest_outer returns NULL for rank > 1 inputs", {
  scope <- quickr:::new_scope(function() NULL)
  scope[["x"]] <- quickr:::Variable("double", list(2L, 2L))
  scope[["y"]] <- quickr:::Variable("double", list(2L))
  args <- list(quote(x), quote(y))
  result <- quickr:::infer_dest_outer(args, scope)
  expect_null(result)
})

test_that("infer_dest_outer works with valid vector inputs", {
  scope <- quickr:::new_scope(function() NULL)
  scope[["x"]] <- quickr:::Variable("double", list(2L))
  scope[["y"]] <- quickr:::Variable("double", list(3L))
  args <- list(quote(x), quote(y))
  result <- quickr:::infer_dest_outer(args, scope)
  expect_true(S7::S7_inherits(result, quickr:::Variable))
  expect_equal(result@dims, list(2L, 3L))
})

test_that("infer_dest_triangular returns NULL for < 2 args", {
  scope <- quickr:::new_scope(function() NULL)
  args <- list(quote(A))
  result <- quickr:::infer_dest_triangular(args, scope)
  expect_null(result)
})

test_that("infer_dest_triangular returns NULL when A or B not resolved", {
  scope <- quickr:::new_scope(function() NULL)
  args <- list(quote(A), quote(B))
  result <- quickr:::infer_dest_triangular(args, scope)
  expect_null(result)
})

test_that("infer_dest_triangular returns NULL for wrong ranks", {
  scope <- quickr:::new_scope(function() NULL)
  # A not rank 2
  scope[["A"]] <- quickr:::Variable("double", list(2L))
  scope[["B"]] <- quickr:::Variable("double", list(2L))
  args <- list(quote(A), quote(B))
  result <- quickr:::infer_dest_triangular(args, scope)
  expect_null(result)

  # B rank 0
  scope[["A"]] <- quickr:::Variable("double", list(2L, 2L))
  scope[["B"]] <- quickr:::Variable("double")
  result <- quickr:::infer_dest_triangular(args, scope)
  expect_null(result)

  # B rank > 2
  scope[["B"]] <- quickr:::Variable("double", list(2L, 2L, 2L))
  result <- quickr:::infer_dest_triangular(args, scope)
  expect_null(result)
})

test_that("infer_dest_triangular returns correct dims for valid inputs", {
  scope <- quickr:::new_scope(function() NULL)
  scope[["A"]] <- quickr:::Variable("double", list(2L, 2L))
  scope[["B"]] <- quickr:::Variable("double", list(2L, 3L))
  args <- list(quote(A), quote(B))
  result <- quickr:::infer_dest_triangular(args, scope)
  expect_true(S7::S7_inherits(result, quickr:::Variable))
  expect_equal(result@dims, list(2L, 3L))
})
