test_that("matrix multiplication matches R for common shapes", {
  mat_mat <- function(mat_A, mat_B) {
    declare(
      type(mat_A = double(4, 3)),
      type(mat_B = double(3, 5))
    )
    mat_A %*% mat_B
  }

  mat_mat_square <- function(mat_A, mat_B) {
    declare(
      type(mat_A = double(3, 3)),
      type(mat_B = double(3, 3))
    )
    mat_A %*% mat_B
  }

  vec_mat <- function(vec, mat_A) {
    declare(
      type(vec = double(3)),
      type(mat_A = double(3, 4))
    )
    vec %*% mat_A
  }

  mat_vec <- function(mat_A, vec) {
    declare(
      type(mat_A = double(3, 4)),
      type(vec = double(4))
    )
    mat_A %*% vec
  }

  vec_vec <- function(vec_A, vec_B) {
    declare(
      type(vec_A = double(3)),
      type(vec_B = double(3))
    )
    vec_A %*% vec_B
  }

  set.seed(1)
  mat_A <- matrix(rnorm(4 * 3), nrow = 4)
  mat_B <- matrix(rnorm(3 * 5), nrow = 3)
  mat_sq_A <- matrix(rnorm(3 * 3), nrow = 3)
  mat_sq_B <- matrix(rnorm(3 * 3), nrow = 3)
  vec_3 <- rnorm(3)
  mat_3x4 <- matrix(rnorm(3 * 4), nrow = 3)
  vec_4 <- rnorm(4)

  expect_quick_equal(mat_mat, list(mat_A = mat_A, mat_B = mat_B))
  expect_quick_equal(
    mat_mat_square,
    list(mat_A = mat_sq_A, mat_B = mat_sq_B)
  )
  expect_quick_equal(vec_mat, list(vec = vec_3, mat_A = mat_3x4))
  expect_quick_equal(mat_vec, list(mat_A = mat_3x4, vec = vec_4))
  expect_quick_equal(vec_vec, list(vec_A = vec_3, vec_B = vec_3))
})

test_that("matrix multiplication handles transposed operands", {
  matmul_t_left <- function(x, y) {
    declare(
      type(x = double(4, 3)),
      type(y = double(4, 5))
    )
    t(x) %*% y
  }

  matmul_t_right <- function(x, y) {
    declare(
      type(x = double(4, 3)),
      type(y = double(5, 3))
    )
    x %*% t(y)
  }

  matmul_t_both <- function(x, y) {
    declare(
      type(x = double(4, 3)),
      type(y = double(5, 4))
    )
    t(x) %*% t(y)
  }

  set.seed(4)
  x <- matrix(rnorm(4 * 3), nrow = 4)
  y_left <- matrix(rnorm(4 * 5), nrow = 4)
  y_right <- matrix(rnorm(5 * 3), nrow = 5)
  y_both <- matrix(rnorm(5 * 4), nrow = 5)

  expect_quick_equal(matmul_t_left, list(x = x, y = y_left))
  expect_quick_equal(matmul_t_right, list(x = x, y = y_right))
  expect_quick_equal(matmul_t_both, list(x = x, y = y_both))
})

test_that("matrix multiplication handles chained mixes", {
  chain_mix <- function(a, b, c) {
    declare(
      type(a = double(4, 3)),
      type(b = double(5, 3)),
      type(c = double(5, 5))
    )
    (a %*% t(b)) %*% c + 0.25 * (a %*% t(b))
  }

  set.seed(7)
  a <- matrix(rnorm(4 * 3), nrow = 4)
  b <- matrix(rnorm(5 * 3), nrow = 5)
  c <- matrix(rnorm(5 * 5), nrow = 5)

  expect_quick_equal(chain_mix, list(a = a, b = b, c = c))
})

test_that("matrix multiplication handles 1x1 and 1xN/Nx1 shapes", {
  mat_mat_1x1 <- function(a, b) {
    declare(
      type(a = double(1, 1)),
      type(b = double(1, 1))
    )
    a %*% b
  }

  mat_row_col <- function(row, col) {
    declare(
      type(row = double(1, 4)),
      type(col = double(4, 1))
    )
    row %*% col
  }

  mat_col_row <- function(col, row) {
    declare(
      type(col = double(4, 1)),
      type(row = double(1, 4))
    )
    col %*% row
  }

  set.seed(5)
  a <- matrix(rnorm(1), nrow = 1)
  b <- matrix(rnorm(1), nrow = 1)
  row <- matrix(rnorm(4), nrow = 1)
  col <- matrix(rnorm(4), nrow = 4)

  expect_quick_equal(mat_mat_1x1, list(a = a, b = b))
  expect_quick_equal(mat_row_col, list(row = row, col = col))
  expect_quick_equal(mat_col_row, list(col = col, row = row))
})

test_that("matrix multiplication handles t(vec) orientation", {
  tvec_mat <- function(vec, mat_A) {
    declare(
      type(vec = double(3)),
      type(mat_A = double(3, 4))
    )
    t(vec) %*% mat_A
  }

  mat_tvec <- function(mat_A, vec) {
    declare(
      type(mat_A = double(4, 1)),
      type(vec = double(4))
    )
    mat_A %*% t(vec)
  }

  set.seed(6)
  vec <- rnorm(3)
  mat_A <- matrix(rnorm(3 * 4), nrow = 3)
  vec_long <- rnorm(4)
  mat_B <- matrix(rnorm(4), nrow = 4)

  expect_quick_equal(tvec_mat, list(vec = vec, mat_A = mat_A))
  expect_quick_equal(mat_tvec, list(mat_A = mat_B, vec = vec_long))
})

test_that("matrix multiplication errors on non-conformable arguments", {
  matmul_bad <- function(x, y) {
    declare(
      type(x = double(2, 3)),
      type(y = double(2, 4))
    )
    x %*% y
  }

  x <- matrix(rnorm(2 * 3), nrow = 2)
  y <- matrix(rnorm(2 * 4), nrow = 2)

  expect_error(matmul_bad(x, y), "non-conformable")
  expect_error(quick(matmul_bad), "non-conformable arguments in %*%")
})

test_that("matrix multiplication rejects incompatible destinations", {
  dest_mismatch <- function() {
    declare(type(x = double(2)))
    a <- matrix(1.5, 2L, 2L)
    x <- a %*% a
    x
  }

  expect_error(quick(dest_mismatch), "incompatible rank for %\\*%")
})

test_that("crossprod and tcrossprod match R", {
  cross_fun <- function(x, y) {
    declare(
      type(x = double(6, 4)),
      type(y = double(6, 4))
    )
    crossprod(x, y)
  }

  tcross_fun <- function(x, y) {
    declare(
      type(x = double(6, 4)),
      type(y = double(6, 4))
    )
    tcrossprod(x, y)
  }

  set.seed(2)
  x <- matrix(rnorm(6 * 4), nrow = 6)
  y <- matrix(rnorm(6 * 4), nrow = 6)

  expect_quick_equal(cross_fun, list(x = x, y = y))
  expect_quick_equal(tcross_fun, list(x = x, y = y))
})

test_that("single-argument crossprod/tcrossprod match R", {
  cross_single <- function(x) {
    declare(type(x = double(5, 4)))
    crossprod(x)
  }

  tcross_single <- function(x) {
    declare(type(x = double(5, 4)))
    tcrossprod(x)
  }

  cross_vec <- function(x) {
    declare(type(x = double(5)))
    crossprod(x)
  }

  tcross_vec <- function(x) {
    declare(type(x = double(5)))
    tcrossprod(x)
  }

  set.seed(3)
  x <- matrix(rnorm(5 * 4), nrow = 5)
  v <- rnorm(5)

  expect_quick_equal(cross_single, list(x = x))
  expect_quick_equal(tcross_single, list(x = x))
  expect_quick_equal(cross_vec, list(x = v))
  expect_quick_equal(tcross_vec, list(x = v))
})

test_that("outer supports multiplication and %o%", {
  outer_default <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    outer(x, y)
  }

  outer_mul <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    outer(x, y, "*")
  }

  outer_op <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    x %o% y
  }

  set.seed(10)
  x <- rnorm(3)
  y <- rnorm(4)

  expect_quick_equal(outer_default, list(x = x, y = y))
  expect_quick_equal(outer_mul, list(x = x, y = y))
  expect_quick_equal(outer_op, list(x = x, y = y))
})

test_that("blas operations support preallocated outputs", {
  matmul_out <- function(A, B) {
    declare(
      type(A = double(2, 3)),
      type(B = double(3, 2)),
      type(out = double(2, 2))
    )
    out <- A %*% B
    out
  }

  crossprod_out <- function(x) {
    declare(
      type(x = double(4, 3)),
      type(out = double(3, 3))
    )
    out <- crossprod(x)
    out
  }

  outer_out <- function(x, y) {
    declare(
      type(x = double(2)),
      type(y = double(3)),
      type(out = double(2, 3))
    )
    out <- outer(x, y)
    out
  }

  set.seed(12)
  A <- matrix(rnorm(2 * 3), nrow = 2)
  B <- matrix(rnorm(3 * 2), nrow = 3)
  X <- matrix(rnorm(4 * 3), nrow = 4)
  x <- rnorm(2)
  y <- rnorm(3)

  expect_quick_equal(matmul_out, list(A = A, B = B))
  expect_quick_equal(crossprod_out, list(x = X))
  expect_quick_equal(outer_out, list(x = x, y = y))
})

test_that("outer errors on unsupported FUN", {
  outer_add <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    outer(x, y, "+")
  }

  set.seed(1)
  x <- rnorm(3)
  y <- rnorm(4)

  expect_error(quick(outer_add), "outer\\(\\) only supports FUN = \"\\*\"")
})

test_that("forwardsolve and backsolve match R", {
  forward_vec <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4))
    )
    forwardsolve(L, b)
  }

  forward_mat <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4, 2))
    )
    forwardsolve(L, b)
  }

  back_vec <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4))
    )
    backsolve(U, b)
  }

  back_mat <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4, 2))
    )
    backsolve(U, b)
  }

  back_transpose <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4))
    )
    backsolve(U, b, transpose = TRUE)
  }

  forward_upper <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4))
    )
    forwardsolve(U, b, upper.tri = TRUE)
  }

  forward_transpose <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4))
    )
    forwardsolve(L, b, transpose = TRUE)
  }

  back_lower <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4))
    )
    backsolve(L, b, upper.tri = FALSE)
  }

  set.seed(11)
  base <- matrix(rnorm(16), nrow = 4)
  L <- base
  L[upper.tri(L)] <- 0
  diag(L) <- diag(L) + 5
  U <- base
  U[lower.tri(U)] <- 0
  diag(U) <- diag(U) + 5
  b_vec <- rnorm(4)
  b_mat <- matrix(rnorm(8), nrow = 4)

  expect_quick_equal(forward_vec, list(L = L, b = b_vec))
  expect_quick_equal(forward_mat, list(L = L, b = b_mat))
  expect_quick_equal(back_vec, list(U = U, b = b_vec))
  expect_quick_equal(back_mat, list(U = U, b = b_mat))
  expect_quick_equal(back_transpose, list(U = U, b = b_vec))
  expect_quick_equal(forward_upper, list(U = U, b = b_vec))
  expect_quick_equal(forward_transpose, list(L = L, b = b_vec))
  expect_quick_equal(back_lower, list(L = L, b = b_vec))
})
