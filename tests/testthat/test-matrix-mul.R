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

  expect_quick_identical(mat_mat, list(mat_A = mat_A, mat_B = mat_B))
  expect_quick_identical(
    mat_mat_square,
    list(mat_A = mat_sq_A, mat_B = mat_sq_B)
  )
  expect_quick_identical(vec_mat, list(vec = vec_3, mat_A = mat_3x4))
  expect_quick_identical(mat_vec, list(mat_A = mat_3x4, vec = vec_4))
  expect_quick_identical(vec_vec, list(vec_A = vec_3, vec_B = vec_3))
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

  expect_quick_identical(matmul_t_left, list(x = x, y = y_left))
  expect_quick_identical(matmul_t_right, list(x = x, y = y_right))
  expect_quick_identical(matmul_t_both, list(x = x, y = y_both))
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

  expect_quick_identical(cross_fun, list(x = x, y = y))
  expect_quick_identical(tcross_fun, list(x = x, y = y))
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

  expect_quick_identical(cross_single, list(x = x))
  expect_quick_identical(tcross_single, list(x = x))
  expect_quick_identical(cross_vec, list(x = v))
  expect_quick_identical(tcross_vec, list(x = v))
})
