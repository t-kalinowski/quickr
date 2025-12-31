test_that("matrix multiplication matches R for small inputs", {
  matmul <- function(x, y, n, k, m) {
    declare(
      type(x = double(n, k)),
      type(y = double(k, m)),
      type(n = integer(1)),
      type(k = integer(1)),
      type(m = integer(1))
    )
    x %*% y
  }

  set.seed(1)
  n <- 5L
  k <- 3L
  m <- 4L
  x <- matrix(rnorm(n * k), nrow = n)
  y <- matrix(rnorm(k * m), nrow = k)

  expect_quick_identical(
    matmul,
    list(x = x, y = y, n = n, k = k, m = m)
  )
})

test_that("matrix multiplication handles transposed operands", {
  matmul_t_left <- function(x, y, n, k, m) {
    declare(
      type(x = double(n, k)),
      type(y = double(n, m)),
      type(n = integer(1)),
      type(k = integer(1)),
      type(m = integer(1))
    )
    t(x) %*% y
  }

  matmul_t_right <- function(x, y, n, k, m) {
    declare(
      type(x = double(n, k)),
      type(y = double(m, k)),
      type(n = integer(1)),
      type(k = integer(1)),
      type(m = integer(1))
    )
    x %*% t(y)
  }

  set.seed(4)
  n <- 4L
  k <- 3L
  m <- 5L
  x <- matrix(rnorm(n * k), nrow = n)
  y_left <- matrix(rnorm(n * m), nrow = n)
  y_right <- matrix(rnorm(m * k), nrow = m)

  expect_quick_identical(
    matmul_t_left,
    list(x = x, y = y_left, n = n, k = k, m = m)
  )
  expect_quick_identical(
    matmul_t_right,
    list(x = x, y = y_right, n = n, k = k, m = m)
  )
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
  cross_fun <- function(x, y, n, k) {
    declare(
      type(x = double(n, k)),
      type(y = double(n, k)),
      type(n = integer(1)),
      type(k = integer(1))
    )
    crossprod(x, y)
  }

  tcross_fun <- function(x, y, n, k) {
    declare(
      type(x = double(n, k)),
      type(y = double(n, k)),
      type(n = integer(1)),
      type(k = integer(1))
    )
    tcrossprod(x, y)
  }

  set.seed(2)
  n <- 6L
  k <- 4L
  x <- matrix(rnorm(n * k), nrow = n)
  y <- matrix(rnorm(n * k), nrow = n)

  expect_quick_identical(cross_fun, list(x = x, y = y, n = n, k = k))
  expect_quick_identical(tcross_fun, list(x = x, y = y, n = n, k = k))
})

test_that("single-argument crossprod/tcrossprod match R", {
  cross_single <- function(x, n, k) {
    declare(
      type(x = double(n, k)),
      type(n = integer(1)),
      type(k = integer(1))
    )
    crossprod(x)
  }

  tcross_single <- function(x, n, k) {
    declare(
      type(x = double(n, k)),
      type(n = integer(1)),
      type(k = integer(1))
    )
    tcrossprod(x)
  }

  cross_vec <- function(x, n) {
    declare(
      type(x = double(n)),
      type(n = integer(1))
    )
    crossprod(x)
  }

  tcross_vec <- function(x, n) {
    declare(
      type(x = double(n)),
      type(n = integer(1))
    )
    tcrossprod(x)
  }

  set.seed(3)
  n <- 5L
  k <- 4L
  x <- matrix(rnorm(n * k), nrow = n)
  v <- rnorm(n)

  expect_quick_identical(cross_single, list(x = x, n = n, k = k))
  expect_quick_identical(tcross_single, list(x = x, n = n, k = k))
  expect_quick_identical(cross_vec, list(x = v, n = n))
  expect_quick_identical(tcross_vec, list(x = v, n = n))
})
