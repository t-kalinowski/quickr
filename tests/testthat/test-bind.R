# Unit tests for cbind() and rbind()

expect_bind_equal <- function(fn, ...) {
  qfn := quick(fn)
  args_list <- rlang::list2(...)
  args_list <- lapply(args_list, function(x) if (!is.list(x)) list(x) else x)

  for (args in args_list) {
    fn_res <- do.call(fn, args)
    qfn_res <- do.call(qfn, args)
    expect_identical(dim(fn_res), dim(qfn_res))
    expect_equal(unname(fn_res), unname(qfn_res))
    expect_identical(typeof(fn_res), typeof(qfn_res))
  }
}

test_that("cbind binds vectors and matrices with scalar recycling", {
  cbind_vec <- function(x, y, s) {
    declare(type(x = double(n)), type(y = double(n)), type(s = double(1)))
    cbind(x, y, s)
  }

  set.seed(1)
  x <- runif(4)
  y <- runif(4)
  s <- 2.5
  expect_bind_equal(cbind_vec, list(x, y, s))

  cbind_mat <- function(A, v) {
    declare(type(A = double(n, m)), type(v = double(n)))
    cbind(A, v)
  }

  A <- matrix(runif(6), nrow = 3)
  v <- runif(3)
  expect_bind_equal(cbind_mat, list(A, v))

  cbind_int <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(n)))
    cbind(x, y)
  }

  x_int <- 1:3
  y_int <- 4:6
  expect_bind_equal(cbind_int, list(x_int, y_int))
})

test_that("rbind binds vectors and matrices with scalar recycling", {
  rbind_vec <- function(x, y, s) {
    declare(type(x = double(n)), type(y = double(n)), type(s = double(1)))
    rbind(x, y, s)
  }

  set.seed(2)
  x <- runif(5)
  y <- runif(5)
  s <- -1.25
  expect_bind_equal(rbind_vec, list(x, y, s))

  rbind_mat <- function(A, v) {
    declare(type(A = double(n, m)), type(v = double(m)))
    rbind(A, v)
  }

  A <- matrix(runif(6), nrow = 2)
  v <- runif(3)
  expect_bind_equal(rbind_mat, list(A, v))

  rbind_int <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(n)))
    rbind(x, y)
  }

  x_int <- 1:4
  y_int <- 5:8
  expect_bind_equal(rbind_int, list(x_int, y_int))
})

test_that("cbind/rbind enforce common lengths", {
  bad_cbind <- function(x, y) {
    declare(type(x = double(2)), type(y = double(3)))
    cbind(x, y)
  }

  expect_error(
    quick(bad_cbind),
    "common row count",
    fixed = TRUE
  )

  bad_rbind <- function(A, B) {
    declare(type(A = double(2, 3)), type(B = double(2, 4)))
    rbind(A, B)
  }

  expect_error(
    quick(bad_rbind),
    "common column count",
    fixed = TRUE
  )
})

test_that("cbind/rbind reject rank > 2 inputs with clear errors", {
  capture_bind_error <- function(expr) {
    tryCatch(expr, error = function(e) cat(conditionMessage(e), "\n"))
  }

  bad_cbind <- function(x) {
    declare(type(x = double(2, 2, 2)))
    cbind(x)
  }

  bad_rbind <- function(x) {
    declare(type(x = double(2, 2, 2)))
    rbind(x)
  }

  expect_snapshot({
    capture_bind_error(r2f(bad_cbind))
    capture_bind_error(r2f(bad_rbind))
  })
})

test_that("cbind/rbind handle mixed rank-2, rank-1, and rank-0 inputs", {
  cbind_mixed <- function(A, v1, s1, B, s2, v2, C, v3, s3) {
    declare(
      type(A = double(n, 2)),
      type(B = double(n, 1)),
      type(C = double(n, 3)),
      type(v1 = double(n)),
      type(v2 = double(n)),
      type(v3 = double(n)),
      type(s1 = double(1)),
      type(s2 = double(1)),
      type(s3 = double(1))
    )
    cbind(A, v1, s1, B, s2, v2, C, v3, s3)
  }

  rbind_mixed <- function(A, v1, s1, B, s2, v2, C, v3, s3) {
    declare(
      type(A = double(2, m)),
      type(B = double(1, m)),
      type(C = double(3, m)),
      type(v1 = double(m)),
      type(v2 = double(m)),
      type(v3 = double(m)),
      type(s1 = double(1)),
      type(s2 = double(1)),
      type(s3 = double(1))
    )
    rbind(A, v1, s1, B, s2, v2, C, v3, s3)
  }

  set.seed(42)
  n <- 2L
  A <- matrix(runif(n * 2L), nrow = n)
  B <- matrix(runif(n * 1L), nrow = n)
  C <- matrix(runif(n * 3L), nrow = n)
  v1 <- runif(n)
  v2 <- runif(n)
  v3 <- runif(n)
  s1 <- -0.5
  s2 <- 1.25
  s3 <- 0.0

  expect_bind_equal(
    cbind_mixed,
    list(A, v1, s1, B, s2, v2, C, v3, s3)
  )

  m <- 3L
  A2 <- matrix(runif(2L * m), nrow = 2L)
  B2 <- matrix(runif(1L * m), nrow = 1L)
  C2 <- matrix(runif(3L * m), nrow = 3L)
  w1 <- runif(m)
  w2 <- runif(m)
  w3 <- runif(m)
  t1 <- 2.0
  t2 <- -1.0
  t3 <- 0.5

  expect_bind_equal(
    rbind_mixed,
    list(A2, w1, t1, B2, t2, w2, C2, w3, t3)
  )
})
