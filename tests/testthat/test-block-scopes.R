test_that("array-expression subscripting hoists into a block-scoped temp", {
  fn <- function(x) {
    declare(type(x = double(3, 4)))
    out <- ifelse((x > 0.0)[2, 3], 1.0, 0.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Fortran disallows (expr)(i,j); quickr uses a block-local temp array."
  )

  set.seed(1)
  x <- matrix(runif(12) - 0.5, 3, 4)
  expect_quick_identical(fn, list(x))
})

test_that("block-scoped temps work for rank-3 array expression subscripting", {
  fn <- function(x) {
    declare(type(x = double(2, 3, 4)))
    out <- ifelse((x > 0.0)[2, 3, 4], 1.0, 0.0)
    out
  }

  set.seed(1)
  x <- array(runif(24) - 0.5, dim = c(2, 3, 4))
  expect_quick_identical(fn, list(x))
})

test_that("block-scoped temps allocate on the heap for runtime shapes", {
  fn <- function(x) {
    declare(type(x = double(n, m)))
    out <- ifelse((x > 0.0)[1, 1], 1.0, 0.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = paste(
      "Block temps with runtime sizes are allocatable so flang doesn't",
      "stack-allocate large work arrays."
    )
  )

  set.seed(1)
  x <- matrix(runif(64) - 0.5, 8, 8)
  expect_quick_identical(fn, list(x))
})

test_that("block-scoped temps work for deferred-shape intermediates", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- (x[x > 0.0])[1]
    out
  }

  x <- c(-1.0, 2.0, -3.0)
  expect_quick_identical(fn, list(x))
})

test_that("generated temps do not shadow user variables", {
  block_fn <- function(Btmp1.) {
    declare(type(Btmp1. = double(1)))
    Btmp1. + runif(1)
  }
  root_fn <- function(Tmp1.) {
    declare(type(Tmp1. = double(2)))
    Tmp1. + runif(2)
  }
  block_qfn <- quick(block_fn)
  root_qfn <- quick(root_fn)

  set.seed(144)
  expected <- block_fn(2)
  expected_next <- runif(1)

  set.seed(144)
  actual <- block_qfn(2)
  actual_next <- runif(1)

  expect_identical(actual, expected)
  expect_identical(actual_next, expected_next)

  set.seed(145)
  expected <- root_fn(c(2, 3))
  expected_next <- runif(1)

  set.seed(145)
  actual <- root_qfn(c(2, 3))
  actual_next <- runif(1)

  expect_identical(actual, expected)
  expect_identical(actual_next, expected_next)
})

test_that("later user variables do not shadow generated temps", {
  root_fn <- function(n) {
    declare(type(n = integer(1)))
    z <- seq_len(n)
    Tmp1. <- 2L
    sum(z) + Tmp1.
  }
  block_fn <- function(x) {
    declare(type(x = double(2)))
    Btmp1. <- x + numeric(2)
    sum(Btmp1.)
  }

  expect_quick_identical(root_fn, list(2L))
  expect_quick_identical(block_fn, list(c(1, 2)))
})
