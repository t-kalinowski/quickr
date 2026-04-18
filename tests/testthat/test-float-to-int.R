# Float-to-int conversion semantics (truncation vs floor/ceiling).

skip_on_cran()

test_that("as.integer(double) truncates toward zero", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- as.integer(x)
    out
  }

  expect_translation_snapshots(fn)

  x <- c(-2.9, -2.1, -1.9, -1.1, -0.9, -0.1, 0, 0.1, 0.9, 1.1, 1.9, 2.1)
  expect_quick_identical(fn, x)
})

test_that("trunc() returns double and truncates toward zero", {
  fn_d <- function(x) {
    declare(type(x = double(NA)))
    out <- trunc(x)
    out
  }

  expect_translation_snapshots(fn_d)

  x <- c(-2.9, -2.1, -1.9, -1.1, -0.9, -0.1, 0, 0.1, 0.9, 1.1, 1.9, 2.1)
  expect_quick_equal(fn_d, x)

  fn_i <- function(x) {
    declare(type(x = integer(NA)))
    out <- trunc(x)
    out
  }

  expect_translation_snapshots(fn_i)

  xi <- as.integer(c(-3, -1, 0, 2, 5))
  expect_quick_equal(fn_i, xi)
  expect_identical(typeof(fn_i(xi)), "double")
})

test_that("as.integer(integer division) truncates toward zero", {
  fn <- function(a, b) {
    declare(type(a = integer(n)), type(b = integer(n)))
    out <- as.integer(a / b)
    out
  }

  expect_translation_snapshots(fn)

  a <- as.integer(c(7, -7, 7, -7, 3, -3))
  b <- as.integer(c(2, 2, -2, -2, 2, 2))
  expect_quick_identical(fn, list(a, b))
})
