# Float-to-int conversion semantics (truncation vs floor/ceiling).

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
