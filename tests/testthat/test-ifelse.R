# Unit test for ifelse translation

skip_on_cran()

test_that("ifelse", {
  fn <- function(a, b) {
    declare(
      type(a = integer(n)),
      type(b = integer(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_identical(fn, list(-10:10, integer(21)))

  # double version of above
  fn <- function(a, b) {
    declare(
      type(a = double(n)),
      type(b = double(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_equal(fn, list(seq(-5, 5, length.out = 20), double(20)))
})

test_that("ifelse promotes branches and shapes like test", {
  fn <- function(c, a) {
    declare(type(c = logical(n)), type(a = double(n)))
    ifelse(c, 1L, a)
  }
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(c(TRUE, FALSE, TRUE), c(2, 4, 6)))

  # logical branches join as logical
  fn2 <- function(c, a) {
    declare(type(c = logical(n)), type(a = logical(n)))
    ifelse(c, FALSE, a)
  }
  expect_quick_equal(fn2, list(c(TRUE, FALSE, TRUE), c(TRUE, TRUE, FALSE)))
})

test_that("ifelse with scalar test and array branch errors cleanly", {
  fn <- function(c, a) {
    declare(type(c = logical(1)), type(a = double(n)))
    ifelse(c, a, 0)
  }
  expect_error(quick(fn), "shape of `test`")
})
