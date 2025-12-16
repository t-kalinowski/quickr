# Tests for dynamic dimension expressions evaluated in Fortran environment

test_that("arithmetic expressions in dimensions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    x <- double(n * 2L)
    y <- double(n - 1L)
    length(x) + length(y)
  }
  expect_translation_snapshots(fn)

  expect_identical(fn(4L), 11L)
  expect_identical(fn(7L), 20L)
  expect_quick_identical(fn, 4L, 7L)
})


test_that("integer division and modulus in dimensions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- double(n %/% 2L + n %% 2L)
    length(out)
  }
  expect_translation_snapshots(fn)

  expect_identical(fn(5L), 3L)
  expect_identical(fn(8L), 4L)
  expect_quick_identical(fn, 5L, 8L)
})


test_that("matrix dimension expressions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- matrix(1, n + 1L, n %/% 2L + 1L)
    dim(out)
  }
  expect_translation_snapshots(fn)

  expect_identical(fn(3L), c(4L, 2L))
  expect_identical(fn(6L), c(7L, 4L))
  expect_quick_identical(fn, 3L, 6L)
})
