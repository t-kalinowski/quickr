# c(matrix), as.vector(), and as.integer()/as.double() of arrays all
# flatten column-major, matching R's drop-dims semantics.

skip_on_cran()

test_that("c() flattens matrix and array arguments column-major", {
  c_mat <- function(m) {
    declare(type(m = double(2, 3)))
    c(m)
  }
  expect_quick_identical(c_mat, list(matrix(as.double(1:6), 2, 3)))

  c_mixed <- function(m, v) {
    declare(type(m = double(2, 2)), type(v = double(3)))
    c(m, v)
  }
  expect_quick_identical(
    c_mixed,
    list(matrix(as.double(1:4), 2, 2), c(7, 8, 9))
  )

  c_two_mats <- function(a, b) {
    declare(type(a = integer(2, 2)), type(b = integer(1, 3)))
    c(a, b)
  }
  expect_quick_identical(
    c_two_mats,
    list(matrix(1:4, 2, 2), matrix(7:9, 1, 3))
  )
})

test_that("as.vector() drops dimensions, preserving or coercing the mode", {
  # default mode preserves type
  av_dbl <- function(m) {
    declare(type(m = double(2, 3)))
    as.vector(m)
  }
  expect_quick_identical(av_dbl, list(matrix(as.double(1:6), 2, 3)))

  av_lgl <- function(m) {
    declare(type(m = logical(2, 2)))
    as.vector(m)
  }
  expect_quick_identical(av_lgl, list(matrix(c(TRUE, FALSE, TRUE, TRUE), 2, 2)))

  # mode = "double" delegates to as.double()
  av_coerce <- function(m) {
    declare(type(m = integer(2, 2)))
    as.vector(m, mode = "double")
  }
  expect_quick_identical(av_coerce, list(matrix(1:4, 2, 2)))
})

test_that("as.vector() refuses unsupported modes and non-constant modes", {
  bad_mode <- function(m) {
    declare(type(m = double(2)))
    as.vector(m, mode = "list")
  }
  expect_error(quick(bad_mode), "as.vector() does not support mode", fixed = TRUE)
})

test_that("as.integer() drops dimensions for an int-backed logical matrix", {
  # External logical args are integer-backed; the flatten must still apply
  # (it used to be skipped by an early return, keeping the matrix dims).
  fn <- function(m) {
    declare(type(m = logical(2, 3)))
    as.integer(m)
  }
  expect_quick_identical(
    fn,
    list(matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), 2, 3))
  )
})
