# c(matrix), as.vector(), and as.integer()/as.double() of arrays all
# flatten column-major, matching R's drop-dims semantics.

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
  missing_x <- function() {
    as.vector()
  }
  expect_error(
    quick(missing_x),
    "as.vector() expects `x`",
    fixed = TRUE
  )

  bad_mode <- function(m) {
    declare(type(m = double(2)))
    as.vector(m, mode = "list")
  }
  expect_error(
    quick(bad_mode),
    "as.vector() does not support mode",
    fixed = TRUE
  )
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

test_that("flattened logical matrix expressions support logical consumers", {
  coerce <- function(x) {
    declare(type(x = logical(2)))
    as.integer(as.vector(diag(x, 2L, 2L)))
  }
  concatenate <- function(x) {
    declare(type(x = logical(2)))
    c(diag(x, 2L, 2L), TRUE)
  }
  reduce <- function(x) {
    declare(type(x = logical(2)))
    any(as.vector(diag(x, 2L, 2L)))
  }
  expect_quick_identical(coerce, c(TRUE, FALSE))
  expect_quick_identical(concatenate, c(TRUE, FALSE))
  expect_quick_identical(reduce, c(TRUE, FALSE), c(FALSE, FALSE))
})

test_that("flattened singleton matrices broadcast as scalars", {
  fn <- function(x) {
    declare(type(x = double(1, 1)))
    as.vector(x) + c(1, 2, 3)
  }
  expect_quick_identical(fn, list(matrix(2, 1, 1)))
})

test_that("flattened singleton expressions and arrays broadcast as scalars", {
  fn <- function(x) {
    declare(type(x = double(1, 1, 1)))
    as.integer(x + 1) + c(1L, 2L, 3L)
  }
  expect_quick_identical(fn, list(array(2.5, c(1, 1, 1))))

  logical_fn <- function(x) {
    declare(type(x = logical(1, 1)))
    as.vector(x) & c(TRUE, FALSE, TRUE)
  }
  expect_quick_identical(logical_fn, list(matrix(TRUE, 1, 1)))
})

test_that("as.vector numeric coercions ignore local coercion bindings", {
  for (mode in c("integer", "double", "numeric")) {
    fn <- eval(substitute(
      function(x) {
        declare(type(x = double(2)))
        as.integer <- function(y) y + 100
        as.double <- function(y) y + 100
        as.vector(x, mode = MODE)
      },
      list(MODE = mode)
    ))
    expect_quick_identical(fn, c(1.5, 2.5))
  }
})

test_that("singleton matrix concatenation uses scalar storage", {
  fn <- function(m) {
    declare(type(m = double(1, 1)))
    c(m)
  }
  expect_quick_identical(fn, list(matrix(2, 1, 1)))

  with_empty <- function(m) {
    declare(type(m = double(1, 1)))
    c(integer(0), m, numeric(0))
  }
  expect_quick_identical(with_empty, list(matrix(2, 1, 1)))

  logical_fn <- function(m) {
    declare(type(m = logical(1, 1)))
    c(m)
  }
  expect_quick_identical(logical_fn, list(matrix(TRUE, 1, 1)))
})
