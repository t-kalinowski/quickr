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

test_that("ifelse with statically mismatched branch lengths is a compile error", {
  fn <- function(c, a) {
    declare(type(c = logical(3)), type(a = double(2)))
    ifelse(c, a, 0)
  }
  expect_error(quick(fn), "R-style recycling is not supported")
})

test_that("ifelse guards unknown branch lengths at runtime", {
  fn <- function(c, a, b) {
    declare(type(c = logical(NA)), type(a = double(NA)), type(b = double(NA)))
    ifelse(c, a, b)
  }
  # locks the size guards: a bare merge() with runtime-mismatched
  # assumed-shape vectors read past the shorter branch (returned garbage
  # like 4.65e-310 where R recycles)
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  cc <- c(TRUE, FALSE, TRUE)
  a <- c(10, 20, 30)
  b <- c(1, 2, 3)
  expect_identical(qfn(cc, a, b), ifelse(cc, a, b))

  expect_error(qfn(cc, c(10, 20), b), "match the shape of `test`")
  expect_error(qfn(cc, a, c(1, 2, 3, 4)), "match the shape of `test`")
})
