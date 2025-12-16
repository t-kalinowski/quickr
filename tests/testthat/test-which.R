# Unit tests for which.max and related functions

test_that("which.max", {
  fn <- function(a) {
    declare(type(a = double(NA)))

    out <- which.max(a)
    out
  }

  expect_snapshot(r2f(fn))

  a1 <- c(1.0, 3.0, 2.0)
  a2 <- c(-1.0, -2.0, -0.5)
  a3 <- c(1.0, 3.0, 3.0)

  expect_identical(fn(a1), 2L)
  expect_identical(fn(a2), 3L)
  expect_identical(fn(a3), 2L)
  expect_quick_identical(fn, a1, a2, a3)

  # bench::mark(fn(x), {anyNA(x); qfn(x)}) -> r; print(r); plot(r);

  # qfn(-c(1, 2, 3, 2, 1))
  # fn(-c(1, 2, 3, 2, 1))

  # ---------------------

  # now with logical
  fn <- function(a) {
    declare(type(a = logical(NA)))

    out <- which.max(a)
    out
  }

  expect_snapshot(r2f(fn))

  lgl1 <- c(FALSE, TRUE, FALSE)
  lgl2 <- c(FALSE, FALSE, TRUE)
  lgl3 <- c(FALSE, FALSE, FALSE)

  expect_identical(fn(lgl1), 2L)
  expect_identical(fn(lgl2), 3L)
  expect_identical(fn(lgl3), 1L)
  expect_quick_identical(fn, lgl1, lgl2, lgl3)
})

test_that("which.max/which.min on local logical arrays", {
  fn_max <- function(x) {
    declare(type(x = double(NA)))

    lgl <- x > 0
    out <- which.max(lgl)
    out
  }

  x1 <- c(-1, -2, -3)
  x2 <- c(-1, 2, 3)
  x3 <- c(-1, 0, 2)

  expect_identical(fn_max(x1), 1L)
  expect_identical(fn_max(x2), 2L)
  expect_identical(fn_max(x3), 3L)
  expect_quick_identical(fn_max, x1, x2, x3)

  fn_min <- function(x) {
    declare(type(x = double(NA)))

    lgl <- x > 0
    out <- which.min(lgl)
    out
  }

  y1 <- c(1, 2, 3) # all TRUE -> 1 (R semantics)
  y2 <- c(1, -1, 2)
  y3 <- c(1, 2, -1)

  expect_identical(fn_min(y1), 1L)
  expect_identical(fn_min(y2), 2L)
  expect_identical(fn_min(y3), 3L)
  expect_quick_identical(fn_min, y1, y2, y3)
})

#   qfn_find_loc_int <- quick("fn", fn)
#   qfn_find_loc_lgl <- quick("fn", fn)
#
#
#   bench::mark(fn(x), {anyNA(x); qfn(x)}) -> r; print(r); plot(r);
#   bench::mark(fn(x), qfn(x)) -> r; print(r); plot(r);
#   bench::mark(qfn_find_loc_int(x), qfn_find_loc_lgl(x)) -> r; print(r); plot(r);

# })

test_that("which.max/which.min", {
  fn <- function(lgl1, int1, dbl1) {
    declare(type(lgl1 = logical(NA)))
    declare(type(int1 = integer(NA)))
    declare(type(dbl1 = double(NA)))
    out <- c(
      which.min(lgl1),
      which.min(int1),
      which.min(dbl1),
      which.max(lgl1),
      which.max(int1),
      which.max(dbl1),
      which.max(dbl1[dbl1 < 0])
    )
    out
  }

  r2f(fn)
  # qfn := quick(fn)
  #
  # lgl1 = sample(c(TRUE, FALSE), 111, TRUE)
  # int1 = sample.int(222)
  # dbl1 = runif (333, -1, 1)
  #
  # # expect_equal(
  # bench::mark(relative=T,
  #   qfn(lgl1, int1, dbl1),
  #   fn(lgl1, int1, dbl1)
  # )

  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(
      lgl1 = sample(c(TRUE, FALSE), 10, TRUE),
      int1 = sample.int(100),
      dbl1 = runif(100, -1, 1)
    )
  )
})
