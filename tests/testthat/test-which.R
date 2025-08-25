# Unit tests for which.max and related functions

test_that("which.max", {
  fn <- function(a) {
    declare(type(a = double(NA)))

    out <- which.max(a)
    out
  }

  expect_snapshot(r2f(fn))

  qfn <- quick(fn)

  x = sample.int(1000) + runif(1000)

  expect_identical(fn(x), qfn(x))

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

  x <- logical(1000)
  x[500] <- TRUE

  qfn <- quick(fn)
  expect_identical(fn(x), qfn(x))
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
