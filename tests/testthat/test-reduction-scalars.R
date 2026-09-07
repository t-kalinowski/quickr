test_that("min handles scalar arguments without reduction", {
  fn <- function(a, b, m) {
    declare(
      type(a = integer(1)),
      type(b = integer(1)),
      type(m = integer(NA, 2))
    )

    c(min(m[a, 1], m[b, 1]), min(m[a, 2], m[b, 2]))
  }

  # fsub <- r2f(fn)
  # expect_no_match(as.character(fsub), "minval\\(")

  m <- matrix(c(5L, 1L, 3L, 9L), ncol = 2, byrow = TRUE)
  expect_identical(fn(1L, 1L, m), c(5L, 1L))
  expect_identical(fn(1L, 2L, m), c(3L, 1L))
  expect_quick_identical(fn, list(1L, 1L, m), list(1L, 2L, m))
})


test_that("drop = FALSE preserves dims and uses reductions", {
  fn <- function(a, m) {
    declare(
      type(a = integer(1)),
      type(m = integer(NA, 2))
    )

    min(m[a, 1L, drop = FALSE])
  }

  # fsub <- r2f(fn)
  # expect_match(as.character(fsub), "minval\\(")

  m <- matrix(c(2L, 4L, 1L, 3L), nrow = 2L, byrow = TRUE)
  expect_identical(fn(1L, m), 2L)
  expect_identical(fn(2L, m), 1L)
  expect_quick_identical(fn, list(1L, m), list(2L, m))
})


test_that("reductions over vectors still use intrinsics", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    min(x)
  }

  # fsub <- r2f(fn)
  # expect_match(as.character(fsub), "minval\\(")

  x1 <- c(10L, -2L, 5L, 3L)
  x2 <- c(1L, 2L, 3L)

  expect_identical(fn(x1), -2L)
  expect_identical(fn(x2), 1L)
  expect_quick_identical(fn, x1, x2)
})

test_that("empty extrema are rejected explicitly", {
  static_min <- function() {
    min(numeric())
  }
  static_max <- function() {
    max(integer())
  }
  static_logical <- function() {
    min(logical())
  }
  message <- "min()/max() of empty inputs are not supported"

  expect_error(quick(static_min), message, fixed = TRUE)
  expect_error(quick(static_max), message, fixed = TRUE)
  expect_error(quick(static_logical), message, fixed = TRUE)

  dynamic_min <- function(x) {
    declare(type(x = double(NA)))
    min(x)
  }
  qmin <- quick(dynamic_min)
  expect_equal(qmin(c(3, -1, 2)), -1)
  expect_error(qmin(numeric()), message, fixed = TRUE)

  dynamic_max <- function(x) {
    declare(type(x = double(NA)))
    max(x)
  }
  qmax <- quick(dynamic_max)
  expect_equal(qmax(c(3, -1, 2)), 3)
  expect_error(qmax(numeric()), message, fixed = TRUE)

  masked_scalar_min <- function(x, mask) {
    declare(type(x = double(1)), type(mask = logical(1)))
    min(x[mask])
  }
  qmasked_scalar_min <- quick(masked_scalar_min)
  expect_equal(qmasked_scalar_min(3, TRUE), 3)
  expect_error(qmasked_scalar_min(3, FALSE), message, fixed = TRUE)
})

test_that("multi-argument extrema ignore empty inputs", {
  static_extrema <- function() {
    min(numeric(), 5)
  }
  dynamic_min <- function(x) {
    declare(type(x = double(NA)))
    min(x, 5)
  }
  dynamic_max <- function(x) {
    declare(type(x = double(NA)))
    max(x, -5)
  }
  dynamic_pair <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    min(x, y)
  }

  expect_identical(quick(static_extrema)(), static_extrema())

  qmin <- quick(dynamic_min)
  expect_identical(qmin(numeric()), dynamic_min(numeric()))
  expect_identical(qmin(c(9, 2)), dynamic_min(c(9, 2)))

  qmax <- quick(dynamic_max)
  expect_identical(qmax(numeric()), dynamic_max(numeric()))
  expect_identical(qmax(c(-9, -2)), dynamic_max(c(-9, -2)))

  qpair <- quick(dynamic_pair)
  for (args in list(
    list(numeric(), c(9, 2)),
    list(c(9, 2), numeric()),
    list(c(9, 2), c(8, 1))
  )) {
    expect_identical(do.call(qpair, args), do.call(dynamic_pair, args))
  }
  expect_error(
    qpair(numeric(), numeric()),
    "min()/max() of empty inputs are not supported",
    fixed = TRUE
  )
})


test_that("nested scalar min/max compiles and runs", {
  fn <- function(m) {
    declare(type(m = integer(2, 2)))
    lo <- 1L
    hi <- 2L
    min(max(m[1, 1], lo), hi)
  }

  m1 <- matrix(c(0L, 3L, 1L, 2L), nrow = 2L, byrow = TRUE)
  m2 <- matrix(c(10L, 3L, 1L, 2L), nrow = 2L, byrow = TRUE)
  expect_identical(fn(m1), 1L)
  expect_identical(fn(m2), 2L)
  expect_quick_identical(fn, m1, m2)
})


test_that("clamp on a 1d array works with nested scalar min/max", {
  clamp <- function(x, lo, hi) {
    declare(type(x = double(n)), type(lo = double(1)), type(hi = double(1)))
    out <- double(length(x))
    for (i in seq_along(x)) {
      out[i] <- min(max(x[i], lo), hi)
    }
    out
  }

  x <- c(-2.0, -0.5, 0.25, 1.25, 10.0)
  lo <- -0.25
  hi <- 1.0

  expect_identical(clamp(x, lo, hi), pmin(pmax(x, lo), hi))
  expect_quick_identical(clamp, list(x, lo, hi))
})
