skip_on_cran()

test_that("sapply reads named iterables without changing values or storage", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- sapply((x), function(value) {
      value <- value * 2
      value
    })
    sum(as.double(out))
  }
  expect_quick_identical(fn, numeric(), 2, c(-3, 0, 4))

  matrix_values <- function(x) {
    declare(type(x = integer(NA, NA)))
    out <- sapply(x, function(value) value + 1L)
    out
  }
  expect_translation_snapshots(matrix_values)
  expect_quick_identical(matrix_values, matrix(1:6, 2), matrix(3L))

  empty_matrix <- function(x) {
    declare(type(x = integer(NA, NA)))
    out <- sapply(x, function(value) value + 1L)
    length(out)
  }
  expect_quick_identical(
    empty_matrix,
    matrix(integer(), 0, 3),
    matrix(integer(), 3, 0)
  )

  scalar <- function(x) {
    declare(type(x = double(1)))
    out <- sapply(x, function(value) value + 1)
    out
  }
  expect_quick_identical(scalar, -2, 0, 3)
})

test_that("sapply converts integer-backed logical values at the call site", {
  fn <- function(x) {
    declare(type(x = logical(NA)))
    out <- sapply(x, function(value) as.integer(!value))
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, TRUE, FALSE, c(TRUE, FALSE, TRUE))

  native_flags <- function(x) {
    declare(type(x = double(NA)))
    flags <- x > 0
    out <- sapply(flags, function(value) as.integer(!value))
    out
  }
  expect_translation_snapshots(native_flags)
  expect_quick_identical(native_flags, c(-2, 0, 3))
})

test_that("sapply reads shared logical input in parallel", {
  skip_if_no_openmp()
  fn <- function(x) {
    declare(type(x = logical(NA)))
    declare(parallel())
    out <- sapply(x, function(value) as.integer(!value))
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, rep(c(TRUE, FALSE, FALSE), 1000))
})

test_that("sapply retains its input when the callback changes its binding", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    change <- function(value) {
      x[length(x)] <<- 99
      value
    }
    out <- sapply(x, change)
    list(out, x)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(1, 2, 3), 1)

  before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(before))
  qfn <- quick(fn)
  x <- c(1, 2, 3)
  alias <- x
  expect_identical(qfn(x), fn(x))
  expect_identical(x, c(1, 2, 3))
  expect_identical(alias, x)
})

test_that("sapply follows a formal alias through a nested writer", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    change <- function() {
      x[length(x)] <<- 99
      NULL
    }
    step <- function(value) {
      change()
      value
    }
    map <- function(values) {
      out <- sapply(values, step)
      out
    }
    out <- map(x)
    list(out, x)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(1, 2, 3), 1)
})

test_that("sapply distinguishes shadowed bindings in callback writes", {
  fn <- function(x, other) {
    declare(type(x = double(NA)), type(other = double(NA)))
    step <- function(value) {
      x[length(x)] <<- 99
      value * 2
    }
    map <- function(x) {
      out <- sapply(x, step)
      out
    }
    out <- map(other)
    list(out, x)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(c(1, 2, 3), c(4, 5, 6)))
  shared <- c(1, 2, 3)
  expect_quick_identical(fn, list(shared, shared))
})

test_that("sapply retains its input when output replaces the same binding", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    x <- sapply(x, function(value) sum(x) + value)
    x
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(1, 2, 3), 1)
})

test_that("sapply preserves computed-iterable and callback RNG order", {
  fn <- function(n) {
    declare(type(n = integer(1)), type(out = double(n)))
    out <- sapply(runif(n), function(value) value + runif(1L))
    out
  }
  expect_translation_snapshots(fn)
  before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(before))
  qfn <- quick(fn)
  set.seed(42)
  expected <- fn(5L)
  expected_seed <- .Random.seed
  set.seed(42)
  expect_identical(qfn(5L), expected)
  expect_identical(.Random.seed, expected_seed)
})
