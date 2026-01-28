# Public API tests for optional NULL defaults in local closures

test_that("optional NULL default treats explicit NULL as missing", {
  fn <- function(x) {
    declare(type(x = double(1)))
    f <- function(a = NULL) {
      if (is.null(a)) {
        a <- x + 1
      }
      a * 2
    }
    f(NULL)
  }

  expect_quick_identical(fn, list(2), list(5))
})

test_that("optional NULL defaults work with multiple optional arguments", {
  fn <- function(x) {
    declare(type(x = double(1)))

    f <- function(a = NULL, b = NULL) {
      if (is.null(a)) {
        a <- x + 1
      }
      if (is.null(b)) {
        b <- 2
      }
      a + b
    }

    c(
      f(),
      f(b = 5),
      f(a = 10),
      f(a = 10, b = 5)
    )
  }

  qfn <- quick(fn)
  expect_identical(qfn(2), c(5, 8, 12, 15))
  expect_identical(qfn(5), c(8, 11, 12, 15))
})

test_that("local closure calls error for unknown or missing arguments", {
  unknown_arg <- function(x) {
    declare(type(x = double(1)))
    f <- function(a = NULL, ...) a
    f(b = x)
  }
  expect_error(
    quick(unknown_arg),
    "call has unknown argument\\(s\\): b",
    fixed = FALSE
  )

  missing_required <- function(x) {
    declare(type(x = double(1)))
    f <- function(a) a + x
    f()
  }
  expect_error(
    quick(missing_required),
    "call is missing required argument\\(s\\): a",
    fixed = FALSE
  )
})

test_that("local closure calls error when required arguments have no inferred type", {
  bad <- function(x) {
    declare(type(x = double(1)))
    f <- function(a) a + x
    f(NULL)
  }

  expect_error(
    quick(bad),
    "call has argument\\(s\\) without inferred types",
    fixed = FALSE
  )
})

test_that("optional NULL defaults must be initialized before use", {
  bad <- function(x) {
    declare(type(x = double(n)))

    f <- function(i, shift = NULL) {
      x[i] + shift
    }

    f(1L)
  }

  expect_error(
    quick(bad),
    "optional argument\\(s\\) used without initializing when NULL: shift",
    fixed = FALSE
  )
})

test_that("optional NULL defaults can be assigned before use", {
  fn <- function(x) {
    declare(type(x = double(1)))

    f <- function(shift = NULL) {
      shift <- 2
      x + shift
    }

    f()
  }

  expect_quick_identical(fn, list(3))
})

test_that("optional arg shadow names avoid existing fortran names", {
  fn <- function(x) {
    declare(type(x = double(1)))
    declare(type(`x..local.` = double(1)))
    `x..local.` <- x + 1

    f <- function(x = NULL) {
      if (is.null(x)) {
        x <- 2
      }
      x + `x..local.`
    }

    f()
  }

  expect_quick_identical(fn, list(1))
})

test_that("local closure calls returning NULL cannot be used as values", {
  bad <- function(x) {
    declare(type(x = double(1)))
    f <- function() NULL
    1 + f()
  }

  expect_error(
    quick(bad),
    "local closure calls that return `NULL` cannot be used as values",
    fixed = TRUE
  )
})
