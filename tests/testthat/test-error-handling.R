test_that("stop propagates errors from quickr functions", {
  stop_fn <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop("x must be nonnegative")
    }
    x + 1
  }

  q_stop <- quick(stop_fn)
  expect_error(q_stop(-1), "x must be nonnegative", fixed = TRUE)
  expect_equal(q_stop(1), 2)
})

test_that("stop with empty message still signals an error", {
  stop_empty <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop("")
    }
    x + 1
  }

  expect_error(quick(stop_empty)(-1), "quickr error", fixed = TRUE)
})

test_that("stop with one-character message propagates", {
  stop_one <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop("x")
    }
    x + 1
  }

  expect_error(quick(stop_one)(-1), "x", fixed = TRUE)
})

test_that("stop with newline escapes in error message", {
  stop_newline <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop("a\nb")
    }
    x + 1
  }

  err <- tryCatch(quick(stop_newline)(-1), error = identity)
  expect_true(inherits(err, "error"))
  expect_equal(conditionMessage(err), "a\\nb")
})

test_that("long stop messages are truncated", {
  # Include spaces so Fortran line continuations can split the literal.
  long_msg <- paste(rep("abcdefghij", 30), collapse = " ")
  stop_long <- eval(bquote(function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop(.(long_msg))
    }
    x + 1
  }))

  err <- tryCatch(quick(stop_long)(-1), error = identity)
  expect_true(inherits(err, "error"))
  msg <- conditionMessage(err)
  expect_equal(nchar(msg), 255L)
  expect_equal(msg, substr(long_msg, 1, 255L))
})

test_that("long stop messages without spaces are rejected", {
  long_msg <- paste(rep("abcdefghij", 30), collapse = "")
  stop_long <- eval(bquote(function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop(.(long_msg))
    }
    x + 1
  }))

  expect_error(
    quick(stop_long),
    "Error message is too long to fit in a single Fortran line without spaces.",
    fixed = TRUE
  )
})

test_that("lapack rank deficiency errors surface in quickr", {
  solve_singular <- function(A, b) {
    declare(type(A = double(2, 2)), type(b = double(2)))
    solve(A, b)
  }

  qr_singular <- function(A, b) {
    declare(type(A = double(2, 2)), type(b = double(2)))
    qr.solve(A, b)
  }

  A <- matrix(c(1, 2, 2, 4), nrow = 2)
  b <- c(1, 2)

  expect_error(quick(solve_singular)(A, b), "dgesv", fixed = TRUE)
  expect_error(quick(qr_singular)(A, b), "rank deficient", fixed = TRUE)
})

test_that("stop() translation includes error plumbing", {
  stop_fn <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop("x must be nonnegative")
    }
    x + 1
  }

  expect_translation_snapshots(
    stop_fn,
    note = "stop() should emit error message and C bridge checks"
  )
})

test_that("stop() translation wraps long error messages", {
  long_msg <- paste(rep("abcdefghij", 15), collapse = " ")
  stop_fn <- eval(bquote(function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      stop(.(long_msg))
    }
    x + 1
  }))

  expect_translation_snapshots(stop_fn)
})
