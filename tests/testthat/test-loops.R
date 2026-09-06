# Unit tests for loop constructs

test_that("repeat/break", {
  inc_to_5 <- function(x) {
    declare(type(x = integer(1)))
    repeat {
      if (x >= 5L) {
        break
      }
      x <- x + 1L
    }
    x
  }

  expect_translation_snapshots(inc_to_5)
  expect_quick_identical(inc_to_5, -1L, 0L, 4L, 5L)
})

test_that("parallel loops isolate body-local scratch bindings", {
  skip_if_no_openmp()

  fn <- function(x, n, out) {
    declare(
      type(x = double(n)),
      type(n = integer(1)),
      type(out = double(n))
    )
    declare(parallel())
    for (i in seq_len(n)) {
      scratch <- x[i] * 2
      out[i] <- scratch + 1
    }
    out
  }

  x <- as.double(seq_len(10000L))
  expect_quick_identical(fn, list(x, length(x), double(length(x))))
})

test_that("repeat + next", {
  inc_to_5_skip_neg <- function(x) {
    declare(type(x = integer(1)))
    repeat {
      x <- x + 1L
      if (x < 0L) {
        next
      }
      if (x >= 5L) break
    }
    x
  }

  expect_translation_snapshots(inc_to_5_skip_neg)
  expect_quick_identical(inc_to_5_skip_neg, -3L, -1L, 0L, 4L, 5L)
})

test_that("break/for", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    for (i in 1:10) {
      x <- x + 1L
      if (x >= 5L) {
        break
      }
    }
    x
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, -1L, 0L, 4L, 5L)
})

test_that("while", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    while (x < 5L) {
      x <- x + 1L
    }
    x
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, -1L, 0L, 4L, 5L)
})

test_that("while + next", {
  inc_to_5_skip_neg_while <- function(x) {
    declare(type(x = integer(1)))
    while (x < 5L) {
      x <- x + 1L
      if (x < 0L) next
    }
    x
  }

  expect_translation_snapshots(inc_to_5_skip_neg_while)
  expect_quick_identical(inc_to_5_skip_neg_while, -3L, -1L, 0L, 4L, 5L)
})

test_that("while + break", {
  inc_to_5_break_while <- function(x) {
    declare(type(x = integer(1)))
    while (TRUE) {
      if (x >= 5L) {
        break
      }
      x <- x + 1L
    }
    x
  }

  expect_translation_snapshots(inc_to_5_break_while)
  expect_quick_identical(inc_to_5_break_while, -1L, 0L, 4L, 5L)
})

test_that("expr return value", {
  fn <- function(x) {
    declare(type(x = integer(NA)))
    x + 1L
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, 1:10)
})

test_that("single-statement while/repeat bodies re-run their hoisted statements", {
  # A non-`{` loop body whose lone statement hoists code (here a BLAS
  # call) must emit that code inside the loop; hoisting it out of the
  # loop would freeze the body's work at its first evaluation. `for` is
  # covered in test-for-iterables.R.
  #
  # Keep the direct repeat assignment as a translation regression because
  # it cannot terminate. Exercise the generated repeat path separately with
  # a bounded one-statement body whose else branch repeats the same BLAS work.
  # fmt: skip
  squarings_while <- function(m) {
    declare(type(m = double(2, 2)))
    while (m[1, 1] < 100) m <- m %*% m
    m
  }

  expect_translation_snapshots(squarings_while)
  expect_quick_identical(squarings_while, list(diag(2) * 2))

  # fmt: skip
  squarings_repeat <- function(m) {
    declare(type(m = double(2, 2)))
    repeat m <- m %*% m
    m
  }

  expect_translation_snapshots(squarings_repeat)

  # fmt: skip
  bounded_squarings_repeat <- function(m) {
    declare(type(m = double(2, 2)))
    repeat if (m[1, 1] >= 100) break else m <- m %*% m
    m
  }

  expect_quick_identical(bounded_squarings_repeat, list(diag(2) * 2))
})
