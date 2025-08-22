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
      x = x + 1L
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
      x = x + 1L
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


# -------------------------------------------------------------------------
# Remainder (%%) and integer division (%/%) operators
# -------------------------------------------------------------------------
