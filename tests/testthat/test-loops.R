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

test_that("lazy while-condition errors preserve preceding effects", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    predicate <- function() {
      while ((runif(1) + abs()) > 0) {
        later <- runif(2)
      }
      TRUE
    }
    flag && predicate()
  }
  qfn <- quick(fn)
  withr::local_seed(153)
  before <- .Random.seed
  expect_identical(qfn(FALSE), fn(FALSE))
  expect_identical(.Random.seed, before)
  set.seed(153)
  expect_error(fn(TRUE), "abs")
  expected_seed <- .Random.seed
  set.seed(153)
  expect_error(qfn(TRUE), "abs")
  expect_identical(.Random.seed, expected_seed)
})

test_that("serial index loops leave the final iteration value", {
  seq_final <- function(n) {
    declare(type(n = integer(1)))
    for (i in seq(1L, n)) {
      NULL
    }
    i
  }

  colon_final <- function(n) {
    declare(type(n = integer(1)))
    for (i in n:1L) {
      NULL
    }
    i
  }

  stepped_final <- function(n) {
    declare(type(n = integer(1)))
    for (i in seq(1L, n, by = 2L)) {
      NULL
    }
    i
  }

  reversed_final <- function(n) {
    declare(type(n = integer(1)))
    for (i in rev(seq(1L, n, by = 2L))) {
      NULL
    }
    i
  }

  expect_translation_snapshots(seq_final)
  expect_translation_snapshots(stepped_final)
  expect_translation_snapshots(reversed_final)
  expect_quick_identical(seq_final, -2L, 0L, 1L, 5L)
  expect_quick_identical(colon_final, -2L, 0L, 1L, 5L)
  expect_quick_identical(stepped_final, 1L, 5L, 6L)
  expect_quick_identical(reversed_final, 1L, 5L, 6L)
})

test_that("serial index loops preserve next and break values", {
  final_value <- function(n, stop_at) {
    declare(type(n = integer(1)), type(stop_at = integer(1)))
    for (i in seq_len(n)) {
      if (i == stop_at) {
        break
      }
      next
    }
    i
  }

  expect_translation_snapshots(final_value)
  expect_quick_identical(final_value, list(5L, 3L), list(5L, 0L))
})

test_that("serial index loops allow their R variable to be reassigned", {
  reassigned <- function(n) {
    declare(type(n = integer(1)))
    total <- 0L
    for (i in seq_len(n)) {
      total <- total + i
      i <- i + 10L
    }
    c(total, i)
  }

  nested <- function(n) {
    declare(type(n = integer(1)))
    total <- 0L
    for (i in seq_len(n)) {
      total <- total + i
      for (i in 3L:4L) {
        total <- total + i
      }
      total <- total + i
    }
    c(total, i)
  }

  expect_translation_snapshots(reassigned)
  expect_translation_snapshots(nested)
  expect_quick_identical(reassigned, 1L, 4L)
  expect_quick_identical(nested, 1L, 4L)
})

test_that("empty index loops execute no iterations", {
  along_final <- function(x) {
    declare(type(x = double(NA)))
    out <- 0L
    for (i in seq_along(x)) {
      out <- i
    }
    if (length(x) > 0L) {
      out <- i
    }
    out
  }

  reversed_final <- function(n) {
    declare(type(n = integer(1)))
    out <- 0L
    for (i in rev(seq_len(n))) {
      out <- i
    }
    if (n > 0L) {
      out <- i
    }
    out
  }

  expect_translation_snapshots(along_final)
  expect_quick_identical(along_final, numeric(), 1, c(1, 2, 3))
  expect_quick_identical(reversed_final, 0L, 1L, 4L)
})
