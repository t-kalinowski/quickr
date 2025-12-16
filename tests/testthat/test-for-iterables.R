# Unit tests for `for (... in <iterable>)` lowering

test_that("for() supports parenthesized iterables", {
  sum_i <- function(n) {
    declare(type(n = integer(1)))
    s <- 0L
    for (i in (1:n)) {
      s <- s + i
    }
    s
  }

  sum_seq_len <- function(n) {
    declare(type(n = integer(1)))
    s <- 0L
    for (i in (seq_len(n))) {
      s <- s + i
    }
    s
  }

  sum_seq_along <- function(x) {
    declare(type(x = double(NA)))
    s <- 0L
    for (i in (seq_along(x))) {
      s <- s + i
    }
    s
  }

  expect_quick_identical(sum_i, -1L, 0L, 1L, 5L)
  expect_quick_identical(sum_seq_len, 0L, 1L, 5L)
  expect_quick_identical(sum_seq_along, numeric(), c(1, 2, 3))
})

test_that("seq_along() works for scalars", {
  fn <- function(x) {
    declare(type(x = double(1)))
    s <- 0L
    for (i in seq_along(x)) {
      s <- s + i
    }
    s
  }

  expect_quick_identical(fn, 0, 1, -3)
})

test_that("for() supports seq() direction and step", {
  digits_from_seq <- function(a, b) {
    declare(type(a = integer(1)), type(b = integer(1)))
    out <- 0L
    for (i in seq(a, b)) {
      out <- out * 10L + i
    }
    out
  }

  sum_by_2 <- function(n) {
    declare(type(n = integer(1)))
    s <- 0L
    for (i in seq(1L, n, by = 2L)) {
      s <- s + i
    }
    s
  }

  expect_quick_identical(digits_from_seq, list(3L, 1L), list(1L, 3L))
  expect_quick_identical(sum_by_2, 1L, 5L, 6L)
})

test_that("for() iterable errors are clear", {
  sym_iterable <- function(x) {
    declare(type(x = integer(NA)))
    s <- 0L
    for (i in x) {
      s <- s + i
    }
    s
  }

  call_iterable <- function(n) {
    declare(type(n = integer(1)))
    s <- 0L
    for (i in rev(1:n)) {
      s <- s + i
    }
    s
  }

  non_integer_seq <- function() {
    s <- 0L
    for (i in seq(1, 2, by = 0.5)) {
      s <- s + i
    }
    s
  }

  non_integer_seq_len <- function() {
    s <- 0L
    for (i in seq_len(2.5)) {
      s <- s + i
    }
    s
  }

  expect_error(
    quick(sym_iterable),
    regexp = "unsupported iterable in for\\(\\): x"
  )
  expect_error(
    quick(call_iterable),
    regexp = "unsupported iterable in for\\(\\): rev\\("
  )
  expect_error(
    quick(non_integer_seq),
    regexp = "non-integer seq\\(\\)'s not implemented yet"
  )
  expect_error(
    quick(non_integer_seq_len),
    regexp = "seq_len\\(\\) expects an integer scalar"
  )
})
