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

test_that("for() supports scalar indexing in singleton iterables", {
  seq_len_scalar <- function(x) {
    declare(type(x = double(1)))
    for (i in seq_len(1L)) {
      x[i] <- x[i] + 1.5
    }
    x
  }

  colon_scalar <- function(x) {
    declare(type(x = double(1)))
    for (i in 1:1) {
      x[i] <- x[i] + 2.0
    }
    x
  }

  expect_quick_identical(seq_len_scalar, 0, 1, -2.5)
  expect_quick_identical(colon_scalar, 0, 1, -2.5)
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
  unsupported_iterable <- function() {
    s <- 0L
    for (i in rev(list(1L, 2L))) {
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

  literal_iterable <- function() {
    s <- 0L
    for (i in 1L) {
      s <- s + i
    }
    s
  }

  expect_error(
    quick(unsupported_iterable),
    regexp = "unsupported iterable in for\\(\\): rev\\(list\\("
  )
  expect_error(
    quick(non_integer_seq),
    regexp = "non-integer seq\\(\\)'s not implemented yet"
  )
  expect_error(
    quick(non_integer_seq_len),
    regexp = "seq_len\\(\\) expects an integer scalar"
  )
  expect_error(
    quick(literal_iterable),
    regexp = "unsupported iterable in for\\(\\): 1"
  )
})

test_that("for() supports rev() on index iterables", {
  rev_colon <- function(n) {
    declare(type(n = integer(1)))
    out <- 0L
    for (i in rev(1:n)) {
      out <- out * 10L + i
    }
    out
  }

  rev_seq <- function() {
    out <- 0L
    for (i in rev(seq(1L, 6L, by = 2L))) {
      out <- out * 10L + i
    }
    out
  }

  rev_seq_len <- function(n) {
    declare(type(n = integer(1)))
    out <- 0L
    for (i in rev(seq_len(n))) {
      out <- out * 10L + i
    }
    out
  }

  rev_seq_along <- function(x) {
    declare(type(x = double(NA)))
    out <- 0L
    for (i in rev(seq_along(x))) {
      out <- out * 10L + i
    }
    out
  }

  expect_quick_identical(rev_colon, 0L, 1L, 3L, 5L)
  expect_quick_identical(rev_seq, list())
  expect_quick_identical(rev_seq_len, 0L, 1L, 5L)
  expect_quick_identical(rev_seq_along, numeric(), c(1, 2, 3))
})

test_that("for() rev(seq()) validates scalar bounds and step", {
  non_scalar_bounds <- function(x) {
    declare(type(x = integer(NA)))
    s <- 0L
    for (i in rev(seq(1L, x))) {
      s <- s + i
    }
    s
  }

  non_scalar_step <- function(x) {
    declare(type(x = integer(NA)))
    s <- 0L
    for (i in rev(seq(1L, 5L, by = x))) {
      s <- s + i
    }
    s
  }

  expect_error(
    quick(non_scalar_bounds),
    regexp = "seq\\(\\) iterable bounds must be scalars"
  )
  expect_error(
    quick(non_scalar_step),
    regexp = "seq\\(\\) iterable step must be a scalar"
  )
})

test_that("for() supports iterating over a symbol (value iteration)", {
  sum_values <- function(x) {
    declare(type(x = double(NA)))
    s <- 0
    for (v in x) {
      s <- s + v
    }
    s
  }

  count_true <- function(x) {
    declare(type(x = logical(NA)))
    n <- 0L
    for (v in x) {
      if (v) n <- n + 1L
    }
    n
  }

  linearize_matrix <- function(m) {
    declare(type(m = integer(NA, NA)))
    out <- integer(length(m))
    j <- 1L
    for (v in m) {
      out[j] <- v
      j <- j + 1L
    }
    out
  }

  iterable_hoisted <- function(x) {
    declare(type(x = integer(NA)))
    y <- x
    out <- 0L
    for (v in y) {
      out <- out * 10L + v
      y[length(y)] <- 9L
    }
    out
  }

  expect_quick_identical(sum_values, numeric(), c(1, 2, 3))
  expect_quick_identical(
    count_true,
    logical(),
    c(TRUE, FALSE, TRUE),
    c(FALSE, FALSE)
  )
  expect_quick_identical(linearize_matrix, matrix(1:6, nrow = 2))
  expect_quick_identical(iterable_hoisted, 1:3, c(1L, 2L, 3L, 4L))
})

test_that("for() supports rev() for value iteration", {
  rev_values <- function(x) {
    declare(type(x = integer(NA)))
    out <- 0L
    for (v in rev(x)) {
      out <- out * 10L + v
    }
    out
  }

  rev_matrix_values <- function(m) {
    declare(type(m = integer(NA, NA)))
    out <- 0L
    for (v in rev(m)) {
      out <- out * 10L + v
    }
    out
  }

  expect_quick_identical(rev_values, 1:3, c(1L, 2L, 3L, 4L))
  expect_quick_identical(rev_matrix_values, matrix(1:6, nrow = 2))
})
