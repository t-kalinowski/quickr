# Unit test for seq translation

test_that("seq", {
  fn <- function(a, b) {
    declare(
      type(a = integer(1)),
      type(b = integer(1))
    )

    out <- sum(seq(a, b))
    out
  }

  expect_quick_identical(fn, list(-1L, 10L), list(10L, -5L))
})

test_that("seq supports literal bounds and steps", {
  seq_range <- function() {
    out <- seq(2L, 5L)
    out
  }

  seq_step <- function() {
    out <- seq(1L, 5L, by = 2L)
    out
  }

  expect_quick_identical(seq_range, list())
  expect_quick_identical(seq_step, list())
})

test_that("seq_along returns linear indices for arrays", {
  seq_along_matrix <- function(x) {
    declare(type(x = double(m, n)))
    out <- seq_along(x)
    out
  }

  expect_quick_identical(
    seq_along_matrix,
    list(matrix(as.double(1:6), nrow = 2))
  )
})

test_that("seq rejects length.out and along.with", {
  bad_length_out <- function() {
    out <- seq(1L, 5L, length.out = 3L)
    out
  }

  bad_along_with <- function(x) {
    declare(type(x = double(NA)))
    out <- seq(along.with = x)
    out
  }

  expect_error(
    quick(bad_length_out),
    regexp = "seq\\(length.out=, along.with=\\) not implemented yet"
  )
  expect_error(
    quick(bad_along_with),
    regexp = "seq\\(length.out=, along.with=\\) not implemented yet"
  )
})

test_that("seq-like iterables work in subset indexing", {
  seq_index <- function(x, start, end) {
    declare(
      type(x = double(5)),
      type(start = integer(1)),
      type(end = integer(1))
    )
    out <- x[seq(start, end)]
    out
  }

  seq_len_index <- function(x) {
    declare(type(x = double(5)))
    out <- x[seq_len(length(x))]
    out
  }

  seq_along_index <- function(x) {
    declare(type(x = double(3)))
    out <- x[seq_along(x)]
    out
  }

  expect_quick_identical(
    seq_index,
    list(c(1, 2, 3, 4, 5), 2L, 4L),
    list(c(5, 4, 3, 2, 1), 1L, 3L)
  )
  expect_quick_identical(seq_len_index, list(c(1, 2, 3, 4, 5)))
  expect_quick_identical(seq_along_index, list(c(2, 4, 6)))
})
