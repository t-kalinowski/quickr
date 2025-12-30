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
