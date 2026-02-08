test_that("subscripts can use rev(seq_len()) on multi-d arrays", {
  fn <- function(x1) {
    declare(type(x1 = integer(2L, 3L)))
    x1[seq_len(2L), rev(seq_len(3L)), drop = FALSE]
  }

  x1 <- matrix(1:6, nrow = 2, ncol = 3)
  expect_quick_identical(fn, x1)
})

test_that("slice assignment supports vector subscripts", {
  fn <- function(out, block) {
    declare(type(out = integer(2L, 3L)))
    declare(type(block = integer(2L, 3L)))
    out[seq_len(2L), rev(seq_len(3L))] <- block
    out
  }

  out <- matrix(0L, nrow = 2, ncol = 3)
  block <- matrix(1:6, nrow = 2, ncol = 3)
  expect_quick_identical(fn, list(out, block))
})
