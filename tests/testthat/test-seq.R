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
