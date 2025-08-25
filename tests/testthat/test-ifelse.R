# Unit test for ifelse translation

test_that("ifelse", {
  fn <- function(a, b) {
    declare(
      type(a = integer(n)),
      type(b = integer(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_identical(fn, list(-10:10, integer(21)))

  # double version of above
  fn <- function(a, b) {
    declare(
      type(a = double(n)),
      type(b = double(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_equal(fn, list(seq(-5, 5, length.out = 20), double(20)))
})
