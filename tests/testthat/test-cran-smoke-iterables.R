# Compact iterable coverage kept on CRAN.

test_that("iterable smoke: for loops over vectors compile and run", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    total <- 0.0
    for (elt in x) {
      total <- total + elt
    }
    total
  }

  expect_quick_equal(fn, list(c(0.5, 1.5, 2.5)))
})
