# Compact vector/reduction coverage kept on CRAN.

test_that("vector smoke: unary intrinsic and rev compile and run", {
  sin_fn <- function(x) {
    declare(type(x = double(NA)))
    sin(x)
  }

  rev_fn <- function(x) {
    declare(type(x = double(NA)))
    rev(x)
  }

  x <- seq(-1, 1, length.out = 5)
  expect_quick_equal(sin_fn, list(x))
  expect_quick_identical(rev_fn, list(c(1, 2, 3)))
})

test_that("vector smoke: masked reductions and subsets compile and run", {
  masked_sum <- function(x) {
    declare(type(x = double(NA)))
    sum(x[x > 0])
  }

  subset_sum <- function(m) {
    declare(type(m = double(NA, NA)))
    sum(m[m > 3.0])
  }

  expect_quick_equal(masked_sum, list(c(-2, 1, 3, -4, 5)))

  m <- matrix(as.double(1:6), nrow = 2L, ncol = 3L, byrow = TRUE)
  expect_quick_equal(subset_sum, list(m))
})
