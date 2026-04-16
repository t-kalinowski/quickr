# Compact end-to-end closure coverage kept on CRAN.

test_that("closure smoke: sapply lowers and runs", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    sapply(seq_along(x), function(i) x[i] + 1.0)
  }

  set.seed(1)
  x <- runif(6)
  expect_quick_identical(fn, list(x))
})

test_that("closure smoke: local closure host mutation works", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    bump <- function() {
      x <<- x + 1.0
      NULL
    }
    bump()
    x
  }

  set.seed(2)
  x <- runif(6)
  expect_quick_identical(fn, list(x))
})
