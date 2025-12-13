test_that("sapply(seq_along(x), f) lowers with named local closure", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    f <- function(i) x[i] * 2
    out <- sapply(seq_along(out), f)
    out
  }

  set.seed(1)
  x <- runif(25)
  expect_quick_identical(fn, list(x))
})

test_that("sapply lowering supports inline closures with explicit captures", {
  fn <- function(x, thresh) {
    declare(type(x = double(NA)), type(thresh = double(1)))
    out <- logical(length(x))
    out <- sapply(seq_along(out), function(i) x[i] > thresh)
    out
  }

  set.seed(1)
  x <- runif(25)
  expect_quick_identical(fn, list(x, 0.5))
})
