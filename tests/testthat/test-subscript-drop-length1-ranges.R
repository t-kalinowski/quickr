test_that("subscript drop=TRUE drops length-1 ranges like 1:1", {
  fn <- function(x) {
    declare(type(x = double(1, 1, 2, 3, 4)))
    x[1:1, 1:1, 1:2, 1:3, 1:4]
  }

  x <- array(as.double(1:24), dim = c(1, 1, 2, 3, 4))
  expect_quick_identical(fn, list(x))
})
