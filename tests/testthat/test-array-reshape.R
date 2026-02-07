test_that("array() supports reshaping non-scalar data", {
  fn <- function(x) {
    declare(type(x = integer(2L, 3L, 4L)))
    array(as.double(x), dim = c(2L, 3L, 4L))
  }

  set.seed(1)
  x <- array(sample(1:10, 24, replace = TRUE), dim = c(2L, 3L, 4L))
  expect_quick_identical(fn, list(x))
})
