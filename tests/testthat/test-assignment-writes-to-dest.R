test_that("assignment is not skipped when RHS does not write to dest", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x <- 1
    x
  }
  qfn <- quick(fn)

  args <- list(x = 2)
  expect_equal(do.call(fn, args), do.call(qfn, args))
})
