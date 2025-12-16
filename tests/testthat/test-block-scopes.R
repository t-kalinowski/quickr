test_that("array-expression subscripting hoists into a block-scoped temp", {
  fn <- function(x) {
    declare(type(x = double(3, 4)))
    out <- ifelse((x > 0.0)[2, 3], 1.0, 0.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Fortran disallows (expr)(i,j); quickr uses a block-local temp array."
  )

  set.seed(1)
  x <- matrix(runif(12) - 0.5, 3, 4)
  expect_quick_identical(fn, list(x))
})

test_that("block-scoped temps work for rank-3 array expression subscripting", {
  fn <- function(x) {
    declare(type(x = double(2, 3, 4)))
    out <- ifelse((x > 0.0)[2, 3, 4], 1.0, 0.0)
    out
  }

  set.seed(1)
  x <- array(runif(24) - 0.5, dim = c(2, 3, 4))
  expect_quick_identical(fn, list(x))
})
