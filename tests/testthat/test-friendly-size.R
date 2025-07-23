test_that("dimension mismatch errors show axis", {
  fn <- function(x) {
    declare(type(x = double(2, 3)))
    sum(x)
  }
  qfn <- quick(fn)

  expect_error(
    qfn(matrix(as.double(1:6), nrow = 3, ncol = 2)),
    "dim(x)[1] must be 2, not 3",
    fixed = TRUE
  )

  expect_error(
    qfn(matrix(as.double(1:4), nrow = 2, ncol = 2)),
    "dim(x)[2] must be 3, not 2",
    fixed = TRUE
  )
})
