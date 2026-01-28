# Public API tests for vector/matrix elementwise recycling rules

test_that("vector-matrix elementwise ops only recycle by nrow", {
  ok <- function(mat, vec) {
    declare(type(mat = double(2, 3)), type(vec = double(2)))
    mat + vec
  }

  set.seed(1)
  mat <- matrix(runif(6), nrow = 2, ncol = 3)
  vec <- runif(2)
  expect_quick_identical(ok, list(mat, vec))

  bad <- function(mat, vec) {
    declare(type(mat = double(2, 3)), type(vec = double(3)))
    mat + vec
  }

  expect_error(
    quick(bad),
    "vector length equal to the matrix first dimension \\(nrow\\)",
    fixed = FALSE
  )
})
