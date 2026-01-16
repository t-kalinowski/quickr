# Coverage-focused tests for matrix parsing helpers.

test_that("transpose parsing handles scalars and rejects rank > 2", {
  scalar_left <- function(B) {
    declare(type(B = double(2, 2)))
    t(1.25) %*% B
  }

  expect_error(
    quick(scalar_left),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )

  rank3_left <- function(x, B) {
    declare(type(x = double(2, 2, 2)), type(B = double(2, 2)))
    t(x) %*% B
  }

  expect_error(
    quick(rank3_left),
    "t() only supports rank 0-2 inputs",
    fixed = TRUE
  )
})

test_that("matrix helpers validate logical args", {
  bad_chol <- function(A) {
    declare(type(A = double(2, 2)))
    chol(A, pivot = 1)
  }

  expect_error(
    quick(bad_chol),
    "chol\\(\\) only supports literal pivot = TRUE/FALSE"
  )

  bad_diag <- function(x) {
    declare(type(x = double(3)))
    diag(x, names = 1)
  }

  expect_error(
    quick(bad_diag),
    "diag\\(\\) only supports literal names = TRUE/FALSE"
  )
})
