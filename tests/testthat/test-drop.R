# Public API tests for drop()

test_that("drop() translation snapshots", {
  drop_row <- function(A) {
    declare(type(A = double(1L, 3L)))
    drop(A)
  }
  drop_col <- function(A) {
    declare(type(A = double(3L, 1L)))
    drop(A)
  }
  drop_row_unknown <- function(A) {
    declare(type(A = double(1L, NA)))
    drop(A)
  }
  drop_col_unknown <- function(A) {
    declare(type(A = double(NA, 1L)))
    drop(A)
  }
  drop_row_n <- function(A, n) {
    declare(type(n = integer(1)))
    declare(type(A = double(1L, n)))
    drop(A)
  }
  drop_col_n <- function(A, n) {
    declare(type(n = integer(1)))
    declare(type(A = double(n, 1L)))
    drop(A)
  }

  expect_translation_snapshots(drop_row)
  expect_translation_snapshots(drop_col)
})

test_that("drop() matches base R for singleton matrices", {
  drop_row <- function(A) {
    declare(type(A = double(1L, 3L)))
    drop(A)
  }
  drop_col <- function(A) {
    declare(type(A = double(3L, 1L)))
    drop(A)
  }
  drop_row_unknown <- function(A) {
    declare(type(A = double(1L, NA)))
    drop(A)
  }
  drop_col_unknown <- function(A) {
    declare(type(A = double(NA, 1L)))
    drop(A)
  }
  drop_row_n <- function(A, n) {
    declare(type(n = integer(1)))
    declare(type(A = double(1L, n)))
    drop(A)
  }
  drop_col_n <- function(A, n) {
    declare(type(n = integer(1)))
    declare(type(A = double(n, 1L)))
    drop(A)
  }
  drop_none <- function(A) {
    declare(type(A = double(3L, 2L)))
    drop(A)
  }

  expect_quick_identical(drop_row, list(matrix(runif(3), nrow = 1L)))
  expect_quick_identical(drop_col, list(matrix(runif(3), ncol = 1L)))
  expect_quick_identical(drop_row_unknown, list(matrix(runif(5), nrow = 1L)))
  expect_quick_identical(drop_col_unknown, list(matrix(runif(7), ncol = 1L)))
  expect_quick_identical(drop_row_n, list(matrix(runif(4), nrow = 1L), 4L))
  expect_quick_identical(drop_col_n, list(matrix(runif(6), ncol = 1L), 6L))
  expect_quick_identical(drop_none, list(matrix(runif(6), nrow = 3L)))
  expect_error(quick(drop_row_n)(matrix(1, 1, 1), 1L), "runtime extent")
  expect_error(quick(drop_col_n)(matrix(1, 1, 1), 1L), "runtime extent")
})

test_that("drop() errors for rank > 2 inputs", {
  fn <- function(A) {
    declare(type(A = double(2, 2, 2)))
    drop(A)
  }

  expect_error(
    quick(fn),
    "drop() only supports rank 0-2 inputs",
    fixed = TRUE
  )
})
