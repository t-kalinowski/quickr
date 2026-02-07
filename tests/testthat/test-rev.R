test_that("rev() reverses vectors", {
  fn_dbl <- function(x) {
    declare(type(x = double(NA)))
    rev(x)
  }
  expect_quick_identical(
    fn_dbl,
    list(as.double(1:5)),
    list(as.double(c(2, -1, 3, 0)))
  )

  fn_int <- function(x) {
    declare(type(x = integer(NA)))
    rev(x)
  }
  expect_quick_identical(fn_int, list(as.integer(c(1, 0, -3, 2))))

  # Logical arguments are passed via the bind(c) interface as integer storage.
  fn_lgl <- function(x) {
    declare(type(x = logical(NA)))
    rev(x)
  }
  expect_quick_identical(fn_lgl, list(c(TRUE, FALSE, TRUE, FALSE)))
  expect_quick_identical(fn_lgl, list(c(TRUE, NA, FALSE, TRUE)))
})

test_that("rev() hoists array expressions", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    rev(x + 1)
  }
  expect_quick_identical(fn, list(as.double(1:6)))
})

test_that("rev() is a no-op for scalars", {
  fn <- function(x) {
    declare(type(x = double(1)))
    rev(x)
  }
  expect_quick_identical(fn, list(as.double(3)))
})
