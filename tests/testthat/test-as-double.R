test_that("as.double casts integer and logical values", {
  fn_int <- function(x) {
    declare(type(x = integer(n)))
    as.double(x)
  }
  expect_quick_identical(fn_int, list(1:3))

  fn_lgl <- function(x) {
    declare(type(x = logical(n)))
    as.double(x)
  }
  expect_quick_identical(fn_lgl, list(c(TRUE, FALSE, TRUE)))
})

test_that("as.double drops dimensions for matrices and allows slice assignment", {
  as_vec <- function(a) {
    declare(type(a = integer(2L, 3L)))
    as.double(a)
  }
  expect_quick_identical(as_vec, list(matrix(1:6, 2, 3)))

  assign_slice <- function(a) {
    declare(type(a = integer(2L, 3L)))
    out <- double(6L)
    out[1:6] <- as.double(a)
    out
  }
  expect_quick_identical(assign_slice, list(matrix(1:6, 2, 3)))
})

test_that("/ performs real division for integer and logical inputs", {
  div_int <- function(a, b) {
    declare(type(a = integer(n)), type(b = integer(n)))
    a / b
  }
  expect_quick_equal(div_int, list(1:3, c(2L, 2L, 2L)))

  div_lgl <- function(a, b) {
    declare(type(a = logical(n)), type(b = logical(n)))
    a / b
  }
  expect_quick_equal(div_lgl, list(c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE)))
})
