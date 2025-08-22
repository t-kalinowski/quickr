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