
# Tests for dynamic dimension expressions evaluated in Fortran environment

test_that("arithmetic expressions in dimensions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    x <- double(n * 2L)
    y <- double(n - 1L)
    length(x) + length(y)
  }
  qfn <- quick(fn)
  expect_identical(qfn(4L), fn(4L))
  expect_identical(qfn(7L), fn(7L))
})


test_that("integer division and modulus in dimensions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- double(n %/% 2L + n %% 2L)
    length(out)
  }
  qfn <- quick(fn)
  expect_identical(qfn(5L), fn(5L))
  expect_identical(qfn(8L), fn(8L))
})


test_that("matrix dimension expressions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- matrix(1, n + 1L, n %/% 2L + 1L)
    dim(out)
  }
  qfn <- quick(fn)
  expect_identical(qfn(3L), fn(3L))
  expect_identical(qfn(6L), fn(6L))
})

