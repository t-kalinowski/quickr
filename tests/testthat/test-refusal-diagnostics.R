test_that("character declarations are refused with a clean message", {
  fn <- function(x) {
    declare(type(x = character(1)))
    x
  }
  expect_error(
    quick(fn),
    "character values are not supported by quickr"
  )
})

test_that("unsupported complex operations are refused with R's messages", {
  complex_lt <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x < y
  }
  expect_error(quick(complex_lt), "invalid comparison with complex values")

  complex_eq <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x == y
  }
  expect_quick_identical(complex_eq, list(1i, 1i))
  expect_quick_identical(complex_eq, list(1i, 2i))

  complex_mod <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x %% y
  }
  expect_error(quick(complex_mod), "unimplemented complex operation")
})

test_that("complex operands are refused in linear algebra", {
  complex_matmul <- function(x, y) {
    declare(type(x = complex(2)), type(y = complex(2)))
    x %*% y
  }
  expect_error(
    quick(complex_matmul),
    "%*% does not support complex operands",
    fixed = TRUE
  )

  complex_mixed <- function(x, y) {
    declare(type(x = complex(2, 2)), type(y = double(2, 2)))
    x %*% y
  }
  expect_error(quick(complex_mixed), "does not support complex operands")

  complex_solve <- function(x) {
    declare(type(x = complex(2, 2)))
    solve(x)
  }
  expect_error(quick(complex_solve), "does not support complex operands")

  complex_crossprod <- function(x) {
    declare(type(x = complex(2, 2)))
    crossprod(x)
  }
  expect_error(quick(complex_crossprod), "does not support complex operands")

  complex_t <- function(x) {
    declare(type(x = complex(2, 2)))
    t(x)
  }
  expect_quick_identical(
    complex_t,
    list(matrix(c(1 + 1i, 2 + 0i, 3 - 1i, 4 + 2i), 2, 2))
  )
})
