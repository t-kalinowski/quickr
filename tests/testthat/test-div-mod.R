# Unit tests for modulus and integer division

test_that("%% and %/%", {
  x <- -7:7
  x <- expand.grid(a = x, b = x)
  x <- x[x$b != 0, ] # avoid divisor == 0
  x <- as.list(x)

  rem_double <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    a %% b
  }

  expect_translation_snapshots(rem_double)
  expect_quick_equal(rem_double, lapply(x, function(v) v + 0.2))

  rem_int <- function(a, b) {
    declare(type(a = integer(n)), type(b = integer(n)))
    a %% b
  }
  expect_quick_identical(rem_int, x)

  div_double <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    a %/% b
  }

  expect_translation_snapshots(div_double)
  expect_quick_equal(div_double, lapply(x, function(v) v + 0.2))

  div_int <- function(a, b) {
    declare(type(a = integer(n)), type(b = integer(n)))
    a %/% b
  }
  expect_quick_identical(div_int, x)
})
