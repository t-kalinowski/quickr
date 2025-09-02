test_that("multiple return values", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    z <- x + 2
    list(y = y, z = z)
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})

test_that("multiple return values via assignment", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    z <- x + 2
    out <- list(y = y, z = z)
    out
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})

test_that("single return variable still works", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    y
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})

test_that("custom names for multiple return values are preserved", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1L
    z <- x + 2L
    list(abc = y, def = z)
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})

test_that("no names add to unnamed elements", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1L
    z <- x + 2L
    list(y, z)
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})


test_that("single-element unnamed list returns a list", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1L
    list(y)
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})


test_that("single-element named list preserves name", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1L
    list(abc = y)
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})


test_that("mixed named and unnamed list work", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1L
    z <- x + 2L
    list(y, abc = z, x)
  }
  qfn <- quick(fn)
  x <- 1:3
  expect_equal(qfn(x), fn(x))
})


test_that("errors if list is used outside return pattern [r2f error]", {
  ## list not last or second to last
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    z <- x + 2
    out <- list(y = y, z = z)
    gg <- 1 + 1
    out
  }
  expect_error(quick(fn), "Unsupported function: list")
  ## List is being accessed
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    z <- x + 2
    out <- list(y = y, z = z)
    out$y
  }
  expect_error(quick(fn), "Unsupported function: list")
})


test_that("errors om nested list and non-symbols [validate_list_symbols]", {
  ## Nested lists
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    z <- x + 2
    out <- list(y = y, z = z, list(x = x))
    out
  }
  x <- 1:3
  expect_error(quick(fn), "all elements of the list must be symbols")

  ## non-symbols in list
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    z <- x + 2
    list(a = 1L, b = 2L)
  }
  x <- 1:3
  expect_error(quick(fn), "all elements of the list must be symbols")

  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1
    z <- x + 2
    list(1L)
  }
  x <- 1:3
  expect_error(quick(fn), "all elements of the list must be symbols")
})


test_that("errors on non-syntactic names in return list", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1L
    list(`a b` = y)
  }
  expect_error(quick(fn), "only syntactic names are valid")
})
