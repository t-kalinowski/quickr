test_that("multiple return values", {
  fn <- function(x) {
    declare(
      type(x = double(n)),
      type(y = double(n)),
      type(z = double(n))
    )
    y <- x + 1
    z <- x + 2
    list(y = y, z = z)
  }
  qfn <- quick(fn)
  x <- as.double(1:3)
  expect_equal(qfn(x), fn(x))
})

test_that("multiple return values via assignment", {
  fn <- function(x) {
    declare(
      type(x = double(n)),
      type(y = double(n)),
      type(z = double(n))
    )
    y <- x + 1
    z <- x + 2
    out <- list(y = y, z = z)
    out
  }
  qfn <- quick(fn)
  x <- as.double(1:3)
  expect_equal(qfn(x), fn(x))
})

test_that("single return variable still works", {
  fn <- function(x) {
    declare(
      type(x = double(n)),
      type(y = double(n))
    )
    y <- x + 1
    y
  }
  qfn <- quick(fn)
  x <- as.double(1:3)
  expect_equal(qfn(x), fn(x))
})

test_that("Errors if list is used outside return pattern [r2f error]", {
  ## list not last or second to last
  fn <- function(x) {
    declare(
      type(x = double(n)),
      type(y = double(n)),
      type(z = double(n))
    )
    y <- x + 1
    z <- x + 2
    out <- list(y = y, z = z)
    gg <- 1 + 1
    out
  }
  expect_error(quick(fn), "Unsupported function: list")
  ## List is being accessed
  fn <- function(x) {
    declare(
      type(x = double(n)),
      type(y = double(n)),
      type(z = double(n))
    )
    y <- x + 1
    z <- x + 2
    out <- list(y = y, z = z)
    out$y
  }
  expect_error(quick(fn), "Unsupported function: list")

})