test_that("multiple return values", {
  slow_fun <- function(x) {
    declare(
      type(x = double(n)),
      type(y = double(n)),
      type(z = double(n))
    )
    y <- x + 1
    z <- x + 2
    list(y = y, z = z)
  }
  quick_fun <- quick(slow_fun)
  x <- as.double(1:3)
  expect_equal(quick_fun(x), slow_fun(x))
})

test_that("multiple return values via assignment", {
  slow_fun <- function(x) {
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
  quick_fun <- quick(slow_fun)
  x <- as.double(1:3)
  expect_equal(quick_fun(x), slow_fun(x))
})

test_that("single return variable still works", {
  slow_fun <- function(x) {
    declare(
      type(x = double(n)),
      type(y = double(n))
    )
    y <- x + 1
    y
  }
  quick_fun <- quick(slow_fun)
  x <- as.double(1:3)
  expect_equal(quick_fun(x), slow_fun(x))
})
