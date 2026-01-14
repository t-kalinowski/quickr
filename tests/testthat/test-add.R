# Unit tests for simple addition

test_that("add1", {
  slow_add1 <- function(x) {
    declare(type(x = double(NA)))
    x <- x + 1
    x
  }

  expect_quick_equal(slow_add1, as.double(1:3), c(1, 2, 3), 1)
})

test_that("add2", {
  slow_add2 <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(n)))
    out <- x + y
    out
  }

  x <- 1:3
  y <- 4:6
  expect_quick_identical(slow_add2, list(x, y))
})
