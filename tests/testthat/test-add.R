# Unit tests for simple addition

test_that("add1", {
  slow_add1 <- function(x) {
    declare(type(x = double(NA)))
    x <- x + 1
    x
  }
  r2f(slow_add1)

  fsub <- new_fortran_subroutine("slow_add1", slow_add1)
  cwrapper <- make_c_bridge(fsub)

  expect_snapshot(
    {
      slow_add1
      cat(fsub)
      cat(cwrapper)
    },
    transform = scrub_environment
  )

  quick_add1 <- quick(name = "slow_add1", slow_add1)
  expect_equal(quick_add1(as.double(1:3)), slow_add1(as.double(1:3)))
  expect_equal(quick_add1(c(1, 2, 3)), slow_add1(c(1, 2, 3)))
  expect_equal(quick_add1(1), slow_add1(1))
})

test_that("add2", {
  slow_add2 <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(n)))
    out <- x + y
    out
  }
  # r2f(slow_add2)

  fsub <- new_fortran_subroutine("slow_add2", slow_add2)
  cwrapper <- make_c_bridge(fsub)

  expect_snapshot(
    {
      slow_add2
      cat(fsub)
      cat(cwrapper)
    },
    transform = scrub_environment
  )

  x <- 1:3
  y <- 4:6
  quick_add2 <- quick(name = "slow_add2", slow_add2)
  expect_equal(quick_add2(x, y), slow_add2(x, y))
})
