test_that("S7 named binding compiles and calls quick functions", {
  S7::`:=`(
    increment,
    quick(function(x) {
      declare(type(x = double(n)))
      x + 1
    })
  )

  expect_identical(increment(c(1, 2)), c(2, 3))
})

test_that("logical symbols retain their shape through assignments", {
  fn <- function(x) {
    declare(type(x = logical(NA)))
    y <- x
    y
  }

  expect_quick_identical(fn, c(TRUE, FALSE), logical())
})
