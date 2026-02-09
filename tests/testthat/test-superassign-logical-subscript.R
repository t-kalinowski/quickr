test_that("logical-vector subscripts work for [<<- LHS", {
  logical_superassign <- function(x) {
    declare(type(x = double(n)))
    f <- function() {
      x[x > 0] <<- 0
      NULL
    }
    f()
    x
  }

  x <- c(-1, 2, -3, 4)
  expect_quick_identical(logical_superassign, x)
})
