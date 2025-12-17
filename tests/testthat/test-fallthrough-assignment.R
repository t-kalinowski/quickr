test_that("fall-through assignment is supported", {
  expect_quick_identical(
    function(x) {
      declare(type(x = double(1)))
      a <- b <- 1
      x + a + b
    },
    8
  )

  expect_quick_identical(
    function(x) {
      declare(type(x = double(1)))
      a <- (b <- 1)
      x + a + b
    },
    8
  )

  expect_quick_identical(
    function(x) {
      declare(type(x = double(1)))
      a <- b <- c <- x + 1
      a + b + c
    },
    8
  )
})
