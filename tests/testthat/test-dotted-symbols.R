test_that("quick() supports dotted argument names", {
  expect_quick_identical(
    function(foo.bar) {
      declare(type(foo.bar = double(3)))
      foo.bar + 1
    },
    foo.bar = c(1, 2, 3)
  )
})

test_that("quick() supports dotted local variable names", {
  expect_quick_identical(
    function(x) {
      declare(type(x = double(3)))
      out.var <- x + 1
      out.var
    },
    x = c(1, 2, 3)
  )
})

test_that("quick() supports dotted for-loop variables", {
  expect_quick_identical(
    function(x) {
      declare(type(x = double(3)))
      for (i.j in seq_along(x)) {
        x[i.j] <- x[i.j] + 1
      }
      x
    },
    x = c(1, 2, 3)
  )
})
