# Unit test for mask hoisting

test_that("hoist mask", {
  # no mask to hoist
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- max(x)
    out
  }

  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub)

  expect_snapshot(
    {
      fn
      cat(fsub)
      cat(cwrapper)
    },
    transform = scrub_environment
  )

  x <- runif(100, -10, 10)
  qfn := quick(fn)
  expect_equal(qfn(x), fn(x))

  # mask hoists
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- max(x[x >= 0])
    out
  }

  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub)

  expect_snapshot(
    {
      fn
      cat(fsub)
      cat(cwrapper)
    },
    transform = scrub_environment
  )

  x <- runif(100, -10, 10)
  qfn := quick(fn)
  expect_equal(qfn(x), fn(x))
  # bench::mark(qfn(x), fn(x), relative = T)
})
