# Unit test for mask hoisting

skip_on_cran()

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

test_that("any()/all() drop an inherited hoist_mask from an enclosing reduction", {
  # An enclosing numeric reduction threads its own hoist_mask through
  # `...`; any()/all() must install a fresh mask hoister for their own
  # argument instead of forwarding both (which handed the `[` handler
  # two hoist_mask arguments).
  fn <- function(x, m) {
    declare(type(x = double(n)), type(m = logical(n)))
    out <- sum(x * as.double(any(x[m] > 1)))
    out
  }
  x <- c(0.5, 2, 3)
  expect_quick_identical(fn, list(x, c(TRUE, FALSE, TRUE)))
  expect_quick_identical(fn, list(x, c(TRUE, FALSE, FALSE)))

  fn_all <- function(x, m) {
    declare(type(x = double(n)), type(m = logical(n)))
    out <- sum(x * as.double(all(x[m] > 1)))
    out
  }
  expect_quick_identical(fn_all, list(x, c(FALSE, TRUE, TRUE)))
  expect_quick_identical(fn_all, list(x, c(TRUE, FALSE, TRUE)))
})
