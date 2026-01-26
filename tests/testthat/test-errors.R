test_that("case-sensitive variable name clashes", {
  expect_snapshot(
    quick(function(j) {
      declare(type(j = integer(1)))
      J <- double(length = j)
      J
    }),
    error = TRUE
  )
})

test_that("non-final expressions must be assigned", {
  expect_error(regexp = "all expressions except the final return", {
    quick(function(x) {
      declare(type(x = double(1)))
      x + 1
      x
    })
  })

  expect_error(regexp = "all expressions except the final return", {
    quick(function(x) {
      declare(type(x = double(1)))
      x
      x
    })
  })

  expect_error(regexp = "all expressions except the final return", {
    quick(function(x) {
      declare(type(x = double(1)))
      x <- x + 1
      x + 1
      x
    })
  })
})

test_that("value-returning local closures can be called as statements", {
  fn <- function(x) {
    declare(type(x = double(1)))

    apply_boundary_conditions <- function() {
      x <<- x + 1
    }

    apply_boundary_conditions()
    x
  }

  # r2f(fn)

  expect_quick_identical(fn, list(1))

  fn <- function(x) {
    declare(type(x = double(1)))

    apply_boundary_conditions <- function() {
      x <<- x + 1
      x
    }

    apply_boundary_conditions()
    x
  }

  # r2f(fn)

  expect_quick_identical(fn, list(1))
})

test_that("reserved or underscored names are rejected", {
  expect_snapshot(
    quick(function(x) {
      `_bad` <- x + 1L
      `_bad`
    }),
    error = TRUE
  )

  expect_snapshot(
    quick(function(x) {
      `bad_` <- x + 1L
      `bad_`
    }),
    error = TRUE
  )

  expect_snapshot(
    quick(function(int) {
      int
    }),
    error = TRUE
  )

  expect_snapshot(
    quick(function(`foo.bar`, foo_bar) {
      1
    }),
    error = TRUE
  )
})

test_that("closure return mode must match output mode", {
  expect_error(
    quick(function(x) {
      declare(type(x = double(1)))
      out <- integer(1)
      compute <- function() x + 1
      out <- compute()
      out
    }),
    "closure result mode.*does not match output mode"
  )
})

test_that("closure must return scalar for scalar outputs", {
  expect_error(
    quick(function(x) {
      declare(type(x = double(3)))
      s <- 0
      compute <- function() x
      s <- compute()
      s
    }),
    "closure must return a scalar"
  )
})

test_that("missing argument declarations get a clear error", {
  expect_error(
    quick(function(x) {
      x + 1
    }),
    "arg not declared: x"
  )
})
