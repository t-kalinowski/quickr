test_that("case-sensitive variable name clashes", {
  expect_error(regexp = "case-insensitive.+`j`.+`J`", {
    quick(function(j) {
      declare(type(j = integer(1)))
      J <- double(length = j)
      J
    })
  })
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
  expect_error(
    quick(function(x) {
      declare(type(x = integer(1)))
      `_bad` <- x + 1L
      `_bad`
    }),
    "symbols cannot start or end with '_'",
    fixed = TRUE
  )

  expect_error(
    quick(function(x) {
      declare(type(x = integer(1)))
      `bad_` <- x + 1L
      `bad_`
    }),
    "symbols cannot start or end with '_'",
    fixed = TRUE
  )

  expect_error(
    quick(function(int) {
      declare(type(int = integer(1)))
      int
    }),
    "symbols cannot start or end with '_'",
    fixed = TRUE
  )
})
