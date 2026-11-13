skip_on_cran()

test_that("prod promotes integer arrays before multiplying", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    prod(x)
  }

  expect_quick_identical(
    fn,
    c(50000L, 50000L),
    c(-50000L, 50000L),
    2L,
    integer()
  )
  expect_translation_snapshots(fn)
})

test_that("prod returns double for logical arrays", {
  fn <- function(x) {
    declare(type(x = logical(n)))
    prod(x)
  }

  expect_quick_identical(fn, c(TRUE, FALSE), c(TRUE, TRUE), TRUE, logical())
  expect_translation_snapshots(fn)
})

test_that("prod promotes integer and logical scalars and literals", {
  fn <- function(x, flag) {
    declare(type(x = integer(1)), type(flag = logical(1)))
    c(prod(x), prod(flag), prod(x, flag), prod(2L), prod(TRUE))
  }

  expect_quick_identical(fn, list(50000L, TRUE), list(-2L, FALSE))
  expect_translation_snapshots(fn)
})

test_that("prod promotes each argument before a multi-argument product", {
  fn <- function(x, y, flag) {
    declare(
      type(x = integer(n)),
      type(y = integer(m)),
      type(flag = logical(1))
    )
    prod(first = x, na.rm = FALSE, second = y, flag)
  }

  expect_quick_identical(
    fn,
    list(c(50000L, 50000L), c(2L, 3L), TRUE),
    list(50000L, 50000L, TRUE),
    list(integer(), c(2L, 3L), TRUE),
    list(integer(), integer(), FALSE)
  )
  expect_translation_snapshots(fn)
})

test_that("masked integer products promote before reducing", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    prod(x[x > 0L])
  }

  expect_quick_identical(fn, c(50000L, -1L, 50000L), -2:-1, integer())
  expect_translation_snapshots(fn)
})

test_that("masked logical products retain the empty-product identity", {
  fn <- function(x, mask) {
    declare(type(x = logical(n)), type(mask = logical(n)))
    prod(x[mask])
  }

  expect_quick_identical(
    fn,
    list(c(TRUE, FALSE), c(TRUE, FALSE)),
    list(c(TRUE, FALSE), c(FALSE, TRUE)),
    list(c(TRUE, FALSE), c(FALSE, FALSE)),
    list(logical(), logical())
  )
  expect_translation_snapshots(fn)
})

test_that("prod arguments give local closures the promoted result mode", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    identity <- function(value) value
    identity(prod(x))
  }

  expect_quick_identical(fn, c(50000L, 50000L), integer())
  expect_translation_snapshots(fn)
})

test_that("prod retains complex result mode", {
  fn <- function(x) {
    declare(type(x = complex(n)))
    prod(x)
  }

  expect_quick_identical(fn, c(1 + 2i, 3 - 1i), 2i, complex())
  expect_translation_snapshots(fn)
})
