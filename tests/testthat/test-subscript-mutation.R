skip_on_cran()

test_that("subscript replacement preserves inputs that are not returned", {
  fn <- function(x) {
    declare(type(x = double(3)))
    x[2L] <- 9
    sum(x)
  }

  expect_translation_snapshots(fn)
  x <- c(1, 2, 3)
  expected <- fn(x)
  expect_identical(quick(fn)(x), expected)
  expect_identical(x, c(1, 2, 3))
})

test_that("logical replacement preserves the caller's vector and mask", {
  fn <- function(x, mask) {
    declare(type(x = double(n)), type(mask = logical(n)))
    x[mask] <- 0
    x
  }

  expect_translation_snapshots(fn)
  x <- c(1, 2, 3)
  mask <- c(TRUE, FALSE, TRUE)
  expected <- fn(x, mask)
  expect_identical(quick(fn)(x, mask), expected)
  expect_identical(x, c(1, 2, 3))
  expect_identical(mask, c(TRUE, FALSE, TRUE))
})

test_that("subscript replacement copies a modified argument before aliases read it", {
  fn <- function(x, y) {
    declare(type(x = double(3)), type(y = double(3)))
    x[1L] <- x[1L] + 1
    list(x = x, y = y)
  }

  expect_translation_snapshots(fn)
  x <- c(1, 2, 3)
  expected <- fn(c(1, 2, 3), c(1, 2, 3))
  expect_identical(quick(fn)(x, x), expected)
  expect_identical(x, c(1, 2, 3))
})
