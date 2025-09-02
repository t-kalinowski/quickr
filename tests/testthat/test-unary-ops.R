# Unit tests for unary operators (!, +, -)

test_that("logical not", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    lgl <- x > 1L
    not_lgl <- !lgl
    not_lgl
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(x = as.integer(0:3)))
})

test_that("unary minus", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- -x
    y
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(x = 1:3))
})

test_that("unary plus", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    y <- +x
    y
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(x = 1:3))
})
