test_that("array() supports reshaping non-scalar data", {
  fn <- function(x) {
    declare(type(x = integer(2L, 3L, 4L)))
    array(as.double(x), dim = c(2L, 3L, 4L))
  }

  set.seed(1)
  x <- array(sample(1:10, 24, replace = TRUE), dim = c(2L, 3L, 4L))
  expect_quick_identical(fn, list(x))
})

test_that("array() reshape accepts numeric dim vectors", {
  fn <- function(x) {
    declare(type(x = integer(2L, 3L, 4L)))
    array(as.double(x), dim = c(2, 3, 4))
  }

  set.seed(1)
  x <- array(sample(1:10, 24, replace = TRUE), dim = c(2L, 3L, 4L))
  expect_quick_identical(fn, list(x))
})

test_that("array() reshape accepts scalar dims", {
  fn <- function(x) {
    declare(type(x = integer(24L)))
    # Rank-1 arrays carry a `dim` attribute in base R, but quickr treats them as
    # plain vectors; wrap in `c()` so both sides compare identically while still
    # exercising the `array(dim=scalar)` lowering.
    c(array(as.double(x), dim = 24))
  }

  set.seed(1)
  x <- sample(1:10, 24, replace = TRUE)
  expect_quick_identical(fn, list(x))
})
