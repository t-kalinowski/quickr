test_that("rep.int works in subset indexing", {
  f <- function(x) {
    declare(type(x = double(10)))
    x[rep.int(1L, 5L)]
  }

  expect_quick_identical(f, list(as.double(1:10)))
})

test_that("rep.int is not silently supported outside subset indexing", {
  # Regression: a rep.int() handler intended for subscripts must not change
  # program semantics when called as a general value constructor.
  f <- function(n) {
    declare(type(n = integer(1)))
    rep.int(1.5, n)
  }

  expect_error(quick(f), "rep.int", fixed = TRUE)
})

test_that("rep.int fails cleanly (no internal slot errors) for NULL args in indexing", {
  bad_x <- function(x) {
    declare(type(x = double(10)))
    x[rep.int(NULL, 2L)]
  }
  bad_times <- function(x) {
    declare(type(x = double(10)))
    x[rep.int(1L, NULL)]
  }

  # Prior bug: would error with "no applicable method for '@' applied to an
  # object of class \"NULL\"" rather than a controlled message.
  expect_error(quick(bad_x), "rep.int\\(\\)", fixed = FALSE)
  expect_error(quick(bad_times), "rep.int\\(\\)", fixed = FALSE)
})

test_that("rep.int supports named times= in subset indexing", {
  f <- function(x) {
    declare(type(x = double(10)))
    x[rep.int(3L, times = 2L)]
  }

  expect_quick_identical(f, list(as.double(1:10)))
})

test_that("rep.int supports non-literal times in subset indexing", {
  f <- function(x, n) {
    declare(type(x = double(10)))
    declare(type(n = integer(1)))
    x[rep.int(2L, n)]
  }

  expect_quick_identical(f, list(as.double(1:10), 4L))
})
