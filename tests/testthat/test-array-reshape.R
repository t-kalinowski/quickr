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

test_that("array() reshape works when data is scalar-emitted (e.g. integer(n))", {
  fn <- function() {
    # `integer(3)` currently lowers to scalar `0` with a non-scalar value shape.
    # The array() reshape path must produce valid Fortran anyway.
    array(integer(3L), dim = c(1L, 3L))
  }

  expect_quick_identical(fn, list())
})

test_that("array() reshape accepts literal dim vectors in the AST", {
  dim_const <- c(2L, 3L, 4L)
  fn <- eval(bquote(function(x) {
    declare(type(x = integer(2L, 3L, 4L)))
    array(as.double(x), dim = .(dim_const))
  }))

  set.seed(1)
  x <- array(sample(1:10, 24, replace = TRUE), dim = c(2L, 3L, 4L))
  expect_quick_identical(fn, list(x))
})

test_that("array() reshape accepts dim as a literal sequence (2:4)", {
  fn <- function(x) {
    declare(type(x = integer(2L, 3L, 4L)))
    array(as.double(x), dim = 2:4)
  }

  set.seed(1)
  x <- array(sample(1:10, 24, replace = TRUE), dim = c(2L, 3L, 4L))
  expect_quick_identical(fn, list(x))
})

test_that("array() reshape accepts dim passed as a variable bound to a literal sequence", {
  fn <- function(x) {
    declare(type(x = integer(2L, 3L, 4L)))
    d <- 2:4
    array(as.double(x), dim = d)
  }

  set.seed(1)
  x <- array(sample(1:10, 24, replace = TRUE), dim = c(2L, 3L, 4L))
  expect_quick_identical(fn, list(x))
})

test_that("array() fill reshape handles dim expressions that lower to comma-containing Fortran", {
  fn <- function(y, x) {
    declare(type(y = double(NA, NA)), type(x = double(nrow(y), ncol(y))))

    # `dim(x)` uses the dims declared above, which dims2f() lowers to
    # `size(y, 1), size(y, 2)` (commas inside expressions). Codegen must not
    # split on commas in Fortran output.
    array(integer(nrow(y) * ncol(y)), dim = dim(x))
  }

  set.seed(1)
  y <- matrix(runif(6), 2, 3)
  x <- y
  expect_quick_identical(fn, list(y, x))
})

test_that("array() reshape supports dim = 1 for non-scalar data", {
  fn <- function(x) {
    declare(type(x = integer(2L, 3L, 4L)))
    # Rank-1 length-1 arrays are scalar-like in quickr; index the first element
    # to compare against base R without relying on `dim` attributes.
    array(as.double(x), dim = 1L)[1]
  }

  set.seed(1)
  x <- array(sample(1:10, 24, replace = TRUE), dim = c(2L, 3L, 4L))
  expect_quick_identical(fn, list(x))
})
