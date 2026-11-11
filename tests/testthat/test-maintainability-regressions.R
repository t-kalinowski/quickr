test_that("diag identity sizes accept scalar wrappers", {
  for (expr in list(quote(c(3L)), quote((3L)), quote(c(TRUE)))) {
    fn <- as.function(list(substitute(
      {
        diag(x)
      },
      list(x = expr)
    )))
    expect_quick_identical(fn, list())
  }
})

test_that("explicit diag names uses the diagonal construction form", {
  for (use_names in c(TRUE, FALSE)) {
    fn <- as.function(list(substitute(
      {
        diag(3L, names = x)
      },
      list(x = use_names)
    )))
    expect_quick_identical(fn, list())
  }
})

test_that("reassignment shape guards preserve singleton axes", {
  fn <- function(a) {
    declare(type(a = double(n, 3)))
    x <- matrix(0, 1, 3)
    x <- a
    x
  }
  expect_quick_identical(fn, list(matrix(1:3 + 0, 1, 3)))
  compiled <- quick(fn)
  expect_error(
    compiled(matrix(1:6 + 0, 2, 3)),
    "reassignment must preserve the shape"
  )
})

test_that("diagonal sizes preserve division before integer coercion", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    diag(nrow = as.integer(n / 2L * 2L))
  }
  expect_quick_identical(fn, 3L, 5L)
})

test_that("binding sizes preserve division before integer coercion", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(as.integer(n / 2L * 2L))))
    cbind(x, 0, deparse.level = 0)
  }
  expect_quick_identical(fn, list(3L, as.double(1:3)))
})

test_that("dimension division preserves double precision", {
  fn <- function(x) {
    declare(type(x = double(1)))
    m <- diag(nrow = as.integer(x / 1L))
    sum(m)
  }
  expect_quick_identical(fn, 2.99999999, 3.00000001)
})

test_that("integer-only functions can divide dimensions", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = integer(as.integer(n / 2L))))
    x + 1L
  }
  expect_quick_identical(fn, list(5L, 1:2))
})

test_that("size coercions reject local as.integer functions", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    as.integer <- function(x) x + 1L
    numeric(as.integer(n))
  }
  expect_identical(fn(2L), numeric(3))
  expect_error(
    quick(fn),
    "local as.integer() functions are not supported in size expressions",
    fixed = TRUE
  )

  builtin <- function(n) {
    declare(type(n = integer(1)))
    numeric(as.integer(n))
  }
  expect_quick_identical(builtin, 2L)

  identity <- function(x) {
    declare(type(x = double(1)))
    as.integer <- function(x) x + 1L
    m <- diag(x)
    numeric(nrow(m))
  }
  expect_quick_identical(identity, 2.9)
})

test_that("size arithmetic preserves double operands until integer coercion", {
  fn <- function(x, values) {
    declare(type(x = double(1)), type(values = double(as.integer(x))))
    numeric(as.integer(10L - x))
  }
  expect_quick_identical(fn, list(2.9, c(1, 2)))

  multiplied <- function(x, values) {
    declare(type(x = double(1)), type(values = double(as.integer(2L * x))))
    numeric(as.integer(2L * x))
  }
  # The input shape check catches early truncation before an undersized
  # result could be allocated and written by the generated function.
  expect_quick_identical(multiplied, list(2.9, as.double(1:5)))
})

test_that("replacement values precede subscript effects", {
  local <- function() {
    x <- c(0, 0, 0)
    x[as.integer(runif(1L, 1, 4))] <- runif(1L)
    x
  }
  host <- function() {
    x <- c(0, 0, 0)
    replace <- function() {
      x[as.integer(runif(1L, 1, 4))] <<- runif(1L)
      NULL
    }
    replace()
    x
  }
  for (fn in list(local, host)) {
    compiled <- quick(fn)
    expect_identical(set_seed_and_call(compiled), set_seed_and_call(fn))
  }

  binding <- function() {
    x <- c(0, 0, 0)
    value <- 2
    index <- function() {
      value <<- 3
      1L
    }
    x[index()] <- value
    x
  }
  expect_quick_identical(binding, list())

  deferred <- function(x) {
    declare(type(x = double(3)), type(value = double(NA)))
    value <- x
    x[rev(seq_along(x))] <- value
    x
  }
  expect_quick_identical(deferred, c(2, 3, 4))
})
