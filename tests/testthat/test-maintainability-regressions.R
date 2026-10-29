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
