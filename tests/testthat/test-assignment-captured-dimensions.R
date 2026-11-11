test_that("superassignment uses captured extents when a formal shadows them", {
  fn <- function(n, m, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m))
    )
    f <- function(n) {
      x <<- y
      NULL
    }
    f(1L)
    x
  }

  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4)))
  qfn <- quick(fn)
  expect_error(
    qfn(2L, 3L, c(1, 2), c(3, 4, 5)),
    "reassignment must preserve the shape of `x`",
    fixed = TRUE
  )
})

test_that("superassignment uses captured extents when a local shadows them", {
  fn <- function(n, m, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m))
    )
    f <- function() {
      n <- 1L
      x <<- y
      NULL
    }
    f()
    x
  }

  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4)))
})

test_that("assignment expressions retain captured replacement extents", {
  fn <- function(n, m, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m))
    )
    f <- function(m) {
      x <<- y + 1
      NULL
    }
    f(1L)
    x
  }

  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4)))
  qfn <- quick(fn)
  expect_error(
    qfn(2L, 3L, c(1, 2), c(3, 4, 5)),
    "reassignment must preserve the shape of `x`",
    fixed = TRUE
  )
})

test_that("local assignments retain captured replacement extents", {
  fn <- function(n, m, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m))
    )
    f <- function(m) {
      declare(type(a = double(2)))
      a <- y
      x <<- a
      NULL
    }
    f(1L)
    x
  }

  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4)))
})

test_that("equal dimension names in different scopes do not prove equal extents", {
  fn <- function(n, x, replacement_n) {
    declare(
      type(n = integer(1)),
      type(x = double(n)),
      type(replacement_n = integer(1))
    )
    f <- function(n) {
      declare(type(y = double(n)))
      y <- double(n)
      x <<- y
      NULL
    }
    f(replacement_n)
    x
  }

  expect_quick_identical(fn, list(2L, c(1, 2), 2L))
  qfn <- quick(fn)
  expect_error(
    qfn(2L, c(1, 2), 3L),
    "reassignment must preserve the shape of `x`",
    fixed = TRUE
  )
})

test_that("reassigning a dimension binding does not change stored extents", {
  fn <- function(n, m, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m))
    )
    n <- 1L
    x <- y
    x
  }

  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4)))
})

test_that("captured matrix assignment checks each stored extent", {
  fn <- function(n, m, p, q, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(p = integer(1)),
      type(q = integer(1)),
      type(x = double(n, m)),
      type(y = double(p, q))
    )
    f <- function(n, m, p, q) {
      x <<- y
      NULL
    }
    f(1L, 1L, 1L, 1L)
    x
  }

  x <- matrix(as.double(1:6), 2, 3)
  y <- matrix(as.double(7:12), 2, 3)
  expect_quick_identical(fn, list(2L, 3L, 2L, 3L, x, y))
  qfn <- quick(fn)
  expect_error(
    qfn(2L, 3L, 3L, 2L, x, t(y)),
    "reassignment must preserve the shape of `x`",
    fixed = TRUE
  )
})

test_that("vector fills use the replacement scope's extent", {
  fn <- function(n, x, replacement_n) {
    declare(
      type(n = integer(1)),
      type(x = double(n)),
      type(replacement_n = integer(1))
    )
    f <- function(n) {
      x <<- numeric(n)
      NULL
    }
    f(replacement_n)
    x
  }

  expect_quick_identical(fn, list(2L, c(1, 2), 2L))
  qfn <- quick(fn)
  expect_error(
    qfn(2L, c(1, 2), 3L),
    "reassignment must preserve the shape of `x`",
    fixed = TRUE
  )
})

test_that("matrix and array fills use the replacement scope's extents", {
  for (fill in list(quote(matrix(value, n, m)), quote(array(value, c(n, m))))) {
    fn <- eval(substitute(
      function(n, m, x, replacement_n, replacement_m, value) {
        declare(
          type(n = integer(1)),
          type(m = integer(1)),
          type(x = double(n, m)),
          type(replacement_n = integer(1)),
          type(replacement_m = integer(1)),
          type(value = double(1))
        )
        f <- function(n, m) {
          x <<- FILL
          NULL
        }
        f(replacement_n, replacement_m)
        x
      },
      list(FILL = fill)
    ))

    x <- matrix(as.double(1:6), 2, 3)
    expect_quick_identical(fn, list(2L, 3L, x, 2L, 3L, 7))
    qfn <- quick(fn)
    expect_error(
      qfn(2L, 3L, x, 3L, 2L, 7),
      "reassignment must preserve the shape of `x`",
      fixed = TRUE
    )
  }
})

test_that("a shadowed self-size owner does not disable assignment guards", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    f <- function() {
      x <- c(0, 0)
      x <<- y
      NULL
    }
    f()
    x
  }

  expect_quick_identical(fn, list(c(1, 2), c(3, 4)))
  qfn <- quick(fn)
  expect_error(
    qfn(c(1, 2), c(3, 4, 5)),
    "reassignment must preserve the shape of `x`",
    fixed = TRUE
  )
})

test_that("assignment size inquiries do not repeat random draws", {
  fn_assign <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n)))
    x <- runif(n)
    x
  }
  fn_superassign <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n)))
    f <- function() {
      x <<- runif(n)
      NULL
    }
    f()
    x
  }

  for (fn in list(fn_assign, fn_superassign)) {
    qfn <- quick(fn)
    withr::local_seed(123)
    expected <- fn(3L, double(3))
    expected_next <- runif(1)
    set.seed(123)
    expect_identical(qfn(3L, double(3)), expected)
    expect_identical(runif(1), expected_next)
  }
})

test_that("inferred deferred locals can change their extents", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    y <- x[x > 0]
    y <- x
    sum(y)
  }

  expect_quick_identical(fn, c(-1, 2, 3))
})
