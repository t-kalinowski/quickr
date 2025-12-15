test_that("direct-call statement closure can host-mutate with x <<- expr", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function() {
      x <<- x + 1
      NULL
    }
    f()
    x
  }

  qfn <- quick(fn)
  x0 <- c(1, 2, 3)
  x <- x0
  out <- qfn(x)

  expect_identical(out, fn(x0))
  expect_identical(x, x0)
})

test_that("direct-call statement closure supports rank>1 linear x[i] <<- ...", {
  fn <- function(x, k) {
    declare(type(x = double(a, b)), type(k = integer(1)))
    f <- function() {
      x[k] <<- x[k] + 1
      NULL
    }
    f()
    x
  }

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  expect_quick_identical(fn, list(x, 1L), list(x, 5L), list(x, 12L))
})

test_that("direct-call statement closure supports missing indices in x[ , ] <<- ...", {
  fn <- function(nx, ny) {
    declare(type(nx = integer(1)), type(ny = integer(1)))
    temp <- matrix(0, nx, ny)
    j <- 2L

    f <- function() {
      temp[, 1] <<- 1
      temp[1, ] <<- 2
      temp[, j] <<- 3
      temp[nx, ] <<- 4
      NULL
    }

    f()
    temp
  }

  expect_quick_identical(fn, list(3L, 4L), list(2L, 3L))
})

test_that("direct-call statement closure can use hoist+block inside contains", {
  fn <- function(nx, ny) {
    declare(type(nx = integer(1)), type(ny = integer(1)))
    temp <- matrix(0, nx, ny)

    bc <- function() {
      temp[1, 1] <<- (temp + 1.0)[1, 1]
      temp[nx, ny] <<- (temp + 2.0)[nx, ny]
      NULL
    }

    bc()
    temp
  }

  expect_quick_identical(fn, list(3L, 4L), list(2L, 2L))
})

test_that("sapply closures can use hoist paths inside contains", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    out <- sapply(seq_along(out), function(i) (x + 1.0)[i])
    out
  }

  set.seed(1)
  x <- runif(10)
  expect_quick_identical(fn, list(x))
})

test_that("sapply closures can use reduction mask-hoist paths inside contains", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    out <- sapply(seq_along(out), function(i) sum(x[x > 0.0]))
    out
  }

  set.seed(1)
  x <- runif(20) - 0.5
  expect_quick_identical(fn, list(x))
})

test_that("<<- is rejected outside local closures", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    x <<- x + 1
    x
  }

  expect_error(
    r2f(fn),
    "<<- is only supported inside local closures",
    fixed = TRUE
  )
})

test_that("<<- errors when target does not exist in the host scope", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function() {
      y <<- 1
      NULL
    }
    f()
    x
  }

  expect_error(r2f(fn), "existing variable", fixed = TRUE)
})

test_that("<<- errors when targeting a closure formal", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function(x) {
      x <<- x + 1
      NULL
    }
    f(1.0)
    x
  }

  expect_error(r2f(fn), "must not shadow closure formals", fixed = TRUE)
})

test_that("statement local closure calls must end with NULL", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function() 1
    f()
    x
  }

  expect_error(r2f(fn), "must end with `NULL`", fixed = TRUE)
})

test_that("local closure calls are rejected in expression position", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function(i) x[i]
    out <- double(1)
    out <- 1.0 + f(1L)
    out
  }

  expect_error(r2f(fn), "must be assigned", fixed = TRUE)
})

test_that("sapply() errors if the closure superassigns to the output variable", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    f <- function(i) {
      out[i] <<- x[i] + 1.0
      out[i]
    }
    out <- sapply(seq_along(out), f)
    out
  }

  expect_error(
    r2f(fn),
    "closure must not superassign to its output variable",
    fixed = TRUE
  )
})

test_that("<<- supports the same subset targets as [<- (e.g. x[1:2] <<- ...)", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function() {
      x[1:2] <<- 0
      NULL
    }
    f()
    x
  }

  expect_quick_identical(fn, list(as.double(1:5)))
})

test_that("<<- errors on drop = FALSE", {
  fn <- function(x) {
    declare(type(x = double(m, n)))
    f <- function() {
      x[1, 1, drop = FALSE] <<- 0
      NULL
    }
    f()
    x
  }

  expect_error(
    r2f(fn),
    "drop = FALSE not supported for superassignment",
    fixed = TRUE
  )
})
