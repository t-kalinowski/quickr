test_that("<<- mutates host locals in statement local closures", {
  fn <- function(nx, ny) {
    declare(
      type(nx = integer(1)),
      type(ny = integer(1))
    )
    temp <- matrix(0, nx, ny)

    bc <- function() {
      temp[1, ] <<- 1
      temp[nx, ] <<- 2
      temp[, 1] <<- 3
      temp[, ny] <<- 4
      NULL
    }

    bc()
    temp
  }

  expect_quick_identical(fn, list(4L, 5L), list(2L, 3L))
})

test_that("<<- mutating a host argument preserves caller copy-on-modify semantics", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function(i) {
      x[i] <<- x[i] + 1
      NULL
    }
    f(1L)
    x
  }

  qfn <- quick(fn)
  x0 <- c(1, 2, 3)
  x <- x0
  out <- qfn(x)

  expect_identical(out, fn(x0))
  expect_identical(x, x0)
})

test_that("sapply + <<- can mutate host and return values", {
  fn <- function(x) {
    declare(type(x = double(NA)), type(out = double(length(x))))
    f <- function(i) {
      x[i] <<- x[i] * 2
      x[i]
    }
    out <- sapply(seq_along(x), f)
    list(x = x, out = out)
  }

  set.seed(1)
  x <- runif(10)
  expect_quick_identical(fn, list(x))
})

test_that("<- cannot assign to captured variables (use <<- instead)", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    f <- function(i) {
      x <- x * 2
      x[i]
    }
    out <- sapply(seq_along(out), f)
    out
  }

  expect_error(r2f(fn), "must not assign to captured variables", fixed = TRUE)
})

test_that("<<- supports 2D subset targets", {
  fn <- function(nx, ny, dt) {
    declare(
      type(nx = integer(1)),
      type(ny = integer(1)),
      type(dt = double(1))
    )
    temp <- matrix(0, nx, ny)

    bump <- function(i, j) {
      temp[i, j] <<- temp[i, j] + dt
      NULL
    }

    bump(2L, 3L)
    bump(nx, ny)
    temp
  }

  expect_quick_identical(fn, list(4L, 5L, 0.25), list(2L, 3L, 1.0))
})
