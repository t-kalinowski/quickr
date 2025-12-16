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

  x1 <- c(1, 2, 3)
  x2 <- c(-1, 0, 4)

  expect_identical(fn(x1), c(2, 2, 3))
  expect_identical(fn(x2), c(0, 0, 4))
  expect_quick_identical(fn, x1, x2, as.double(seq(-10, 10)))

  qfn <- quick(fn)
  for (x0 in list(x1, x2)) {
    x <- x0
    out <- qfn(x)
    expect_identical(out, fn(x0))
    expect_identical(x, x0)
  }
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

test_that("<- shadows captured variables (use <<- to mutate the host)", {
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

  x1 <- c(1, 2, 3)
  x2 <- c(-1.5, 0.25, 2.0)

  expect_identical(fn(x1), x1 * 2)
  expect_identical(fn(x2), x2 * 2)
  expect_quick_identical(fn, x1, x2, as.double(seq(-10, 10)))

  qfn <- quick(fn)
  for (x0 in list(x1, x2)) {
    x <- x0
    out <- qfn(x)

    expect_identical(out, x0 * 2)
    expect_identical(out, fn(x0))
    expect_identical(x, x0)
  }
})

test_that("<<- targets the host when the name is shadowed by <- (vector)", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function(i) {
      x <- x + 1.0
      x[i] <<- x[i] + 1.0
      NULL
    }
    f(1L)
    x
  }

  x1 <- as.double(1:5)
  x2 <- c(-1.5, 0.25, 2.0)

  expected1 <- x1
  expected1[[1L]] <- x1[[1L]] + 2.0
  expected2 <- x2
  expected2[[1L]] <- x2[[1L]] + 2.0

  expect_identical(fn(x1), expected1)
  expect_identical(fn(x2), expected2)
  expect_quick_identical(fn, x1, x2, as.double(seq(-10, 10)))

  qfn <- quick(fn)
  for (x0 in list(x1, x2)) {
    x <- x0
    out <- qfn(x)

    expected <- x0
    expected[[1L]] <- x0[[1L]] + 2.0
    expect_identical(out, expected)
    expect_identical(out, fn(x0))
    expect_identical(x, x0)
  }
})

test_that("<<- targets the host when the name is shadowed by <- (matrix subset)", {
  fn <- function(x) {
    declare(type(x = double(3, 4)))
    i <- 2L
    j <- 3L

    f <- function() {
      x <- x + 1.0
      x[i, j] <<- x[i, j] + 1.0
      NULL
    }
    f()
    x
  }

  x1 <- matrix(as.double(1:12), 3, 4)
  x2 <- matrix(as.double(-11:0), 3, 4)

  expected1 <- x1
  expected1[2, 3] <- x1[2, 3] + 2.0
  expected2 <- x2
  expected2[2, 3] <- x2[2, 3] + 2.0

  expect_identical(fn(x1), expected1)
  expect_identical(fn(x2), expected2)
  expect_quick_identical(fn, x1, x2)

  qfn <- quick(fn)
  for (x0 in list(x1, x2)) {
    x <- x0
    out <- qfn(x)

    expected <- x0
    expected[2, 3] <- x0[2, 3] + 2.0
    expect_identical(out, expected)
    expect_identical(out, fn(x0))
    expect_identical(x, x0)
  }
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
