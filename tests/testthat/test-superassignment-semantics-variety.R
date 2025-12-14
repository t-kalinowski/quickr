test_that("<<- supports whole-vector superassignment via x[] across lengths", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function() {
      x[] <<- x + 1.0
      NULL
    }
    f()
    x
  }

  expect_quick_identical(fn, list(1), list(c(1, 2)), list(c(1, 2, 3, 4, 5)))
})

test_that("<<- supports vector indexing with double scalar indices (casts to int)", {
  fn <- function(x, idx) {
    declare(type(x = double(NA)), type(idx = double(1)))
    f <- function() {
      x[idx] <<- x[idx] + 1.0
      NULL
    }
    f()
    x
  }

  x <- c(1, 2, 3, 4)
  expect_quick_identical(fn, list(x, 1), list(x, 2), list(x, 4))
})

test_that("<<- supports matrix element and section targets together", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    j <- 2L
    f <- function() {
      x[1, ] <<- 1.0
      x[, j] <<- x[, j] + 2.0
      x[2, 2] <<- x[2, 2] + 3.0
      NULL
    }
    f()
    x
  }

  set.seed(1)
  expect_quick_identical(
    fn,
    list(matrix(runif(6), 2, 3)),
    list(matrix(runif(12), 3, 4))
  )
})

test_that("<<- mutating a matrix argument preserves caller copy-on-modify semantics", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    f <- function() {
      x[1, ] <<- 0.0
      x[, 1] <<- x[, 1] + 1.0
      NULL
    }
    f()
    x
  }

  qfn <- quick(fn)
  set.seed(1)
  x0 <- matrix(runif(12), 3, 4)
  x <- x0
  out <- qfn(x)

  expect_identical(out, fn(x0))
  expect_identical(x, x0)
})

test_that("<<- supports 3D slice superassignment with missing indices", {
  fn <- function(nx, ny, nz) {
    declare(type(nx = integer(1)), type(ny = integer(1)), type(nz = integer(1)))
    a <- array(0.0, c(nx, ny, nz))

    f <- function(k) {
      a[,, k] <<- as.double(k)
      a[1, 1, k] <<- a[1, 1, k] + 0.5
      NULL
    }

    f(1L)
    f(nz)
    a
  }

  expect_quick_identical(fn, list(2L, 3L, 2L), list(3L, 2L, 3L))
})

test_that("sapply + <<- can mutate a matrix host argument and return a matrix", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    out <- x
    f <- function(j) {
      x[, j] <<- x[, j] * 2.0
      x[, j]
    }
    out <- sapply(seq_len(ncol(x)), f, simplify = "array")
    list(x = x, out = out)
  }

  qfn <- quick(fn)
  set.seed(1)
  x0 <- matrix(runif(12), 3, 4)
  x <- x0
  out <- qfn(x)

  expect_identical(out, fn(x0))
  expect_identical(x, x0)
})

test_that("sapply + <<- supports host scalar accumulation", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    acc <- 0.0
    f <- function(i) {
      acc <<- acc + x[i]
      acc
    }
    out <- sapply(seq_along(x), f)
    list(acc = acc, out = out)
  }

  set.seed(1)
  x <- runif(10)
  expect_quick_identical(fn, list(x))
})

test_that("statement closures can be called in loops while mutating host arrays", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    x <- integer(n)
    f <- function(i) {
      x[i] <<- i
      NULL
    }
    for (i in seq_len(n)) {
      f(i)
    }
    x
  }

  expect_quick_identical(fn, list(1L), list(2L), list(7L))
})

test_that("<<- supports rank>1 linear indexing with computed scalar indices", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    k <- nrow(x) * ncol(x)
    kk <- as.double(k)
    f <- function() {
      x[k] <<- x[k] + 1.0
      x[kk] <<- x[kk] + 2.0
      NULL
    }
    f()
    x
  }

  set.seed(1)
  expect_quick_identical(
    fn,
    list(matrix(runif(6), 2, 3)),
    list(matrix(runif(12), 3, 4))
  )
})

test_that("<<- supports 3D element superassignment via a[i, j, k]", {
  fn <- function(nx, ny, nz) {
    declare(type(nx = integer(1)), type(ny = integer(1)), type(nz = integer(1)))
    a <- array(0.0, c(nx, ny, nz))

    bump <- function(i, j, k) {
      a[i, j, k] <<- as.double(i + j + k)
      NULL
    }

    bump(1L, 1L, 1L)
    bump(nx, ny, nz)
    a
  }

  expect_quick_identical(fn, list(2L, 3L, 2L), list(3L, 2L, 3L))
})
