test_that("local closures support if/else control flow in sapply()", {
  fn <- function(x, thresh) {
    declare(type(x = double(NA)), type(thresh = double(1)))
    out <- double(length(x))
    f <- function(i) {
      val <- 0.0
      if (x[i] > thresh) {
        val <- x[i] * 2.0
      } else {
        val <- x[i] + 1.0
      }
      val
    }
    out <- sapply(seq_along(out), f)
    out
  }

  x0 <- c(0.2, 0.4)
  expect_identical(fn(x0, 0.3), c(1.2, 0.8))
  expect_identical(fn(x0, 0.5), c(1.2, 1.4))

  set.seed(1)
  x <- runif(20)
  expect_quick_identical(
    fn,
    list(x0, 0.3),
    list(x0, 0.5),
    list(x, 0.5)
  )
})

test_that("local closures support repeat/break/next control flow in sapply()", {
  fn <- function(x, thresh, limit) {
    declare(
      type(x = double(NA)),
      type(thresh = double(1)),
      type(limit = double(1))
    )
    out <- double(length(x))
    f <- function(i) {
      acc <- 0.0
      j <- 0L
      repeat {
        j <- j + 1L
        if (j > i) {
          break
        }
        if (x[j] < thresh) {
          next
        }
        acc <- acc + x[j]
        if (acc > limit) break
      }
      acc
    }
    out <- sapply(seq_along(out), f)
    out
  }

  x0 <- c(0.1, 0.5, 0.6)
  expect_identical(fn(x0, 0.2, 0.8), c(0.0, 0.5, 1.1))
  expect_identical(fn(x0, 0.2, 0.4), c(0.0, 0.5, 0.5))

  set.seed(1)
  x <- runif(15)
  expect_quick_identical(
    fn,
    list(x0, 0.2, 0.8),
    list(x0, 0.2, 0.4),
    list(x, 0.2, 1.0)
  )
})

test_that("statement local closures support while loops + <<- host mutation", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    x <- double(n)
    fill <- function() {
      i <- 1L
      while (i <= n) {
        x[i] <<- as.double(i)
        i <- i + 1L
      }
      NULL
    }
    fill()
    x
  }

  expect_identical(fn(1L), 1.0)
  expect_identical(fn(3L), c(1.0, 2.0, 3.0))
  expect_quick_identical(fn, list(1L), list(2L), list(3L), list(7L))
})

test_that("loop variables in local closures shadow host variables", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    j <- 999L
    out <- double(length(x))
    f <- function(i) {
      acc <- 0.0
      for (j in seq_len(i)) {
        acc <- acc + x[j]
      }
      acc
    }
    out <- sapply(seq_along(out), f)
    list(j = j, out = out)
  }

  x1 <- c(1, 2, 3)
  x2 <- c(0, 0, 0)
  expect_identical(fn(x1), list(j = 999L, out = c(1, 3, 6)))
  expect_identical(fn(x2), list(j = 999L, out = c(0, 0, 0)))

  set.seed(1)
  x <- runif(10)
  expect_quick_identical(fn, list(x1), list(x2), list(x))
})

test_that("inline function calls are lowered correctly", {
  fn <- function(x) {
    declare(type(x = double(1)))
    y <- (function(a) a + 1)(x)
    y
  }

  expect_identical(fn(1), 2)
  expect_quick_identical(fn, list(1), list(5))
})

test_that("parenthesized closure calls work", {
  fn <- function(x) {
    declare(type(x = double(1)))
    f <- function(a) a * 2
    y <- ((f))(x)
    y
  }

  expect_identical(fn(3), 6)
  expect_quick_identical(fn, list(3), list(7))
})

test_that("closure returning logical assigned to return variable", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    check <- function(i) x[i] > 0.5
    out <- sapply(seq_along(x), check)
    out
  }

  x <- c(0.1, 0.6, 0.3, 0.9)
  expect_identical(fn(x), c(FALSE, TRUE, FALSE, TRUE))
  expect_quick_identical(fn, list(x))
})

test_that("sapply over scalar iterable works", {
  fn <- function(x) {
    declare(type(x = double(1)))
    out <- sapply(x, function(v) v * 2)
    out
  }

  expect_identical(fn(3), 6)
  expect_quick_identical(fn, list(3), list(5))
})

test_that("closure captures and shadows host variable on subscript assign", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    modify <- function(i) {
      x[i] <- x[i] + 1
      x[i]
    }
    out <- sapply(seq_along(x), modify)
    out
  }

  x <- c(1, 2, 3)
  expect_identical(fn(x), c(2, 3, 4))
  expect_quick_identical(fn, list(x))
})

test_that("superassignment to non-existent variable errors", {
  expect_error(
    quick(function(x) {
      declare(type(x = double(NA)))
      modify <- function(i) {
        nonexistent[i] <<- 1
      }
      modify(1L)
      x
    }),
    "must resolve to an existing variable"
  )
})

test_that("closure with scalar indexing on scalar value via superassign", {
  fn <- function(x) {
    declare(type(x = double(1)))
    modify <- function() {
      x[1] <<- x[1] + 1
      NULL
    }
    modify()
    x
  }

  expect_identical(fn(5), 6)
  expect_quick_identical(fn, list(5), list(10))
})
