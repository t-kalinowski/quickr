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

  set.seed(1)
  x <- runif(20)
  expect_quick_identical(fn, list(x, 0.5))
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

  set.seed(1)
  x <- runif(15)
  expect_quick_identical(fn, list(x, 0.2, 1.0))
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

  expect_quick_identical(fn, list(1L), list(2L), list(7L))
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

  set.seed(1)
  x <- runif(10)
  expect_quick_identical(fn, list(x))
})
