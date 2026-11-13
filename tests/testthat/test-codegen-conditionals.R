skip_on_cran()

# Full translations make the selection, allocation, and error paths reviewable.
# Native behavior is covered by test-ifelse.R, test-logical.R,
# test-matrix-lapack.R, and test-openmp-parallelization.R.

test_that("shared-shape ifelse writes one inline selection", {
  fn <- function(test, yes, no) {
    declare(
      type(test = logical(n)),
      type(yes = double(n)),
      type(no = double(n))
    )
    out <- ifelse(test, yes + 1, no)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("matrix ifelse promotes branches without changing result shape", {
  fn <- function(test, yes) {
    declare(type(test = logical(n, m)), type(yes = integer(n, m)))
    ifelse(test, yes, 0.5)
  }
  expect_translation_snapshots(fn)
})

test_that("logical ifelse preserves the C logical storage convention", {
  fn <- function(test, yes) {
    declare(type(test = logical(n)), type(yes = logical(n)))
    ifelse(test, yes, FALSE)
  }
  expect_translation_snapshots(fn)
})

test_that("nested ifelse captures its result before later mutations", {
  fn <- function(test, x) {
    declare(type(test = logical(n)), type(x = double(n)))
    change <- function() {
      test <<- !test
      x <<- x + 10
      0
    }
    ifelse(test, x, -x) + change()
  }
  expect_translation_snapshots(fn)
})

test_that("ifelse computes an effectful selector once", {
  fn <- function() {
    ifelse(runif(3) > 0.5, 1, -1)
  }
  expect_translation_snapshots(fn)
})

test_that("scalar ifelse runs only the selected effectful branch", {
  fn <- function(test, x) {
    declare(type(test = logical(1)), type(x = double(1)))
    ifelse(test, runif(1), x)
  }
  expect_translation_snapshots(fn)
})

test_that("vector ifelse allocates an effectful branch when selected", {
  fn <- function(test, n) {
    declare(type(test = logical(3)), type(n = integer(1)))
    ifelse(test, runif(n), 0)
  }
  expect_translation_snapshots(fn)
})

test_that("ifelse allocates a named BLAS result inside its selected branch", {
  fn <- function(test, x) {
    declare(type(test = logical(k, k)), type(x = double(n, k)))
    ifelse(test, crossprod(x), 0)
  }
  expect_translation_snapshots(fn)
})

test_that("an ordinary fill allocates after a conditional LAPACK write", {
  fn <- function(n, a, b, flag) {
    declare(
      type(n = integer(1)),
      type(a = double(n, n)),
      type(b = double(n)),
      type(flag = logical(1))
    )
    if (flag) {
      z <- solve(a, b)
    }
    z <- numeric(n)
    sum(z)
  }
  expect_translation_snapshots(fn)
})

test_that("initialization before a conditional write retains its allocation", {
  fn <- function(n, a, b, flag) {
    declare(
      type(n = integer(1)),
      type(a = double(n, n)),
      type(b = double(n)),
      type(flag = logical(1))
    )
    z <- numeric(n)
    if (flag) {
      z <- solve(a, b)
    }
    sum(z)
  }
  expect_translation_snapshots(fn)
})

test_that("conditional writes retain storage across loop iterations", {
  fn <- function(n, a, b) {
    declare(
      type(n = integer(1)),
      type(a = double(n, n)),
      type(b = double(n))
    )
    out <- 0
    for (i in 1:3) {
      if (i > 1L) {
        z <- solve(a, b)
      }
      z <- numeric(n)
      out <- out + sum(z)
    }
    out
  }
  expect_translation_snapshots(fn)
})

test_that("lazy closure diagnostics retain effects before the error", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    predicate <- function() {
      draw <- runif(1)
      invalid <- abs()
      later <- runif(2)
      TRUE
    }
    flag && predicate()
  }
  expect_translation_snapshots(fn)
})

test_that("lazy while-condition diagnostics retain their hoisted effects", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    predicate <- function() {
      while ((runif(1) + abs()) > 0) {
        later <- runif(2)
      }
      TRUE
    }
    flag && predicate()
  }
  expect_translation_snapshots(fn)
})

test_that("parallel loops keep generated temporaries inside iteration blocks", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- numeric(length(x))
    declare(parallel())
    for (i in seq_along(x)) {
      out[i] <- sqrt(abs(x[i]))
    }
    out
  }
  expect_translation_snapshots(fn)
})

test_that("parallel sapply propagates a closure error through cancellation", {
  fn <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    out <- sapply(seq_len(1L), function(i) {
      if (x < 0) {
        stop("x must be nonnegative")
      }
      x + 1
    })
    out
  }
  expect_translation_snapshots(fn)
})
