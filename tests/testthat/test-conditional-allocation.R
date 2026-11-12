test_that("ordinary assignment allocates after a skipped solve branch", {
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
  expect_quick_identical(
    fn,
    list(3L, diag(3), c(1, 2, 3), FALSE),
    list(3L, diag(3), c(1, 2, 3), TRUE)
  )
})

test_that("ordinary array copies work after a skipped matrix branch", {
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
    z <- b + 1
    sum(z)
  }
  expect_quick_equal(
    fn,
    list(3L, diag(3), c(1, 2, 3), FALSE),
    list(3L, diag(3), c(1, 2, 3), TRUE)
  )
})

test_that("conditional writes preserve storage across loop iterations", {
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
  expect_quick_identical(fn, list(3L, diag(3), c(1, 2, 3)))
})

test_that("initialization before a conditional matrix write stays allocated", {
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
  expect_quick_equal(
    fn,
    list(3L, diag(3), c(1, 2, 3), FALSE),
    list(3L, diag(3), c(1, 2, 3), TRUE)
  )
})

test_that("closure and sapply writers retain the local's allocation", {
  closure_fn <- function(n, a, b, flag) {
    declare(
      type(n = integer(1)),
      type(a = double(n, n)),
      type(b = double(n)),
      type(flag = logical(1))
    )
    keep <- function(x) x
    if (flag) {
      z <- solve(a, b)
    }
    z <- keep(b)
    sum(z)
  }
  sapply_fn <- function(n, a, b, flag) {
    declare(
      type(n = integer(1)),
      type(a = double(n, n)),
      type(b = double(n)),
      type(flag = logical(1))
    )
    if (flag) {
      z <- solve(a, b)
    }
    z <- sapply(seq_len(n), function(i) b[i])
    sum(z)
  }
  for (fn in list(closure_fn, sapply_fn)) {
    expect_quick_equal(
      fn,
      list(3L, diag(3), c(1, 2, 3), FALSE),
      list(3L, diag(3), c(1, 2, 3), TRUE)
    )
  }
})
