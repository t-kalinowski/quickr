test_that("sapply lowers scalar-return closures (named + inline)", {
  fn_named <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    f <- function(i) x[i] * 2
    out <- sapply(seq_along(out), f)
    out
  }

  fn_inline <- function(x, thresh) {
    declare(type(x = double(NA)), type(thresh = double(1)))
    out <- logical(length(x))
    out <- sapply(seq_along(out), function(i) x[i] > thresh)
    out
  }

  expect_translation_snapshots(
    fn_named,
    note = "Named local closure lowered to an internal subroutine using host association for captures (no capture arguments)."
  )
  expect_translation_snapshots(
    fn_inline,
    note = "Inline closure lowered to an internal subroutine using host association for captures (no capture arguments)."
  )

  set.seed(1)
  x <- runif(25)
  expect_quick_identical(fn_named, list(x))
  expect_quick_identical(fn_inline, list(x, 0.5))
})

test_that("sapply supports integer scalar return", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- integer(length(x))
    out <- sapply(seq_along(out), function(i) i * 2L)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Integer scalar return produces an integer vector."
  )

  set.seed(1)
  x <- runif(10)
  expect_quick_identical(fn, list(x))
})

test_that("sapply supports vector return -> matrix output", {
  fn <- function(x) {
    declare(type(x = double(m, n)))
    out <- matrix(0, nrow(x), ncol(x))
    out <- sapply(seq_len(ncol(x)), function(j) x[, j] * 2.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Vector return from a closure simplifies to a rank-2 output."
  )

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  expect_quick_identical(fn, list(x))
})

test_that("sapply supports logical vector return -> logical matrix output", {
  fn <- function(x, thresh) {
    declare(type(x = double(m, n)), type(thresh = double(1)))
    out <- matrix(FALSE, nrow(x), ncol(x))
    out <- sapply(seq_len(ncol(x)), function(j) x[, j] > thresh)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Logical vector return simplifies to a logical matrix output."
  )

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  expect_quick_identical(fn, list(x, 0.5))
})

test_that("sapply supports matrix return -> rank-3 output", {
  fn <- function(x, k) {
    declare(type(x = double(m, n)), type(k = integer(1)))
    out <- array(0, dim = c(nrow(x), ncol(x), k))
    out <- sapply(seq_len(k), function(t) x + as.double(t), simplify = "array")
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Matrix return from a closure simplifies to a rank-3 output."
  )

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  expect_quick_identical(fn, list(x, 3L))
})

test_that("sapply supports slice returns for higher-rank captures", {
  fn_rank3 <- function(x) {
    declare(type(x = double(a, b, k)))
    out <- array(0, dim = dim(x))
    out <- sapply(
      seq_len(dim(x)[3]),
      function(t) x[,, t] + 1.0,
      simplify = "array"
    )
    out
  }

  fn_rank4 <- function(x) {
    declare(type(x = double(a, b, c, k)))
    out <- array(0, dim = dim(x))
    out <- sapply(
      seq_len(dim(x)[4]),
      function(t) x[,,, t] * 2.0,
      simplify = "array"
    )
    out
  }

  expect_translation_snapshots(
    fn_rank3,
    note = "Matrix slice x[,,t] returned into a rank-3 result."
  )
  expect_translation_snapshots(
    fn_rank4,
    note = "Rank-3 slice x[,,,t] returned into a rank-4 result."
  )

  set.seed(1)
  x3 <- array(runif(24), dim = c(2, 3, 4))
  x4 <- array(runif(48), dim = c(2, 3, 2, 4))
  expect_quick_identical(fn_rank3, list(x3))
  expect_quick_identical(fn_rank4, list(x4))
})

test_that("sapply supports rank-3 return -> rank-4 output", {
  fn <- function(x, k) {
    declare(type(x = double(a, b, c)), type(k = integer(1)))
    out <- array(0, dim = c(dim(x), k))
    out <- sapply(seq_len(k), function(t) x + as.double(t), simplify = "array")
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Rank-3 return from a closure simplifies to a rank-4 output."
  )

  set.seed(1)
  x <- array(runif(24), dim = c(2, 3, 4))
  expect_quick_identical(fn, list(x, 2L))
})

test_that("closures can capture and index rank>1 arrays with x[i] linear indexing", {
  fn <- function(x) {
    declare(type(x = double(3, 4)))
    out <- double(length(x))
    out <- sapply(seq_along(out), function(i) x[i] + 1.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Linear indexing x[i] on a rank-2 capture uses computed subscripts."
  )

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  expect_quick_identical(fn, list(x))
})

test_that("sapply can write into an existing argument variable", {
  fn <- function(out, x) {
    declare(type(out = double(n)), type(x = double(n)))
    out <- sapply(seq_along(out), function(i) x[i] + 1.0)
    out
  }

  set.seed(1)
  x <- runif(10)
  out <- rep(0, length(x))
  expect_quick_identical(fn, list(out, x))
})

test_that("sapply preserves R semantics when output is also captured", {
  fn <- function(x) {
    declare(type(x = double(m, n)))
    out <- matrix(0, nrow(x), ncol(x))
    out <- x
    out <- sapply(seq_len(ncol(out)), function(j) out[, j] + 1.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "When the output is captured by the closure, quickr uses a hidden temp to avoid in-place updates during the loop."
  )

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  expect_quick_identical(fn, list(x))
})

test_that("closures may assign to captured variables (shadowing the host)", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    f <- function(i) {
      x[i] <- x[i] + 1.0
      x[i]
    }
    out <- sapply(seq_along(out), f)
    out
  }

  qfn <- quick(fn)
  x0 <- as.double(1:10)
  x <- x0
  out <- qfn(x)

  expect_identical(out, x0 + 1.0)
  expect_identical(out, fn(x0))
  expect_identical(x, x0)
})

test_that("shadowed captures are reinitialized for each sapply() iteration", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    out <- sapply(seq_along(out), function(i) {
      x <- x + as.double(i)
      sum(x)
    })
    out
  }

  set.seed(1)
  x <- runif(6)
  expect_quick_identical(fn, list(x))
})

test_that("local closures can be called directly with multiple arguments", {
  fn <- function(nx, ny, k, dt, steps) {
    declare(
      type(nx = integer(1)),
      type(ny = integer(1)),
      type(k = double(1)),
      type(dt = double(1)),
      type(steps = integer(1))
    )

    temp <- matrix(0, nx, ny)
    temp[nx / 2, ny / 2] <- 100

    apply_boundary_conditions <- function(temp) {
      temp[1, ] <- 0
      temp[nx, ] <- 0
      temp[, 1] <- 0
      temp[, ny] <- 0
      temp
    }

    update_temperature <- function(temp, k, dt) {
      temp_new <- temp
      for (i in 2:(nx - 1)) {
        for (j in 2:(ny - 1)) {
          temp_new[i, j] <- temp[i, j] + k * dt
        }
      }
      temp_new
    }

    for (step in seq_len(steps)) {
      temp <- apply_boundary_conditions(temp)
      temp <- update_temperature(temp, k, dt)
    }

    temp
  }

  set.seed(1)
  expect_quick_identical(fn, list(10L, 8L, 0.1, 0.01, 3L))
})
