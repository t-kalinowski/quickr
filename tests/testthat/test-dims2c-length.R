test_that("declare dims support nested length() (vector)", {
  fn <- function(x) {
    declare(type(x = double(NA)), type(out = double(length(x) + 1)))
    out[1] <- 0
    out
  }

  expect_translation_snapshots(
    fn,
    note = "dims2c must translate length(x) inside arithmetic, not evaluate base::length() on the C expression string."
  )
})

test_that("declare dims support nested length() (rank>1)", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)), type(out = double(length(x) + 1)))
    out[1] <- 0
    out
  }

  expect_translation_snapshots(
    fn,
    note = "dims2c must translate length(x) for rank>1 arrays (length == prod(dim))."
  )
})

test_that("declare dims support min() in size expressions", {
  fn <- function(n, m) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(out = double(min(n, m)))
    )
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)
  expect_identical(qfn(2L, 5L), as.double(1:2))
  expect_identical(qfn(4L, 3L), as.double(1:3))
})

test_that("declare dims support max() in size expressions", {
  fn <- function(n, m) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(out = double(max(n, m)))
    )
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)
  expect_identical(qfn(2L, 5L), as.double(1:5))
  expect_identical(qfn(4L, 3L), as.double(1:4))
})
