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
