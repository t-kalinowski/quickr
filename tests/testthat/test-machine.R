test_that("quick() supports .Machine$double.eps", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x + .Machine$double.eps
  }

  expect_translation_snapshots(fn)
  expect_quick_equal(fn, x = 1)
})

test_that("quick() errors for unsupported .Machine$ fields", {
  expect_error(
    quick(function(x) {
      declare(type(x = double(1)))
      x + .Machine$integer.max
    }),
    "`.Machine$integer.max` is not supported",
    fixed = TRUE
  )
})
