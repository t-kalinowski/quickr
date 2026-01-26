# Unit tests for C bridge hoisting of size expressions

test_that("size check blocks redeclare hoisted size temps", {
  fn <- function(n, m, a, b) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(a = double(min(n, m))),
      type(b = double(min(n, m))),
      type(out = double(min(n, m)))
    )
    out <- double(min(n, m))
    for (i in seq_len(length(out))) {
      out[i] <- a[i] + b[i]
    }
    out
  }

  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(3L, 5L, c(1, 2, 3), c(10, 20, 30)),
    list(5L, 3L, c(1, 2, 3), c(4, 5, 6))
  )
})
