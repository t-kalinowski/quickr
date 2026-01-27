# Public API regression tests for dotted argument names in size expressions

test_that("dotted argument names work in size expressions", {
  fn <- function(foo.bar) {
    declare(type(foo.bar = integer(1)))

    out <- double(foo.bar)
    for (i in seq_len(foo.bar)) {
      out[i] <- as.double(i)
    }
    out
  }

  qfn <- quick(fn)
  expect_identical(qfn(1L), 1.0)
  expect_identical(qfn(5L), as.double(1:5))
})
