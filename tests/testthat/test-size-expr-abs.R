# Public API tests for abs() support in declared size expressions

skip_on_cran()

test_that("declare() size expressions support abs()", {
  fn <- function(start, end) {
    declare(
      type(start = integer(1)),
      type(end = integer(1)),
      type(out = double(abs(end - start) + 1L))
    )
    out <- double(abs(end - start) + 1L)
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  qfn <- quick(fn)
  expect_identical(qfn(1L, 1L), 1.0)
  expect_identical(qfn(2L, 5L), as.double(1:4))
  expect_identical(qfn(5L, 2L), as.double(1:4))
})

test_that("declare() size expressions validate abs() arity", {
  bad <- function(n, m) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(out = double(abs(n, m)))
    )
    out <- double(abs(n, m))
    out
  }

  expect_error(quick(bad), "unused argument", fixed = TRUE)
})

test_that("declare() size expressions support as.integer()", {
  # as.integer() reaches size expressions through diag()'s identity form,
  # but it is spellable on its own: INT() in Fortran, a cast in the bridge
  fn <- function(x, n) {
    declare(type(x = double(1)), type(n = integer(1)))
    out <- double(as.integer(x) + n)
    out
  }
  expect_quick_equal(fn, list(2, 3L), list(2.9, 1L))

  bad <- function(x) {
    declare(type(x = double(1)))
    out <- double(as.integer(x, 2L))
    out
  }
  expect_error(quick(bad), "expects one argument", fixed = TRUE)
})
