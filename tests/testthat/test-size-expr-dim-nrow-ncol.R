# Public API tests for nrow()/ncol()/dim() size expression lowering in the C bridge

test_that("declare() size expressions support nrow() and ncol()", {
  fn <- function(x) {
    declare(
      type(x = double(NA, NA)),
      type(out = double(nrow(x) + ncol(x)))
    )
    out <- double(nrow(x) + ncol(x))
    out[1] <- 1
    out
  }

  set.seed(1)
  x <- matrix(runif(6), nrow = 2, ncol = 3)
  qfn <- quick(fn)
  out <- qfn(x)
  expect_identical(length(out), nrow(x) + ncol(x))
  expect_identical(out[[1L]], 1.0)
})

test_that("declare() size expressions support dim(x)[axis] indices", {
  fn <- function(x) {
    declare(
      type(x = double(NA, NA)),
      type(out = double(dim(x)[1L] + dim(x)[2L]))
    )
    out <- double(dim(x)[1L] + dim(x)[2L])
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  set.seed(2)
  x <- matrix(runif(12), nrow = 3, ncol = 4)
  expect_quick_identical(fn, list(x))
})

test_that("declare() size expressions validate nrow()/ncol() arity", {
  bad_nrow <- function(x) {
    declare(type(x = double(NA, NA)), type(out = double(nrow(x, 1L))))
    out <- double(1L)
    out
  }
  bad_ncol <- function(x) {
    declare(type(x = double(NA, NA)), type(out = double(ncol(x, 1L))))
    out <- double(1L)
    out
  }

  expect_error(quick(bad_nrow), "unused argument", fixed = TRUE)
  expect_error(quick(bad_ncol), "unused argument", fixed = TRUE)
})

test_that("declare() size expressions reject unsupported calls", {
  bad <- function(n) {
    declare(type(n = integer(1)), type(out = double(sum(n))))
    out <- double(1L)
    out
  }

  expect_error(quick(bad), "could not find function \"sum\"", fixed = TRUE)
})

test_that("declare() size expressions require dim() to reference a symbol", {
  bad <- function(x) {
    declare(type(x = double(dim(1)[1L])))
    sum(x)
  }

  expect_error(
    quick(bad),
    "dim\\(\\) size expressions must refer to a symbol",
    fixed = FALSE
  )
})
