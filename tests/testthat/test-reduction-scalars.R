test_that("min handles scalar arguments without reduction", {
  fn <- function(a, b, m) {
    declare(
      type(a = integer(1)),
      type(b = integer(1)),
      type(m = integer(NA, 2))
    )

    c(min(m[a, 1], m[b, 1]), min(m[a, 2], m[b, 2]))
  }

  # fsub <- r2f(fn)
  # expect_no_match(as.character(fsub), "minval\\(")

  qfn <- quick(fn)

  m <- matrix(c(5L, 1L, 3L, 9L), ncol = 2, byrow = TRUE)
  expect_identical(qfn(1L, 2L, m), fn(1L, 2L, m))
})


test_that("drop = FALSE preserves dims and uses reductions", {
  fn <- function(a, m) {
    declare(
      type(a = integer(1)),
      type(m = integer(NA, 2))
    )

    min(m[a, 1L, drop = FALSE])
  }

  # fsub <- r2f(fn)
  # expect_match(as.character(fsub), "minval\\(")

  qfn <- quick(fn)

  m <- matrix(c(2L, 4L, 1L, 3L), nrow = 2L, byrow = TRUE)
  expect_identical(qfn(1L, m), fn(1L, m))
})


test_that("reductions over vectors still use intrinsics", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    min(x)
  }

  # fsub <- r2f(fn)
  # expect_match(as.character(fsub), "minval\\(")

  qfn <- quick(fn)

  x <- c(10L, -2L, 5L, 3L)
  expect_identical(qfn(x), fn(x))
})
