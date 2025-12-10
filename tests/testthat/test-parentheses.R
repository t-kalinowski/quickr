# Unit tests for parentheses preservation

test_that("parentheses preserve arithmetic precedence for scalars", {
  fn <- function(a, b, c, d) {
    declare(
      type(a = double(1)),
      type(b = double(1)),
      type(c = double(1)),
      type(d = double(1))
    )

    (a + b) / (c + d)
  }

  cases <- list(
    list(1, 2, 3, 4),
    list(5.5, -3, 2, 1),
    list(-10, 4, 7, 5)
  )

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!cases)
})

test_that("parentheses preserve precedence in vectorized expressions", {
  fn <- function(x, y) {
    declare(type(x = double(n)), type(y = double(n)))
    (x + 1) * (y - 1)
  }

  cases <- list(
    list(c(1, 2, 3), c(3, 2, 1)),
    list(c(-1, 0, 1), c(1, 1, 1))
  )

  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!cases)
})
