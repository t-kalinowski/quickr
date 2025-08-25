# Unit tests for logical operations

test_that("between", {
  between <- function(x, left, right) {
    declare({
      type(x = double(n))
      type(left = double(1))
      type(right = double(1))
    })
    out <- x >= left & x <= right
    out
  }

  expect_translation_snapshots(between)
  expect_quick_identical(between, list(x = runif(100), left = .4, right = .6))
})

test_that("logical ops", {
  test_args <- list(
    list(1, 2),
    list(2, 1),
    list(-2, 2),
    list(-2, -2),
    list(3, 3),
    list(4, 1),
    list(1, 4)
  )

  fn <- function(a, b) {
    declare(
      type(a = double(1)),
      type(b = double(1))
    )

    delta <- a - b
    if (delta < 0) {
      delta <- (-1) * delta
    }

    a_gt_b <- a > b
    b_gt_a <- b > a
    delta_lt_3 <- delta <= 3

    out <- (a_gt_b || b_gt_a) && delta_lt_3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # simpler version of above
  fn <- function(a, b) {
    declare({
      type(a = double(1))
      type(b = double(1))
    })

    delta <- abs(a - b)
    out <- (a != b) & (delta <= 3)
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # even simpler version
  fn <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    out <- (a != b) && abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # vectorized version
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    out <- (a != b) & abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  .[a, b] <- .mapply(c, test_args, NULL)
  expect_quick_identical(fn, list(a, b))
})
