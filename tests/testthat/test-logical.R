# Unit tests for logical operations

skip_on_cran()

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

test_that("parentheses preserve logical precedence", {
  fn_a <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    cond <- (x > 8L || x <= 0L) && (y > 8L || y <= 0L)
    cond
  }

  fn_b <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    cond_x <- x > 8L || x <= 0L
    cond_y <- y > 8L || y <= 0L
    cond_x && cond_y
  }

  cases <- list(
    list(9L, 1L),
    list(9L, 9L),
    list(0L, 9L),
    list(1L, 0L),
    list(5L, 5L)
  )

  expect_translation_snapshots(fn_a)
  expect_translation_snapshots(fn_b)
  expect_quick_identical(fn_a, !!!cases)
  expect_quick_identical(fn_b, !!!cases)
})

test_that("&& and || require length-1 operands, like R", {
  vec_and <- function(x, y) {
    declare(type(x = logical(3)), type(y = logical(3)))
    x && y
  }
  expect_error(quick(vec_and), "length-1 operands")

  vec_or <- function(x, y) {
    declare(type(x = logical(n)), type(y = logical(n)))
    x || y
  }
  expect_error(quick(vec_or), "length-1 operands")

  numeric_and <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    a && b
  }
  expect_error(quick(numeric_and), "logical operands")
})

test_that("&& and || short-circuit like R's scalar operators", {
  # The right operand indexes past the end of x whenever the left side
  # already decides; R never evaluates it.
  guarded_index <- function(i, x) {
    declare(type(i = integer(1)), type(x = double(3)))
    out <- 0
    if (i <= 3L && x[i] > 0) {
      out <- 1
    }
    out
  }
  expect_translation_snapshots(guarded_index)
  expect_quick_identical(guarded_index, list(5L, c(1, 2, 3)))
  expect_quick_identical(guarded_index, list(2L, c(1, 2, 3)))
  expect_quick_identical(guarded_index, list(2L, c(1, -2, 3)))

  or_guarded <- function(a, x, i) {
    declare(type(a = logical(1)), type(x = double(2)), type(i = integer(1)))
    out <- a || x[i] > 0
    out
  }
  expect_quick_identical(or_guarded, list(TRUE, c(1, 2), 9L))
  expect_quick_identical(or_guarded, list(FALSE, c(-1, 2), 2L))
})

test_that("while re-evaluates hoisted condition code every iteration", {
  # The canonical scan idiom: the && lowering hoists statements, which
  # must re-run per iteration, not once before the loop.
  scan_positive <- function(x) {
    declare(type(x = double(n)))
    i <- 1L
    n <- length(x)
    while (i <= n && x[i] > 0) {
      i <- i + 1L
    }
    i
  }
  expect_translation_snapshots(scan_positive)
  expect_quick_identical(scan_positive, list(c(1, 2, -1, 5)))
  expect_quick_identical(scan_positive, list(c(1, 2, 3)))
  expect_quick_identical(scan_positive, list(c(-1, 2)))
})
