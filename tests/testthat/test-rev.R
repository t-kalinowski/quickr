test_that("rev() reverses vectors", {
  fn_dbl <- function(x) {
    declare(type(x = double(NA)))
    rev(x)
  }
  expect_quick_identical(
    fn_dbl,
    list(as.double(1:5)),
    list(as.double(c(2, -1, 3, 0)))
  )

  fn_int <- function(x) {
    declare(type(x = integer(NA)))
    rev(x)
  }
  expect_quick_identical(fn_int, list(as.integer(c(1, 0, -3, 2))))

  # Logical arguments are passed via the bind(c) interface as integer storage.
  fn_lgl <- function(x) {
    declare(type(x = logical(NA)))
    rev(x)
  }
  expect_quick_identical(fn_lgl, list(c(TRUE, FALSE, TRUE, FALSE)))
  expect_quick_identical(fn_lgl, list(c(TRUE, NA, FALSE, TRUE)))
})

test_that("rev() preserves bind(c) logical scalar storage (incl. NA)", {
  fn <- function(m) {
    declare(type(m = logical(1)))
    rev(m)
  }

  expect_quick_identical(fn, list(TRUE))
  expect_quick_identical(fn, list(FALSE))
  expect_quick_identical(fn, list(NA))
})

test_that("rev(rev()) preserves bind(c) logical storage (incl. NA)", {
  fn <- function(m) {
    declare(type(m = logical(NA)))
    rev(rev(m))
  }

  m <- c(TRUE, FALSE, NA, TRUE, FALSE)
  expect_quick_identical(fn, list(m))
})

test_that("rev() result can flow into logical coercions and reductions", {
  fn_as_integer <- function(m) {
    declare(type(m = logical(NA)))
    as.integer(rev(m))
  }
  fn_which_max <- function(m) {
    declare(type(m = logical(NA)))
    which.max(rev(m))
  }
  fn_which_min <- function(m) {
    declare(type(m = logical(NA)))
    which.min(rev(m))
  }

  m <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
  expect_quick_identical(fn_as_integer, list(m))
  expect_quick_identical(fn_which_max, list(m))
  expect_quick_identical(fn_which_min, list(m))
})

test_that("rev() works inside compound logical expressions", {
  fn <- function(x) {
    declare(type(x = logical(NA)))
    out <- ifelse(rev(x) & x, 1L, 0L)
    out
  }

  x <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  expect_quick_identical(fn, list(x))
})

test_that("rev() works as an external logical mask in x[mask]", {
  fn <- function(x, m) {
    declare({
      type(x = double(NA))
      type(m = logical(NA))
    })
    sum(x[rev(m)])
  }

  x <- as.double(seq_len(10))
  m <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
  expect_quick_equal(fn, list(x, m))
})

test_that("rev() mask assignment preserves bind(c) storage through local symbols", {
  fn <- function(x, m) {
    declare({
      type(x = double(NA))
      type(m = logical(NA))
    })
    r <- rev(m)
    sum(x[r])
  }

  x <- as.double(seq_len(12))
  m <- c(
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    TRUE,
    TRUE
  )
  expect_quick_equal(fn, list(x, m))
})

test_that("rev() works on local Fortran logical vectors and masks", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    m <- x > 0
    sum(x[rev(m)])
  }

  x <- as.double(c(-2, -1, 0, 1, 2, 3, -4, 5))
  expect_quick_equal(fn, list(x))
})

test_that("rev() can be used as a reduction mask (sum(x[mask]))", {
  fn <- function(x, m) {
    declare({
      type(x = double(NA))
      type(m = logical(NA))
    })
    sum(x[rev(m)])
  }

  x <- as.double(seq_len(20))
  m <- rep(c(TRUE, FALSE, TRUE, TRUE), length.out = length(x))
  expect_quick_equal(fn, list(x, m))
})

test_that("rev() hoists array expressions", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    rev(x + 1)
  }
  expect_quick_identical(fn, list(as.double(1:6)))
})

test_that("rev() is a no-op for scalars", {
  fn <- function(x) {
    declare(type(x = double(1)))
    rev(x)
  }
  expect_quick_identical(fn, list(as.double(3)))
})
