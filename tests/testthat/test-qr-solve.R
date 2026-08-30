skip_on_cran()

test_that("qr.solve translation uses linpack path", {
  qr_solve_vec <- function(a, b) {
    declare(
      type(a = double(n, k)),
      type(b = double(n))
    )
    qr.solve(a, b)
  }

  qr_solve_mat <- function(a, b) {
    declare(
      type(a = double(n, k)),
      type(b = double(n, m))
    )
    qr.solve(a, b)
  }

  expect_translation_snapshots(
    qr_solve_vec,
    note = "qr.solve uses dqrdc2/dqrcf (vector rhs)"
  )
  expect_translation_snapshots(
    qr_solve_mat,
    note = "qr.solve uses dqrdc2/dqrcf (matrix rhs)"
  )
})

test_that("qr.solve quick matches base R", {
  qr_solve_vec <- function(a, b) {
    declare(
      type(a = double(n, k)),
      type(b = double(n))
    )
    qr.solve(a, b)
  }

  qr_solve_mat <- function(a, b) {
    declare(
      type(a = double(n, k)),
      type(b = double(n, m))
    )
    qr.solve(a, b)
  }

  set.seed(123)
  a <- matrix(rnorm(40), nrow = 10)
  b <- rnorm(10)
  expect_quick_equal(qr_solve_vec, list(a, b))

  b_mat <- matrix(rnorm(20), nrow = 10)
  expect_quick_equal(qr_solve_mat, list(a, b_mat))

  a_wide <- matrix(rnorm(30), nrow = 5)
  b_wide <- rnorm(5)
  expect_quick_equal(qr_solve_vec, list(a_wide, b_wide))
})

test_that("qr.solve rejects a known zero-width matrix right-hand side", {
  fn <- function(a, b) {
    declare(type(a = double(3, 2)), type(b = double(3, 0)))
    qr.solve(a, b)
  }

  expect_error(
    quick(fn),
    "qr.solve zero-sized outputs are not supported",
    fixed = TRUE
  )
})

test_that("qr.solve rejects a dynamic zero-width matrix right-hand side", {
  fn <- function(a, b) {
    declare(type(a = double(3, 2)), type(b = double(3, NA)))
    qr.solve(a, b)
  }
  a <- rbind(c(1, 0), c(0, 1), c(1, 1))
  b <- matrix(double(), 3, 0)

  expect_error(
    quick(fn)(a, b),
    "qr.solve zero-sized outputs are not supported",
    fixed = TRUE
  )
})

test_that("qr.solve rejects zero-width output before rank deficiency", {
  fn <- function(a, b) {
    declare(type(a = double(3, 2)), type(b = double(3, NA)))
    qr.solve(a, b)
  }
  a <- cbind(as.double(1:3), as.double(1:3))
  b <- matrix(double(), 3, 0)
  qfn <- expect_no_warning(quick(fn))

  expect_error(
    qfn(a, b),
    "qr.solve zero-sized outputs are not supported",
    fixed = TRUE
  )
})

test_that("qr.solve rejects zero coefficient and output extents", {
  coefficient_message <- "qr.solve coefficient matrices with zero extents are not supported"
  output_message <- "qr.solve zero-sized outputs are not supported"
  known_zero_rows <- function(a, b) {
    declare(type(a = double(0, 2)), type(b = double(0, 1)))
    qr.solve(a, b)
  }
  known_zero_cols <- function(a, b) {
    declare(type(a = double(2, 0)), type(b = double(2, 1)))
    qr.solve(a, b)
  }
  dynamic <- function(a, b) {
    declare(type(a = double(NA, NA)), type(b = double(NA, NA)))
    qr.solve(a, b)
  }

  expect_error(quick(known_zero_rows), coefficient_message, fixed = TRUE)
  expect_error(quick(known_zero_cols), output_message, fixed = TRUE)

  code <- r2f(dynamic)
  expect_match(as.character(code), coefficient_message, fixed = TRUE)
  expect_match(as.character(code), output_message, fixed = TRUE)
  qfn <- expect_no_warning(quick(dynamic))
  expect_error(
    qfn(matrix(double(), 0, 2), matrix(double(), 0, 1)),
    coefficient_message,
    fixed = TRUE
  )
  expect_error(
    qfn(matrix(double(), 2, 0), matrix(double(), 2, 1)),
    output_message,
    fixed = TRUE
  )
})
