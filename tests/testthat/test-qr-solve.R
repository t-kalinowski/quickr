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
