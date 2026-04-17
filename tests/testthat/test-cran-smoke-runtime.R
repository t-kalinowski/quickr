# Compact end-to-end coverage kept on CRAN.

test_that("CRAN smoke: core language and matrix paths compile and run", {
  fn <- function(
    vals,
    mask_mat,
    lhs,
    rhs_mat,
    chol_mat,
    solve_rhs,
    design,
    response
  ) {
    declare(
      type(vals = double(NA)),
      type(mask_mat = double(NA, NA)),
      type(lhs = double(2, 3)),
      type(rhs_mat = double(3, 2)),
      type(chol_mat = double(n, n)),
      type(solve_rhs = double(n)),
      type(design = double(p, k)),
      type(response = double(p))
    )

    bump <- function() {
      vals <<- vals + 1.0
      NULL
    }
    bump()

    shifted <- sapply(seq_along(vals), function(i) sin(vals[i]))
    reversed <- rev(shifted)
    filtered <- reversed[reversed > 0]

    lang_total <- 0.0
    for (elt in filtered) {
      lang_total <- lang_total + elt
    }

    matrix_total <- sum(mask_mat[mask_mat > 3.0])
    matrix_total <- matrix_total + sum(lhs %*% rhs_mat)
    matrix_total <- matrix_total + sum(solve(chol_mat, solve_rhs))
    matrix_total <- matrix_total + sum(qr.solve(design, response))
    matrix_total <- matrix_total + sum(chol(chol_mat))

    lang_total + matrix_total
  }

  vals <- c(-2, -1, 0, 1, 2, 3)
  mask_mat <- matrix(as.double(1:6), nrow = 2L, ncol = 3L, byrow = TRUE)

  set.seed(2)
  lhs <- matrix(rnorm(6), nrow = 2L)
  rhs_mat <- matrix(rnorm(6), nrow = 3L)
  n <- 4L
  k <- 2L
  base <- matrix(rnorm(n * n), nrow = n)
  chol_mat <- crossprod(base) + diag(n)
  solve_rhs <- rnorm(n)
  design <- matrix(rnorm(6 * k), nrow = 6L)
  response <- rnorm(6)

  q_fn <- expect_no_warning(quick(fn))
  expected <- fn(
    vals,
    mask_mat,
    lhs,
    rhs_mat,
    chol_mat,
    solve_rhs,
    design,
    response
  )
  actual <- q_fn(
    vals,
    mask_mat,
    lhs,
    rhs_mat,
    chol_mat,
    solve_rhs,
    design,
    response
  )

  expect_equal(actual, expected)
  expect_identical(typeof(actual), typeof(expected))
})
