test_that("solve matches R for vector, matrix, and inverse", {
  solve_vec <- function(A, b) {
    declare(
      type(A = double(n, n)),
      type(b = double(n))
    )
    solve(A, b)
  }

  solve_mat <- function(A, B) {
    declare(
      type(A = double(n, n)),
      type(B = double(n, k))
    )
    solve(A, B)
  }

  solve_inv <- function(A) {
    declare(type(A = double(n, n)))
    solve(A)
  }

  set.seed(1)
  n <- 5
  k <- 3
  base <- matrix(rnorm(n * n), n, n)
  A <- crossprod(base) + diag(n)
  b <- rnorm(n)
  B <- matrix(rnorm(n * k), n, k)

  expect_quick_equal(solve_vec, list(A = A, b = b))
  expect_quick_equal(solve_mat, list(A = A, B = B))
  expect_quick_equal(solve_inv, list(A = A))
})

test_that("solve handles column RHS matrices and 1x1 systems", {
  solve_col <- function(A, B) {
    declare(
      type(A = double(n, n)),
      type(B = double(n, 1L))
    )
    solve(A, B)
  }

  solve_scalar <- function(A, b) {
    declare(
      type(A = double(1L, 1L)),
      type(b = double(1L))
    )
    solve(A, b)
  }

  set.seed(10)
  n <- 4
  base <- matrix(rnorm(n * n), n, n)
  A <- crossprod(base) + diag(n)
  B <- matrix(rnorm(n), n, 1L)

  expect_quick_equal(solve_col, list(A = A, B = B))
  expect_quick_equal(solve_scalar, list(A = matrix(2.5, 1L, 1L), b = 1.25))
})

test_that("solve supports least-squares for rectangular systems", {
  solve_ls_vec <- function(X, y) {
    declare(
      type(X = double(n, k)),
      type(y = double(n))
    )
    solve(X, y)
  }

  solve_ls_mat <- function(X, Y) {
    declare(
      type(X = double(n, k)),
      type(Y = double(n, p))
    )
    solve(X, Y)
  }

  set.seed(123)
  n <- 20
  k <- 5
  p <- 3
  X <- matrix(rnorm(n * k), n, k)
  y <- rnorm(n)
  Y <- matrix(rnorm(n * p), n, p)

  q_solve_ls_vec <- expect_warning(quick(solve_ls_vec), NA)
  q_solve_ls_mat <- expect_warning(quick(solve_ls_mat), NA)

  expect_equal(q_solve_ls_vec(X, y), qr.solve(X, y))
  expect_equal(q_solve_ls_mat(X, Y), qr.solve(X, Y))
})

test_that("qr.solve matches R for vectors and matrices", {
  qr_solve_ls_vec <- function(X, y) {
    declare(
      type(X = double(n, k)),
      type(y = double(n))
    )
    qr.solve(X, y)
  }

  qr_solve_ls_mat <- function(X, Y) {
    declare(
      type(X = double(n, k)),
      type(Y = double(n, p))
    )
    qr.solve(X, Y)
  }

  qr_solve_square <- function(A, B) {
    declare(
      type(A = double(n, n)),
      type(B = double(n, k))
    )
    qr.solve(A, B)
  }

  set.seed(124)
  n <- 20
  k <- 5
  p <- 3
  X <- matrix(rnorm(n * k), n, k)
  y <- rnorm(n)
  Y <- matrix(rnorm(n * p), n, p)

  base <- matrix(rnorm(n * n), n, n)
  A <- crossprod(base) + diag(n)
  B <- matrix(rnorm(n * k), n, k)

  q_qr_solve_ls_vec <- expect_warning(quick(qr_solve_ls_vec), NA)
  q_qr_solve_ls_mat <- expect_warning(quick(qr_solve_ls_mat), NA)
  q_qr_solve_square <- expect_warning(quick(qr_solve_square), NA)

  expect_equal(q_qr_solve_ls_vec(X, y), qr.solve(X, y))
  expect_equal(q_qr_solve_ls_mat(X, Y), qr.solve(X, Y))
  expect_equal(q_qr_solve_square(A, B), qr.solve(A, B))
})

test_that("solve uses dgesv for known square and dgels for rectangular systems", {
  solve_square <- function(A, b) {
    declare(
      type(A = double(n, n)),
      type(b = double(n))
    )
    solve(A, b)
  }

  solve_square_fixed <- function(A, b) {
    declare(
      type(A = double(3, 3)),
      type(b = double(3))
    )
    solve(A, b)
  }

  solve_rect <- function(A, b) {
    declare(type(A = double(3, 2)), type(b = double(3)))
    solve(A, b)
  }

  solve_rect_named <- function(A, b) {
    declare(
      type(A = double(n, k)),
      type(b = double(n))
    )
    solve(A, b)
  }

  square_named_fortran <- paste(
    capture.output(cat(r2f(solve_square))),
    collapse = "\n"
  )
  square_fixed_fortran <- paste(
    capture.output(cat(r2f(solve_square_fixed))),
    collapse = "\n"
  )
  rect_fixed_fortran <- paste(
    capture.output(cat(r2f(solve_rect))),
    collapse = "\n"
  )
  rect_named_fortran <- paste(
    capture.output(cat(r2f(solve_rect_named))),
    collapse = "\n"
  )

  expect_match(square_named_fortran, "\\bcall\\s+dgesv\\b", ignore.case = TRUE)
  expect_no_match(
    square_named_fortran,
    "\\bcall\\s+dgels\\b",
    ignore.case = TRUE
  )

  expect_match(square_fixed_fortran, "\\bcall\\s+dgesv\\b", ignore.case = TRUE)
  expect_no_match(
    square_fixed_fortran,
    "\\bcall\\s+dgels\\b",
    ignore.case = TRUE
  )

  expect_match(rect_fixed_fortran, "\\bcall\\s+dgels\\b", ignore.case = TRUE)
  expect_no_match(rect_fixed_fortran, "\\bcall\\s+dgesv\\b", ignore.case = TRUE)

  expect_match(rect_named_fortran, "\\bcall\\s+dgels\\b", ignore.case = TRUE)
  expect_no_match(rect_named_fortran, "\\bcall\\s+dgesv\\b", ignore.case = TRUE)
})

test_that("chol and chol2inv match R", {
  chol_fn <- function(A) {
    declare(type(A = double(n, n)))
    chol(A)
  }

  chol2inv_fn <- function(A) {
    declare(type(A = double(n, n)))
    U <- chol(A)
    chol2inv(U)
  }

  set.seed(2)
  n <- 4
  base <- matrix(rnorm(n * n), n, n)
  A <- crossprod(base) + diag(n)

  expect_quick_equal(chol_fn, list(A = A))
  expect_quick_equal(chol2inv_fn, list(A = A))
})

test_that("chol and chol2inv handle 1x1 matrices", {
  chol_scalar <- function(A) {
    declare(type(A = double(1L, 1L)))
    chol(A)
  }

  chol2inv_scalar <- function(A) {
    declare(type(A = double(1L, 1L)))
    U <- chol(A)
    chol2inv(U)
  }

  expect_quick_equal(chol_scalar, list(A = matrix(3.2, 1L, 1L)))
  expect_quick_equal(chol2inv_scalar, list(A = matrix(3.2, 1L, 1L)))
})

test_that("diag matches R for vectors, matrices, and sizes", {
  diag_vec <- function(x) {
    declare(type(x = double(n)))
    diag(x)
  }

  diag_mat <- function(A) {
    declare(type(A = double(m, n)))
    diag(A)
  }

  diag_size <- function() {
    diag(3L)
  }

  diag_value <- function(x) {
    declare(type(x = double(1)))
    diag(x, nrow = 2, ncol = 3)
  }

  diag_named_order <- function(x) {
    declare(type(x = double(n)))
    diag(nrow = 3L, x = x)
  }

  set.seed(3)
  x <- rnorm(4)
  A <- matrix(rnorm(2 * 3), nrow = 2)

  expect_quick_equal(diag_vec, list(x = x))
  expect_quick_equal(diag_mat, list(A = A))
  expect_quick_equal(diag_size, list())
  expect_quick_equal(diag_value, list(x = 2.5))
  expect_quick_equal(diag_named_order, list(x = c(1, 2)))
})

test_that("diag handles missing x with nrow/ncol and 1x1 matrices", {
  diag_nrow <- function(n) {
    declare(type(n = integer(1)))
    diag(nrow = n)
  }

  diag_ncol <- function(n) {
    declare(type(n = integer(1)))
    diag(ncol = n)
  }

  diag_rowvec <- function(A) {
    declare(type(A = double(1L, n)))
    diag(A)
  }

  expect_quick_equal(diag_nrow, list(n = 3L))
  expect_error(diag_ncol(2L), "nrow")
  expect_error(quick(diag_ncol)(2L), "nrow")
  expect_quick_equal(
    diag_rowvec,
    list(A = matrix(runif(3), nrow = 1L))
  )
})

test_that("Test bad path in lapack functions", {
  solve_bad_rank <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    solve(a, b)
  }

  solve_non_square <- function(a, b) {
    declare(type(a = double(2, 3)), type(b = double(2)))
    solve(a, b)
  }

  solve_bad_rhs <- function(a, b) {
    declare(type(a = double(2, 2)), type(b = double(2, 2, 2)))
    solve(a, b)
  }

  chol_bad_rank <- function(a) {
    declare(type(a = double(2, 2, 2)))
    chol(a)
  }

  chol_pivot <- function(a) {
    declare(type(a = double(2, 2)))
    chol(a, pivot = TRUE)
  }

  chol_linpack <- function(a) {
    declare(type(a = double(2, 2)))
    chol(a, LINPACK = TRUE)
  }

  chol2inv_size <- function(a) {
    declare(type(a = double(2, 2)))
    chol2inv(a, size = 1L)
  }

  chol2inv_bad_rank <- function(a) {
    declare(type(a = double(2)))
    chol2inv(a)
  }

  diag_bad_rank <- function(a) {
    declare(type(a = double(2, 2, 2)))
    diag(a)
  }

  diag_matrix_nrow <- function(a) {
    declare(type(a = double(2, 2)))
    diag(a, nrow = 2L)
  }

  diag_missing <- function() {
    diag()
  }

  expect_error(quick(solve_bad_rank), "expects a matrix")
  expect_warning(quick(solve_non_square), NA)
  expect_error(quick(solve_bad_rhs), "only supports vector or matrix")
  expect_error(quick(chol_bad_rank), "expects a matrix")
  expect_error(quick(chol_pivot), "pivot = TRUE")
  expect_error(quick(chol_linpack), "LINPACK")
  expect_error(quick(chol2inv_size), "does not support size")
  expect_error(quick(chol2inv_bad_rank), "expects a matrix")
  expect_error(
    quick(diag_bad_rank),
    "only supports scalar, vector, or matrix"
  )
  expect_error(quick(diag_matrix_nrow), "cannot be specified")
  expect_error(quick(diag_missing), "argument \"nrow\" is missing")
})

test_that("lapack functions test non-square matrix errors", {
  solve_rect <- function(A, b) {
    declare(type(A = double(3, 2)), type(b = double(3)))
    solve(A, b)
  }

  chol_rect <- function(A) {
    declare(type(A = double(3, 2)))
    chol(A)
  }

  chol2inv_rect <- function(R) {
    declare(type(R = double(3, 2)))
    chol2inv(R)
  }

  expect_warning(quick(solve_rect), NA)
  expect_error(quick(chol_rect), "requires a square matrix")
  expect_error(quick(chol2inv_rect), "requires a square matrix")
})

test_that("solve with scalar RHS is rejected", {
  solve_scalar_rhs <- function(A, b) {
    declare(type(A = double(2, 2)), type(b = double(1)))
    solve(A, b[1])
  }

  expect_error(
    quick(solve_scalar_rhs),
    "non-conformable arguments|expects a vector or matrix"
  )
})

test_that("diag with various nrow/ncol combinations", {
  diag_with_nrow <- function(n) {
    declare(type(n = integer(1)))
    diag(nrow = n)
  }

  diag_with_ncol <- function(n, m) {
    declare(type(n = integer(1)), type(m = integer(1)))
    diag(nrow = n, ncol = m)
  }

  diag_vec_nrow_ncol <- function(x, n, m) {
    declare(
      type(x = double(3)),
      type(n = integer(1)),
      type(m = integer(1))
    )
    diag(x, nrow = n, ncol = m)
  }

  expect_quick_equal(diag_with_nrow, list(n = 3L))
  expect_quick_equal(diag_with_ncol, list(n = 2L, m = 3L))
  expect_quick_equal(diag_vec_nrow_ncol, list(x = c(1, 2, 3), n = 4L, m = 4L))
})

test_that("linear model example matches R", {
  my_lm <- function(X, y) {
    declare(type(X = double(n, k)), type(y = double(n)))
    n <- nrow(X)
    k <- ncol(X)

    XtX <- crossprod(X)
    Xty <- crossprod(X, y)
    coef <- solve(XtX, Xty)

    res <- y - X %*% coef
    ## crossprod(res)[1] was added by codex to silence 1x1 warnings in R
    ##
    s2 <- crossprod(res)[1] / (n - k)

    U <- chol(XtX)
    XtX_inv <- chol2inv(U)

    std_err <- sqrt(diag(XtX_inv) * s2)
    df <- n - k

    list(
      coefficients = coef,
      stderr = std_err,
      df.residual = df
    )
  }

  set.seed(4)
  n <- 30
  k <- 4
  X <- matrix(rnorm(n * k), n, k)
  y <- rnorm(n)

  expect_quick_equal(my_lm, list(X = X, y = y))
})

test_that("crossprod temp can be indexed directly", {
  fn <- function(x) {
    declare(type(x = double(n, k)))
    crossprod(x)[1]
  }

  set.seed(11)
  x <- matrix(rnorm(6), 3, 2)
  expect_quick_equal(fn, list(x = x))
})
