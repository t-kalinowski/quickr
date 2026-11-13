test_that("matrix multiplication matches R for common shapes", {
  mat_mat <- function(mat_A, mat_B) {
    declare(
      type(mat_A = double(4, 3)),
      type(mat_B = double(3, 5))
    )
    mat_A %*% mat_B
  }

  mat_mat_square <- function(mat_A, mat_B) {
    declare(
      type(mat_A = double(3, 3)),
      type(mat_B = double(3, 3))
    )
    mat_A %*% mat_B
  }

  vec_mat <- function(vec, mat_A) {
    declare(
      type(vec = double(3)),
      type(mat_A = double(3, 4))
    )
    vec %*% mat_A
  }

  mat_vec <- function(mat_A, vec) {
    declare(
      type(mat_A = double(3, 4)),
      type(vec = double(4))
    )
    mat_A %*% vec
  }

  vec_vec <- function(vec_A, vec_B) {
    declare(
      type(vec_A = double(3)),
      type(vec_B = double(3))
    )
    vec_A %*% vec_B
  }

  set.seed(1)
  mat_A <- matrix(rnorm(4 * 3), nrow = 4)
  mat_B <- matrix(rnorm(3 * 5), nrow = 3)
  mat_sq_A <- matrix(rnorm(3 * 3), nrow = 3)
  mat_sq_B <- matrix(rnorm(3 * 3), nrow = 3)
  vec_3 <- rnorm(3)
  mat_3x4 <- matrix(rnorm(3 * 4), nrow = 3)
  vec_4 <- rnorm(4)

  expect_quick_equal(mat_mat, list(mat_A = mat_A, mat_B = mat_B))
  expect_quick_equal(
    mat_mat_square,
    list(mat_A = mat_sq_A, mat_B = mat_sq_B)
  )
  expect_quick_equal(vec_mat, list(vec = vec_3, mat_A = mat_3x4))
  expect_quick_equal(mat_vec, list(mat_A = mat_3x4, vec = vec_4))
  expect_quick_equal(vec_vec, list(vec_A = vec_3, vec_B = vec_3))
})

test_that("matrix multiplication handles transposed operands", {
  matmul_t_left <- function(x, y) {
    declare(
      type(x = double(4, 3)),
      type(y = double(4, 5))
    )
    t(x) %*% y
  }

  matmul_t_right <- function(x, y) {
    declare(
      type(x = double(4, 3)),
      type(y = double(5, 3))
    )
    x %*% t(y)
  }

  matmul_t_both <- function(x, y) {
    declare(
      type(x = double(4, 3)),
      type(y = double(5, 4))
    )
    t(x) %*% t(y)
  }

  set.seed(4)
  x <- matrix(rnorm(4 * 3), nrow = 4)
  y_left <- matrix(rnorm(4 * 5), nrow = 4)
  y_right <- matrix(rnorm(5 * 3), nrow = 5)
  y_both <- matrix(rnorm(5 * 4), nrow = 5)

  expect_quick_equal(matmul_t_left, list(x = x, y = y_left))
  expect_quick_equal(matmul_t_right, list(x = x, y = y_right))
  expect_quick_equal(matmul_t_both, list(x = x, y = y_both))
})

test_that("matrix multiplication transposes singleton concatenations", {
  for (expr in list(
    quote(t(c(2L)) %*% 3),
    quote(2 %*% t(c(3))),
    quote(t(c(TRUE)) %*% 3)
  )) {
    fn <- as.function(list(call("{", expr)))
    expect_quick_identical(fn, list())
  }
})

test_that("matrix multiplication handles chained mixes", {
  chain_mix <- function(a, b, c) {
    declare(
      type(a = double(4, 3)),
      type(b = double(5, 3)),
      type(c = double(5, 5))
    )
    (a %*% t(b)) %*% c + 0.25 * (a %*% t(b))
  }

  set.seed(7)
  a <- matrix(rnorm(4 * 3), nrow = 4)
  b <- matrix(rnorm(5 * 3), nrow = 5)
  c <- matrix(rnorm(5 * 5), nrow = 5)

  expect_quick_equal(chain_mix, list(a = a, b = b, c = c))
})

test_that("matrix multiplication handles 1x1 and 1xN/Nx1 shapes", {
  mat_mat_1x1 <- function(a, b) {
    declare(
      type(a = double(1, 1)),
      type(b = double(1, 1))
    )
    a %*% b
  }

  mat_row_col <- function(row, col) {
    declare(
      type(row = double(1, 4)),
      type(col = double(4, 1))
    )
    row %*% col
  }

  mat_col_row <- function(col, row) {
    declare(
      type(col = double(4, 1)),
      type(row = double(1, 4))
    )
    col %*% row
  }

  set.seed(5)
  a <- matrix(rnorm(1), nrow = 1)
  b <- matrix(rnorm(1), nrow = 1)
  row <- matrix(rnorm(4), nrow = 1)
  col <- matrix(rnorm(4), nrow = 4)

  expect_quick_equal(mat_mat_1x1, list(a = a, b = b))
  expect_quick_equal(mat_row_col, list(row = row, col = col))
  expect_quick_equal(mat_col_row, list(col = col, row = row))
})

test_that("matrix multiplication handles t(vec) orientation", {
  tvec_mat <- function(vec, mat_A) {
    declare(
      type(vec = double(3)),
      type(mat_A = double(3, 4))
    )
    t(vec) %*% mat_A
  }

  mat_tvec <- function(mat_A, vec) {
    declare(
      type(mat_A = double(4, 1)),
      type(vec = double(4))
    )
    mat_A %*% t(vec)
  }

  set.seed(6)
  vec <- rnorm(3)
  mat_A <- matrix(rnorm(3 * 4), nrow = 3)
  vec_long <- rnorm(4)
  mat_B <- matrix(rnorm(4), nrow = 4)

  expect_quick_equal(tvec_mat, list(vec = vec, mat_A = mat_A))
  expect_quick_equal(mat_tvec, list(mat_A = mat_B, vec = vec_long))
})

test_that("matrix multiplication handles transposed matrix in vector cases", {
  vec_tmat <- function(vec, mat_A) {
    declare(
      type(vec = double(3)),
      type(mat_A = double(4, 3))
    )
    vec %*% t(mat_A)
  }

  tmat_vec <- function(mat_A, vec) {
    declare(
      type(mat_A = double(4, 3)),
      type(vec = double(4))
    )
    t(mat_A) %*% vec
  }

  set.seed(15)
  vec_3 <- rnorm(3)
  mat_4x3 <- matrix(rnorm(12), nrow = 4)
  vec_4 <- rnorm(4)

  expect_quick_equal(vec_tmat, list(vec = vec_3, mat_A = mat_4x3))
  expect_quick_equal(tmat_vec, list(mat_A = mat_4x3, vec = vec_4))
})

test_that("matrix multiplication errors on non-conformable arguments", {
  matmul_bad <- function(x, y) {
    declare(
      type(x = double(2, 3)),
      type(y = double(2, 4))
    )
    x %*% y
  }

  x <- matrix(rnorm(2 * 3), nrow = 2)
  y <- matrix(rnorm(2 * 4), nrow = 2)

  expect_error(matmul_bad(x, y), "non-conformable")
  expect_error(quick(matmul_bad), "non-conformable arguments in %*%")
})

test_that("matrix multiplication rejects incompatible destinations", {
  dest_mismatch <- function() {
    declare(type(x = double(2)))
    a <- matrix(1.5, 2L, 2L)
    x <- a %*% a
    x
  }

  # The destination's declared rank no longer matches the result; the
  # reassignment shape check rejects it (routing through a temp first).
  expect_error(
    quick(dest_mismatch),
    "replacement rank (2) differs",
    fixed = TRUE
  )
})

test_that("BLAS matrix ops coerce integer and logical inputs to double", {
  matmul_int <- function(A, B) {
    declare(type(A = integer(2, 2)), type(B = integer(2, 2)))
    A %*% B
  }

  crossprod_int <- function(x) {
    declare(type(x = integer(3, 2)))
    crossprod(x)
  }

  outer_int <- function(x, y) {
    declare(type(x = integer(2)), type(y = integer(3)))
    outer(x, y)
  }

  matmul_lgl <- function(A, B) {
    declare(type(A = logical(2, 2)), type(B = logical(2, 2)))
    A %*% B
  }

  A <- matrix(as.integer(c(1, 2, 3, 4)), nrow = 2)
  B <- matrix(as.integer(c(2, 0, 1, -1)), nrow = 2)
  x <- matrix(as.integer(1:6), nrow = 3)
  v2 <- as.integer(c(1, -2))
  v3 <- as.integer(c(3, 0, 4))

  set.seed(123)
  A_lgl <- matrix(sample(c(TRUE, FALSE), 4, TRUE), nrow = 2)
  B_lgl <- matrix(sample(c(TRUE, FALSE), 4, TRUE), nrow = 2)

  expect_quick_equal(matmul_int, list(A = A, B = B))
  expect_quick_equal(crossprod_int, list(x = x))
  expect_quick_equal(outer_int, list(x = v2, y = v3))
  expect_quick_equal(matmul_lgl, list(A = A_lgl, B = B_lgl))
})

test_that("matrix multiplication avoids unsafe in-place aliasing", {
  fn <- function(A, B) {
    declare(type(A = double(2, 2)), type(B = double(2, 2)))
    A <- (A) %*% B
    A
  }

  set.seed(42)
  A0 <- matrix(rnorm(4), nrow = 2)
  B <- matrix(rnorm(4), nrow = 2)

  qfn <- quick(fn)
  A_orig <- A0 + 0
  out <- qfn(A0, B)

  expect_identical(A0, A_orig)
  expect_equal(out, fn(A_orig, B))
})

test_that("renamed matrix bindings avoid unsafe in-place aliasing", {
  fn <- function(A, B) {
    declare(type(A = double(2, 2)), type(B = double(2, 2)))
    unused <- ifelse(c(TRUE, FALSE), c(1, 2), c(3, 4))
    btmp1. <- A
    btmp1. <- btmp1. %*% B
    btmp1.
  }

  A <- matrix(c(1, 2, 3, 4), nrow = 2)
  B <- matrix(c(2, 0, 1, -1), nrow = 2)
  expect_quick_equal(fn, list(A, B))
})

test_that("matrix multiplication handles expression inputs without mutating sources", {
  fn <- function(A, B) {
    declare(type(A = double(2, 2)), type(B = double(2, 2)))
    A <- (A + 1) %*% B
    A
  }

  set.seed(101)
  A0 <- matrix(rnorm(4), nrow = 2)
  B <- matrix(rnorm(4), nrow = 2)

  qfn <- quick(fn)
  A_orig <- A0 + 0
  out <- qfn(A0, B)

  expect_identical(A0, A_orig)
  expect_equal(out, fn(A_orig, B))
})

test_that("crossprod handles parenthesized inputs without aliasing", {
  fn <- function(A) {
    declare(type(A = double(2, 2)))
    A <- crossprod(((A)))
    A
  }

  set.seed(202)
  A0 <- matrix(rnorm(4), nrow = 2)

  qfn <- quick(fn)
  A_orig <- A0 + 0
  out <- qfn(A0)

  expect_identical(A0, A_orig)
  expect_equal(out, fn(A_orig))
})

test_that("triangular solves can write into the RHS variable safely", {
  fn <- function(U, b) {
    declare(type(U = double(2, 2)), type(b = double(2)))
    b <- backsolve(U, b)
    b
  }

  U <- matrix(c(2, 1, 0, 3), nrow = 2, byrow = TRUE)
  b0 <- c(1.25, -0.5)

  qfn <- quick(fn)
  b_orig <- b0 + 0
  out <- qfn(U, b0)

  expect_identical(b0, b_orig)
  expect_equal(out, fn(U, b_orig))
})

test_that("triangular solves accept parenthesized RHS safely", {
  fn <- function(U, b) {
    declare(type(U = double(2, 2)), type(b = double(2)))
    b <- backsolve(U, (b))
    b
  }

  U <- matrix(c(2, 1, 0, 3), nrow = 2, byrow = TRUE)
  b0 <- c(1.25, -0.5)

  qfn <- quick(fn)
  b_orig <- b0 + 0
  out <- qfn(U, b0)

  expect_identical(b0, b_orig)
  expect_equal(out, fn(U, b_orig))
})

test_that("crossprod and tcrossprod match R", {
  cross_fun <- function(x, y) {
    declare(
      type(x = double(6, 4)),
      type(y = double(6, 4))
    )
    crossprod(x, y)
  }

  tcross_fun <- function(x, y) {
    declare(
      type(x = double(6, 4)),
      type(y = double(6, 4))
    )
    tcrossprod(x, y)
  }

  set.seed(2)
  x <- matrix(rnorm(6 * 4), nrow = 6)
  y <- matrix(rnorm(6 * 4), nrow = 6)

  expect_quick_equal(cross_fun, list(x = x, y = y))
  expect_quick_equal(tcross_fun, list(x = x, y = y))
})

test_that("single-argument crossprod/tcrossprod match R", {
  cross_single <- function(x) {
    declare(type(x = double(5, 4)))
    crossprod(x)
  }

  tcross_single <- function(x) {
    declare(type(x = double(5, 4)))
    tcrossprod(x)
  }

  cross_vec <- function(x) {
    declare(type(x = double(5)))
    crossprod(x)
  }

  tcross_vec <- function(x) {
    declare(type(x = double(5)))
    tcrossprod(x)
  }

  set.seed(3)
  x <- matrix(rnorm(5 * 4), nrow = 5)
  v <- rnorm(5)

  expect_quick_equal(cross_single, list(x = x))
  expect_quick_equal(tcross_single, list(x = x))
  expect_quick_equal(cross_vec, list(x = v))
  expect_quick_equal(tcross_vec, list(x = v))
})

test_that("crossprod and tcrossprod handle vector inputs", {
  cross_vec_mat <- function(x, y) {
    declare(
      type(x = double(4)),
      type(y = double(4, 3))
    )
    crossprod(x, y)
  }

  cross_vec_vec <- function(x, y) {
    declare(
      type(x = double(4)),
      type(y = double(4))
    )
    crossprod(x, y)
  }

  tcross_vec_vec <- function(x, y) {
    declare(
      type(x = double(4)),
      type(y = double(5))
    )
    tcrossprod(x, y)
  }

  set.seed(13)
  x <- rnorm(4)
  y_cross <- matrix(rnorm(12), nrow = 4)
  y_vec <- rnorm(4)
  y_vec_long <- rnorm(5)

  expect_quick_equal(cross_vec_mat, list(x = x, y = y_cross))
  expect_quick_equal(cross_vec_vec, list(x = x, y = y_vec))
  expect_quick_equal(tcross_vec_vec, list(x = x, y = y_vec_long))
})

test_that("tcrossprod uses vector length against matrix columns", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA, NA)))
    tcrossprod(x, y)
  }

  qfn <- quick(fn)
  x <- as.double(1:3)
  y <- matrix(as.double(1:6), nrow = 2L)
  expect_equal(qfn(x, y), fn(x, y))
  expect_error(
    qfn(x, matrix(as.double(1:4), nrow = 2L)),
    "non-conformable arguments in tcrossprod",
    fixed = TRUE
  )
})

test_that("tcrossprod treats right-hand vectors as columns", {
  static <- function(x, y) {
    declare(type(x = double(2, 1)), type(y = double(3)))
    tcrossprod(x, y)
  }
  dynamic <- function(x, y) {
    declare(type(x = double(NA, NA)), type(y = double(NA)))
    tcrossprod(x, y)
  }

  code <- as.character(r2f(dynamic))
  expect_match(code, "size(x, 2, kind=c_ptrdiff_t) /= 1", fixed = TRUE)
  expect_false(grepl("size(y, 2", code, fixed = TRUE))

  x <- matrix(as.double(1:2), nrow = 2L)
  y <- as.double(1:3)
  expect_quick_equal(static, list(x, y))

  qfn <- quick(dynamic)
  expect_equal(qfn(x, y), dynamic(x, y))

  wide <- matrix(as.double(1:6), nrow = 2L)
  expect_error(dynamic(wide, y), "non-conformable arguments", fixed = TRUE)
  expect_error(qfn(wide, y), "non-conformable arguments", fixed = TRUE)
})

test_that("crossprod rejects incompatible destination dimensions", {
  crossprod_bad_dest <- function(x) {
    declare(type(x = double(4, 3)), type(out = double(2, 2)))
    out <- crossprod(x)
    out
  }

  expect_error(
    quick(crossprod_bad_dest),
    "dimension 1 would change from 2 to 3",
    fixed = TRUE
  )
})

test_that("BLAS output checks an unproven destination shape before writing", {
  # crossprod(x) is 3x3. Check the (m, m) destination's actual shape before
  # writing so neither a smaller nor a larger destination reaches dsyrk.
  fn <- function(x, m) {
    declare(type(x = double(n, 3)), type(m = integer(1)))
    out <- matrix(0, m, m)
    out <- crossprod(x)
    out
  }
  qfn := quick(fn)
  x <- matrix(as.double(1:6), nrow = 2L)
  expect_equal(qfn(x, 3L), crossprod(x))
  expect_error(
    qfn(x, 4L),
    "reassignment must preserve the shape of `out`"
  )
})

test_that("outer supports multiplication and %o%", {
  outer_default <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    outer(x, y)
  }

  outer_mul <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    outer(x, y, "*")
  }

  outer_op <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    x %o% y
  }

  set.seed(10)
  x <- rnorm(3)
  y <- rnorm(4)

  expect_quick_equal(outer_default, list(x = x, y = y))
  expect_quick_equal(outer_mul, list(x = x, y = y))
  expect_quick_equal(outer_op, list(x = x, y = y))
})

test_that("outer supports scalar inputs", {
  outer_scalar <- function(x, y) {
    declare(
      type(x = double(NA)),
      type(y = double(3))
    )
    out <- outer(x, y)
    out
  }

  outer_scalar_op <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(NA))
    )
    out <- x %o% y
    out
  }

  set.seed(16)
  x <- 1.25
  y <- rnorm(3)
  x_vec <- rnorm(3)
  y_scalar <- -0.5

  expect_quick_equal(outer_scalar, list(x = x, y = y))
  expect_quick_equal(outer_scalar_op, list(x = x_vec, y = y_scalar))
})

test_that("blas operations support preallocated outputs", {
  matmul_out <- function(A, B) {
    declare(
      type(A = double(2, 3)),
      type(B = double(3, 2)),
      type(out = double(2, 2))
    )
    out <- A %*% B
    out
  }

  crossprod_out <- function(x) {
    declare(
      type(x = double(4, 3)),
      type(out = double(3, 3))
    )
    out <- crossprod(x)
    out
  }

  outer_out <- function(x, y) {
    declare(
      type(x = double(2)),
      type(y = double(3)),
      type(out = double(2, 3))
    )
    out <- outer(x, y)
    out
  }

  set.seed(12)
  A <- matrix(rnorm(2 * 3), nrow = 2)
  B <- matrix(rnorm(3 * 2), nrow = 3)
  X <- matrix(rnorm(4 * 3), nrow = 4)
  x <- rnorm(2)
  y <- rnorm(3)

  expect_quick_equal(matmul_out, list(A = A, B = B))
  expect_quick_equal(crossprod_out, list(x = X))
  expect_quick_equal(outer_out, list(x = x, y = y))
})

test_that("matrix multiplication temporaries can be indexed directly", {
  fn <- function(A, x) {
    declare(
      type(A = double(n, k)),
      type(x = double(k))
    )
    (A %*% x)[, 1]
  }

  set.seed(42)
  A <- matrix(rnorm(12), nrow = 3)
  x <- rnorm(ncol(A))
  expect_quick_equal(fn, list(A = A, x = x))
})

test_that("outer errors on unsupported FUN", {
  outer_add <- function(x, y) {
    declare(
      type(x = double(3)),
      type(y = double(4))
    )
    outer(x, y, "+")
  }

  set.seed(1)
  x <- rnorm(3)
  y <- rnorm(4)

  expect_error(quick(outer_add), "outer\\(\\) only supports FUN = \"\\*\"")
})

test_that("forwardsolve and backsolve match R", {
  forward_vec <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4))
    )
    forwardsolve(L, b)
  }

  forward_mat <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4, 2))
    )
    forwardsolve(L, b)
  }

  back_vec <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4))
    )
    backsolve(U, b)
  }

  back_mat <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4, 2))
    )
    backsolve(U, b)
  }

  back_transpose <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4))
    )
    backsolve(U, b, transpose = TRUE)
  }

  forward_upper <- function(U, b) {
    declare(
      type(U = double(4, 4)),
      type(b = double(4))
    )
    forwardsolve(U, b, upper.tri = TRUE)
  }

  forward_transpose <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4))
    )
    forwardsolve(L, b, transpose = TRUE)
  }

  back_lower <- function(L, b) {
    declare(
      type(L = double(4, 4)),
      type(b = double(4))
    )
    backsolve(L, b, upper.tri = FALSE)
  }

  set.seed(11)
  base <- matrix(rnorm(16), nrow = 4)
  L <- base
  L[upper.tri(L)] <- 0
  diag(L) <- diag(L) + 5
  U <- base
  U[lower.tri(U)] <- 0
  diag(U) <- diag(U) + 5
  b_vec <- rnorm(4)
  b_mat <- matrix(rnorm(8), nrow = 4)

  expect_quick_equal(forward_vec, list(L = L, b = b_vec))
  expect_quick_equal(forward_mat, list(L = L, b = b_mat))
  expect_quick_equal(back_vec, list(U = U, b = b_vec))
  expect_quick_equal(back_mat, list(U = U, b = b_mat))
  expect_quick_equal(back_transpose, list(U = U, b = b_vec))
  expect_quick_equal(forward_upper, list(U = U, b = b_vec))
  expect_quick_equal(forward_transpose, list(L = L, b = b_vec))
  expect_quick_equal(back_lower, list(L = L, b = b_vec))
})

test_that("repeated BLAS operations reuse symbolic output storage", {
  fn <- function(x, steps) {
    declare(type(x = double(n, k)), type(steps = integer(1)))
    out <- matrix(0, ncol(x), ncol(x))
    for (i in seq_len(steps)) {
      out <- crossprod(x)
    }
    out
  }

  x <- matrix(as.double(1:6), 2, 3)
  expect_quick_equal(fn, list(x, 0L), list(x, 1L), list(x, 3L))

  # The returned destination is allocated by the C wrapper. Repeated BLAS
  # writes must not allocate a native result array and copy it into `out`.
  code <- as.character(r2f(fn))
  expect_false(grepl("allocate(", code, fixed = TRUE))
})

test_that("symbolic output guards run before BLAS writes", {
  fn <- function(x, m) {
    declare(type(x = double(n, k)), type(m = integer(1)))
    out <- matrix(0, m, m)
    out <- crossprod(x)
    out
  }

  x <- matrix(as.double(1:6), 2, 3)
  qfn <- quick(fn)
  expect_equal(qfn(x, 3L), fn(x, 3L))
  for (m in c(2L, 4L)) {
    expect_error(
      qfn(x, m),
      "reassignment must preserve the shape of `out`",
      fixed = TRUE
    )
  }

  code <- as.character(r2f(fn))
  expect_lt(
    regexpr(
      "reassignment must preserve the shape of `out`",
      code,
      fixed = TRUE
    )[[1L]],
    regexpr("call dsyrk(", code, fixed = TRUE)[[1L]]
  )
})

test_that("BLAS reuse checks stored extents after dimension bindings change", {
  fn <- function(x, m) {
    declare(type(x = double(n, k)), type(m = integer(1)))
    out <- matrix(0, m, m)
    m <- m + 1L
    out <- crossprod(x)
    sum(out)
  }

  x <- matrix(as.double(1:6), 2, 3)
  expect_quick_equal(fn, list(x, 3L))
  expect_error(
    quick(fn)(x, 2L),
    "reassignment must preserve the shape of `out`",
    fixed = TRUE
  )
})

test_that("symbolic BLAS destinations check literal singleton result axes", {
  fn <- function(x, m) {
    declare(type(x = double(n, 1)), type(m = integer(1)))
    out <- matrix(0, m, m)
    out <- crossprod(x)
    out
  }

  x <- matrix(c(1, 2, 3), ncol = 1)
  qfn <- quick(fn)
  expect_equal(qfn(x, 1L), fn(x, 1L))
  expect_equal(qfn(matrix(numeric(), 0, 1), 1L), matrix(0, 1, 1))
  for (m in c(0L, 2L)) {
    expect_error(
      qfn(x, m),
      "reassignment must preserve the shape of `out`",
      fixed = TRUE
    )
  }
})

test_that("triangular solves retain RHS storage extents after rebinding", {
  for (operation in c("forwardsolve", "backsolve")) {
    vector_rhs <- eval(substitute(
      function(a, b, m) {
        declare(
          type(a = double(2, 2)),
          type(b = double(m)),
          type(m = integer(1))
        )
        out <- numeric(2)
        m <- m + 1L
        out <- SOLVE(a, b)
        out
      },
      list(SOLVE = as.name(operation))
    ))
    matrix_rhs <- eval(substitute(
      function(a, b, m, columns) {
        declare(
          type(a = double(2, 2)),
          type(b = double(2, m)),
          type(m = integer(1)),
          type(columns = integer(1))
        )
        out <- matrix(0, 2, columns)
        m <- m + 1L
        out <- SOLVE(a, b)
        out
      },
      list(SOLVE = as.name(operation))
    ))

    a <- matrix(c(2, 1, 1, 4), 2, 2)
    b <- matrix(c(3, 5, 7, 9), 2, 2)
    expect_quick_equal(vector_rhs, list(a, b[, 1], 2L))
    qmatrix <- quick(matrix_rhs)
    expect_equal(qmatrix(a, b, 2L, 2L), matrix_rhs(a, b, 2L, 2L))
    for (columns in c(1L, 3L)) {
      expect_error(
        qmatrix(a, b, 2L, columns),
        "reassignment must preserve the shape of `out`",
        fixed = TRUE
      )
    }
  }
})

test_that("BLAS reuse allocates locals after untaken earlier BLAS writes", {
  fn <- function(a, b, flag) {
    declare(
      type(a = double(n, n)),
      type(b = double(n, n)),
      type(flag = logical(1))
    )
    if (flag) {
      z <- solve(a, b)
    }
    z <- a %*% b
    sum(z)
  }

  a <- diag(c(2, 3))
  b <- matrix(as.double(1:4), 2, 2)
  expect_quick_equal(fn, list(a, b, FALSE), list(a, b, TRUE))
})

test_that("symbolic BLAS outputs preserve alias and deferred-shape behavior", {
  alias <- function(x) {
    declare(type(x = double(n, n)))
    x <- crossprod(x)
    x
  }
  deferred <- function(x) {
    declare(type(x = double(n, k)), type(out = double(NA, NA)))
    out <- x
    out <- crossprod(x)
    sum(out)
  }

  x <- matrix(as.double(1:4), 2, 2)
  saved <- x + 0
  expect_equal(quick(alias)(x), alias(x))
  expect_identical(x, saved)
  expect_quick_equal(deferred, matrix(as.double(1:6), 2, 3))
})
