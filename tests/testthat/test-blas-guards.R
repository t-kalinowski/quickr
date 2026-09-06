# Runtime conformability guards in BLAS/LAPACK lowerings: dims that cannot
# be verified at compile time get a size() check before the BLAS call
# (never a compile-time warning, never an unchecked call).
test_that("matrix-vector %*% guards an unknown vector length", {
  fn <- function(m, x) {
    declare(type(m = double(3, 3)), type(x = double(NA)))
    m %*% x
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(diag(3), as.double(1:3)), diag(3) %*% 1:3)
  # was: dgemv read past the end of x, returning garbage
  expect_error(
    qfn(diag(3), as.double(1:2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("matrix-matrix %*% guards before allocating its result", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m, p)))
    sum(a %*% b)
  }

  code <- as.character(r2f(fn))
  guard <- regexpr("non-conformable arguments in %*%", code, fixed = TRUE)
  allocation <- regexpr("allocate(", code, fixed = TRUE)
  expect_lt(guard, allocation)

  qfn <- quick(fn)
  expect_equal(qfn(matrix(as.double(1:4), 2, 2), diag(2)), 10)
  expect_error(
    qfn(matrix(as.double(1:2), 2, 1), matrix(as.double(1:4), 2, 2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("matrix-vector %*% guards before allocating its result", {
  fn <- function(a, x) {
    declare(type(a = double(m, k)), type(x = double(n)))
    sum(a %*% x)
  }

  code <- as.character(r2f(fn))
  guard <- regexpr("non-conformable arguments in %*%", code, fixed = TRUE)
  allocation <- regexpr("allocate(", code, fixed = TRUE)
  expect_lt(guard, allocation)

  qfn <- quick(fn)
  expect_equal(qfn(matrix(as.double(1:4), 2, 2), c(1, 1)), 10)
  expect_error(
    qfn(matrix(as.double(1:2), 2, 1), c(1, 1)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("matrix-matrix %*% guards before allocating a reusable local", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m, p)))
    out <- a %*% b
    sum(out)
  }

  code <- as.character(r2f(fn))
  guard <- regexpr("non-conformable arguments in %*%", code, fixed = TRUE)
  allocation <- regexpr("allocate(out(", code, fixed = TRUE)
  expect_lt(guard, allocation)

  qfn <- quick(fn)
  expect_equal(qfn(matrix(as.double(1:4), 2, 2), diag(2)), 10)
  expect_error(
    qfn(matrix(as.double(1:2), 2, 1), matrix(as.double(1:4), 2, 2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("matrix-vector %*% guards before allocating a reusable local", {
  fn <- function(a, x) {
    declare(type(a = double(m, k)), type(x = double(n)))
    out <- a %*% x
    sum(out)
  }

  code <- as.character(r2f(fn))
  guard <- regexpr("non-conformable arguments in %*%", code, fixed = TRUE)
  allocation <- regexpr("allocate(out(", code, fixed = TRUE)
  expect_lt(guard, allocation)

  qfn <- quick(fn)
  expect_equal(qfn(matrix(as.double(1:4), 2, 2), c(1, 1)), 10)
  expect_error(
    qfn(matrix(as.double(1:2), 2, 1), c(1, 1)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("reused BLAS locals retain their earlier allocation", {
  gemm <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m, k)))
    out <- a + 0
    out <- a %*% b
    sum(out)
  }
  gemv <- function(a, x) {
    declare(type(a = double(m, 1)), type(x = double(n)))
    out <- a + 0
    out <- a %*% x
    sum(out)
  }

  for (fn in list(gemm, gemv)) {
    code <- as.character(r2f(fn))
    allocation <- regexpr("allocate(out(", code, fixed = TRUE)
    initialization <- regexpr("out = (a + 0.0_c_double)", code, fixed = TRUE)
    expect_lt(allocation, initialization)
  }

  qgemm <- quick(gemm)
  a <- matrix(as.double(1:4), 2, 2)
  b <- diag(2)
  expect_equal(qgemm(a, b), gemm(a, b))
  expect_error(
    qgemm(matrix(as.double(1:2), 2, 1), matrix(as.double(1:2), 2, 1)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )

  qgemv <- quick(gemv)
  a <- matrix(as.double(1:3), 3, 1)
  expect_equal(qgemv(a, 2), gemv(a, 2))
  expect_error(
    qgemv(a, c(1, 2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("reused BLAS locals are allocated on every reachable path", {
  fn <- function(a, b, flag) {
    declare(
      type(a = double(n, k)),
      type(b = double(k, n)),
      type(flag = logical(1))
    )
    if (flag) {
      out <- a %*% b
      extracted <- diag(a)
      constructed <- a %*% b
      outered <- a %*% b
    }
    out <- crossprod(b)
    extracted <- diag(a)
    constructed <- diag(1, nrow = nrow(a), ncol = ncol(b))
    outered <- outer(a[, 1L], a[, 1L])
    sum(out) + sum(extracted) + sum(constructed) + sum(outered)
  }

  code <- as.character(r2f(fn))
  allocation_guards <- gregexpr(
    "if (.not. allocated(",
    code,
    fixed = TRUE
  )[[1L]]
  expect_length(allocation_guards[allocation_guards > 0L], 8L)

  a <- matrix(as.double(1:6), 2, 3)
  b <- matrix(as.double(1:6), 3, 2)
  qfn <- quick(fn)
  expect_equal(qfn(a, b, FALSE), fn(a, b, FALSE))
  expect_equal(qfn(a, b, TRUE), fn(a, b, TRUE))
})
test_that("BLAS destinations are not reused across unproven shapes", {
  local <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(k, p)))
    out <- a + 0
    out <- a %*% b
    sum(out)
  }
  external <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(k, p)))
    out <- a + 0
    out <- a %*% b
    out
  }

  a <- matrix(as.double(1:6), 2, 3)
  b <- matrix(as.double(1:3), 3, 1)
  expect_quick_equal(local, list(a, b))
  expect_error(
    quick(external),
    "cannot change the shape of an external assignment target",
    fixed = TRUE
  )
})
test_that("renamed BLAS return destinations remain output arguments", {
  fn <- function(a, b, n) {
    declare(
      type(a = double(m, k)),
      type(b = double(k, p)),
      type(n = integer(1))
    )
    x <- runif(n)
    Tmp1. <- a %*% b
    Tmp1.
  }

  qfn <- quick(fn)
  a <- matrix(as.double(1:6), 2, 3)
  b <- matrix(as.double(1:6), 3, 2)

  set.seed(823)
  expected <- fn(a, b, 2L)
  expected_seed <- .Random.seed

  set.seed(823)
  actual <- qfn(a, b, 2L)
  actual_seed <- .Random.seed

  expect_equal(actual, expected)
  expect_identical(actual_seed, expected_seed)
})
test_that("%*% evaluates effectful operands before a runtime shape error", {
  matmul <- function(m) {
    declare(type(m = double(n, n)))
    m %*% runif(3)
  }
  cross <- function(m) {
    declare(type(m = double(n, 2)), type(out = double(2, 1)))
    out <- crossprod(m, runif(3))
    out
  }
  triangular <- function(m) {
    declare(type(m = double(n, n)))
    forwardsolve(m, runif(3))
  }
  solve_rhs <- function(m) {
    declare(type(m = double(n, n)))
    solve(m, runif(3))
  }

  expect_rng_advance <- function(qfn, input, message) {
    set.seed(123)
    expect_error(qfn(input), message)
    actual_seed <- .Random.seed

    set.seed(123)
    runif(3)
    expect_identical(actual_seed, .Random.seed)
  }

  expect_rng_advance(quick(matmul), diag(2), "non-conformable arguments in %*%")
  expect_rng_advance(
    quick(cross),
    matrix(as.double(1:4), 2, 2),
    "non-conformable arguments in crossprod"
  )
  expect_rng_advance(
    quick(triangular),
    diag(2),
    "non-conformable arguments in triangular solve"
  )
  expect_rng_advance(
    quick(solve_rhs),
    diag(2),
    "non-conformable arguments in solve"
  )
})
test_that("qr.solve evaluates tol before a runtime shape error", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m)))
    qr.solve(a, b, tol = runif(1))
  }
  qfn <- quick(fn)

  set.seed(125)
  expect_error(
    qfn(diag(2), as.double(1:3)),
    "non-conformable arguments in qr.solve"
  )
  actual_seed <- .Random.seed

  set.seed(125)
  runif(1)
  expect_identical(actual_seed, .Random.seed)
})
test_that("nested %*% operands preserve left-to-right evaluation", {
  fn <- function() {
    runif(2) %*% matrix(runif(4), 2, 2)
  }
  set.seed(124)
  expected <- fn()
  set.seed(124)
  expect_identical(quick(fn)(), expected)
})
test_that("double BLAS entry points reject raw storage", {
  fn <- function(a, b) {
    declare(type(a = raw(2)), type(b = raw(2)))
    a %*% b
  }
  expect_error(quick(fn), "does not support raw")
})
test_that("tcrossprod materializes scalar-backed array operands", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    tcrossprod(array(0, dim = n))
  }

  expect_quick_identical(fn, list(3L))
})
test_that("vector-matrix %*% guards an unknown vector length", {
  fn <- function(x, m) {
    declare(type(x = double(NA)), type(m = double(3, 3)))
    t(x) %*% m
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(as.double(1:3), diag(3)), t(as.double(1:3)) %*% diag(3))
  expect_error(
    qfn(as.double(1:5), diag(3)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("triangular solve guards squareness and RHS length", {
  fn <- function(l, x) {
    declare(type(l = double(n, k)), type(x = double(NA)))
    forwardsolve(l, x)
  }
  qfn <- expect_no_warning(quick(fn))
  l <- matrix(c(1, 2, 0, 3), 2, 2)
  expect_equal(qfn(l, c(1, 5)), forwardsolve(l, c(1, 5)))
  expect_error(
    qfn(l, c(1, 5, 9)),
    "non-conformable arguments in triangular solve"
  )
  expect_error(
    qfn(matrix(as.double(1:6), 2, 3), c(1, 5)),
    "triangular solve requires a square matrix"
  )
})
test_that("LAPACK solves reject zero-sized outputs before library calls", {
  solve_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n, p)))
    solve(a, b)
  }
  forward_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n, p)))
    forwardsolve(a, b)
  }
  back_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n, p)))
    backsolve(a, b)
  }

  a <- matrix(numeric(), 0, 0)
  for (fn in list(solve_fn, forward_fn, back_fn)) {
    qfn <- quick(fn)
    expect_error(
      qfn(a, matrix(numeric(), 0, 1)),
      "zero-sized outputs are not supported",
      fixed = TRUE
    )
    expect_error(
      qfn(diag(2), matrix(numeric(), 2, 0)),
      "zero-sized outputs are not supported",
      fixed = TRUE
    )
  }
})
test_that("vector %*% vector guards unknown lengths as whole sizes", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    x %*% y
  }
  # was: the fallthrough guard hardcoded rank-2 axes, emitting size(x, 2)
  # on a rank-1 array -- a gfortran error that made a conformable
  # unknown-length dot product fail to compile at all
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(c(1, 2, 3), c(4, 5, 6)), c(1, 2, 3) %*% c(4, 5, 6))
  expect_error(
    qfn(c(1, 2), c(4, 5, 6)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("solve() guards an unknown RHS length", {
  fn <- function(a, b) {
    declare(type(a = double(2, 2)), type(b = double(NA)))
    solve(a, b)
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(diag(2), c(1, 2)), c(1, 2))
  expect_error(qfn(diag(2), c(1, 2, 3)), "non-conformable arguments in solve")
})

test_that("solve(a) and chol() guard squareness", {
  solve_rhs <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(NA)))
    solve(a, b)
  }
  qsolve_rhs <- expect_no_warning(quick(solve_rhs))
  expect_error(
    qsolve_rhs(matrix(as.double(1:6), 2, 3), as.double(1:4)),
    "solve requires a square matrix"
  )

  inv <- function(a) {
    declare(type(a = double(n, k)))
    solve(a)
  }
  qinv <- expect_no_warning(quick(inv))
  expect_equal(qinv(diag(2)), diag(2))
  expect_error(
    qinv(matrix(as.double(1:6), 2, 3)),
    "solve requires a square matrix"
  )

  chol_fn <- function(a) {
    declare(type(a = double(n, k)))
    chol(a)
  }
  qchol <- expect_no_warning(quick(chol_fn))
  expect_equal(qchol(diag(2)), diag(2))
  expect_error(
    qchol(matrix(as.double(1:6), 2, 3)),
    "chol requires a square matrix"
  )
})
test_that("square guards precede symbolic inverse and Cholesky allocations", {
  inverse <- function(a) {
    declare(type(a = double(n, k)))
    sum(solve(a))
  }
  cholesky <- function(a) {
    declare(type(a = double(n, k)))
    sum(chol(a))
  }
  chol_inverse <- function(a) {
    declare(type(a = double(n, k)))
    sum(chol2inv(a))
  }

  cases <- list(
    list(fn = inverse, message = "solve requires a square matrix"),
    list(fn = cholesky, message = "chol requires a square matrix"),
    list(fn = chol_inverse, message = "chol2inv requires a square matrix")
  )
  for (case in cases) {
    code <- strsplit(as.character(r2f(case$fn)), "\n", fixed = TRUE)[[1L]]
    output_decl <- grep(
      "real\\(c_double\\), allocatable :: .*\\(:, :\\)",
      code
    )
    expect_length(output_decl, 1L)
    output_name <- sub(
      ".*:: ([^(:]+)\\(:, :\\).*",
      "\\1",
      code[[output_decl]]
    )
    guard_line <- grep(case$message, code, fixed = TRUE)
    allocation_line <- grep(
      paste0("allocate(", output_name, "("),
      code,
      fixed = TRUE
    )
    expect_length(guard_line, 1L)
    expect_length(allocation_line, 1L)
    expect_lt(guard_line, allocation_line)

    qfn <- quick(case$fn)
    expect_equal(qfn(diag(2)), 2)
    expect_error(
      qfn(matrix(as.double(1:6), 2, 3)),
      case$message,
      fixed = TRUE
    )
    expect_error(
      qfn(matrix(double(), 0, 0)),
      "zero-sized outputs are not supported",
      fixed = TRUE
    )
  }
})
test_that("solve guards squareness before allocating system workspaces", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(NA)))
    sum(solve(a, b))
  }

  code <- strsplit(as.character(r2f(fn)), "\n", fixed = TRUE)[[1L]]
  guard_line <- grep("solve requires a square matrix", code, fixed = TRUE)
  allocation_lines <- grep("^ *allocate\\(", code)
  expect_length(guard_line, 1L)
  expect_length(allocation_lines, 3L)
  expect_true(all(guard_line < allocation_lines))

  qfn <- quick(fn)
  expect_equal(qfn(diag(2), c(1, 2)), 3)
  expect_error(
    qfn(matrix(as.double(1:6), 2, 3), c(1, 2)),
    "solve requires a square matrix",
    fixed = TRUE
  )
})
test_that("inverse solve guards before allocating all workspaces", {
  fn <- function(a) {
    declare(type(a = double(n, k)))
    sum(solve(a))
  }

  code <- strsplit(as.character(r2f(fn)), "\n", fixed = TRUE)[[1L]]
  guard_line <- grep("solve requires a square matrix", code, fixed = TRUE)
  allocation_lines <- grep("^ *allocate\\(", code)
  expect_length(guard_line, 1L)
  expect_length(allocation_lines, 3L)
  expect_true(all(guard_line < allocation_lines))

  qfn <- quick(fn)
  expect_equal(qfn(diag(2)), 2)
  expect_error(
    qfn(matrix(as.double(1:6), 2, 3)),
    "solve requires a square matrix",
    fixed = TRUE
  )
})
test_that("triangular solve guards before allocating a nested result", {
  fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(NA)))
    sum(forwardsolve(a, b))
  }

  code <- strsplit(as.character(r2f(fn)), "\n", fixed = TRUE)[[1L]]
  guard_line <- grep(
    "non-conformable arguments in triangular solve",
    code,
    fixed = TRUE
  )
  allocation_lines <- grep("^ *allocate\\(", code)
  expect_length(guard_line, 1L)
  expect_gt(length(allocation_lines), 0L)
  expect_true(all(guard_line < allocation_lines))

  qfn <- quick(fn)
  expect_equal(qfn(diag(2), c(1, 2)), 3)
  expect_error(
    qfn(diag(2), as.double(1:3)),
    "non-conformable arguments in triangular solve",
    fixed = TRUE
  )
})
test_that("qr.solve guards before allocating nested workspaces", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m, p)))
    sum(qr.solve(a, b))
  }

  code <- strsplit(as.character(r2f(fn)), "\n", fixed = TRUE)[[1L]]
  guard_line <- grep(
    "non-conformable arguments in qr.solve",
    code,
    fixed = TRUE
  )
  allocation_lines <- grep("^ *allocate\\(", code)
  expect_length(guard_line, 1L)
  expect_gt(length(allocation_lines), 0L)
  expect_true(all(guard_line < allocation_lines))

  qfn <- quick(fn)
  a <- matrix(as.double(c(1, 0, 1, 0, 1, 1)), 3, 2)
  b <- matrix(as.double(1:6), 3, 2)
  expect_equal(qfn(a, b), sum(qr.solve(a, b)))
  expect_error(
    qfn(a, matrix(as.double(1:8), 2, 4)),
    "non-conformable arguments in qr.solve",
    fixed = TRUE
  )
})
test_that("matrix-matrix %*% returns zeros for a known empty contraction", {
  fn <- function(a, b) {
    declare(type(a = double(2, 0)), type(b = double(0, 3)))
    a %*% b
  }
  expect_quick_identical(
    fn,
    list(matrix(double(), 2, 0), matrix(double(), 0, 3))
  )
})
test_that("matrix-vector %*% returns zeros for a known empty contraction", {
  fn <- function(a, x) {
    declare(type(a = double(2, 0)), type(x = double(0)))
    a %*% x
  }
  expect_quick_identical(fn, list(matrix(double(), 2, 0), double()))
})
test_that("%*% returns zeros for a symbolic empty contracted dimension", {
  fn <- function(a, b, k) {
    declare(
      type(a = double(2, k)),
      type(b = double(k, 3)),
      type(k = integer(1))
    )
    a %*% b
  }
  expect_quick_identical(
    fn,
    list(matrix(double(), 2, 0), matrix(double(), 0, 3), 0L)
  )
})
test_that("symmetric products return zeros for known empty contractions", {
  cross_vec <- function(x) {
    declare(type(x = double(0)))
    crossprod(x)
  }
  cross_mat <- function(x) {
    declare(type(x = double(0, 2)))
    crossprod(x)
  }
  tcross_mat <- function(x) {
    declare(type(x = double(2, 0)))
    tcrossprod(x)
  }
  cross_symbolic <- function(x, n) {
    declare(type(x = double(n, 2)), type(n = integer(1)))
    crossprod(x)
  }
  tcross_symbolic <- function(x, n) {
    declare(type(x = double(2, n)), type(n = integer(1)))
    tcrossprod(x)
  }

  expect_quick_identical(cross_vec, list(double()))
  expect_quick_identical(cross_mat, list(matrix(double(), 0, 2)))
  expect_quick_identical(tcross_mat, list(matrix(double(), 2, 0)))
  expect_quick_identical(cross_symbolic, list(matrix(double(), 0, 2), 0L))
  expect_quick_identical(tcross_symbolic, list(matrix(double(), 2, 0), 0L))
})
test_that("matrix BLAS rejects known zero-sized outputs", {
  matrix_matrix <- function(a, b) {
    declare(type(a = double(0, 2)), type(b = double(2, 3)))
    a %*% b
  }
  matrix_vector <- function(a, x) {
    declare(type(a = double(0, 2)), type(x = double(2)))
    a %*% x
  }
  tcross_vec <- function(x) {
    declare(type(x = double(0)))
    tcrossprod(x)
  }
  cross_mat <- function(x) {
    declare(type(x = double(0, 0)))
    crossprod(x)
  }
  outer_left <- function(x, y) {
    declare(type(x = double(0)), type(y = double(2)))
    outer(x, y)
  }
  outer_right <- function(x, y) {
    declare(type(x = double(2)), type(y = double(0)))
    x %o% y
  }

  expect_error(quick(matrix_matrix), "zero-sized outputs are not supported")
  expect_error(quick(matrix_vector), "zero-sized outputs are not supported")
  expect_error(quick(tcross_vec), "zero-sized outputs are not supported")
  expect_error(quick(cross_mat), "zero-sized outputs are not supported")
  expect_error(quick(outer_left), "zero-sized outputs are not supported")
  expect_error(quick(outer_right), "zero-sized outputs are not supported")
})
test_that("matrix BLAS guards unknown output extents at runtime", {
  matrix_matrix <- function(a, b) {
    declare(type(a = double(NA, 2)), type(b = double(2, 3)))
    a %*% b
  }
  matrix_vector <- function(a, x) {
    declare(type(a = double(NA, 2)), type(x = double(2)))
    a %*% x
  }
  cross_mat <- function(x) {
    declare(type(x = double(2, NA)))
    crossprod(x)
  }
  tcross_mat <- function(x) {
    declare(type(x = double(NA, 2)))
    tcrossprod(x)
  }
  outer_fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    outer(x, y)
  }

  q_matrix_matrix <- expect_no_warning(quick(matrix_matrix))
  q_matrix_vector <- expect_no_warning(quick(matrix_vector))
  q_cross_mat <- expect_no_warning(quick(cross_mat))
  q_tcross_mat <- expect_no_warning(quick(tcross_mat))
  q_outer <- expect_no_warning(quick(outer_fn))
  message <- "zero-sized outputs are not supported"

  expect_error(
    q_matrix_matrix(matrix(double(), 0, 2), matrix(double(), 2, 3)),
    message
  )
  expect_error(q_matrix_vector(matrix(double(), 0, 2), double(2)), message)
  expect_error(q_cross_mat(matrix(double(), 2, 0)), message)
  expect_error(q_tcross_mat(matrix(double(), 0, 2)), message)
  expect_equal(q_outer(as.double(1:2), as.double(3:4)), outer(1:2, 3:4))
  expect_error(q_outer(double(), as.double(1:2)), message)
  expect_error(q_outer(as.double(1:2), double()), message)
})
test_that("NA dims are never treated as equal", {
  fn <- function(a, b) {
    declare(type(a = double(NA, NA)), type(b = double(NA, NA)))
    a %*% b
  }
  qfn <- expect_no_warning(quick(fn))
  m <- matrix(as.double(1:4), 2, 2)
  expect_equal(qfn(m, m), m %*% m)
  # was: identical(NA, NA) blessed the pair with no check at all
  expect_error(
    qfn(m, matrix(as.double(1:6), 3, 2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
test_that("crossprod with unverifiable dims compiles and guards at runtime", {
  fn <- function(x, y, n, p, m, k) {
    declare(
      type(n = integer(1)),
      type(p = integer(1)),
      type(m = integer(1)),
      type(k = integer(1)),
      type(x = double(n, m)),
      type(y = double(p, k))
    )
    crossprod(x, y)
  }

  qfn <- quick(fn)
  x <- matrix(as.double(1:6), 2, 3)
  y <- matrix(as.double(6:1), 2, 3)
  expect_equal(qfn(x, y, 2L, 2L, 3L, 3L), crossprod(x, y))
  expect_error(
    qfn(x, matrix(as.double(1:6), 3, 2), 2L, 3L, 3L, 2L),
    "non-conformable arguments in crossprod"
  )
})
test_that("unverifiable %*% dims compile and guard at runtime", {
  fn <- function(A, B, n, m, k) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(k = integer(1)),
      type(A = double(n, m)),
      type(B = double(k, n))
    )
    A %*% B
  }

  c_code <- as.character(r2f(fn)@c_bridge)
  guard <- regexpr("non-conformable arguments in %*%", c_code, fixed = TRUE)
  allocation <- regexpr("Rf_allocVector(REALSXP", c_code, fixed = TRUE)
  expect_true(guard > 0L && guard < allocation)
  qfn <- expect_no_warning(quick(fn))
  A <- matrix(as.double(1:6), 2, 3)
  B <- matrix(as.double(6:1), 3, 2)
  expect_equal(qfn(A, B, 2L, 3L, 3L), A %*% B)
  expect_error(
    qfn(A, matrix(as.double(1:4), 2, 2), 2L, 3L, 2L),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})

test_that("cross-product return allocations validate contracted dimensions", {
  cross <- function(x, y) {
    declare(type(x = double(NA, NA)), type(y = double(NA, NA)))
    crossprod(x, y)
  }
  tcross <- function(x, y) {
    declare(type(x = double(NA, NA)), type(y = double(NA, NA)))
    tcrossprod(x, y)
  }
  x <- matrix(as.double(1:6), 2L, 3L)
  y <- matrix(as.double(1:8), 2L, 4L)
  old_limit <- mem.maxVSize()
  withr::defer(mem.maxVSize(old_limit))
  # A regression must report a normal R allocation error, never attempt an
  # 80 GB allocation in the test process.
  mem.maxVSize(max(256, 2 * sum(gc()[, 2L])))
  for (fn in list(cross, tcross)) {
    qfn <- quick(fn)
    expect_error(qfn(x, t(x)), "non-conformable arguments")
    if (identical(fn, tcross)) {
      expect_equal(qfn(t(x), t(y)), fn(t(x), t(y)))
    } else {
      expect_equal(qfn(x, y), fn(x, y))
    }
    a <- matrix(1, 1L, 100000L)
    b <- matrix(1, 2L, 100000L)
    if (identical(fn, tcross)) {
      a <- t(a)
      b <- t(b)
    }
    expect_error(qfn(a, b), "non-conformable arguments")
  }
})

test_that("square-matrix return requirements precede C allocation", {
  old_limit <- mem.maxVSize()
  withr::defer(mem.maxVSize(old_limit))
  mem.maxVSize(max(256, 2 * sum(gc()[, 2L])))
  for (op in c("solve", "chol", "chol2inv")) {
    fn <- function(a) {
      declare(type(a = double(n, m)))
      solve(a)
    }
    body(fn)[[3L]][[1L]] <- as.symbol(op)
    qfn <- quick(fn)
    expect_equal(qfn(diag(2)), fn(diag(2)))
    expect_error(
      qfn(matrix(1, 46000L, 1L)),
      paste(op, "requires a square matrix")
    )
  }
})

test_that("solve return requirements check both squareness and RHS rows", {
  old_limit <- mem.maxVSize()
  withr::defer(mem.maxVSize(old_limit))
  mem.maxVSize(max(256, 2 * sum(gc()[, 2L])))
  fn <- function(a, b) {
    declare(type(a = double(n, m)), type(b = double(k, p)))
    solve(a, b)
  }
  qfn <- quick(fn)
  expect_equal(qfn(diag(2), diag(2)), diag(2))
  expect_error(
    qfn(matrix(1, 1L, 46000L), matrix(1, 1L, 46000L)),
    "solve requires a square matrix"
  )
  expect_error(
    qfn(matrix(numeric(), 0L, 0L), matrix(1, 1L, 46000L)),
    "non-conformable arguments in solve"
  )
})

test_that("return shape requirements survive copies and precede list allocation", {
  old_limit <- mem.maxVSize()
  withr::defer(mem.maxVSize(old_limit))
  mem.maxVSize(max(256, 2 * sum(gc()[, 2L])))
  alias <- function(a) {
    declare(type(a = double(n, m)))
    x <- solve(a)
    y <- x
    y
  }
  listed <- function(a) {
    declare(type(a = double(n, m)))
    x <- tcrossprod(a)
    y <- solve(a)
    list(x, y)
  }
  for (fn in list(alias, listed)) {
    qfn <- quick(fn)
    expect_equal(qfn(diag(2)), fn(diag(2)))
    expect_error(qfn(matrix(1, 46000L, 1L)), "solve requires a square matrix")
  }
})

test_that("vector products and QR solves validate before oversized returns", {
  old_limit <- mem.maxVSize()
  withr::defer(mem.maxVSize(old_limit))
  mem.maxVSize(max(256, 2 * sum(gc()[, 2L])))
  matvec <- function(a, b) {
    declare(type(a = double(n, m)), type(b = double(k)))
    a %*% b
  }
  vecmat <- function(a, b) {
    declare(type(a = double(k)), type(b = double(n, m)))
    a %*% b
  }
  qr_fn <- function(a, b) {
    declare(type(a = double(n, m)), type(b = double(k, p)))
    qr.solve(a, b)
  }
  qmatvec <- quick(matvec)
  qvecmat <- quick(vecmat)
  qsolve <- quick(qr_fn)
  expect_error(
    qmatvec(matrix(numeric(), 100000000L, 0L), 1),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
  expect_error(
    qvecmat(1, matrix(numeric(), 0L, 100000000L)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
  expect_error(
    qsolve(diag(1), matrix(numeric(), 0L, 100000000L)),
    "non-conformable arguments in qr.solve",
    fixed = TRUE
  )
  expect_equal(qmatvec(diag(2), c(1, 2)), matvec(diag(2), c(1, 2)))
  expect_equal(qvecmat(c(1, 2), diag(2)), vecmat(c(1, 2), diag(2)))
  a <- rbind(diag(2), c(1, 1))
  b <- matrix(c(1, 2, 3), 3, 1)
  expect_equal(qsolve(a, b), qr_fn(a, b))
})

test_that("return preflight preserves RNG effects and conditional execution", {
  fn <- function(a) {
    declare(type(a = double(n, m)))
    noise <- runif(1L)
    solve(a)
  }
  qfn <- quick(fn)
  a <- matrix(1, 2, 1)
  withr::local_seed(335)
  expect_error(qfn(a), "solve requires a square matrix")
  actual_seed <- .Random.seed
  set.seed(335)
  expect_error(fn(a), "square")
  expect_identical(actual_seed, .Random.seed)

  conditional <- function(a, flag) {
    declare(type(a = double(n, m)), type(flag = logical(1)))
    if (flag) {
      out <- solve(a)
    } else {
      out <- matrix(0, nrow(a), nrow(a))
    }
    out
  }
  qconditional <- quick(conditional)
  expect_equal(qconditional(a, FALSE), conditional(a, FALSE))
  expect_equal(qconditional(diag(2), TRUE), conditional(diag(2), TRUE))
})
