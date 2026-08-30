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
      type(b = double(k, p)),
      type(flag = logical(1))
    )
    if (flag) {
      out <- a %*% b
    }
    out <- a %*% b
    sum(out)
  }

  code <- as.character(r2f(fn))
  allocation_guards <- gregexpr(
    "if (.not. allocated(out)) allocate(out(",
    code,
    fixed = TRUE
  )[[1L]]
  expect_length(allocation_guards[allocation_guards > 0L], 2L)

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

test_that("SYRK point-allocates reused conditional destinations", {
  fn <- function(flag, a, b, x) {
    declare(
      type(flag = logical(1)),
      type(a = double(n, k)),
      type(b = double(k, n)),
      type(x = double(m, n))
    )
    if (flag) {
      out <- a %*% b
    }
    out <- crossprod(x)
    sum(out)
  }

  a <- matrix(as.double(1:6), 2, 3)
  b <- matrix(as.double(1:6), 3, 2)
  x <- matrix(as.double(1:6), 3, 2)
  qfn <- quick(fn)
  expected <- sum(crossprod(x))
  expect_equal(qfn(FALSE, a, b, x), expected)
  expect_equal(qfn(TRUE, a, b, x), expected)
})

test_that("diag point-allocates reused conditional destinations", {
  extract <- function(flag, a, b, x) {
    declare(
      type(flag = logical(1)),
      type(a = double(n, n)),
      type(b = double(n)),
      type(x = double(n, n))
    )
    if (flag) {
      out <- solve(a, b)
    }
    out <- diag(x)
    sum(out)
  }
  construct <- function(flag, n, a, b, x) {
    declare(
      type(flag = logical(1)),
      type(n = integer(1)),
      type(a = double(n, n)),
      type(b = double(n, n)),
      type(x = double(n))
    )
    if (flag) {
      out <- a %*% b
    }
    out <- diag(x, nrow = n, ncol = n)
    sum(out)
  }

  a <- matrix(as.double(1:4), 2, 2)
  b <- as.double(1:2)
  x <- matrix(as.double(1:4), 2, 2)
  qextract <- quick(extract)
  expected <- sum(diag(x))
  expect_equal(qextract(FALSE, a, b, x), expected)
  expect_equal(qextract(TRUE, a, b, x), expected)

  qconstruct <- quick(construct)
  expected <- sum(diag(b, nrow = 2, ncol = 2))
  expect_equal(qconstruct(FALSE, 2L, a, a, b), expected)
  expect_equal(qconstruct(TRUE, 2L, a, a, b), expected)
})

test_that("outer point-allocates reused conditional destinations", {
  fn <- function(flag, a, b, x, y) {
    declare(
      type(flag = logical(1)),
      type(a = double(n, k)),
      type(b = double(k, p)),
      type(x = double(n)),
      type(y = double(p))
    )
    if (flag) {
      out <- a %*% b
    }
    out <- outer(x, y)
    sum(out)
  }

  a <- matrix(as.double(1:6), 2, 3)
  b <- matrix(as.double(1:6), 3, 2)
  x <- as.double(1:2)
  y <- as.double(3:4)
  qfn <- quick(fn)
  expected <- sum(outer(x, y))
  expect_equal(qfn(FALSE, a, b, x, y), expected)
  expect_equal(qfn(TRUE, a, b, x, y), expected)
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
    declare(type(a = double(n, n)), type(b = double(n)))
    solve(a, b)
  }
  forward_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n)))
    forwardsolve(a, b)
  }
  back_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n)))
    backsolve(a, b)
  }

  a <- matrix(numeric(), 0, 0)
  for (fn in list(solve_fn, forward_fn, back_fn)) {
    expect_error(
      quick(fn)(a, numeric()),
      "zero-sized outputs are not supported",
      fixed = TRUE
    )
  }
})

test_that("matrix solve right-hand sides reject zero output widths", {
  solve_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n, p)))
    sum(solve(a, b))
  }
  qr_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n, p)))
    sum(qr.solve(a, b))
  }
  forward_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n, p)))
    sum(forwardsolve(a, b))
  }
  back_fn <- function(a, b) {
    declare(type(a = double(n, n)), type(b = double(n, p)))
    sum(backsolve(a, b))
  }

  a <- diag(2)
  b <- matrix(as.double(1:4), 2, 2)
  empty_b <- matrix(numeric(), 2, 0)
  for (fn in list(solve_fn, qr_fn, forward_fn, back_fn)) {
    qfn <- quick(fn)
    expect_equal(qfn(a, b), fn(a, b), tolerance = 1e-10)
    expect_error(
      qfn(a, empty_b),
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
