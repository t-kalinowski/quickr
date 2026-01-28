# Coverage-oriented public API tests (exercise compiled calls via quick()).

test_that("C bridge size checks handle scalar size args (including dotted args)", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n)))
    sum(x)
  }
  qfn <- quick(fn)

  expect_identical(qfn(3L, as.double(1:3)), 6.0)
  expect_error(
    qfn(4L, as.double(1:3)),
    "length\\(x\\) must equal n, but are 3 and 4",
    fixed = FALSE
  )

  dotted <- function(foo.bar, x) {
    declare(type(foo.bar = integer(1)), type(x = double(foo.bar)))
    sum(x)
  }
  qdotted <- quick(dotted)

  expect_identical(qdotted(2L, c(0.25, 0.75)), 1.0)
  expect_error(
    qdotted(3L, c(0.25, 0.75)),
    "length\\(x\\) must equal foo\\.bar, but are 2 and 3",
    fixed = FALSE
  )
})

test_that("qr.solve supports explicit tol and errors on rank-deficient matrices", {
  fn_tol <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(n)))
    qr.solve(a, b, tol = 1e-7)
  }

  set.seed(1)
  a <- matrix(rnorm(50), nrow = 10)
  b <- rnorm(10)
  qtol <- quick(fn_tol)
  expect_equal(qtol(a, b), qr.solve(a, b, tol = 1e-7))

  fn_rd <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(n)))
    qr.solve(a, b)
  }
  qrd <- quick(fn_rd)

  a_rd <- cbind(1, 1, rnorm(10))
  b_rd <- rnorm(10)
  expect_error(
    qrd(a_rd, b_rd),
    "rank deficient matrix in qr.solve",
    fixed = TRUE
  )
})

test_that("svd() $u/$d/$v can be used to reconstruct a matrix", {
  fn <- function(x) {
    declare(type(x = double(m, n)))
    s <- svd(x)
    s$u %*% diag(s$d) %*% t(s$v)
  }

  set.seed(2)
  x <- matrix(rnorm(12), nrow = 4, ncol = 3)
  qfn <- quick(fn)
  expect_equal(qfn(x), x, tolerance = 1e-10)
})

test_that("local closure calls use keyword arguments when optional args are missing", {
  fn <- function(x) {
    declare(type(x = double(n)))

    f <- function(i, shift = NULL, scale = NULL) {
      if (is.null(shift)) {
        shift <- 1
      }
      if (is.null(scale)) {
        scale <- 2
      }
      (x[i] + shift) * scale
    }

    g <- function(i) f(i, scale = 3)
    sapply(seq_along(x), g)
  }

  set.seed(3)
  x <- runif(20)
  expect_quick_equal(fn, list(x))
})

test_that("subset designators support drop = FALSE for scalar indices", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    A[1, 2, drop = FALSE]
  }

  set.seed(4)
  A <- matrix(runif(20), nrow = 5)
  expect_quick_identical(fn, list(A))
})

test_that("indexing a length-1 vector with [1] is a no-op", {
  fn <- function(x) {
    declare(type(x = double(1L)))
    x[1] + 1
  }
  qfn <- quick(fn)
  expect_identical(qfn(2.5), 3.5)
})

test_that("subset superassignment works and rejects unsupported forms", {
  ok <- function(A) {
    declare(type(A = double(m, n)))

    f <- function() {
      A[1, 1] <<- 0
      NULL
    }

    f()
    A
  }

  set.seed(5)
  A <- matrix(runif(12), nrow = 4)
  qok <- quick(ok)
  out <- qok(A)
  expect_identical(out[1, 1], 0.0)

  bad_drop <- function(A) {
    declare(type(A = double(m, n)))
    f <- function() {
      A[1, 1, drop = FALSE] <<- A[1, 1, drop = FALSE]
    }
    f()
    A
  }
  expect_error(
    quick(bad_drop),
    "drop = FALSE not supported for superassignment",
    fixed = TRUE
  )

  bad_base <- function(A) {
    declare(type(A = double(m, n)))
    f <- function() {
      (A)[1, 1] <<- 0
    }
    f()
    A
  }
  expect_error(
    quick(bad_base),
    "only superassignment to x[...] is supported",
    fixed = TRUE
  )

  missing_target <- function(A) {
    declare(type(A = double(m, n)))
    f <- function() {
      B[1, 1] <<- 0
    }
    f()
    A
  }
  expect_error(
    quick(missing_target),
    "<<- targets must resolve to an existing variable",
    fixed = TRUE
  )
})

test_that("subset translation errors are user-facing", {
  rank_mismatch <- function(A) {
    declare(type(A = double(m, n)))
    A[1, 2, 3]
  }
  expect_error(
    quick(rank_mismatch),
    "number of args to x[...] must match the rank of x",
    fixed = TRUE
  )

  logical_superassign <- function(x) {
    declare(type(x = double(n)))
    f <- function() {
      x[x > 0] <<- 0
      NULL
    }
    f()
    x
  }
  expect_error(
    quick(logical_superassign),
    "logical subscript vectors are not supported for assignment",
    fixed = TRUE
  )

  bad_index_rank <- function(x, idx) {
    declare(type(x = double(n)), type(idx = integer(2L, 2L)))
    x[idx]
  }
  expect_error(
    quick(bad_index_rank),
    "all args to x[...] must be logical or integer of rank 0 or 1",
    fixed = TRUE
  )
})

test_that("svd() rejects unsupported arguments and access patterns", {
  missing_x <- function() {
    svd(NULL)$d
  }
  expect_error(
    quick(missing_x),
    "svd\\(\\) expects `x`",
    fixed = FALSE
  )

  bad_nu <- function(x) {
    declare(type(x = double(m, n)))
    svd(x, nu = 0)$d
  }
  expect_error(quick(bad_nu), "svd() does not support `nu` yet", fixed = TRUE)

  bad_nv <- function(x) {
    declare(type(x = double(m, n)))
    svd(x, nv = 0)$d
  }
  expect_error(quick(bad_nv), "svd() does not support `nv` yet", fixed = TRUE)

  bad_linpack <- function(x) {
    declare(type(x = double(m, n)))
    svd(x, LINPACK = TRUE)$d
  }
  expect_error(
    quick(bad_linpack),
    "svd() does not support LINPACK",
    fixed = TRUE
  )

  bad_field <- function(x) {
    declare(type(x = double(m, n)))
    svd(x)$foo
  }
  expect_error(
    quick(bad_field),
    "`$` only supports `.Machine$double.eps` and `svd()` results ($d, $u, $v)",
    fixed = TRUE
  )

  bare_call <- function(x) {
    declare(type(x = double(m, n)))
    svd(x)
    1
  }
  expect_error(
    quick(bare_call),
    "svd() must be assigned to a symbol or accessed via $d, $u, $v",
    fixed = TRUE
  )
})

test_that("svd() assignment cannot overwrite declared variables", {
  fn <- function(x) {
    declare(type(x = double(m, n)))
    declare(type(s = double(1L)))
    s <- svd(x)
    1
  }

  expect_error(
    quick(fn),
    "svd() result cannot overwrite declared variable: s",
    fixed = TRUE
  )
})

test_that("invalid size expressions produce helpful errors", {
  bad_length_literal <- function(x) {
    declare(type(x = double(length(1 + 2))))
    sum(x)
  }
  expect_error(
    quick(bad_length_literal),
    "length() size expressions must refer to a symbol",
    fixed = TRUE
  )

  bad_length_unknown <- function(x) {
    declare(type(x = double(length(y))))
    sum(x)
  }
  expect_error(
    quick(bad_length_unknown),
    "could not resolve size: y",
    fixed = TRUE
  )

  bad_dim_axis <- function(y, x) {
    declare(type(y = double(n)), type(x = double(dim(y)[1.5])))
    sum(x)
  }
  expect_error(
    quick(bad_dim_axis),
    "dim(x)[axis] requires integer axis",
    fixed = TRUE
  )

  bad_dim_rank <- function(y, x) {
    declare(type(y = double(n)), type(x = double(dim(y)[2])))
    sum(x)
  }
  expect_error(
    quick(bad_dim_rank),
    "insufficient rank of variable",
    fixed = TRUE
  )
})

test_that("size expressions support max() and integer division", {
  fn <- function(n, x) {
    declare(
      type(n = integer(1)),
      type(x = double(max(n %/% 2L, 2L, 3L)))
    )
    sum(x)
  }
  qfn <- quick(fn)

  expect_identical(qfn(8L, rep(1, 4)), 4.0)
  expect_error(qfn(8L, rep(1, 3)))
})

test_that("C bridge size expressions support min()", {
  fn <- function(n, x) {
    declare(
      type(n = integer(1)),
      type(x = double(min(n, 5L)))
    )
    sum(x)
  }

  qfn <- quick(fn)
  expect_identical(qfn(3L, as.double(1:3)), 6.0)
  expect_identical(qfn(10L, as.double(1:5)), 15.0)
})

test_that("vector ops accept mixed constant/symbol sizes", {
  fn <- function(n, x, y) {
    declare(
      type(n = integer(1)),
      type(x = double(3L)),
      type(y = double(n))
    )
    x + y
  }

  qfn <- quick(fn)
  expect_identical(
    qfn(3L, as.double(1:3), as.double(4:6)),
    as.double(c(5, 7, 9))
  )
})

test_that("size expressions support length() and modulo", {
  fn <- function(x) {
    declare(
      type(x = double(n)),
      type(out = double(length(x) %% 3L + 1L))
    )
    out <- double(length(x) %% 3L + 1L)
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  x <- runif(5)
  expect_quick_identical(fn, list(x))
})

test_that("symbolic size names reuse fortranized symbols", {
  fn <- function(foo.bar, x, y) {
    declare(
      type(foo.bar = integer(1)),
      type(x = double(foo.bar)),
      type(y = double(foo.bar))
    )
    x + y
  }

  qfn <- quick(fn)
  expect_identical(
    qfn(3L, as.double(1:3), as.double(4:6)),
    as.double(c(5, 7, 9))
  )
})

test_that("quick() returns named lists from compiled functions", {
  fn <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    sum <- a + b
    diff <- a - b
    list(sum = sum, diff = diff)
  }

  qfn <- quick(fn)
  expect_identical(qfn(2, 1), fn(2, 1))
})

test_that("triangular solve handles matrix right-hand sides", {
  fn <- function(U, B) {
    declare(type(U = double(n, n)), type(B = double(n, k)))
    backsolve(U, B)
  }

  set.seed(6)
  n <- 3
  k <- 2
  U <- matrix(rnorm(n * n), nrow = n)
  U[lower.tri(U)] <- 0
  B <- matrix(rnorm(n * k), nrow = n)

  qfn <- quick(fn)
  expect_equal(qfn(U, B), backsolve(U, B), tolerance = 1e-10)
})

test_that("inline local closure calls compile", {
  fn <- function(x) {
    declare(type(x = double(1)))
    (function(y) y + 1)(x)
  }

  expect_quick_identical(fn, list(2))
})

test_that("math intrinsics handle doubles with log10", {
  fn <- function(x) {
    declare(type(x = double(n)))
    out <- sin(x) + cos(x)
    out <- out + sqrt(x) + log(x) + exp(x)
    out <- out + floor(x) + ceiling(x)
    out <- out + log10(x)
    out
  }

  x <- seq(0.1, 2, length.out = 6)
  expect_quick_equal(fn, list(x))
})

test_that("math intrinsics handle complex log10 and components", {
  fn <- function(z) {
    declare(type(z = complex(n)))
    out <- Re(z) + Im(z) + Mod(z) + Arg(z) + abs(z)
    out <- out + Re(log10(z)) + Im(log10(z))
    out <- out + Re(Conj(z))
    out
  }

  z <- complex(
    real = seq(1, 6, length.out = 6),
    imaginary = seq(2, 7, length.out = 6)
  )
  qfn <- quick(fn)
  expect_equal(qfn(z), fn(z), tolerance = 1e-12)
})

test_that("reductions hoist logical masks and linear indexing works", {
  fn_mask <- function(x) {
    declare(type(x = double(n)))
    sum(x[x > 0])
  }
  x <- c(-1, 2, -3, 4)
  expect_quick_equal(fn_mask, list(x))

  fn_linear <- function(A) {
    declare(type(A = double(m, n)))
    A[3]
  }
  A <- matrix(as.double(1:6), nrow = 2)
  expect_quick_identical(fn_linear, list(A))
})

test_that("reductions reject distinct masks within one expression", {
  fn <- function(x, a, b) {
    declare(
      type(x = double(n)),
      type(a = logical(n)),
      type(b = logical(n))
    )
    sum(x[a] + x[b])
  }
  expect_error(
    quick(fn),
    "reduction expressions only support a single logical mask",
    fixed = TRUE
  )
})

test_that("cbind handles matrix, vector, and scalar inputs with symbolic sizes", {
  fn <- function(A, v, s) {
    declare(type(A = double(n, k)), type(v = double(n)), type(s = double(1)))
    cbind(A, v, s)
  }

  set.seed(7)
  A <- matrix(rnorm(6), nrow = 2)
  v <- rnorm(2)
  s <- 1.25
  qfn <- quick(fn)
  expect_equal(unname(qfn(A, v, s)), unname(cbind(A, v, s)))
})

test_that("cbind preserves logical inputs", {
  fn <- function(a, b) {
    declare(type(a = logical(n)), type(b = logical(n)))
    cbind(a, b)
  }

  a <- c(TRUE, FALSE, TRUE)
  b <- c(FALSE, TRUE, FALSE)
  qfn <- quick(fn)
  expect_identical(unname(qfn(a, b)), unname(cbind(a, b)))
})

test_that("rbind coerces logical inputs to integer mode", {
  fn <- function(x, lgl, s) {
    declare(type(x = integer(n)), type(lgl = logical(n)), type(s = integer(1)))
    rbind(x, lgl, s)
  }

  x <- as.integer(1:3)
  lgl <- c(TRUE, FALSE, TRUE)
  s <- 2L
  qfn <- quick(fn)
  expect_identical(unname(qfn(x, lgl, s)), unname(rbind(x, lgl, s)))
})

test_that("cbind supports dynamic column sizes with common row counts", {
  bad_cols <- function(x, y) {
    declare(type(x = double(n, NA)), type(y = double(n, NA)))
    cbind(x, y)
  }
  x <- matrix(as.double(1:6), nrow = 2)
  y <- matrix(as.double(7:12), nrow = 2)
  qcols <- quick(bad_cols)
  expect_identical(unname(qcols(x, y)), unname(cbind(x, y)))

  bad_rows <- function(x, y) {
    declare(type(x = double(NA, n)), type(y = double(NA, n)))
    cbind(x, y)
  }
  expect_error(
    quick(bad_rows),
    "cbind() requires inputs with a common row count",
    fixed = TRUE
  )
})

test_that("crossprod supports two-argument form", {
  fn <- function(x, y) {
    declare(type(x = double(n, k)), type(y = double(n, m)))
    crossprod(x, y)
  }

  set.seed(8)
  x <- matrix(rnorm(6), nrow = 3)
  y <- matrix(rnorm(9), nrow = 3)
  qfn <- quick(fn)
  expect_equal(qfn(x, y), crossprod(x, y), tolerance = 1e-12)
})

test_that("crossprod errors on non-conformable arguments", {
  fn <- function(x, y) {
    declare(type(x = double(2L, 3L)), type(y = double(4L, 5L)))
    crossprod(x, y)
  }

  expect_error(
    quick(fn),
    "non-conformable arguments in crossprod",
    fixed = TRUE
  )
})

test_that("matmul assignment infers destination shape", {
  fn <- function(A, B) {
    declare(type(A = double(n, k)), type(B = double(k, m)))
    out <- (A %*% B)
    out
  }

  set.seed(9)
  A <- matrix(rnorm(6), nrow = 2)
  B <- matrix(rnorm(6), nrow = 3)
  qfn <- quick(fn)
  expect_equal(qfn(A, B), A %*% B, tolerance = 1e-12)
})

test_that("double literals preserve precision in compiled code", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x + 1.23456789
  }

  qfn <- quick(fn)
  expect_equal(qfn(0), 1.23456789, tolerance = 1e-12)
})

test_that("character constructor is not implemented", {
  fn <- function() {
    character(1)
  }

  expect_error(quick(fn), "not implemented yet", fixed = TRUE)
})

test_that("raw constructor is not implemented", {
  fn <- function() {
    raw(1)
  }

  expect_error(quick(fn), "not implemented yet", fixed = TRUE)
})

test_that("cbind errors for complex mixed with non-complex", {
  fn <- function(x, y) {
    declare(type(x = complex(n)), type(y = double(n)))
    cbind(x, y)
  }

  expect_error(
    quick(fn),
    "cbind() does not support mixing complex with other types",
    fixed = TRUE
  )
})

test_that("cbind requires known and common row sizes", {
  mismatched_rows <- function(x, y) {
    declare(type(x = double(n)), type(y = double(m)))
    cbind(x, y)
  }
  expect_error(
    quick(mismatched_rows),
    "cbind() requires inputs with a common row count",
    fixed = TRUE
  )
})

test_that("rbind requires common column sizes", {
  mismatched_cols <- function(x, y) {
    declare(type(x = double(n)), type(y = double(m)))
    rbind(x, y)
  }
  expect_error(
    quick(mismatched_cols),
    "rbind() requires inputs with a common column count",
    fixed = TRUE
  )
})

test_that("cbind supports symbolic size expressions", {
  fn <- function(n, x, y) {
    declare(
      type(n = integer(1)),
      type(x = double(n + 1L)),
      type(y = double(n + 1L))
    )
    cbind(x, y)
  }

  qfn <- quick(fn)
  x <- as.double(1:4)
  y <- as.double(5:8)
  expect_equal(unname(qfn(3L, x, y)), unname(cbind(x, y)))
})

test_that("tcrossprod supports single-argument form", {
  fn <- function(x) {
    declare(type(x = double(n, k)))
    tcrossprod(x)
  }

  set.seed(10)
  x <- matrix(rnorm(12), nrow = 3)
  qfn <- quick(fn)
  expect_equal(qfn(x), tcrossprod(x), tolerance = 1e-12)
})

test_that("svd() $u and $v can be accessed inline", {
  fn <- function(x) {
    declare(type(x = double(m, n)))
    sum(svd(x)$u) + sum(svd(x)$v)
  }

  set.seed(11)
  x <- matrix(rnorm(12), nrow = 4)
  qfn <- quick(fn)
  expect_equal(qfn(x), fn(x), tolerance = 1e-10)
})

test_that("crossprod assignment infers destination shape", {
  fn <- function(x) {
    declare(type(x = double(n, k)))
    out <- crossprod(x)
    out
  }

  set.seed(12)
  x <- matrix(rnorm(12), nrow = 3)
  qfn <- quick(fn)
  expect_equal(qfn(x), crossprod(x), tolerance = 1e-12)
})

test_that("local closure calls can be assigned", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i) x[i] + 1
    y <- f(2L)
    y
  }

  x <- as.double(1:5)
  expect_quick_equal(fn, list(x))
})

test_that("local closure calls returning NULL cannot be assigned", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i) {
      x[i] <- x[i] + 1
      NULL
    }
    y <- f(1L)
    y
  }

  expect_error(
    quick(fn),
    "local closure calls that return `NULL` cannot be assigned",
    fixed = TRUE
  )
})

test_that("optional args must be initialized before use", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i, shift = NULL) x[i] + shift
    f(1L)
  }

  expect_error(
    quick(fn),
    "optional argument\\(s\\) used without initializing",
    fixed = FALSE
  )
})

test_that("optional args can be initialized via is.null branches", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i, shift = NULL) {
      if (is.null(shift)) {
        shift <- 2
      }
      x[i] + shift
    }
    f(1L)
  }

  x <- as.double(1:3)
  expect_quick_equal(fn, list(x))
})

test_that("optional args can be assigned before use", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i, shift = NULL) {
      shift <- 3
      x[i] + shift
    }
    f(1L)
  }

  x <- as.double(1:3)
  expect_quick_equal(fn, list(x))
})

test_that("optional args can be initialized in else branches", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i, shift = NULL) {
      if (!is.null(shift)) {
        shift <- shift + 1
      } else {
        shift <- 1
      }
      x[i] + shift
    }
    f(1L)
  }

  x <- as.double(1:3)
  expect_quick_equal(fn, list(x))
})

test_that("local subset assignment shadows host variables", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function() {
      x[1] <- x[1] + 1
      NULL
    }
    f()
    x
  }

  x <- as.double(1:3)
  expect_quick_identical(fn, list(x))
})

test_that("subset superassignment with linear indexing updates host", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    f <- function() {
      A[3] <<- 0
      NULL
    }
    f()
    A
  }

  A <- matrix(as.double(1:6), nrow = 2)
  qfn <- quick(fn)
  out <- qfn(A)
  expect_identical(out[3], 0.0)
})

test_that("subset superassignment supports scalar indices", {
  fn <- function(s) {
    declare(type(s = double(1L)))
    f <- function() {
      s[1] <<- s[1] + 1
      NULL
    }
    f()
    s
  }

  qfn <- quick(fn)
  expect_identical(qfn(2), 3.0)
})

test_that("subset superassignment errors outside local closures", {
  fn <- function(x) {
    declare(type(x = double(n)))
    x[1] <<- 0
    x
  }
  expect_error(
    quick(fn),
    "<<- is only supported inside local closures",
    fixed = TRUE
  )
})

test_that("elementwise vector-matrix ops reshape vectors", {
  fn_left <- function(x, A) {
    declare(type(x = double(n)), type(A = double(n, k)))
    x + A
  }
  fn_right <- function(A, x) {
    declare(type(A = double(n, k)), type(x = double(n)))
    A + x
  }

  set.seed(13)
  A <- matrix(rnorm(6), nrow = 2)
  x <- rnorm(2)
  ql <- quick(fn_left)
  qr <- quick(fn_right)
  expect_equal(ql(x, A), x + A)
  expect_equal(qr(A, x), A + x)
})

test_that("elementwise vector lengths must recycle cleanly", {
  fn <- function(x, y) {
    declare(type(x = double(3L)), type(y = double(2L)))
    x + y
  }

  expect_error(
    quick(fn),
    "elementwise vector operations require lengths that recycle cleanly",
    fixed = TRUE
  )
})

test_that("recycling uses fortranized size expressions", {
  fn <- function(foo.bar, x, y) {
    declare(
      type(foo.bar = integer(1)),
      type(x = double(foo.bar + 1L)),
      type(y = double(foo.bar + 1L))
    )
    x + y
  }

  qfn <- quick(fn)
  x <- as.double(1:4)
  y <- as.double(5:8)
  expect_equal(qfn(3L, x, y), x + y)
})

test_that("mask hoisting accepts repeated masks", {
  same_mask <- function(x) {
    declare(type(x = double(n)))
    sum(x[x > 0] + x[x > 0])
  }

  x <- c(-1, 2, -3, 4)
  expect_quick_equal(same_mask, list(x))
})

test_that("local closures enforce unique fortranized argument names", {
  fn <- function(x) {
    declare(type(x = double(1)))
    f <- function(foo.bar, foo_bar) foo.bar + foo_bar
    f(foo.bar = x, foo_bar = x)
  }
  expect_error(
    quick(fn),
    "Fortran is case-insensitive; these names conflict when mapped",
    fixed = TRUE
  )
})

test_that("local closure calls error on missing required arguments", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i, j) x[i] + j
    f(i = 1L)
  }
  expect_error(
    quick(fn),
    "call is missing required argument",
    fixed = TRUE
  )
})

test_that("local subset assignment errors on unknown symbols", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function() {
      y[1] <- 1
      NULL
    }
    f()
    x
  }
  expect_error(
    quick(fn),
    "could not resolve symbol: y",
    fixed = TRUE
  )
})

test_that("superassignment errors on rank mismatches", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    f <- function() {
      A[1, 2, 3] <<- 0
      NULL
    }
    f()
    A
  }
  expect_error(
    quick(fn),
    "number of args to x\\[\\.\\.\\.\\] must match the rank of x",
    fixed = FALSE
  )
})

test_that("local closure calls can be used for side effects", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i) {
      x[i] <- x[i] + 1
      NULL
    }
    f(1L)
    x
  }

  x <- as.double(1:3)
  qfn <- quick(fn)
  expect_identical(qfn(x), x)
})

test_that("local subset assignment supports logical subscripts", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    A[c(TRUE, FALSE), ] <- A[c(TRUE, FALSE), ]
    A
  }

  A <- matrix(as.double(1:6), nrow = 2)
  expect_quick_identical(fn, list(A))
})

test_that("r2f debug mode compiles successfully", {
  fn <- function(x) {
    declare(type(x = double(n)))
    x + 1
  }
  withr::local_options(list(quickr.r2f.debug = TRUE))
  qfn <- quick(fn)
  expect_equal(qfn(as.double(1:3)), as.double(2:4))
})

test_that("referencing undeclared arguments errors with helpful message", {
  fn <- function(x) {
    y <- x + 1
    y
  }

  expect_error(
    quick(fn),
    "arg not declared",
    fixed = TRUE
  )
})

test_that("quick() runs when DEVTOOLS_LOAD is set and pkgload is available", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x + 1
  }

  withr::local_envvar(c(DEVTOOLS_LOAD = "quickr"))
  qfn <- quick(fn)
  expect_identical(qfn(1), 2.0)
})

test_that("unary intrinsic handlers run for complex inputs", {
  fn <- function(z) {
    declare(type(z = complex(n)))
    Arg(Conj(z)) + Mod(z)
  }

  z <- complex(real = c(1, -2, 0.5), imaginary = c(0.5, -1, 2))
  qfn <- quick(fn)
  expect_equal(qfn(z), Arg(Conj(z)) + Mod(z))
})

test_that("cbind casts logical to integer when integer inputs are present", {
  fn <- function(x, y) {
    declare(type(x = integer(n)), type(y = logical(n)))
    cbind(x, y)
  }

  x <- 1:3
  y <- c(TRUE, FALSE, TRUE)
  qfn <- quick(fn)
  actual <- qfn(x, y)
  expected <- cbind(x, y)
  dimnames(actual) <- NULL
  dimnames(expected) <- NULL
  expect_identical(actual, expected)
})

test_that("local closure optional args initialize before use", {
  fn <- function(x) {
    declare(type(x = double(n)))
    f <- function(i, shift = NULL) {
      if (is.null(shift)) {
        shift <- 1.5
      }
      x[i] + shift
    }
    f(1L)
  }

  set.seed(10)
  x <- runif(6)
  expect_quick_equal(fn, list(x))
})

test_that("local closure optional args error when used without init", {
  bad <- function(x) {
    declare(type(x = double(n)))
    f <- function(i, shift = NULL) {
      if (!is.null(shift)) {
        shift <- shift + 1
      }
      x[i] + shift
    }
    f(1L)
  }

  expect_error(
    quick(bad),
    "optional argument\\(s\\) used without initializing",
    fixed = FALSE
  )
})

test_that("local closure call assignment handles target reads", {
  fn <- function(x) {
    declare(type(x = double(n)), type(out = double(1)))
    f <- function(i, add = NULL) {
      if (is.null(add)) {
        add <- 0
      }
      x[i] + add
    }
    out <- 0
    out <- f(1L, out)
    out
  }

  set.seed(11)
  x <- runif(4)
  qfn <- quick(fn)
  expect_equal(qfn(x), fn(x))
})

test_that("sapply assignment supports simplify = TRUE and output reuse", {
  fn <- function(x) {
    declare(type(x = double(n)), type(out = double(n)))
    out <- x
    out <- sapply(((seq_along(x))), function(i) out[i] + x[i], simplify = TRUE)
    out
  }

  set.seed(12)
  x <- runif(5)
  expect_quick_equal(fn, list(x))
})

test_that("sapply over matrix values uses linear indexing", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    out <- sapply(A, function(v) v * 2)
    out
  }

  set.seed(13)
  A <- matrix(runif(6), nrow = 2)
  expect_quick_equal(fn, list(A))
})

test_that("svd supports $u and $v directly", {
  fn_u <- function(x) {
    declare(type(x = double(3, 2)))
    svd(x)$u
  }
  fn_v <- function(x) {
    declare(type(x = double(3, 2)))
    svd(x)$v
  }

  x <- matrix(c(3, 0, 0, 0, 2, 0), nrow = 3)
  qfn_u <- quick(fn_u)
  qfn_v <- quick(fn_v)
  expect_equal(qfn_u(x), svd(x)$u, tolerance = 1e-12)
  expect_equal(qfn_v(x), svd(x)$v, tolerance = 1e-12)
})

test_that("quick errors when DEVTOOLS_LOAD is set without pkgload", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x + 1
  }

  withr::local_envvar(DEVTOOLS_LOAD = "quickr")
  expect_error(
    testthat::with_mocked_bindings(
      quick(fn),
      requireNamespace = function(...) FALSE,
      .package = "base"
    ),
    "Please install 'pkgload'",
    fixed = TRUE
  )
})

test_that("unary intrinsics exercise handler registration paths", {
  fn <- function(x) {
    declare(type(x = double(n)))
    sin(x) + cos(x)
  }

  x <- c(0, 0.5, 1)
  expect_quick_equal(fn, list(x))
})

test_that("cbind rejects complex mixed with real", {
  fn <- function(x, y) {
    declare(type(x = complex(n)), type(y = double(n)))
    cbind(x, y)
  }

  expect_error(
    quick(fn),
    "mixing complex with other types",
    fixed = TRUE
  )
})

test_that("cbind errors on unsupported coercion to logical", {
  fn <- function(x, y) {
    declare(type(x = raw(n)), type(y = logical(n)))
    cbind(x, y)
  }

  expect_error(
    quick(fn),
    "does not support coercion",
    fixed = TRUE
  )
})

test_that("cbind combines symbolic column sizes", {
  fn <- function(x, y, n, m) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n, 2L)),
      type(y = double(n, m))
    )
    cbind(x, y)
  }

  x <- matrix(1, nrow = 3, ncol = 2)
  y <- matrix(2, nrow = 3, ncol = 4)
  out <- quick(fn)(x, y, 3L, 4L)
  expect_identical(dim(out), c(3L, 6L))
})

test_that("cbind requires common row counts", {
  fn <- function(x, y) {
    declare(type(x = double(2L, 2L)), type(y = double(3L, 1L)))
    cbind(x, y)
  }

  expect_error(
    quick(fn),
    "requires inputs with a common row count",
    fixed = TRUE
  )
})

test_that("svd rejects unsupported field access", {
  fn <- function(x) {
    declare(type(x = double(3L, 2L)))
    svd(x)$foo
  }

  x <- matrix(c(3, 0, 0, 0, 2, 0), nrow = 3)
  expect_error(
    quick(fn)(x),
    "`$` only supports `.Machine$double.eps` and `svd()` results ($d, $u, $v)",
    fixed = TRUE
  )
})

test_that("svd returns a named list", {
  fn <- function(x) {
    declare(type(x = double(3L, 2L)))
    s <- svd(x)
    s
  }

  x <- matrix(c(3, 0, 0, 0, 2, 0), nrow = 3)
  out <- quick(fn)(x)
  expect_named(out, c("d", "u", "v"))
  expect_equal(out$d, svd(x)$d)
  recon <- out$u %*% diag(out$d, nrow = length(out$d)) %*% t(out$v)
  expect_equal(recon, x, tolerance = 1e-8)
})
