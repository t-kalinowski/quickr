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
    "only supports `.Machine$double.eps` and `svd()` results",
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
