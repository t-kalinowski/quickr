test_that("captured BLAS operands keep their stored extents under shadowing", {
  for (result in list(
    quote(sum(x %*% x)),
    quote({
      z <- x %*% x
      sum(z)
    }),
    quote(sum((x %*% x) %*% x)),
    quote(sum((x + 1) %*% x))
  )) {
    fn <- eval(substitute(
      function(n, x) {
        declare(type(n = integer(1)), type(x = double(n, n)))
        f <- function(n) RESULT
        f(1L)
      },
      list(RESULT = result)
    ))

    expect_quick_identical(fn, list(2L, matrix(1, 2, 2)))
  }
})

test_that("closure array results retain captured BLAS extents", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    f <- function(n) {
      x %*% x
    }
    f(1L)
  }

  expect_quick_identical(fn, list(2L, matrix(1, 2, 2)))
})

test_that("equal dimension names across scopes do not make BLAS operands conform", {
  fn <- function(n, x, local_n) {
    declare(
      type(n = integer(1)),
      type(x = double(n, n)),
      type(local_n = integer(1))
    )
    f <- function(n) {
      declare(type(y = double(n, n)))
      y <- matrix(1, n, n)
      sum(x %*% y)
    }
    f(local_n)
  }

  expect_quick_identical(fn, list(3L, matrix(1, 3, 3), 3L))
  qfn <- quick(fn)
  expect_error(
    qfn(3L, matrix(1, 3, 3), 2L),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})

test_that("BLAS does not reuse a destination with shadowed dimensions", {
  fn <- function(n, x, local_n) {
    declare(
      type(n = integer(1)),
      type(x = double(n, n)),
      type(local_n = integer(1))
    )
    f <- function(n) {
      declare(type(out = double(n, n)))
      out <- x %*% x
      sum(out)
    }
    f(local_n)
  }

  expect_quick_identical(fn, list(3L, matrix(1, 3, 3), 3L))
  qfn <- quick(fn)
  expect_error(
    qfn(3L, matrix(1, 3, 3), 2L),
    "reassignment must preserve the shape of `out`",
    fixed = TRUE
  )
})

test_that("ifelse checks stored dimensions across scopes", {
  fn <- function(n, m, mask, local_n, local_m) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(mask = logical(n, m)),
      type(local_n = integer(1)),
      type(local_m = integer(1))
    )
    f <- function(n, m) {
      declare(type(y = double(n, m)))
      y <- matrix(1, n, m)
      sum(ifelse(mask, y, 0))
    }
    f(local_n, local_m)
  }

  mask <- matrix(c(TRUE, FALSE), 2, 3)
  expect_quick_identical(fn, list(2L, 3L, mask, 2L, 3L))
  qfn <- quick(fn)
  expect_error(
    qfn(2L, 3L, mask, 3L, 2L),
    "must be scalars or match the shape of `test`",
    fixed = TRUE
  )
})

test_that("fill lengths use captured arrays' stored extents", {
  fn <- function(n, m, x, y, local_m) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m)),
      type(local_m = integer(1))
    )
    f <- function(m) {
      x <<- numeric(length(y))
      NULL
    }
    f(local_m)
    x
  }

  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4), 1L))
  qfn <- quick(fn)
  expect_error(
    qfn(2L, 3L, c(1, 2), c(3, 4, 5), 2L),
    "reassignment must preserve the shape of `x`",
    fixed = TRUE
  )
})

test_that("captured matrix dimension queries keep their storage binding", {
  for (query in list(
    quote(array(1, dim(x))),
    quote(array(1, c(dim(x)[1L], dim(x)[2L]))),
    quote(matrix(1, nrow(x), ncol(x)))
  )) {
    fn <- eval(substitute(
      function(n, m, x) {
        declare(
          type(n = integer(1)),
          type(m = integer(1)),
          type(x = double(n, m))
        )
        f <- function(n, m) sum(QUERY)
        f(1L, 1L)
      },
      list(QUERY = query)
    ))
    expect_quick_identical(fn, list(2L, 3L, matrix(1, 2, 3)))
  }
})

test_that("captured binds retain common declared dimensions", {
  fn <- function(n, x, y) {
    declare(
      type(n = integer(1)),
      type(x = double(n, n)),
      type(y = double(n, n))
    )
    f <- function(n) sum(cbind(x, y)) + sum(rbind(x, y))
    f(1L)
  }
  expect_quick_identical(fn, list(2L, matrix(1, 2, 2), matrix(2, 2, 2)))
})

test_that("closure results retain caller declaration dimensions", {
  local_source <- function(n) {
    declare(type(n = integer(1)))
    x <- matrix(1, n, n)
    f <- function(n) x %*% x
    out <- f(1L)
    out
  }
  declared_result <- function(n, x) {
    declare(
      type(n = integer(1)),
      type(x = double(n, n)),
      type(out = double(n, n))
    )
    f <- function(n) x %*% x
    out <- f(1L)
    out
  }
  expect_quick_identical(local_source, 2L)
  expect_quick_identical(declared_result, list(2L, matrix(1, 2, 2)))
})

test_that("captured vector transposes render stored extents", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n)))
    f <- function(n) sum(t(x)) + sum(t(x) %*% x)
    f(1L)
  }
  expect_quick_identical(fn, list(3L, c(2, 3, 4)))
})

test_that("random constructor lengths use captured extents exactly once", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n)))
    f <- function(n) sum(runif(length(x)))
    f(1L)
  }
  qfn <- quick(fn)
  withr::local_seed(184)
  expected <- fn(3L, c(2, 3, 4))
  expected_seed <- .Random.seed
  set.seed(184)
  expect_identical(qfn(3L, c(2, 3, 4)), expected)
  expect_identical(.Random.seed, expected_seed)
})

test_that("array formals keep the supplied array dimensions", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    f <- function(a, n) sum(a %*% a)
    f(x, 1L)
  }
  expect_quick_identical(fn, list(2L, matrix(1, 2, 2)))
})

test_that("optional arrays diagnose shadowed inherited dimensions", {
  supplied_shadowed <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    f <- function(a = NULL, n) {
      if (is.null(a)) {
        a <- x
      }
      sum(a %*% a)
    }
    f(x, 1L)
  }
  local_shadowed <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    f <- function(a = NULL) {
      if (is.null(a)) {
        a <- x
      }
      n <- 1L
      sum(a %*% a)
    }
    f(x)
  }
  dotted_shadowed <- function(n.rows) {
    declare(type(n.rows = integer(1)))
    x <- matrix(1, n.rows, n.rows)
    f <- function(a = NULL, n.rows) {
      if (is.null(a)) {
        a <- matrix(1, n.rows, n.rows)
      }
      sum(a %*% a)
    }
    f(x, 1L)
  }
  supplied <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    f <- function(a = NULL) {
      if (is.null(a)) {
        a <- x
      }
      z <- a %*% a
      sum(z) + n
    }
    f(x)
  }
  omitted <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    f <- function(a = NULL, n) {
      if (is.null(a)) {
        a <- x
      }
      z <- a %*% a
      sum(z)
    }
    f(n = 1L)
  }
  diagnostic <- "optional array argument `a` has dimensions shadowed by closure bindings: n"
  expect_error(quick(supplied_shadowed), diagnostic, fixed = TRUE)
  expect_error(quick(local_shadowed), diagnostic, fixed = TRUE)
  expect_error(
    quick(dotted_shadowed),
    paste0(diagnostic, "_rows"),
    fixed = TRUE
  )
  expect_quick_identical(supplied, list(2L, matrix(1, 2, 2)))
  expect_quick_identical(omitted, list(2L, matrix(1, 2, 2)))
})
