skip_on_cran()

test_that("case-sensitive variable name clashes", {
  expect_snapshot(
    quick(function(j) {
      declare(type(j = integer(1)))
      J <- double(length = j)
      J
    }),
    error = TRUE
  )
})

test_that("non-final expressions must be assigned", {
  expect_error(regexp = "all expressions except the final return", {
    quick(function(x) {
      declare(type(x = double(1)))
      x + 1
      x
    })
  })

  expect_error(regexp = "all expressions except the final return", {
    quick(function(x) {
      declare(type(x = double(1)))
      x
      x
    })
  })

  expect_error(regexp = "all expressions except the final return", {
    quick(function(x) {
      declare(type(x = double(1)))
      x <- x + 1
      x + 1
      x
    })
  })
})

test_that("value-returning local closures can be called as statements", {
  fn <- function(x) {
    declare(type(x = double(1)))

    apply_boundary_conditions <- function() {
      x <<- x + 1
    }

    apply_boundary_conditions()
    x
  }

  # r2f(fn)

  expect_quick_identical(fn, list(1))

  fn <- function(x) {
    declare(type(x = double(1)))

    apply_boundary_conditions <- function() {
      x <<- x + 1
      x
    }

    apply_boundary_conditions()
    x
  }

  # r2f(fn)

  expect_quick_identical(fn, list(1))
})

test_that("reserved or underscored names are rejected", {
  expect_snapshot(
    quick(function(x) {
      `_bad` <- x + 1L
      `_bad`
    }),
    error = TRUE
  )

  expect_snapshot(
    quick(function(x) {
      `bad_` <- x + 1L
      `bad_`
    }),
    error = TRUE
  )

  expect_snapshot(
    quick(function(int) {
      int
    }),
    error = TRUE
  )

  expect_snapshot(
    quick(function(`foo.bar`, foo_bar) {
      1
    }),
    error = TRUE
  )
})

test_that("closure return mode must match output mode", {
  expect_error(
    quick(function(x) {
      declare(type(x = double(1)))
      out <- integer(1)
      compute <- function() x + 1
      out <- compute()
      out
    }),
    "closure result mode.*does not match output mode"
  )
})

test_that("closure must return scalar for scalar outputs", {
  expect_error(
    quick(function(x) {
      declare(type(x = double(3)))
      s <- 0
      compute <- function() x
      s <- compute()
      s
    }),
    "closure must return a scalar"
  )
})

test_that("missing argument declarations get a clear error", {
  expect_error(
    quick(function(x) {
      x + 1
    }),
    "arg not declared: x"
  )
})

test_that("declare() type() calls validate syntax", {
  too_many <- function(x, y) {
    declare(type(x = double(1), y = double(1)))
    x + y
  }
  expect_error(
    quick(too_many),
    "only one variable can be declared per type\\(\\) call",
    fixed = FALSE
  )

  missing_name <- function(x) {
    declare(type(double(1)))
    x
  }
  expect_error(
    quick(missing_name),
    "name must be provided as:",
    fixed = TRUE
  )

  bad_mode <- function(x) {
    declare(type(x = double))
    x
  }
  expect_error(
    quick(bad_mode),
    "only atomic modes are supported",
    fixed = TRUE
  )
})

test_that("reductions reject named arguments like na.rm", {
  for (reducer in c("max", "min", "sum", "prod")) {
    fn <- eval(bquote(function(x) {
      declare(type(x = double(NA)))
      out <- .(as.name(reducer))(x, na.rm = TRUE)
      out
    }))
    expect_error(
      quick(fn),
      "do not support named arguments",
      fixed = TRUE
    )
  }
})


test_that("assigning an expression that produces no value errors cleanly", {
  fn <- function(x) {
    declare(type(x = logical(1)))
    y <- if (x) 1 else 2
    y
  }
  expect_error(
    quick(fn),
    "cannot assign `if (x) 1 else 2`: expression does not produce a value",
    fixed = TRUE
  )
})

test_that("unsupported complex operations are refused with R's messages", {
  # Order comparisons on complex values: R errors, so must quickr -- with
  # a clean message, not a raw gfortran failure.
  complex_lt <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x < y
  }
  expect_error(quick(complex_lt), "invalid comparison with complex values")

  # Equality is supported, as in R.
  complex_eq <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x == y
  }
  expect_quick_identical(complex_eq, list(1i, 1i))
  expect_quick_identical(complex_eq, list(1i, 2i))

  # modulo() has no complex form in Fortran; R refuses too.
  complex_mod <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x %% y
  }
  expect_error(quick(complex_mod), "unimplemented complex operation")
})

test_that("complex operands are refused in linear algebra", {
  # The real BLAS/LAPACK lowerings (dgemm, dgesv, ...) would read complex
  # storage as reals and return a plausible wrong answer where R returns a
  # complex result: complex(2) %*% complex(2) returned a real dot product
  # of the real parts. Refuse at compile time instead.
  complex_matmul <- function(x, y) {
    declare(type(x = complex(2)), type(y = complex(2)))
    x %*% y
  }
  expect_error(
    quick(complex_matmul),
    "%*% does not support complex operands",
    fixed = TRUE
  )

  # One complex operand is enough to poison the d* routine.
  complex_mixed <- function(x, y) {
    declare(type(x = complex(2, 2)), type(y = double(2, 2)))
    x %*% y
  }
  expect_error(quick(complex_mixed), "does not support complex operands")

  complex_solve <- function(x) {
    declare(type(x = complex(2, 2)))
    solve(x)
  }
  expect_error(quick(complex_solve), "does not support complex operands")

  complex_crossprod <- function(x) {
    declare(type(x = complex(2, 2)))
    crossprod(x)
  }
  expect_error(quick(complex_crossprod), "does not support complex operands")

  # t() alone is mode-preserving and keeps working on complex values.
  complex_t <- function(x) {
    declare(type(x = complex(2, 2)))
    t(x)
  }
  expect_quick_identical(
    complex_t,
    list(matrix(c(1 + 1i, 2 + 0i, 3 - 1i, 4 + 2i), 2, 2))
  )
})
