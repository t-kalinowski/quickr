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

test_that("guarded OpenMP iterations stop when cancellation is disabled", {
  withr::local_envvar(c(
    OMP_CANCELLATION = "false",
    OMP_NUM_THREADS = "1",
    OMP_THREAD_LIMIT = "1",
    OMP_DYNAMIC = "false"
  ))
  skip_if_no_openmp()

  guarded <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    out <- 0
    declare(parallel())
    for (i in seq_len(1L)) {
      out <- sum(x + y)
      out <- out + 1
    }
    out
  }

  code <- as.character(r2f(guarded))
  guard <- regexpr("elementwise vector operations", code, fixed = TRUE)
  cycle <- regexpr("cycle", code, fixed = TRUE)
  later_statement <- regexpr(
    "out = (out + 1.0_c_double)",
    code,
    fixed = TRUE
  )
  expect_true(all(c(guard, cycle, later_statement) > 0L))
  expect_lt(guard, cycle)
  expect_lt(cycle, later_statement)

  qguarded <- quick(guarded)
  expect_error(
    qguarded(c(1, 2), c(1, 2, 3)),
    "elementwise vector operations"
  )
})

test_that("nested guarded loops cycle the OpenMP worksharing loop", {
  guarded <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    for (i in seq_len(1L)) {
      while (TRUE) {
        stop("boom")
      }
    }
    x
  }

  code <- as.character(r2f(guarded))
  expect_match(code, "cycle quickr_omp_loop", fixed = TRUE)
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

  # A bare atomic mode symbol is a form error (dims are missing), not a
  # mode error; the old check fired "only atomic modes are supported"
  # precisely when the mode *was* atomic.
  missing_dims <- function(x) {
    declare(type(x = double))
    x
  }
  expect_error(
    quick(missing_dims),
    "the mode must be a call with dimensions, as in: type(x = double(<dims>))",
    fixed = TRUE
  )

  bad_mode_call <- function(x) {
    declare(type(x = foo(1)))
    x
  }
  expect_error(
    quick(bad_mode_call),
    "only atomic modes are supported, not: foo",
    fixed = TRUE
  )

  bad_mode_symbol <- function(x) {
    declare(type(x = foo))
    x
  }
  expect_error(
    quick(bad_mode_symbol),
    "only atomic modes are supported, not: foo",
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
