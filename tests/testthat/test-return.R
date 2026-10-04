# return() in quick() functions and local closures: trailing returns,
# early returns from branches and loops, and the rules every return site
# must follow (same type and shape as the other return values).

skip_on_cran()

test_that("a trailing return() works like a final value", {
  sym <- function(x) {
    declare(type(x = double(NA)))
    y <- x + 1
    return(y)
  }
  expect_quick_identical(sym, list(c(1, 2, 3)))

  expr <- function(x) {
    declare(type(x = double(NA)))
    return(x * 2)
  }
  expect_quick_identical(expr, list(c(1, 2, 3)))

  lst <- function(x) {
    declare(type(x = integer(n)))
    y <- x + 1L
    z <- x * 2L
    return(list(y = y, z = z))
  }
  expect_quick_identical(lst, list(1:3))
})

test_that("early return() from an if() guard", {
  fn <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      return(0)
    }
    sqrt(x)
  }
  expect_quick_identical(fn, list(-4), list(9), list(0))
})

test_that("if/else chains ending in return() on every path", {
  sign_of <- function(x) {
    declare(type(x = double(1)))
    if (x > 0) {
      return(1L)
    } else if (x < 0) {
      return(-1L)
    } else {
      return(0L)
    }
  }
  expect_quick_identical(sign_of, list(3), list(-2), list(0))

  abs_val <- function(x) {
    declare(type(x = double(1)))
    if (x > 0) return(x) else return(-x)
  }
  expect_quick_identical(abs_val, list(2.5), list(-2.5))
})

test_that("early return() from loops", {
  first_above <- function(x, t) {
    declare(type(x = double(NA)), type(t = double(1)))
    for (i in seq_along(x)) {
      if (x[i] > t) {
        return(i)
      }
    }
    0L
  }
  expect_quick_identical(
    first_above,
    list(c(1, 5, 9), 4),
    list(c(1, 2), 4)
  )

  collatz_steps <- function(n) {
    declare(type(n = integer(1)))
    steps <- 0L
    repeat {
      if (n == 1L) {
        return(steps)
      }
      if (n %% 2L == 0L) {
        n <- n %/% 2L
      } else {
        n <- 3L * n + 1L
      }
      steps <- steps + 1L
    }
    steps
  }
  expect_quick_identical(collatz_steps, list(1L), list(6L), list(27L))

  countdown <- function(n) {
    declare(type(n = integer(1)))
    while (n > 0L) {
      if (n == 3L) {
        return(n * 10L)
      }
      n <- n - 1L
    }
    n
  }
  expect_quick_identical(countdown, list(5L), list(2L))

  find_pair <- function(x, target) {
    declare(type(x = integer(NA)), type(target = integer(1)))
    for (i in seq_along(x)) {
      for (j in seq_along(x)) {
        if (i != j && x[i] + x[j] == target) {
          return(i * 100L + j)
        }
      }
    }
    -1L
  }
  expect_quick_identical(
    find_pair,
    list(c(1L, 4L, 6L, 9L), 10L),
    list(c(1L, 2L), 10L)
  )
})

test_that("early return() of vectors, logicals, and input arguments", {
  vec <- function(x, flag) {
    declare(type(x = double(NA)), type(flag = logical(1)))
    if (flag) {
      return(x + 1)
    }
    x * 2
  }
  expect_quick_identical(vec, list(c(1, 2, 3), TRUE), list(c(1, 2, 3), FALSE))

  lgl <- function(x) {
    declare(type(x = double(NA)))
    if (x[1] < 0) {
      return(x < 0)
    }
    x > 1
  }
  expect_quick_identical(lgl, list(c(-1, 2, 0.5)), list(c(1, 2, 0.5)))

  # Every return() returns the same variable, which is also an input.
  same_input <- function(x) {
    declare(type(x = double(NA)))
    if (x[1] > 0) {
      return(x)
    }
    x <- x * 2
    x
  }
  expect_quick_identical(same_input, list(c(1, 2)), list(c(-1, 2)))

  # Different variables of runtime length must match in length.
  either <- function(x, y, use_x) {
    declare(
      type(x = double(NA)),
      type(y = double(NA)),
      type(use_x = logical(1))
    )
    if (use_x) {
      return(x)
    }
    y
  }
  expect_quick_identical(
    either,
    list(c(1, 2), c(3, 4), TRUE),
    list(c(1, 2), c(3, 4), FALSE)
  )
  qfn <- quick(either)
  expect_error(
    qfn(c(1, 2, 3), c(3, 4), FALSE),
    "all return() values must have the same shape",
    fixed = TRUE
  )
})

test_that("return() in local closures", {
  trailing <- function(x) {
    declare(type(x = double(NA)))
    total <- function(v) {
      s <- sum(v)
      return(s * 2)
    }
    total(x)
  }
  expect_quick_identical(trailing, list(c(1, 2, 3)))

  one_liner <- function(x) {
    declare(type(x = double(NA)))
    double_it <- function(v) return(v * 2)
    double_it(x)
  }
  expect_quick_identical(one_liner, list(c(1, 2, 3)))

  clamp_loop <- function(x) {
    declare(type(x = double(NA)))
    clamp <- function(v) {
      if (v < 0) {
        return(0)
      }
      if (v > 1) {
        return(1)
      }
      v
    }
    out <- double(length(x))
    for (i in seq_along(x)) {
      out[i] <- clamp(x[i])
    }
    out
  }
  expect_quick_identical(clamp_loop, list(c(-1, 0.5, 2)))

  assigned <- function(x) {
    declare(type(x = double(1)))
    safe_sqrt <- function(v) {
      if (v < 0) {
        return(-1)
      }
      sqrt(v)
    }
    y <- safe_sqrt(x)
    y + 1
  }
  expect_quick_identical(assigned, list(-4), list(16))

  in_sapply <- function(x) {
    declare(type(x = double(NA)))
    out <- sapply(seq_along(x), function(i) {
      if (x[i] < 0) {
        return(0)
      }
      x[i] * 10
    })
    out
  }
  expect_quick_identical(in_sapply, list(c(-1, 2, -3, 4)))
})

test_that("a bare return() in a closure whose value is not used", {
  fn <- function(x, limit) {
    declare(type(x = double(NA)), type(limit = integer(1)))
    out <- double(length(x))
    fill <- function(i) {
      if (i > limit) {
        return()
      }
      out[i] <<- x[i] * 2
    }
    for (i in seq_along(x)) {
      fill(i)
    }
    out
  }
  expect_quick_identical(fn, list(c(1, 2, 3, 4), 2L))
})

test_that("return() values must agree on type", {
  int_then_double <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      return(0L)
    }
    sqrt(x)
  }
  expect_error(
    quick(int_then_double),
    "all return() values must have the same type; found double where another return value is integer",
    fixed = TRUE
  )

  closure_types <- function(x) {
    declare(type(x = double(1)))
    f <- function(v) {
      if (v < 0) {
        return(0L)
      }
      v
    }
    f(x)
  }
  expect_error(
    quick(closure_types),
    "closure result mode (double) does not match output mode (integer)",
    fixed = TRUE
  )
})

test_that("return() values must agree on shape", {
  scalar_vs_vector <- function(x) {
    declare(type(x = double(NA)))
    if (x[1] < 0) {
      return(0)
    }
    x
  }
  expect_error(
    quick(scalar_vs_vector),
    "all return() values must have the same shape",
    fixed = TRUE
  )
})

test_that("unsupported uses of return() are compile errors", {
  no_value <- function(x) {
    declare(type(x = double(1)))
    if (x < 0) {
      return()
    }
    x
  }
  expect_error(
    quick(no_value),
    "return() must return a value in a quick() function",
    fixed = TRUE
  )

  early_list <- function(x) {
    declare(type(x = double(NA)))
    if (x[1] < 0) {
      return(list(a = x))
    }
    x
  }
  expect_error(
    quick(early_list),
    "return(list(...)) is only supported as the last statement",
    fixed = TRUE
  )

  list_result <- function(x) {
    declare(type(x = double(NA)))
    if (x[1] < 0) {
      return(x)
    }
    y <- x + 1
    list(x = x, y = y)
  }
  expect_error(
    quick(list_result),
    "an early return() cannot be combined with a list result yet",
    fixed = TRUE
  )

  value_position <- function(x) {
    declare(type(x = double(1)))
    y <- x + return(1)
    y
  }
  expect_error(
    quick(value_position),
    "return() must be used as a statement",
    fixed = TRUE
  )

  ends_in_loop <- function(x) {
    declare(type(x = double(NA)))
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        return(i)
      }
    }
  }
  expect_error(
    quick(ends_in_loop),
    "must end with a value or return() on every path",
    fixed = TRUE
  )

  in_parallel <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    declare(parallel())
    for (i in seq_along(x)) {
      if (x[i] < 0) {
        return(out)
      }
      out[i] <- x[i]
    }
    out
  }
  expect_error(
    quick(in_parallel),
    "return() is not supported inside a parallel() loop",
    fixed = TRUE
  )
})
