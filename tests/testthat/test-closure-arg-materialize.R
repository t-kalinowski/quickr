# Local closure calls accept non-trivial argument expressions (function
# calls, subsets, guarded arithmetic) when evaluating them before the call is
# indistinguishable from R's lazy promise forcing: the argument has no side
# effects and the closure always uses it before anything observable.

skip_on_cran()

test_that("closure arguments can be calls to other local closures", {
  scalar_result <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    compute_value <- function(v) sum(v) * 2
    my_fun <- function(a, b) a * b
    my_fun(compute_value(x), y)
  }
  expect_quick_identical(scalar_result, list(c(1, 2.5, -3), 4))

  vector_result <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    scale <- function(v) v * 3
    my_fun <- function(a, b) sum(a) + b
    my_fun(scale(x), y)
  }
  expect_quick_identical(vector_result, list(c(1, 2.5, -3), 4))

  nested <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    inner <- function(v) v + 1
    outer_fun <- function(a, b) a - b
    outer_fun(inner(x[1L]), y)
  }
  expect_quick_identical(nested, list(c(10, 20), 3))
})

test_that("closure arguments can be subsets", {
  constant_index <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    my_fun <- function(a, b) a * b
    my_fun(x[5L], y)
  }
  expect_quick_identical(constant_index, list(as.double(1:6), 2))

  in_loop <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    my_fun <- function(a, b) a * b + 1
    out <- double(length(x))
    for (i in seq_along(x)) {
      out[i] <- my_fun(x[i], y)
    }
    out
  }
  expect_quick_identical(in_loop, list(c(1, -2, 3.5), 2))

  range_index <- function(x) {
    declare(type(x = integer(NA)))
    total <- function(v) sum(v)
    total(x[2:4])
  }
  expect_quick_identical(range_index, list(1:6))
})

test_that("closure arguments can be built-in calls and guarded arithmetic", {
  builtins <- function(x) {
    declare(type(x = double(NA)))
    my_fun <- function(a, b) a / b
    my_fun(sum(x), max(x))
  }
  expect_quick_identical(builtins, list(c(1, 5, 2)))

  # `x * 2` needs a runtime non-empty guard for an unknown-length `x`
  guarded <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    my_fun <- function(a, b) sum(a) * b
    my_fun(x * 2, y)
  }
  expect_quick_identical(guarded, list(c(1, 2, 3), 0.5))

  # Unequal lengths keep raising quickr's documented error
  mismatched <- function(x, w) {
    declare(type(x = double(NA)), type(w = double(NA)))
    my_fun <- function(a) sum(a)
    my_fun(x + w)
  }
  expect_quick_identical(mismatched, list(c(1, 2, 3), c(4, 5, 6)))
  qfn <- quick(mismatched)
  expect_error(qfn(c(1, 2, 3), c(4, 5)), "equal lengths")
})

test_that("materialized closure arguments follow argument matching", {
  reordered <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    compute_value <- function(v) sum(v)
    my_fun <- function(a, b) a - b
    my_fun(b = y[2L], a = compute_value(x))
  }
  expect_quick_identical(reordered, list(c(1, 2, 3), c(10, 20)))

  assigned <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    my_fun <- function(a, b) a * b
    z <- my_fun(x[2L], y)
    z + 1
  }
  expect_quick_identical(assigned, list(c(1, 2, 3), 4))
})

test_that("an argument used in a closure's if() condition is materialized", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(1)))
    my_fun <- function(a, b) {
      out <- -b
      if (a > 0) {
        out <- b
      }
      out
    }
    my_fun(x[1L], y)
  }
  expect_quick_identical(fn, list(c(1, 2), 3), list(c(-1, 2), 3))
})

test_that("a pure argument may read a superassigned variable it forces first", {
  fn <- function(x) {
    declare(type(x = double(1)))
    y <- x
    f <- function(a) {
      b <- a
      y <<- 100
      b + y
    }
    f(y + 1)
  }
  expect_quick_identical(fn, list(1), list(-3))
})

test_that("arguments a closure may not use are still rejected", {
  unused <- function(x) {
    declare(type(x = double(NA)))
    ignore <- function(a, b) b * 2
    ignore(x[1L], 3)
  }
  expect_error(
    quick(unused),
    "argument `a = x[1L]` may raise an error, but `ignore` does not always use `a`",
    fixed = TRUE
  )

  branch_only <- function(x, flag) {
    declare(type(x = double(NA)), type(flag = logical(1)))
    my_fun <- function(a, use) {
      if (use) a else 0
    }
    my_fun(x[1L], flag)
  }
  expect_error(
    quick(branch_only),
    "`my_fun` does not always use `a`",
    fixed = TRUE
  )

  reassigned <- function(x) {
    declare(type(x = double(NA)))
    my_fun <- function(a) {
      a <- 1
      a
    }
    my_fun(x[10L])
  }
  expect_error(
    quick(reassigned),
    "`my_fun` does not always use `a`",
    fixed = TRUE
  )
})

test_that("arguments used only after a closure's own effects are rejected", {
  after_cat <- function(x) {
    declare(type(x = double(NA)))
    my_fun <- function(a) {
      cat("start\n")
      a
    }
    my_fun(x[1L])
  }
  expect_error(
    quick(after_cat),
    "`my_fun` does not always use `a` before other effects",
    fixed = TRUE
  )

  after_runif <- function(x) {
    declare(type(x = double(NA)))
    my_fun <- function(a) {
      r <- runif(1)
      a + r
    }
    my_fun(sum(x))
  }
  expect_error(
    quick(after_runif),
    "`my_fun` does not always use `a` before other effects",
    fixed = TRUE
  )
})

test_that("arguments with side effects are rejected", {
  rng <- function() {
    my_fun <- function(a) a * 2
    my_fun(runif(1))
  }
  expect_error(
    quick(rng),
    "argument `a = runif(1)` has side effects",
    fixed = TRUE
  )

  effectful_closure <- function(x) {
    declare(type(x = double(1)))
    state <- x
    bump <- function() {
      state <<- state + 1
      state
    }
    my_fun <- function(a) a * 2
    my_fun(bump())
  }
  expect_error(
    quick(effectful_closure),
    "argument `a = bump()` has side effects",
    fixed = TRUE
  )
})

test_that("arguments reading a variable the closure superassigns first are rejected", {
  # Previously compiled, returning 2 where R returns 101.
  direct <- function(x) {
    declare(type(x = double(1)))
    y <- x
    f <- function(a) {
      y <<- 100
      a
    }
    f(y + 1)
  }
  expect_error(
    quick(direct),
    "argument `a = y + 1` reads `y`, which `f` may modify with `<<-` before using `a`",
    fixed = TRUE
  )

  through_closure <- function(x) {
    declare(type(x = double(1)))
    y <- x
    reset <- function() {
      y <<- 100
      0
    }
    f <- function(a) {
      reset()
      a
    }
    f(y * 2)
  }
  expect_error(
    quick(through_closure),
    "which `f` may modify with `<<-` before using `a`",
    fixed = TRUE
  )
})

test_that("an argument used in a <<- target index is used before the write", {
  # R evaluates `v`, then the index `i`, and only then writes `out[i]`.
  scatter <- function(x, pos) {
    declare(type(x = double(NA)), type(pos = integer(NA)))
    out <- double(8)
    put <- function(i, v) {
      out[i] <<- v * 2
    }
    for (k in seq_along(pos)) {
      put(pos[k], x[k])
    }
    out
  }
  expect_quick_identical(scatter, list(c(1.5, -2, 4), c(3L, 8L, 1L)))

  # The argument appears only inside a computed index.
  mark_slot <- function(x) {
    declare(type(x = integer(NA)))
    seen <- integer(4)
    mark <- function(code) {
      seen[bitwAnd(code, 3L) + 1L] <<- 1L
    }
    for (k in seq_along(x)) {
      mark(x[k])
    }
    seen
  }
  expect_quick_identical(mark_slot, list(c(0L, 5L, 6L)), list(c(3L, 7L)))

  # A different <<- that runs first is still an effect before the argument
  # is used.
  effect_first <- function(x, pos) {
    declare(type(x = double(NA)), type(pos = integer(NA)))
    out <- double(8)
    writes <- 0L
    put <- function(i, v) {
      writes <<- writes + 1L
      out[i] <<- v
    }
    put(pos[1L], x[1L])
    out
  }
  expect_error(
    quick(effect_first),
    "`put` does not always use `i` before other effects",
    fixed = TRUE
  )
})

test_that("arguments forwarded to another local function count as used when it always uses them", {
  midpoints <- function(x) {
    declare(type(x = double(NA)))
    lerp <- function(a, b, t) a + (b - a) * t
    midpoint <- function(a, b) lerp(a, b, 0.5)
    out <- double(length(x) - 1L)
    for (i in seq_len(length(x) - 1L)) {
      out[i] <- midpoint(x[i], x[i + 1L])
    }
    out
  }
  expect_quick_identical(midpoints, list(c(1, 3, 4, 10)))

  # Forwarded through several helpers, including inside an expression.
  layered <- function(x) {
    declare(type(x = double(NA)))
    scale_by <- function(v, k) v * k
    double_it <- function(v) scale_by(v, 2)
    shifted_double <- function(v) double_it(v + 1)
    shifted_double(x[2L])
  }
  expect_quick_identical(layered, list(c(1, 5)))

  # Forwarding into a helper that uses the argument only conditionally is
  # still not "always used".
  conditional_use <- function(x, flag) {
    declare(type(x = double(NA)), type(flag = logical(1)))
    maybe <- function(a, use) {
      out <- 0
      if (use) {
        out <- a
      }
      out
    }
    forward <- function(a, use) maybe(a, use)
    forward(x[1L], flag)
  }
  expect_error(
    quick(conditional_use),
    "`forward` does not always use `a`",
    fixed = TRUE
  )
})
