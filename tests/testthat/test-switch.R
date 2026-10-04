# Numeric switch() lowered to Fortran SELECT CASE, as a statement and as a
# value.

skip_on_cran()

test_that("switch() as a statement dispatches on position", {
  dispatch <- function(ops) {
    declare(type(ops = integer(NA)))
    acc <- 0L
    bump <- function() {
      acc <<- acc + 100L
      0L
    }
    reset <- function() {
      acc <<- 0L
    }
    for (i in seq_along(ops)) {
      switch(
        ops[i] + 1L,
        acc <- acc + 1L,
        {
          acc <- acc * 2L
        },
        bump(),
        reset(),
        next,
        break
      )
      acc <- acc + 1000L
    }
    acc
  }
  expect_translation_snapshots(dispatch)
  expect_quick_identical(
    dispatch,
    list(c(0L, 1L, 2L, 0L, 3L, 1L)),
    # next skips the rest of the iteration, break ends the loop
    list(c(0L, 4L, 1L, 5L, 2L)),
    # out-of-range indices do nothing, as in R
    list(c(-1L, 99L, 0L))
  )
})

test_that("switch() coerces EXPR like R and ignores names", {
  pick <- function(x, flag) {
    declare(type(x = double(1)), type(flag = logical(1)))
    out <- 0L
    switch(x, out <- 1L, out <- 2L, out <- 3L)
    switch(EXPR = flag, out <- out + 10L)
    switch(as.integer(x), first = out <- out + 100L, second = out <- out + 200L)
    out
  }
  expect_quick_identical(
    pick,
    list(2.9, TRUE),
    list(1, FALSE),
    list(3.2, TRUE),
    list(0.5, FALSE)
  )
})

test_that("return() works inside switch() alternatives", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    switch(x, return(10L), {
      x <- x * 3L
    })
    x - 1L
  }
  expect_quick_identical(fn, list(1L), list(2L), list(3L))
})

test_that("switch() as a value", {
  cycles <- function(op) {
    declare(type(op = integer(1)))
    n <- switch(op + 1L, 4L, 8L, 12L)
    n * 2L
  }
  expect_translation_snapshots(cycles)
  expect_quick_identical(cycles, list(0L), list(1L), list(2L))

  vector_result <- function(x, k) {
    declare(type(x = double(NA)), type(k = integer(1)))
    switch(k, x + 1, x * 2, -x)
  }
  expect_quick_identical(
    vector_result,
    list(c(1, 2, 3), 1L),
    list(c(1, 2, 3), 2L),
    list(c(1, 2, 3), 3L)
  )

  closure_result <- function(x) {
    declare(type(x = integer(NA)))
    weight <- function(k) switch(k, 1.5, 2.5, 4)
    out <- double(length(x))
    for (i in seq_along(x)) {
      out[i] <- weight(x[i])
    }
    out
  }
  expect_quick_identical(closure_result, list(c(3L, 1L, 2L)))

  closure_alternatives <- function(op, a) {
    declare(type(op = integer(1)), type(a = double(1)))
    double_it <- function(v) v * 2
    halve <- function(v) v / 2
    switch(op, double_it(a), halve(a))
  }
  expect_quick_identical(closure_alternatives, list(1L, 3), list(2L, 3))
})

test_that("switch() values must agree on type and shape", {
  mixed_types <- function(k) {
    declare(type(k = integer(1)))
    switch(k, 1L, 2.5)
  }
  expect_error(
    quick(mixed_types),
    "all switch() alternatives must give the same type; alternative 2 is double where alternative 1 is integer",
    fixed = TRUE
  )

  scalar_vs_vector <- function(x, k) {
    declare(type(x = double(NA)), type(k = integer(1)))
    switch(k, 0, x)
  }
  expect_error(
    quick(scalar_vs_vector),
    "all switch() alternatives must have the same shape",
    fixed = TRUE
  )

  runtime_lengths <- function(x, y, k) {
    declare(type(x = double(NA)), type(y = double(NA)), type(k = integer(1)))
    switch(k, x, y)
  }
  expect_quick_identical(
    runtime_lengths,
    list(c(1, 2), c(3, 4), 1L),
    list(c(1, 2), c(3, 4), 2L)
  )
  qfn <- quick(runtime_lengths)
  expect_error(
    qfn(c(1, 2), c(3, 4, 5), 2L),
    "all switch() alternatives must have the same shape",
    fixed = TRUE
  )
})

test_that("an out-of-range switch() value is a runtime error", {
  fn <- function(k) {
    declare(type(k = integer(1)))
    switch(k, 10L, 20L)
  }
  qfn <- quick(fn)
  expect_identical(qfn(2L), 20L)
  expect_error(
    qfn(3L),
    "switch() index is out of range; R would return NULL",
    fixed = TRUE
  )
})

test_that("unsupported switch() forms are compile errors", {
  empty_alternative <- function(k) {
    declare(type(k = integer(1)))
    out <- 0L
    switch(k, , out <- 1L)
    out
  }
  expect_error(
    quick(empty_alternative),
    "empty alternative in numeric switch",
    fixed = TRUE
  )

  no_alternatives <- function(k) {
    declare(type(k = integer(1)))
    switch(k)
    k
  }
  expect_error(
    quick(no_alternatives),
    "switch() needs at least one alternative",
    fixed = TRUE
  )

  character_expr <- function(k) {
    declare(type(k = integer(1)))
    out <- 0L
    switch("b", a = out <- 1L, b = out <- 2L)
    out
  }
  expect_error(
    quick(character_expr),
    "only numeric switch() is supported",
    fixed = TRUE
  )

  vector_expr <- function(k) {
    declare(type(k = integer(NA)))
    out <- 0L
    switch(k, out <- 1L, out <- 2L)
    out
  }
  expect_error(
    quick(vector_expr),
    "switch() EXPR must be a length 1 vector",
    fixed = TRUE
  )
})

test_that("switch() works inside a parallel() loop", {
  fn <- function(ops) {
    declare(type(ops = integer(NA)))
    out <- double(length(ops))
    declare(parallel())
    for (i in seq_along(ops)) {
      switch(ops[i], out[i] <- 1.5, out[i] <- 2.5, out[i] <- -1)
    }
    out
  }
  expect_quick_identical(fn, list(c(1L, 3L, 2L, 2L, 9L)))
})

test_that("closure arguments used in only one alternative are not always used", {
  only_one_branch <- function(x, k) {
    declare(type(x = double(NA)), type(k = integer(1)))
    pick <- function(a, j) switch(j, a * 2, 0)
    pick(x[1L], k)
  }
  expect_error(
    quick(only_one_branch),
    "`pick` does not always use `a`",
    fixed = TRUE
  )

  # EXPR is always evaluated, so an argument used there is materialized.
  as_selector <- function(x) {
    declare(type(x = integer(NA)))
    pick <- function(j) switch(j, 10L, 20L)
    pick(x[2L])
  }
  expect_quick_identical(as_selector, list(c(5L, 2L)))
})

test_that("a 256-alternative switch() matches R", {
  alternatives <- vapply(
    0:255,
    function(k) {
      sprintf("{ acc <- bitwXor(acc, %dL) + %dL }", (k * 37L) %% 251L, k)
    },
    ""
  )
  dispatch <- eval(str2lang(sprintf(
    "function(ops) {
      declare(type(ops = integer(NA)))
      acc <- 0L
      for (i in seq_along(ops)) {
        switch(ops[i] + 1L, %s)
      }
      acc
    }",
    paste(alternatives, collapse = ",\n")
  )))
  set.seed(1)
  ops <- sample.int(256L, 5000L, replace = TRUE) - 1L
  expect_quick_identical(dispatch, list(ops))
})
