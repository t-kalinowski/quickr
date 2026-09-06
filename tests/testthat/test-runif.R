test_that("runif generates random numbers", {
  ## test simple runif
  fn <- function(n) {
    declare(type(n = integer(1)))
    runif(n)
  }

  expect_translation_snapshots(fn)
  qrunif <- quick(fn)

  expect_identical(
    set_seed_and_call(runif, 5L),
    set_seed_and_call(qrunif, 5L)
  )

  expect_identical(
    set_seed_and_call(runif, 1L),
    set_seed_and_call(qrunif, 1L)
  )

  # scalar runif in fortran local
  fn <- function(x) {
    declare(type(x = double(NA)))
    x * runif(1)
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  x <- runif(5)
  expect_identical(
    set_seed_and_call(fn, x),
    set_seed_and_call(qfn, x)
  )

  # 1d runif array in fortran local
  fn <- function(x) {
    declare(type(x = double(NA)))
    x * runif(length(x))
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, x),
    set_seed_and_call(qfn, x)
  )
})

test_that("runif rejects non-scalar sample counts", {
  expect_error(
    quick(function() {
      sum(runif(c(5L, 3L)))
    }),
    "runif() requires a scalar sample count",
    fixed = TRUE
  )
  expect_error(
    quick(function(n) {
      declare(type(n = integer(NA)))
      runif(n)
    }),
    "runif() requires a scalar sample count",
    fixed = TRUE
  )
  expect_error(
    quick(function(n) {
      declare(type(n = double(2)))
      sum(runif(n))
    }),
    "runif() requires a scalar sample count",
    fixed = TRUE
  )

  fn <- function() {
    sum(runif(c(5L)))
  }
  qfn <- quick(fn)
  expect_identical(set_seed_and_call(qfn), set_seed_and_call(fn))
  set.seed(42)
  expected <- fn()
  expected_seed <- .Random.seed
  set.seed(42)
  expect_identical(qfn(), expected)
  expect_identical(.Random.seed, expected_seed)
})

test_that("runif rejects non-scalar bounds", {
  expect_error(
    quick(function(n, b) {
      declare(type(n = integer(1)), type(b = double(n)))
      sum(runif(n, max = b))
    }),
    "runif() requires a scalar `max` bound",
    fixed = TRUE
  )
  expect_error(
    quick(function(n, b) {
      declare(type(n = integer(1)), type(b = double(n)))
      sum(runif(n, min = b, max = 10))
    }),
    "runif() requires a scalar `min` bound",
    fixed = TRUE
  )
  expect_error(
    quick(function(n) {
      declare(type(n = integer(1)))
      sum(runif(n, max = c(1, 2)))
    }),
    "runif() requires a scalar `max` bound",
    fixed = TRUE
  )

  # A scalar bound still draws exactly `n` values and leaves R's RNG state
  # where R leaves it.
  fn <- function(n, a, b) {
    declare(type(n = integer(1)), type(a = double(1)), type(b = double(1)))
    runif(n, a, b)
  }
  qfn <- quick(fn)
  set.seed(42)
  expected <- fn(4L, 1, 3)
  expected_seed <- .Random.seed
  set.seed(42)
  expect_identical(qfn(4L, 1, 3), expected)
  expect_identical(.Random.seed, expected_seed)
})

test_that("runif with min/max", {
  fn <- function(n, a, b) {
    declare(
      type(n = integer(1)),
      type(a = double(1)),
      type(b = double(1))
    )
    runif(n, a, b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 10L, 3, 11),
    set_seed_and_call(qfn, 10L, 3, 11)
  )

  expect_identical(
    set_seed_and_call(fn, 1L, 3, 11),
    set_seed_and_call(qfn, 1L, 3, 11)
  )

  fn <- function(n, b) {
    declare(
      type(n = integer(1)),
      type(b = double(1))
    )
    runif(n, max = b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 10L, 20),
    set_seed_and_call(qfn, 10L, 20)
  )

  fn <- function(b) {
    declare(
      type(b = double(1))
    )
    runif(1, max = b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 20),
    set_seed_and_call(qfn, 20)
  )

  fn <- function(b) {
    declare(
      type(b = double(1))
    )
    runif(10, max = b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 20),
    set_seed_and_call(qfn, 20)
  )
})

test_that("impure runif() bounds are evaluated exactly once", {
  # `min` is spliced twice into the emitted expression, and the implied-do
  # for array results would re-evaluate spliced bounds per element; R
  # evaluates bounds once per call.
  fn <- function() {
    out <- runif(2L, runif(1L), 10)
    out
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn),
    set_seed_and_call(qfn)
  )

  set.seed(1)
  qfn()
  q_next <- runif(1L)
  set.seed(1)
  fn()
  r_next <- runif(1L)
  expect_identical(q_next, r_next)
})

test_that("runif evaluates bounds before rejecting a dynamic count", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    sum(runif(n, runif(1L), runif(1L)))
  }
  qfn <- quick(fn)

  set.seed(1)
  expect_error(fn(-1L), "invalid arguments")
  expected_seed <- .Random.seed

  set.seed(1)
  expect_error(qfn(-1L), "sample count must be non-negative")
  expect_identical(.Random.seed, expected_seed)

  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(1)))
    bump <- function() {
      n <<- -1L
      x <<- x + 10
      20
    }
    runif(n, x, bump())
  }
  qfn <- quick(fn)
  expect_equal(set_seed_and_call(fn, 1L, 1), set_seed_and_call(qfn, 1L, 1))
})
