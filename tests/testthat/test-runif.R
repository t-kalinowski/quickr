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
