# Reassignment shape compatibility: quickr cannot re-declare a Fortran
# variable to R's new shape, so rank and every extent must stay
# compatible (the shape analogue of the narrowing check).

skip_on_cran()

test_that("reassignment to a statically different shape is a compile error", {
  fn_vec <- function() {
    x <- numeric(2)
    x <- numeric(3)
    x
  }
  expect_error(
    quick(fn_vec),
    "cannot reassign `x`: dimension 1 would change from 2 to 3",
    fixed = TRUE
  )

  fn_mat <- function() {
    x <- matrix(0, 2, 2)
    x <- matrix(1, 2, 3)
    x
  }
  expect_error(
    quick(fn_mat),
    "cannot reassign `x`: dimension 2 would change from 2 to 3",
    fixed = TRUE
  )

  fn_c <- function() {
    x <- c(1, 2)
    x <- c(1, 2, 3)
    x
  }
  expect_error(
    quick(fn_c),
    "cannot reassign `x`: dimension 1 would change from 2 to 3",
    fixed = TRUE
  )

  fn_rank <- function(a) {
    declare(type(a = double(2, 2)))
    x <- c(1, 2)
    x <- a
    x
  }
  expect_error(
    quick(fn_rank),
    "cannot reassign `x`: replacement rank (2) differs from the declared rank (1)",
    fixed = TRUE
  )
})

test_that("reassignment with symbolic dims gets a runtime shape guard", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(m)))
    x <- a
    x <- b
    x
  }
  qfn := quick(fn)
  expect_identical(qfn(c(1, 2), c(3, 4)), c(3, 4))
  expect_error(
    qfn(c(1, 2), c(3, 4, 5)),
    "reassignment must preserve the shape of `x`"
  )
})

test_that("shape-preserving reassignments still compile", {
  # same symbolic dims: provably equal, no guard needed
  fn_same <- function(a) {
    declare(type(a = double(n)))
    x <- a
    x <- a * 2
    x
  }
  expect_quick_identical(fn_same, c(1, 2, 3))

  # scalar broadcast into an array target keeps working
  fn_scalar <- function(a) {
    declare(type(a = double(n)))
    x <- a
    x <- 0
    sum(x)
  }
  expect_no_error(r2f(fn_scalar))
})
