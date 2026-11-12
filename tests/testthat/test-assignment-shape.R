# Reassignment shape compatibility: quickr cannot re-declare a Fortran
# variable to R's new shape, so rank and every extent must stay
# compatible (the shape analogue of the narrowing check).

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

  fn_deferred_rank <- function(a) {
    declare(type(a = double(2, 2)), type(x = double(NA)))
    x <- a
    1
  }
  expect_error(
    quick(fn_deferred_rank),
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

  # two length-1 values conform whatever their ranks: a declared double(1)
  # is rank 1, a literal is rank 0
  fn_len1 <- function(a) {
    declare(type(a = double(1)))
    a <- 2
    a
  }
  expect_quick_identical(fn_len1, 1)
})

test_that("reassignment between scalar and array shapes is refused", {
  # R rebinds `x` to the scalar; Fortran would broadcast it across the
  # array, so every element would change instead of the shape
  fn_scalar_into_array <- function(a) {
    declare(type(a = double(n)))
    x <- a
    x <- 0
    sum(x)
  }
  expect_error(
    r2f(fn_scalar_into_array),
    "replacement is a scalar but `x` is an array",
    fixed = TRUE
  )

  # the reduction form of the same mistake
  fn_reduce_into_array <- function(a) {
    declare(type(a = double(n)))
    x <- a
    x <- sum(x)
    x
  }
  expect_error(
    r2f(fn_reduce_into_array),
    "replacement is a scalar but `x` is an array",
    fixed = TRUE
  )

  # the other direction: R rebinds to the array, Fortran would keep only
  # the first element
  fn_array_into_scalar <- function(a) {
    declare(type(a = double(3)))
    x <- 1
    x <- a
    x
  }
  expect_error(
    r2f(fn_array_into_scalar),
    "replacement is an array but `x` is a scalar",
    fixed = TRUE
  )
})

test_that("scalar arithmetic and fixed-shape copies retain input dimensions", {
  fn <- function(x) {
    declare(type(x = double(n)))
    y <- x * 2
    y <- y / 2
    y + x
  }

  expect_quick_identical(fn, c(1, 2, 3))

  # Shape-preserving expressions must not recheck equality with their source.
  # The current nonempty policy still needs its separate runtime check.
  code <- as.character(r2f(fn))
  expect_false(grepl("/=", code, fixed = TRUE))
  expect_true(grepl("== 0", code, fixed = TRUE))
})

test_that("full columns retain wrapper-validated dimension equality", {
  fn <- function(x, y) {
    declare(type(x = double(n, m)), type(y = double(n, m)))
    total <- 0
    for (i in seq_len(ncol(x))) {
      total <- total + sum(x[, i] * y[, i])
    }
    total
  }

  x <- matrix(as.double(1:6), 2, 3)
  y <- matrix(as.double(6:1), 2, 3)
  expect_quick_identical(fn, list(x, y))
  expect_false(grepl("/=", as.character(r2f(fn)), fixed = TRUE))
})

test_that("input shape proofs survive scalar dimension rebinding", {
  fn <- function(n, x, y) {
    declare(type(n = integer(1)), type(x = double(n)), type(y = double(n)))
    copy <- x
    n <- n + 1L
    total <- sum(copy + y)
    total + as.double(n)
  }

  expect_quick_identical(fn, list(3L, c(1, 2, 3), c(4, 5, 6)))
})

test_that("independent inputs and deferred local resizes keep shape checks", {
  independent <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    (x * 2) + y
  }
  deferred <- function(x) {
    declare(type(x = double(n)), type(copy = double(NA)))
    copy <- x
    copy <- x[1:2]
    sum(copy + x)
  }

  qindependent <- quick(independent)
  expect_identical(qindependent(c(1, 2), c(3, 4)), c(5, 8))
  expect_error(qindependent(c(1, 2), c(3, 4, 5)), "equal lengths")
  expect_quick_identical(deferred, c(1, 2))
  expect_error(quick(deferred)(c(1, 2, 3)), "equal lengths")
})

test_that("captured copies and shadowed dimensions remain conservative", {
  fn <- function(n, x, local_n) {
    declare(
      type(n = integer(1)),
      type(x = double(n)),
      type(local_n = integer(1))
    )
    copy <- x
    f <- function(n) {
      y <- numeric(n)
      sum(copy + y)
    }
    f(local_n)
  }

  expect_quick_identical(fn, list(3L, c(1, 2, 3), 3L))
  expect_error(quick(fn)(3L, c(1, 2, 3), 2L), "equal lengths")
})
