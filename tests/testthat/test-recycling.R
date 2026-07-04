# Elementwise conformability: R-style recycling is rejected (compile error
# for known mismatches, runtime guard for unknown), fill constructors spread
# inside c(), and matrix(scalar, m, n) materializes a real array.

skip_on_cran()

test_that("known unequal vector lengths are a compile error", {
  # divisible lengths were previously blessed and silently mis-lowered
  divisible <- function(a, b) {
    declare(type(a = double(2)), type(b = double(4)))
    a + b
  }
  expect_error(quick(divisible), "equal lengths")

  ragged <- function(a, b) {
    declare(type(a = double(2)), type(b = double(3)))
    a * b
  }
  expect_error(quick(ragged), "equal lengths")

  zero_len <- function(a, b) {
    declare(type(a = double(0)), type(b = double(4)))
    a + b
  }
  expect_error(quick(zero_len), "equal lengths")
})

test_that("length checks cover comparisons, logical ops, and modulo", {
  comparison <- function(a, b) {
    declare(type(a = double(2)), type(b = double(4)))
    a < b
  }
  expect_error(quick(comparison), "equal lengths")

  logical_op <- function(a, b) {
    declare(type(a = logical(2)), type(b = logical(4)))
    a & b
  }
  expect_error(quick(logical_op), "equal lengths")

  modulo <- function(a, b) {
    declare(type(a = integer(2)), type(b = integer(4)))
    a %% b
  }
  expect_error(quick(modulo), "equal lengths")
})

test_that("symbolic differing lengths get a runtime guard", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(m)))
    a + b
  }
  qfn <- quick(fn)
  expect_identical(qfn(c(1, 2), c(10, 20)), c(11, 22))
  # was: silent truncation to c(11, 22)
  expect_error(qfn(c(1, 2), c(10, 20, 30, 40)), "equal lengths")
})

test_that("identical symbolic lengths stay guard-free and work", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    a - b
  }
  fsub <- r2f(fn)
  expect_no_match(fsub, "quickr_set_error_msg", fixed = TRUE)
  expect_quick_identical(fn, list(c(1, 2, 3), c(10, 20, 30)))
})

test_that("scalar broadcast is unaffected", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(1)))
    a + b
  }
  fsub <- r2f(fn)
  expect_no_match(fsub, "quickr_set_error_msg", fixed = TRUE)
  expect_quick_identical(fn, list(c(1, 2, 3), 10))
})

test_that("matrix-matrix elementwise ops guard unknown dims per axis", {
  fn <- function(a, b) {
    declare(type(a = double(n, k)), type(b = double(m, j)))
    a * b
  }
  qfn <- quick(fn)
  m1 <- matrix(as.double(1:6), 2, 3)
  m2 <- matrix(as.double(6:1), 2, 3)
  expect_identical(qfn(m1, m2), m1 * m2)
  expect_error(qfn(m1, t(m2)), "matching dimensions")
})

test_that("vector-matrix ops with unknown dims guard instead of rejecting", {
  fn <- function(vec, mat) {
    declare(type(vec = double(n)), type(mat = double(m, k)))
    vec + mat
  }
  qfn <- quick(fn)
  mat <- matrix(as.double(1:6), 2, 3)
  vec <- c(10, 20)
  expect_identical(qfn(vec, mat), vec + mat)
  expect_error(qfn(c(10, 20, 30), mat), "matrix first dimension")
})

test_that("guard text is pinned (one snapshot per mechanism)", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(m)))
    a + b
  }
  expect_translation_snapshots(
    fn,
    note = "Symbolic differing lengths emit one statement-level size guard."
  )
})

test_that("fill constructors spread inside c()", {
  known <- function(x) {
    declare(type(x = double(3)))
    c(numeric(2), x)
  }
  expect_quick_identical(known, list(as.double(1:3)))

  symbolic <- function(x, k) {
    declare(type(x = double(3)), type(k = integer(1)))
    c(numeric(k), x)
  }
  expect_quick_identical(symbolic, list(as.double(1:3), 2L))
  expect_quick_identical(symbolic, list(as.double(1:3), 0L))

  promoted <- function(x) {
    declare(type(x = double(1)))
    c(integer(2), x)
  }
  expect_quick_identical(promoted, list(1.5))

  logical_fill <- function(x) {
    declare(type(x = logical(2)))
    c(logical(3), x)
  }
  expect_quick_identical(logical_fill, list(c(TRUE, FALSE)))
})

test_that("matrix(scalar, m, n) materializes where an array is required", {
  reduced <- function() {
    sum(matrix(2, 2, 3))
  }
  expect_quick_identical(reduced, list())

  transposed <- function() {
    t(matrix(1, 2, 3))
  }
  expect_quick_identical(transposed, list())
})

test_that("matrix(scalar, m, n) keeps the broadcast fast path on assignment", {
  fn <- function(n, k) {
    declare(type(n = integer(1)), type(k = integer(1)))
    m <- matrix(0, n, k)
    m
  }
  fsub <- r2f(fn)
  # no hoisted temp: the scalar broadcasts straight into the target
  expect_match(fsub, "m = 0.0_c_double", fixed = TRUE)
  expect_quick_identical(fn, list(2L, 3L))
})
