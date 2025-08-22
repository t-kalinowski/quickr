# Unit tests for size constraints

test_that("size constraint", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n + 1)))
    a = sum(b)
    a
  }

  fsub <- new_fortran_subroutine("fn", fn)
  cbridge <- make_c_bridge(fsub, strict = TRUE)

  qfn := quick(fn)

  expect_error(
    qfn(1, 2),
    regexp = "length(b) must equal (length(a) + 1), but are 1 and 2",
    fixed = TRUE
  )
  expect_translation_snapshots(fn, "call_size_constraint")
  expect_equal(qfn(1, c(2, 3)), 5)
})

test_that("size constraint", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x <- -0.1 + x
    x
  }

  qfn := quick(fn)

  expect_error(
    qfn(c(1, 2)),
    regexp = "length(x) must be 1, not 2",
    fixed = TRUE
  )

  expect_identical(qfn(3), fn(3))
  expect_identical(qfn(-3), fn(-3))
  expect_identical(qfn(0), fn(0))
  expect_identical(qfn(-0), fn(-0))
})
