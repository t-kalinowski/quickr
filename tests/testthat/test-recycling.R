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

test_that("a known length-0 operand is rejected against an unknown length", {
  # R answers numeric(0) here; quickr has no length-0 result to return, so
  # the zero is rejected even when the other operand's length is not a
  # number the compiler can compare it to.
  fill_left <- function(x) {
    declare(type(x = double(n)))
    numeric(0) + x
  }
  expect_error(quick(fill_left), "equal lengths")

  fill_right <- function(x) {
    declare(type(x = double(n)))
    x > numeric(0)
  }
  expect_error(quick(fill_right), "equal lengths")

  declared <- function(a, b) {
    declare(type(a = double(0)), type(b = double(n)))
    a * b
  }
  expect_error(quick(declared), "equal lengths")

  # An NA dim is unknown, not "matches anything"
  unspecified <- function(a, b) {
    declare(type(a = double(NA)), type(b = double(0)))
    a - b
  }
  expect_error(quick(unspecified), "equal lengths")
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

test_that("1x1 matrix operands follow R: arithmetic scalarizes, strict ops reject", {
  # Arithmetic: R recycles a length-1 array against a longer vector
  # (deprecated, hence suppressWarnings, but still R's answer). A 1x1
  # operand that needs a cast used to emit unindexable expression text
  # (`real(b, kind=c_double)(1, 1)`), a gfortran syntax error; it is now
  # hoisted to a temporary before subscripting.
  cast_fn <- function(a, b) {
    declare(type(a = double(3)), type(b = logical(1, 1)))
    a + b
  }
  qfn <- quick(cast_fn)
  a <- c(1.5, 2.5, 3.5)
  b <- matrix(TRUE)
  expect_identical(qfn(a, b), suppressWarnings(cast_fn(a, b)))

  div_fn <- function(a, b) {
    declare(type(a = double(3)), type(b = logical(1, 1)))
    a / b
  }
  qdiv <- quick(div_fn)
  expect_identical(qdiv(a, b), suppressWarnings(div_fn(a, b)))

  # Comparisons and & | do not get R's length-1 array recycling: R errors
  # ("dims [product 1] do not match the length of object"). Scalarizing
  # here would answer where R refuses, so the 1x1 is treated as an
  # ordinary one-row matrix and rejected.
  cmp_fn <- function(a, b) {
    declare(type(a = double(3)), type(b = double(1, 1)))
    a < b
  }
  expect_error(quick(cmp_fn), "matrix first dimension")

  and_fn <- function(a, b) {
    declare(type(a = logical(3)), type(b = logical(1, 1)))
    a & b
  }
  expect_error(quick(and_fn), "matrix first dimension")

  # Unknown vector length against a 1x1: strict ops guard at runtime
  # (length 1 conforms, like R; anything longer is the R error above)
  sym_cmp <- function(a, b) {
    declare(type(a = double(NA)), type(b = double(1, 1)))
    a < b
  }
  qcmp <- quick(sym_cmp)
  expect_identical(qcmp(3, matrix(5)), 3 < matrix(5))
  expect_error(qcmp(c(1, 2, 3), matrix(5)), "matrix first dimension")
})

test_that("1x1 matrix with a symbolic-length vector keeps R's shape", {
  # The result's shape depends on the runtime length: R keeps the 1x1
  # dims for a length-1 vector and drops them for any other length, so no
  # static decision can be right for both. Scalarizing regardless (the
  # old behavior) silently returned a dimensionless vector where R
  # returns a 1x1 matrix. Symbolic lengths now take the vector-matrix
  # rule instead: a runtime guard requires length 1 and the result is a
  # 1x1 matrix; longer vectors error where R would recycle (deprecated).
  fn <- function(m, x) {
    declare(type(m = double(1, 1)), type(x = double(n)))
    m + x
  }
  qfn <- quick(fn)
  expect_identical(qfn(matrix(2), 3), fn(matrix(2), 3))
  expect_error(qfn(matrix(2), c(1, 2, 3)), "matrix first dimension")

  rev_fn <- function(x, m) {
    declare(type(x = double(n)), type(m = double(1, 1)))
    x + m
  }
  qrev <- quick(rev_fn)
  expect_identical(qrev(3, matrix(2)), rev_fn(3, matrix(2)))
  expect_error(qrev(c(1, 2, 3), matrix(2)), "matrix first dimension")
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

test_that("fill constructors materialize where an array is required", {
  # A fill reaching c() through an expression is a real array, not a
  # scalar literal with claimed dims (which emitted one element where the
  # length arithmetic counted two).
  through_op <- function(x) {
    declare(type(x = double(2)))
    c(numeric(2) + 1, x)
  }
  expect_quick_identical(through_op, list(c(5, 6)))

  # Same leak as a silent wrong answer: sum() over a fill expression saw
  # one scalar instead of the filled length.
  reduced <- function() {
    sum(numeric(2) + 3)
  }
  expect_quick_identical(reduced, list())

  symbolic <- function(x, k) {
    declare(type(x = double(2)), type(k = integer(1)))
    c(integer(k) + 1L, x)
  }
  expect_quick_identical(symbolic, list(c(5, 6), 3L))
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

test_that("a closure's return expression materializes fills and matrix()", {
  # A local closure's return expression is compiled on its own, with no
  # enclosing call: the materialization decision sees an empty call stack,
  # so nothing is broadcasting, spreading, or padding the scalar-with-dims
  # form and it has to become a real array.
  fill <- function(x) {
    declare(type(x = double(3)))
    zeros <- function() numeric(3)
    x + zeros()
  }
  expect_quick_identical(fill, list(c(1, 2, 3)))

  mat <- function(x) {
    declare(type(x = double(2, 2)))
    ones <- function() matrix(1, 2, 2)
    x + ones()
  }
  expect_quick_identical(mat, list(matrix(as.double(1:4), 2, 2)))
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
