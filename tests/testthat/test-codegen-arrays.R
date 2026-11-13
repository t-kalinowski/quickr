skip_on_cran()

test_that("codegen: fixed cbind promotes mixed inputs and broadcasts scalars", {
  fn <- function(A, v, s) {
    declare(type(A = double(2, 2)), type(v = integer(2)), type(s = double(1)))
    cbind(A, v, s, deparse.level = 0)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(matrix(as.double(1:4), 2), 5:6, 7))
})

test_that("codegen: fixed rbind writes matrix rows and scalar fills", {
  fn <- function(A, v) {
    declare(type(A = double(2, 3)), type(v = double(3)))
    rbind(A, v, 1, deparse.level = 0)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(matrix(as.double(1:6), 2), c(7, 8, 9)))
})

test_that("codegen: scalar-only binds retain their matrix shapes", {
  fn <- function() {
    columns <- cbind(1, 2, deparse.level = 0)
    rows <- rbind(1, 2, deparse.level = 0)
    list(columns = columns, rows = rows)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list())
})

test_that("codegen: dynamic cbind ignores NULL and checks common lengths", {
  fn <- function(x, y) {
    declare(type(x = double(n)), type(y = double(m)))
    cbind(NULL, x, y, 1, NULL, deparse.level = 0)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(c(1, 2), c(3, 4)))
  expect_error(quick(fn)(c(1, 2), c(3, 4, 5)), "matching dimensions")
})

test_that("codegen: rbind checks independent NA column extents", {
  fn <- function(A, B) {
    declare(type(A = double(2, NA)), type(B = double(1, NA)))
    rbind(A, B, deparse.level = 0)
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)
  cases <- list(
    list(matrix(as.double(1:6), 2), matrix(as.double(7:9), 1)),
    list(matrix(double(), 2, 0), matrix(double(), 1, 0))
  )
  for (args in cases) {
    expect_identical(unname(do.call(qfn, args)), unname(do.call(fn, args)))
  }
})

test_that("codegen: cbind checks a dynamic extent against a fixed extent", {
  fn <- function(A, B) {
    declare(type(A = double(NA, 2)), type(B = double(3, 1)))
    cbind(A, B, deparse.level = 0)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(matrix(as.double(1:6), 3), matrix(as.double(7:9), 3))
  )
})

test_that("codegen: input shape proofs remove elementwise error machinery", {
  fn <- function(x, y) {
    declare(type(x = double(n, m)), type(y = double(n, m)))
    (x + 1) + (y * 2)
  }
  expect_translation_snapshots(
    fn,
    note = "Shared extents are checked in C; derived operands retain that proof and need no Fortran shape or nonempty guard."
  )
  x <- matrix(as.double(1:6), 2)
  expect_quick_identical(
    fn,
    list(x, x + 1),
    list(matrix(double(), 0, 3), matrix(double(), 0, 3))
  )
})

test_that("codegen: independent matrix shapes retain equality guards", {
  fn <- function(x, y) {
    declare(type(x = double(NA, NA)), type(y = double(NA, NA)))
    x + y
  }
  expect_translation_snapshots(
    fn,
    note = "Each stored extent is compared at runtime; matching empty axes remain valid."
  )
  x <- matrix(as.double(1:6), 2)
  expect_quick_identical(
    fn,
    list(x, x + 1),
    list(matrix(double(), 0, 3), matrix(double(), 0, 3))
  )
  expect_error(quick(fn)(x, t(x)), "matching dimensions")
})

test_that("codegen: vector-matrix broadcasting uses the row extent", {
  fn <- function(x, A) {
    declare(type(x = double(n)), type(A = double(n, m)))
    x + A
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(c(1, 2), matrix(as.double(1:6), 2)),
    list(double(), matrix(double(), 0, 3))
  )
})

test_that("codegen: scalar broadcasts preserve empty rank-three arrays", {
  fn <- function(x) {
    declare(type(x = double(NA, NA, NA)))
    x + 1
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(array(as.double(1:12), c(2, 3, 2))),
    list(array(double(), c(2, 0, 3)))
  )
})

test_that("codegen: known empty results preserve numeric and logical modes", {
  fn <- function() {
    values <- numeric(0) + 1
    comparison <- 1 < numeric(0)
    list(values = values, comparison = comparison)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list())
})

test_that("codegen: a known empty operand checks an independent length", {
  fn <- function(x) {
    declare(type(x = double(n)))
    numeric(0) + x
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, double())
  expect_error(quick(fn)(c(1, 2)), "equal lengths")
})

test_that("codegen: concatenation flattens matrix inputs column-major", {
  fn <- function(A, x) {
    declare(type(A = double(2, 2)), type(x = double(3)))
    c(A, x)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(matrix(as.double(1:4), 2), c(5, 6, 7)))
})

test_that("codegen: flattening logical storage drops dimensions", {
  fn <- function(A) {
    declare(type(A = logical(2, 3)))
    as.integer(A)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), 2))
  )
})

test_that("codegen: array reshaping converts and reshapes nonscalar data", {
  fn <- function(x) {
    declare(type(x = integer(2, 3, 4)))
    array(as.double(x), dim = c(2, 3, 4))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(array(1:24, c(2, 3, 4))))
})

test_that("codegen: nested array fills broadcast and guard empty data", {
  fn <- function(k, n, m) {
    declare(
      type(k = integer(1)),
      type(n = integer(1)),
      type(m = integer(1))
    )
    array(numeric(k), dim = c(n, m))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(2L, 2L, 3L), list(0L, 0L, 3L))
  expect_error(
    quick(fn)(0L, 2L, 3L),
    "array() with empty data would produce NA values",
    fixed = TRUE
  )
})

test_that("codegen: nested matrix fills broadcast and guard empty sources", {
  fn <- function(k, n, m) {
    declare(
      type(k = integer(1)),
      type(n = integer(1)),
      type(m = integer(1))
    )
    matrix(integer(k), nrow = n, ncol = m)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(2L, 2L, 3L), list(0L, 0L, 3L))
  expect_error(
    quick(fn)(0L, 2L, 3L),
    "matrix() with empty data would produce NA values",
    fixed = TRUE
  )
})

test_that("codegen: reversing an expression captures it before slicing", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    rev(x + 1)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(1, 2, 3), double())
})

test_that("codegen: reversing logical inputs preserves integer storage", {
  fn <- function(x) {
    declare(type(x = logical(NA)))
    rev(x)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(TRUE, FALSE, NA), logical())
})

test_that("codegen: dynamic sequences lower ascending and descending steps", {
  fn <- function(start, end) {
    declare(type(start = integer(1)), type(end = integer(1)))
    out <- sum(seq(start, end))
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(-1L, 4L), list(4L, -1L))
})

test_that("codegen: dynamic sequence subscripts form array sections", {
  fn <- function(x, start, end) {
    declare(
      type(x = double(5)),
      type(start = integer(1)),
      type(end = integer(1))
    )
    out <- x[seq(start, end)]
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(as.double(1:5), 2L, 4L),
    list(as.double(1:5), 4L, 2L)
  )
})

test_that("codegen: repeated subscripts support a runtime count", {
  fn <- function(x, n) {
    declare(type(x = double(5)), type(n = integer(1)))
    x[rep.int(2L, n)]
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(as.double(1:5), 3L), list(as.double(1:5), 0L))
})

test_that("codegen: integer vector subscripts gather values", {
  fn <- function(x, i) {
    declare(type(x = double(n)), type(i = integer(k)))
    x[i]
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(c(10, 20, 30, 40), c(3L, 1L, 3L)),
    list(double(), integer())
  )
})

test_that("codegen: matrix linear subscripts follow column-major storage", {
  fn <- function(x, i) {
    declare(type(x = double(n, m)), type(i = integer(1)))
    x[i]
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(matrix(as.double(1:6), 2), 3L))
})

test_that("codegen: length-one ranges preserve or drop result rank", {
  fn <- function(x) {
    declare(type(x = double(2, 3)))
    kept <- x[1:1, 1:3, drop = FALSE]
    dropped <- x[1:1, 1:3]
    list(kept = kept, dropped = dropped)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(matrix(as.double(1:6), 2)))
})
