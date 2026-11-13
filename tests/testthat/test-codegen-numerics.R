skip_on_cran()

# Native behavior is covered by test-matrix-lapack.R, test-svd.R,
# test-captured-storage-lifetimes.R, test-reduction-scalars.R,
# test-subset-reduction.R, test-which.R, test-type-promotion.R, and
# test-hoist-unless-name.R. These snapshots expose the distinct numeric
# lowering strategies exercised by those regressions.

test_that("diag extracts the shorter axis of a rectangular matrix", {
  fn <- function(a) {
    declare(type(a = double(m, n)))
    diag(a)
  }
  expect_translation_snapshots(fn)
})

test_that("diag constructs a matrix from vector values", {
  fn <- function(x) {
    declare(type(x = double(n)))
    diag(x)
  }
  expect_translation_snapshots(fn)
})

test_that("diag broadcasts a scalar over a rectangular diagonal", {
  fn <- function(x) {
    declare(type(x = double(1)))
    diag(x, nrow = 2, ncol = 3)
  }
  expect_translation_snapshots(fn)
})

test_that("diag constructs an identity with a runtime size", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    diag(nrow = n)
  }
  expect_translation_snapshots(fn)
})

test_that("SVD returns all three components through a named C list", {
  fn <- function(x) {
    declare(type(x = double(3, 2)))
    result <- svd(x)
    result
  }
  expect_translation_snapshots(fn)
})

test_that("captured SVD components retain stored dimensions", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    result <- svd(x)
    inner <- function(n) sum(result$u %*% result$u)
    inner(1L)
  }
  expect_translation_snapshots(fn)
})

test_that("scalar extrema use min and max without array reduction", {
  fn <- function(m) {
    declare(type(m = integer(2, 2)))
    lo <- 1L
    hi <- 2L
    min(max(m[1, 1], lo), hi)
  }
  expect_translation_snapshots(fn)
})

test_that("retained singleton dimensions use an array reduction", {
  fn <- function(a, m) {
    declare(type(a = integer(1)), type(m = integer(NA, 2)))
    min(m[a, 1L, drop = FALSE])
  }
  expect_translation_snapshots(fn)
})

test_that("numeric reductions emit identities with no data arguments", {
  fn <- function() {
    c(sum(), prod(na.rm = FALSE))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list())
})

test_that("logical reductions emit identities with no data arguments", {
  fn <- function() {
    c(any(na.rm = FALSE), all())
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list())
})

test_that("named numeric data and exact na.rm preserve multi-argument reductions", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    c(
      sum(first = x, na.r = y, na.rm = FALSE),
      prod(first = x, na.rm = FALSE, second = y)
    )
  }
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(c(1, 2, 3), c(4, 5)), list(numeric(), c(4, 5)))
})

test_that("named logical reductions combine independently reduced arguments", {
  fn <- function(x, y) {
    declare(type(x = logical(NA)), type(y = logical(NA)))
    c(
      any(first = x, na.rm = FALSE, second = y),
      all(first = x, na.rm = FALSE, second = y)
    )
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(c(TRUE, FALSE), c(FALSE, TRUE)),
    list(logical(), c(TRUE, TRUE))
  )
})

test_that("sum and extrema cast logical data before calling intrinsics", {
  fn <- function(x) {
    declare(type(x = logical(n)))
    c(sum(x), min(x), max(x))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(TRUE, FALSE, TRUE), c(TRUE, TRUE))
})

test_that("logical scalar and singleton reductions use their value directly", {
  fn <- function(x) {
    declare(type(x = logical(1)))
    c(any(x), all(c(x)))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, TRUE, FALSE)
})

test_that("extrema discard a statically empty argument", {
  fn <- function() {
    min(numeric(), 5)
  }
  expect_translation_snapshots(fn)
})

test_that("extrema track whether any dynamic argument is nonempty", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    min(x, y)
  }
  expect_translation_snapshots(fn)
})

test_that("numeric masked reductions pass masks to Fortran intrinsics", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    c(sum(x[x > 0]), prod(x[x > 0]))
  }
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, c(-2, 1, 3, -4, 5), numeric(), c(-2, -1))
})

test_that("logical masked reductions avoid packing the selected array", {
  fn <- function(x, mask) {
    declare(type(x = logical(n)), type(mask = logical(n)))
    c(any(x[mask]), all(x[mask]))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(c(TRUE, FALSE), c(TRUE, FALSE)),
    list(c(TRUE, FALSE), c(FALSE, FALSE)),
    list(logical(), logical())
  )
})

test_that("scalar masked reductions retain empty-selection identities", {
  fn <- function(x, mask) {
    declare(type(x = logical(1)), type(mask = logical(1)))
    c(any(x[mask]), all(x[c(mask)]))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(TRUE, FALSE), list(FALSE, TRUE))
})

test_that("singleton extrema indices still evaluate effectful operands", {
  fn <- function() {
    hi <- which.max(c(runif(1L)))
    lo <- which.min(c(runif(1L)))
    c(hi, lo, runif(1L))
  }
  expect_translation_snapshots(fn)

  before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(before))
  compiled <- quick(fn)
  expect_identical(set_seed_and_call(compiled), set_seed_and_call(fn))
})

test_that("extrema indices search logical storage and boolean expressions", {
  fn <- function(x) {
    declare(type(x = double(n)))
    positive <- x > 0
    c(
      which.max(positive),
      which.min(positive),
      which.max(x > 1),
      which.min(x > 1)
    )
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(-1, 0, 2), c(2, 3, 4), c(-3, -2, -1))
})

test_that("multi-argument extrema materialize operands in evaluation order", {
  fn <- function(x) {
    declare(type(x = double(n)))
    max(runif(1), x + runif(3))
  }
  expect_translation_snapshots(fn)
})

test_that("ceiling evaluates a reused effectful operand once", {
  fn <- function() {
    out <- ceiling(runif(1L) * 10)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("floor and ceiling reuse names without introducing temporaries", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- floor(x) + ceiling(x)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("derived constants remain runtime math operands", {
  fn <- function() {
    c(
      sqrt(floor(0.5) - 1),
      log10(floor(0.5)),
      exp(floor(1000.5)),
      (floor(0.5) - 1)^0.5
    )
  }
  expect_translation_snapshots(fn)
  before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(before))
  expect_identical(quick(fn)(), suppressWarnings(fn()))
})
