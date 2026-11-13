skip_on_cran()

test_that("C snapshots show strict input checks and modified argument copies", {
  fn <- function(x, y) {
    declare(type(x = double(n)), type(y = double(n)))
    x <- x + 1
    list(changed = x, unchanged = y)
  }
  expect_translation_snapshots(
    fn,
    note = "Strict type checks; only the modified argument is duplicated."
  )
  expect_quick_identical(fn, list(c(1, 2), c(3, 4)), list(double(), double()))
})

test_that("C snapshots show coercion and copy branches in a non-strict bridge", {
  fn <- function(x, y) {
    declare(type(x = double(n)), type(y = double(n)))
    x <- x + 1
    list(changed = x, unchanged = y)
  }
  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub, strict = FALSE)
  expect_snapshot(
    {
      fn
      cat(fsub)
      cat(cwrapper)
    },
    transform = scrub_environment
  )

  # Exercise this internal bridge option through the normal compile path.
  original_bridge <- make_c_bridge
  local_mocked_bindings(
    make_c_bridge = function(fsub, ...) {
      original_bridge(fsub, strict = FALSE, ...)
    },
    .package = "quickr"
  )
  before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(before), add = TRUE)
  qfn <- quick(fn)
  x <- c(1, 2)
  y <- c(3, 4)
  expect_identical(qfn(x, y), fn(x, y))
  expect_identical(x, c(1, 2))
  expect_identical(y, c(3, 4))
  expect_identical(qfn(1:2, 3:4), fn(as.double(1:2), as.double(3:4)))
})

test_that("C snapshots show mixed output storage and array dimensions", {
  fn <- function(x, z) {
    declare(type(x = double(n)), type(z = complex(n)))
    values <- x + Re(z)
    keep <- x > 0
    shifted <- z + 1i
    matrix <- matrix(values, nrow = length(x), ncol = 1L)
    list(values = values, keep = keep, shifted = shifted, matrix = matrix)
  }
  expect_translation_snapshots(
    fn,
    note = "Real, logical, complex, and matrix outputs have separate C allocations."
  )
  expect_quick_identical(fn, list(c(-1, 0, 2), c(1 + 2i, 3 - 1i, 2 + 0i)))
})

test_that("C snapshots show guarded dynamic sequence lengths", {
  fn <- function(step) {
    declare(type(step = integer(1)))
    out <- seq(1L, 9L, by = step)
    out
  }
  expect_translation_snapshots(
    fn,
    note = "The C bridge validates the step before allocating the sequence output."
  )
  expect_quick_identical(fn, 2L, 4L)
  qfn <- quick(fn)
  expect_error(qfn(0L), "invalid '(to - from)/by'", fixed = TRUE)
  expect_error(qfn(-1L), "wrong sign in 'by' argument", fixed = TRUE)
})

test_that("C snapshots preserve floating operands in size expressions", {
  fn <- function(n) {
    declare(type(n = double(1)), type(out = double(as.integer(abs(n) + 1))))
    out <- double(as.integer(abs(n) + 1))
    out
  }
  expect_translation_snapshots(
    fn,
    note = "Rf_asReal and floating abs precede the final length conversion."
  )
  expect_quick_identical(fn, -2.5, 2.5, 0)
})
