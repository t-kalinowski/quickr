skip_on_cran()

test_that("nested zero fills retain types, shapes, and length errors", {
  before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(before))

  for (mode in c("logical", "integer", "double", "numeric")) {
    fill <- get(mode, envir = baseenv())
    for (constructor in c("array", "matrix")) {
      value <- if (constructor == "array") {
        quote(array(FILL(k), dim = c(n, m)))
      } else {
        quote(matrix(FILL(k), nrow = n, ncol = m))
      }
      fn <- eval(substitute(
        function(k, n, m) {
          declare(
            type(k = integer(1)),
            type(n = integer(1)),
            type(m = integer(1))
          )
          VALUE
        },
        list(VALUE = substitute_(value, list(FILL = as.symbol(mode))))
      ))
      qfn <- quick(fn)
      cases <- list(
        list(1L, 1L, 1L),
        list(2L, 2L, 3L),
        list(12L, 2L, 3L),
        list(0L, 0L, 3L),
        list(0L, 2L, 0L),
        list(2L, 0L, 0L)
      )
      for (args in cases) {
        # Exact-size data keeps the oracle valid with _R_CHECK_MATRIX_DATA_=true.
        dims <- c(args[[2L]], args[[3L]])
        expected <- array(fill(prod(dims)), dim = dims)
        expect_identical(do.call(qfn, args), expected)
      }
      expect_error(qfn(-1L, 0L, 3L), "invalid 'length' argument", fixed = TRUE)
      expect_error(qfn(0L, 1L, 1L), "empty data would produce NA values")

      code <- as.character(r2f(fn))
      expect_no_match(code, "reshape(", fixed = TRUE)
      expect_no_match(code, "allocate(", fixed = TRUE)
    }
  }
})

test_that("nested logical fills broadcast into rank-three results", {
  fn <- function(k, n) {
    declare(type(k = integer(1)), type(n = integer(1)))
    array(logical(k), dim = c(2L, n, 3L))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(2L, 4L), list(0L, 0L))
})

test_that("known nonempty fills need no source storage or error machinery", {
  fn <- function() {
    matrix(integer(2L), nrow = 2L, ncol = 3L)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list())
})

test_that("nested fills with scalar targets need no source indexing", {
  fn <- function(k) {
    declare(type(k = integer(1)))
    sum(array(integer(k), dim = 1L) + 1L)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, 1L, 2L, 9L)
})

test_that("nested fills materialize their result for array consumers", {
  fn <- function(k, n) {
    declare(type(k = integer(1)), type(n = integer(1)))
    sum(array(integer(k), dim = c(2L, n)) + 1L) +
      sum(matrix(double(k), nrow = n, ncol = 2L) + 1)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(2L, 3L), list(1L, 1L), list(0L, 0L))
})

test_that("nested fills resize deferred destinations without a source array", {
  fn <- function(k, n, m) {
    declare(
      type(k = integer(1)),
      type(n = integer(1)),
      type(m = integer(1)),
      type(out = double(NA, NA))
    )
    out <- matrix(1, nrow = 2L, ncol = 3L)
    first <- sum(out)
    out <- array(numeric(k), dim = c(n, m))
    first + sum(out + 1)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(2L, 2L, 3L),
    list(1L, 3L, 4L),
    list(0L, 0L, 3L),
    list(0L, 2L, 0L)
  )
})

test_that("fill guards retain effects before an invalid constructor", {
  fn <- function(k) {
    declare(type(k = integer(1)))
    draw <- runif(1L)
    out <- array(numeric(k), dim = c(2L, 2L))
    draw + sum(out)
  }
  before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(before))
  qfn <- quick(fn)
  for (k in c(-1L, 0L)) {
    set.seed(142)
    runif(1L)
    expected <- runif(1L)
    set.seed(142)
    expect_error(qfn(k), "invalid 'length'|empty data")
    expect_identical(runif(1L), expected)
  }
})

test_that("nested fills resize captured storage through superassignment", {
  fn <- function(n) {
    declare(type(n = integer(1)), type(out = double(NA, NA)))
    out <- matrix(1, nrow = 2L, ncol = 3L)
    reset_matrix <- function() {
      out <<- matrix(numeric(n), nrow = n, ncol = 2L)
      sum(out + 1)
    }
    reset_array <- function() {
      out <<- array(numeric(n), dim = c(2L, n))
      sum(out + 2)
    }
    first <- reset_matrix()
    first + reset_array()
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, 0L, 1L, 4L)
})

test_that("local constructors retain their values and evaluation count", {
  fn <- function() {
    calls <- 0L
    numeric <- function() {
      calls <<- calls + 1L
      c(2, 5)
    }
    a <- array(numeric(), dim = c(1L, 2L))
    b <- matrix(numeric(), nrow = 2L, ncol = 2L)
    list(a = a, b = b, calls = calls)
  }
  expect_quick_identical(fn, list())
})
