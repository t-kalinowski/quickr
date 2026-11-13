skip_on_cran()

test_that("quick() supports svd(x)$d", {
  fn <- function(x) {
    declare(type(x = double(3, 2)))
    svd(x)$d
  }

  expect_translation_snapshots(fn)

  code <- paste(as.character(r2f(fn)), collapse = "\n")
  expect_match(code, "call dgesdd('N'", fixed = TRUE)
  expect_false(grepl("transpose(", code, fixed = TRUE))

  x <- matrix(c(3, 0, 0, 0, 2, 0), nrow = 3)
  qfn <- quick(fn)
  expect_equal(qfn(x), svd(x)$d, tolerance = 1e-12)
})

test_that("quick() supports assignment of svd() results", {
  fn <- function(x) {
    declare(type(x = double(3, 2)))
    s <- svd(x)
    s$d
  }

  x <- matrix(c(3, 0, 0, 0, 2, 0), nrow = 3)
  qfn <- quick(fn)
  expect_equal(qfn(x), svd(x)$d, tolerance = 1e-12)
})

test_that("svd(x)$d preserves dynamic shapes and its input", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    svd(x)$d
  }
  expect_translation_snapshots(fn)

  before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(before))
  qfn <- quick(fn)
  set.seed(714)
  inputs <- list(
    matrix(rnorm(24), 8, 3),
    matrix(rnorm(24), 3, 8),
    matrix(rnorm(16), 4, 4),
    matrix(rnorm(4), 1, 4),
    matrix(rnorm(4), 4, 1),
    matrix(3, 1, 1),
    outer(as.double(1:4), as.double(1:3)),
    matrix(0, 3, 2)
  )
  for (x in inputs) {
    original <- x + 0
    expected <- svd(x)$d
    for (i in seq_len(2)) {
      result <- qfn(x)
      expect_equal(result, expected, tolerance = 1e-12)
      expect_identical(typeof(result), "double")
      expect_null(dim(result))
      expect_identical(x, original)
    }
  }
  for (dims in list(c(3L, 0L), c(0L, 3L), c(0L, 0L))) {
    x <- matrix(numeric(), dims[[1L]], dims[[2L]])
    expect_error(qfn(x), "[Dd][Gg][Ee][Ss][Dd][Dd]")
  }
})

test_that("svd(x)$d evaluates an effectful operand once", {
  fn <- function(x) {
    declare(type(x = double(3, 2)))
    svd(x + runif(1))$d
  }
  before <- loaded_dll_paths()
  on.exit(cleanup_new_quick_dlls(before))
  qfn <- quick(fn)
  x <- matrix(as.double(1:6), 3, 2)
  set.seed(715)
  expected <- fn(x)
  expected_seed <- .Random.seed
  set.seed(715)
  expect_equal(qfn(x), expected, tolerance = 1e-12)
  expect_identical(.Random.seed, expected_seed)
})

test_that("values-only and full SVD calls can share a generated function", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    values <- svd(x)$d
    result <- svd(x)
    values + result$d
  }
  expect_quick_equal(fn, matrix(as.double(1:12), 4, 3))
})

test_that("quick() errors when svd() is used without $d/$u/$v", {
  expect_error(
    quick(function(x) {
      declare(type(x = double(3, 2)))
      svd(x)
      sum(x)
    }),
    "svd\\(\\) must be assigned to a symbol or accessed via \\$d, \\$u, \\$v",
    fixed = FALSE
  )
})

test_that("quick() returns svd() results as a named list", {
  fn <- function(x) {
    declare(type(x = double(3, 2)))
    s <- svd(x)
    s
  }

  x <- matrix(c(3, 0, 0, 0, 2, 0), nrow = 3)
  out <- quick(fn)(x)
  expect_named(out, c("d", "u", "v"))
  expect_equal(out$d, svd(x)$d)
  recon <- out$u %*% diag(out$d, nrow = length(out$d)) %*% t(out$v)
  expect_equal(recon, x, tolerance = 1e-8)
})
