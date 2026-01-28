test_that("quick() supports svd(x)$d", {
  fn <- function(x) {
    declare(type(x = double(3, 2)))
    svd(x)$d
  }

  expect_translation_snapshots(fn)

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
