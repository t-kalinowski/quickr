test_that("[ handles scalar, missing, and logical subscripts", {
  m <- matrix(1:6, nrow = 2L, ncol = 3L, byrow = TRUE)

  scalar_drop <- function(m) {
    declare(type(m = integer(NA, NA)))
    m[1L, 1L]
  }
  expect_quick_identical(scalar_drop, list(m))

  scalar_keep <- function(m) {
    declare(type(m = integer(NA, NA)))
    m[1L, 1L, drop = FALSE]
  }
  expect_quick_identical(scalar_keep, list(m))

  whole_col_missing <- function(m) {
    declare(type(m = integer(NA, NA)))
    m[, 2L]
  }
  expect_quick_identical(whole_col_missing, list(m))

  whole_col_logical0 <- function(m) {
    declare(type(m = integer(NA, NA)))
    m[TRUE, 2L]
  }
  expect_quick_identical(whole_col_logical0, list(m))

  logical_rows_sum <- function(m) {
    declare(type(m = integer(NA, NA)))
    sum(m[c(TRUE, FALSE), ])
  }
  expect_quick_identical(logical_rows_sum, list(m))

  pack_filter_sum <- function(m) {
    declare(type(m = integer(NA, NA)))
    sum(m[m > 3L])
  }
  expect_quick_identical(pack_filter_sum, list(m))
})


test_that("reduction intrinsics cover scalar, multi-arg, and mask cases", {
  min_two <- function(x, y) {
    declare(type(x = integer(NA)), type(y = integer(NA)))
    min(x, y)
  }
  fsub <- r2f(min_two)
  expect_match(as.character(fsub), "minval\\(")
  expect_quick_identical(min_two, list(1:4, 2:5))

  min_mix_scalar <- function(x) {
    declare(type(x = integer(NA)))
    min(x, 0L)
  }
  fsub <- r2f(min_mix_scalar)
  expect_match(as.character(fsub), "minval\\(")
  expect_quick_identical(min_mix_scalar, list(c(3L, -1L, 2L)))

  sum_masked <- function(x) {
    declare(type(x = double(NA)))
    sum(x[x > 0])
  }
  fsub <- r2f(sum_masked)
  expect_match(as.character(fsub), "sum\\(x, mask =")
  expect_quick_equal(sum_masked, list(c(-2, 1, 3, -4, 5)))

  prod_two <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    prod(x, y)
  }
  fsub <- r2f(prod_two)
  expect_match(as.character(fsub), "product\\(")
  expect_quick_equal(prod_two, list(c(1, 2, 3), c(4, 5, 6)))
})


test_that("1x1 subsetting keeps dims and C bridge builds", {
  fn <- function(m) {
    declare(type(m = integer(NA, NA)))
    m[1L, 1L, drop = FALSE]
  }

  fsub <- r2f(fn)
  expect_no_error(make_c_bridge(fsub))
})
