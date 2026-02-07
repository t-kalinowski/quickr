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

test_that("any/all reduction intrinsics cover scalar, multi-arg, and mask cases", {
  any_basic <- function(x) {
    declare(type(x = logical(NA)))
    any(x)
  }
  fsub <- r2f(any_basic)
  expect_match(as.character(fsub), "any\\(")
  expect_quick_identical(any_basic, list(c(FALSE, FALSE)), list(c(FALSE, TRUE)))

  all_basic <- function(x) {
    declare(type(x = logical(NA)))
    all(x)
  }
  fsub <- r2f(all_basic)
  expect_match(as.character(fsub), "all\\(")
  expect_quick_identical(all_basic, list(c(TRUE, TRUE)), list(c(TRUE, FALSE)))

  any_two <- function(a, b) {
    declare(type(a = logical(NA)), type(b = logical(NA)))
    any(a, b)
  }
  fsub <- r2f(any_two)
  expect_match(as.character(fsub), "\\.or\\.")
  expect_quick_identical(
    any_two,
    list(a = c(FALSE, FALSE), b = c(FALSE, FALSE)),
    list(a = c(FALSE, FALSE), b = c(TRUE, FALSE))
  )

  all_two <- function(a, b) {
    declare(type(a = logical(NA)), type(b = logical(NA)))
    all(a, b)
  }
  fsub <- r2f(all_two)
  expect_match(as.character(fsub), "\\.and\\.")
  expect_quick_identical(
    all_two,
    list(a = c(TRUE, TRUE), b = c(TRUE, TRUE)),
    list(a = c(TRUE, TRUE), b = c(FALSE, TRUE))
  )

  # Scalar masked subset: preserve empty-selection semantics.
  any_scalar_masked_empty <- function(x) {
    declare(type(x = logical(1)))
    any(x[c(FALSE)])
  }
  expect_quick_identical(any_scalar_masked_empty, list(TRUE), list(FALSE))

  all_scalar_masked_empty <- function(x) {
    declare(type(x = logical(1)))
    all(x[c(FALSE)])
  }
  expect_quick_identical(all_scalar_masked_empty, list(TRUE), list(FALSE))

  # Scalar masked subset with a scalar mask variable: should compile and
  # preserve empty-selection semantics.
  any_scalar_masked_var <- function(x, m) {
    declare(type(x = logical(1)), type(m = logical(1)))
    any(x[m])
  }
  expect_quick_identical(
    any_scalar_masked_var,
    list(x = TRUE, m = FALSE),
    list(x = FALSE, m = TRUE)
  )

  all_scalar_masked_var <- function(x, m) {
    declare(type(x = logical(1)), type(m = logical(1)))
    all(x[m])
  }
  expect_quick_identical(
    all_scalar_masked_var,
    list(x = FALSE, m = FALSE),
    list(x = FALSE, m = TRUE)
  )

  # Scalar `x` with a longer logical mask diverges from simple empty/non-empty
  # semantics in R because selecting out-of-range positions yields NAs. We don't
  # implement that behavior here, so fail fast instead of silently emitting
  # wrong code.
  any_scalar_masked_long_mask <- function(x) {
    declare(type(x = logical(1)))
    any(x[c(FALSE, TRUE)])
  }
  expect_error(r2f(any_scalar_masked_long_mask), "scalar masked subsets")

  all_scalar_masked_long_mask <- function(x) {
    declare(type(x = logical(1)))
    all(x[c(FALSE, TRUE)])
  }
  expect_error(r2f(all_scalar_masked_long_mask), "scalar masked subsets")

  # 1-element vector expressions like c(FALSE) compile to Fortran array
  # constructors (`[.false.]`) but any()/all() must still return scalars.
  any_array_ctor_len1 <- function() {
    any(c(FALSE))
  }
  expect_quick_identical(any_array_ctor_len1, list())

  all_array_ctor_len1 <- function() {
    all(c(TRUE))
  }
  expect_quick_identical(all_array_ctor_len1, list())

  # Masked reduction over a 1-element array constructor: `[` can hoist the mask
  # while leaving `x` as a rank-1 array constructor (`[.true.]`), but the
  # reduction result must still be scalar.
  any_array_ctor_len1_masked <- function() {
    any(c(TRUE)[c(TRUE)])
  }
  expect_quick_identical(any_array_ctor_len1_masked, list())

  all_array_ctor_len1_masked_empty <- function() {
    all(c(TRUE)[c(FALSE)])
  }
  expect_quick_identical(all_array_ctor_len1_masked_empty, list())

  # Length-1 mask constructors (c(TRUE)/c(FALSE)) are hoisted as rank-1 array
  # constructors but should behave like scalars via recycling in R. The lowering
  # must scalarize them so elementwise `.and.`/`.or.` remain conformable.
  any_mask_ctor_len1 <- function(x) {
    declare(type(x = double(NA)))
    pred <- x > 1
    any(pred[c(TRUE)])
  }
  fsub <- r2f(any_mask_ctor_len1)
  expect_false(grepl("\\.and\\. \\(\\[", as.character(fsub)))
  expect_quick_identical(
    any_mask_ctor_len1,
    list(c(-2, -1)),
    list(c(0.2, 2))
  )

  all_mask_ctor_len1_empty <- function(x) {
    declare(type(x = double(NA)))
    pred <- x > 1
    all(pred[c(FALSE)])
  }
  fsub <- r2f(all_mask_ctor_len1_empty)
  expect_false(grepl("\\.not\\. \\(\\[", as.character(fsub)))
  expect_false(grepl("\\.or\\. \\(\\[", as.character(fsub)))
  expect_quick_identical(
    all_mask_ctor_len1_empty,
    list(c(-2, -1)),
    list(c(0.2, 2))
  )

  # Masked subset: preserve empty-selection semantics without pack() temporaries.
  any_masked <- function(x) {
    declare(type(x = double(NA)))
    pred <- x > 1
    any(pred[x > 0])
  }
  fsub <- r2f(any_masked)
  expect_false(grepl("pack\\(", as.character(fsub)))
  expect_match(as.character(fsub), "\\.and\\.")
  expect_match(as.character(fsub), "any\\(")
  expect_quick_identical(
    any_masked,
    list(c(-2, -1)), # empty selection -> any(logical(0)) == FALSE
    list(c(0.2, 0.3)), # selection all FALSE
    list(c(0.5, 2)) # selection contains TRUE
  )

  all_masked <- function(x) {
    declare(type(x = double(NA)))
    pred <- x > 1
    all(pred[x > 0])
  }
  fsub <- r2f(all_masked)
  expect_false(grepl("pack\\(", as.character(fsub)))
  expect_match(as.character(fsub), "\\.not\\.")
  expect_match(as.character(fsub), "\\.or\\.")
  expect_match(as.character(fsub), "all\\(")
  expect_quick_identical(
    all_masked,
    list(c(-2, -1)), # empty selection -> all(logical(0)) == TRUE
    list(c(0.2, 0.3)), # selection all FALSE -> FALSE
    list(c(0.5, 2)) # selection has a FALSE -> FALSE
  )
})


test_that("1x1 subsetting keeps dims and C bridge builds", {
  fn <- function(m) {
    declare(type(m = integer(NA, NA)))
    m[1L, 1L, drop = FALSE]
  }

  fsub <- r2f(fn)
  expect_no_error(make_c_bridge(fsub))
})
