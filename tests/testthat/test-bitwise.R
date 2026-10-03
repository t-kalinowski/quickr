# Unit tests for bitwise operations: bitwAnd, bitwOr, bitwXor, bitwNot,
# bitwShiftL, bitwShiftR

skip_on_cran()

# Edge bit patterns: zero, all ones, extremes of the (non-NA) integer range,
# alternating bits, and small positive/negative values.
bitw_values <- c(
  0L,
  1L,
  -1L,
  .Machine$integer.max,
  -.Machine$integer.max,
  0x55555555L,
  -0x55555556L,
  0x0F0F0F0FL,
  7L,
  -8L,
  12345L,
  -98765L
)

test_that("bitwAnd, bitwOr, and bitwXor match R on integer vectors", {
  grid <- expand.grid(a = bitw_values, b = bitw_values)
  grid <- list(a = grid$a, b = grid$b)

  bitw_and <- function(a, b) {
    declare(type(a = integer(NA)), type(b = integer(NA)))
    bitwAnd(a, b)
  }
  expect_translation_snapshots(bitw_and)
  expect_quick_identical(bitw_and, grid)

  bitw_or <- function(a, b) {
    declare(type(a = integer(NA)), type(b = integer(NA)))
    bitwOr(a, b)
  }
  expect_quick_identical(bitw_or, grid)

  bitw_xor <- function(a, b) {
    declare(type(a = integer(NA)), type(b = integer(NA)))
    bitwXor(a, b)
  }
  expect_quick_identical(bitw_xor, grid)
})

test_that("binary bitw ops broadcast a scalar operand on either side", {
  scalar_right <- function(a, b) {
    declare(type(a = integer(NA)), type(b = integer(1)))
    bitwAnd(a, b)
  }
  expect_quick_identical(
    scalar_right,
    list(bitw_values, 0x0F0F0F0FL),
    list(bitw_values, -1L)
  )

  scalar_left <- function(a, b) {
    declare(type(a = integer(1)), type(b = integer(NA)))
    bitwXor(a, b)
  }
  expect_quick_identical(
    scalar_left,
    list(0x55555555L, bitw_values),
    list(0L, bitw_values)
  )

  scalars <- function(a, b) {
    declare(type(a = integer(1)), type(b = integer(1)))
    bitwOr(a, b)
  }
  expect_quick_identical(scalars, list(5L, 10L), list(-8L, 3L))

  literal <- function(a) {
    declare(type(a = integer(NA)))
    bitwAnd(a, 255L)
  }
  expect_quick_identical(literal, list(bitw_values))
})

test_that("bitwNot matches R", {
  bitw_not <- function(a) {
    declare(type(a = integer(NA)))
    bitwNot(a)
  }
  expect_translation_snapshots(bitw_not)
  expect_quick_identical(bitw_not, list(bitw_values))

  bitw_not_scalar <- function(a) {
    declare(type(a = double(1)))
    bitwNot(a)
  }
  expect_quick_identical(bitw_not_scalar, list(0), list(5.9), list(-5.9))
})

test_that("double operands are truncated toward zero like as.integer()", {
  doubles <- c(0, 5.5, -5.5, 1.999, -1.999, 3, 2^30, -(2^31 - 1))

  dbl_dbl <- function(a, b) {
    declare(type(a = double(NA)), type(b = double(NA)))
    bitwAnd(a, b)
  }
  expect_quick_identical(dbl_dbl, list(doubles, rev(doubles)))

  dbl_int <- function(a, b) {
    declare(type(a = double(NA)), type(b = integer(NA)))
    bitwXor(a, b)
  }
  expect_quick_identical(dbl_int, list(doubles, seq_along(doubles) - 4L))

  dbl_literal <- function(a) {
    declare(type(a = integer(NA)))
    bitwOr(a, 6)
  }
  expect_quick_identical(dbl_literal, list(bitw_values))

  # The result is always an integer
  qfn <- quick(dbl_dbl)
  expect_identical(typeof(qfn(5, 3)), "integer")
})

test_that("bitwShiftL and bitwShiftR match R across shift counts", {
  # Counts outside 0..31 give NA in R (the INT_MIN bit pattern); negative
  # values exercise the logical (unsigned) shift.
  n <- -3:35
  a <- rep_len(c(1L, -1L, 3L, .Machine$integer.max, -16L, 0x55555555L), 39L)

  shift_left <- function(a, n) {
    declare(type(a = integer(NA)), type(n = integer(NA)))
    bitwShiftL(a, n)
  }
  expect_translation_snapshots(shift_left)
  expect_quick_identical(shift_left, list(a, n))

  shift_right <- function(a, n) {
    declare(type(a = integer(NA)), type(n = integer(NA)))
    bitwShiftR(a, n)
  }
  expect_quick_identical(shift_right, list(a, n))

  scalar_count <- function(a, n) {
    declare(type(a = integer(NA)), type(n = integer(1)))
    bitwShiftR(a, n)
  }
  expect_quick_identical(
    scalar_count,
    list(bitw_values, 0L),
    list(bitw_values, 1L),
    list(bitw_values, 31L),
    list(bitw_values, 32L),
    list(bitw_values, -1L)
  )

  scalar_value <- function(a, n) {
    declare(type(a = integer(1)), type(n = integer(NA)))
    bitwShiftL(a, n)
  }
  expect_quick_identical(scalar_value, list(1L, n), list(3L, n))

  # Results that land on the sign bit are NA in R too
  expect_quick_identical(scalar_value, list(1L, 31L), list(3L, 31L))

  double_count <- function(a, n) {
    declare(type(a = double(NA)), type(n = double(1)))
    bitwShiftL(a, n)
  }
  expect_quick_identical(double_count, list(c(1, 2.5, -3.7), 2.9))

  # The count is evaluated once even though the expression uses it thrice
  expr_count <- function(a, n) {
    declare(type(a = integer(NA)), type(n = integer(1)))
    bitwShiftL(a, n * 2L + 1L)
  }
  expect_quick_identical(
    expr_count,
    list(bitw_values, 0L),
    list(bitw_values, 3L),
    list(bitw_values, 16L)
  )
})

test_that("array operands drop their dims, as in R", {
  mat_scalar <- function(x) {
    declare(type(x = integer(3, 2)))
    bitwAnd(x, 6L)
  }
  expect_quick_identical(mat_scalar, list(matrix(1:6, 3)))

  mat_vec <- function(x, y) {
    declare(type(x = double(3, 2)), type(y = integer(6)))
    bitwXor(x, y)
  }
  expect_quick_identical(
    mat_vec,
    list(matrix(c(1.5, -2.7, 3, 4, 5, 6), 3), 1:6)
  )

  mat_mat <- function(x, y) {
    declare(type(x = integer(n, k)), type(y = integer(n, k)))
    bitwOr(x, y)
  }
  expect_quick_identical(
    mat_mat,
    list(matrix(1:6, 2), matrix(6:1, 2)),
    list(matrix(bitw_values, 3), matrix(rev(bitw_values), 3))
  )

  mat_shift <- function(x, n) {
    declare(type(x = integer(NA, NA)), type(n = integer(1)))
    bitwShiftL(x, n)
  }
  expect_quick_identical(mat_shift, list(matrix(bitw_values, 4), 2L))

  mat_not <- function(x) {
    declare(type(x = integer(2, 2, 2)))
    bitwNot(x)
  }
  expect_quick_identical(mat_not, list(array(1:8, c(2, 2, 2))))

  # A 1x1 matrix is a scalar operand once its dims are dropped
  one_by_one <- function(x, y) {
    declare(type(x = integer(1, 1)), type(y = integer(NA)))
    bitwAnd(x, y)
  }
  expect_quick_identical(one_by_one, list(matrix(5L), 1:3))
})

test_that("named and reordered arguments are matched like R", {
  named <- function(x, k) {
    declare(type(x = integer(NA)), type(k = integer(1)))
    bitwShiftL(n = k, a = x)
  }
  expect_quick_identical(named, list(bitw_values, 3L))

  named_binary <- function(x, y) {
    declare(type(x = integer(NA)), type(y = integer(NA)))
    bitwAnd(b = y, a = x)
  }
  expect_quick_identical(named_binary, list(1:8, 8:1))
})

test_that("bitw ops compose inside loops", {
  popcount <- function(x) {
    declare(type(x = integer(NA)))
    out <- integer(length(x))
    for (i in seq_along(x)) {
      v <- x[i]
      count <- 0L
      for (b in 0:31) {
        count <- count + bitwAnd(bitwShiftR(v, b), 1L)
      }
      out[i] <- count
    }
    out
  }
  expect_quick_identical(popcount, list(bitw_values))

  xorshift <- function(seed, n) {
    declare(type(seed = integer(1)), type(n = integer(1)))
    out <- integer(n)
    s <- seed
    for (i in seq_len(n)) {
      s <- bitwXor(s, bitwShiftL(s, 13L))
      s <- bitwXor(s, bitwShiftR(s, 17L))
      s <- bitwXor(s, bitwShiftL(s, 5L))
      out[i] <- s
    }
    out
  }
  expect_quick_identical(xorshift, list(123456789L, 200L))
})

test_that("bitw ops reject logical operands", {
  logical_arg <- function(a, b) {
    declare(type(a = logical(NA)), type(b = integer(NA)))
    bitwAnd(a, b)
  }
  expect_error(
    quick(logical_arg),
    "bitwAnd() requires integer or double arguments, not logical",
    fixed = TRUE
  )

  logical_not <- function(a) {
    declare(type(a = logical(1)))
    bitwNot(a)
  }
  expect_error(
    quick(logical_not),
    "bitwNot() requires integer or double arguments, not logical",
    fixed = TRUE
  )

  logical_count <- function(a, n) {
    declare(type(a = integer(NA)), type(n = logical(1)))
    bitwShiftL(a, n)
  }
  expect_error(
    quick(logical_count),
    "bitwShiftL() requires integer or double arguments, not logical",
    fixed = TRUE
  )
})

test_that("bitw ops require equal lengths or a scalar operand", {
  known <- function(a, b) {
    declare(type(a = integer(3)), type(b = integer(4)))
    bitwAnd(a, b)
  }
  expect_error(quick(known), "equal lengths")

  known_matrix <- function(a, b) {
    declare(type(a = integer(2, 3)), type(b = integer(4)))
    bitwOr(a, b)
  }
  expect_error(quick(known_matrix), "equal lengths")

  unknown <- function(a, n) {
    declare(type(a = integer(NA)), type(n = integer(NA)))
    bitwShiftL(a, n)
  }
  qfn <- quick(unknown)
  expect_identical(qfn(1:3, 1:3), bitwShiftL(1:3, 1:3))
  expect_error(qfn(1:3, 1:2), "equal lengths")
})
