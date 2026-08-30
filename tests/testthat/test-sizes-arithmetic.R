# Unit tests for size expression lowering/evaluation

skip_on_cran()

test_that("constant arithmetic in declared dims is evaluated", {
  fn <- function(x) {
    declare(type(x = double(2L + 3L)))
    sum(x)
  }
  qfn <- quick(fn)

  expect_error(
    qfn(as.double(1:4)),
    "length(x) must be 5, not 4",
    fixed = TRUE
  )
  expect_identical(qfn(as.double(1:5)), 15)

  fn_pow <- function(x) {
    declare(type(x = double(2L^3L)))
    sum(x)
  }
  qfn_pow <- quick(fn_pow)

  expect_error(
    qfn_pow(as.double(1:7)),
    "length(x) must be 8, not 7",
    fixed = TRUE
  )
  expect_identical(qfn_pow(as.double(1:8)), 36)
})

test_that("dynamic exponentiation works in declared dims", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- double(n^2L)
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  expect_quick_identical(fn, list(3L), list(4L))
})

test_that("size division keeps double precision until the final cast", {
  fn <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    double(as.integer(x / y))
  }

  code <- suppressWarnings(r2f(fn))
  expect_match(
    as.character(code),
    "real(x, kind=c_double) / real(y, kind=c_double)",
    fixed = TRUE
  )
  expect_match(as.character(code), "kind=c_ptrdiff_t", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(x)", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(y)", fixed = TRUE)

  expect_quick_identical(fn, list(1.99999999, 1))
})

test_that("size integer division evaluates numeric operands before casting", {
  fn <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    double(x %/% y)
  }

  code <- suppressWarnings(r2f(fn))
  expect_match(as.character(code), "aint(", fixed = TRUE)
  expect_match(as.character(code), "real(x, kind=c_double)", fixed = TRUE)
  expect_match(as.character(code), "real(y, kind=c_double)", fixed = TRUE)
  expect_match(code@c_bridge, "floor(", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(x)", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(y)", fixed = TRUE)

  qfn <- suppressWarnings(quick(fn))
  expect_identical(qfn(3.9, 1.9), fn(3.9, 1.9))
})

test_that("size integer division rounds negative quotients down", {
  fn <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    double((x %/% y) + 3L)
  }

  expect_quick_identical(fn, list(-3L, 2L))
})

test_that("size floor division remains real before outer arithmetic", {
  fn <- function(x, y, z) {
    declare(
      type(x = double(1)),
      type(y = double(1)),
      type(z = double(1))
    )
    out <- double(1)
    local <- double(as.integer((x %/% y) / z) + 10L)
    out[1] <- as.double(length(local))
    out
  }

  code <- r2f(fn)
  expect_match(as.character(code), "aint(", fixed = TRUE)
  expect_quick_identical(
    fn,
    list(1e20, 3, 1e19),
    list(-1e20, 3, 1e19)
  )

  returned <- function(x, y, z) {
    declare(
      type(x = double(1)),
      type(y = double(1)),
      type(z = double(1))
    )
    out <- double(as.integer((x %/% y) / z) + 10L)
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }
  expect_quick_identical(
    returned,
    list(1e20, 3, 1e19),
    list(-1e20, 3, 1e19)
  )
})

test_that("size min and max use one numeric domain", {
  min_fn <- function(n, x) {
    declare(
      type(n = integer(1)),
      type(x = double(min(n %/% 2L, 5L, 6L)))
    )
    sum(x)
  }
  max_fn <- function(n, x) {
    declare(
      type(n = integer(1)),
      type(x = double(max(n %/% 2L, 5L, 6L)))
    )
    sum(x)
  }

  min_code <- as.character(r2f(min_fn))
  max_code <- as.character(r2f(max_fn))
  expect_match(min_code, "min(real(", fixed = TRUE)
  expect_match(max_code, "max(real(", fixed = TRUE)
  for (code in list(min_code, max_code)) {
    expect_match(code, "real(5, kind=c_double)", fixed = TRUE)
    expect_match(code, "real(6, kind=c_double)", fixed = TRUE)
    expect_match(code, "kind=c_ptrdiff_t", fixed = TRUE)
  }

  expect_quick_identical(min_fn, list(8L, as.double(1:4)))
  expect_quick_identical(max_fn, list(8L, as.double(1:6)))
})

test_that("one-argument size min and max are identity operations", {
  min_fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(min(n))))
    sum(x)
  }
  max_fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(max(n))))
    sum(x)
  }

  expect_quick_identical(min_fn, list(3L, as.double(1:3)))
  expect_quick_identical(max_fn, list(4L, as.double(1:4)))
})

test_that("zero-argument size min and max fail at translation", {
  min_fn <- function(x) {
    declare(type(x = double(min())))
    sum(x)
  }
  max_fn <- function(x) {
    declare(type(x = double(max())))
    sum(x)
  }

  expect_error(
    quick(min_fn),
    "min() size expressions require at least one argument",
    fixed = TRUE
  )
  expect_error(
    quick(max_fn),
    "max() size expressions require at least one argument",
    fixed = TRUE
  )
})

test_that("size modulo uses the divisor's sign", {
  fn <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    double((x %% y) + 1L)
  }

  code <- r2f(fn)
  expect_match(as.character(code), "modulo(", fixed = TRUE)
  expect_match(code@c_bridge, "quickr_modulo(", fixed = TRUE)

  expect_quick_identical(fn, list(-3L, 2L))
})

test_that("size modulo avoids cancellation in the C bridge", {
  fn <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    out <- double(as.integer((x %% y) * 20) + 10L)
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  code <- r2f(fn)
  expect_match(
    code@c_bridge,
    "double remainder = fmod(x, y);",
    fixed = TRUE
  )
  expect_false(grepl("fmod(fmod(", code@c_bridge, fixed = TRUE))
  expect_match(code@c_bridge, "#include <math.h>", fixed = TRUE)

  expect_quick_identical(fn, list(1, 0.1), list(1, -0.1))

  boundary <- function(x, y, z) {
    declare(
      type(x = double(1)),
      type(y = double(1)),
      type(z = double(as.integer((x %% y) / y) + 1L))
    )
    sum(z)
  }
  expect_quick_identical(boundary, list(1, -2^54, c(1, 2)))
})

test_that("size powers use the double domain before casting", {
  fn <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    double(as.integer((x^-1L) * y))
  }

  code <- r2f(fn)
  expect_match(as.character(code), "real(x, kind=c_double)", fixed = TRUE)
  expect_match(as.character(code), "kind=c_ptrdiff_t", fixed = TRUE)
  expect_match(code@c_bridge, "R_pow", fixed = TRUE)

  expect_quick_identical(fn, list(2L, 4L))

  constant <- function() {
    double(as.integer((2L^-1L) * 4L))
  }
  expect_quick_identical(constant, list())
})

test_that("size powers retain integer exponent type", {
  fn <- function(x, exponent, scale) {
    declare(
      type(x = integer(1)),
      type(exponent = integer(1)),
      type(scale = integer(1))
    )
    out <- double(as.integer((x^exponent) * scale) + 10L)
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  code <- r2f(fn)
  expect_match(as.character(code), "**(exponent)", fixed = TRUE)
  expect_false(
    grepl("real(exponent, kind=c_double)", as.character(code), fixed = TRUE)
  )

  expect_quick_identical(fn, list(-2L, -1L, 4L))

  compound <- function(x, exponent) {
    declare(
      type(x = integer(1)),
      type(exponent = integer(1))
    )
    double(as.integer(x^(exponent %/% 1L)) + 10L)
  }
  expect_match(
    as.character(r2f(compound)),
    "**(int(",
    fixed = TRUE
  )
  expect_quick_identical(compound, list(-2L, 3L))
})

test_that("size powers reject runtime real exponents", {
  fn <- function(x, exponent) {
    declare(
      type(x = integer(1)),
      type(exponent = double(1))
    )
    double(as.integer(x^exponent) + 10L)
  }

  expect_error(
    quick(fn),
    "size expression powers require an integer exponent",
    fixed = TRUE
  )
})

test_that("dim/length/nrow/ncol are supported in allocation sizes", {
  vec <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x) + 1L)
    length(out)
  }
  expect_quick_identical(vec, list(as.double(1:3)), list(as.double(1)))

  mat <- function(x) {
    declare(type(x = double(NA, NA)))
    out <- double(length(x) + 1L)
    length(out)
  }
  expect_quick_identical(mat, list(matrix(as.double(1:12), nrow = 3, ncol = 4)))

  mat_dims <- function(x) {
    declare(type(x = double(NA, NA)))
    out1 <- double(dim(x)[2L] + 1L)
    out2 <- double(nrow(x) + ncol(x))
    c(length(out1), length(out2))
  }
  expect_quick_identical(
    mat_dims,
    list(matrix(as.double(1:12), nrow = 3, ncol = 4)),
    list(matrix(as.double(1:6), nrow = 2, ncol = 3))
  )
})

test_that("dim(x)[axis] errors when axis exceeds rank", {
  bad <- function(x) {
    declare(type(x = double(NA, NA)))
    double(dim(x)[3L])
  }

  expect_error(quick(bad), "insufficient rank", fixed = TRUE)
})
