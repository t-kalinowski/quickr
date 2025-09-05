# Unit tests for unary intrinsic functions

test_that("double unary intrinsics", {
  double_intrinsics <- c(
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sqrt",
    "exp",
    "log",
    "log10",
    "floor",
    "ceiling",
    "abs"
  )

  for (intr in double_intrinsics) {
    fn <- eval(str2lang(sprintf(
      "function(x) {
         declare(type(x = double(NA)))
         out <- %s(x)
         out
       }",
      intr
    )))

    expect_translation_snapshots(fn)

    x <- switch(
      intr,
      asin = seq(-1, 1, length.out = 20),
      acos = seq(-1, 1, length.out = 20),
      sqrt = seq(0, 10, length.out = 20),
      log = seq(.1, 10, length.out = 20),
      log10 = seq(.1, 10, length.out = 20),
      exp = seq(-2, 2, length.out = 20),
      seq(-5, 5, length.out = 20)
    )
    expect_quick_equal(fn, x)
  }
})

test_that("integer unary intrinsics", {
  integer_intrinsics <- c("abs")

  for (intr in integer_intrinsics) {
    fn <- eval(str2lang(sprintf(
      "function(x) {
         declare(type(x = integer(NA)))
         out <- %s(x)
         out
       }",
      intr
    )))

    expect_translation_snapshots(fn)

    x <- as.integer(seq(-5, 5, length.out = 20))
    expect_quick_identical(fn, x)
  }
})

test_that("complex unary intrinsics", {
  set.seed(123)
  x <- seq(-5, 5, length.out = 30)
  z <- complex(real = x, imaginary = sample(x))

  complex_intrinsics <- c(
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sqrt",
    "exp",
    "log",
    "log10",
    "Re",
    "Im",
    "Mod",
    "Arg",
    "Conj"
  )

  for (intr in complex_intrinsics) {
    fn <- eval(str2lang(sprintf(
      "function(z) {
         declare(type(z = complex(NA)))
         out <- %s(z)
         out
       }",
      intr
    )))

    expect_translation_snapshots(fn)
    expect_quick_equal(fn, z)
  }
})


test_that("unary logical 'not'-operator on vector", {
  fn <- function(x) {
    declare(type(x = integer(n)))
    lgl <- x > 1L
    not_lgl <- !lgl
    not_lgl
  }

  x1 <- -3:3
  x2 <- 0:5
  expect_quick_identical(fn, list(x1), list(x2))
})

test_that("unary minus and plus for integer and double", {
  fn_neg_i <- function(x) {
    declare(type(x = integer(n)))
    y <- -x
    y
  }
  fn_pos_i <- function(x) {
    declare(type(x = integer(n)))
    y <- +x
    y
  }
  xi <- -5:5
  expect_quick_identical(fn_neg_i, list(xi))
  expect_quick_identical(fn_pos_i, list(xi))

  fn_neg_d <- function(x) {
    declare(type(x = double(n)))
    y <- -x
    y
  }
  fn_pos_d <- function(x) {
    declare(type(x = double(n)))
    y <- +x
    y
  }
  xd <- -5:5 + 0.5
  expect_quick_equal(fn_neg_d, list(xd))
  expect_quick_equal(fn_pos_d, list(xd))
})

test_that("logical not local used as ifelse mask compiles and runs", {
  fn <- function(x) {
    declare(type(x = logical(NA)))
    y <- !x
    out <- ifelse(y, 1L, 0L)
    out
  }

  # Ensure translation and execution are correct
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(c(TRUE, FALSE, TRUE)))
})
