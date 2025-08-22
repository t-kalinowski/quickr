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
