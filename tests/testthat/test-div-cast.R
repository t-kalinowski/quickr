test_that("division casts integers", {
  div_int <- function(a, b) {
    declare(type(a = integer(n)), type(b = integer(n)))
    a / b
  }

  expect_translation_snapshots(div_int)

  a <- as.integer(-5:5)
  b <- as.integer(sample(a))
  expect_quick_equal(div_int, list(a, b))
})


test_that("division casts double and integer", {
  div_dbl_int <- function(a, b) {
    declare(type(a = double(n)), type(b = integer(n)))
    a / b
  }

  expect_translation_snapshots(div_dbl_int)

  a <- as.double(-5:5)
  b <- as.integer(sample(-5:5))
  expect_quick_equal(div_dbl_int, list(a, b))
})


test_that("division casts logical", {
  div_dbl_lgl <- function(a, b) {
    declare(type(a = double(n)), type(b = logical(n)))
    a / b
  }

  expect_translation_snapshots(div_dbl_lgl)

  a <- c(-3, 0, 3)
  b <- c(TRUE, FALSE, TRUE)
  expect_quick_equal(div_dbl_lgl, list(a, b))
})


test_that("division casts complex", {
  div_cplx <- function(a, b) {
    declare(type(a = complex(n)), type(b = complex(n)))
    a / b
  }

  expect_translation_snapshots(div_cplx)

  real <- -2:2
  imag <- rev(real)
  a <- complex(real = real, imaginary = imag)
  b <- sample(a[a != 0+0i], length(a), replace = TRUE)
  expect_quick_equal(div_cplx, list(a, b))
})


test_that("division in my_mean maintains double precision", {
  my_mean <- function(x) {
    declare(type(x = double(NA)))
    mu <- sum(x) / length(x)
    mu
  }

  expect_translation_snapshots(my_mean)

  x <- c(3, 5, 7, 8)
  expect_quick_equal(my_mean, list(x))
})

