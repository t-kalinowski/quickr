test_that("large local arrays are emitted as allocatable + allocate()", {
  f <- function(x) {
    declare(type(x = double(16, 64, 64, 16)))
    a <- x + x
    b <- a + x
    b
  }

  fsub <- r2f(f)
  code <- as.character(fsub)

  expect_match(code, "allocatable[^\\n]*:: a\\(")
  expect_match(code, "allocate\\(a\\(")

  fq <- quick(f)
  set.seed(1)
  x <- array(runif(16 * 64 * 64 * 16), dim = c(16, 64, 64, 16))
  expect_identical(f(x), fq(x))
})

test_that("multiple local arrays do not stack overflow under flang", {
  f <- function(x) {
    declare(type(x = double(16, 48, 48, 16)))
    a <- x + x
    b <- a + x
    c <- b + x
    c
  }

  fq <- quick(f)
  set.seed(2)
  x <- array(runif(16 * 48 * 48 * 16), dim = c(16, 48, 48, 16))
  expect_identical(f(x), fq(x))
})

test_that("local allocatable threshold accounts for storage mode", {
  mk <- function(mode, nrow = 300L, ncol = 300L) {
    force(mode)
    force(nrow)
    force(ncol)
    if (identical(mode, "integer")) {
      function(x) {
        declare(type(x = integer(nrow, ncol)))
        a <- x + x
        out <- a + x
        out
      }
    } else if (identical(mode, "complex")) {
      function(x) {
        declare(type(x = complex(nrow, ncol)))
        a <- x + x
        out <- a + x
        out
      }
    } else if (identical(mode, "logical")) {
      function(x) {
        declare(type(x = logical(nrow, ncol)))
        a <- !x
        out <- a & x
        out
      }
    } else {
      stop("unknown mode: ", mode)
    }
  }

  modes <- c("integer", "complex", "logical")
  for (mode in modes) {
    f <- mk(mode)
    code <- as.character(r2f(f))
    expect_match(code, "allocatable[^\\n]*:: a\\(")
    expect_match(code, "allocate\\(a\\(")
  }

  set.seed(3)
  x_int <- matrix(sample.int(100, 300L * 300L, replace = TRUE), nrow = 300L)
  expect_identical(quick(mk("integer"))(x_int), mk("integer")(x_int))

  set.seed(4)
  x_cplx <- matrix(
    complex(
      real = runif(300L * 300L),
      imaginary = runif(300L * 300L)
    ),
    nrow = 300L
  )
  expect_equal(quick(mk("complex"))(x_cplx), mk("complex")(x_cplx))

  set.seed(5)
  x_lgl <- matrix(
    sample(c(TRUE, FALSE), 300L * 300L, replace = TRUE),
    nrow = 300L
  )
  expect_identical(quick(mk("logical"))(x_lgl), mk("logical")(x_lgl))
})

test_that("unknown local element counts are treated as allocatable", {
  f <- function(x, n) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    a <- x + x
    out <- a + x
    out
  }

  code <- as.character(r2f(f))
  expect_match(code, "allocatable[^\\n]*:: a\\(")
  expect_match(code, "allocate\\(a\\(")

  set.seed(6)
  x <- matrix(runif(40 * 40), nrow = 40)
  expect_identical(quick(f)(x, 40L), f(x, 40L))
})
