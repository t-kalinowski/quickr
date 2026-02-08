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
