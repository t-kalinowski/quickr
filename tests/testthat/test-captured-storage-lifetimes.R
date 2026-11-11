test_that("stored aliases keep their shape after the captured source changes", {
  fn <- function(y) {
    declare(type(y = double(NA)), type(x = double(NA)))
    x <- y
    f <- function() {
      a <- x
      x <<- c(10, 20)
      sum(a %*% a)
    }
    f()
  }

  expect_quick_identical(fn, c(2, 3, 4))
})

test_that("captured SVD components use stored dimensions", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    s <- svd(x)
    f <- function(n) sum(s$u %*% s$u)
    f(1L)
  }

  expect_quick_equal(fn, list(2L, diag(c(2, 1))))
})

test_that("subset assignment shadows retain captured array dimensions", {
  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(n, n)))
    f <- function(n) {
      x[1, 1] <- 2
      sum(x)
    }
    f(1L)
  }

  expect_quick_identical(fn, list(2L, matrix(1, 2, 2)))
})
