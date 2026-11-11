test_that("input dimension contracts do not add checks to loops", {
  fn <- function(x, y) {
    declare(type(x = double(n)), type(y = double(n)))
    out <- numeric(3)
    for (i in 1:3) {
      out[i] <- sum(x %*% y)
    }
    out
  }

  expect_false(grepl(
    "quickr_set_error_msg",
    as.character(r2f(fn)),
    fixed = TRUE
  ))
  expect_quick_identical(fn, list(c(1, 2), c(3, 4)), list(double(), double()))
  qfn <- quick(fn)
  expect_error(
    qfn(c(1, 2), c(3, 4, 5)),
    "length(y) must equal length(x)",
    fixed = TRUE
  )
})

test_that("dimension contracts distinguish promoted singleton axes", {
  fn <- function(x, y) {
    declare(type(x = double(2, n)), type(y = double(n)))
    tcrossprod(x, y)
  }

  expect_quick_identical(fn, list(matrix(1, 2, 1), 2))
  qfn <- quick(fn)
  expect_error(
    qfn(matrix(1, 2, 3), c(1, 2, 3)),
    "non-conformable arguments in tcrossprod",
    fixed = TRUE
  )
})

test_that("pure closures do not inherit unrelated error checks", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    y <- x + 1
    sapply(y, function(z) z * 2)
  }

  expect_false(grepl(
    "if (quickr_err_msg(1) /=",
    as.character(r2f(fn)),
    fixed = TRUE
  ))
  expect_quick_identical(fn, c(1, 2, 3))
  qfn <- quick(fn)
  expect_error(qfn(double()), "equal lengths", fixed = TRUE)
})

test_that("nested closures still propagate errors from their callees", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function(z) {
      g <- function(v) {
        if (v < 0) {
          stop("negative input")
        }
        v * 2
      }
      g(z) + 1
    }
    sapply(x, f)
  }

  expect_quick_identical(fn, c(1, 2, 3))
  qfn <- quick(fn)
  expect_error(qfn(c(1, -2, 3)), "negative input", fixed = TRUE)
})
