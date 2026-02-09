test_that("subscript drop=TRUE drops length-1 ranges like 1:1", {
  fn <- function(x) {
    declare(type(x = double(1, 1, 2, 3, 4)))
    x[1:1, 1:1, 1:2, 1:3, 1:4]
  }

  x <- array(as.double(1:24), dim = c(1, 1, 2, 3, 4))
  expect_quick_identical(fn, list(x))
})

test_that("subset assignment preserves length-1 range rank on the LHS", {
  fn <- function() {
    declare({
      type(out = double(2L, 4L, 3L))
      type(tmp = double(1L, 4L, 3L))
    })

    out <- array(0.0, dim = c(2L, 4L, 3L))
    tmp <- array(1.0, dim = c(1L, 4L, 3L))

    for (i in seq_len(2L)) {
      out[i:i, , ] <- tmp
    }

    out
  }

  fq <- quick(fn)
  expect_identical(fn(), fq())
})
