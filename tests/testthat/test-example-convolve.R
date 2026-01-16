# Example: convolution

test_that("convolve", {
  # options("quickr.r2f.debug" = TRUE)
  slow_convolve <- function(a, b) {
    declare(type(a = double(NA)))
    declare(type(b = double(NA)))

    ab <- double(length(a) + length(b) - 1)
    for (i in seq_along(a)) {
      for (j in seq_along(b)) {
        ab[i + j - 1] = ab[i + j - 1] + a[i] * b[j]
      }
    }
    ab
  }

  fsub <- new_fortran_subroutine("slow_convolve", slow_convolve)
  cwrapper <- make_c_bridge(fsub)

  expect_snapshot(
    {
      slow_convolve
      cat(fsub)
      cat(cwrapper)
    },
    transform = scrub_environment
  )

  quick_convolve <- quick(name = "quick_convolve", slow_convolve)
  a <- 1:3
  b <- 10:15
  expect_error(
    quick_convolve(a, b),
    "must be 'double', not 'integer'",
    fixed = TRUE
  )
  a <- as.double(1:3)
  b <- as.double(10:15)
  expect_equal(quick_convolve(a, b), slow_convolve(a, b))
  a <- as.double(0:3)
  b <- 1
  expect_equal(quick_convolve(a, b), slow_convolve(a, b))
})
