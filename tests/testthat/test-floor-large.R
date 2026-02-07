# Large-magnitude floor()/ceiling() behavior.
#
# R's floor()/ceiling() return doubles. Our Fortran lowering should match R,
# including for values that exceed the range of Fortran default integers on some
# compilers (a potential overflow hazard if we lower via FLOOR()/CEILING()).

test_that("floor() and ceiling() match R for large magnitude doubles", {
  fn_floor <- function(x) {
    declare(type(x = double(NA)))
    out <- floor(x)
    out
  }

  fn_ceiling <- function(x) {
    declare(type(x = double(NA)))
    out <- ceiling(x)
    out
  }

  imax <- as.double(.Machine$integer.max)
  x <- c(
    -2.9,
    -1.1,
    -0.1,
    0,
    0.1,
    1.1,
    2.9,
    imax - 0.2,
    -(imax - 0.2),
    (imax + 1000) + 0.2,
    -((imax + 1000) + 0.2),
    1e12 + 0.2,
    -(1e12 + 0.2)
  )

  expect_quick_equal(fn_floor, x)
  expect_quick_equal(fn_ceiling, x)
})
