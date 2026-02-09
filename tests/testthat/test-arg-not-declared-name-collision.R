test_that("arg-not-declared error is stable under parent-env name collisions", {
  local({
    # A same-named object exists in the function's enclosure.
    x <- 10:13

    f <- function(x) {
      y <- x[2:1]
      y
    }

    expect_error(quick(f), "arg not declared: x", fixed = TRUE)
  })
})
