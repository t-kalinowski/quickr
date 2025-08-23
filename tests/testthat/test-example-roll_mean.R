# Example: rolling mean computation

test_that("roll_mean", {
  slow_roll_mean <- function(x, weights, normalize = TRUE) {
    declare(
      type(x = double(NA)),
      type(weights = double(NA)),
      type(normalize = logical(1))
    )
    out <- double(length(x) - length(weights) + 1)
    n <- length(weights)
    if (normalize) {
      weights <- weights / sum(weights) * length(weights)
    }

    for (i in seq_along(out)) {
      out[i] <- sum(x[i:(i + n - 1)] * weights) / length(weights)
    }
    out
  }

  x <- dnorm(seq(-3, 3, len = 2000))
  weights <- runif(30)

  expect_translation_snapshots(slow_roll_mean)
  expect_quick_equal(slow_roll_mean, list(x, weights))
})
