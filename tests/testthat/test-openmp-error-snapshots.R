test_that("openmp error handling snapshots include cancellation", {
  single_parallel <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    for (i in seq_len(1L)) {
      if (x < 0) {
        stop("x must be nonnegative")
      }
    }
    x + 1
  }

  nested_parallel <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    for (i in seq_len(1L)) {
      declare(parallel())
      for (j in seq_len(1L)) {
        if (x < 0) {
          stop("x must be nonnegative")
        }
      }
    }
    x + 1
  }

  expect_translation_snapshots(
    single_parallel,
    note = "Single OpenMP parallel loop with stop()"
  )
  expect_translation_snapshots(
    nested_parallel,
    note = "Nested OpenMP parallel loops with stop()"
  )
})
