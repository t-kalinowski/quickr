# Unit tests for declare(type()) variants

test_that("declare(type()) variants", {
  # exprs in {
  quick_seq <- (function(start, end) {
    declare({
      type(start = integer(1))
      type(end = integer(1))
      type(out = integer(end - start + 1))
    })
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))

  # args, with missing trailing arg
  quick_seq <- (function(start, end) {
    declare(
      type(start = integer(1)),
      type(end = integer(1)),
      type(out = integer(end - start + 1)),
    )
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))

  # args, without missing trailing arg
  quick_seq <- (function(start, end) {
    declare(
      type(start = integer(1)),
      type(end = integer(1)),
      type(out = integer(end - start + 1))
    )
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))

  # seperate declare() calls
  quick_seq <- (function(start, end) {
    declare(type(start = integer(1)))
    declare(type(end = integer(1)))
    declare(type(out = integer(end - start + 1)))
    out <- seq(start, end)
    out
  })
  expect_quick_identical(quick_seq, list(1L, 5L))
})
