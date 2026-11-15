skip_on_cran()

test_that("zero-argument local closures can return logical constants", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    yes <- function() TRUE
    no <- function() FALSE
    positive <- yes()
    negative <- no()
    c(positive, negative, x > 0L)
  }

  expect_quick_identical(fn, -1L, 0L, 1L)
  expect_translation_snapshots(fn)
})

test_that("logical-only closures work in selected and skipped lazy branches", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    yes <- function() TRUE
    no <- function() FALSE
    c(flag && yes(), flag && no(), flag || yes(), flag || no())
  }

  expect_quick_identical(fn, FALSE, TRUE)
  expect_translation_snapshots(fn)
})
