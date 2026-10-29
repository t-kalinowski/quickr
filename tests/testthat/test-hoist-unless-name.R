# floor()/ceiling() splice their argument into the emitted expression three
# times; non-trivial arguments must be hoisted to a temporary so side effects
# (e.g. RNG state) happen exactly once.

test_that("floor() does not evaluate an impure argument multiple times", {
  fn <- function() {
    out <- floor(runif(1L) * 10)
    out
  }
  qfn <- quick(fn)

  set.seed(1234)
  q_res <- qfn()
  q_next <- runif(1L)

  set.seed(1234)
  r_res <- fn()
  r_next <- runif(1L)

  expect_identical(q_res, r_res)
  expect_equal(q_next, r_next)
})

test_that("ceiling() does not evaluate an impure argument multiple times", {
  fn <- function() {
    out <- ceiling(runif(1L) * 10)
    out
  }
  qfn <- quick(fn)

  set.seed(1234)
  q_res <- qfn()
  q_next <- runif(1L)

  set.seed(1234)
  r_res <- fn()
  r_next <- runif(1L)

  expect_identical(q_res, r_res)
  expect_equal(q_next, r_next)
})

test_that("floor() hoists non-name arguments to a temporary", {
  fn <- function() {
    out <- floor(runif(1L) * 10)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("floor()/ceiling() on names and literals emit no temporary", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- floor(x) + ceiling(x)
    out
  }
  fsub <- r2f(fn)
  expect_no_match(fsub, "block", fixed = TRUE)
  expect_quick_identical(fn, list(c(-2.5, -1, 0.5, 3)))

  literals <- function() {
    floor(2.5) + ceiling(1.5)
  }
  expect_no_match(as.character(r2f(literals)), "block", fixed = TRUE)
  expect_quick_identical(literals, list())
})

test_that("literal zero division remains a runtime expression", {
  fn <- function() {
    1 / 0
  }
  expect_quick_identical(fn, list())
})

test_that("log10 constants remain inside short-circuit branches", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    flag && (log10(0) < 0)
  }
  expect_quick_identical(fn, list(FALSE), list(TRUE))
})
