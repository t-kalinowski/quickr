# Unit tests for subscript validation: negative/zero rejection and
# runtime guards on index ranges

skip_on_cran()

test_that("negative and zero subscripts are rejected at compile time", {
  fn <- function(x) {
    declare(type(x = double(4)))
    x[-1L]
  }
  expect_error(quick(fn), "negative subscripts|subscripts must be positive")

  fn0 <- function(x) {
    declare(type(x = double(4)))
    x[0L]
  }
  expect_error(quick(fn0), "subscripts must be positive")

  fni <- function(x, i) {
    declare(type(x = double(4)), type(i = integer(1)))
    x[-i]
  }
  expect_error(quick(fni), "negative subscripts")

  fnr <- function(x) {
    declare(type(x = double(4)))
    x[-(1:2)]
  }
  expect_error(quick(fnr), "negative subscripts")

  fnc <- function(x) {
    declare(type(x = double(4)))
    x[c(-1L, -2L)]
  }
  expect_error(quick(fnc), "negative subscripts|subscripts must be positive")

  fnm <- function(x) {
    declare(type(x = double(3, 3)))
    x[1L, -2L]
  }
  expect_error(quick(fnm), "negative subscripts|subscripts must be positive")
})

test_that("literal subscripts beyond a known extent are compile errors", {
  # R pads out-of-range reads with NA; quickr refuses at compile time
  fn <- function(x) {
    declare(type(x = double(3)))
    x[4L]
  }
  expect_error(quick(fn), "exceeds its dimension's extent \\(3\\)")

  frange <- function(x) {
    declare(type(x = double(3)))
    x[2:4]
  }
  expect_error(quick(frange), "exceeds its dimension's extent \\(3\\)")

  fc <- function(x) {
    declare(type(x = double(3)))
    x[c(1L, 4L)]
  }
  expect_error(quick(fc), "exceeds its dimension's extent \\(3\\)")

  fmat <- function(m) {
    declare(type(m = double(2, 3)))
    m[1L, 5L]
  }
  expect_error(quick(fmat), "exceeds its dimension's extent \\(3\\)")

  # linear indexing on a matrix checks against the total size
  flin <- function(m) {
    declare(type(m = double(2, 3)))
    m[7L]
  }
  expect_error(quick(flin), "exceeds its dimension's extent \\(6\\)")

  # in-range literals and symbolic subscripts/extents are untouched
  fok <- function(x) {
    declare(type(x = double(3)))
    x[3L] + x[1:3][1L]
  }
  expect_quick_identical(fok, list(as.double(1:3)))

  fsym <- function(x, i) {
    declare(type(x = double(3)), type(i = integer(1)))
    x[i]
  }
  expect_no_error(quick(fsym))

  fna <- function(x) {
    declare(type(x = double(NA)))
    x[5L]
  }
  expect_no_error(quick(fna))
})

test_that("numeric literal subscripts are validated after integer coercion", {
  ftruncate <- function(x) {
    declare(type(x = double(3)))
    x[3.9]
  }
  expect_quick_identical(ftruncate, list(as.double(1:3)))

  fzero <- function(x) {
    declare(type(x = double(3)))
    x[0.5]
  }
  expect_error(quick(fzero), "subscripts must be positive")
})

test_that("assignment subscripts get the same validation as reads", {
  # x[-1L] <- 9 compiled into a silent out-of-bounds Fortran write
  fneg <- function(x) {
    declare(type(x = double(3)))
    x[-1L] <- 9
    x
  }
  expect_error(quick(fneg), "subscripts must be positive")

  fzero <- function(x) {
    declare(type(x = double(3)))
    x[0L] <- 1
    x
  }
  expect_error(quick(fzero), "subscripts must be positive")

  foob <- function(x) {
    declare(type(x = double(3)))
    x[4L] <- 1
    x
  }
  expect_error(quick(foob), "exceeds its dimension's extent \\(3\\)")

  fmat <- function(m) {
    declare(type(m = double(2, 3)))
    m[3L, 1L] <- 1
    m
  }
  expect_error(quick(fmat), "exceeds its dimension's extent \\(2\\)")

  # superassignment from a local closure shares the path
  fsuper <- function(x) {
    declare(type(x = double(3)))
    bump <- function() {
      x[4L] <<- 1
    }
    bump()
    x
  }
  expect_error(quick(fsuper), "exceeds its dimension's extent \\(3\\)")

  # valid writes still compile and match R
  fok <- function(m) {
    declare(type(m = double(2, 3)))
    m[2L, 3L] <- 1
    m[, 2L] <- 0
    m
  }
  expect_quick_identical(fok, list(matrix(as.double(1:6), 2, 3)))
})

test_that("x[a:b] guards against non-positive bounds at runtime", {
  fn <- function(x, n) {
    declare(type(x = double(NA)), type(n = integer(1)))
    x[1:n]
  }
  expect_translation_snapshots(fn) # pins the guard text
  qfn := quick(fn)
  expect_equal(qfn(as.double(1:5), 3L), c(1, 2, 3))
  expect_error(qfn(as.double(1:5), 0L), "bounds >= 1")
  expect_error(qfn(as.double(1:5), -2L), "bounds >= 1")

  # descending ranges still work
  fdesc <- function(x, n) {
    declare(type(x = double(NA)), type(n = integer(1)))
    x[n:1]
  }
  qdesc := quick(fdesc)
  expect_equal(qdesc(as.double(1:4), 4L), c(4, 3, 2, 1))
  expect_error(qdesc(as.double(1:4), 0L), "bounds >= 1")
})

test_that("literal in-range bounds emit no guard; bad literals error at compile time", {
  fn <- function(x) {
    declare(type(x = double(5)))
    x[2:4]
  }
  expect_translation_snapshots(fn) # pins the absence of a guard
  expect_quick_identical(fn, list(as.double(1:5)))

  fbad <- function(x) {
    declare(type(x = double(5)))
    x[0:2]
  }
  expect_error(quick(fbad), "bounds >= 1")
})

test_that("x[seq(a, b, by)] requires a literal step and guards wrong signs", {
  # the result length divides by the step, and the C bridge evaluates it
  # before any runtime guard could run -> compile error
  fn <- function(x, k) {
    declare(type(x = double(NA)), type(k = integer(1)))
    x[seq(1L, 5L, by = k)]
  }
  expect_error(quick(fn), "literal `by`")

  # literal step, symbolic bound: R errors on the wrong-sign case
  fs <- function(x, n) {
    declare(type(x = double(NA)), type(n = integer(1)))
    x[seq(5L, n, by = 1L)]
  }
  qfs := quick(fs)
  expect_equal(qfs(as.double(1:9), 8L), c(5, 6, 7, 8))
  expect_error(qfs(as.double(1:9), 2L), "wrong sign")
})

test_that("seq() value with a symbolic by is sized by the step", {
  fn <- function(k) {
    declare(type(k = integer(1)))
    seq(1L, 9L, by = k)
  }
  sum_fn <- function(k) {
    declare(type(k = integer(1)))
    sum(seq(1L, 9L, by = k))
  }

  qfn := quick(fn)
  qsum := quick(sum_fn)
  expect_identical(qfn(2L), seq(1L, 9L, by = 2L))
  expect_identical(qfn(4L), seq(1L, 9L, by = 4L))
  expect_identical(qsum(2L), sum(seq(1L, 9L, by = 2L)))
  expect_error(
    qfn(0L),
    "invalid '(to - from)/by'",
    fixed = TRUE
  )
  expect_error(
    qfn(-2L),
    "wrong sign in 'by' argument",
    fixed = TRUE
  )
  expect_error(
    qsum(0L),
    "invalid '(to - from)/by'",
    fixed = TRUE
  )
  expect_error(
    qsum(-2L),
    "wrong sign in 'by' argument",
    fixed = TRUE
  )
})

test_that("x[seq_len(n)] with n = 0 returns a zero-length result like R", {
  fn <- function(x, n) {
    declare(type(x = double(NA)), type(n = integer(1)))
    x[seq_len(n)]
  }
  qfn := quick(fn)
  expect_equal(qfn(as.double(1:5), 3L), c(1, 2, 3))
  expect_equal(qfn(as.double(1:5), 0L), double(0))
})
