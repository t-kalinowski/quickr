skip_on_cran()

test_that("codegen: deferred vector fills allocate and resize", {
  fn <- function(n) {
    declare(type(n = integer(1)), type(out = double(NA)))
    out <- numeric(3)
    out <- numeric(n)
    out <- numeric(1)
    out[1L] <- 7
    sum(out)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, 0L, 1L, 3L, 7L)
})

test_that("codegen: deferred matrix fills resize axes and reuse storage", {
  fn <- function(n) {
    declare(type(n = integer(1)), type(out = double(NA, NA)))
    out <- matrix(2, 2, 3)
    first <- sum(out)
    out <- matrix(3, n, 2)
    second <- sum(out)
    out <- matrix(4, n, 2)
    first + second + sum(out)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, 0L, 1L, 2L, 5L)
})

test_that("codegen: deferred logical fills convert the interface value", {
  fn <- function(flag) {
    declare(type(flag = logical(1)), type(out = logical(NA, NA)))
    out <- matrix(flag, 2, 3)
    sum(out)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, TRUE, FALSE)
})

test_that("codegen: a deferred fill reads old storage before resizing", {
  fn <- function() {
    declare(type(out = double(NA, NA)))
    out <- matrix(1, 2, 2)
    out <- matrix(sum(out), 3, 3)
    sum(out)
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list())
})

test_that("codegen: reassignment checks independent stored extents", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(m)))
    x <- a
    x <- b
    x
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(c(1, 2), c(3, 4)))
  expect_error(
    quick(fn)(c(1, 2), c(3, 4, 5)),
    "reassignment must preserve the shape"
  )
})

test_that("codegen: shape-preserving reassignment reuses a local", {
  fn <- function(a) {
    declare(type(a = double(n)))
    x <- a
    x <- a * 2
    x
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(1, 2, 3), double())
})

test_that("codegen: rebound dimension names do not replace stored extents", {
  fn <- function(n, m, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m))
    )
    n <- 1L
    x <- y
    x
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4)))
})

test_that("codegen: captured assignments use extents outside a shadowing formal", {
  fn <- function(n, m, x, y) {
    declare(
      type(n = integer(1)),
      type(m = integer(1)),
      type(x = double(n)),
      type(y = double(m))
    )
    replace <- function(n) {
      x <<- y
      NULL
    }
    replace(1L)
    x
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(2L, 2L, c(1, 2), c(3, 4)))
})

test_that("codegen: captured copies outlive replacement of their source", {
  fn <- function(y) {
    declare(type(y = double(NA)), type(x = double(NA)))
    x <- y
    read <- function() {
      saved <- x
      x <<- c(10, 20)
      sum(saved)
    }
    read()
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(2, 3, 4))
})

test_that("codegen: flattening a deferred local queries its allocated size", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)), type(m = double(NA, NA)))
    m <- x
    sum(as.vector(m))
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(matrix(as.double(1:6), 2)),
    list(matrix(double(), 0, 3))
  )
})

test_that("codegen: replacement ranges retain length-one axes", {
  fn <- function() {
    declare(type(out = double(2, 3, 2)), type(block = double(1, 3, 2)))
    out <- array(0, c(2, 3, 2))
    block <- array(1, c(1, 3, 2))
    for (i in seq_len(2L)) {
      out[i:i, , ] <- block
    }
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list())
})

test_that("codegen: vector replacement preserves an aliased right-hand side", {
  fn <- function(x, i) {
    declare(type(x = double(n)), type(i = integer(k)))
    out <- x
    out[i] <- out[rev(i)]
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(c(10, 20, 30, 40), c(1L, 3L)))
})

test_that("codegen: replacement values are captured before computed indices", {
  fn <- function(x) {
    declare(type(x = double(3)))
    position <- 0L
    index <- function() {
      position <<- position + 1L
      position
    }
    value <- function() {
      position <<- 1L
      7
    }
    x[index()] <- value()
    x
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, c(10, 20, 30))
})

test_that("codegen: scalar logical replacement writes only selected values", {
  fn <- function(x, mask) {
    declare(type(x = double(n)), type(mask = logical(n)))
    out <- x
    out[mask] <- 0
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(
    fn,
    list(c(1, 2, 3), c(TRUE, FALSE, TRUE)),
    list(double(), logical())
  )
})

test_that("codegen: logical replacement vectors consume selected values in order", {
  fn <- function(x, mask, values) {
    declare(
      type(x = double(n)),
      type(mask = logical(n)),
      type(values = double(k))
    )
    out <- x
    out[mask] <- values
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(c(1, 2, 3), c(TRUE, FALSE, TRUE), c(8, 9)))
})

test_that("codegen: matrix replacement hoists vector indices", {
  fn <- function(out, block) {
    declare(type(out = integer(2, 3)), type(block = integer(2, 3)))
    out[seq_len(2L), rev(seq_len(3L))] <- block
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(matrix(0L, 2, 3), matrix(1:6, 2, 3)))
})
