test_that("translation snapshots: statement closure with host-associated <<-", {
  fn <- function(nx, ny) {
    declare(type(nx = integer(1)), type(ny = integer(1)))
    temp <- matrix(0, nx, ny)

    bc <- function() {
      temp[1, ] <<- 1
      temp[nx, ] <<- 2
      temp[, 1] <<- 3
      temp[, ny] <<- 4
      NULL
    }

    bc()
    temp
  }

  expect_translation_snapshots(
    fn,
    note = "Statement local closure lowered as internal subroutine; <<- subset targets are host-associated (no capture arg/decl in the closure)."
  )
})

test_that("translation snapshots: sapply + <<- mutates host + returns value", {
  fn <- function(x) {
    declare(type(x = double(NA)), type(out = double(length(x))))
    f <- function(i) {
      x[i] <<- x[i] * 2
      x[i]
    }
    out <- sapply(seq_along(x), f)
    list(x = x, out = out)
  }

  expect_translation_snapshots(
    fn,
    note = "Local closure lowered under contains; x is host-associated (no capture arg), marked modified so the C bridge duplicates x to preserve R semantics."
  )
})

test_that("translation snapshots: 3D slice superassignment with missing indices", {
  fn <- function(nx, ny, nz) {
    declare(type(nx = integer(1)), type(ny = integer(1)), type(nz = integer(1)))
    a <- array(0.0, c(nx, ny, nz))

    f <- function(k) {
      a[,, k] <<- as.double(k)
      a[1, 1, k] <<- a[1, 1, k] + 0.5
      NULL
    }

    f(1L)
    f(nz)
    a
  }

  expect_translation_snapshots(
    fn,
    note = "3D array section designators compile to host-associated Fortran slices (a(:, :, k))."
  )
})

test_that("translation snapshots: sapply simplify='array' + <<- mutates host matrix argument", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    out <- x
    f <- function(j) {
      x[, j] <<- x[, j] * 2.0
      x[, j]
    }
    out <- sapply(seq_len(ncol(x)), f, simplify = "array")
    list(x = x, out = out)
  }

  expect_translation_snapshots(
    fn,
    note = "sapply() lowering with higher-rank output and host mutation; x is duplicated by the C bridge due to modification tracking."
  )
})
