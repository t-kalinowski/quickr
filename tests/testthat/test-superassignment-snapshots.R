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
