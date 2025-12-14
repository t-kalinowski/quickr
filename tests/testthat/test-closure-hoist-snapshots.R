test_that("translation snapshots: hoist+block inside contains (sapply closure)", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    out <- sapply(seq_along(out), function(i) (x + 1.0)[i])
    out
  }

  expect_translation_snapshots(
    fn,
    note = "The closure body indexes an array expression, which forces a hoisted temporary and a Fortran block inside the internal procedure."
  )
})

test_that("translation snapshots: hoist+block inside contains (statement closure + <<-)", {
  fn <- function(nx, ny) {
    declare(type(nx = integer(1)), type(ny = integer(1)))
    temp <- matrix(0, nx, ny)

    bc <- function() {
      temp[1, 1] <<- (temp + 1.0)[1, 1]
      temp[nx, ny] <<- (temp + 2.0)[nx, ny]
      NULL
    }

    bc()
    temp
  }

  expect_translation_snapshots(
    fn,
    note = "Statement local closure lowered under contains; RHS indexing forces hoisting + a block inside the internal procedure; LHS uses [<<- to mutate the host variable."
  )
})
