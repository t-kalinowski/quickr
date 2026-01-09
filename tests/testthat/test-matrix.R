# Unit tests for matrix creation and implicit size reuse

test_that("matrix", {
  fn <- function(a, b) {
    declare(type(a = integer(1)))
    declare(type(b = integer(1)))

    out <- matrix(0, a, b)
    out
  }

  fsub <- r2f(fn)
  make_c_bridge(fsub)

  (r2f(fn))
  expect_snapshot(r2f(fn))

  qfn <- quick(fn)

  # expect_identical(qfn(3, 4), fn(3, 4))  ## strict = TRUE by default
  # expect_identical(qfn(3L, 4), fn(3L, 4))
  expect_identical(qfn(3L, 4L), fn(3L, 4L))

  fn <- function(val, nc, nr) {
    # declare({
    #   type(val = double(1))
    #   type(a = integer(1))
    #   type(b = integer(1))
    # })
    declare(
      type(val = double(1)),
      type(nc = integer(1)),
      type(nr = integer(1))
    )

    out <- matrix(val, nc, nr)
    out
  }
  qfn <- quick(fn)

  qfn(1.1, 3L, 3L)

  expect_identical(qfn(2.3, 3L, 4L), fn(2.3, 3L, 4L))
  expect_identical(qfn(2.3, 3L, 4L), matrix(2.3, 3L, 4L))
  # bench::mark(fn(2.3, 3, 4), matrix(2.3, 3, 4), qfn(2.3, 3, 4)) -> r; print(r); plot(r)
})

test_that("reuse implicit size", {
  fn <- function(a1, a2) {
    declare(type(a1 = double(n)))
    declare(type(a2 = double(n, n)))
    out <- a1 + a2[1, ]
    out
  }

  fsub <- r2f(fn)
  c_wrapper <- make_c_bridge(fsub)
  qfn <- quick(fn)

  expect_snapshot({
    print(fsub)
    cat(c_wrapper)
  })

  n <- 400
  a1 <- as.double(1:n)
  a2 <- matrix(runif(n), n, n)

  expect_identical(fn(a1, a2), qfn(a1, a2))

  # bench::mark(fn(a1, a2), qfn(a1, a2)) |> print() |> plot()
})

test_that("elementwise vector and singleton matrix keep matrix shape", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(n)),
      type(mat = double(1L, n))
    )
    left_side_vec <- vec + mat
    right_side_vec <- mat + vec
    out <- left_side_vec + right_side_vec
    out
  }

  expect_quick_identical(
    fn,
    list(runif(3), matrix(runif(3), nrow = 1L))
  )
})

test_that("elementwise ops reshape vectors for singleton matrices", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(n)),
      type(mat = double(1L, n))
    )
    a <- vec - mat
    b <- mat * vec
    c <- vec / mat
    a + b + c
  }

  expect_quick_identical(
    fn,
    list(runif(3) + 1, matrix(runif(3) + 1, nrow = 1L))
  )
})

test_that("1x1 matrix preserves matrix result with length-1 vector", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(1L)),
      type(mat = double(1L, 1L))
    )
    vec + mat
  }

  expect_quick_identical(
    fn,
    list(runif(1), matrix(runif(1), nrow = 1L))
  )
})
