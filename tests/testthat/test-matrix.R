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

test_that("matrix() reshapes non-scalar data expressions", {
  fn <- function(x) {
    declare(type(x = double(2L, 3L)))
    matrix(as.integer(x), nrow = 2L, ncol = 3L)
  }

  x <- matrix(as.double(1:6), nrow = 2L, ncol = 3L)
  expect_quick_identical(fn, list(x))
})

test_that("matrix() does not evaluate data expressions twice", {
  fn <- function() {
    matrix(runif(4L), nrow = 2L, ncol = 2L)
  }
  qfn <- quick(fn)

  set.seed(1234)
  qfn()
  q_next <- runif(1L)

  set.seed(1234)
  fn()
  r_next <- runif(1L)

  expect_equal(q_next, r_next)
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

test_that("vector-matrix ops only recycle along the first dimension", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(3)),
      type(mat = double(1L, 3))
    )
    left_side_vec <- vec + mat
    right_side_vec <- mat + vec
    out <- left_side_vec + right_side_vec
    out
  }

  expect_error(
    quick(fn),
    "elementwise vector-matrix operations require a scalar or a vector length equal to the matrix first dimension (nrow)",
    fixed = TRUE
  )
})

test_that("elementwise vector and singleton column matrix keep matrix shape", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(n)),
      type(mat = double(n, 1L))
    )
    left_side_vec <- vec + mat
    right_side_vec <- mat + vec
    out <- left_side_vec + right_side_vec
    out
  }

  expect_quick_identical(
    fn,
    list(runif(4), matrix(runif(4), ncol = 1L))
  )
})

test_that("elementwise vector-matrix reuse works across columns", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(3)),
      type(mat = double(3, 2))
    )
    left <- vec * mat
    right <- mat * vec
    left + right
  }

  vec <- as.double(1:3)
  mat <- matrix(as.double(1:6), nrow = 3)

  expect_quick_identical(fn, list(vec, mat))
})

test_that("elementwise vector-matrix ops allow scalar vectors", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(1)),
      type(mat = double(3, 2))
    )
    vec + mat
  }

  vec <- 2.5
  mat <- matrix(as.double(1:6), nrow = 3)

  expect_quick_identical(fn, list(vec, mat))
})

test_that("elementwise vector-matrix ops reject longer vectors", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(2)),
      type(mat = double(3, 2))
    )
    vec + mat
  }

  expect_error(
    quick(fn),
    "elementwise vector-matrix operations require a scalar or a vector length equal to the matrix first dimension (nrow)",
    fixed = TRUE
  )
})

test_that("elementwise matrix operations require matching dimensions", {
  fn <- function(a, b) {
    declare(
      type(a = double(2, 3)),
      type(b = double(2, 2))
    )
    a + b
  }

  expect_error(
    quick(fn),
    "elementwise matrix operations require matching dimensions",
    fixed = TRUE
  )
})

test_that("elementwise vector operations require matching lengths", {
  fn <- function(a, b) {
    declare(
      type(a = double(2)),
      type(b = double(3))
    )
    a + b
  }

  expect_error(
    quick(fn),
    "elementwise vector operations require lengths that recycle cleanly unless one operand is scalar",
    fixed = TRUE
  )
})

test_that("elementwise vector ops allow scalar vectors", {
  fn <- function(a, b) {
    declare(
      type(a = double(1)),
      type(b = double(3))
    )
    a + b
  }

  expect_quick_identical(fn, list(2.5, as.double(1:3)))
})

test_that("elementwise ops reshape vectors for singleton matrices", {
  fn <- function(vec, mat) {
    declare(
      type(vec = double(n)),
      type(mat = double(n, 1L))
    )
    a <- vec - mat
    b <- mat * vec
    c <- vec / mat
    a + b + c
  }

  expect_quick_identical(
    fn,
    list(runif(3) + 1, matrix(runif(3) + 1, ncol = 1L))
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

test_that("1x1 matrix with length-3 vector yields a vector", {
  fn <- function(vec, mat_1_1) {
    declare(
      type(vec = double(3)),
      type(mat_1_1 = double(1, 1))
    )
    a <- vec + mat_1_1
    b <- mat_1_1 + vec
    out <- list(a = a, b = b)
    out
  }

  suppressWarnings(expect_quick_identical(
    fn,
    list(c(1, 2, 3), matrix(1, nrow = 1L, ncol = 1L))
  ))
})

test_that("indexing function like transposed expressions hoists temporaries that can be accessed", {
  fn <- function(x) {
    declare(type(x = double(5, 5)))
    first_element <- t(x)[1]
    second_row <- t(x)[2, ]
    third_col <- t(x)[, 3]
    sub_matrix <- t(x)[c(1, 2), c(3, 4)]
    list(
      first_element = first_element,
      second_row = second_row,
      third_col = third_col,
      sub_matrix = sub_matrix
    )
  }

  x <- matrix(runif(25), 5, 5)
  expect_quick_identical(fn, list(x = x))
})
