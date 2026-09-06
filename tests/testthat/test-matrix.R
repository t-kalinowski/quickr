# Unit tests for matrix creation and implicit size reuse

skip_on_cran()

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
    "elementwise vector operations require equal lengths or a scalar operand; R-style recycling is not supported",
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

test_that("t() and diag() preserve integer mode", {
  tfn <- function(m) {
    declare(type(m = integer(2, 3)))
    t(m)
  }
  expect_quick_equal(tfn, list(matrix(1:6, 2, 3))) # typeof integer

  tvec <- function(x) {
    declare(type(x = integer(3)))
    t(x)
  }
  expect_quick_equal(tvec, list(1:3)) # R: 1 x 3 integer matrix

  dfn <- function(m) {
    declare(type(m = integer(3, 3)))
    diag(m)
  }
  expect_quick_equal(dfn, list(matrix(1:9, 3, 3))) # R: integer vector

  # same, through the inferred-destination path (out <- diag(m))
  dfn2 <- function(m) {
    declare(type(m = integer(3, 3)))
    out <- diag(m)
    out
  }
  expect_translation_snapshots(dfn2)
  expect_quick_equal(dfn2, list(matrix(1:9, 3, 3)))

  dvec <- function(x) {
    declare(type(x = integer(3)))
    diag(x)
  }
  expect_quick_equal(dvec, list(1:3)) # R: integer matrix

  # x recycled along the diagonal of a non-square result
  drect <- function(x) {
    declare(type(x = integer(2)))
    diag(x, 3L, 4L)
  }
  expect_quick_equal(drect, list(1:2))

  # identity forms stay double, as in R
  dident <- function() {
    out <- diag(3L)
    out
  }
  expect_quick_equal(dident, list())
})

test_that("t() and diag() preserve logical mode", {
  m <- matrix(c(TRUE, FALSE, TRUE, TRUE), 2, 2)

  tfn <- function(m) {
    declare(type(m = logical(2, 2)))
    t(m)
  }
  expect_quick_equal(tfn, list(m))

  dfn <- function(m) {
    declare(type(m = logical(2, 2)))
    diag(m)
  }
  expect_quick_equal(dfn, list(m))
})

test_that("diag() preserves integer-backed logical storage in an intermediate", {
  fn <- function(m) {
    declare(type(m = logical(2, 2)))
    d <- diag(m)
    as.integer(d)
  }

  m <- matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2)
  expect_match(
    as.character(r2f(fn)),
    "integer(c_int) :: d(2)",
    fixed = TRUE
  )
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(m))
})

test_that("diag() initializes integer-backed logical outputs as integers", {
  fn <- function(x) {
    declare(type(x = logical(2)))
    diag(x, 3L, 4L)
  }

  expect_match(as.character(r2f(fn)), "out_ = 0_c_int", fixed = TRUE)
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, list(c(TRUE, FALSE)))
})
