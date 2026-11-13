test_that("input dimension contracts validate operands used in loops", {
  fn <- function(x, y) {
    declare(type(x = double(n)), type(y = double(n)))
    out <- numeric(3)
    for (i in 1:3) {
      out[i] <- sum(x %*% y)
    }
    out
  }

  expect_quick_identical(fn, list(c(1, 2), c(3, 4)), list(double(), double()))
  qfn <- quick(fn)
  expect_error(
    qfn(c(1, 2), c(3, 4, 5)),
    "length(y) must equal length(x)",
    fixed = TRUE
  )
})

test_that("dimension contracts distinguish promoted singleton axes", {
  fn <- function(x, y) {
    declare(type(x = double(2, n)), type(y = double(n)))
    tcrossprod(x, y)
  }

  expect_quick_identical(fn, list(matrix(1, 2, 1), 2))
  qfn <- quick(fn)
  expect_error(
    qfn(matrix(1, 2, 3), c(1, 2, 3)),
    "non-conformable arguments in tcrossprod",
    fixed = TRUE
  )

  # In the opposite argument order the vector is a row, and its length is
  # the contracted dimension rather than a promoted singleton axis.
  reversed <- function(x, y) {
    declare(type(x = double(n)), type(y = double(2, n)))
    tcrossprod(x, y)
  }
  expect_quick_identical(
    reversed,
    list(2, matrix(1, 2, 1)),
    list(c(1, 2, 3), matrix(as.double(1:6), 2, 3))
  )
})

test_that("input dimension proofs follow retained array axes", {
  rows <- function(x, y) {
    declare(type(x = double(n, m)), type(y = double(m)))
    total <- 0
    for (i in seq_len(nrow(x))) {
      total <- total + sum(x[i, ] + y)
    }
    total
  }
  row_count <- function(x, y) {
    declare(type(x = double(n, m)), type(y = double(n)))
    sum(x[1, ] + y)
  }

  expect_quick_identical(rows, list(matrix(as.double(1:6), 2, 3), c(1, 2, 3)))
  # The retained row extent comes from input axis 2, now result axis 1.
  expect_false(grepl("/=", as.character(r2f(rows)), fixed = TRUE))

  expect_quick_identical(row_count, list(matrix(as.double(1:4), 2, 2), c(1, 2)))
  expect_error(
    quick(row_count)(matrix(as.double(1:6), 2, 3), c(1, 2)),
    "equal lengths",
    fixed = TRUE
  )
})

test_that("shadowed arrays do not inherit root input dimension contracts", {
  for (body in alist(
    {
      declare(type(x = double(n, n)))
      x <- matrix(1, n, n)
      sum(x %*% y)
    },
    {
      declare(type(z = double(n, n)))
      z <- matrix(1, n, n)
      g <- function(x) sum(x %*% y)
      g(z)
    }
  )) {
    fn <- eval(substitute(
      function(n, x, y, local_n) {
        declare(
          type(n = integer(1)),
          type(x = double(n, n)),
          type(y = double(n, n)),
          type(local_n = integer(1))
        )
        f <- function(n) BODY
        f(local_n)
      },
      list(BODY = body)
    ))

    qfn <- quick(fn)
    x <- y <- matrix(1, 3, 3)
    expect_identical(qfn(3L, x, y, 3L), fn(3L, x, y, 3L))
    expect_error(
      qfn(3L, x, y, 2L),
      "non-conformable arguments in %*%",
      fixed = TRUE
    )
  }
})

test_that("sapply evaluates pure closures after guarded expressions", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    values <- x + y
    sapply(values, function(z) z * 2)
  }

  expect_quick_identical(fn, list(c(1, 2, 3), c(4, 5, 6)))
  qfn <- quick(fn)
  expect_error(qfn(c(1, 2), c(3, 4, 5)), "equal lengths", fixed = TRUE)
})

test_that("replacement values precede guards in computed indices", {
  local <- function(i, j) {
    declare(type(i = integer(NA)), type(j = integer(NA)))
    out <- c(0, 0, 0)
    out[i + j] <- runif(1L)
    out
  }
  host <- function(i, j) {
    declare(type(i = integer(NA)), type(j = integer(NA)))
    out <- c(0, 0, 0)
    replace <- function() {
      out[i + j] <<- runif(1L)
      NULL
    }
    replace()
    out
  }

  withr::local_seed(151)
  for (fn in list(local, host)) {
    qfn <- quick(fn)
    for (indices in list(integer(), c(1L, 2L))) {
      offsets <- integer(length(indices))
      set.seed(151)
      expected <- fn(indices, offsets)
      expected_seed <- .Random.seed
      set.seed(151)
      expect_identical(qfn(indices, offsets), expected)
      expect_identical(.Random.seed, expected_seed)
    }

    set.seed(151)
    runif(1L)
    expected_seed <- .Random.seed
    set.seed(151)
    expect_error(qfn(c(1L, 2L), 0L), "equal lengths", fixed = TRUE)
    expect_identical(.Random.seed, expected_seed)
  }
})

test_that("nested closures still propagate errors from their callees", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    f <- function(z) {
      g <- function(v) {
        if (v < 0) {
          stop("negative input")
        }
        v * 2
      }
      g(z) + 1
    }
    sapply(x, f)
  }

  expect_quick_identical(fn, c(1, 2, 3))
  qfn <- quick(fn)
  expect_error(qfn(c(1, -2, 3)), "negative input", fixed = TRUE)
})
