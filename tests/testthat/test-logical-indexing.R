# Unit tests for indexing external logical arrays (passed from R as logical,
# represented as 0/1 integer storage inside the generated Fortran interface).

test_that("scalar logical indexing works in ifelse()", {
  fn <- function(pred) {
    declare(type(pred = logical(3, 4)))
    out <- double(1)
    out[1] <- ifelse(pred[2, 3], 1.0, 0.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Regression: indexing external logical arrays must compile (no invalid subscripting of booleanized expressions)."
  )

  set.seed(1)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(pred))
})

test_that("indexing a logical expression works (comparison)", {
  fn <- function(x) {
    declare(type(x = double(3, 4)))
    out <- double(1)
    out[1] <- ifelse(((((x > 0.0))))[2, 3], 1.0, 0.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Ensures indexing a logical expression compiles by hoisting the array expression to a temporary before subscripting (no invalid (expr)(i,j))."
  )

  set.seed(1)
  x <- matrix(runif(12, -1, 1), 3, 4)
  expect_quick_identical(fn, list(x))
})

test_that("indexing a logical expression works (boolean ops)", {
  fn <- function(x) {
    declare(type(x = double(3, 4)))
    out <- double(1)
    out[1] <- ifelse(((((x > 0.0) & (x < 0.5))))[2, 3], 1.0, 0.0)
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Ensures indexing a boolean expression compiles by hoisting to a temporary before subscripting (no invalid (expr)(i,j))."
  )

  set.seed(1)
  x <- matrix(runif(12, -1, 1), 3, 4)
  expect_quick_identical(fn, list(x))
})

test_that("scalar logical indexing works in if() with nested boolean logic", {
  fn <- function(x, pred) {
    declare(type(x = double(3, 4)), type(pred = logical(3, 4)))
    out <- double(1)
    out[1] <- 0.0

    # exercise multiple reads of pred[i, j] inside a larger condition
    if ((pred[2, 3] && (x[2, 3] > 0.0)) || (!pred[2, 3] && (x[2, 3] < 0.0))) {
      out[1] <- x[2, 3] + 1.0
    } else {
      out[1] <- x[2, 3] - 1.0
    }

    out
  }

  set.seed(1)
  x <- matrix(runif(12, -1, 1), 3, 4)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(x, pred))
})

test_that("slice logical indexing works in nested ifelse() expressions", {
  fn <- function(x, pred) {
    declare(type(x = double(3, 4)), type(pred = logical(3, 4)))
    out <- double(4)

    # vector mask + nested ifelse -> merge(merge(...), merge(...), mask)
    out <- ifelse(
      pred[2, ],
      ifelse(x[2, ] > 0.0, x[2, ] + 1.0, x[2, ] + 2.0),
      ifelse(x[2, ] < 0.0, x[2, ] - 1.0, x[2, ] - 2.0)
    )

    out
  }

  set.seed(1)
  x <- matrix(runif(12, -1, 1), 3, 4)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(x, pred))
})

test_that("logical slice indexing works for pred[, j] and pred[i, ]", {
  fn <- function(pred) {
    declare(type(pred = logical(3, 4)))
    out <- double(2)
    out[1] <- sum(ifelse(pred[, 2], 1.0, 0.0))
    out[2] <- sum(ifelse(pred[3, ], 1.0, 0.0))
    out
  }

  set.seed(1)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(pred))
})

test_that("drop = FALSE logical indexing works and compiles in larger expressions", {
  fn <- function(x, pred) {
    declare(type(x = double(3, 4)), type(pred = logical(3, 4)))
    out <- double(1)

    # keep dims (1x1), then use it in a larger scalar expression
    out[1] <- sum(ifelse(pred[2, 3, drop = FALSE], x[2, 3, drop = FALSE], 0.0))
    out[1] <- out[1] + sum(ifelse(!pred[2, 3, drop = FALSE], 1.0, 0.0))

    out
  }

  set.seed(1)
  x <- matrix(runif(12, -1, 1), 3, 4)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(x, pred))
})

test_that("logical indexing inside nested loops drives elementwise updates", {
  fn <- function(x, pred) {
    declare(type(x = double(3, 4)), type(pred = logical(3, 4)))
    out <- matrix(0.0, 3, 4)

    for (i in 1:3) {
      for (j in 1:4) {
        if (pred[i, j]) {
          if (x[i, j] > 0.0) {
            out[i, j] <- x[i, j] * 2.0
          } else {
            out[i, j] <- (-x[i, j]) * 3.0
          }
        } else {
          # mix in scalar logical-as-double conversion to exercise both paths
          out[i, j] <- x[i, j] + as.double(pred[i, j])
        }
      }
    }

    out
  }

  set.seed(1)
  x <- matrix(runif(12, -1, 1), 3, 4)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(x, pred))
})

test_that("rank-3 external logical indexing works in complex reductions", {
  fn <- function(x, pred) {
    declare(type(x = double(2, 3, 4)), type(pred = logical(2, 3, 4)))
    out <- double(1)
    out[1] <- 0.0

    for (i in 1:2) {
      for (j in 1:3) {
        for (k in 1:4) {
          # more complex expression mixing arithmetic and boolean logic
          if (pred[i, j, k] && (x[i, j, k] > 0.25)) {
            out[1] <- out[1] + x[i, j, k] * 2.0
          } else if ((!pred[i, j, k]) && (x[i, j, k] < -0.25)) {
            out[1] <- out[1] - x[i, j, k] * 3.0
          } else {
            out[1] <- out[1] + (x[i, j, k] + as.double(pred[i, j, k]))
          }
        }
      }
    }

    out
  }

  set.seed(1)
  x <- array(runif(24, -1, 1), dim = c(2, 3, 4))
  pred <- array(sample(c(TRUE, FALSE), 24, TRUE), dim = c(2, 3, 4))
  expect_quick_identical(fn, list(x, pred))
})

test_that("logical indexing can be combined with other vector masks and reductions", {
  fn <- function(x, pred) {
    declare(type(x = double(3, 4)), type(pred = logical(3, 4)))
    out <- double(1)

    # build a numeric row, then reduce it; mask combines pred row with x-based mask
    out[1] <- sum(ifelse(
      pred[2, ] & (x[2, ] > 0.0),
      x[2, ] * 2.0,
      x[2, ] / 2.0
    ))
    out
  }

  set.seed(1)
  x <- matrix(runif(12, -1, 1), 3, 4)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(x, pred))
})

test_that("external logical masks work in x[pred] (pack / masked reduction)", {
  fn <- function(x, pred) {
    declare(type(x = double(3, 4)), type(pred = logical(3, 4)))
    out <- double(1)
    out[1] <- sum(x[pred])
    out
  }

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  pred <- matrix(sample(c(TRUE, FALSE), 12, TRUE), 3, 4)
  expect_quick_identical(fn, list(x, pred))
})

test_that("logical declared inputs reject integer 0/1 arrays (strict typing)", {
  fn <- function(pred) {
    declare(type(pred = logical(3, 4)))
    out <- double(1)
    out[1] <- ifelse(pred[2, 3], 1.0, 0.0)
    out
  }

  qfn <- quick(fn)
  set.seed(1)
  pred <- matrix(sample(0:1, 12, TRUE), 3, 4)
  expect_error(qfn(pred), "typeof\\(pred\\) must be 'logical'")
})

test_that("expression indexing with logical mask avoids extra materialization", {
  fn <- function(x, y, z, a) {
    declare(
      type(x = double(3, 4)),
      type(y = double(3, 4)),
      type(z = double(3, 4)),
      type(a = double(1))
    )
    out <- double(1)
    out[1] <- sum((x + y)[z > a])
    out
  }

  expect_translation_snapshots(
    fn,
    note = "Mask subsetting should stay as a single pack-like operation (avoid pushing mask into operands and duplicating mask evaluation)."
  )

  set.seed(1)
  x <- matrix(runif(12), 3, 4)
  y <- matrix(runif(12), 3, 4)
  z <- matrix(runif(12), 3, 4)
  a <- 0.5
  expect_quick_identical(fn, list(x, y, z, a))
})
