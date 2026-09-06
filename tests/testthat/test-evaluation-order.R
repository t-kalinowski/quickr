test_that("local closure calls reject effectful argument promises", {
  forward <- function() {
    pair <- function(first, second) {
      c(first, second)
    }
    pair(runif(1), runif(1) + 1)
  }
  reverse <- function() {
    pair <- function(first, second) {
      c(second, first)
    }
    pair(runif(1), runif(1) + 1)
  }

  message <- "local closure calls only support pure argument expressions"
  expect_error(quick(forward), message, fixed = TRUE)
  expect_error(quick(reverse), message, fixed = TRUE)
})
test_that("cbind/rbind evaluate effectful arguments from left to right", {
  cbind_fn <- function() {
    cbind(runif(2), runif(2) + 1)
  }
  rbind_fn <- function() {
    rbind(runif(2), runif(2) + 1)
  }

  set.seed(914)
  cbind_expected <- cbind_fn()
  set.seed(914)
  expect_identical(quick(cbind_fn)(), cbind_expected)

  set.seed(915)
  rbind_expected <- rbind_fn()
  set.seed(915)
  expect_identical(quick(rbind_fn)(), rbind_expected)
})
test_that("later subscript effects cannot change the evaluated base", {
  fn <- function(x) {
    declare(type(x = double(2)))
    first <- function() {
      x[] <<- x + 1
      1L
    }
    x[first()]
  }

  expect_quick_identical(fn, list(c(3, 4)))

  rep_index <- function(v) {
    declare(type(v = integer(2)))
    i <- 1L
    count <- function() {
      i <<- 2L
      2L
    }
    sum(v[rep.int(i, count())])
  }
  expect_quick_identical(rep_index, list(10:11))
})
test_that("later matrix effects cannot change the evaluated left operand", {
  fn <- function(x) {
    declare(type(x = double(1, 1)))
    rhs <- function() {
      x <<- x + 1
      matrix(2, 1, 1)
    }
    x %*% rhs()
  }

  expect_quick_identical(fn, list(matrix(3, 1, 1)))
})
