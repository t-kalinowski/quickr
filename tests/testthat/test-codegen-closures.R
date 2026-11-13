skip_on_cran()

# These complete translations complement the native closure regressions in
# test-sapply-closures.R, test-local-closure-optional-args-more.R, and
# test-closure-deferred-errors.R.

test_that("closure calls lower safe numeric argument expressions", {
  fn <- function(x) {
    declare(type(x = double(1)))
    combine <- function(half, down, up, whole, magnitude) {
      c(half, down, up, whole, magnitude)
    }
    combine(x / 2, floor(x), ceiling(x), trunc(x), sqrt(abs(x)))
  }
  expect_translation_snapshots(fn)
})

test_that("closure calls lower reductions and stored dimension queries", {
  fn <- function(x) {
    declare(type(x = double(NA, NA)))
    combine <- function(total, product, size, rows, cols) {
      c(total, product, as.double(size), as.double(rows), as.double(cols))
    }
    combine(sum(x), prod(x), length(x), nrow(x), ncol(x))
  }
  expect_translation_snapshots(fn)
})

test_that("unused safe closure arguments keep their inferred native types", {
  fn <- function(x, scale) {
    declare(type(x = integer(1)), type(scale = integer(1)))
    ignore <- function(value, literal) 3L
    ignore(x / scale, 1 / 0)
  }
  expect_translation_snapshots(fn)
})

test_that("root-defined closures retain captures through shadowing callers", {
  fn <- function(x) {
    declare(type(x = double(1)))
    identity_local <- function(value) value
    read <- function() identity_local(x)
    outer <- function(x) read()
    outer(2)
  }
  expect_translation_snapshots(fn)
})

test_that("closure defaults use lexical bindings through local shadows", {
  fn <- function(x) {
    declare(type(x = double(1)))
    read <- function(value = x + 1) value
    outer <- function(z) {
      x <- z + 99
      read()
    }
    outer(2)
  }
  expect_translation_snapshots(fn)
})

test_that("bare defaults preserve captured storage through a write", {
  fn <- function(x) {
    declare(type(x = double(1)))
    read <- function(value = x) {
      x <<- 8
      value
    }
    read()
  }
  expect_translation_snapshots(fn)
})

test_that("compound actuals distinguish formal shadows from captured writes", {
  fn <- function(x) {
    declare(type(x = double(1)))
    change <- function(value) {
      x <<- 8
      value
    }
    outer <- function(x) change(x + 1)
    value <- outer(2)
    c(value, x)
  }
  expect_translation_snapshots(fn)
})

test_that("explicit forwarding keeps nested procedure arguments accessible", {
  fn <- function(x) {
    declare(type(x = double(1)))
    identity_local <- function(value) value
    outer <- function(x) {
      inner <- function(value) identity_local(value / 2)
      inner(x)
    }
    outer(2)
  }
  expect_translation_snapshots(fn)
})

test_that("optional closure arguments lower presence guards and keyword calls", {
  fn <- function(x) {
    declare(type(x = double(1)))
    f <- function(a = NULL, b = NULL) {
      if (is.null(a)) {
        a <- x + 1
      }
      if (is.null(b)) {
        b <- 2
      }
      a + b
    }
    c(f(), f(b = 5), f(a = 10, b = 5))
  }
  expect_translation_snapshots(fn)
})

test_that("closure array formals copy before local modification", {
  fn <- function(x) {
    declare(type(x = double(n)))
    modify <- function(value) {
      value[1L] <- value[1L] + 1
      value
    }
    modified <- modify(x)
    list(original = x, modified = modified)
  }
  expect_translation_snapshots(fn)
})

test_that("error-free closure chains omit the shared error buffer", {
  fn <- function(x) {
    declare(type(x = double(1)))
    add_one <- function(value) value + 1
    outer <- function(value) add_one(value)
    outer(x)
  }
  expect_translation_snapshots(fn)
})

test_that("logical closure results use C-compatible output storage", {
  fn <- function(x) {
    declare(type(x = double(1)))
    positive <- function(value) value > 0
    out <- positive(x)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("closure assignment captures arguments before replacing their target", {
  fn <- function(x) {
    declare(type(x = double(n)))
    add_one <- function(value) value + 1
    x <- add_one(x)
    x
  }
  expect_translation_snapshots(fn)
})

test_that("erroring closure chains return before later RNG effects", {
  fn <- function(flag) {
    declare(type(flag = integer(1)))
    fail <- function(flag) {
      if (flag != 0L) {
        stop("bad")
      }
      NULL
    }
    middle <- function(flag) {
      fail(flag)
      NULL
    }
    outer <- function(flag) {
      middle(flag)
      1
    }
    value <- outer(flag)
    draw <- runif(1)
    value + draw
  }
  expect_translation_snapshots(fn)
})

test_that("sapply propagates errors from root-defined callees", {
  fn <- function(flag) {
    declare(type(flag = integer(1)))
    fail <- function(flag) {
      if (flag != 0L) {
        stop("bad")
      }
      NULL
    }
    step <- function(i) {
      fail(flag)
      i
    }
    values <- sapply(seq_len(2L), step)
    draw <- runif(1)
    sum(values) + draw
  }
  expect_translation_snapshots(fn)
})

test_that("sapply reads unchanged named vector values directly", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    out <- sapply(x, function(value) value * 2)
    out
  }
  expect_translation_snapshots(fn)
})

test_that("sapply materializes a stepped sequence before iteration", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- sapply(seq(1L, n, by = 2L), function(value) as.double(value) + 0.5)
    out
  }
  expect_translation_snapshots(fn)
})
