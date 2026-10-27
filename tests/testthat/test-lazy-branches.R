test_that("lazy branches compile overflowing exp constants", {
  and <- function(flag) {
    declare(type(flag = logical(1)))
    flag && exp(1000) > 0
  }
  or <- function(flag) {
    declare(type(flag = logical(1)))
    flag || exp(1000) > 0
  }

  expect_quick_identical(and, list(FALSE), list(TRUE))
  expect_quick_identical(or, list(FALSE), list(TRUE))
})

test_that("lazy branches reject for bindings in the current scope", {
  and <- function() {
    out <- FALSE &&
      {
        for (x in 1L:1L) {}
        NULL
      }
    x
  }
  or <- function() {
    out <- TRUE ||
      {
        for (x in 1L:1L) {}
        NULL
      }
    x
  }
  for (fn in list(and, or)) {
    expect_error(
      quick(fn),
      "does not support assignment expressions",
      fixed = TRUE
    )
  }

  closure_local <- function(flag) {
    declare(type(flag = logical(1)))
    flag &&
      (function() {
        for (x in 1L:1L) {}
        x > 0L
      })()
  }
  expect_quick_identical(closure_local, list(FALSE), list(TRUE))
})

test_that("lazy branches reject declarations in the current scope", {
  expressions <- alist(
    FALSE &&
      {
        declare(type(x = logical(1)))
        x
      },
    TRUE ||
      {
        declare(type(x = logical(1)))
        x
      }
  )
  for (expr in expressions) {
    fn <- eval(bquote(function() {
      out <- .(expr)
      x
    }))
    expect_error(
      quick(fn),
      "does not support assignment expressions",
      fixed = TRUE
    )
  }

  separate <- function(flag) {
    declare(type(flag = logical(1)), type(x = logical(1)))
    x <- TRUE
    flag && x
  }
  expect_quick_identical(separate, list(FALSE), list(TRUE))
})

test_that("scalar logic rejects statement-only print operands", {
  and <- function(flag, x) {
    declare(type(flag = logical(1)), type(x = logical(1)))
    flag && print(x)
  }
  or <- function(flag, x) {
    declare(type(flag = logical(1)), type(x = logical(1)))
    flag || print(x)
  }

  for (fn in list(and, or)) {
    expect_error(quick(fn), "statement-only operands", fixed = TRUE)
  }

  separate <- function(x) {
    declare(type(x = logical(1)))
    print(x)
    TRUE && x
  }
  qfn <- quick(separate)
  output <- capture.output(value <- qfn(TRUE))
  expect_identical(value, TRUE)
  expect_match(paste(output, collapse = "\n"), "1")
})
