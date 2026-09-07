test_that("ifelse rejects assignments before they can change compiler scope", {
  skipped_yes <- function() {
    out <- ifelse(FALSE, x <- 1, 2)
    x
  }
  skipped_no <- function() {
    out <- ifelse(TRUE, 2, (x <- 1) + 1)
    x
  }

  for (fn in list(skipped_yes, skipped_no)) {
    expect_error(
      quick(fn),
      "ifelse() does not support assignment expressions",
      fixed = TRUE
    )
  }

  closure_local <- function(flag) {
    declare(type(flag = logical(1)))
    ifelse(
      flag,
      (function() {
        x <- 1
        x
      })(),
      2
    )
  }
  expect_quick_identical(closure_local, list(FALSE), list(TRUE))
})

test_that("lazy branches compile overflowing exp constants", {
  and <- function(flag) {
    declare(type(flag = logical(1)))
    flag && exp(1000) > 0
  }
  or <- function(flag) {
    declare(type(flag = logical(1)))
    flag || exp(1000) > 0
  }
  select <- function(flag) {
    declare(type(flag = logical(1)))
    ifelse(flag, exp(1000), 0)
  }

  expect_quick_identical(and, list(FALSE), list(TRUE))
  expect_quick_identical(or, list(FALSE), list(TRUE))
  expect_quick_identical(select, list(FALSE), list(TRUE))
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
  yes <- function() {
    out <- ifelse(
      FALSE,
      {
        for (x in 1L:1L) {}
        NULL
      },
      1L
    )
    x
  }
  no <- function() {
    out <- ifelse(TRUE, 1L, {
      for (x in 1L:1L) {}
      NULL
    })
    x
  }
  for (fn in list(and, or, yes, no)) {
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
      },
    ifelse(
      FALSE,
      {
        declare(type(x = logical(1)))
        x
      },
      TRUE
    ),
    ifelse(TRUE, TRUE, {
      declare(type(x = logical(1)))
      x
    })
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

test_that("constant scalar ifelse selectors retain the selected raw mode", {
  yes <- function(x) {
    declare(type(x = raw(1)))
    ifelse(TRUE, x, 1L)
  }
  no <- function(x) {
    declare(type(x = raw(1)))
    ifelse(FALSE, 1L, x)
  }
  integer <- function(x) {
    declare(type(x = raw(1)))
    ifelse(TRUE, 1L, x)
  }

  expect_quick_identical(yes, list(as.raw(0)), list(as.raw(255)))
  expect_quick_identical(no, list(as.raw(0)), list(as.raw(255)))
  expect_quick_identical(integer, list(as.raw(255)))
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
