test_that("numeric reductions accept named data and the default na.rm", {
  for (reducer in c("min", "max", "sum", "prod")) {
    fn <- eval(bquote(function(x, y) {
      declare(type(x = double(NA)), type(y = double(NA)))
      .(as.name(reducer))(first = x, na.rm = FALSE, second = y)
    }))
    expect_quick_equal(
      fn,
      list(c(1, 2, 3), c(4, 5)),
      list(numeric(), c(4, 5)),
      list(c(1, 2, 3), numeric())
    )
  }
})

test_that("only the exact na.rm name selects the reduction option", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    sum(na.r = x, named = 2, na.rm = FALSE)
  }
  expect_quick_identical(fn, c(1, 2, 3), numeric())
})

test_that("logical reductions accept named data and the default na.rm", {
  for (reducer in c("any", "all")) {
    fn <- eval(bquote(function(x) {
      declare(type(x = logical(NA)))
      .(as.name(reducer))(na.rm = FALSE, values = x)
    }))
    expect_quick_identical(fn, c(TRUE, FALSE), TRUE, FALSE, logical())
  }
})

test_that("reduction identities work with no data arguments", {
  for (reducer in c("sum", "prod", "any", "all")) {
    for (explicit_default in c(FALSE, TRUE)) {
      call <- if (explicit_default) {
        as.call(list(as.name(reducer), na.rm = FALSE))
      } else {
        as.call(list(as.name(reducer)))
      }
      fn <- eval(bquote(function() {
        .(call)
      }))
      expect_quick_identical(fn, list())
    }
  }
})

test_that("na.rm expressions and duplicate options are rejected", {
  for (reducer in c("min", "max", "sum", "prod", "any", "all")) {
    fn <- eval(bquote(function(x, remove_missing) {
      declare(type(x = double(NA)), type(remove_missing = logical(1)))
      .(as.name(reducer))(x, na.rm = remove_missing)
    }))
    expect_error(quick(fn), "`na.rm` must be the literal FALSE", fixed = TRUE)

    fn <- eval(bquote(function(x) {
      declare(type(x = double(NA)))
      .(as.name(reducer))(x, na.rm = FALSE, na.rm = FALSE)
    }))
    expect_error(
      quick(fn),
      'formal argument "na.rm" matched by multiple actual arguments',
      fixed = TRUE
    )
  }
})
