test_that("operator normalization handles symbol-free calls (public API)", {
  fn <- function(x) {
    declare(type(x = integer(1)))
    abs(-1L) + x
  }
  expect_quick_identical(fn, 1L, 10L)
})

test_that("C bridge hoisting can be disabled (public API)", {
  old <- options(quickr.c_bridge.hoist = FALSE)
  on.exit(options(old), add = TRUE)

  fn <- function(x, y, n) {
    declare({
      type(n = integer(1))
      type(x = double(n))
      type(y = double(n + 1))
      type(out = double(n))
    })
    out <- x + 1
    out
  }

  qfn <- quick(fn)
  expect_identical(qfn(c(1, 2), c(10, 11, 12), 2L), c(2, 3))
})

test_that("C-bridge size-expression errors are surfaced through quick()", {
  mk <- function(dim_expr, include_n = TRUE) {
    if (isTRUE(include_n)) {
      eval(bquote(function(n, x) {
        declare(type(n = integer(1)), type(x = double(.(dim_expr))))
        x
      }))
    } else {
      eval(bquote(function(x) {
        declare(type(x = double(.(dim_expr))))
        x
      }))
    }
  }

  expect_error(
    quick(mk(quote(x[1]), include_n = FALSE)),
    "unsupported size expression"
  )
  expect_error(
    quick(mk(as.call(list(as.name("("), quote(unknown))))),
    "could not resolve size"
  )
  expect_error(quick(mk(quote(dim(unknown)[1]))), "could not resolve size")
  expect_error(quick(mk(quote(unknown + 1))), "could not resolve size")
})

test_that("rank-0 arguments are rejected by the C bridge (public API)", {
  fn <- function(n) {
    declare(type(n = integer()))
    n
  }
  expect_error(quick(fn), "bad rank")
})
