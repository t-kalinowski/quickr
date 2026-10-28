for (op in c("&&", "||")) {
  for (expr in alist(
    TRUE == z,
    z != TRUE,
    (TRUE + z) == 0,
    abs(TRUE + z) > 0
  )) {
    test_that(paste(op, "defers unsupported conversions in", deparse1(expr)), {
      left <- if (op == "&&") quote(flag) else quote(!flag)
      condition <- as.call(list(as.name(op), left, expr))
      fn <- eval(bquote(function(flag, z) {
        declare(type(flag = logical(1)), type(z = complex(1)))
        .(condition)
      }))
      qfn <- quick(fn)
      expect_identical(qfn(FALSE, 1i), fn(FALSE, 1i))
      expect_error(
        qfn(TRUE, 1i),
        "does not support coercion from logical to complex"
      )
    })
  }
}

test_that("literal unsupported conversions stay inside skipped branches", {
  and <- function() {
    FALSE && (TRUE == 1i)
  }
  or <- function() {
    TRUE || (1i != TRUE)
  }
  expect_quick_identical(and, list())
  expect_quick_identical(or, list())
})

test_that("short-circuit comparisons retain supported real-mode promotions", {
  fn <- function(flag, x) {
    declare(type(flag = logical(1)), type(x = double(1)))
    flag && (abs(TRUE + x) == 2)
  }
  expect_quick_identical(fn, list(FALSE, 1), list(TRUE, 1), list(TRUE, 2))
})
