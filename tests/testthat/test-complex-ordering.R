for (op in c("<", "<=", ">", ">=")) {
  test_that(paste(op, "defers complex ordering in either operand position"), {
    for (operands in list(alist(z, 0), alist(0, z))) {
      comparison <- as.call(c(list(as.name(op)), operands))
      fn <- eval(bquote(function(flag, z) {
        declare(type(flag = logical(1)), type(z = complex(1)))
        flag && .(comparison)
      }))

      qfn <- quick(fn)
      expect_identical(qfn(FALSE, 1i), fn(FALSE, 1i))
      expect_error(
        qfn(TRUE, 1i),
        "invalid comparison with complex values",
        fixed = TRUE
      )
    }
  })
}

test_that("or defers ordering with a complex right operand", {
  or <- function(flag, z) {
    declare(type(flag = logical(1)), type(z = complex(1)))
    flag || (0 < z)
  }

  qor <- quick(or)
  expect_identical(qor(TRUE, 1i), or(TRUE, 1i))
  expect_error(
    qor(FALSE, 1i),
    "invalid comparison with complex values",
    fixed = TRUE
  )
})
