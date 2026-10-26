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
        "ordering comparisons do not support complex operands",
        fixed = TRUE
      )
    }
  })
}

test_that("or and ifelse defer ordering with a complex right operand", {
  or <- function(flag, z) {
    declare(type(flag = logical(1)), type(z = complex(1)))
    flag || (0 < z)
  }
  select <- function(flag, z) {
    declare(type(flag = logical(1)), type(z = complex(1)))
    ifelse(flag, 0 < z, FALSE)
  }

  qor <- quick(or)
  expect_identical(qor(TRUE, 1i), or(TRUE, 1i))
  expect_error(
    qor(FALSE, 1i),
    "ordering comparisons do not support complex operands",
    fixed = TRUE
  )

  qselect <- quick(select)
  expect_identical(qselect(FALSE, 1i), select(FALSE, 1i))
  expect_error(
    qselect(TRUE, 1i),
    "ordering comparisons do not support complex operands",
    fixed = TRUE
  )
})
