test_that("short-circuit operators defer infinite literal diagnostics", {
  comparisons <- c(
    alist(Inf > 0, -Inf < 0, 1e1000 > 0),
    list(call("==", complex(real = Inf, imaginary = 1), 1i))
  )
  for (comparison in comparisons) {
    for (op in c("&&", "||")) {
      expr <- as.call(list(as.name(op), quote(flag), comparison))
      fn <- eval(bquote(function(flag) {
        declare(type(flag = logical(1)))
        .(expr)
      }))
      skip <- identical(op, "||")
      expect_quick_identical(fn, list(skip))
      qfn <- quick(fn)
      expect_error(
        qfn(!skip),
        "Infinite literals are not supported",
        fixed = TRUE
      )
    }
  }
})

test_that("ifelse defers infinite literal diagnostics until selection", {
  yes <- function(flag) {
    declare(type(flag = logical(1)))
    ifelse(flag, Inf, 1)
  }
  no <- function(flag) {
    declare(type(flag = logical(1)))
    ifelse(flag, 1, -Inf)
  }

  expect_quick_identical(yes, list(FALSE))
  expect_quick_identical(no, list(TRUE))
  expect_error(
    quick(yes)(TRUE),
    "Infinite literals are not supported",
    fixed = TRUE
  )
  expect_error(
    quick(no)(FALSE),
    "Infinite literals are not supported",
    fixed = TRUE
  )
})

test_that("infinite literals are refused without restricting runtime values", {
  literal <- function() {
    Inf
  }
  expect_error(
    quick(literal),
    "Infinite literals are not supported",
    fixed = TRUE
  )

  argument <- function(flag, x) {
    declare(type(flag = logical(1)), type(x = double(1)))
    flag && (x > 0)
  }
  expect_quick_identical(
    argument,
    list(FALSE, Inf),
    list(TRUE, Inf),
    list(TRUE, -Inf)
  )
})
