test_that("case-sensitive variable name clashes", {
  expect_error(regexp = "case-insensitive.+`j`.+`J`", {
    quick(function(j) {
      declare(type(j = integer(1)))
      J <- double(length = j)
      J
    })
  })
})
