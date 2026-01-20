test_that("quick() supports mixed-case compiled names", {
  qSim <- quick(
    name = "qSim",
    function(x) {
      declare(type(x = double(n)))
      x + 1
    }
  )

  expect_identical(qSim(c(1, 2, 3)), c(2, 3, 4))
})

test_that("quick() errors early for invalid compiled names", {
  expect_error(
    quick(
      name = "foo.bar",
      function(x) {
        declare(type(x = double(n)))
        x
      }
    ),
    "Invalid `quick\\(\\)` name",
    fixed = FALSE
  )

  expect_error(
    quick(
      name = "1foo",
      function(x) {
        declare(type(x = double(n)))
        x
      }
    ),
    "Suggested name: 'quick_1foo'",
    fixed = TRUE
  )
})
