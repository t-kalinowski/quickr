test_that("quick compiles and calls a rebound namespace handler", {
  original <- lower_short_circuit_operator
  called <- FALSE
  # Coverage instrumentation rebinds handlers after package registration.
  local_mocked_bindings(
    lower_short_circuit_operator = function(...) {
      called <<- TRUE
      original(...)
    }
  )

  fn <- function(x) {
    declare(type(x = logical(1)))
    x && TRUE
  }
  expect_quick_identical(fn, TRUE, FALSE)
  expect_true(called)
})
