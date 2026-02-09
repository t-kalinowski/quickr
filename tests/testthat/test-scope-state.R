# Exercise quickr_scope state accessors to keep refactors covered by covr.

test_that("quickr_scope @ access reads and writes state", {
  scope <- quickr:::new_scope(function(x) x, parent = emptyenv())

  expect_identical(scope@kind, "subroutine")
  expect_true(is.function(scope@closure))

  expect_false(quickr:::scope_uses_rng(scope))
  scope@uses_rng <- TRUE
  expect_true(quickr:::scope_uses_rng(scope))
})

test_that(".AtNames.quickr_scope includes state fields", {
  scope <- quickr:::new_scope(NULL, parent = emptyenv())
  nms <- quickr:::.AtNames.quickr_scope(scope)
  expect_true("kind" %in% nms)
  expect_true("get_unique_var" %in% nms)
})
