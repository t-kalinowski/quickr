test_that("r2f() attaches `r` metadata for bind(c) logical symbols", {
  # Regression test: r2f() must attach attr(,"r") even for the symbol-lowering
  # path used by bind(c) logical arguments. Missing `r` can break downstream
  # compilation logic that relies on var@r (e.g. size inference).
  fn <- function(m) {
    declare(type(m = logical(NA)))
    a <- m
    a
  }

  fsub <- quickr:::r2f(fn)
  a_var <- get0("a", fsub@scope)
  expect_true(inherits(a_var, Variable))
  expect_identical(a_var@r, quote(m))
})

