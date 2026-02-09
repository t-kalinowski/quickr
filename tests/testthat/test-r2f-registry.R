# Unit tests for r2f handler registry

test_that("register_r2f_handler sets dest_supported attribute", {
  handler <- function(e, scope, ...) NULL
  result <- quickr:::register_r2f_handler(
    "test_handler_dest",
    handler,
    dest_supported = TRUE
  )
  expect_true(inherits(result, quickr:::R2FHandler))
  expect_identical(result@dest_supported, TRUE)
})

test_that("register_r2f_handler stores dest_infer metadata", {
  handler <- function(e, scope, ...) NULL
  infer_fn <- function(args, scope) NULL
  result <- quickr:::register_r2f_handler(
    "test_handler_infer",
    handler,
    dest_infer = infer_fn
  )
  expect_true(inherits(result, quickr:::R2FHandler))
  expect_identical(result@dest_infer, infer_fn)
  expect_identical(result@dest_infer_name, "infer_fn")
})

test_that("register_r2f_handler keeps anonymous dest_infer without a name", {
  handler <- function(e, scope, ...) NULL
  infer_fn <- function(args, scope) NULL
  result <- quickr:::register_r2f_handler(
    "test_handler_infer_anon",
    handler,
    dest_infer = (function(args, scope) infer_fn(args, scope))
  )
  expect_true(inherits(result, quickr:::R2FHandler))
  expect_true(is.function(result@dest_infer))
  expect_null(result@dest_infer_name)
})

test_that("register_r2f_handler sets match.fun attribute when not TRUE", {
  handler <- function(e, scope, ...) NULL
  match_fn <- function(x, envir) x
  result <- quickr:::register_r2f_handler(
    "test_handler_match",
    handler,
    match_fun = match_fn
  )
  expect_true(inherits(result, quickr:::R2FHandler))
  expect_identical(result@match_fun, match_fn)
})

test_that("register_r2f_handler does not set match.fun when TRUE", {
  handler <- function(e, scope, ...) NULL
  result <- quickr:::register_r2f_handler(
    "test_handler_match_true",
    handler,
    match_fun = TRUE
  )
  expect_true(inherits(result, quickr:::R2FHandler))
  expect_null(result@match_fun)
})

test_that("register_r2f_handler registers multiple names", {
  handler <- function(e, scope, ...) NULL
  withr::defer(
    rm(list = c("multi_test_a", "multi_test_b"), envir = quickr:::r2f_handlers),
    envir = environment()
  )
  quickr:::register_r2f_handler(
    c("multi_test_a", "multi_test_b"),
    handler
  )
  expect_identical(
    quickr:::r2f_handlers[["multi_test_a"]],
    quickr:::r2f_handlers[["multi_test_b"]]
  )
})
