# Unit tests for r2f handler registry

test_that("register_r2f_handler sets dest_supported attribute", {
  handler <- function(e, scope, ...) NULL
  result <- quickr:::register_r2f_handler(
    "test_handler_dest",
    handler,
    dest_supported = TRUE
  )
  expect_identical(attr(result, "dest_supported"), TRUE)
})

test_that("register_r2f_handler sets dest_infer attribute", {
  handler <- function(e, scope, ...) NULL
  infer_fn <- function(args, scope) NULL
  result <- quickr:::register_r2f_handler(
    "test_handler_infer",
    handler,
    dest_infer = infer_fn
  )
  if (identical(Sys.getenv("R_COVR"), "true")) {
    expect_identical(attr(result, "dest_infer"), "infer_fn")
  } else {
    expect_identical(attr(result, "dest_infer"), infer_fn)
  }
})

test_that("register_r2f_handler sets match.fun attribute when not TRUE", {
  handler <- function(e, scope, ...) NULL
  match_fn <- function(x, envir) x
  result <- quickr:::register_r2f_handler(
    "test_handler_match",
    handler,
    match_fun = match_fn
  )
  expect_identical(attr(result, "match.fun"), match_fn)
})

test_that("register_r2f_handler does not set match.fun when TRUE", {
  handler <- function(e, scope, ...) NULL
  result <- quickr:::register_r2f_handler(
    "test_handler_match_true",
    handler,
    match_fun = TRUE
  )
  expect_null(attr(result, "match.fun"))
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
