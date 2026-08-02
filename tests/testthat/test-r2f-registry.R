# Unit tests for r2f handler registry

skip_on_cran()

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

test_that("register_r2f_handler records the name of a namespace-level handler", {
  withr::defer(
    rm(list = "test_handler_named", envir = quickr:::r2f_handlers),
    envir = environment()
  )
  # Passed as a bare symbol, the way the package's own top-level registrations
  # do it -- `quickr:::last` would be a call, with no name to record.
  result <- quickr:::register_r2f_handler("test_handler_named", last)
  expect_identical(result@fun_name, "last")
})

test_that("register_r2f_handler leaves anonymous and local handlers unnamed", {
  local_handler <- function(e, scope, ...) NULL
  withr::defer(
    rm(
      list = c("test_handler_unnamed_anon", "test_handler_unnamed_local"),
      envir = quickr:::r2f_handlers
    ),
    envir = environment()
  )
  anon <- quickr:::register_r2f_handler(
    "test_handler_unnamed_anon",
    function(e, scope, ...) NULL
  )
  # A symbol, but bound in a call frame rather than a namespace, so the name
  # would mean something else the next time the frame is entered.
  local <- quickr:::register_r2f_handler(
    "test_handler_unnamed_local",
    local_handler
  )
  expect_null(anon@fun_name)
  expect_null(local@fun_name)
})

test_that("dispatch re-resolves a named handler's namespace binding", {
  # covr rebinds its instrumented copies into the namespace after the package
  # has loaded -- that is, after registration captured the function object.
  # Mocking the binding reproduces that sequence exactly.
  original <- last
  withr::defer(
    rm(list = "test_handler_rebound", envir = quickr:::r2f_handlers),
    envir = environment()
  )
  registered <- quickr:::register_r2f_handler("test_handler_rebound", last)
  expect_identical(registered@fun_name, "last")

  local_mocked_bindings(last = function(x) "rebound")
  resolved <- quickr:::get_r2f_handler(quote(test_handler_rebound))
  expect_identical(resolved("ignored"), "rebound")

  # Resolving hands back a copy; the registry still holds what was registered.
  expect_identical(
    S7::S7_data(quickr:::r2f_handlers[["test_handler_rebound"]]),
    original
  )
})

test_that("dispatch leaves unnamed handlers alone", {
  handler <- function(e, scope, ...) "anonymous"
  withr::defer(
    rm(list = "test_handler_untouched", envir = quickr:::r2f_handlers),
    envir = environment()
  )
  quickr:::register_r2f_handler("test_handler_untouched", handler)
  resolved <- quickr:::get_r2f_handler(quote(test_handler_untouched))
  expect_null(resolved@fun_name)
  expect_identical(S7::S7_data(resolved), handler)
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
