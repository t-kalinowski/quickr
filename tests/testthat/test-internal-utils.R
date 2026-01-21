# Unit tests for internal helper utilities

test_that("zzz .onLoad can be invoked", {
  expect_error(quickr:::.onLoad(), NA)
})

test_that("nil-coalescing %||% returns first non-NULL", {
  expect_identical(quickr:::`%||%`(NULL, 1), 1)
  expect_identical(quickr:::`%||%`(0, 1), 0)
})

test_that("S7-style := assigns and preserves name", {
  local({
    quickr:::`:=`(f, function() 1L)
    expect_true(is.function(f))
    expect_identical(attr(f, "name"), "f")

    quickr:::`:=`(e, new.env())
    expect_true(is.environment(e))
    expect_identical(attr(e, "name"), "e")

    new_obj <- function(name) name
    quickr:::`:=`(x, new_obj())
    expect_identical(x, "x")

    expect_error(quickr:::`:=`(1, function() 1L), "left hand side")
    expect_error(quickr:::`:=`(bad, 1), "right hand side")
    expect_error(quickr:::`:=`(dup, new_obj(name = "x")), "matched by multiple")
  })
})

test_that("append helpers work for common cases", {
  expect_identical(quickr:::`append<-`(1:2, value = 3L), c(1L, 2L, 3L))
  expect_identical(
    quickr:::`append<-`(1:2, after = 1L, value = 3L),
    c(1L, 3L, 2L)
  )

  expect_identical(quickr:::`append1<-`(1:2, 3L), c(1L, 2L, 3L))
  expect_error(quickr:::`append1<-`(1:2, "x"))

  expect_identical(quickr:::`prepend<-`(1:2, 0L), c(0L, 1L, 2L))
})

test_that("map2 validates lengths and preserves names", {
  expect_error(
    quickr:::map2(1:2, 1:3, `+`),
    ".x and .y must have the same length"
  )

  x <- setNames(1:2, c("a", "b"))
  out <- quickr:::map2(x, 1L, `+`)
  expect_identical(names(out), c("a", "b"))
  expect_identical(out, list(a = 2L, b = 3L))
})

test_that("discard and drop_nulls behave as expected", {
  expect_identical(quickr:::discard(1:5, \(x) x %% 2L == 0L), c(1L, 3L, 5L))

  x <- list(a = 1, b = NULL, c = 3)
  expect_identical(names(quickr:::drop_nulls(x)), c("a", "c"))

  x <- list(a = NULL, b = NULL, c = 3)
  expect_identical(names(quickr:::drop_nulls(x, c("a", "c"))), c("b", "c"))
})

test_that("new_function and str_flatten_args are usable", {
  f <- quickr:::new_function(args = alist(x = ), body = quote(x + 1L))
  expect_identical(f(1L), 2L)

  expect_identical(
    quickr:::str_flatten_args("a", "b", multiline = FALSE),
    "a,b"
  )
  expect_true(
    grepl("\n", quickr:::str_flatten_args("a", "b", "c", multiline = TRUE))
  )
})

test_that("parent.pkg detects namespaces and set_names mutates names", {
  expect_identical(quickr:::parent.pkg(environment(quickr::quick)), "quickr")
  expect_null(quickr:::parent.pkg(new.env(parent = emptyenv())))

  x <- setNames(1:2, c("a", "b"))
  expect_identical(names(quickr:::set_names(x, toupper)), c("A", "B"))
})

test_that("zip_lists zips named lists and errors on mismatches", {
  a <- list(x = 1, y = 2)
  b <- list(x = 3, y = 4)
  expect_identical(
    quickr:::zip_lists(a, b),
    list(x = list(1, 3), y = list(2, 4))
  )

  bad_names <- structure(list(1, 2), names = c("x", "x"))
  expect_error(quickr:::zip_lists(list(bad_names)), "All names must be unique")

  expect_error(
    quickr:::zip_lists(list(x = 1, y = 2), list(x = 1, y = 2, z = 3)),
    "all elements must have the same length"
  )

  expect_error(
    quickr:::zip_lists(list(x = 1, y = 2), list(x = 1, z = 2)),
    "must match"
  )
})

test_that("defer schedules cleanup code on exit", {
  e <- new.env(parent = emptyenv())

  f <- function() {
    quickr:::defer(assign("done", TRUE, envir = e))
    invisible()
  }
  f()
  expect_true(isTRUE(e$done))
})

test_that("block temp allocation helpers handle degenerate dims", {
  var_scalar <- quickr:::Variable("double")
  expect_identical(quickr:::block_tmp_element_count(var_scalar), NA_integer_)

  scope <- quickr:::new_scope(NULL)
  var_one <- quickr:::Variable("double", list(quote(1L)))
  expect_false(quickr:::block_tmp_allocatable(var_one, scope))
})

test_that("iterable helpers cover edge cases", {
  expect_null(quickr:::r2f_iterable_context(NULL))
  expect_null(quickr:::r2f_iterable_context(character()))
  expect_null(quickr:::r2f_iterable_context("seq"))
  expect_null(quickr:::r2f_iterable_context(c("foo", "seq")))
  expect_identical(quickr:::r2f_iterable_context(c("[", "seq")), "[")
  expect_identical(quickr:::r2f_iterable_context(c("for", "seq")), "for")

  var_na <- quickr:::Variable("double", list(NA_integer_))
  expect_identical(quickr:::value_length_expr(var_na), NA_integer_)

  expect_identical(quickr:::seq_like_length_expr(NULL, 1L), NA_integer_)
  expect_identical(quickr:::seq_like_length_expr(1L, 1), 1L)
  expect_error(
    quickr:::seq_like_length_expr(1L, 2L, 0L),
    "invalid '\\(to - from\\)/by'"
  )

  scope <- new.env(parent = emptyenv())
  expect_error(
    quickr:::seq_like_parse(
      "seq",
      list(from = quote(expr = ), to = 5L),
      scope
    ),
    "seq\\(\\) requires both `from` and `to`"
  )
  expect_error(
    quickr:::seq_like_parse(
      "seq",
      list(from = 5L, to = quote(expr = )),
      scope
    ),
    "seq\\(\\) requires both `from` and `to`"
  )
  out <- quickr:::seq_like_parse(
    "seq",
    list(from = 1L, to = 3L, by = quote(expr = )),
    scope
  )
  expect_null(out$by)
  expect_error(
    quickr:::seq_like_parse("banana", list(), scope),
    "unsupported iterable"
  )

  expect_false(quickr:::iterable_is_singleton_one(1L, scope))
  expect_false(quickr:::iterable_is_singleton_one(quote(foo(1L)), scope))
  expect_false(quickr:::iterable_is_singleton_one(quote(seq(1L)), scope))
  expect_false(
    quickr:::iterable_is_singleton_one(quote(seq_along(x)), scope)
  )

  scope2 <- quickr:::new_scope(function() NULL)
  expect_error(
    quickr:::r2f_for_iterable(quote(x), scope2),
    "unsupported iterable in for\\(\\)"
  )
})

test_that("iterable_is_singleton_one handles unknown iterable kinds", {
  scope <- new.env(parent = emptyenv())
  local_mocked_bindings(
    seq_like_parse = function(...) list(kind = "bogus"),
    .package = "quickr"
  )

  expect_false(quickr:::iterable_is_singleton_one(quote(seq_len(1L)), scope))
})

test_that("quickr_r_cmd adds .exe extension on Windows when needed", {
  result <- quickr:::quickr_r_cmd(
    os_type = "windows",
    r_home = function(sub) "/path/to/R",
    file_exists = function(x) FALSE
  )
  expect_equal(result, "/path/to/R.exe")
})

test_that("set_names handles list argument", {
  x <- 1:2
  names(x) <- c("a", "b")
  result <- quickr:::set_names(x, "c", "d")
  expect_equal(names(result), c("c", "d"))
})

test_that("zip_lists reorders elements when names match but different order", {
  a <- list(x = 1, y = 2)
  b <- list(y = 4, x = 3)
  result <- quickr:::zip_lists(a, b)
  expect_equal(result$x, list(1, 3))
  expect_equal(result$y, list(2, 4))
})

test_that("is_scalar returns correct values", {
  expect_true(quickr:::is_scalar(1))
  expect_true(quickr:::is_scalar("a"))
  expect_false(quickr:::is_scalar(1:2))
  expect_false(quickr:::is_scalar(character()))
})

test_that("quickr_ordered_env detects untracked names", {
  env <- quickr:::new_ordered_env()
  # Directly assign without going through [[<-
  base::assign("sneaky", 1, envir = env)
  expect_error(
    suppressWarnings(names(env)),
    "untracked name"
  )
})

test_that("print.quickr_ordered_env outputs bindings", {
  env <- quickr:::new_ordered_env()
  env[["x"]] <- 1
  env[["y"]] <- 2
  out <- capture.output(print(env))
  expect_true(any(grepl("x", out)))
  expect_true(any(grepl("with bindings", out)))
})

test_that("check_assignment_compatible handles NULL value", {
  target <- quickr:::Variable("double", list(1L))
  expect_silent(quickr:::check_assignment_compatible(target, NULL))
})
