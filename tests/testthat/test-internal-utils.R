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
