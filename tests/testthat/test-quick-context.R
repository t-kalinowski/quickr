skip_on_cran()

test_that("quick requires explicit names inside packages", {
  pkg_call <- function() {
    quick(function(x) {
      declare(type(x = double(1)))
      x + 1
    })
  }
  environment(pkg_call) <- asNamespace("stats")

  expect_error(pkg_call(), "must provide a unique `name`", fixed = TRUE)
})

test_that("quick returns a closure in package context when named", {
  pkg_call <- function(fn) {
    quick("pkg_fun", fn)
  }
  environment(pkg_call) <- asNamespace("stats")

  fn <- function(x) {
    declare(type(x = double(1)))
    x + 1
  }
  qfn <- pkg_call(fn)
  expect_true(is.function(qfn))
  expect_identical(attr(qfn, "quickr_original", exact = TRUE), fn)
})

test_that("quick activates the collector during pkgload load_code", {
  skip_if_not_installed("pkgload")

  withr::local_envvar(DEVTOOLS_LOAD = "quickr-test")
  quickr:::collector$get_collected()
  withr::defer(quickr:::collector$get_collected())

  local_mocked_bindings(
    dump_collected = function() invisible(NULL),
    .package = "quickr"
  )
  local_mocked_bindings(
    load_code = function(code) code(),
    .package = "pkgload"
  )

  fn <- function(x) {
    declare(type(x = double(1)))
    x + 1
  }
  qfn <- pkgload::load_code(function() quick(fn))

  expect_true(is.function(qfn))
  expect_true(quickr:::collector$is_active())
  expect_identical(attr(qfn, "quickr_original", exact = TRUE), fn)
})

test_that("quick retains the input function before rewriting it", {
  dlls_before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(dlls_before))

  fn <- local({
    default <- 3L
    function(x = default) {
      declare(type(x = integer(1)))
      x + 1L
    }
  })
  attr(fn, "name") <- "original_name"
  attr(fn, "purpose") <- "test metadata"

  qfn <- quick(fn, name = "compiled_name")
  original <- attr(qfn, "quickr_original", exact = TRUE)
  expect_identical(original, fn)
  expect_identical(formals(qfn), formals(fn))
  expect_identical(environment(qfn), environment(fn))
  expect_null(attr(fn, "quickr_original", exact = TRUE))
  expect_identical(qfn(), fn())
  expect_identical(qfn(9L), fn(9L))

  environment(fn)$default <- 8L
  expect_identical(original(), fn())
  expect_identical(qfn(), fn())
})

test_that("the retained function can be saved and compiled again", {
  dlls_before <- loaded_dll_paths()
  withr::defer(cleanup_new_quick_dlls(dlls_before))

  fn <- function(x = 2L) {
    declare(type(x = integer(1)))
    x + 1L
  }
  qfn <- quick("saved_original", fn)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(attr(qfn, "quickr_original", exact = TRUE), path)
  original <- readRDS(path)

  expect_identical(body(original), body(fn))
  expect_identical(formals(original), formals(fn))
  expect_identical(attributes(original), attributes(fn))
  restored <- quick("restored_original", original)
  expect_identical(restored(), fn())
  expect_identical(restored(9L), fn(9L))
})
