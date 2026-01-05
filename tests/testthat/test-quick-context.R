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
  pkg_call <- function() {
    quick("pkg_fun", function(x) {
      declare(type(x = double(1)))
      x + 1
    })
  }
  environment(pkg_call) <- asNamespace("stats")

  qfn <- pkg_call()
  expect_true(is.function(qfn))
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

  qfn <- pkgload::load_code(function() {
    quick(function(x) {
      declare(type(x = double(1)))
      x + 1
    })
  })

  expect_true(is.function(qfn))
  expect_true(quickr:::collector$is_active())
})
