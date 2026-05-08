skip_on_cran()

test_that("quickr_cached_r_cmd_config_value retries failed lookups", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  system2_stub <- function(command, args, stdout = "", stderr = "", ...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(structure(" value ", status = 1L))
    }
    "gfortran"
  }

  local_mocked_bindings(system2 = system2_stub, .package = "base")

  expect_identical(quickr_cached_r_cmd_config_value("FC", cache = cache), "")
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "gfortran"
  )
  expect_identical(
    quickr_cached_r_cmd_config_value("FC", cache = cache),
    "gfortran"
  )
  expect_equal(calls, 2L)
})

test_that("quickr_cached_flang_available retries failed probes", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  which_stub <- function(x) if (x == "flang-new") "/tmp/flang-new" else ""
  system2_stub <- function(...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(structure("error", status = 1L))
    }
    "flang version"
  }

  local_mocked_bindings(
    Sys.which = which_stub,
    system2 = system2_stub,
    .package = "base"
  )

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_false(result$available)

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_true(result$available)
  expect_equal(calls, 2L)
})

test_that("quickr_cached_flang_available revalidates successful probes", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  which_stub <- function(x) if (x == "flang-new") "/tmp/flang-new" else ""
  system2_stub <- function(...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return("flang version")
    }
    structure("error", status = 1L)
  }

  local_mocked_bindings(
    Sys.which = which_stub,
    system2 = system2_stub,
    .package = "base"
  )

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_true(result$available)

  result <- quickr_cached_flang_available(cache = cache)
  expect_identical(result$path, "/tmp/flang-new")
  expect_false(result$available)
  expect_equal(calls, 2L)
})
