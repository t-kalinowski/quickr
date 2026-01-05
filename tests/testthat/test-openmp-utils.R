test_that("openmp_makevars_lines uses explicit env flags", {
  withr::local_envvar(c(
    QUICKR_OPENMP_FFLAGS = "-fopenmp",
    QUICKR_OPENMP_LIBS = "-fopenmp"
  ))

  expect_equal(
    quickr:::openmp_makevars_lines(),
    c("PKG_FFLAGS += -fopenmp", "PKG_LIBS += -fopenmp")
  )
})

test_that("openmp_makevars_lines errors when flags are missing", {
  local_mocked_bindings(
    openmp_fflags = function() "",
    .package = "quickr"
  )

  expect_error(
    quickr:::openmp_makevars_lines(),
    "OpenMP was requested but no OpenMP flags",
    class = "quickr_openmp_unavailable"
  )
})

test_that("openmp_makevars_lines errors when linker flags are missing", {
  local_mocked_bindings(
    openmp_fflags = function() "-fopenmp",
    openmp_link_flags = function(...) "",
    .package = "quickr"
  )

  expect_error(
    quickr:::openmp_makevars_lines(),
    "OpenMP was requested but no OpenMP linker flags",
    class = "quickr_openmp_unavailable"
  )
})

test_that("openmp_config_value caches toolchain lookups", {
  cache_env <- environment(quickr:::openmp_config_value)
  old_cache <- cache_env$cached
  old_config <- cache_env$quickr_r_cmd_config_value
  withr::defer(cache_env$cached <- old_cache)
  withr::defer({
    if (is.null(old_config)) {
      rm(quickr_r_cmd_config_value, envir = cache_env)
    } else {
      cache_env$quickr_r_cmd_config_value <- old_config
    }
  })
  cache_env$cached <- NULL

  calls <- 0
  cache_env$quickr_r_cmd_config_value <- function(...) {
    calls <<- calls + 1
    "value"
  }

  expect_equal(quickr:::openmp_config_value("QUICKR_TEST_CACHE"), "value")
  expect_equal(quickr:::openmp_config_value("QUICKR_TEST_CACHE"), "value")
  expect_equal(calls, 1)
})
