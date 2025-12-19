# Unit tests for compiler selection helpers

test_that("quickr_env_is_true recognizes common truthy values", {
  withr::local_envvar(c(QUICKR_PREFER_FLANG = ""))
  expect_false(quickr:::quickr_env_is_true("QUICKR_PREFER_FLANG"))

  withr::local_envvar(c(QUICKR_PREFER_FLANG = "1"))
  expect_true(quickr:::quickr_env_is_true("QUICKR_PREFER_FLANG"))

  withr::local_envvar(c(QUICKR_PREFER_FLANG = "YeS"))
  expect_true(quickr:::quickr_env_is_true("QUICKR_PREFER_FLANG"))
})

test_that("quickr_flang_path and quickr_prefer_flang are deterministic with stubs", {
  which <- function(x) {
    if (x == "flang-new") {
      "/tmp/flang-new"
    } else if (x == "flang") {
      "/tmp/flang"
    } else {
      ""
    }
  }

  expect_identical(quickr:::quickr_flang_path(which = which), "/tmp/flang-new")

  withr::local_options(
    quickr.prefer_flang = NULL,
    quickr.prefer_flang_force = NULL,
    quickr.prefer_flang_auto = TRUE
  )
  withr::local_envvar(c(QUICKR_PREFER_FLANG = ""))

  expect_true(quickr:::quickr_prefer_flang(sysname = "Darwin", which = which))
  expect_false(quickr:::quickr_prefer_flang(sysname = "Linux", which = which))

  withr::local_options(quickr.prefer_flang = FALSE)
  expect_false(quickr:::quickr_prefer_flang(sysname = "Darwin", which = which))

  withr::local_options(
    quickr.prefer_flang = NULL,
    quickr.prefer_flang_force = TRUE
  )
  expect_true(quickr:::quickr_prefer_flang(sysname = "Linux", which = which))
})

test_that("quickr_fcompiler_env writes Makevars when flang is usable", {
  temp <- withr::local_tempdir()
  prefix <- file.path(temp, "flang")
  dir.create(file.path(prefix, "bin"), recursive = TRUE)
  dir.create(file.path(prefix, "lib"), recursive = TRUE)

  flang <- file.path(prefix, "bin", "flang-new")
  file.create(flang)
  file.create(file.path(prefix, "lib", "libflang_rt.runtime.dylib"))

  which <- function(x) if (x == "flang-new") flang else ""

  cache_env <- environment(quickr:::quickr_flang_runtime_flags)
  old_cache <- cache_env$cache
  cache_env$cache <- NULL
  on.exit(cache_env$cache <- old_cache, add = TRUE)

  build_dir <- file.path(temp, "build")
  dir.create(build_dir)

  env <- quickr:::quickr_fcompiler_env(
    build_dir = build_dir,
    which = which,
    prefer_flang = TRUE,
    prefer_flang_force = TRUE,
    sysname = "Darwin"
  )
  expect_true(startsWith(env, "R_MAKEVARS_USER="))
  expect_true(file.exists(sub("R_MAKEVARS_USER=", "", env, fixed = TRUE)))
})
