skip_on_cran()

test_that("quickr_fcompiler_env prefers flang-new when requested", {
  which_stub <- function(cmd) {
    if (identical(cmd, "flang-new")) {
      return("/opt/bin/flang-new")
    }
    ""
  }
  local_mocked_bindings(
    Sys.which = which_stub,
    system2 = function(...) "",
    .package = "base"
  )

  withr::local_options(quickr.fortran_compiler = "flang")

  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  env <- quickr:::quickr_fcompiler_env(
    build_dir,
    sysname = "Linux"
  )

  expect_true(startsWith(env, "R_MAKEVARS_USER="))
  makevars_path <- sub("^R_MAKEVARS_USER=", "", env)
  expect_true(file.exists(makevars_path))
  expect_equal(
    readLines(makevars_path),
    c("FC=/opt/bin/flang-new", "F77=/opt/bin/flang-new")
  )
})

test_that("quickr_fcompiler_env falls back to flang when flang-new missing", {
  which_stub <- function(cmd) {
    if (identical(cmd, "flang")) {
      return("/opt/bin/flang")
    }
    ""
  }
  local_mocked_bindings(
    Sys.which = which_stub,
    system2 = function(...) "",
    .package = "base"
  )

  withr::local_options(quickr.fortran_compiler = "flang")

  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  env <- quickr:::quickr_fcompiler_env(
    build_dir,
    sysname = "Linux"
  )

  expect_true(startsWith(env, "R_MAKEVARS_USER="))
  makevars_path <- sub("^R_MAKEVARS_USER=", "", env)
  expect_true(file.exists(makevars_path))
  expect_equal(
    readLines(makevars_path),
    c("FC=/opt/bin/flang", "F77=/opt/bin/flang")
  )
})

test_that("quickr_fcompiler_env returns empty when disabled or unavailable", {
  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  local_mocked_bindings(
    quickr_prefer_flang = function(...) FALSE,
    .package = "quickr"
  )

  withr::local_options(quickr.fortran_compiler = "gfortran")
  expect_equal(
    quickr:::quickr_fcompiler_env(
      build_dir,
      config_value = function(name) if (identical(name, "FC")) "clang" else ""
    ),
    character()
  )

  withr::local_options(quickr.fortran_compiler = "auto")
  expect_equal(
    quickr:::quickr_fcompiler_env(
      build_dir,
      config_value = function(name) if (identical(name, "FC")) "clang" else ""
    ),
    character()
  )
})

test_that("quickr_fcompiler_env uses caller-provided sysname for flang auto preference", {
  temp <- withr::local_tempdir()
  prefix <- file.path(temp, "flang")
  dir.create(file.path(prefix, "bin"), recursive = TRUE)
  dir.create(file.path(prefix, "lib"), recursive = TRUE)
  flang <- file.path(prefix, "bin", "flang-new")
  file.create(flang)
  file.create(file.path(prefix, "lib", "libflang_rt.runtime.dylib"))

  cache_env <- environment(quickr:::quickr_flang_runtime_flags)
  old_cache <- cache_env$cache
  cache_env$cache <- NULL
  on.exit(cache_env$cache <- old_cache, add = TRUE)

  local_mocked_bindings(
    Sys.which = function(cmd) {
      if (identical(cmd, "flang-new")) {
        return(flang)
      }
      ""
    },
    system2 = function(...) "",
    .package = "base"
  )

  withr::local_options(quickr.fortran_compiler = "auto")

  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  env <- quickr:::quickr_fcompiler_env(
    build_dir,
    sysname = "Darwin"
  )

  expect_true(startsWith(env, "R_MAKEVARS_USER="))
  makevars_path <- sub("^R_MAKEVARS_USER=", "", env)
  expect_true(file.exists(makevars_path))
  flang_lib <- file.path(
    dirname(dirname(normalizePath(
      flang,
      winslash = "/",
      mustWork = FALSE
    ))),
    "lib"
  )
  expect_equal(
    readLines(makevars_path),
    c(
      sprintf("FC=%s", flang),
      sprintf("F77=%s", flang),
      paste(
        "FLIBS +=",
        sprintf("-L%s", flang_lib),
        "-lflang_rt.runtime"
      )
    )
  )
})

test_that("quickr.fortran_compiler = \"gfortran\" disables auto preference", {
  withr::local_options(quickr.fortran_compiler = "gfortran")

  expect_false(quickr_prefer_flang())
})
