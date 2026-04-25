skip_on_cran()

test_that("quickr_fcompiler_env prefers flang-new when requested", {
  which <- function(cmd) {
    if (identical(cmd, "flang-new")) {
      return("/opt/bin/flang-new")
    }
    ""
  }

  withr::local_options(quickr.fortran_compiler = "flang")

  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  env <- quickr:::quickr_fcompiler_env(
    build_dir,
    system2 = function(...) "",
    which = which,
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
  which <- function(cmd) {
    if (identical(cmd, "flang")) {
      return("/opt/bin/flang")
    }
    ""
  }

  withr::local_options(quickr.fortran_compiler = "flang")

  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  env <- quickr:::quickr_fcompiler_env(
    build_dir,
    system2 = function(...) "",
    which = which,
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
  which <- function(cmd) ""
  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)

  withr::local_options(quickr.fortran_compiler = "gfortran")
  expect_equal(
    quickr:::quickr_fcompiler_env(
      build_dir,
      which = which,
      config_value = function(name) if (identical(name, "FC")) "clang" else ""
    ),
    character()
  )

  withr::local_options(quickr.fortran_compiler = "auto")
  expect_equal(
    quickr:::quickr_fcompiler_env(
      build_dir,
      which = which,
      config_value = function(name) if (identical(name, "FC")) "clang" else ""
    ),
    character()
  )
})

test_that("quickr.fortran_compiler = \"gfortran\" disables auto preference", {
  withr::local_options(quickr.fortran_compiler = "gfortran")

  expect_false(quickr_prefer_flang())
})
