test_that("quickr_fcompiler_env prefers flang-new when requested", {
  which <- function(cmd) {
    if (identical(cmd, "flang-new")) {
      return("/opt/bin/flang-new")
    }
    ""
  }

  old_opts <- options(
    quickr.prefer_flang_force = NULL,
    quickr.prefer_flang_auto = FALSE,
    quickr.prefer_flang = NULL
  )
  on.exit(options(old_opts), add = TRUE)

  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  env <- quickr:::quickr_fcompiler_env(
    build_dir,
    prefer_flang = TRUE,
    which = which
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

  old_opts <- options(
    quickr.prefer_flang_force = NULL,
    quickr.prefer_flang_auto = FALSE,
    quickr.prefer_flang = NULL
  )
  on.exit(options(old_opts), add = TRUE)

  build_dir <- tempfile("quickr-build-")
  dir.create(build_dir)
  env <- quickr:::quickr_fcompiler_env(
    build_dir,
    prefer_flang = TRUE,
    which = which
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
  expect_equal(
    quickr:::quickr_fcompiler_env(
      build_dir,
      prefer_flang = FALSE,
      which = which
    ),
    character()
  )
  expect_equal(
    quickr:::quickr_fcompiler_env(
      build_dir,
      prefer_flang = TRUE,
      which = which
    ),
    character()
  )
})

test_that("quickr_prefer_flang defaults to TRUE on macOS when flang exists", {
  which <- function(cmd) {
    if (identical(cmd, "flang-new")) {
      return("/opt/bin/flang-new")
    }
    ""
  }

  old_opts <- options(
    quickr.prefer_flang_force = NULL,
    quickr.prefer_flang_auto = TRUE,
    quickr.prefer_flang = NULL
  )
  on.exit(options(old_opts), add = TRUE)

  old_env <- Sys.getenv("QUICKR_PREFER_FLANG", unset = NA_character_)
  on.exit(
    {
      if (is.na(old_env)) {
        Sys.unsetenv("QUICKR_PREFER_FLANG")
      } else {
        Sys.setenv(QUICKR_PREFER_FLANG = old_env)
      }
    },
    add = TRUE
  )
  Sys.unsetenv("QUICKR_PREFER_FLANG")

  expect_true(quickr:::quickr_prefer_flang(sysname = "Darwin", which = which))
  expect_false(quickr:::quickr_prefer_flang(sysname = "Linux", which = which))
})

test_that("quickr.prefer_flang = FALSE disables auto preference", {
  which <- function(cmd) {
    if (identical(cmd, "flang-new")) {
      return("/opt/bin/flang-new")
    }
    ""
  }

  old_opts <- options(
    quickr.prefer_flang_force = NULL,
    quickr.prefer_flang_auto = TRUE,
    quickr.prefer_flang = FALSE
  )
  on.exit(options(old_opts), add = TRUE)

  old_env <- Sys.getenv("QUICKR_PREFER_FLANG", unset = NA_character_)
  on.exit(
    {
      if (is.na(old_env)) {
        Sys.unsetenv("QUICKR_PREFER_FLANG")
      } else {
        Sys.setenv(QUICKR_PREFER_FLANG = old_env)
      }
    },
    add = TRUE
  )
  Sys.unsetenv("QUICKR_PREFER_FLANG")

  expect_false(quickr:::quickr_prefer_flang(sysname = "Darwin", which = which))
})
