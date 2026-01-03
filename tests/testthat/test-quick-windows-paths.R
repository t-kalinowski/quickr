test_that("quickr_windows_add_dll_paths is a no-op off Windows", {
  withr::local_envvar(PATH = "path-entry")

  expect_false(
    isTRUE(quickr:::quickr_windows_add_dll_paths(
      flags = character(),
      os_type = "unix"
    ))
  )
  expect_identical(Sys.getenv("PATH"), "path-entry")
})

test_that("quickr_windows_add_dll_paths adds missing directories on Windows", {
  temp <- withr::local_tempdir()
  lib_dir <- file.path(temp, "lib")
  bin_dir <- file.path(temp, "bin")
  compiler_dir <- file.path(temp, "compiler")
  dir.create(lib_dir)
  dir.create(bin_dir)
  dir.create(compiler_dir)

  withr::local_envvar(c(
    PATH = temp,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  res <- quickr:::quickr_windows_add_dll_paths(
    flags = c(paste0("-L", lib_dir)),
    os_type = "windows",
    config_value = function(...) "",
    which = function(cmds) setNames(file.path(compiler_dir, cmds), cmds)
  )

  expect_true(isTRUE(res))
  path <- Sys.getenv("PATH")
  bin_sibling <- file.path(lib_dir, "..", "bin")
  expect_true(grepl(lib_dir, path, fixed = TRUE))
  expect_true(
    grepl(bin_sibling, path, fixed = TRUE) ||
      grepl(bin_dir, path, fixed = TRUE)
  )
  expect_true(grepl(compiler_dir, path, fixed = TRUE))
})

test_that("quickr_windows_add_dll_paths leaves PATH unchanged when complete", {
  temp <- withr::local_tempdir()
  lib_dir <- file.path(temp, "lib")
  bin_dir <- file.path(temp, "bin")
  compiler_dir <- file.path(temp, "compiler")
  dir.create(lib_dir)
  dir.create(bin_dir)
  dir.create(compiler_dir)

  base_path <- paste(
    c(lib_dir, bin_dir, R.home("bin"), compiler_dir),
    collapse = ";"
  )
  withr::local_envvar(c(
    PATH = base_path,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  res <- quickr:::quickr_windows_add_dll_paths(
    flags = c(paste0("-L", lib_dir)),
    os_type = "windows",
    config_value = function(...) "",
    which = function(cmds) setNames(file.path(compiler_dir, cmds), cmds)
  )

  expect_false(isTRUE(res))
  expect_identical(Sys.getenv("PATH"), base_path)
})
