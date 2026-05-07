skip_on_cran()

test_that("quickr_windows_add_dll_paths is a no-op off Windows", {
  withr::local_envvar(PATH = "path-entry")

  expect_length(
    quickr:::quickr_windows_add_dll_paths(
      flags = character(),
      os_type = "unix"
    ),
    0
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

  expect_type(res, "character")
  path <- Sys.getenv("PATH")
  path_entries <- strsplit(path, ";", fixed = TRUE)[[1L]]
  path_entries <- path_entries[nzchar(path_entries)]
  path_norm <- tolower(normalizePath(
    path_entries,
    winslash = "\\",
    mustWork = FALSE
  ))
  bin_sibling <- file.path(lib_dir, "..", "bin")
  lib_dir_norm <- tolower(normalizePath(
    lib_dir,
    winslash = "\\",
    mustWork = FALSE
  ))
  bin_sibling_norm <- tolower(normalizePath(
    bin_sibling,
    winslash = "\\",
    mustWork = FALSE
  ))
  bin_dir_norm <- tolower(normalizePath(
    bin_dir,
    winslash = "\\",
    mustWork = FALSE
  ))

  expect_true(lib_dir_norm %in% path_norm)
  expect_true(
    bin_sibling_norm %in% path_norm || bin_dir_norm %in% path_norm
  )
  expect_true(
    lib_dir_norm %in%
      tolower(normalizePath(
        res,
        winslash = "\\",
        mustWork = FALSE
      ))
  )
  compiler_dir_norm <- tolower(normalizePath(
    compiler_dir,
    winslash = "\\",
    mustWork = FALSE
  ))
  expect_true(compiler_dir_norm %in% path_norm)
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

  expect_type(res, "character")
  expect_identical(Sys.getenv("PATH"), base_path)
})

test_that("quickr_windows_load_dll_dependencies preloads known runtime DLLs", {
  temp <- withr::local_tempdir()
  lib_dir <- file.path(temp, "lib")
  bin_dir <- file.path(temp, "bin")
  dir.create(lib_dir)
  dir.create(bin_dir)
  file.create(
    file.path(lib_dir, "libgfortran-5.dll"),
    file.path(bin_dir, "libgfortran-5.dll"),
    file.path(bin_dir, "libquadmath-0.dll"),
    file.path(lib_dir, "Rlapack.dll")
  )

  loaded <- character()
  res <- quickr_windows_load_dll_dependencies(
    c(lib_dir, bin_dir),
    os_type = "windows",
    dyn_load = function(path) {
      loaded <<- c(loaded, path)
      structure(list(), class = "DLLInfo")
    }
  )

  expected <- normalizePath(
    c(
      file.path(bin_dir, "libquadmath-0.dll"),
      file.path(lib_dir, "libgfortran-5.dll"),
      file.path(lib_dir, "Rlapack.dll")
    ),
    winslash = "\\",
    mustWork = FALSE
  )
  expect_identical(res, expected)
  expect_identical(loaded, expected)
})
