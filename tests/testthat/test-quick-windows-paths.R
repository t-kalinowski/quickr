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

test_that("quickr_windows_add_dll_paths returns post-update PATH order", {
  temp <- withr::local_tempdir()
  lib_dir <- file.path(temp, "lib")
  config_dir <- file.path(temp, "config")
  dir.create(lib_dir)
  dir.create(config_dir)

  base_path <- paste(c(config_dir, lib_dir), collapse = ";")
  withr::local_envvar(c(
    PATH = base_path,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  res <- quickr_windows_add_dll_paths(
    flags = c(paste0("-L", lib_dir)),
    os_type = "windows",
    config_value = function(name) {
      if (identical(name, "FC")) {
        return(file.path(config_dir, "gfortran"))
      }
      ""
    },
    which = function(cmds) setNames(rep("", length(cmds)), cmds)
  )

  path_entries <- strsplit(Sys.getenv("PATH"), ";", fixed = TRUE)[[1L]]
  path_entries <- path_entries[nzchar(path_entries)]
  res_norm <- tolower(normalizePath(res, winslash = "\\", mustWork = FALSE))
  path_norm <- tolower(normalizePath(
    path_entries,
    winslash = "\\",
    mustWork = FALSE
  ))
  config_norm <- tolower(normalizePath(
    config_dir,
    winslash = "\\",
    mustWork = FALSE
  ))
  lib_norm <- tolower(normalizePath(
    lib_dir,
    winslash = "\\",
    mustWork = FALSE
  ))

  expect_lt(match(config_norm, res_norm), match(lib_norm, res_norm))
  expect_lt(match(config_norm, path_norm), match(lib_norm, path_norm))
})

test_that("quickr_windows_add_dll_paths excludes unrelated PATH directories", {
  temp <- withr::local_tempdir()
  lib_dir <- file.path(temp, "lib")
  unrelated_dir <- file.path(temp, "unrelated")
  dir.create(lib_dir)
  dir.create(unrelated_dir)
  file.create(file.path(unrelated_dir, "libopenblas-bad.dll"))

  withr::local_envvar(c(
    PATH = paste(c(unrelated_dir, lib_dir), collapse = ";"),
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  res <- quickr_windows_add_dll_paths(
    flags = c(paste0("-L", lib_dir)),
    os_type = "windows",
    config_value = function(...) "",
    which = function(cmds) setNames(rep("", length(cmds)), cmds)
  )

  res_norm <- tolower(normalizePath(res, winslash = "\\", mustWork = FALSE))
  unrelated_norm <- tolower(normalizePath(
    unrelated_dir,
    winslash = "\\",
    mustWork = FALSE
  ))

  expect_false(unrelated_norm %in% res_norm)
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
