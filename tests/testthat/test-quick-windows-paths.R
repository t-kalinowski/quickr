skip_on_cran()

test_that("quickr_windows_add_dll_paths is a no-op off Windows", {
  withr::local_envvar(PATH = "path-entry")

  expect_length(
    quickr_windows_add_dll_paths(
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

  res <- quickr_windows_add_dll_paths(
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

  expect_identical(
    tolower(normalizePath(res[[1L]], winslash = "\\", mustWork = FALSE)),
    lib_dir_norm
  )
  expect_true(lib_dir_norm %in% path_norm)
  expect_true(
    bin_sibling_norm %in% path_norm || bin_dir_norm %in% path_norm
  )
  compiler_dir_norm <- tolower(normalizePath(
    compiler_dir,
    winslash = "\\",
    mustWork = FALSE
  ))
  expect_true(compiler_dir_norm %in% path_norm)
})

test_that("quickr_windows_add_dll_paths adds bin next to arch lib dirs", {
  temp <- withr::local_tempdir()
  lib_arch_dir <- file.path(temp, "rtools", "lib", "x64")
  bin_dir <- file.path(temp, "rtools", "bin")
  dir.create(lib_arch_dir, recursive = TRUE)
  dir.create(bin_dir, recursive = TRUE)

  withr::local_envvar(c(
    PATH = temp,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  res <- quickr_windows_add_dll_paths(
    flags = c(paste0("-L", lib_arch_dir)),
    os_type = "windows",
    config_value = function(...) "",
    which = function(cmds) setNames(rep("", length(cmds)), cmds)
  )

  expect_type(res, "character")
  path_entries <- strsplit(Sys.getenv("PATH"), ";", fixed = TRUE)[[1L]]
  path_entries <- path_entries[nzchar(path_entries)]
  path_norm <- tolower(normalizePath(
    path_entries,
    winslash = "\\",
    mustWork = FALSE
  ))
  bin_dir_norm <- tolower(normalizePath(
    bin_dir,
    winslash = "\\",
    mustWork = FALSE
  ))

  expect_true(bin_dir_norm %in% path_norm)
})

test_that("quickr_windows_add_dll_paths adds usr/bin from Rtools link dirs", {
  temp <- withr::local_tempdir()
  lib_arch_dir <- file.path(
    temp,
    "rtools45",
    "x86_64-w64-mingw32.static.posix",
    "lib",
    "x64"
  )
  usr_bin <- file.path(temp, "rtools45", "usr", "bin")
  dir.create(lib_arch_dir, recursive = TRUE)
  dir.create(usr_bin, recursive = TRUE)

  withr::local_envvar(c(
    PATH = temp,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  quickr_windows_add_dll_paths(
    flags = c(paste0("-L", lib_arch_dir)),
    os_type = "windows",
    config_value = function(...) "",
    which = function(cmds) setNames(rep("", length(cmds)), cmds)
  )

  path_entries <- strsplit(Sys.getenv("PATH"), ";", fixed = TRUE)[[1L]]
  path_entries <- path_entries[nzchar(path_entries)]
  path_norm <- tolower(normalizePath(
    path_entries,
    winslash = "\\",
    mustWork = FALSE
  ))
  usr_bin_norm <- tolower(normalizePath(
    usr_bin,
    winslash = "\\",
    mustWork = FALSE
  ))

  expect_true(usr_bin_norm %in% path_norm)
})

test_that("quickr_windows_add_dll_paths writes native Windows PATH entries", {
  temp <- withr::local_tempdir()
  lib_dir <- file.path(temp, "lib")
  dir.create(lib_dir)

  withr::local_envvar(c(
    PATH = temp,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  quickr_windows_add_dll_paths(
    flags = c(paste0("-L", lib_dir)),
    os_type = "windows",
    config_value = function(...) "",
    which = function(cmds) setNames(rep("", length(cmds)), cmds)
  )

  path_entries <- strsplit(Sys.getenv("PATH"), ";", fixed = TRUE)[[1L]]
  path_entries <- path_entries[nzchar(path_entries)]
  lib_dir_norm <- tolower(normalizePath(
    lib_dir,
    winslash = "\\",
    mustWork = FALSE
  ))
  path_norm <- tolower(normalizePath(
    path_entries,
    winslash = "\\",
    mustWork = FALSE
  ))
  lib_entry <- path_entries[path_norm == lib_dir_norm][[1L]]

  expect_true(nzchar(lib_entry))
  if (identical(.Platform$OS.type, "windows")) {
    expect_match(lib_entry, "\\\\")
    expect_false(grepl("/", lib_entry, fixed = TRUE))
  }
})

test_that("quickr_windows_add_dll_paths adds BINPREF directories", {
  temp <- withr::local_tempdir()
  binpref <- file.path(temp, "rtools", "bin")
  dir.create(binpref, recursive = TRUE)

  withr::local_envvar(c(
    PATH = temp,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  res <- quickr_windows_add_dll_paths(
    flags = character(),
    os_type = "windows",
    config_value = function(name) {
      if (identical(name, "BINPREF")) {
        return(paste0(binpref, "/"))
      }
      ""
    },
    which = function(cmds) setNames(rep("", length(cmds)), cmds)
  )

  expect_type(res, "character")
  path_entries <- strsplit(Sys.getenv("PATH"), ";", fixed = TRUE)[[1L]]
  path_entries <- path_entries[nzchar(path_entries)]
  path_norm <- tolower(normalizePath(
    path_entries,
    winslash = "\\",
    mustWork = FALSE
  ))
  binpref_norm <- tolower(normalizePath(
    binpref,
    winslash = "\\",
    mustWork = FALSE
  ))
  expect_true(
    binpref_norm %in%
      tolower(normalizePath(
        res,
        winslash = "\\",
        mustWork = FALSE
      ))
  )
  expect_true(binpref_norm %in% path_norm)
})

test_that("quickr_windows_add_dll_paths adds BINPREF prefix directories", {
  temp <- withr::local_tempdir()
  binpref <- file.path(temp, "rtools", "bin")
  dir.create(binpref, recursive = TRUE)

  withr::local_envvar(c(
    PATH = temp,
    RTOOLS45_HOME = "",
    RTOOLS44_HOME = "",
    RTOOLS43_HOME = "",
    RTOOLS42_HOME = "",
    RTOOLS40_HOME = "",
    RTOOLS_HOME = ""
  ))

  res <- quickr_windows_add_dll_paths(
    flags = character(),
    os_type = "windows",
    config_value = function(name) {
      if (identical(name, "BINPREF")) {
        return(file.path(binpref, "x86_64-w64-mingw32.static.posix-"))
      }
      ""
    },
    which = function(cmds) setNames(rep("", length(cmds)), cmds)
  )

  expect_type(res, "character")
  path_entries <- strsplit(Sys.getenv("PATH"), ";", fixed = TRUE)[[1L]]
  path_entries <- path_entries[nzchar(path_entries)]
  path_norm <- tolower(normalizePath(
    path_entries,
    winslash = "\\",
    mustWork = FALSE
  ))
  binpref_norm <- tolower(normalizePath(
    binpref,
    winslash = "\\",
    mustWork = FALSE
  ))
  expect_true(
    binpref_norm %in%
      tolower(normalizePath(
        res,
        winslash = "\\",
        mustWork = FALSE
      ))
  )
  expect_true(binpref_norm %in% path_norm)
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

  res <- quickr_windows_add_dll_paths(
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
