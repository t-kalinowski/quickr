# Unit tests for compiler selection helpers

test_that("quickr_r_cmd_config_value captures only stdout", {
  expect_identical(
    deparse(formals(quickr:::quickr_r_cmd_config_value)$system2),
    "base::system2"
  )

  observed_stdout <- NULL
  observed_stderr <- NULL
  system2_stub <- function(
    command,
    args,
    stdout = "",
    stderr = "",
    ...
  ) {
    observed_stdout <<- stdout
    observed_stderr <<- stderr
    " value "
  }

  expect_identical(
    quickr:::quickr_r_cmd_config_value(
      "CC",
      r_cmd = "R",
      system2 = system2_stub
    ),
    "value"
  )
  expect_identical(observed_stdout, TRUE)
  expect_identical(observed_stderr, FALSE)
})

test_that("quickr_r_cmd_config_value returns empty on command failure", {
  system2_stub <- function(command, args, stdout = "", stderr = "", ...) {
    structure(" value ", status = 1L)
  }

  expect_identical(
    quickr:::quickr_r_cmd_config_value(
      "CC",
      r_cmd = "R",
      system2 = system2_stub
    ),
    ""
  )
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
  system2_stub <- function(command, args, stdout = TRUE, stderr = TRUE, ...) {
    "flang version"
  }

  expect_identical(quickr:::quickr_flang_path(which = which), "/tmp/flang-new")

  withr::local_options(quickr.fortran_compiler = "auto")

  expect_true(quickr:::quickr_prefer_flang(
    sysname = "Darwin",
    which = which,
    system2 = system2_stub
  ))
  expect_false(quickr:::quickr_prefer_flang(sysname = "Linux", which = which))
})

test_that("quickr_prefer_flang respects quickr.fortran_compiler", {
  withr::local_options(quickr.fortran_compiler = "flang")
  expect_true(quickr:::quickr_prefer_flang(sysname = "Linux"))

  withr::local_options(quickr.fortran_compiler = "gfortran")
  expect_false(quickr:::quickr_prefer_flang(sysname = "Darwin"))
})

test_that("quickr_fortran_compiler_option validates values", {
  withr::local_options(quickr.fortran_compiler = "auto")
  expect_null(quickr:::quickr_fortran_compiler_option())

  withr::local_options(quickr.fortran_compiler = "flang")
  expect_identical(quickr:::quickr_fortran_compiler_option(), "flang")

  withr::local_options(quickr.fortran_compiler = "gfortran")
  expect_identical(quickr:::quickr_fortran_compiler_option(), "gfortran")

  withr::local_options(quickr.fortran_compiler = "nope")
  expect_error(
    quickr:::quickr_fortran_compiler_option(),
    "options(quickr.fortran_compiler)",
    fixed = TRUE
  )
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

  withr::local_options(quickr.fortran_compiler = "flang")
  env <- quickr:::quickr_fcompiler_env(
    build_dir = build_dir,
    which = which,
    system2 = function(...) "",
    sysname = "Darwin"
  )
  expect_true(startsWith(env, "R_MAKEVARS_USER="))
  expect_true(file.exists(sub("R_MAKEVARS_USER=", "", env, fixed = TRUE)))
})

test_that("quickr_fcompiler_env errors when flang is explicitly requested but unavailable", {
  build_dir <- withr::local_tempdir()

  withr::local_options(quickr.fortran_compiler = "flang")
  expect_error(
    quickr:::quickr_fcompiler_env(
      build_dir = build_dir,
      which = function(cmd) "",
      system2 = function(...) structure("", status = 1L)
    ),
    "configured to use flang",
    fixed = TRUE
  )
})

test_that("quickr_flang_available returns unavailable when system2 fails", {
  which_stub <- function(x) if (x == "flang-new") "/tmp/flang-new" else ""
  system2_fail <- function(...) structure("error", status = 1L)

  result <- quickr:::quickr_flang_available(
    which = which_stub,
    system2 = system2_fail
  )
  expect_identical(result$path, "/tmp/flang-new")
  expect_false(result$available)
})

test_that("quickr_disable_flang_auto sets auto_disabled flag", {
  state <- new.env(parent = emptyenv())
  state$auto_disabled <- FALSE
  state$fallback_warned <- FALSE

  result <- quickr:::quickr_disable_flang_auto(state = state)
  expect_true(result)
  expect_true(state$auto_disabled)
})

test_that("quickr_warn_flang_fallback_once warns only once", {
  state <- new.env(parent = emptyenv())
  state$fallback_warned <- FALSE

  expect_warning(
    quickr:::quickr_warn_flang_fallback_once(state = state),
    "flang compilation failed"
  )
  expect_true(state$fallback_warned)

  # Second call should not warn
  expect_silent(quickr:::quickr_warn_flang_fallback_once(state = state))
})

test_that("quickr_warn_compiler_failure_once warns only once", {
  state <- new.env(parent = emptyenv())
  state$warned <- FALSE

  expect_warning(
    quickr:::quickr_warn_compiler_failure_once("test error", state = state),
    "test error"
  )
  expect_true(state$warned)

  # Second call should not warn
  expect_silent(quickr:::quickr_warn_compiler_failure_once(
    "test error 2",
    state = state
  ))
})

test_that("quickr_flang_runtime_flags returns empty for non-Darwin", {
  result <- quickr:::quickr_flang_runtime_flags(
    "/usr/bin/flang",
    sysname = "Linux"
  )
  expect_identical(result, character())
})

test_that("quickr_flang_runtime_flags returns empty for empty flang path", {
  result <- quickr:::quickr_flang_runtime_flags("", sysname = "Darwin")
  expect_identical(result, character())
})

test_that("quickr_flang_runtime_flags returns empty when flang does not exist", {
  # Clear cache
  cache_env <- environment(quickr:::quickr_flang_runtime_flags)
  old_cache <- cache_env$cache
  cache_env$cache <- NULL
  on.exit(cache_env$cache <- old_cache, add = TRUE)

  result <- quickr:::quickr_flang_runtime_flags(
    "/nonexistent/path/to/flang",
    sysname = "Darwin"
  )
  expect_identical(result, character())
})

test_that("quickr_flang_runtime_flags returns empty when runtime lib not found", {
  temp <- withr::local_tempdir()
  flang <- file.path(temp, "bin", "flang-new")
  dir.create(dirname(flang), recursive = TRUE)
  file.create(flang)
  # Create lib dir but no runtime lib
  dir.create(file.path(temp, "lib"))

  # Clear cache
  cache_env <- environment(quickr:::quickr_flang_runtime_flags)
  old_cache <- cache_env$cache
  cache_env$cache <- NULL
  on.exit(cache_env$cache <- old_cache, add = TRUE)

  result <- quickr:::quickr_flang_runtime_flags(flang, sysname = "Darwin")
  expect_identical(result, character())
})

test_that("quickr_fortran_compiler_option errors for non-string", {
  withr::local_options(quickr.fortran_compiler = 123)
  expect_error(
    quickr:::quickr_fortran_compiler_option(),
    "must be a single string"
  )
})

test_that("quickr_prefer_flang returns FALSE when flang_auto_disabled", {
  state <- new.env(parent = emptyenv())
  state$auto_disabled <- TRUE
  state$fallback_warned <- FALSE

  withr::local_options(quickr.fortran_compiler = "auto")

  # Inject the state
  local_mocked_bindings(
    quickr_flang_auto_disabled = function(...) TRUE,
    .package = "quickr"
  )

  expect_false(quickr:::quickr_prefer_flang(sysname = "Darwin"))
})

test_that("quickr_fcompiler_env handles flang unavailable for non-explicit request", {
  build_dir <- withr::local_tempdir()

  withr::local_options(quickr.fortran_compiler = "auto")

  # Mock flang as preferred but unavailable
  local_mocked_bindings(
    quickr_prefer_flang = function(...) TRUE,
    quickr_flang_available = function(...) {
      list(path = "/tmp/flang", available = FALSE)
    },
    .package = "quickr"
  )

  # Should return character() since not explicit and flang unavailable
  result <- quickr:::quickr_fcompiler_env(
    build_dir = build_dir,
    which = function(x) "",
    system2 = function(...) "",
    sysname = "Darwin"
  )
  expect_identical(result, character())
})

test_that("quickr_fcompiler_env errors when flang runtime not found on Darwin explicit", {
  temp <- withr::local_tempdir()
  flang <- file.path(temp, "bin", "flang-new")
  dir.create(dirname(flang), recursive = TRUE)
  file.create(flang)

  # Clear cache
  cache_env <- environment(quickr:::quickr_flang_runtime_flags)
  old_cache <- cache_env$cache
  cache_env$cache <- NULL
  on.exit(cache_env$cache <- old_cache, add = TRUE)

  build_dir <- file.path(temp, "build")
  dir.create(build_dir)

  withr::local_options(quickr.fortran_compiler = "flang")

  expect_error(
    quickr:::quickr_fcompiler_env(
      build_dir = build_dir,
      which = function(x) if (x == "flang-new") flang else "",
      system2 = function(...) "",
      sysname = "Darwin"
    ),
    "could not locate the flang runtime library"
  )
})

test_that("quickr_fcompiler_env falls back when flang runtime not found non-explicit", {
  temp <- withr::local_tempdir()
  flang <- file.path(temp, "bin", "flang-new")
  dir.create(dirname(flang), recursive = TRUE)
  file.create(flang)
  dir.create(file.path(temp, "lib"))

  # Clear cache
  cache_env <- environment(quickr:::quickr_flang_runtime_flags)
  old_cache <- cache_env$cache
  cache_env$cache <- NULL
  on.exit(cache_env$cache <- old_cache, add = TRUE)

  build_dir <- file.path(temp, "build")
  dir.create(build_dir)

  withr::local_options(quickr.fortran_compiler = "auto")

  result <- quickr:::quickr_fcompiler_env(
    build_dir = build_dir,
    which = function(x) if (x == "flang-new") flang else "",
    system2 = function(...) "",
    sysname = "Darwin"
  )
  # Should fall back to character() since runtime not found and not explicit
  expect_identical(result, character())
})

test_that("compile cleans existing build directories and reports failures", {
  fsub <- r2f(function(x) {
    declare(type(x = double(1)))
    x + 1
  })

  build_dir <- withr::local_tempdir()
  file.create(file.path(build_dir, "stale.txt"))

  calls <- 0L
  system2_stub <- function(
    command,
    args,
    stdout = TRUE,
    stderr = TRUE,
    env = character(),
    ...
  ) {
    if (
      length(args) >= 3L &&
        identical(args[[1L]], "CMD") &&
        identical(args[[2L]], "config")
    ) {
      return("")
    }
    calls <<- calls + 1L
    if (calls == 1L) {
      return(structure("flang fail", status = 1))
    }
    structure("fallback fail", status = 2)
  }

  local_mocked_bindings(
    system2 = system2_stub,
    .package = "base"
  )
  local_mocked_bindings(
    quickr_fcompiler_env = function(...) "ENV=1",
    .package = "quickr"
  )

  expect_error(
    suppressWarnings(quickr:::compile(fsub, build_dir = build_dir)),
    "Compilation Error",
    fixed = TRUE
  )
  expect_true(dir.exists(build_dir))
  expect_false(file.exists(file.path(build_dir, "stale.txt")))
})
