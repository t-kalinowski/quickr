quickr_flang_path <- function() {
  flang_new <- Sys.which("flang-new")
  if (nzchar(flang_new)) {
    return(flang_new)
  }
  flang <- Sys.which("flang")
  if (nzchar(flang)) {
    return(flang)
  }
  ""
}

quickr_flang_available <- function() {
  flang <- quickr_flang_path()
  quickr_flang_available_at_path(flang)
}

quickr_flang_available_at_path <- function(flang) {
  stopifnot(is_string(flang))

  if (!nzchar(flang)) {
    return(list(path = "", available = FALSE))
  }
  probe <- tryCatch(
    system2(flang, "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) structure(character(), status = 1L)
  )
  if (!is.null(attr(probe, "status"))) {
    return(list(path = flang, available = FALSE))
  }
  list(path = flang, available = TRUE)
}

quickr_cached_flang_available <- function(
  cache = quickr_compiler_probe_cache
) {
  stopifnot(is.environment(cache))

  flang <- quickr_flang_path()
  cache_key <- paste("flang_available", flang, sep = "\r")
  cached <- get0(cache_key, envir = cache, inherits = FALSE, ifnotfound = NULL)
  if (!is.null(cached)) {
    fresh <- quickr_flang_available_at_path(flang)
    if (!isTRUE(fresh$available)) {
      rm(list = cache_key, envir = cache)
    }
    return(fresh)
  }

  cached <- quickr_flang_available_at_path(flang)
  if (isTRUE(cached$available)) {
    assign(cache_key, cached, envir = cache)
  }

  cached
}

quickr_flang_state <- local({
  state <- new.env(parent = emptyenv())
  state$auto_disabled <- FALSE
  state$fallback_warned <- FALSE
  state
})

quickr_flang_auto_disabled <- function(state = quickr_flang_state) {
  isTRUE(state$auto_disabled)
}

quickr_disable_flang_auto <- function(state = quickr_flang_state) {
  state$auto_disabled <- TRUE
  invisible(state$auto_disabled)
}

quickr_warn_flang_fallback_once <- function(state = quickr_flang_state) {
  if (isTRUE(state$fallback_warned)) {
    return(invisible(TRUE))
  }
  warning(
    paste(
      "flang compilation failed; falling back to gfortran and disabling",
      "automatic flang preference for this session."
    ),
    call. = FALSE
  )
  state$fallback_warned <- TRUE
  invisible(TRUE)
}

quickr_compiler_warning_state <- local({
  state <- new.env(parent = emptyenv())
  state$warned <- FALSE
  state
})

quickr_warn_compiler_failure_once <- function(
  message,
  state = quickr_compiler_warning_state
) {
  if (isTRUE(state$warned)) {
    return(invisible(TRUE))
  }
  warning(message, call. = FALSE)
  state$warned <- TRUE
  invisible(TRUE)
}

quickr_flang_runtime_flags <- local({
  cache <- NULL

  function(flang, sysname = Sys.info()[["sysname"]]) {
    stopifnot(is_string(flang))
    if (!nzchar(flang) || sysname != "Darwin") {
      return(character())
    }
    if (!file.exists(flang)) {
      return(character())
    }
    if (!is.null(cache)) {
      return(cache)
    }

    resolved <- normalizePath(flang, winslash = "/", mustWork = FALSE)

    prefix <- dirname(dirname(resolved))
    libdirs <- unique(
      c(
        file.path(prefix, "lib"),
        file.path(prefix, "lib64"),
        Sys.glob(file.path(prefix, "lib", "clang", "*", "lib", "darwin")),
        Sys.glob(file.path(prefix, "lib64", "clang", "*", "lib", "darwin"))
      )
    )

    runtime <- "libflang_rt.runtime.dylib"
    libdir <- keep(libdirs, function(d) file.exists(file.path(d, runtime)))

    if (!length(libdir)) {
      cache <<- character()
      return(cache)
    }

    cache <<- c(sprintf("-L%s", libdir[[1L]]), "-lflang_rt.runtime")
    cache
  }
})

quickr_fortran_compiler_option <- function(
  opt = getOption("quickr.fortran_compiler")
) {
  if (is.null(opt)) {
    return(NULL)
  }
  if (!is_string(opt)) {
    stop(
      "`options(quickr.fortran_compiler)` must be a single string.",
      call. = FALSE
    )
  }
  opt <- tolower(trimws(opt))
  if (!nzchar(opt) || opt %in% c("auto", "default", "system")) {
    return(NULL)
  }
  if (opt %in% c("flang", "flang-new")) {
    return("flang")
  }
  if (opt %in% c("gfortran", "gnu")) {
    return("gfortran")
  }
  stop(
    "`options(quickr.fortran_compiler)` must be one of ",
    "\"flang\", \"gfortran\", or \"auto\".",
    call. = FALSE
  )
}

quickr_prefer_flang <- function() {
  compiler_opt <- quickr_fortran_compiler_option()
  if (identical(compiler_opt, "flang")) {
    return(TRUE)
  }
  if (identical(compiler_opt, "gfortran")) {
    return(FALSE)
  }
  if (quickr_flang_auto_disabled()) {
    return(FALSE)
  }

  # Best-effort: on macOS, prefer flang if it is available.
  if (Sys.info()[["sysname"]] == "Darwin") {
    info <- quickr_cached_flang_available()
    return(isTRUE(info$available))
  }

  FALSE
}

quickr_default_fortran_makevars_lines <- function(
  config_value = quickr_cached_r_cmd_config_value
) {
  fc <- trimws(config_value("FC"))
  if (!nzchar(fc)) {
    return(character())
  }

  compiler <- strsplit(fc, "\\s+")[[1L]][[1L]]
  compiler <- basename(compiler)
  if (!grepl("^gfortran(?:-[0-9]+)?(?:\\.exe)?$", compiler)) {
    return(character())
  }

  # GCC 12+ uses a very-cheap vectorizer cost model at -O2. That keeps the
  # default build conservative for loops like quickr's generated array kernels.
  # Relaxing the cost model restores the vectorized code path without changing
  # the compiler or generated Fortran source.
  "PKG_FFLAGS += -fvect-cost-model=cheap"
}

quickr_fcompiler_env <- function(
  build_dir,
  write_lines = writeLines,
  sysname = Sys.info()[["sysname"]],
  use_openmp = FALSE,
  link_flags = character(),
  config_value = quickr_cached_r_cmd_config_value
) {
  stopifnot(is.character(build_dir), length(build_dir) == 1L, nzchar(build_dir))

  use_openmp <- isTRUE(use_openmp)
  link_flags <- link_flags[nzchar(link_flags)]
  compiler_opt <- quickr_fortran_compiler_option()
  explicit_request <- identical(compiler_opt, "flang")

  flang <- ""
  flang_runtime <- character()
  use_flang <- quickr_prefer_flang()
  if (use_flang) {
    flang_info <- quickr_cached_flang_available()
    flang <- flang_info$path
    if (!isTRUE(flang_info$available)) {
      if (isTRUE(explicit_request)) {
        stop(
          "quickr was configured to use flang, but flang was not available or could not be executed.\n",
          "Ensure flang is on your PATH and that `flang --version` succeeds, or switch compilers with:\n",
          "  options(quickr.fortran_compiler = \"gfortran\")\n"
        )
      }
      use_flang <- FALSE
      flang <- ""
    }
  }
  if (use_openmp && use_flang && !isTRUE(explicit_request)) {
    use_flang <- FALSE
    flang <- ""
  }
  if (use_flang) {
    flang_runtime <- if (sysname == "Darwin") {
      quickr_flang_runtime_flags(flang = flang, sysname = sysname)
    } else {
      character()
    }
    if (sysname == "Darwin" && !length(flang_runtime)) {
      if (isTRUE(explicit_request)) {
        stop(
          "quickr was configured to use flang (",
          flang,
          ") but could not locate the flang runtime library (libflang_rt.runtime.dylib) to link against.\n",
          "Either reinstall flang so the runtime is available, or switch compilers with:\n",
          "  options(quickr.fortran_compiler = \"gfortran\")\n"
        )
      }
      use_flang <- FALSE
      flang_runtime <- character()
    }
  }

  default_makevars_lines <- if (use_flang) {
    character()
  } else {
    quickr_default_fortran_makevars_lines(config_value = config_value)
  }

  if (!use_flang && !use_openmp && !length(default_makevars_lines)) {
    return(character())
  }

  makevars_path <- file.path(build_dir, "Makevars.quickr")
  makevars_lines <- c(
    if (use_flang) {
      c(
        sprintf("FC=%s", flang),
        sprintf("F77=%s", flang),
        if (length(flang_runtime)) {
          paste(c("FLIBS +=", flang_runtime), collapse = " ")
        }
      )
    },
    default_makevars_lines,
    if (use_openmp) openmp_makevars_lines(),
    if (length(link_flags)) {
      paste("PKG_LIBS +=", paste(link_flags, collapse = " "))
    }
  )
  write_lines(
    makevars_lines,
    makevars_path
  )
  sprintf("R_MAKEVARS_USER=%s", makevars_path)
}
