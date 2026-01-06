quickr_flang_path <- function(which = Sys.which) {
  flang_new <- which("flang-new")
  if (nzchar(flang_new)) {
    return(flang_new)
  }
  flang <- which("flang")
  if (nzchar(flang)) {
    return(flang)
  }
  ""
}

quickr_flang_available <- function(
  which = Sys.which,
  system2 = base::system2
) {
  flang <- quickr_flang_path(which = which)
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

quickr_prefer_flang <- function(
  sysname = Sys.info()[["sysname"]],
  which = Sys.which,
  system2 = base::system2
) {
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
  if (sysname == "Darwin") {
    info <- quickr_flang_available(which = which, system2 = system2)
    return(isTRUE(info$available))
  }

  FALSE
}

quickr_fcompiler_env <- function(
  build_dir,
  which = Sys.which,
  system2 = base::system2,
  write_lines = writeLines,
  sysname = Sys.info()[["sysname"]],
  use_openmp = FALSE,
  link_flags = character()
) {
  stopifnot(is.character(build_dir), length(build_dir) == 1L, nzchar(build_dir))

  use_openmp <- isTRUE(use_openmp)
  link_flags <- link_flags[nzchar(link_flags)]
  compiler_opt <- quickr_fortran_compiler_option()
  explicit_request <- identical(compiler_opt, "flang")

  flang <- ""
  flang_runtime <- character()
  use_flang <- isTRUE(quickr_prefer_flang(
    sysname = sysname,
    which = which,
    system2 = system2
  ))
  if (use_flang) {
    flang_info <- quickr_flang_available(which = which, system2 = system2)
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

  if (!use_flang && !use_openmp) {
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
