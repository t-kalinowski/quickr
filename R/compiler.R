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

quickr_env_is_true <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (!nzchar(val)) {
    return(FALSE)
  }
  tolower(val) %in% c("1", "true", "t", "yes", "y", "on")
}

quickr_prefer_flang <- function(
  sysname = Sys.info()[["sysname"]],
  which = Sys.which
) {
  opt <- getOption("quickr.prefer_flang")
  if (isFALSE(opt)) {
    return(FALSE)
  }
  if (quickr_env_is_true("QUICKR_PREFER_FLANG")) {
    return(TRUE)
  }
  if (isTRUE(getOption("quickr.prefer_flang_force"))) {
    return(TRUE)
  }
  if (interactive() && isTRUE(opt)) {
    return(TRUE)
  }

  # Best-effort: on macOS, prefer flang if it is available.
  if (
    isTRUE(getOption("quickr.prefer_flang_auto", TRUE)) && sysname == "Darwin"
  ) {
    return(nzchar(quickr_flang_path(which = which)))
  }

  FALSE
}

quickr_fcompiler_env <- function(
  build_dir,
  which = Sys.which,
  prefer_flang = quickr_prefer_flang(which = which),
  prefer_flang_force = isTRUE(getOption("quickr.prefer_flang_force")) ||
    quickr_env_is_true("QUICKR_PREFER_FLANG"),
  write_lines = writeLines,
  sysname = Sys.info()[["sysname"]]
) {
  stopifnot(is.character(build_dir), length(build_dir) == 1L, nzchar(build_dir))

  if (!isTRUE(prefer_flang)) {
    return(character())
  }
  flang <- quickr_flang_path(which = which)
  if (!nzchar(flang)) {
    return(character())
  }

  flang_runtime <- if (sysname == "Darwin") {
    quickr_flang_runtime_flags(flang = flang, sysname = sysname)
  } else {
    character()
  }
  if (sysname == "Darwin" && !length(flang_runtime)) {
    if (isTRUE(prefer_flang_force)) {
      stop(
        "quickr was configured to use flang (",
        flang,
        ") but could not locate the flang runtime library (libflang_rt.runtime.dylib) to link against.\n",
        "Either reinstall flang so the runtime is available, or disable flang selection with:\n",
        "  options(quickr.prefer_flang = FALSE)\n",
        "or:\n",
        "  Sys.setenv(QUICKR_PREFER_FLANG = 0)\n"
      )
    }
    return(character())
  }

  makevars_path <- file.path(build_dir, "Makevars.quickr")
  write_lines(
    c(
      sprintf("FC=%s", flang),
      sprintf("F77=%s", flang),
      if (length(flang_runtime)) {
        paste(c("FLIBS +=", flang_runtime), collapse = " ")
      }
    ),
    makevars_path
  )
  sprintf("R_MAKEVARS_USER=%s", makevars_path)
}
