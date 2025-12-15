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

  function(
    flang,
    sysname = Sys.info()[["sysname"]],
    write_lines = writeLines
  ) {
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

    tmpdir <- tempfile("quickr-flang-probe-")
    dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
    src <- file.path(tmpdir, "probe.f90")
    out <- file.path(tmpdir, "probe.dylib")
    write_lines("subroutine quickr_flang_probe(); end subroutine", src)

    probe <- tryCatch(
      suppressWarnings(system2(
        flang,
        c("-###", "-shared", "-o", out, src),
        stdout = TRUE,
        stderr = TRUE
      )),
      error = function(e) character()
    )
    probe <- paste(probe, collapse = " ")
    dirs <- unique(unlist(regmatches(
      probe,
      gregexpr("\"?-L[^\" ]+\"?", probe, perl = TRUE)
    )))
    dirs <- gsub("^\"?-L", "", dirs)
    dirs <- gsub("\"$", "", dirs)
    libdir <- NULL
    for (d in dirs) {
      if (file.exists(file.path(d, "libflang_rt.runtime.dylib"))) {
        libdir <- d
        break
      }
    }
    if (is.null(libdir)) {
      cache <- character()
      return(cache)
    }

    cache <- c(sprintf("-L%s", libdir), "-lflang_rt.runtime")
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

  flang_runtime <- quickr_flang_runtime_flags(
    flang = flang,
    sysname = sysname,
    write_lines = write_lines
  )

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
