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
  write_lines = writeLines
) {
  stopifnot(is.character(build_dir), length(build_dir) == 1L, nzchar(build_dir))

  if (!isTRUE(prefer_flang)) {
    return(character())
  }
  flang <- quickr_flang_path(which = which)
  if (!nzchar(flang)) {
    return(character())
  }
  makevars_path <- file.path(build_dir, "Makevars.quickr")
  write_lines(
    c(
      sprintf("FC=%s", flang),
      sprintf("F77=%s", flang)
    ),
    makevars_path
  )
  sprintf("R_MAKEVARS_USER=%s", makevars_path)
}
