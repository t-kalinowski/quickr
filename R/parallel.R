# Parallel/OpenMP helpers for quickr declarations and codegen.

get_pending_parallel <- function(scope) {
  if (is.null(scope) || !inherits(scope, "quickr_scope")) {
    return(NULL)
  }
  scope@pending_parallel
}

has_pending_parallel <- function(scope) !is.null(get_pending_parallel(scope))

set_pending_parallel <- function(scope, decl) {
  stopifnot(inherits(scope, "quickr_scope"), is.list(decl))
  scope@pending_parallel <- decl
  invisible(scope)
}

take_pending_parallel <- function(scope) {
  if (is.null(scope) || !inherits(scope, "quickr_scope")) {
    return(NULL)
  }
  decl <- scope@pending_parallel
  scope@pending_parallel <- NULL
  decl
}

mark_openmp_used <- function(scope) {
  stopifnot(inherits(scope, "quickr_scope"))
  root <- scope_root(scope)
  attr(root, "uses_openmp") <- TRUE
  invisible(root)
}

is_parallel_decl_call <- function(e) {
  is.call(e) &&
    is.symbol(e[[1L]]) &&
    as.character(e[[1L]]) %in% c("parallel", "omp")
}

parse_parallel_decl <- function(e) {
  stopifnot(is_parallel_decl_call(e))
  args <- as.list(e)[-1L]
  if (length(args)) {
    stop(
      as.character(e[[1L]]),
      "() does not accept arguments yet.",
      call. = FALSE
    )
  }
  list(backend = "omp", source = as.character(e[[1L]]))
}

unwrap_parens <- function(x) {
  while (is_call(x, quote(`(`)) && length(x) == 2L) {
    x <- x[[2L]]
  }
  x
}

is_parallel_target_stmt <- function(stmt) {
  if (!is.call(stmt)) {
    return(FALSE)
  }
  if (identical(stmt[[1L]], quote(`for`))) {
    return(TRUE)
  }
  if (
    identical(stmt[[1L]], quote(`<-`)) ||
      identical(stmt[[1L]], quote(`=`))
  ) {
    if (length(stmt) < 3L) {
      return(FALSE)
    }
    rhs <- unwrap_parens(stmt[[3L]])
    return(is_sapply_call(rhs))
  }
  FALSE
}

check_pending_parallel_target <- function(stmt, scope) {
  if (!has_pending_parallel(scope)) {
    return()
  }
  if (is_call(stmt, quote(declare))) {
    return()
  }
  if (is_parallel_target_stmt(stmt)) {
    return()
  }
  stop(
    "parallel()/omp() must be followed by a for-loop or sapply() assignment.",
    call. = FALSE
  )
}

check_pending_parallel_consumed <- function(scope) {
  if (!has_pending_parallel(scope)) {
    return()
  }
  stop(
    "parallel()/omp() must be followed by a for-loop or sapply() assignment.",
    call. = FALSE
  )
}

openmp_parallel_do <- function(private = NULL) {
  if (is.null(private) || !length(private)) {
    return("!$omp parallel do")
  }
  private <- unique(as.character(private))
  glue("!$omp parallel do private({str_flatten_commas(private)})")
}

openmp_parallel_end <- function() "!$omp end parallel do"

openmp_directives <- function(parallel, private = NULL) {
  if (is.null(parallel)) {
    return(list(prefix = NULL, suffix = NULL))
  }
  if (!identical(parallel$backend, "omp")) {
    stop("unsupported parallel backend: ", parallel$backend, call. = FALSE)
  }
  list(
    prefix = openmp_parallel_do(private = private),
    suffix = openmp_parallel_end()
  )
}

openmp_config_value <- local({
  cached <- NULL

  function(name) {
    if (is.null(cached)) {
      cached <<- list()
    }
    cached_value <- cached[[name]]
    if (!is.null(cached_value)) {
      return(cached_value)
    }

    r_cmd <- R.home("bin/R")
    if (identical(.Platform$OS.type, "windows") && !file.exists(r_cmd)) {
      r_cmd <- paste0(r_cmd, ".exe")
    }
    out <- tryCatch(
      suppressWarnings(system2(
        r_cmd,
        c("CMD", "config", name),
        stdout = TRUE,
        stderr = TRUE
      )),
      error = function(e) character()
    )
    status <- attr(out, "status")
    if (!is.null(status)) {
      cached[[name]] <<- ""
      return("")
    }
    value <- trimws(paste(out, collapse = " "))
    if (!nzchar(value) || grepl("^ERROR:", value)) {
      cached[[name]] <<- ""
      return("")
    }
    cached[[name]] <<- value
    value
  }
})

openmp_fflags <- function() {
  env_flags <- trimws(Sys.getenv("QUICKR_OPENMP_FFLAGS", ""))
  if (nzchar(env_flags)) {
    return(env_flags)
  }

  config_flags <- openmp_config_value("SHLIB_OPENMP_FFLAGS")
  if (nzchar(config_flags)) {
    return(config_flags)
  }

  fc <- openmp_config_value("FC")
  if (grepl("(gfortran|flang)", fc, ignore.case = TRUE)) {
    return("-fopenmp")
  }

  ""
}

openmp_link_flags <- function(fflags = openmp_fflags()) {
  env_flags <- trimws(Sys.getenv("QUICKR_OPENMP_LIBS", ""))
  if (nzchar(env_flags)) {
    return(env_flags)
  }

  config_flags <- openmp_config_value("SHLIB_OPENMP_CFLAGS")
  if (nzchar(config_flags)) {
    return(config_flags)
  }

  cc <- openmp_config_value("CC")
  if (nzchar(fflags) && !grepl("clang", cc, ignore.case = TRUE)) {
    return(fflags)
  }

  fc <- openmp_config_value("FC")
  if (grepl("gfortran", fc, ignore.case = TRUE)) {
    compiler <- strsplit(fc, "\\s+")[[1L]][[1L]]
    libname <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
      "libgomp.dylib"
    } else {
      paste0("libgomp", .Platform$dynlib.ext)
    }
    path <- tryCatch(
      suppressWarnings(system2(
        compiler,
        paste0("-print-file-name=", libname),
        stdout = TRUE,
        stderr = TRUE
      )),
      error = function(e) character()
    )
    status <- attr(path, "status")
    if (!is.null(status)) {
      return("")
    }
    path <- trimws(paste(path, collapse = " "))
    if (!nzchar(path) || identical(path, libname)) {
      return("")
    }
    return(paste0("-L", dirname(path), " -lgomp"))
  }

  ""
}

openmp_makevars_lines <- function() {
  fflags <- openmp_fflags()
  if (!nzchar(fflags)) {
    stop(
      "OpenMP was requested but no OpenMP flags were found for this toolchain.",
      "\nSet QUICKR_OPENMP_FFLAGS to your compiler's OpenMP flags.",
      call. = FALSE
    )
  }
  libs <- openmp_link_flags(fflags = fflags)
  if (!nzchar(libs)) {
    stop(
      "OpenMP was requested but no OpenMP linker flags were found.",
      "\nSet QUICKR_OPENMP_LIBS to your linker OpenMP flags.",
      call. = FALSE
    )
  }
  c(
    paste("PKG_FFLAGS +=", fflags),
    paste("PKG_LIBS +=", libs)
  )
}
