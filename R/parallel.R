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

scope_openmp_depth <- function(scope) {
  if (!inherits(scope, "quickr_scope")) {
    return(0L)
  }
  depth <- attr(scope, "openmp_depth", exact = TRUE)
  if (is.null(depth)) {
    0L
  } else {
    as.integer(depth)
  }
}

scope_in_openmp <- function(scope) {
  scope_openmp_depth(scope) > 0L
}

enter_openmp_scope <- function(scope) {
  if (!inherits(scope, "quickr_scope")) {
    return(NULL)
  }
  previous_depth <- attr(scope, "openmp_depth", exact = TRUE)
  depth <- scope_openmp_depth(scope)
  attr(scope, "openmp_depth") <- depth + 1L
  previous_depth
}

exit_openmp_scope <- function(scope, previous_depth) {
  if (!inherits(scope, "quickr_scope")) {
    return(invisible(NULL))
  }
  if (is.null(previous_depth)) {
    attr(scope, "openmp_depth") <- NULL
  } else {
    attr(scope, "openmp_depth") <- as.integer(previous_depth)
  }
  invisible(TRUE)
}

openmp_abort <- function(message, class = "quickr_openmp_error") {
  stop(
    structure(
      list(message = message, call = sys.call(-1)),
      class = c(class, "error", "condition")
    )
  )
}

is_parallel_decl_call <- function(e) {
  is.call(e) &&
    is.symbol(e[[1L]]) &&
    as.character(e[[1L]]) %in% c("parallel", "omp")
}

parse_parallel_decl <- function(e) {
  stopifnot(is_parallel_decl_call(e))
  args <- as.list(e)[-1L]
  arg_names <- names(args) %||% rep("", length(args))

  private <- NULL
  for (i in seq_along(args)) {
    nm <- arg_names[i]
    val <- args[[i]]
    if (nm == "private") {
      if (is_call(val, quote(c))) {
        elems <- as.list(val)[-1L]
        if (!all(vapply(elems, is.symbol, logical(1L)))) {
          stop(
            "private must be a symbol or c() of symbols, got: ",
            deparse(val),
            call. = FALSE
          )
        }
        private <- vapply(elems, as.character, character(1L))
      } else if (is.symbol(val)) {
        private <- as.character(val)
      } else {
        stop(
          "private must be a symbol or c() of symbols, got: ",
          deparse(val),
          call. = FALSE
        )
      }
    } else {
      stop(
        "unknown argument to ",
        as.character(e[[1L]]),
        "(): ",
        if (nzchar(nm)) nm else deparse(val),
        call. = FALSE
      )
    }
  }

  list(backend = "omp", source = as.character(e[[1L]]), private = private)
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

  function(name, config_value = quickr_r_cmd_config_value) {
    if (is.null(cached)) {
      cached <<- list()
    }
    cached_value <- cached[[name]]
    if (!is.null(cached_value)) {
      return(cached_value)
    }
    value <- config_value(name)
    if (!nzchar(value)) {
      value <- ""
    }
    cached[[name]] <<- value
    value
  }
})

validate_parallel_private <- function(private, scope) {
  if (is.null(private) || !length(private)) {
    return(invisible(TRUE))
  }
  stopifnot(inherits(scope, "quickr_scope"))
  private <- unique(as.character(private))
  unknown <- private[
    !vapply(
      private,
      function(name) inherits(get0(name, scope), Variable),
      logical(1L)
    )
  ]
  if (length(unknown)) {
    stop(
      "could not resolve private symbol",
      if (length(unknown) > 1L) "s" else "",
      ": ",
      str_flatten_commas(unknown),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

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
    openmp_abort(
      paste(
        "OpenMP was requested but no OpenMP flags were found for this toolchain.",
        "Set QUICKR_OPENMP_FFLAGS to your compiler's OpenMP flags.",
        sep = "\n"
      ),
      class = "quickr_openmp_unavailable"
    )
  }
  libs <- openmp_link_flags(fflags = fflags)
  if (!nzchar(libs)) {
    openmp_abort(
      paste(
        "OpenMP was requested but no OpenMP linker flags were found.",
        "Set QUICKR_OPENMP_LIBS to your linker OpenMP flags.",
        sep = "\n"
      ),
      class = "quickr_openmp_unavailable"
    )
  }
  c(
    paste("PKG_FFLAGS +=", fflags),
    paste("PKG_LIBS +=", libs)
  )
}
