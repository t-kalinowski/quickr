#' Compile a Quick Function
#'
#' Compile an R function.
#'
#' @param fun An R function
#' @param name String, name to use for the function. This is optional in
#'   regular usage but required in an R package. As a convenience, arguments
#'   `fun` and `name` can also be supplied as positional arguments to `quick` with
#'   `name` in the first position.
#'
#' @details
#'
#' ## `declare(type())` syntax:
#'
#' The shape and mode of all function arguments must be declared. Local and
#' return variables may optionally also be declared.
#'
#' `declare(type())` also has support for declaring size constraints, or
#' size relationships between variables. Here are some examples of declare
#' calls:
#'
#' ```r
#' declare(type(x = double(NA))) # x is a 1-d double vector of any length
#' declare(type(x = double(10))) # x is a 1-d double vector of length 10
#' declare(type(x = double(1)))  # x is a scalar double
#'
#' declare(type(x = integer(2, 3)))  # x is a 2-d integer matrix with dim (2, 3)
#' declare(type(x = integer(NA, 3))) # x is a 2-d integer matrix with dim (<any>, 3)
#'
#' # x is a 4-d logical matrix with dim (<any>, 24, 24, 3)
#' declare(type(x = logical(NA, 24, 24, 3)))
#'
#' # x and y are 1-d double vectors of any length
#' declare(type(x = double(NA)),
#'         type(y = double(NA)))
#'
#' # x and y are 1-d double vectors of the same length
#' declare(
#'   type(x = double(n)),
#'   type(y = double(n)),
#' )
#'
#' # x and y are 1-d double vectors, where length(y) == length(x) + 2
#' declare(type(x = double(n)),
#'         type(y = double(n+2)))
#' ```
#'
#' You can provide declarations to `declare()` as:
#'
#' - Multiple arguments to a single `declare()` call
#' - Separate `declare()` calls
#' - Multiple arguments within a code block (`{}`) inside `declare()`
#'
#' ```r
#' declare(
#'   type(x = double(n)),
#'   type(y = double(n)),
#' )
#'
#' declare(type(x = double(n)))
#' declare(type(y = double(n)))
#'
#' declare({
#'   type(x = double(n))
#'   type(y = double(n))
#' })
#' ```
#'
#' ## Return values
#'
#' The shape and type of a function return value must be known at compile
#' time. In most situations, this will be automatically inferred by
#' `quick()`. However, if the output is dynamic, then you may need to
#' provide a hint. For example, returning the result of `seq()` will fail
#' because the output shape cannot be inferred.
#'
#' ```r
#' # Will fail to compile:
#' quick_seq <- quick(function(start, end) {
#'   declare({
#'     type(start = integer(1))
#'     type(end = integer(1))
#'   })
#'   out <- seq(start, end)
#'   out
#' })
#' ```
#'
#' However, if the output size can be declared as a dynamic expression using
#' other values known at runtime, compilation will succeed:
#'
#' ```r
#' # Succeeds:
#' quick_seq <- quick(function(start, end) {
#'   declare({
#'     type(start = integer(1))
#'     type(end = integer(1))
#'     type(out = integer(end - start + 1))
#'   })
#'   out <- seq(start, end)
#'   out
#' })
#' quick_seq(1L, 5L)
#' ```
#'
#' ## Fortran compiler
#'
#' quickr compiles via `R CMD SHLIB` and will normally use the same toolchain
#' that R was built/configured with.
#'
#' quickr only uses LLVM flang when it is explicitly requested or, on macOS,
#' when flang is available on `PATH` (and `flang --version` succeeds). If flang
#' is requested but unavailable, compilation errors. If flang compilation
#' fails, quickr retries with the default toolchain; on success it emits a
#' one-time warning and disables automatic flang preference for the rest of the
#' session.
#'
#' In interactive use, you can explicitly control this with:
#'
#' ```r
#' options(quickr.fortran_compiler = "flang")
#' ```
#'
#' To disable the macOS auto-preference, set
#' `options(quickr.fortran_compiler = "gfortran")`.
#'
#' @returns A quicker R function.
#' @export
#' @examples
#' \donttest{
#' add_ab <- quick(function(a, b) {
#'   declare(type(a = double(n)),
#'           type(b = double(n)))
#'   a + b
#' })
#' add_ab(1, 2)
#' add_ab(c(1, 2, 3), c(4, 5, 6))
#' }
quick <- function(fun, name = NULL) {
  if (is.null(name)) {
    name <- if (is.symbol(substitute(fun))) {
      deparse(substitute(fun))
    } else {
      make_unique_name(prefix = "anonymous_quick_function_")
    }
  } else if (is.function(name) && is_string(fun)) {
    .[name, fun] <- list(fun, name)
  }

  if (nzchar(pkgname <- Sys.getenv("DEVTOOLS_LOAD"))) {
    if (!collector$is_active()) {
      if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("Please install 'pkgload'")
      }
      for (i in seq_along(sys.calls())) {
        if (identical(sys.function(i), pkgload::load_code)) {
          collector$activate(paste0(pkgname, ":quick_funcs"))
          defer(dump_collected(), sys.frame(i), after = TRUE)
          break
        }
      }
    }
  }

  if (collector$is_active()) {
    # we are in a quickr::compile_package() or a devtools::load_all() call,
    # merely collecting functions at this point.
    quick_closure <- create_quick_closure(name, fun)
    collector$add(name = name, closure = fun, quick_closure = quick_closure)
    return(quick_closure)
  }

  pkgname <- parent.pkg()
  if (!is.null(pkgname) && pkgname != "quickr") {
    if (startsWith(name, 'anonymous_quick_function_')) {
      stop(
        'When used in an R package, you must provide a unique `name` to every `quick()` call.\n',
        'For example: `my_fun <- quick("my_fun", function(x) ....)'
      )
    }
    # we are in a package - but outside a quickr::compile_package() call.
    return(create_quick_closure(name, fun))
  }

  # not in a package. Compile and load eagerly.
  attr(fun, "name") <- name
  fun <- compile(r2f(fun))
  attr(fun, "name") <- NULL

  fun
}

compile <- function(fsub, build_dir = tempfile(paste0(fsub@name, "-build-"))) {
  stopifnot(inherits(fsub, FortranSubroutine))

  name <- fsub@name
  c_wrapper <- make_c_bridge(fsub)

  if (dir.exists(build_dir)) {
    unlink(build_dir, recursive = T)
  }
  if (!dir.exists(build_dir)) {
    dir.create(build_dir)
  }
  owd <- setwd(build_dir)
  on.exit(setwd(owd))

  fsub_path <- paste0(name, "_fsub.f90")
  c_wrapper_path <- paste0(name, "_c_wrapper.c")
  dll_path <- paste0(name, .Platform$dynlib.ext)
  writeLines(fsub, fsub_path)
  writeLines(c_wrapper, c_wrapper_path)

  # Link against the same BLAS/LAPACK/Fortran libs as the running R
  # to support generated calls to vendor BLAS (e.g., dgemm, dgesv).
  cfg <- quickr_r_cmd_config_value
  BLAS_LIBS <- strsplit(cfg("BLAS_LIBS"), "[[:space:]]+")[[1]]
  LAPACK_LIBS <- strsplit(cfg("LAPACK_LIBS"), "[[:space:]]+")[[1]]
  FLIBS <- strsplit(cfg("FLIBS"), "[[:space:]]+")[[1]]
  BLAS_LIBS <- BLAS_LIBS[nzchar(BLAS_LIBS)]
  LAPACK_LIBS <- LAPACK_LIBS[nzchar(LAPACK_LIBS)]
  FLIBS <- FLIBS[nzchar(FLIBS)]
  link_flags <- c(LAPACK_LIBS, BLAS_LIBS, FLIBS)

  use_openmp <- isTRUE(attr(fsub@scope, "uses_openmp", exact = TRUE))
  suppressWarnings({
    env <- quickr_fcompiler_env(
      build_dir = build_dir,
      use_openmp = use_openmp,
      link_flags = link_flags
    )
    r_args_base <- c(
      "CMD SHLIB --use-LTO",
      "-o",
      dll_path,
      fsub_path,
      c_wrapper_path
    )
    r_args_libs <- c(r_args_base, link_flags)
    is_windows <- identical(.Platform$OS.type, "windows")
    r_args <- if (length(env) && !is_windows) r_args_base else r_args_libs
    result <- system2(
      R.home("bin/R"),
      r_args,
      stdout = TRUE,
      stderr = TRUE,
      env = env
    )
    if (!is.null(attr(result, "status")) && length(env) && !use_openmp) {
      result2 <- system2(
        R.home("bin/R"),
        r_args_libs,
        stdout = TRUE,
        stderr = TRUE
      )
      if (is.null(attr(result2, "status"))) {
        result <- result2
        quickr_disable_flang_auto()
        quickr_warn_flang_fallback_once()
      } else {
        # Prefer to show the flang attempt first, then the fallback attempt.
        result <- c(
          "--- flang attempt ---",
          result,
          "",
          "--- fallback attempt ---",
          result2
        )
        attr(result, "status") <- attr(result2, "status")
      }
    }
  })

  status <- attr(result, "status")
  if (!is.null(status)) {
    # Adjust the compiler error so RStudio console formatter doesn't mangle
    # the actual error message https://github.com/rstudio/rstudio/issues/16365
    result <- gsub("Error: ", "Compiler Error: ", result, fixed = TRUE)
    quickr_warn_compiler_failure_once(
      paste(
        c(result, "---", sprintf("Compiler exit status: %s", status)),
        collapse = "\n"
      )
    )
    if (use_openmp) {
      openmp_abort(
        paste(
          "OpenMP was requested but compilation with OpenMP flags failed.",
          "quickr will not fall back to a non-OpenMP build.",
          "Resolve the OpenMP toolchain or remove the parallel declarations.",
          sep = "\n"
        ),
        class = "quickr_openmp_ignored"
      )
    }
    stop("Compilation Error", call. = FALSE)
  }

  quickr_windows_add_dll_paths(link_flags)

  # tryCatch(dyn.unload(dll_path), error = identity)
  dll <- tryCatch(
    dyn.load(dll_path),
    error = function(e) {
      if (use_openmp) {
        openmp_abort(
          paste(
            "OpenMP was requested but the compiled shared library failed to load.",
            "This usually means the OpenMP runtime (libgomp/libomp) was not found.",
            "Original error:",
            conditionMessage(e),
            sep = "\n"
          ),
          class = "quickr_openmp_load_failed"
        )
      }
      stop(e)
    }
  )
  c_wrapper_name <- paste0(fsub@name, "_")
  ptr <- getNativeSymbolInfo(c_wrapper_name, dll)$address

  create_quick_closure(fsub@name, fsub@closure, native_symbol = ptr)
}

quickr_windows_add_dll_paths <- function(
  flags,
  os_type = .Platform$OS.type,
  config_value = quickr_r_cmd_config_value,
  which = Sys.which
) {
  if (!identical(os_type, "windows")) {
    return(invisible(FALSE))
  }
  dirs <- flags[grepl("^-L", flags)]
  dirs <- sub("^-L", "", dirs)
  dirs <- dirs[nzchar(dirs)]

  bin_siblings <- file.path(dirs, "..", "bin")

  config_values <- c(
    config_value("BINPREF"),
    config_value("FC"),
    config_value("F77"),
    config_value("CC"),
    config_value("CXX")
  )
  config_paths <- vapply(
    config_values,
    function(value) {
      value <- trimws(value)
      if (!nzchar(value)) {
        return("")
      }
      value <- sub("^\"([^\"]+)\".*", "\\1", value)
      value <- sub("^'([^']+)'.*", "\\1", value)
      strsplit(value, "\\s+")[[1L]][[1L]]
    },
    character(1)
  )
  config_bins <- unique(dirname(config_paths[nzchar(config_paths)]))

  r_bin <- R.home("bin")
  r_bin_x64 <- file.path(r_bin, "x64")
  r_bin_i386 <- file.path(r_bin, "i386")

  rtools_roots <- Sys.getenv(c(
    "RTOOLS45_HOME",
    "RTOOLS44_HOME",
    "RTOOLS43_HOME",
    "RTOOLS42_HOME",
    "RTOOLS40_HOME",
    "RTOOLS_HOME"
  ))
  rtools_roots <- rtools_roots[nzchar(rtools_roots)]
  rtools_bins <- unique(c(
    file.path(rtools_roots, "usr", "bin"),
    file.path(rtools_roots, "mingw64", "bin"),
    file.path(rtools_roots, "ucrt64", "bin"),
    file.path(rtools_roots, "x86_64-w64-mingw32.static.posix", "bin"),
    file.path(rtools_roots, "x86_64-w64-mingw32.static", "bin"),
    file.path(rtools_roots, "x86_64-w64-mingw32", "bin")
  ))

  compilers <- which(c("gfortran", "gcc", "clang", "flang"))
  compilers <- compilers[nzchar(compilers)]
  compiler_bins <- unique(dirname(compilers))

  dirs <- unique(c(
    dirs,
    bin_siblings,
    config_bins,
    r_bin,
    r_bin_x64,
    r_bin_i386,
    rtools_bins,
    compiler_bins
  ))
  dirs <- dirs[nzchar(dirs)]
  dirs <- dirs[dir.exists(dirs)]
  if (!length(dirs)) {
    return(invisible(FALSE))
  }

  path <- Sys.getenv("PATH", unset = "")
  existing <- strsplit(path, ";", fixed = TRUE)[[1]]
  existing <- existing[nzchar(existing)]
  existing_norm <- tolower(normalizePath(
    existing,
    winslash = "\\",
    mustWork = FALSE
  ))
  dirs_norm <- tolower(normalizePath(dirs, winslash = "\\", mustWork = FALSE))
  to_add <- dirs[!dirs_norm %in% existing_norm]
  if (length(to_add)) {
    Sys.setenv(PATH = paste(c(to_add, existing), collapse = ";"))
    return(invisible(TRUE))
  }

  invisible(FALSE)
}


create_quick_closure <- function(
  name,
  closure,
  native_symbol = as.name(paste0(name, "_"))
) {
  body(closure) <- as.call(c(
    quote(.External),
    native_symbol,
    lapply(names(formals(closure)), as.name)
  ))
  closure
}


check_all_var_names_valid <- function(fun) {
  nms <- unique(c(names(formals(fun)), all.vars(body(fun), functions = FALSE)))
  invalid <- endsWith(nms, "_") |
    startsWith(nms, "_") |
    nms %in%
      c(
        # clashes with Fortran subroutine symbols
        "c_int",
        "c_double",
        "c_ptrdiff_t",

        # clashes with C bridge symbols
        "int", #, "double",
        quickr_error_msg_name(),
        quickr_error_setter_name()

        # ??? (clashes with R symbols?)
        # "double", "integer"
      )
  if (any(invalid)) {
    stop(
      "symbols cannot start or end with '_', but found: ",
      glue_collapse(invalid, ", ", last = ", and ")
    )
  }

  case_clashes <- nms[is_duplicate(tolower(nms))]
  if (length(case_clashes)) {
    stop(
      "Fortran is case-insensitive; these names conflict when case is ignored: ",
      glue_collapse(glue::backtick(case_clashes), ", ", last = " and ")
    )
  }
}


# ---- utils ----

make_unique_name <- local({
  i <- 0L
  function(prefix = "tmp") {
    paste0(prefix, i <<- i + 1L)
  }
})

is_duplicate <- function(x) {
  out <- duplicated(x) | duplicated(x, fromLast = TRUE)
  names(out) <- names(x)
  out
}
