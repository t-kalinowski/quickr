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
#' @returns A quicker R function.
#' @export
#' @examples
#' add_ab <- quick(function(a, b) {
#'   declare(type(a = double(n)),
#'           type(b = double(n)))
#'   out <- a + b
#'   out
#' })
#' add_ab(1, 2)
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

  suppressWarnings({
    result <- system2(
      R.home("bin/R"),
      c("CMD SHLIB --use-LTO", "-o", dll_path, fsub_path, c_wrapper_path),
      stdout = TRUE,
      stderr = TRUE
    )
  })
  if (!is.null(status <- attr(result, "status"))) {
    # Adjust the compiler error so RStudio console formatter doesn't mangle
    # the actual error message https://github.com/rstudio/rstudio/issues/16365
    result <- gsub("Error: ", "Compiler Error: ", result, fixed = TRUE)
    writeLines(result, stderr())
    cat("---\nCompiler exit status:", status, "\n", file = stderr())
    stop("Compilation Error", call. = FALSE)
  }

  # tryCatch(dyn.unload(dll_path), error = identity)
  dll <- dyn.load(dll_path)
  c_wrapper_name <- paste0(fsub@name, "_")
  ptr <- getNativeSymbolInfo(c_wrapper_name, dll)$address

  create_quick_closure(fsub@name, fsub@closure, native_symbol = ptr)
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
        "int" #, "double",

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
