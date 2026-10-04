#' Include R code from a file
#'
#' `include()` inserts the R code in a file at the point where it is called,
#' exactly as if the file's contents were written there. It lets a `quick()`
#' function keep its helper functions, constants, and other code in separate
#' files.
#'
#' When a function is compiled with [quick()], each `include()` call is
#' replaced by the parsed contents of its file before compilation. When the
#' same function runs as plain R, `include()` evaluates the file's code in the
#' calling function's frame. Both give the same result.
#'
#' - `include()` must be a top-level statement of a function body (the
#'   `quick()` function or a local function defined inside it); it cannot be
#'   used conditionally, inside a loop, or as part of an expression.
#' - The path is evaluated when the function is compiled, in the function's
#'   environment, so it can be computed (e.g. `file.path(dir, "helpers.R")`)
#'   but cannot depend on the function's arguments.
#' - Relative paths are resolved against the working directory, as with
#'   [source()]. In a package, this is the package root during
#'   `pkgload::load_all()` and [compile_package()].
#' - Included files can include other files. Include cycles are an error.
#' - Included code follows the same rules as code written inline.
#'
#' @param path Path to an R source file.
#' @returns `NULL`, invisibly.
#' @export
#' @examples
#' \donttest{
#' helpers <- tempfile(fileext = ".R")
#' writeLines(c(
#'   "gain <- 2.5",
#'   "clamp <- function(v, lo, hi) {",
#'   "  if (v < lo) return(lo)",
#'   "  if (v > hi) return(hi)",
#'   "  v",
#'   "}"
#' ), helpers)
#'
#' scale_clamp <- quick(function(x) {
#'   declare(type(x = double(NA)))
#'   include(helpers)
#'   out <- double(length(x))
#'   for (i in seq_along(x)) {
#'     out[i] <- clamp(x[i] * gain, 0, 1)
#'   }
#'   out
#' })
#' scale_clamp(c(-1, 0.1, 0.3, 2))
#' }
include <- function(path) {
  path_label <- include_path_label(path)
  file <- include_resolve_path(path, path_label)
  include_check_cycle(file, path_label, include_runtime$files, include_runtime$labels)
  include_runtime$files <- c(include_runtime$files, file)
  include_runtime$labels <- c(include_runtime$labels, path_label)
  on.exit({
    n <- length(include_runtime$files)
    include_runtime$files <- include_runtime$files[-n]
    include_runtime$labels <- include_runtime$labels[-n]
  })

  env <- parent.frame()
  for (expr in include_parse_file(file, path_label)) {
    eval(expr, env)
  }
  invisible()
}

# Files being included by the running (uncompiled) include() calls, to
# detect cycles the same way compilation does.
include_runtime <- new.env(parent = emptyenv())
include_runtime$files <- character()
include_runtime$labels <- character()


# --- Local Helpers ---

is_include_call <- function(e) {
  is.call(e) &&
    (identical(e[[1L]], quote(include)) ||
      identical(e[[1L]], quote(quickr::include)) ||
      identical(e[[1L]], quote(quickr:::include)))
}

include_path_label <- function(path) {
  if (!is_string(path)) {
    stop(
      "include() path must be a single string, not ",
      deparse1(path),
      call. = FALSE
    )
  }
  path
}

# Resolve a path against the working directory, as source() does.
include_resolve_path <- function(path, label) {
  file <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(file) || dir.exists(file)) {
    stop(
      "include() file not found: \"",
      label,
      "\" (resolved to \"",
      file,
      "\")",
      call. = FALSE
    )
  }
  file
}

include_check_cycle <- function(file, label, files, labels) {
  if (file %in% files) {
    chain <- c(labels[seq(match(file, files), length(labels))], label)
    stop(
      "include() cycle: ",
      paste0("\"", chain, "\"", collapse = " -> "),
      call. = FALSE
    )
  }
  invisible()
}

include_parse_file <- function(file, label) {
  tryCatch(
    as.list(parse(file = file, keep.source = FALSE)),
    error = function(e) {
      stop(
        "could not parse included file \"",
        label,
        "\": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

# Replace include() statements in the body of `fun` (and in the bodies of
# local functions defined in it) with the parsed contents of their files.
# Used by: new_fortran_subroutine()
expand_includes <- function(fun) {
  stopifnot(is.function(fun))
  if (!"include" %in% all.names(body(fun))) {
    return(fun)
  }
  body(fun) <- include_expand_body(
    body(fun),
    env = environment(fun),
    files = character(),
    labels = character()
  )
  fun
}

# A function body: include() may appear as one of its top-level statements.
include_expand_body <- function(body, env, files, labels) {
  stmts <- if (is_call(body, "{")) as.list(body)[-1L] else list(body)
  expanded <- include_expand_statements(stmts, env, files, labels)
  if (!is_call(body, "{") && length(expanded) == 1L) {
    return(expanded[[1L]])
  }
  as.call(c(quote(`{`), expanded))
}

include_expand_statements <- function(stmts, env, files, labels) {
  out <- list()
  for (i in seq_along(stmts)) {
    stmt <- stmts[[i]]
    if (is_include_call(stmt)) {
      out <- c(out, include_file_statements(stmt, env, files, labels))
    } else {
      out <- c(out, list(include_expand_nested(stmt, env, files, labels)))
    }
  }
  out
}

# Any other expression: include() is an error here, except inside the body
# of a function defined within it.
include_expand_nested <- function(e, env, files, labels) {
  if (!is.call(e)) {
    return(e)
  }
  if (is_include_call(e)) {
    stop(
      "include() must be a top-level statement of a function body: ",
      deparse1(e),
      call. = FALSE
    )
  }
  if (is_function_call(e)) {
    if (length(e) >= 3L && !is.null(e[[3L]])) {
      e[[3L]] <- include_expand_body(e[[3L]], env, files, labels)
    }
    return(e)
  }
  for (i in seq_along(e)) {
    el <- e[[i]]
    if (identical(el, quote(expr = )) || !is.call(el)) {
      next
    }
    new_el <- include_expand_nested(el, env, files, labels)
    if (!identical(new_el, el)) {
      e[[i]] <- new_el
    }
  }
  e
}

# The statements an include() call stands for.
include_file_statements <- function(call, env, files, labels) {
  args <- as.list(call)[-1L]
  if (length(args) != 1L || !(names(args) %||% "") %in% c("", "path")) {
    stop(
      "include() takes a single file path: ",
      deparse1(call),
      call. = FALSE
    )
  }
  path_expr <- args[[1L]]
  path <- tryCatch(
    eval(path_expr, env),
    error = function(e) {
      stop(
        "include() paths are resolved when the function is compiled, ",
        "so `",
        deparse1(path_expr),
        "` must not depend on the function's arguments or variables: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  label <- include_path_label(path)
  file <- include_resolve_path(path, label)
  include_check_cycle(file, label, files, labels)

  stmts <- include_parse_file(file, label)
  include_expand_statements(stmts, env, c(files, file), c(labels, label))
}
