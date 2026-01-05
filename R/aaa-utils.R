#' @importFrom glue glue glue_data trim as_glue glue_collapse single_quote
#' @importFrom dotty .
#' @importFrom stats setNames
#' @importFrom utils gethash hashtab remhash sethash str
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x

quickr_r_cmd <- function(
  os_type = .Platform$OS.type,
  r_home = R.home,
  file_exists = file.exists
) {
  r_cmd <- r_home("bin/R")
  if (identical(os_type, "windows") && !file_exists(r_cmd)) {
    r_cmd <- paste0(r_cmd, ".exe")
  }
  r_cmd
}

quickr_r_cmd_config_value <- function(
  name,
  r_cmd = quickr_r_cmd(),
  system2 = system2
) {
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
    return("")
  }
  value <- trimws(paste(out, collapse = " "))
  if (!nzchar(value) || grepl("^ERROR:", value)) {
    return("")
  }
  value
}


# @export
# This will be exported by S7 next release.
`:=` <- function(left, right) {
  name <- substitute(left)
  if (!is.symbol(name)) {
    stop("left hand side must be a symbol")
  }

  right <- substitute(right)
  if (!is.call(right)) {
    stop("right hand side must be a call")
  }

  if (
    is.symbol(cl <- right[[1L]]) &&
      as.character(cl) %in% c("function", "new.env")
  ) {
    # attach "name" attr for usage like:
    # foo := function(){}
    # foo := new.env()
    right <- eval(right, parent.frame())
    attr(right, "name") <- as.character(name)
  } else {
    # for all other usage,
    # inject name as a named arg, so that
    #   foo := new_class(...)
    # becomes
    #   foo <- new_class(..., name = "foo")

    right <- as.call(c(as.list(right), list(name = as.character(name))))

    ## skip check; if duplicate 'name' arg is an issue the call itself will signal an error.
    # if (hasName(right, "name")) stop("duplicate `name` argument.")

    ## alternative code path that injects `name` as positional arg instead
    # right <- as.list(right)
    # right <- as.call(c(right[[1L]], as.character(name), right[-1L]))
  }

  eval(call("<-", name, right), parent.frame())
}

`%error%` <- function(x, y) tryCatch(x, error = function(e) y)

`append<-` <- function(x, after, value) {
  if (missing(after)) c(x, value) else append(x, value, after = after)
}

`append1<-` <- function(x, value) {
  stopifnot(is.list(x) || identical(mode(x), mode(value)))
  x[[length(x) + 1L]] <- value
  x
}

`prepend<-` <- function(x, value) {
  c(vector(typeof(x)), value, x)
}

`add<-` <- `+` #function(x, value) x + value

map_int <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = 0L, ...)
map_lgl <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ...)
map_chr <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = "", ...)

imap <- function(.x, .f, ...) {
  out <- .mapply(.f, list(.x, names(.x) %||% seq_along(.x)), list(...))
  names(out) <- names(.x)
  out
}

map2 <- function(.x, .y, .f, ...) {
  if (length(.x) != length(.y) && length(.x) != 1L && length(.y) != 1L) {
    stop(
      ".x and .y must have the same length, or one of them must have length 1"
    )
  }
  out <- .mapply(.f, list(.x, .y), list(...))
  if (length(.x) == length(out)) {
    names(out) <- names(.x)
  }
  out
}

discard <- function(.x, .f, ...) {
  .x[!vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ...)]
}

keep <- function(.x, .f, ...) {
  .x[vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ...)]
}

compact <- function(.x) {
  .x[as.logical(lengths(.x, use.names = FALSE))]
}

drop_nulls <- function(x, i) {
  if (missing(i)) {
    x[!vapply(X = x, FUN = is.null, FUN.VALUE = FALSE, USE.NAMES = FALSE)]
  } else {
    drop <- logical(length(x))
    names(drop) <- names(x)
    drop[i] <-
      vapply(X = x[i], FUN = is.null, FUN.VALUE = FALSE, USE.NAMES = FALSE)
    x[!drop]
  }
}

last <- function(x) x[[length(x)]]
drop_last <- function(x) x[-length(x)]

compile_nonreturn_statements <- function(stmts, scope) {
  if (!length(stmts)) {
    check_pending_parallel_consumed(scope)
    return("")
  }

  compiled <- vector("list", length(stmts))
  for (i in seq_along(stmts)) {
    stmt <- stmts[[i]]
    check_pending_parallel_target(stmt, scope)
    stmt_f <- r2f(stmt, scope)
    if (!is.null(stmt_f@value)) {
      stop(
        "all expressions except the final return must compile to a statement (no value); found: ",
        deparse1(stmt),
        call. = FALSE
      )
    }
    compiled[[i]] <- stmt_f
  }

  check_pending_parallel_consumed(scope)
  str_flatten_lines(compiled)
}
# fmt: skip
{
is_scalar_na      <- function(x) is.atomic(x)    && !is.object(x) && length(x) == 1L && is.na(x)
is_scalar_atomic  <- function(x) is.atomic(x)    && !is.object(x) && length(x) == 1L && !is.na(x)
is_scalar_integer <- function(x) is.integer(x)   && !is.object(x) && length(x) == 1L && !is.na(x)
is_string         <- function(x) is.character(x) && length(x) == 1L && !is.na(x) # could also be 'glue' class.
is_bool           <- function(x) is.logical(x)   && !is.object(x) && length(x) == 1L && !is.na(x)
is_number         <- function(x) is.numeric(x)   && !is.object(x) && length(x) == 1L && !is.na(x)
is_scalar_integerish <- function(x) is_number(x) && trunc(x) == x
is_wholenumber    <- function(x) is.numeric(x)   && !is.object(x) && length(x) == 1L && !is.na(x) &&
  x >= 0L && (is.integer(x) || is.double(x) && trunc(x) == x)
}

new_function <- function(args = NULL, body = NULL, env = parent.frame()) {
  as.function.default(c(args, body %||% list(NULL)), env)
}

is_call <- function(x, name = NULL) {
  is.call(x) && (is.null(name) || identical(as.symbol(name), x[[1L]]))
}

str_flatten <- function(x, collapse = "") {
  paste0(as.character(unlist(x, use.names = FALSE)), collapse = collapse)
}

str_flatten_lines <- function(...) {
  paste0(unlist(c(character(), ...), use.names = FALSE), collapse = "\n")
}

str_flatten_commas <- function(...) {
  paste0(unlist(c(character(), ...), use.names = FALSE), collapse = ", ")
}

str_flatten_args <- function(..., multiline = length(dots) >= 3) {
  dots <- unlist(c(character(), ...), use.names = FALSE)
  if (multiline) {
    dots <- paste0("\n  ", dots, collapse = ",")
    paste(dots, "\n")
  } else {
    paste0(dots, collapse = ",")
  }
}

interleave <- function(x, y) {
  stopifnot(is.atomic(x), is.atomic(y), length(y) == 1L, typeof(x) == typeof(y))
  drop_last(as.vector(rbind(x, y, deparse.level = 0L)))
}

str_split_lines <- function(...) {
  x <- c(...) |>
    unlist(use.names = FALSE) |>
    strsplit("\n", fixed = TRUE)
  x[!lengths(x)] <- ""
  x |>
    unlist(use.names = FALSE) |>
    trimws("right")
}

indent <- function(x, n = 2L) {
  x <- str_split_lines(x)
  x <- sub("[ \t\r]+$", "", x, perl = TRUE) # trim trailing whitespace
  paste0(strrep(" ", n), x, collapse = "\n")
}

# fmt: skip
parent.pkg <- function(env = parent.frame(2)) {
  if (isNamespace(env <- topenv(env)))
    as.character(getNamespaceName(env)) # unname
  else
    NULL # print visible
}

set_names <- function(x, nm = x, ...) {
  names(x) <- as.character(
    if (is.function(nm)) {
      nm(names(x), ...)
    } else {
      unlist(list(nm, ...), use.names = FALSE)
    }
  )
  x
}

zip_lists <- function(...) {
  x <- if (...length() == 1L) ..1 else list(...)

  if (is.character(nms.1 <- names(x.1 <- x[[1L]]))) {
    if (anyDuplicated(nms.1) || anyNA(nms.1) || any(nms.1 == "")) {
      stop(
        "All names must be unique.",
        " (Use `unname()` for positional matching.)"
      )
    }
  }

  if (length(setdiff(lengths(x), 1L)) != 1L) {
    stop("all elements must have the same length")
  }

  for (i in seq_along(x)) {
    if (identical(nms.1, nms.i <- names(x[[i]]))) {
      next
    }
    if (setequal(nms.1, nms.i)) {
      x[[i]] <- x[[i]][nms.1]
      next
    }
    stop(
      "All names of arguments provided to `zip_lists()` must match.",
      " Call `unname()` on each argument if you want positional matching"
    )
  }
  ans <- .mapply(list, x, NULL)
  names(ans) <- nms.1
  ans
}

is_missing <- function(x) missing(x) || identical(x, quote(expr = ))


is_type_call <- function(e) {
  is.call(e) && identical(e[[1]], quote(type))
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}

substitute_ <- function(expr, env) {
  do.call(base::substitute, list(expr, env))
}

defer <- function(expr, env = parent.frame(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}

is_scalar <- function(x) identical(length(x), 1L)
