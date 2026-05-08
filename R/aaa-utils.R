#' @importFrom glue glue glue_data trim as_glue glue_collapse single_quote
#' @importFrom dotty .
#' @importFrom stats setNames
#' @importFrom utils gethash hashtab remhash sethash str
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x

fortranize_name <- function(name, prefix = "v_") {
  stopifnot(is_string(name), is_string(prefix), nzchar(prefix))
  out <- gsub("[^A-Za-z0-9_]", "_", name)
  if (!nzchar(out) || !grepl("^[A-Za-z]", out)) {
    out <- paste0(prefix, out)
  }
  out
}

fortranize_expr_symbols <- function(expr) {
  if (!is.symbol(expr) && !is.call(expr)) {
    return(expr)
  }
  syms <- all.vars(expr)
  if (!length(syms)) {
    return(expr)
  }
  replacements <- setNames(
    lapply(syms, \(sym) as.symbol(fortranize_name(sym))),
    syms
  )
  substitute_(expr, list2env(replacements, parent = emptyenv()))
}

scope_fortran_symbol <- function(sym, scope) {
  stopifnot(is.symbol(sym))
  if (is.null(scope)) {
    return(sym)
  }
  var <- get0(as.character(sym), scope)
  if (inherits(var, Variable) && !is.null(var@name)) {
    return(as.symbol(var@name))
  }
  sym
}

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

quickr_compiler_probe_cache <- new.env(parent = emptyenv())

quickr_default_makevars_names <- function(
  platform = R.version$platform
) {
  unique(c(
    paste0("Makevars-", platform),
    "Makevars.ucrt",
    "Makevars.win64",
    "Makevars.win32",
    "Makevars.win",
    "Makevars"
  ))
}

quickr_r_etc_path <- function(
  file,
  r_home = R.home(),
  r_arch = Sys.getenv("R_ARCH", unset = "")
) {
  file.path(r_home, paste0("etc", r_arch), file)
}

quickr_default_site_makevars_path <- function() {
  quickr_r_etc_path("Makevars.site")
}

quickr_makeconf_path <- function() {
  quickr_r_etc_path("Makeconf")
}

quickr_makevars_paths <- function() {
  user_makevars <- Sys.getenv("R_MAKEVARS_USER", unset = NA_character_)
  user_paths <- if (!is.na(user_makevars) && nzchar(user_makevars)) {
    user_makevars
  } else {
    user_roots <- unique(c(
      Sys.getenv("HOME", unset = ""),
      Sys.getenv("R_USER", unset = "")
    ))
    user_roots <- user_roots[nzchar(user_roots)]
    if (length(user_roots)) {
      as.vector(outer(
        user_roots,
        file.path(".R", quickr_default_makevars_names()),
        file.path
      ))
    } else {
      character()
    }
  }

  site_makevars <- Sys.getenv("R_MAKEVARS_SITE", unset = NA_character_)
  site_paths <- if (!is.na(site_makevars) && nzchar(site_makevars)) {
    site_makevars
  } else {
    quickr_default_site_makevars_path()
  }

  unique(c(user_paths, site_paths))
}

quickr_file_signature <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(path)) {
    return(paste(path, "<missing>", sep = "="))
  }

  info <- file.info(path)
  hash <- unname(tools::md5sum(path))
  paste(path, info$size, info$mtime, hash, sep = "=")
}

quickr_makevars_include_paths <- function(paths) {
  quickr_makevars_include_paths_impl(
    normalizePath(paths, winslash = "/", mustWork = FALSE),
    visited = character()
  )
}

quickr_makevars_include_paths_impl <- function(paths, visited) {
  paths <- unique(normalizePath(paths, winslash = "/", mustWork = FALSE))
  paths <- setdiff(paths, visited)
  out <- character()

  for (path in paths) {
    if (!file.exists(path)) {
      next
    }

    included <- quickr_makevars_direct_include_paths(path)
    out <- unique(c(out, included))
    if (length(included)) {
      out <- unique(c(
        out,
        quickr_makevars_include_paths_impl(included, c(visited, path))
      ))
    }
  }

  out
}

quickr_makevars_direct_include_paths <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- quickr_join_makevars_continuations(lines)
  variables <- quickr_makevars_variable_map(lines)
  include_paths <- unlist(
    lapply(
      lines,
      quickr_makevars_line_include_paths,
      variables = variables,
      makevars_path = path
    ),
    use.names = FALSE
  )

  unique(include_paths)
}

quickr_join_makevars_continuations <- function(lines) {
  out <- character()
  current <- ""

  for (line in lines) {
    if (nzchar(current)) {
      line <- paste(current, trimws(line, which = "left"))
    }

    if (grepl("\\\\$", line)) {
      current <- sub("\\\\$", "", line)
      next
    }

    out <- c(out, line)
    current <- ""
  }

  if (nzchar(current)) {
    out <- c(out, current)
  }

  out
}

quickr_makevars_variable_map <- function(lines) {
  lines <- trimws(vapply(lines, quickr_strip_make_comment, character(1)))
  assignments <- regexec(
    "^([A-Za-z_][A-Za-z0-9_.-]*)[[:space:]]*[:?+]?=[[:space:]]*(.*)$",
    lines,
    perl = TRUE
  )
  matches <- regmatches(lines, assignments)
  matches <- matches[lengths(matches) == 3L]
  if (!length(matches)) {
    return(character())
  }

  values <- vapply(matches, `[[`, character(1), 3L)
  names(values) <- vapply(matches, `[[`, character(1), 2L)
  values
}

quickr_makevars_line_include_paths <- function(
  line,
  variables,
  makevars_path
) {
  line <- trimws(quickr_strip_make_comment(line))
  match <- regexec(
    "^(-?include|sinclude)[[:space:]]+(.+)$",
    line,
    perl = TRUE
  )
  parts <- regmatches(line, match)[[1]]
  if (length(parts) != 3L) {
    return(character())
  }

  spec <- quickr_expand_makevars_variables(parts[[3]], variables)
  paths <- strsplit(trimws(spec), "[[:space:]]+")[[1]]
  paths <- paths[nzchar(paths)]
  paths <- unlist(
    lapply(paths, quickr_resolve_makevars_include_path, makevars_path),
    use.names = FALSE
  )

  unique(paths)
}

quickr_strip_make_comment <- function(line) {
  sub("(^|[^\\\\])#.*$", "\\1", line, perl = TRUE)
}

quickr_expand_makevars_variables <- function(text, variables) {
  for (i in seq_len(20L)) {
    match <- regexpr(
      "\\$\\(([A-Za-z_][A-Za-z0-9_.-]*)\\)|\\$\\{([A-Za-z_][A-Za-z0-9_.-]*)\\}",
      text,
      perl = TRUE
    )
    if (match[[1]] == -1L) {
      break
    }

    token <- regmatches(text, match)
    name <- sub("^\\$[({]([A-Za-z_][A-Za-z0-9_.-]*)[)}]$", "\\1", token)
    value <- quickr_makevars_variable_value(name, variables)
    regmatches(text, match) <- value
  }

  text
}

quickr_makevars_variable_value <- function(name, variables) {
  if (name %in% names(variables)) {
    return(variables[[name]])
  }

  if (identical(name, "R_HOME")) {
    return(R.home())
  }

  Sys.getenv(name, unset = "")
}

quickr_resolve_makevars_include_path <- function(path, makevars_path) {
  path <- path.expand(path)
  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    return(quickr_expand_makevars_include_globs(path))
  }

  quickr_expand_makevars_include_globs(unique(c(
    path,
    file.path(dirname(makevars_path), path)
  )))
}

quickr_expand_makevars_include_globs <- function(paths) {
  out <- unlist(
    lapply(paths, \(path) {
      matches <- Sys.glob(path)
      if (length(matches)) {
        matches
      } else {
        path
      }
    }),
    use.names = FALSE
  )

  normalizePath(out, winslash = "/", mustWork = FALSE)
}

quickr_makevars_signature <- function() {
  paths <- quickr_makevars_paths()
  if (!length(paths)) {
    return("")
  }
  paths <- unique(c(paths, quickr_makevars_include_paths(paths)))
  paste(vapply(paths, quickr_file_signature, character(1)), collapse = "\r")
}

quickr_makeconf_signature <- function() {
  quickr_file_signature(quickr_makeconf_path())
}

quickr_toolchain_env_signature <- function() {
  vars <- c(
    "PATH",
    "BINPREF",
    "HOME",
    "R_USER",
    "R_ARCH",
    "CC",
    "CXX",
    "FC",
    "F77",
    "CFLAGS",
    "CXXFLAGS",
    "FFLAGS",
    "FCFLAGS",
    "LDFLAGS",
    "LIBS",
    "MAKE",
    "R_MAKEVARS_SITE",
    "R_MAKEVARS_USER"
  )
  env <- Sys.getenv(vars, unset = NA_character_)
  paste(
    c(
      paste(names(env), env, sep = "="),
      paste("MAKEVARS", quickr_makevars_signature(), sep = "="),
      paste("MAKECONF", quickr_makeconf_signature(), sep = "=")
    ),
    collapse = "\r"
  )
}

quickr_r_cmd_config_cache_key <- function(name) {
  paste("r_cmd_config", name, quickr_toolchain_env_signature(), sep = "\r")
}

quickr_cached_r_cmd_config_value <- function(
  name,
  cache = quickr_compiler_probe_cache
) {
  stopifnot(is_string(name), is.environment(cache))

  cache_key <- quickr_r_cmd_config_cache_key(name)
  cached <- get0(cache_key, envir = cache, inherits = FALSE, ifnotfound = NULL)
  if (is.null(cached)) {
    probe <- quickr_r_cmd_config_probe(name)
    cached <- probe$value
    if (isTRUE(probe$ok)) {
      assign(cache_key, cached, envir = cache)
    }
  }

  cached
}

quickr_r_cmd_config_value <- function(
  name,
  r_cmd = quickr_r_cmd(),
  system2 = base::system2
) {
  quickr_r_cmd_config_probe(
    name = name,
    r_cmd = r_cmd,
    system2 = system2
  )$value
}

quickr_r_cmd_config_probe <- function(
  name,
  r_cmd = quickr_r_cmd(),
  system2 = base::system2
) {
  stopifnot(is_string(name), is_string(r_cmd), is.function(system2))

  out <- tryCatch(
    suppressWarnings(system2(
      r_cmd,
      c("CMD", "config", name),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) structure(character(), status = 1L)
  )
  status <- attr(out, "status")
  if (!is.null(status)) {
    return(list(value = "", ok = FALSE))
  }
  value <- trimws(paste(out, collapse = " "))
  if (!nzchar(value)) {
    return(list(value = "", ok = TRUE))
  }
  if (grepl("^ERROR:", value)) {
    return(list(value = "", ok = FALSE))
  }

  list(value = value, ok = TRUE)
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
