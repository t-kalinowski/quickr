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
  platform = Sys.getenv("R_PLATFORM", unset = R.version$platform),
  os_type = Sys.getenv("R_OSTYPE", unset = .Platform$OS.type)
) {
  if (identical(os_type, "windows")) {
    return(c(
      "Makevars.ucrt",
      "Makevars.win64",
      "Makevars.win",
      "Makevars"
    ))
  }

  c(paste0("Makevars-", platform), "Makevars")
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

quickr_regular_file_exists <- function(path) {
  info <- file.info(path)
  !is.na(info$isdir) && !info$isdir
}

quickr_default_user_makevars_paths <- function() {
  home <- Sys.getenv("HOME", unset = "")
  if (!nzchar(home)) {
    return(character())
  }

  file.path(home, ".R", quickr_default_makevars_names())
}

quickr_makevars_paths <- function() {
  user_makevars <- Sys.getenv("R_MAKEVARS_USER", unset = NA_character_)
  user_paths <- if (!is.na(user_makevars) && nzchar(user_makevars)) {
    if (quickr_regular_file_exists(user_makevars)) {
      user_makevars
    } else {
      unique(c(user_makevars, quickr_default_user_makevars_paths()))
    }
  } else {
    quickr_default_user_makevars_paths()
  }

  site_makevars <- Sys.getenv("R_MAKEVARS_SITE", unset = NA_character_)
  site_paths <- if (!is.na(site_makevars) && nzchar(site_makevars)) {
    site_makevars
  } else {
    quickr_default_site_makevars_path()
  }

  unique(c(site_paths, user_paths))
}

quickr_first_existing_file <- function(paths) {
  paths <- paths[vapply(paths, quickr_regular_file_exists, logical(1))]
  if (!length(paths)) {
    return(character())
  }

  paths[[1]]
}

quickr_active_makevars_paths <- function() {
  site_makevars <- Sys.getenv("R_MAKEVARS_SITE", unset = NA_character_)
  site_path <- if (!is.na(site_makevars) && nzchar(site_makevars)) {
    site_makevars
  } else {
    quickr_default_site_makevars_path()
  }
  site_path <- if (quickr_regular_file_exists(site_path)) {
    site_path
  } else {
    character()
  }

  user_makevars <- Sys.getenv("R_MAKEVARS_USER", unset = NA_character_)
  user_path <- if (
    !is.na(user_makevars) &&
      nzchar(user_makevars) &&
      quickr_regular_file_exists(user_makevars)
  ) {
    user_makevars
  } else {
    quickr_first_existing_file(quickr_default_user_makevars_paths())
  }

  unique(c(site_path, user_path))
}

quickr_file_signature <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!quickr_regular_file_exists(path)) {
    return(paste(path, "<missing>", sep = "="))
  }

  info <- file.info(path)
  hash <- unname(tools::md5sum(path))
  paste(path, info$size, info$mtime, hash, sep = "=")
}

quickr_makevars_include_paths <- function(paths) {
  scan <- quickr_makevars_scan_include_paths(
    normalizePath(paths, winslash = "/", mustWork = FALSE),
    variables = character(),
    visited = character()
  )
  scan$paths
}

quickr_makevars_scan_include_paths <- function(paths, variables, visited) {
  paths <- unique(normalizePath(paths, winslash = "/", mustWork = FALSE))
  out <- character()
  vars <- variables

  for (path in paths) {
    scan <- quickr_makevars_scan_one_include_path(path, vars, visited)
    out <- unique(c(out, scan$paths))
    vars <- scan$variables
  }

  list(paths = out, variables = vars)
}

quickr_makevars_scan_one_include_path <- function(path, variables, visited) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (path %in% visited || !quickr_regular_file_exists(path)) {
    return(list(paths = character(), variables = variables))
  }

  lines <- readLines(path, warn = FALSE)
  lines <- quickr_join_makevars_continuations(lines)
  out <- character()
  vars <- variables
  conditional_stack <- list()

  for (line in lines) {
    line <- trimws(quickr_strip_make_comment(line))
    if (!nzchar(line)) {
      next
    }

    conditional <- quickr_makevars_update_conditionals(
      line,
      vars,
      conditional_stack
    )
    if (!is.null(conditional)) {
      conditional_stack <- conditional
      next
    }

    if (!quickr_makevars_conditionals_active(conditional_stack)) {
      next
    }

    assignment <- quickr_makevars_assignment(line)
    if (!is.null(assignment)) {
      vars <- quickr_makevars_apply_assignment(vars, assignment)
      next
    }

    included <- quickr_makevars_include_paths_from_line(line, vars, path)
    out <- unique(c(out, included))
    if (length(included)) {
      scan <- quickr_makevars_scan_include_paths(
        included,
        vars,
        c(visited, path)
      )
      out <- unique(c(out, scan$paths))
      vars <- scan$variables
    }
  }

  list(paths = out, variables = vars)
}

quickr_makevars_update_conditionals <- function(line, variables, stack) {
  condition <- quickr_makevars_condition_result(line, variables)
  if (!is.null(condition)) {
    parent_active <- quickr_makevars_conditionals_active(stack)
    stack[[length(stack) + 1L]] <- list(
      parent_active = parent_active,
      matched = condition,
      active = parent_active && condition
    )
    return(stack)
  }

  if (grepl("^else([[:space:]]|$)", line)) {
    if (!length(stack)) {
      return(NULL)
    }

    rest <- trimws(sub("^else", "", line))
    top <- stack[[length(stack)]]
    condition <- if (nzchar(rest)) {
      quickr_makevars_condition_result(rest, variables)
    } else {
      TRUE
    }
    if (is.null(condition)) {
      condition <- TRUE
    }
    top$active <- top$parent_active && !top$matched && condition
    top$matched <- top$matched || condition
    stack[[length(stack)]] <- top
    return(stack)
  }

  if (identical(line, "endif")) {
    if (!length(stack)) {
      return(NULL)
    }

    stack[[length(stack)]] <- NULL
    return(stack)
  }

  NULL
}

quickr_makevars_conditionals_active <- function(stack) {
  if (!length(stack)) {
    return(TRUE)
  }

  isTRUE(stack[[length(stack)]]$active)
}

quickr_makevars_condition_result <- function(line, variables) {
  match <- regexec(
    "^(ifeq|ifneq)[[:space:]]*\\((.*),(.*)\\)$",
    line,
    perl = TRUE
  )
  parts <- regmatches(line, match)[[1]]
  if (length(parts) == 4L) {
    left <- quickr_makevars_condition_value(parts[[3]], variables)
    right <- quickr_makevars_condition_value(parts[[4]], variables)
    equal <- identical(left, right)
    return(if (identical(parts[[2]], "ifeq")) equal else !equal)
  }

  match <- regexec(
    "^(ifeq|ifneq)[[:space:]]+(['\"])(.*)\\2[[:space:]]+(['\"])(.*)\\4$",
    line,
    perl = TRUE
  )
  parts <- regmatches(line, match)[[1]]
  if (length(parts) == 6L) {
    left <- quickr_makevars_condition_value(parts[[4]], variables)
    right <- quickr_makevars_condition_value(parts[[6]], variables)
    equal <- identical(left, right)
    return(if (identical(parts[[2]], "ifeq")) equal else !equal)
  }

  match <- regexec(
    "^(ifdef|ifndef)[[:space:]]+(.+)$",
    line,
    perl = TRUE
  )
  parts <- regmatches(line, match)[[1]]
  if (length(parts) == 3L) {
    value <- quickr_makevars_variable_value(trimws(parts[[3]]), variables)
    defined <- nzchar(value)
    return(if (identical(parts[[2]], "ifdef")) defined else !defined)
  }

  NULL
}

quickr_makevars_condition_value <- function(value, variables) {
  value <- trimws(quickr_expand_makevars_variables(value, variables))
  sub("^(['\"])(.*)\\1$", "\\2", value, perl = TRUE)
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

quickr_makevars_assignment <- function(line) {
  match <- regexec(
    "^(?:(?:export|override|private)[[:space:]]+)*([A-Za-z_][A-Za-z0-9_.-]*)[[:space:]]*([:?+]?=)[[:space:]]*(.*)$",
    line,
    perl = TRUE
  )
  parts <- regmatches(line, match)[[1]]
  if (length(parts) != 4L) {
    return(NULL)
  }

  list(name = parts[[2]], operator = parts[[3]], value = parts[[4]])
}

quickr_makevars_apply_assignment <- function(variables, assignment) {
  name <- assignment$name
  value <- assignment$value

  if (
    identical(assignment$operator, "?=") &&
      (name %in%
        names(variables) ||
        !is.na(Sys.getenv(name, unset = NA_character_)))
  ) {
    return(variables)
  }

  if (identical(assignment$operator, ":=")) {
    value <- quickr_expand_makevars_variables(value, variables)
  }

  if (identical(assignment$operator, "+=") && name %in% names(variables)) {
    value <- paste(variables[[name]], value)
  }

  variables[[name]] <- value
  variables
}

quickr_makevars_include_paths_from_line <- function(
  line,
  variables,
  makevars_path
) {
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
  spec <- quickr_expand_makevars_wildcard_functions(spec)
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

quickr_expand_makevars_wildcard_functions <- function(text) {
  for (i in seq_len(20L)) {
    match <- regexpr(
      "\\$\\(wildcard[[:space:]]+([^()]*)\\)|\\$\\{wildcard[[:space:]]+([^{}]*)\\}",
      text,
      perl = TRUE
    )
    if (match[[1]] == -1L) {
      break
    }

    token <- regmatches(text, match)
    spec <- sub(
      "^\\$\\(wildcard[[:space:]]+([^()]*)\\)$",
      "\\1",
      token,
      perl = TRUE
    )
    if (identical(spec, token)) {
      spec <- sub(
        "^\\$\\{wildcard[[:space:]]+([^{}]*)\\}$",
        "\\1",
        token,
        perl = TRUE
      )
    }
    regmatches(text, match) <- quickr_makevars_wildcard_value(spec)
  }

  text
}

quickr_makevars_wildcard_value <- function(spec) {
  paths <- strsplit(trimws(spec), "[[:space:]]+")[[1]]
  paths <- paths[nzchar(paths)]
  matches <- unlist(lapply(paths, Sys.glob), use.names = FALSE)
  if (!length(matches)) {
    return("")
  }

  paste(
    normalizePath(matches, winslash = "/", mustWork = FALSE),
    collapse = " "
  )
}

quickr_makevars_variable_value <- function(name, variables) {
  if (name %in% names(variables)) {
    return(variables[[name]])
  }

  if (identical(name, "R_HOME")) {
    return(R.home())
  }
  if (identical(name, "CURDIR")) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }

  Sys.getenv(name, unset = "")
}

quickr_resolve_makevars_include_path <- function(path, makevars_path) {
  path <- path.expand(path)
  quickr_expand_makevars_include_globs(path)
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

quickr_makevars_all_paths <- function() {
  paths <- quickr_makevars_paths()
  active_paths <- quickr_active_makevars_paths()
  if (!length(paths) && !length(active_paths)) {
    return(character())
  }

  unique(c(paths, quickr_makevars_include_paths(active_paths)))
}

quickr_active_makevars_all_paths <- function() {
  paths <- quickr_active_makevars_paths()
  if (!length(paths)) {
    return(character())
  }

  unique(c(paths, quickr_makevars_include_paths(paths)))
}

quickr_makevars_signature <- function() {
  paths <- quickr_makevars_all_paths()
  if (!length(paths)) {
    return("")
  }
  paste(vapply(paths, quickr_file_signature, character(1)), collapse = "\r")
}

quickr_makevars_env_signature <- function() {
  paths <- quickr_active_makevars_all_paths()
  paths <- paths[vapply(paths, quickr_regular_file_exists, logical(1))]
  if (!length(paths)) {
    return("")
  }

  refs <- unique(unlist(
    lapply(paths, quickr_makevars_variable_refs),
    use.names = FALSE
  ))
  if (!length(refs)) {
    return("")
  }

  values <- vapply(refs, quickr_makevars_env_value, character(1))
  paste(paste(names(values), values, sep = "="), collapse = "\r")
}

quickr_makevars_has_uncached_functions <- function() {
  paths <- quickr_active_makevars_all_paths()
  paths <- paths[vapply(paths, quickr_regular_file_exists, logical(1))]
  if (!length(paths)) {
    return(FALSE)
  }

  any(vapply(paths, quickr_file_has_makevars_uncached_function, logical(1)))
}

quickr_file_has_makevars_uncached_function <- function(path) {
  lines <- readLines(path, warn = FALSE)
  any(grepl(
    "\\$\\((shell|wildcard)[[:space:]]|\\$\\{(shell|wildcard)[[:space:]]",
    lines,
    perl = TRUE
  ))
}

quickr_makevars_variable_refs <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- quickr_join_makevars_continuations(lines)
  lines <- vapply(lines, quickr_strip_make_comment, character(1))
  unique(c(
    quickr_makevars_text_variable_refs(paste(lines, collapse = "\n")),
    quickr_makevars_conditional_variable_refs(lines)
  ))
}

quickr_makevars_conditional_variable_refs <- function(lines) {
  lines <- trimws(lines)
  refs <- unlist(
    lapply(lines, \(line) {
      match <- regexec(
        "^(?:else[[:space:]]+)?(?:ifdef|ifndef)[[:space:]]+([A-Za-z_][A-Za-z0-9_.-]*)$",
        line,
        perl = TRUE
      )
      parts <- regmatches(line, match)[[1]]
      if (length(parts) == 2L) {
        parts[[2]]
      } else {
        character()
      }
    }),
    use.names = FALSE
  )

  unique(refs)
}

quickr_makevars_text_variable_refs <- function(text) {
  matches <- gregexpr(
    "\\$\\(([A-Za-z_][A-Za-z0-9_.-]*)\\)|\\$\\{([A-Za-z_][A-Za-z0-9_.-]*)\\}",
    text,
    perl = TRUE
  )
  tokens <- regmatches(text, matches)[[1]]
  if (!length(tokens)) {
    return(character())
  }

  unique(sub(
    "^\\$[({]([A-Za-z_][A-Za-z0-9_.-]*)[)}]$",
    "\\1",
    tokens
  ))
}

quickr_makevars_env_value <- function(name) {
  if (identical(name, "R_HOME")) {
    return(R.home())
  }
  if (identical(name, "CURDIR")) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }

  Sys.getenv(name, unset = "")
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
    "R_PLATFORM",
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
      paste("MAKEVARS_ENV", quickr_makevars_env_signature(), sep = "="),
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

  if (quickr_makevars_has_uncached_functions()) {
    return(quickr_r_cmd_config_probe(name)$value)
  }

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
