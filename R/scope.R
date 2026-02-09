new_ordered_env <- function(parent = emptyenv()) {
  env <- new.env(parent = parent)
  class(env) <- "quickr_ordered_env"
  env
}

#' @export
`[[<-.quickr_ordered_env` <- function(x, name, value) {
  attr(x, "ordered_names") <- unique(c(attr(x, "ordered_names", TRUE), name))
  # Allow scopes to pre-declare which symbols are "return" variables (external
  # outputs). When logical, quickr represents these using integer storage.
  if (inherits(value, Variable)) {
    return_names <- if (inherits(x, "quickr_scope")) {
      scope_get(x, "return_names")
    } else {
      attr(x, "return_names", exact = TRUE)
    }
    if (!is.null(return_names) && as.character(name) %in% return_names) {
      value@is_return <- TRUE
      if (identical(value@mode, "logical")) {
        value@logical_as_int <- TRUE
      }
    }
  }
  assign(name, value, envir = x)
  x
  # NextMethod()
}

#' @export
`[[.quickr_ordered_env` <- function(x, name) {
  get0(name, x) # name can be a symbols too
}

#' @export
names.quickr_ordered_env <- function(x) {
  all_names <- ls(envir = x, sorted = FALSE)
  ordered_names <- attr(x, "ordered_names", TRUE)
  if (!setequal(all_names, ordered_names)) {
    warning("untracked name")
    stop("untracked name")
  }
  ordered_names
}

#' @export
as.list.quickr_ordered_env <- function(x, ...) {
  out <- as.list.environment(x, all.names = TRUE, ...)
  out[names.quickr_ordered_env(x)]
}

#' @export
print.quickr_ordered_env <- function(x, ...) {
  emit("env (class: ", str_flatten_commas(class(x)), ") with bindings:")
  str(as.list.quickr_ordered_env(x), no.list = TRUE)
}


check_assignment_compatible <- function(target, value) {
  if (is.null(value)) {
    return()
  }
  stopifnot(exprs = {
    inherits(target, Variable)
    inherits(value, Variable)
    passes_as_scalar(target) ||
      passes_as_scalar(value) ||
      target@rank == value@rank
  })
}

new_scope <- function(closure, parent = emptyenv()) {
  scope <- new_ordered_env(parent = parent)
  class(scope) <- unique(c("quickr_scope", class(scope)))
  state <- new.env(parent = emptyenv())
  attr(scope, "state") <- state

  state$closure <- closure
  state$kind <- if (is.null(closure)) "block" else "subroutine"
  state$return_names <- character()
  state$internal_procs <- list()

  state$get_unique_var <- local({
    i <- 0L
    function(...) {
      prefix <- switch(
        state$kind %||% "subroutine",
        block = "btmp",
        closure = "ctmp",
        subroutine = "tmp",
        "tmp"
      )
      name <- paste0(prefix, i <<- i + 1L, "_")
      (scope[[name]] <- Variable(..., name = name))
    }
  })

  state$get_unique_proc <- local({
    i <- 0L
    function(prefix = "closure") {
      stopifnot(is_string(prefix))
      paste0(prefix, i <<- i + 1L, "_")
    }
  })

  state$new_child <- function(kind = c("block", "closure")) {
    kind <- match.arg(kind)
    child <- new_scope(closure = NULL, parent = scope)
    scope_set(child, "kind", kind)
    child
  }

  state$assign <- function(name, value) {
    stopifnot(inherits(value, Variable), is.symbol(name) || is_string(name))
    name <- as.character(name)
    existing <- get0(name, scope)
    if (inherits(existing, Variable)) {
      check_assignment_compatible(existing, value)
    }
    value@name <- name
    assign(name, value, scope)
  }

  state$add_internal_proc <- function(proc) {
    stopifnot(is.list(proc), is_string(proc$name), is_string(proc$code))
    procs <- state$internal_procs %||% list()
    procs[[proc$name]] <- proc
    state$internal_procs <- procs
    invisible(proc)
  }

  scope
}

scope_return_var_names <- function(scope) {
  stopifnot(inherits(scope, "quickr_scope"))
  return_var_names <- closure_return_var_names(scope_closure(scope))
  if (!length(return_var_names)) {
    return(return_var_names)
  }

  is_list_return <- is_call(last(body(scope_closure(scope))), quote(list))
  values <- unname(return_var_names)
  names_in <- names(return_var_names)
  if (is.null(names_in)) {
    names_in <- rep("", length(values))
  }

  expanded_values <- character()
  expanded_names <- character()

  for (i in seq_along(values)) {
    value <- values[[i]]
    name <- names_in[[i]]
    obj <- get0(value, scope, inherits = FALSE)
    if (inherits(obj, SvdResult)) {
      comps <- list(d = obj@d, u = obj@u, v = obj@v)
      comps <- drop_nulls(comps)
      comp_names <- names(comps)
      name_prefix <- if (!is_list_return && identical(name, value)) "" else name
      if (nzchar(name_prefix)) {
        comp_names <- paste0(name_prefix, ".", comp_names)
      }
      expanded_values <- c(
        expanded_values,
        map_chr(comps, \(var) var@name)
      )
      expanded_names <- c(expanded_names, comp_names)
      next
    }

    expanded_values <- c(expanded_values, value)
    expanded_names <- c(expanded_names, name)
  }

  names(expanded_values) <- expanded_names
  expanded_values
}


#' @export
`@.quickr_scope` <- function(x, name) {
  name <- as.character(name)
  scope_get(x, name)
}

#' @export
`@<-.quickr_scope` <- function(x, name, value) {
  name <- as.character(name)
  scope_set(x, name, value)
  x
}

#' @importFrom utils .AtNames findMatches
#' @export
.AtNames.quickr_scope <- function(x, pattern = "") {
  nms <- names(attributes(x)) %||% character()
  st <- attr(x, "state", exact = TRUE)
  if (is.environment(st)) {
    nms <- unique(c(nms, ls(envir = st, all.names = TRUE)))
  }
  findMatches(pattern, nms)
}
