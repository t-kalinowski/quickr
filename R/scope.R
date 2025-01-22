

new_ordered_env <- function(parent = emptyenv()) {
  env <- new.env(parent = parent)
  class(env) <- "quickr_ordered_env"
  env
}

#' @export
`[[<-.quickr_ordered_env` <- function(x, name, value) {
  attr(x, "ordered_names") <- unique(c(attr(x, "ordered_names", TRUE), name))
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
  if (is.null(value)) return()
  stopifnot(exprs = {
    inherits(target, Variable)
    inherits(value, Variable)
    passes_as_scalar(target) || passes_as_scalar(value) || target@rank == value@rank
  })
}

new_scope <- function(closure, parent = emptyenv()) {
  scope <- new_ordered_env(parent = parent)
  class(scope) <- unique(c("quickr_scope", class(scope)))
  attr(scope, "closure") <- closure


  attr(scope, "get_unique_var") <- local({
    i <- 0L
    function(...) {
      name <- paste0("tmp", i <<- i + 1L, "_")
      (scope[[name]] <- Variable(..., name = name))
    }
  })
  attr(scope, "assign") <- function(name, value) {
    stopifnot(inherits(value, Variable), is.symbol(name) || is_string(name))
    name <- as.character(name)
    if (exists(name, scope))
      check_assignment_compatible(get(name, scope), value)
    value@name <- name
    assign(name, value, scope)
  }
  scope
}


#' @export
`@.quickr_scope` <- function(x, name) attr(x, name, exact = TRUE)

#' @export
`@<-.quickr_scope` <- function(x, name, value) `attr<-`(x, name, value = value)

#' @importFrom utils .AtNames findMatches
#' @export
.AtNames.quickr_scope <- function(x, pattern = "")
  findMatches(pattern, names(attributes(x)))

