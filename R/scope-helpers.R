# Scope helpers.
#
# quickr scopes are environments with reference semantics. Historically, we
# stored a lot of mutable state in ad-hoc env attributes and accessed it via a
# custom `@.quickr_scope` method, which can be confused with S7 `@` property
# access. These helpers make scope state access explicit and grep-able.

scope_get <- function(scope, name, default = NULL) {
  stopifnot(inherits(scope, "quickr_scope"), is_string(name))
  val <- attr(scope, name, exact = TRUE)
  if (is.null(val)) default else val
}

scope_set <- function(scope, name, value) {
  stopifnot(inherits(scope, "quickr_scope"), is_string(name))
  attr(scope, name) <- value
  invisible(scope)
}

scope_kind <- function(scope) {
  scope_get(scope, "kind")
}

scope_closure <- function(scope) {
  scope_get(scope, "closure")
}

scope_uses_rng <- function(scope) {
  isTRUE(scope_get(scope, "uses_rng", FALSE))
}

scope_mark_uses_rng <- function(scope) {
  scope_set(scope, "uses_rng", TRUE)
  invisible(TRUE)
}

scope_uses_errors_flag <- function(scope) {
  isTRUE(scope_get(scope, "uses_errors", FALSE))
}

scope_mark_uses_errors_flag <- function(scope) {
  scope_set(scope, "uses_errors", TRUE)
  invisible(TRUE)
}

scope_uses_openmp_flag <- function(scope) {
  isTRUE(scope_get(scope, "uses_openmp", FALSE))
}

scope_mark_uses_openmp_flag <- function(scope) {
  scope_set(scope, "uses_openmp", TRUE)
  invisible(TRUE)
}

scope_forbid_superassign <- function(scope) {
  scope_get(scope, "forbid_superassign", character())
}

scope_host_scope <- function(scope) {
  scope_get(scope, "host_scope")
}

scope_new_child <- function(scope, kind) {
  stopifnot(inherits(scope, "quickr_scope"), is_string(kind))
  f <- scope_get(scope, "new_child")
  stopifnot(is.function(f))
  f(kind)
}

scope_unique_var <- function(scope, ...) {
  f <- scope_get(scope, "get_unique_var")
  stopifnot(is.function(f))
  f(...)
}

scope_unique_proc <- function(scope, prefix = "closure") {
  stopifnot(is_string(prefix))
  f <- scope_get(scope, "get_unique_proc")
  stopifnot(is.function(f))
  f(prefix = prefix)
}

scope_add_internal_proc <- function(scope, proc) {
  f <- scope_get(scope, "add_internal_proc")
  stopifnot(is.function(f))
  f(proc)
}
