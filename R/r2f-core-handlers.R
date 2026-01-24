# r2f-core-handlers.R
# Handlers for core language constructs: declare, Fortran, (, {

# --- Handlers ---

r2f_handlers[["declare"]] <- function(args, scope, ...) {
  for (a in args) {
    if (is_missing(a)) {
      next
    }
    if (is_parallel_decl_call(a)) {
      if (has_pending_parallel(scope)) {
        stop("parallel()/omp() declaration already pending.", call. = FALSE)
      }
      set_pending_parallel(scope, parse_parallel_decl(a))
    } else if (is_type_call(a)) {
      var <- type_call_to_var(a)
      var@is_arg <- var@name %in% names(formals(scope@closure))
      if (identical(var@mode, "logical") && isTRUE(var@is_arg)) {
        var@logical_as_int <- TRUE
      }
      scope[[var@name]] <- var
    } else if (is_call(a, quote(`{`))) {
      Recall(as.list(a)[-1], scope)
    }
  }

  Fortran("")
}


r2f_handlers[["Fortran"]] <- function(args, scope = NULL, ...) {
  if (!is_string(args[[1]])) {
    stop("Fortran() must be called with a string")
  }
  Fortran(args[[1]])
  # enable passing through literal fortran code
  # used like:
  #   Fortran("nearest(x, 1)", double(length(x)))
  #   Fortran("nearest(x, 1)", x)
  #   Fortran("x = nearest(x, 1)")
}

r2f_handlers[["("]] <- function(args, scope, ...) {
  x <- r2f(args[[1L]], scope, ...)
  Fortran(glue("({x})"), x@value)
}

r2f_handlers[["{"]] <- function(args, scope, ..., hoist = NULL) {
  # every top level R-expr / fortran statement gets its own hoist target.
  x <- vector("list", length(args))
  for (i in seq_along(args)) {
    stmt <- args[[i]]
    check_pending_parallel_target(stmt, scope)
    x[[i]] <- r2f(stmt, scope, ...)
  }
  check_pending_parallel_consumed(scope)
  code <- str_flatten_lines(x)

  # browser()
  value <- (if (length(args)) last(x)@value) %||% Variable()
  Fortran(code, value)
}
