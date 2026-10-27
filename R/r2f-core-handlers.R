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
      r_name <- var@r_name %||% var@name
      var@is_arg <- r_name %in% names(formals(scope_closure(scope)))
      if (identical(var@mode, "logical") && isTRUE(var@is_arg)) {
        var@logical_as_int <- TRUE
      }
      scope[[r_name]] <- var
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

r2f_handlers[["stop"]] <- function(args, scope = NULL, ...) {
  if (!length(args)) {
    msg <- "Execution halted"
  } else {
    if (
      length(args) != 1L ||
        !is.character(args[[1L]]) ||
        length(args[[1L]]) != 1L
    ) {
      stop(
        "stop() only supports a single string literal message",
        call. = FALSE
      )
    }
    msg <- args[[1L]]
  }

  mark_scope_uses_errors(scope)
  Fortran(str_flatten_lines(quickr_error_fortran_lines(msg, scope = scope)))
}

r2f_handlers[["("]] <- function(args, scope, ...) {
  x <- r2f(args[[1L]], scope, ...)
  Fortran(glue("({x})"), x@value)
}

r2f_handlers[["$"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 2L)
  base <- args[[1L]]
  field <- args[[2L]]
  field_name <- if (is.symbol(field)) {
    as.character(field)
  } else if (is_string(field)) {
    field
  } else {
    stop("`$` expects a symbol or string field name", call. = FALSE)
  }

  if (is.symbol(base) && identical(as.character(base), ".Machine")) {
    if (identical(field_name, "double.eps")) {
      return(Fortran("epsilon(1.0_c_double)", Variable("double")))
    }
    stop("`.Machine$", field_name, "` is not supported", call. = FALSE)
  }

  if (field_name %in% c("d", "u", "v")) {
    if (is.symbol(base)) {
      base_name <- as.character(base)
      base_val <- if (is.null(scope)) NULL else get0(base_name, scope)
      if (inherits(base_val, SvdResult)) {
        out_var <- switch(
          field_name,
          d = base_val@d,
          u = base_val@u,
          v = base_val@v
        )
        if (is.null(out_var)) {
          stop(
            "svd() result does not include `$",
            field_name,
            "`",
            call. = FALSE
          )
        }
        return(Fortran(out_var@name, out_var))
      }
    }
    if (is_call(base, "svd")) {
      return(svd_component_from_call(
        base,
        field_name,
        scope,
        ...,
        hoist = hoist
      ))
    }
  }

  stop(
    "`$` only supports `.Machine$double.eps` and `svd()` results ($d, $u, $v)",
    call. = FALSE
  )
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
