# Assignment-related r2f handlers and helpers

assignment_dispatch_call_target <- function(
  target,
  args,
  scope,
  ...,
  hoist,
  assign_op
) {
  if (!is.call(target)) {
    return(NULL)
  }
  target_callable <- target[[1L]]
  stopifnot(is.symbol(target_callable))
  name <- as.symbol(paste0(as.character(target_callable), assign_op))
  handler <- get_r2f_handler(name)
  handler(args, scope, ..., hoist = hoist)
}

assignment_extract_fallthrough <- function(rhs) {
  rhs_unwrapped <- rhs
  while (is_call(rhs_unwrapped, "(") && length(rhs_unwrapped) == 2L) {
    rhs_unwrapped <- rhs_unwrapped[[2L]]
  }
  if (
    (is_call(rhs_unwrapped, "<-") || is_call(rhs_unwrapped, "=")) &&
      length(rhs_unwrapped) == 3L &&
      is.symbol(rhs_unwrapped[[2L]])
  ) {
    return(list(
      target = rhs_unwrapped[[2L]],
      rhs = rhs_unwrapped[[3L]]
    ))
  }
  NULL
}

assignment_fortran_name <- function(name, scope) {
  stopifnot(is_string(name))
  base <- fortranize_name(name)
  if (scope_is_closure(scope) && inherits(get0(name, scope), Variable)) {
    make_shadow_fortran_name(scope, base)
  } else {
    base
  }
}

assignment_is_local_closure_call <- function(rhs, scope) {
  is.call(rhs) &&
    is.symbol(rhs[[1L]]) &&
    inherits(scope[[as.character(rhs[[1L]])]], LocalClosure)
}

register_r2f_handler(
  "<-",
  function(args, scope, ..., hoist = NULL) {
    target <- args[[1L]]
    if (
      !is.null(
        out <- assignment_dispatch_call_target(
          target,
          args,
          scope,
          ...,
          hoist = hoist,
          assign_op = "<-"
        )
      )
    ) {
      return(out)
    }

    # It sure seems like it's be nice if the Fortran() constructor
    # took mode and dims as args directly,
    # without needing to go through Variable...
    stopifnot(is.symbol(target))
    name <- as.character(target)

    rhs <- args[[2L]]

    # Fall-through assignment: `a <- b <- expr` (or `a <- (b <- expr)`).
    # R evaluates this right-to-left and returns the assigned value, i.e.
    # `a <- (b <- expr)` is equivalent to `b <- expr; a <- b`.
    if (!is.null(fallthrough <- assignment_extract_fallthrough(rhs))) {
      inner_stmt <- r2f(
        call("<-", fallthrough$target, fallthrough$rhs),
        scope,
        ...,
        hoist = hoist
      )
      outer_stmt <- r2f(
        call("<-", target, fallthrough$target),
        scope,
        ...,
        hoist = hoist
      )
      return(Fortran(str_flatten_lines(inner_stmt, outer_stmt)))
    }

    # Local closure definition: `f <- function(i) ...`
    if (is_function_call(rhs)) {
      scope[[name]] <- as_local_closure(
        rhs,
        environment(scope@closure),
        name = name
      )
      return(Fortran(""))
    }

    # Local closure call: `x <- f(...)` where `f <- function(...) ...` in scope.
    if (assignment_is_local_closure_call(rhs, scope)) {
      return(compile_closure_call_assignment(
        name,
        rhs,
        scope,
        ...,
        hoist = hoist
      ))
    }

    # Targeted higher-order lowering: `out <- sapply(seq_along(x), f)`
    if (is_sapply_call(rhs)) {
      parallel <- take_pending_parallel(scope)
      return(
        compile_sapply_assignment(
          name,
          rhs,
          scope,
          ...,
          hoist = hoist,
          parallel = parallel
        )
      )
    }

    rhs_unwrapped <- unwrap_parens(rhs)
    if (is_call(rhs_unwrapped, "svd")) {
      return(compile_svd_assignment(
        name,
        rhs_unwrapped,
        scope,
        ...,
        hoist = hoist
      ))
    }

    dest_allowed <- dest_supported_for_call(rhs)

    # If target already exists (declared), thread destination hint to a single BLAS-capable child
    var <- get0(name, scope, inherits = FALSE)
    existing_binding <- !is.null(var) && inherits(var, Variable)
    inferred_var <- NULL
    fortran_name <- NULL
    if (!existing_binding && dest_allowed) {
      inferred_var <- dest_infer_for_call(rhs, scope)
      fortran_name <- assignment_fortran_name(name, scope)
    }

    if (existing_binding) {
      value <- if (dest_allowed) {
        r2f(rhs, scope, ..., hoist = hoist, dest = var)
      } else {
        r2f(rhs, scope, ..., hoist = hoist)
      }
    } else if (inherits(inferred_var, Variable)) {
      var <- inferred_var
      var@name <- fortran_name
      value <- r2f(rhs, scope, ..., hoist = hoist, dest = var)
    } else {
      value <- r2f(rhs, scope, ..., hoist = hoist)
    }

    # immutable / copy-on-modify usage of Variable()
    if (!existing_binding) {
      # The var does not exist -> this is a binding to a new symbol
      # Create a fresh Variable carrying only mode/dims and a new name.
      if (!inherits(var, Variable)) {
        src <- value@value
        var <- Variable(mode = src@mode, dims = src@dims)
      }
      if (is.null(fortran_name)) {
        fortran_name <- assignment_fortran_name(name, scope)
      }
      var@r_name <- name
      var@name <- fortran_name
      # keep a reference to the R expression assigned, if available
      tryCatch(
        var@r <- attr(value, "r", TRUE),
        error = function(e) NULL
      )
      scope[[name]] <- var
    } else {
      # The var already exists, this assignment is a modification / reassignment
      check_assignment_compatible(var, value@value)
      var@modified <- TRUE
      # could probably drop this @modified property, and instead track
      # if the var populated by declare is identical at the end (e.g., perhaps by
      # address, or by attaching a unique id to each var, or ???)
      assign(name, var, scope)
    }

    # If child consumed destination (e.g., BLAS wrote directly into LHS), skip assignment
    if (isTRUE(attr(value, "writes_to_dest", TRUE))) {
      Fortran("")
    } else {
      Fortran(glue("{var@name} = {value}"))
    }
  }
)

register_r2f_handler(
  "[<-",
  function(args, scope = NULL, ...) {
    # TODO: handle logical subsetting here, which must become a where a construct like:
    #   x[lgl] <- val
    # becomes
    # where (lgl)
    #   x = val
    # end where
    # ! but if {va} references {x}, it will only see the subset x, not the full {x}
    # e.g.,
    # sum(x) is not the same as `where lgl \n sum(x) \n end where`
    # ditto for ifelse() ?
    # e <- as.list(e)

    stopifnot(is_call(target_call <- args[[1L]], "["))

    lhs <- compile_subscript_lhs(target_call, scope, ..., target = "local")
    value <- r2f(args[[2L]], scope, ...)

    Fortran(str_flatten_lines(lhs$pre, glue("{lhs$lhs} = {value}")))
  }
)

register_r2f_handler(
  "<<-",
  function(args, scope, ..., hoist = NULL) {
    if (is.null(scope) || !identical(scope@kind, "closure")) {
      stop("<<- is only supported inside local closures")
    }

    target <- args[[1L]]
    if (
      !is.null(
        out <- assignment_dispatch_call_target(
          target,
          args,
          scope,
          ...,
          hoist = hoist,
          assign_op = "<<-"
        )
      )
    ) {
      return(out)
    }

    stopifnot(is.symbol(target))
    name <- as.character(target)

    formal_names <- names(formals(scope@closure)) %||% character()
    if (name %in% formal_names) {
      stop("<<- targets must not shadow closure formals: ", name)
    }

    forbidden <- attr(scope, "forbid_superassign", exact = TRUE) %||%
      character()
    if (name %in% forbidden) {
      stop("closure must not superassign to its output variable: ", name)
    }

    host_scope <- scope@host_scope %||%
      stop("internal error: missing host scope")
    host_var <- get0(name, host_scope)
    if (!inherits(host_var, Variable)) {
      stop(
        "<<- targets must resolve to an existing variable in the enclosing quick() scope: ",
        name
      )
    }

    host_var@modified <- TRUE
    host_scope[[name]] <- host_var

    value <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_assignment_compatible(host_var, value@value)

    Fortran(glue("{host_var@name} = {value}"))
  }
)

register_r2f_handler(
  "[<<-",
  function(args, scope, ..., hoist = NULL) {
    if (is.null(scope) || !identical(scope@kind, "closure")) {
      stop("<<- is only supported inside local closures")
    }

    stopifnot(is_call(target <- args[[1L]], "["))
    subset_call <- target

    base <- subset_call[[2L]]
    if (!is.symbol(base)) {
      stop("only superassignment to x[...] is supported")
    }
    name <- as.character(base)

    formal_names <- names(formals(scope@closure)) %||% character()
    if (name %in% formal_names) {
      stop("<<- targets must not shadow closure formals: ", name)
    }

    forbidden <- attr(scope, "forbid_superassign", exact = TRUE) %||%
      character()
    if (name %in% forbidden) {
      stop("closure must not superassign to its output variable: ", name)
    }

    host_scope <- scope@host_scope %||%
      stop("internal error: missing host scope")
    host_var <- get0(name, host_scope)
    if (!inherits(host_var, Variable)) {
      stop(
        "<<- targets must resolve to an existing variable in the enclosing quick() scope: ",
        name
      )
    }

    host_var@modified <- TRUE
    host_scope[[name]] <- host_var

    lhs <- compile_subscript_lhs(
      subset_call,
      scope,
      ...,
      hoist = hoist,
      target = "host"
    )
    value <- r2f(args[[2L]], scope, ..., hoist = hoist)
    Fortran(glue("{lhs$lhs} = {value}"))
  }
)

register_r2f_handler("=", r2f_handlers[["<-"]])
