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
  used <- unique(c(
    scope_fortran_names(scope),
    scope_generated_fortran_names(scope)
  ))
  if (
    (scope_is_closure(scope) && inherits(get0(name, scope), Variable)) ||
      tolower(base) %in% tolower(used)
  ) {
    make_shadow_fortran_name(scope, base, used = used)
  } else {
    base
  }
}

assignment_is_local_closure_call <- function(rhs, scope) {
  is.call(rhs) &&
    is.symbol(rhs[[1L]]) &&
    inherits(scope[[as.character(rhs[[1L]])]], LocalClosure)
}

materialize_unknown_reassignment_value <- function(target, value, hoist) {
  stopifnot(inherits(target, Variable), inherits(value, Fortran))
  if (
    !inherits(value@value, Variable) ||
      is.null(value@value@dims) ||
      !any(vapply(value@value@dims, is_scalar_na, logical(1L))) ||
      !is.null(value@value@name) ||
      (!target@is_external && has_self_size_dims(target))
  ) {
    return(value)
  }
  materialize_via_hoist(
    value,
    mode = value@value@mode,
    dims = value@value@dims,
    hoist = hoist,
    logical_storage = logical_as_int(value@value)
  )
}

assignment_expression_has_constructor <- function(e) {
  e <- unwrap_parens(e)
  if (!is.call(e)) {
    return(FALSE)
  }
  callable <- unwrap_parens(e[[1L]])
  if (is.symbol(callable)) {
    name <- as.character(callable)
    if (
      name %in%
        c(
          "logical",
          "integer",
          "double",
          "numeric",
          "matrix",
          "array"
        )
    ) {
      return(TRUE)
    }
    if (identical(name, "function")) {
      return(FALSE)
    }
  }
  children <- as.list(e)[-1L]
  for (i in seq_along(children)) {
    if (is_missing(children[[i]])) {
      next
    }
    if (assignment_expression_has_constructor(children[[i]])) {
      return(TRUE)
    }
  }
  FALSE
}

allocate_new_guarded_constructor_local_at_point <- function(
  name,
  var,
  scope,
  hoist
) {
  stopifnot(
    is_string(name),
    inherits(var, Variable),
    inherits(scope, "quickr_scope"),
    is.environment(hoist)
  )
  if (
    !scope_kind(scope) %in% c("subroutine", "closure") ||
      name %in%
        (scope_get(scope, "return_names", character()) %||%
          character()) ||
      (var@r_name %||% var@name) %in% names(formals(scope_closure(scope))) ||
      !subroutine_local_allocatable(var, scope) ||
      !hoist$contains_runtime_guard()
  ) {
    return(invisible(var))
  }
  point_allocated <- scope_get(
    scope,
    "point_allocated_local_names",
    character()
  )
  scope_set(
    scope,
    "point_allocated_local_names",
    unique(c(point_allocated, var@name))
  )
  hoist$emit(glue("allocate({var@name}({dims2f(var@dims, scope)}))"))
  invisible(var)
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
        environment(scope_closure(scope)),
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
        r2f(
          rhs,
          scope,
          ...,
          hoist = hoist,
          dest = var,
          assignment_name = name
        )
      } else {
        r2f(rhs, scope, ..., hoist = hoist, assignment_name = name)
      }
    } else if (inherits(inferred_var, Variable)) {
      var <- inferred_var
      var@r_name <- name
      var@name <- fortran_name
      return_names <- scope_get(scope, "return_names", character()) %||%
        character()
      if (name %in% return_names) {
        var@is_return <- TRUE
        if (identical(var@mode, "logical")) {
          var@logical_as_int <- TRUE
        }
      }
      value <- r2f(
        rhs,
        scope,
        ...,
        hoist = hoist,
        dest = var,
        assignment_name = name
      )
    } else {
      value <- r2f(rhs, scope, ..., hoist = hoist, assignment_name = name)
    }

    guarded_constructor <- assignment_expression_has_constructor(rhs_unwrapped)
    initialized_local_names <- scope_get(
      scope,
      "initialized_local_names",
      character()
    )

    # immutable / copy-on-modify usage of Variable()
    if (!existing_binding) {
      # The var does not exist -> this is a binding to a new symbol
      # Create a fresh Variable carrying only mode/dims and a new name.
      if (inherits(value, Fortran) && is.null(value@value)) {
        stop(
          "cannot assign `",
          deparse1(rhs),
          "`: expression does not produce a value",
          call. = FALSE
        )
      }
      if (!inherits(var, Variable)) {
        src <- value@value
        var <- Variable(mode = src@mode, dims = src@dims)
      }
      if (
        inherits(value, Fortran) &&
          inherits(value@value, Variable) &&
          identical(value@value@mode, "logical") &&
          logical_as_int(value@value) &&
          !isTRUE(value@logical_booleanized)
      ) {
        # Keep bind(c) logicals as integer storage when the RHS is an
        # integer-backed expression (e.g. rev(x) for external logicals).
        var@logical_as_int <- TRUE
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
      register_openmp_private(scope, var@name)
    } else {
      # The var already exists, this assignment is a modification / reassignment
      if (is.null(var@r_name)) {
        var@r_name <- name
      }
      if (
        is.null(var@mode) &&
          inherits(value@value, Variable) &&
          !is.null(value@value@mode)
      ) {
        var@mode <- value@value@mode
        var@dims <- value@value@dims
      }
      check_reassignment_narrowing(name, var, value@value)
      value <- materialize_unknown_reassignment_value(var, value, hoist)
      check_assignment_compatible(
        name,
        var,
        value@value,
        hoist = hoist,
        scope = scope
      )
      var@modified <- TRUE
      # Subsequent size expressions must not reuse the expression that
      # initialized this binding. Preserve a new expression when it does not
      # refer back to the binding itself; self-referential updates cannot be
      # folded safely into a result shape.
      var@r <- if (name %in% all.vars(rhs)) NA_integer_ else rhs
      # could probably drop this @modified property, and instead track
      # if the var populated by declare is identical at the end (e.g., perhaps by
      # address, or by attaching a unique id to each var, or ???)
      assign(name, var, scope)
    }

    if (
      !var@name %in% initialized_local_names &&
        guarded_constructor &&
        any(
          !vapply(
            var@dims,
            size_expr_is_known_nonnegative,
            logical(1L)
          )
        ) &&
        (!inherits(value, Fortran) || !isTRUE(value@writes_to_dest))
    ) {
      allocate_new_guarded_constructor_local_at_point(
        name,
        var,
        scope,
        hoist
      )
    }
    scope_set(
      scope,
      "initialized_local_names",
      unique(c(initialized_local_names, var@name))
    )

    # If child consumed destination (e.g., BLAS wrote directly into LHS), skip assignment
    if (inherits(value, Fortran) && isTRUE(value@writes_to_dest)) {
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

    # Subassignment cannot re-type the base variable any more than
    # whole-variable reassignment can: `x[1L] <- 2.5` on an integer `x`
    # would silently truncate where R promotes `x` to double.
    base_name <- as.character(target_call[[2L]])
    check_reassignment_narrowing(
      base_name,
      get0(base_name, scope),
      value@value,
      whole_binding = FALSE
    )

    Fortran(str_flatten_lines(lhs$pre, glue("{lhs$lhs} = {value}")))
  }
)

# Validate and resolve the target of a superassignment (`x <<- v`,
# `x[i] <<- v`) to its host-scope Variable: the name must not shadow a
# closure formal or the closure's output variable, and must already exist
# in the enclosing quick() scope. Marks the host variable modified.
# Used by: `<<-`, `[<<-`, compile_subscript_lhs() (r2f-closures.R)
resolve_superassign_target <- function(name, scope) {
  formal_names <- names(formals(scope_closure(scope))) %||% character()
  if (name %in% formal_names) {
    stop("<<- targets must not shadow closure formals: ", name)
  }

  forbidden <- scope_forbid_superassign(scope)
  if (name %in% forbidden) {
    stop("closure must not superassign to its output variable: ", name)
  }

  host_scope <- scope_host_scope(scope) %||%
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
  host_var
}

register_r2f_handler(
  "<<-",
  function(args, scope, ..., hoist = NULL) {
    if (is.null(scope) || !identical(scope_kind(scope), "closure")) {
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

    host_var <- resolve_superassign_target(name, scope)

    value <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_reassignment_narrowing(name, host_var, value@value)
    value <- materialize_unknown_reassignment_value(host_var, value, hoist)
    check_assignment_compatible(
      name,
      host_var,
      value@value,
      hoist = hoist,
      scope = scope
    )

    Fortran(glue("{host_var@name} = {value}"))
  }
)

register_r2f_handler(
  "[<<-",
  function(args, scope, ..., hoist = NULL) {
    if (is.null(scope) || !identical(scope_kind(scope), "closure")) {
      stop("<<- is only supported inside local closures")
    }

    stopifnot(is_call(target <- args[[1L]], "["))
    subset_call <- target

    base <- subset_call[[2L]]
    if (!is.symbol(base)) {
      stop("only superassignment to x[...] is supported")
    }
    name <- as.character(base)

    host_var <- resolve_superassign_target(name, scope)

    lhs <- compile_subscript_lhs(
      subset_call,
      scope,
      ...,
      hoist = hoist,
      target = "host"
    )
    value <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_reassignment_narrowing(
      name,
      host_var,
      value@value,
      whole_binding = FALSE
    )
    Fortran(glue("{lhs$lhs} = {value}"))
  }
)

register_r2f_handler("=", r2f_handlers[["<-"]])
