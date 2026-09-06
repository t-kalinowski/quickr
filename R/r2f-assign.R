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
      closure <- as_local_closure(
        rhs,
        environment(scope_closure(scope)),
        name = name
      )
      # Nested R scopes share the root Fortran CONTAINS section. Give their
      # procedures distinct names even when the R bindings shadow each other.
      closure@proc_name <- if (scope_is_closure(scope)) {
        scope_unique_proc(scope_root(scope), prefix = "nested_closure")
      } else {
        name
      }
      scope[[name]] <- closure
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
      value <- r2f(rhs, scope, ..., hoist = hoist, dest = var)
    } else {
      value <- r2f(rhs, scope, ..., hoist = hoist)
    }

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
        var <- Variable(
          mode = src@mode,
          dims = src@dims,
          c_bridge_dim_checks = src@c_bridge_dim_checks
        )
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
      check_assignment_compatible(var, value@value)
      var@modified <- TRUE
      # could probably drop this @modified property, and instead track
      # if the var populated by declare is identical at the end (e.g., perhaps by
      # address, or by attaching a unique id to each var, or ???)
      assign(name, var, scope)
    }

    initialized_local_names <- scope_get(
      scope,
      "initialized_local_names",
      character()
    )
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
    check_reassignment_narrowing(base_name, get0(base_name, scope), value@value)

    Fortran(str_flatten_lines(lhs$pre, glue("{lhs$lhs} = {value}")))
  }
)

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

    value <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_reassignment_narrowing(name, host_var, value@value)
    check_assignment_compatible(host_var, value@value)

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

    lhs <- compile_subscript_lhs(
      subset_call,
      scope,
      ...,
      hoist = hoist,
      target = "host"
    )
    value <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_reassignment_narrowing(name, host_var, value@value)
    Fortran(glue("{lhs$lhs} = {value}"))
  }
)

register_r2f_handler("=", r2f_handlers[["<-"]])

# A Fortran declaration does not establish an R binding. Check source-level
# control flow before accepting reads of locals, including the final return.
check_definite_assignment <- function(closure, scope, captured = character()) {
  locals <- character()
  # Closure bindings are static and unique within this lexical scope, so build
  # their registry once before walking control flow.
  # Fortran host association makes captures readable regardless of the R
  # control flow that created them, so check them wherever a closure is reached.
  closures <- list()
  collect <- function(expr) {
    if (is_missing(expr) || !is.call(expr) || is_function_call(expr)) {
      return(invisible(NULL))
    }
    if (
      (is_call(expr, "<-") || is_call(expr, "=")) &&
        length(expr) == 3L &&
        is.symbol(expr[[2L]])
    ) {
      name <- as.character(expr[[2L]])
      locals <<- union(locals, name)
      if (is_function_call(expr[[3L]])) {
        closures[[name]] <<- list(
          definition = expr[[3L]],
          captures = closure_free_names(expr[[3L]])
        )
      }
    }
    if (is_call(expr, "for") && length(expr) == 4L) {
      locals <<- union(locals, as.character(expr[[2L]]))
    }
    if (!is_call(expr, "declare")) {
      lapply(as.list(expr)[-1L], collect)
    }
    invisible(NULL)
  }
  collect(body(closure))

  # NULL denotes a path that cannot reach the following statement.
  join <- function(left, right) {
    if (is.null(left)) {
      return(right)
    }
    if (is.null(right)) {
      return(left)
    }
    intersect(left, right)
  }
  require_assigned <- function(name, assigned) {
    if (name %in% locals && !name %in% assigned) {
      stop(
        "local variable `",
        name,
        "` may be uninitialized; assign it before use on every path",
        call. = FALSE
      )
    }
    invisible(NULL)
  }
  read <- function(
    name,
    assigned,
    seen = character(),
    captures = closures[[name]]$captures %||% character()
  ) {
    require_assigned(name, assigned)
    # Follow closure dependencies at this use point, checking each cycle only
    # once while still visiting its remaining captures.
    if (name %in% seen) {
      return(invisible(NULL))
    }
    for (capture in captures) {
      read(capture, assigned, union(seen, name))
    }
    invisible(NULL)
  }
  walk <- function(expr, assigned) {
    if (is.null(assigned) || is_missing(expr)) {
      return(assigned)
    }
    if (is.symbol(expr)) {
      read(as.character(expr), assigned)
      return(assigned)
    }
    if (!is.call(expr)) {
      return(assigned)
    }
    if (is_function_call(expr)) {
      # An anonymous closure is reached where it appears, e.g. as a sapply()
      # argument, so its captures must be initialized by that point.
      for (capture in closure_free_names(expr)) {
        read(capture, assigned)
      }
      return(assigned)
    }
    if (is_call(expr, "declare")) {
      return(assigned)
    }
    if ((is_call(expr, "<-") || is_call(expr, "=")) && length(expr) == 3L) {
      if (is.symbol(expr[[2L]]) && is_function_call(expr[[3L]])) {
        # Defining a closure establishes its binding but reads no captures yet.
        return(union(assigned, as.character(expr[[2L]])))
      }
      assigned <- walk(expr[[3L]], assigned)
      if (is.null(assigned)) {
        return(NULL)
      }
      if (is.symbol(expr[[2L]])) {
        return(union(assigned, as.character(expr[[2L]])))
      }
      return(walk(expr[[2L]], assigned))
    }
    if (is_call(expr, "if") && length(expr) %in% c(3L, 4L)) {
      assigned <- walk(expr[[2L]], assigned)
      yes <- walk(expr[[3L]], assigned)
      no <- if (length(expr) == 4L) walk(expr[[4L]], assigned) else assigned
      return(join(yes, no))
    }
    if (is_call(expr, "for") && length(expr) == 4L) {
      assigned <- walk(expr[[3L]], assigned)
      iterator <- as.character(expr[[2L]])
      after <- walk(expr[[4L]], union(assigned, iterator))
      iterable <- unwrap_parens(expr[[3L]])
      nonempty <- is_call(iterable, "seq_len") &&
        length(iterable) == 2L &&
        is_scalar_integerish(iterable[[2L]]) &&
        iterable[[2L]] > 0L
      if (is.symbol(iterable)) {
        var <- get0(as.character(iterable), scope)
        nonempty <- inherits(var, Variable) &&
          all(vapply(var@dims, is_scalar_integerish, logical(1L))) &&
          all(unlist(var@dims) > 0L)
        if (nonempty) assigned <- union(assigned, iterator)
      }
      if (nonempty && !any(all.names(expr[[4L]]) %in% c("break", "next"))) {
        return(union(assigned, setdiff(after, iterator)))
      }
      # The iterable may be empty; neither its variable nor body assignments
      # establish bindings after the loop.
      return(assigned)
    }
    if (is_call(expr, "while") && length(expr) == 3L) {
      assigned <- walk(expr[[2L]], assigned)
      walk(expr[[3L]], assigned)
      return(assigned)
    }
    if (is_call(expr, "repeat") && length(expr) == 2L) {
      walk(expr[[2L]], assigned)
      # Conservatively require initialization before loops, including repeat:
      # an earlier break/next can bypass an assignment in the body.
      return(assigned)
    }
    if (is_call(expr, "break") || is_call(expr, "next")) {
      return(NULL)
    }
    if ((is_call(expr, "&&") || is_call(expr, "||")) && length(expr) == 3L) {
      assigned <- walk(expr[[2L]], assigned)
      return(join(assigned, walk(expr[[3L]], assigned)))
    }
    # The callee may be parenthesized, as `maybe_lower_local_closure_call()`
    # also allows, and is not visited by the argument walk below.
    callee <- unwrap_parens(expr[[1L]])
    if (is.symbol(callee)) {
      name <- as.character(callee)
      definition <- closures[[name]]$definition
      if (is.null(definition)) {
        read(name, assigned)
      } else {
        read(
          name,
          assigned,
          captures = closure_call_free_names(definition, expr)
        )
      }
    } else if (is_function_call(callee)) {
      for (capture in closure_call_free_names(callee, expr)) {
        read(capture, assigned)
      }
    } else {
      assigned <- walk(callee, assigned)
      if (is.null(assigned)) {
        return(NULL)
      }
    }
    for (arg in as.list(expr)[-1L]) {
      assigned <- walk(arg, assigned)
    }
    assigned
  }
  walk(body(closure), union(names(formals(closure)), captured))
  invisible(NULL)
}
