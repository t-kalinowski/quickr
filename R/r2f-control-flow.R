# r2f-control-flow.R
# Handlers for control flow: if, while, repeat, break, next, for

# --- Handlers ---

r2f_handlers[["if"]] <- function(args, scope, ..., hoist = NULL) {
  # cond uses the current hoist context.
  cond <- r2f(args[[1]], scope, ..., hoist = hoist)

  # true and false branchs gets their own hoist target.
  true <- r2f(args[[2]], scope, ..., hoist = NULL)
  check_pending_parallel_consumed(scope)

  if (length(args) == 2) {
    Fortran(glue(
      "
      if ({cond}) then
      {indent(true)}
      end if
      "
    ))
  } else {
    false <- r2f(args[[3]], scope, ..., hoist = NULL)
    check_pending_parallel_consumed(scope)
    Fortran(glue(
      "
      if ({cond}) then
      {indent(true)}
      else
      {indent(false)}
      end if
      "
    ))
  }
}


# TODO: return

# ---- repeat ----
r2f_handlers[["repeat"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  body <- r2f(args[[1]], scope, ...)
  check_pending_parallel_consumed(scope)
  Fortran(glue(
    "do
    {indent(body)}
    end do
    "
  ))
}

# ---- break ----
r2f_handlers[["break"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 0L)
  Fortran("exit")
}

# ---- next ----
r2f_handlers[["next"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 0L)
  Fortran("cycle")
}

# ---- while ----
r2f_handlers[["while"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 2L)
  cond <- r2f(args[[1]], scope, ...)
  body <- r2f(args[[2]], scope, ...) ## should we set a new hoist target here?
  check_pending_parallel_consumed(scope)
  Fortran(glue(
    "do while ({cond})
    {indent(body)}
    end do
    "
  ))
}

# ---- for ----
r2f_handlers[["for"]] <- function(args, scope, ...) {
  .[var, iterable, body] <- args
  stopifnot(is.symbol(var))
  var <- as.character(var)
  existing <- get0(var, scope, inherits = FALSE)
  base_fortran <- fortranize_name(var)
  var_name <- if (inherits(existing, Variable) && !is.null(existing@name)) {
    existing@name
  } else if (scope_is_closure(scope) && inherits(get0(var, scope), Variable)) {
    make_shadow_fortran_name(scope, base_fortran)
  } else {
    base_fortran
  }

  iterable_info <- r2f_unwrap_for_iterable(iterable)
  iterable_unwrapped <- iterable_info$iterable
  iterable_reversed <- isTRUE(iterable_info$reversed)
  parallel <- take_pending_parallel(scope)

  # Value iteration: `for (x in foo) { ... }`
  if (is.symbol(iterable_unwrapped)) {
    iterable_name <- as.character(iterable_unwrapped)
    iterable_var <- get0(iterable_name, scope)
    if (!inherits(iterable_var, Variable)) {
      stop(
        "could not resolve iterable in for(): ",
        iterable_name,
        call. = FALSE
      )
    }
    if (is.null(iterable_var@mode)) {
      stop(
        "could not infer iterable type in for(): ",
        iterable_name,
        call. = FALSE
      )
    }

    if (inherits(existing, Variable) && !passes_as_scalar(existing)) {
      stop(
        "for-loop variable must be scalar when iterating values: ",
        var,
        call. = FALSE
      )
    }

    loop_var <- existing %||% Variable(mode = iterable_var@mode)
    loop_var@name <- var_name
    if (identical(loop_var@mode, "logical") && !inherits(existing, Variable)) {
      loop_var@logical_as_int <- logical_as_int(iterable_var)
    }
    loop_var@modified <- TRUE
    scope[[var]] <- loop_var

    iterable_tmp <- scope@get_unique_var(
      mode = iterable_var@mode,
      dims = iterable_var@dims,
      logical_as_int = logical_as_int(iterable_var)
    )
    iterable_tmp_assign <- glue("{iterable_tmp@name} = {iterable_var@name}")

    idx <- scope@get_unique_var("integer")
    end <- if (passes_as_scalar(iterable_var)) {
      "1_c_int"
    } else {
      glue("size({iterable_tmp@name})")
    }

    element_designator <- if (passes_as_scalar(iterable_var)) {
      iterable_tmp@name
    } else if (iterable_var@rank == 1L) {
      glue("{iterable_tmp@name}({idx@name})")
    } else {
      subs <- linear_subscripts_from_1d(
        iterable_tmp@name,
        iterable_var@rank,
        Fortran(idx@name, idx)
      )
      glue("{iterable_tmp@name}({str_flatten_commas(subs)})")
    }

    element_expr <- element_designator
    if (identical(loop_var@mode, "logical")) {
      element_is_int <- logical_as_int(iterable_tmp)
      target_is_int <- logical_as_int(loop_var)
      if (target_is_int && !element_is_int) {
        element_expr <- glue("merge(1_c_int, 0_c_int, {element_expr})")
      } else if (!target_is_int && element_is_int) {
        element_expr <- glue("({element_expr} /= 0)")
      }
    }

    if (!is.null(parallel)) {
      previous_openmp <- enter_openmp_scope(scope)
      on.exit(exit_openmp_scope(scope, previous_openmp), add = TRUE)
    }
    body <- r2f(body, scope, ...)
    check_pending_parallel_consumed(scope)
    loop_stmts <- str_flatten_lines(glue("{var_name} = {element_expr}"), body)

    loop_header <- if (iterable_reversed) {
      glue("do {idx@name} = {end}, 1_c_int, -1_c_int")
    } else {
      glue("do {idx@name} = 1_c_int, {end}")
    }

    directives <- openmp_directives(parallel, private = var_name)
    if (!is.null(parallel)) {
      mark_openmp_used(scope)
    }
    error_check_after <- if (!is.null(parallel)) {
      quickr_error_return_if_set(
        scope,
        openmp_depth = scope_openmp_depth(scope) - 1L
      )
    } else {
      ""
    }
    return(Fortran(glue(
      "
      {iterable_tmp_assign}
      {str_flatten_lines(directives$prefix, loop_header)}
      {indent(loop_stmts)}
      end do
      {str_flatten_lines(directives$suffix, error_check_after)}
      "
    )))
  }

  # Index iteration: `for (i in 1:n) { ... }`
  loop_var <- Variable(mode = "integer", name = var_name)
  if (iterable_is_singleton_one(iterable_unwrapped, scope)) {
    loop_var@loop_is_singleton <- TRUE
  }
  scope[[var]] <- loop_var

  iterable <- r2f_for_iterable(iterable, scope, ...)
  if (!is.null(parallel)) {
    previous_openmp <- enter_openmp_scope(scope)
    on.exit(exit_openmp_scope(scope, previous_openmp), add = TRUE)
  }
  body <- r2f(body, scope, ...)
  check_pending_parallel_consumed(scope)

  directives <- openmp_directives(parallel)
  if (!is.null(parallel)) {
    mark_openmp_used(scope)
  }
  error_check_after <- if (!is.null(parallel)) {
    quickr_error_return_if_set(
      scope,
      openmp_depth = scope_openmp_depth(scope) - 1L
    )
  } else {
    ""
  }
  loop_header <- glue("do {var_name} = {iterable}")
  Fortran(glue(
    "{str_flatten_lines(directives$prefix, loop_header)}
    {indent(body)}
    end do
    {str_flatten_lines(directives$suffix, error_check_after)}
    "
  ))
}
