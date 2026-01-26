# Local closures + higher-order lowering helpers

maybe_lower_local_closure_call <- function(
  e,
  scope,
  ...,
  hoist,
  needs_value
) {
  stopifnot(
    is.call(e),
    inherits(scope, "quickr_scope"),
    inherits(hoist, "environment"),
    is_bool(needs_value)
  )

  callable_unwrapped <- e[[1L]]
  while (
    is_call(callable_unwrapped, quote(`(`)) &&
      length(callable_unwrapped) == 2L
  ) {
    callable_unwrapped <- callable_unwrapped[[2L]]
  }

  if (is.symbol(callable_unwrapped)) {
    callable_name <- as.character(callable_unwrapped)
    closure_obj <- scope[[callable_name]]
    if (!inherits(closure_obj, LocalClosure)) {
      return(NULL)
    }

    call_expr <- as.call(c(list(callable_unwrapped), as.list(e)[-1L]))
    return(compile_closure_call(
      call_expr = call_expr,
      closure_obj = closure_obj,
      proc_name = callable_name,
      scope = scope,
      ...,
      hoist = hoist,
      needs_value = needs_value
    ))
  }

  if (is_function_call(callable_unwrapped)) {
    proc_name <- scope_root(scope)@get_unique_proc(prefix = "closure")
    host_closure <- scope_root(scope)@closure
    closure_obj <- as_local_closure(
      callable_unwrapped,
      env = environment(host_closure),
      name = proc_name
    )
    call_expr <- as.call(c(list(callable_unwrapped), as.list(e)[-1L]))
    return(compile_closure_call(
      call_expr = call_expr,
      closure_obj = closure_obj,
      proc_name = proc_name,
      scope = scope,
      ...,
      hoist = hoist,
      needs_value = needs_value
    ))
  }

  NULL
}


is_function_call <- function(x) {
  is.call(x) && identical(x[[1L]], as.symbol("function"))
}

is_sapply_call <- function(x) is.call(x) && identical(x[[1L]], quote(sapply))


new_local_closure <- function(fun, name = NULL) {
  stopifnot(is.function(fun), is.null(name) || is_string(name))
  LocalClosure(fun = fun, name = name)
}

as_local_closure <- function(fun_expr, env, name = NULL) {
  stopifnot(is_function_call(fun_expr))
  new_local_closure(eval(fun_expr, envir = env), name = name)
}

scope_root <- function(scope) {
  stopifnot(inherits(scope, "quickr_scope"))
  while (
    !identical(scope@kind, "subroutine") &&
      inherits(parent.env(scope), "quickr_scope")
  ) {
    scope <- parent.env(scope)
  }
  scope
}


compile_internal_subroutine <- function(
  proc_name,
  closure_obj,
  parent_scope,
  formal_vars,
  res_var,
  allow_void_return = FALSE,
  forbid_superassign = character()
) {
  stopifnot(is_string(proc_name), inherits(closure_obj, LocalClosure))
  fun <- closure_obj@fun
  stopifnot(is.function(fun))
  stopifnot(is.null(res_var) || inherits(res_var, Variable))
  stopifnot(is_bool(allow_void_return))
  stopifnot(is.character(forbid_superassign))
  forbid_superassign <- unique(forbid_superassign)
  if (length(forbid_superassign) && any(!nzchar(forbid_superassign))) {
    stop("forbid_superassign must contain only non-empty names")
  }

  formal_names <- names(formals(fun)) %||% character()
  if (length(formal_names) && any(!nzchar(formal_names))) {
    stop("local closures must have only named arguments")
  }
  stopifnot(is.list(formal_vars))
  if (!identical((names(formal_vars) %||% character()), formal_names)) {
    stop("internal error: formal vars do not match closure formals")
  }

  used_names <- unique(all.vars(body(fun), functions = FALSE))

  formal_scope <- new_scope(closure = NULL, parent = parent_scope)
  attr(formal_scope, "kind") <- "closure_formals"

  proc_scope <- new_scope(fun, parent = formal_scope)
  attr(proc_scope, "kind") <- "closure"
  attr(proc_scope, "host_scope") <- scope_root(parent_scope)
  attr(proc_scope, "forbid_superassign") <- forbid_superassign

  arg_names <- character()
  for (nm in formal_names) {
    var <- formal_vars[[nm]]
    stopifnot(inherits(var, Variable))

    arg_var <- Variable(mode = var@mode, dims = var@dims, name = nm)
    if (logical_as_int(var)) {
      arg_var@logical_as_int <- TRUE
    }
    formal_scope[[nm]] <- arg_var
    arg_names <- c(arg_names, nm)
  }

  res_name <- NULL

  body_expr <- body(fun)
  stmts <- if (is_call(body_expr, quote(`{`))) {
    as.list(body_expr)[-1L]
  } else {
    list(body_expr)
  }
  if (!length(stmts)) {
    stop("closure body must not be empty")
  }

  body_prefix <- character()
  assign_code <- character()
  if (is.null(res_var)) {
    if (is.null(last(stmts))) {
      stmts <- drop_last(stmts)
    }
    body_prefix <- compile_nonreturn_statements(stmts, proc_scope)
  } else {
    last_expr <- last(stmts)
    prefix <- drop_last(stmts)

    body_prefix <- compile_nonreturn_statements(prefix, proc_scope)

    h <- new_hoist(proc_scope)
    expr_error <- NULL
    expr <- tryCatch(
      r2f(last_expr, proc_scope, hoist = h),
      error = function(e) {
        expr_error <<- e
        NULL
      }
    )

    if (!is.null(expr_error)) {
      if (!isTRUE(allow_void_return)) {
        stop(expr_error)
      }
      # If `last_expr` is itself a local closure call that returns no value,
      # compiling in value-position (with `hoist`) fails. Retry compilation in
      # statement-position.
      stmt_expr <- r2f(last_expr, proc_scope)
      if (!is.null(stmt_expr@value)) {
        stop(expr_error)
      }
      assign_code <- as.character(stmt_expr)
      res_var <- NULL
    } else if (is.null(expr@value)) {
      if (!isTRUE(allow_void_return)) {
        stop("local closure does not return a value")
      }
      assign_code <- h$render(expr)
      res_var <- NULL
    } else if (is.null(expr@value@mode)) {
      stop("could not infer closure return type")
    } else {
      res_name <- "res"
      while (res_name %in% c(formal_names, used_names)) {
        res_name <- paste0(res_name, "_")
      }
      res_var@name <- res_name
      proc_scope[[res_name]] <- res_var
      arg_names <- c(arg_names, res_name)
    }

    if (!is.null(res_var)) {
      if (is.null(res_var@mode)) {
        res_var@mode <- expr@value@mode
        res_var@dims <- expr@value@dims
        proc_scope[[res_name]] <- res_var
      }
      if (!identical(expr@value@mode, res_var@mode)) {
        stop(
          "closure result mode (",
          expr@value@mode,
          ") does not match output mode (",
          res_var@mode,
          ")"
        )
      }
      if (passes_as_scalar(res_var)) {
        if (!passes_as_scalar(expr@value)) {
          stop("closure must return a scalar for scalar outputs")
        }
      } else {
        if (passes_as_scalar(expr@value)) {
          stop("closure must return an array for array outputs")
        }
        if (expr@value@rank != res_var@rank) {
          stop("closure result rank does not match output rank")
        }
        if (
          !identical(
            dims2f(expr@value@dims, proc_scope),
            dims2f(res_var@dims, proc_scope)
          )
        ) {
          stop("closure result shape does not match output shape")
        }
      }
      assign_code <- h$render(glue("{res_name} = {expr}"))
    }
  }

  vars_formals <- scope_vars(formal_scope)
  vars_locals <- scope_vars(proc_scope)
  locals <- vars_locals
  if (!is.null(res_name)) {
    locals <- locals[setdiff(names(locals), res_name)]
  }

  if (length(locals)) {
    bad <- map_lgl(locals, function(v) {
      !passes_as_scalar(v) &&
        grepl(":", dims2f(v@dims, proc_scope), fixed = TRUE)
    })
    if (any(bad)) {
      stop("closures cannot declare allocatable locals yet")
    }
  }

  out_arg <- if (is.null(res_name)) {
    list()
  } else {
    setNames(list(proc_scope[[res_name]]), res_name)
  }
  vars_declared <- c(vars_formals, out_arg, locals)

  decls <- c(
    emit_decls(
      vars_formals,
      formal_scope,
      intents = rep(list("intent(in)"), length(vars_formals)),
      assumed_shape = TRUE,
      allow_allocatable = FALSE
    ),
    if (!is.null(res_name)) {
      emit_decls(
        out_arg,
        formal_scope,
        intents = list("intent(out)"),
        assumed_shape = TRUE,
        allow_allocatable = FALSE
      )
    },
    if (length(locals)) emit_decls(locals, proc_scope) else character()
  )

  body_code <- str_flatten_lines(body_prefix, assign_code)
  used_iso_bindings <- iso_c_binding_symbols(
    vars = vars_declared,
    body_code = body_code,
    logical_is_c_int = logical_as_int,
    uses_rng = FALSE
  )
  use_iso <- if (length(used_iso_bindings)) {
    glue("use iso_c_binding, only: {str_flatten_commas(used_iso_bindings)}")
  } else {
    NULL
  }

  proc_sig <- str_flatten_commas(arg_names)
  proc_code <- glue(
    "
    subroutine {proc_name}({proc_sig})
    {indent(str_flatten_lines(use_iso, \"implicit none\"), 2)}

    {indent(str_flatten_lines(decls), 2)}

    {indent(body_code, 2)}
    end subroutine
    ",
    .null = ""
  )
  proc_code <- glue::trim(proc_code)

  proc_code <- insert_fortran_line_continuations(
    proc_code,
    preserve_attributes = FALSE
  )

  list(
    name = proc_name,
    code = proc_code,
    captures = character(),
    res = res_name,
    res_var = res_var
  )
}

closure_last_expr <- function(fun) {
  stopifnot(is.function(fun))
  body_expr <- body(fun)
  if (is_call(body_expr, quote(`{`))) {
    stmts <- as.list(body_expr)[-1L]
    if (!length(stmts)) {
      stop("closure body must not be empty")
    }
    last(stmts)
  } else {
    body_expr
  }
}

closure_formal_vars <- function(args_f, formal_names) {
  stopifnot(is.list(args_f), is.character(formal_names))
  if (!length(formal_names)) {
    return(setNames(list(), character()))
  }
  if (length(args_f) != length(formal_names)) {
    stop("internal error: argument values do not match closure formals")
  }
  names(args_f) <- formal_names
  formal_vars <- imap(args_f, function(f, nm) {
    stopifnot(inherits(f, Fortran), inherits(f@value, Variable))
    v <- Variable(mode = f@value@mode, dims = f@value@dims, name = nm)
    if (logical_as_int_symbol(f@value)) {
      v@logical_as_int <- TRUE
    }
    v
  })
  names(formal_vars) <- formal_names
  formal_vars
}

match_closure_call_args <- function(
  call_expr,
  closure_obj,
  scope,
  ...,
  hoist = NULL
) {
  stopifnot(is.call(call_expr), inherits(closure_obj, LocalClosure))
  fun <- closure_obj@fun
  call_expr <- match.call(fun, call_expr)
  args_expr <- as.list(call_expr)[-1L]

  formal_names <- names(formals(fun)) %||% character()
  if (!identical((names(args_expr) %||% character()), formal_names)) {
    stop("internal error: match.call did not align closure args")
  }

  args_f <- lapply(args_expr, r2f, scope, ..., hoist = hoist)
  formal_vars <- closure_formal_vars(args_f, formal_names)

  list(
    call_expr = call_expr,
    args_expr = args_expr,
    args_f = args_f,
    formal_names = formal_names,
    formal_vars = formal_vars
  )
}

compile_local_closure_proc <- function(
  proc_name,
  closure_obj,
  scope,
  formal_vars,
  res_var,
  allow_void_return = FALSE,
  forbid_superassign = character()
) {
  proc <- compile_internal_subroutine(
    proc_name,
    closure_obj,
    scope,
    formal_vars = formal_vars,
    res_var = res_var,
    allow_void_return = allow_void_return,
    forbid_superassign = forbid_superassign
  )
  scope_root(scope)@add_internal_proc(proc)
  proc
}

compile_closure_call <- function(
  call_expr,
  closure_obj,
  proc_name,
  scope,
  ...,
  hoist,
  needs_value
) {
  stopifnot(
    is.call(call_expr),
    inherits(closure_obj, LocalClosure),
    is_string(proc_name),
    inherits(scope, "quickr_scope"),
    inherits(hoist, "environment"),
    is_bool(needs_value)
  )

  call_info <- match_closure_call_args(
    call_expr,
    closure_obj,
    scope,
    ...,
    hoist = hoist
  )
  args_f <- call_info$args_f
  formal_vars <- call_info$formal_vars

  last_expr <- closure_last_expr(closure_obj@fun)

  if (is.null(last_expr)) {
    if (needs_value) {
      stop("local closure calls that return `NULL` cannot be used as values")
    }
    proc <- compile_local_closure_proc(
      proc_name,
      closure_obj,
      scope,
      formal_vars = formal_vars,
      res_var = NULL
    )

    call_args <- unname(args_f)
    if (length(call_args)) {
      call_stmt <- glue("call {proc$name}({str_flatten_commas(call_args)})")
      return(Fortran(str_flatten_lines(
        call_stmt,
        quickr_error_return_if_set(scope)
      )))
    }
    return(Fortran(str_flatten_lines(
      glue("call {proc$name}()"),
      quickr_error_return_if_set(scope)
    )))
  }

  proc <- compile_local_closure_proc(
    proc_name,
    closure_obj,
    scope,
    formal_vars = formal_vars,
    res_var = Variable(),
    allow_void_return = !needs_value
  )

  if (!needs_value && is.null(proc$res)) {
    call_args <- unname(args_f)
    if (length(call_args)) {
      call_stmt <- glue("call {proc$name}({str_flatten_commas(call_args)})")
      return(Fortran(str_flatten_lines(
        call_stmt,
        quickr_error_return_if_set(scope)
      )))
    }
    return(Fortran(str_flatten_lines(
      glue("call {proc$name}()"),
      quickr_error_return_if_set(scope)
    )))
  }

  res_var <- proc$res_var
  if (is.null(res_var@mode)) {
    stop("internal error: could not infer closure return type")
  }

  tmp <- hoist$declare_tmp(mode = res_var@mode, dims = res_var@dims)
  call_args <- c(unname(args_f), tmp@name)
  call_stmt <- glue("call {proc$name}({str_flatten_commas(call_args)})")
  call_stmt <- str_flatten_lines(call_stmt, quickr_error_return_if_set(scope))

  if (needs_value) {
    hoist$emit(call_stmt)
    Fortran(tmp@name, tmp)
  } else {
    Fortran(call_stmt)
  }
}

compile_closure_call_assignment <- function(
  target_name,
  call_expr,
  scope,
  ...
) {
  stopifnot(
    is_string(target_name),
    is.call(call_expr),
    is.symbol(call_expr[[1L]])
  )
  hoist <- list(...)$hoist
  if (is.null(hoist)) {
    hoist <- new_hoist(scope)
  }

  closure_name <- as.character(call_expr[[1L]])
  closure_obj <- scope[[closure_name]]
  if (!inherits(closure_obj, LocalClosure)) {
    stop("internal error: expected a LocalClosure")
  }

  target_var <- get0(target_name, scope)
  target_exists <- inherits(target_var, Variable)
  target_fortran_name <- if (target_exists) {
    target_var@name
  } else {
    assignment_fortran_name(target_name, scope)
  }

  if (is.null(closure_last_expr(closure_obj@fun))) {
    stop("local closure calls that return `NULL` cannot be assigned")
  }
  call_info <- match_closure_call_args(call_expr, closure_obj, scope, ...)
  args_expr <- call_info$args_expr
  args_f <- call_info$args_f
  formal_vars <- call_info$formal_vars

  return_names <- attr(scope, "return_names", exact = TRUE) %||% character()
  res_var <- if (target_exists) {
    out <- Variable(
      mode = target_var@mode,
      dims = target_var@dims,
      name = target_fortran_name,
      r_name = target_name
    )
    if (logical_as_int(target_var)) {
      out@logical_as_int <- TRUE
    }
    out
  } else {
    Variable()
  }

  proc <- compile_local_closure_proc(
    closure_name,
    closure_obj,
    scope,
    formal_vars = formal_vars,
    res_var = res_var
  )
  if (!target_exists) {
    inferred_res_var <- proc$res_var
    if (is.null(inferred_res_var@mode)) {
      stop("internal error: could not infer closure return type")
    }
    target_var <- Variable(
      mode = inferred_res_var@mode,
      dims = inferred_res_var@dims
    )
    target_var@r_name <- target_name
    target_var@name <- target_fortran_name
    if (
      identical(inferred_res_var@mode, "logical") &&
        target_name %in% return_names
    ) {
      target_var@logical_as_int <- TRUE
      # Recompile with the correct storage for the output dummy argument.
      res_var <- Variable(mode = target_var@mode, dims = target_var@dims)
      res_var@r_name <- target_name
      res_var@name <- target_fortran_name
      res_var@logical_as_int <- TRUE
      proc <- compile_local_closure_proc(
        closure_name,
        closure_obj,
        scope,
        formal_vars = formal_vars,
        res_var = res_var
      )
    }
    scope[[target_name]] <- target_var
  }
  scope_root(scope)@add_internal_proc(proc)

  arg_reads_target <- any(map_lgl(args_expr, function(e) {
    any(all.vars(e, functions = FALSE) == target_name)
  }))

  res_target <- target_fortran_name
  post <- character()
  if (arg_reads_target) {
    tmp <- hoist$declare_tmp(
      mode = target_var@mode,
      dims = target_var@dims,
      logical_as_int = logical_as_int(target_var)
    )
    res_target <- tmp@name
    post <- glue("{target_fortran_name} = {res_target}")
  }

  call_args <- c(
    unname(args_f),
    res_target
  )

  if (target_exists) {
    target_var@modified <- TRUE
    scope[[target_name]] <- target_var
  }

  Fortran(glue(
    "
    call {proc$name}({str_flatten_commas(call_args)})
    {quickr_error_return_if_set(scope)}
    {str_flatten_lines(post)}
    "
  ))
}

compile_sapply_assignment <- function(
  out_name,
  call_expr,
  scope,
  ...,
  parallel = NULL
) {
  stopifnot(is_string(out_name), is_sapply_call(call_expr))
  hoist <- list(...)$hoist
  if (is.null(hoist)) {
    hoist <- new_hoist(scope)
  }
  args <- as.list(call_expr)[-1L]
  if (length(args) < 2L) {
    stop("sapply() requires at least 2 arguments")
  }

  seq_call <- args[[1L]]
  fun_expr <- args[[2L]]
  simplify <- args$simplify
  if (!is.null(simplify) && !is_missing(simplify)) {
    if (is_string(simplify) && identical(simplify, "array")) {
      # ok
    } else if (is_bool(simplify) && isTRUE(simplify)) {
      simplify <- NULL
    } else {
      stop('Only `sapply(..., simplify = "array")` is supported.')
    }
  }

  out_var <- get0(out_name, scope)
  target_exists <- inherits(out_var, Variable)
  if (target_exists) {
    if (out_var@rank < 1L) {
      stop("sapply() output must be an array: ", out_name)
    }
    if (
      out_var@rank > 2L && (is.null(simplify) || !identical(simplify, "array"))
    ) {
      stop('sapply() that returns arrays requires `simplify = "array"`.')
    }
  }

  env <- environment(scope@closure)
  if (is.symbol(fun_expr)) {
    fun_name <- as.character(fun_expr)
    closure_obj <- scope[[fun_name]]
    if (!inherits(closure_obj, LocalClosure)) {
      stop("unsupported FUN in sapply(): ", fun_name)
    }
    proc_name <- fun_name
  } else if (is_function_call(fun_expr)) {
    proc_name <- scope_root(scope)@get_unique_proc(prefix = "closure")
    closure_obj <- as_local_closure(fun_expr, env, name = proc_name)
  } else {
    stop("unsupported FUN in sapply(); use a local closure or function(i) ...")
  }

  formal_names <- names(formals(closure_obj@fun)) %||% character()
  if (length(formal_names) != 1L || any(!nzchar(formal_names))) {
    stop("sapply() FUN must have exactly one named argument")
  }

  seq_call_unwrapped <- seq_call
  while (
    is_call(seq_call_unwrapped, quote(`(`)) && length(seq_call_unwrapped) == 2L
  ) {
    seq_call_unwrapped <- seq_call_unwrapped[[2L]]
  }
  index_iterable <- is_call(seq_call_unwrapped, quote(seq_len)) ||
    is_call(seq_call_unwrapped, quote(seq_along))

  iterable_len_expr <- NULL
  iterable_tmp <- NULL
  iterable_tmp_assign <- NULL
  iterable_value <- NULL

  if (index_iterable) {
    seq_val <- r2f(seq_call, scope, calls = "for", hoist = hoist)
    if (!is.null(seq_val@value)) {
      iterable_len_expr <- value_length_expr(seq_val@value)
    }
    formal_vars <- list(
      Variable(mode = "integer", name = formal_names[[1L]])
    )
    names(formal_vars) <- formal_names
  } else {
    iterable_val <- r2f(seq_call, scope, calls = "sapply", hoist = hoist)
    if (is.null(iterable_val@value) || is.null(iterable_val@value@mode)) {
      stop("sapply() iterable must have a value")
    }
    iterable_value <- iterable_val@value

    iterable_len_expr <- value_length_expr(iterable_value)

    iterable_tmp <- scope@get_unique_var(
      mode = iterable_value@mode,
      dims = iterable_value@dims
    )
    iterable_tmp_assign <- glue("{iterable_tmp@name} = {iterable_val}")

    formal_vars <- list(
      Variable(mode = iterable_value@mode, name = formal_names[[1L]])
    )
    names(formal_vars) <- formal_names
  }
  res_var <- if (target_exists) {
    res_dims <- if (out_var@rank == 1L) {
      NULL
    } else {
      out_var@dims[seq_len(out_var@rank - 1L)]
    }
    res_out <- Variable(mode = out_var@mode, dims = res_dims)
    if (logical_as_int(out_var)) {
      res_out@logical_as_int <- TRUE
    }
    res_out
  } else {
    Variable()
  }

  proc <- compile_internal_subroutine(
    proc_name,
    closure_obj,
    scope,
    formal_vars,
    res_var,
    forbid_superassign = out_name
  )

  if (!target_exists) {
    inferred <- proc$res_var
    if (is.null(inferred@mode)) {
      stop("internal error: could not infer sapply() output type")
    }
    res_var <- inferred

    return_names <- attr(scope, "return_names", exact = TRUE) %||% character()
    if (res_var@mode == "logical" && out_name %in% return_names) {
      res_var@logical_as_int <- TRUE
      proc <- compile_internal_subroutine(
        proc_name,
        closure_obj,
        scope,
        formal_vars,
        res_var,
        forbid_superassign = out_name
      )
    }

    if (is.null(iterable_len_expr) || is_scalar_na(iterable_len_expr)) {
      stop("sapply() requires a vector input with a known length")
    }
    if (
      res_var@rank > 1L && (is.null(simplify) || !identical(simplify, "array"))
    ) {
      stop('sapply() that returns arrays requires `simplify = "array"`.')
    }
    out_dims <- if (res_var@rank == 0L) {
      list(iterable_len_expr)
    } else {
      c(res_var@dims, list(iterable_len_expr))
    }
    out_var <- Variable(mode = res_var@mode, dims = out_dims, name = out_name)
    if (logical_as_int(res_var)) {
      out_var@logical_as_int <- TRUE
    }
    scope[[out_name]] <- out_var
  }

  scope_root(scope)@add_internal_proc(proc)

  out_target <- out_name
  post_stmts <- character()
  if (out_name %in% all.vars(body(closure_obj@fun), functions = FALSE)) {
    tmp_out <- hoist$declare_tmp(
      mode = out_var@mode,
      dims = out_var@dims,
      logical_as_int = logical_as_int(out_var)
    )
    out_target <- tmp_out@name
    post_stmts <- glue("{out_name} = {out_target}")
  }

  idx <- scope@get_unique_var("integer")
  last_i <- if (index_iterable) {
    if (is.null(iterable_len_expr) || is_scalar_na(iterable_len_expr)) {
      NULL
    } else {
      r2f(iterable_len_expr, scope, hoist = hoist)
    }
  } else if (passes_as_scalar(iterable_value)) {
    "1_c_int"
  } else {
    glue("size({iterable_tmp@name})")
  }
  if (is.null(last_i)) {
    stop("sapply() requires a vector input with a known length")
  }

  res_target <- if (out_var@rank == 1L) {
    glue("{out_target}({idx@name})")
  } else {
    subs <- c(rep(":", out_var@rank - 1L), idx@name)
    glue("{out_target}({str_flatten_commas(subs)})")
  }
  if (passes_as_scalar(out_var)) {
    res_target <- out_target
  }

  element_designator <- if (index_iterable) {
    idx@name
  } else if (passes_as_scalar(iterable_value)) {
    iterable_tmp@name
  } else if (iterable_value@rank == 1L) {
    glue("{iterable_tmp@name}({idx@name})")
  } else {
    subs <- linear_subscripts_from_1d(
      iterable_tmp@name,
      iterable_value@rank,
      Fortran(idx@name, idx)
    )
    glue("{iterable_tmp@name}({str_flatten_commas(subs)})")
  }

  call_args <- str_flatten_commas(c(
    element_designator,
    res_target
  ))

  out_var@modified <- TRUE
  scope[[out_name]] <- out_var

  directives <- openmp_directives(parallel)
  if (!is.null(parallel)) {
    mark_openmp_used(scope)
  }
  error_check_inner <- if (is.null(parallel)) {
    quickr_error_return_if_set(scope)
  } else {
    quickr_error_return_if_set(
      scope,
      openmp_depth = scope_openmp_depth(scope) + 1L
    )
  }
  error_check_after <- if (!is.null(parallel)) {
    quickr_error_return_if_set(
      scope,
      openmp_depth = scope_openmp_depth(scope)
    )
  } else {
    ""
  }
  loop_header <- glue("do {idx@name} = 1_c_int, {last_i}")
  prefix <- str_flatten_lines(
    if (!index_iterable) iterable_tmp_assign else NULL,
    str_flatten_lines(directives$prefix, loop_header)
  )
  Fortran(glue(
    "
    {prefix}
      call {proc_name}({call_args})
      {error_check_inner}
    end do
    {str_flatten_lines(directives$suffix)}
    {error_check_after}
    {str_flatten_lines(post_stmts)}
    "
  ))
}


compile_subset_designator <- function(
  base_var,
  base_name,
  idx_args,
  scope,
  ...,
  hoist = NULL,
  drop = TRUE
) {
  stopifnot(
    inherits(base_var, Variable),
    is_string(base_name),
    inherits(scope, "quickr_scope"),
    is_bool(drop)
  )

  idxs <- whole_doubles_to_ints(idx_args)
  idxs <- imap(idxs, function(idx, i) {
    if (is_missing(idx)) {
      Fortran(":", Variable("integer", base_var@dims[[i]]))
    } else {
      sub <- r2f(idx, scope, ..., hoist = hoist)
      if (sub@value@mode == "double") {
        Fortran(
          glue("int({sub}, kind=c_ptrdiff_t)"),
          Variable("integer", sub@value@dims)
        )
      } else {
        sub
      }
    }
  })

  # Indexing a scalar (rank-1 length-1) with `[1]` is valid in R, but Fortran
  # scalars cannot be subscripted. Treat it as a no-op.
  if (
    passes_as_scalar(base_var) &&
      length(idxs) == 1 &&
      idxs[[1]]@value@mode == "integer" &&
      passes_as_scalar(idxs[[1]]@value)
  ) {
    idx_r <- attr(idxs[[1]], "r", exact = TRUE)
    if (identical(idx_r, 1L) || identical(idx_r, 1)) {
      return(base_name)
    }
    if (isTRUE(idxs[[1]]@value@loop_is_singleton)) {
      return(base_name)
    }
  }

  # R-style linear indexing for rank>1 arrays: x[i]
  if (
    length(idxs) == 1 &&
      idxs[[1]]@value@mode == "integer" &&
      passes_as_scalar(idxs[[1]]@value) &&
      base_var@rank > 1
  ) {
    subs <- linear_subscripts_from_1d(base_name, base_var@rank, idxs[[1]])
    return(as.character(glue("{base_name}({str_flatten_commas(subs)})")))
  }

  if (length(idxs) != base_var@rank) {
    stop(
      "number of args to x[...] must match the rank of x, received:",
      deparse1(as.call(c(quote(`[`), idx_args)))
    )
  }

  idxs <- imap(idxs, function(subscript, i) {
    switch(
      paste0(subscript@value@mode, subscript@value@rank),
      logical0 = {
        Fortran(":", Variable("integer", base_var@dims[[i]]))
      },
      logical1 = {
        stop("logical subscript vectors are not supported for assignment")
      },
      integer0 = {
        if (drop) {
          subscript
        } else {
          Fortran(glue("{subscript}:{subscript}"), Variable("int", 1))
        }
      },
      integer1 = {
        subscript
      },
      stop(
        "all args to x[...] must be logical or integer of rank 0 or 1",
        deparse1(as.call(c(quote(`[`), idx_args)))
      )
    )
  })

  as.character(glue("{base_name}({str_flatten_commas(idxs)})"))
}

compile_subscript_lhs <- function(
  subset_call,
  scope,
  ...,
  hoist = NULL,
  target = c("local", "host")
) {
  stopifnot(is_call(subset_call, quote(`[`)))
  target <- match.arg(target)

  if (target == "local") {
    pre <- NULL

    if (scope_is_closure(scope) && is.symbol(base <- subset_call[[2L]])) {
      base_name <- as.character(base)
      local_base <- get0(base_name, scope, inherits = FALSE)
      if (is.null(local_base) || !inherits(local_base, Variable)) {
        parent_base <- get0(base_name, scope)
        if (!inherits(parent_base, Variable)) {
          stop("could not resolve symbol: ", base_name)
        }

        shadow <- Variable(mode = parent_base@mode, dims = parent_base@dims)
        shadow@r_name <- base_name
        shadow@name <- make_shadow_fortran_name(
          scope,
          fortranize_name(base_name)
        )
        if (logical_as_int(parent_base)) {
          shadow@logical_as_int <- TRUE
        }
        scope[[base_name]] <- shadow

        pre <- glue("{shadow@name} = {parent_base@name}")
      }
    }

    return(list(pre = pre, lhs = r2f(subset_call, scope, ..., hoist = hoist)))
  }

  if (is.null(scope) || !identical(scope@kind, "closure")) {
    stop("host-target subset compilation is only valid inside local closures")
  }

  base <- subset_call[[2L]]
  if (!is.symbol(base)) {
    stop("only superassignment to x[...] is supported")
  }
  name <- as.character(base)

  host_scope <- scope@host_scope %||% stop("internal error: missing host scope")
  host_var <- get0(name, host_scope)
  if (!inherits(host_var, Variable)) {
    stop(
      "<<- targets must resolve to an existing variable in the enclosing quick() scope: ",
      name
    )
  }

  idx_args <- as.list(subset_call)[-1L]
  idx_args <- idx_args[-1L]
  drop <- idx_args$drop %||% TRUE
  idx_args$drop <- NULL
  if (!isTRUE(drop)) {
    stop("drop = FALSE not supported for superassignment")
  }

  designator <- compile_subset_designator(
    base_var = host_var,
    base_name = host_var@name,
    idx_args = idx_args,
    scope = scope,
    ...,
    hoist = hoist,
    drop = drop
  )

  list(pre = NULL, lhs = Fortran(designator))
}
