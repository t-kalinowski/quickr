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
    !identical(scope_kind(scope), "subroutine") &&
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
  forbid_superassign = character(),
  optional_args = character()
) {
  stopifnot(is_string(proc_name), inherits(closure_obj, LocalClosure))
  fun <- closure_obj@fun
  stopifnot(is.function(fun))
  stopifnot(is.null(res_var) || inherits(res_var, Variable))
  stopifnot(is_bool(allow_void_return))
  stopifnot(is.character(forbid_superassign))
  stopifnot(is.character(optional_args))
  forbid_superassign <- unique(forbid_superassign)
  optional_args <- unique(optional_args)
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
  scope_set(formal_scope, "kind", "closure_formals")

  proc_scope <- new_scope(fun, parent = formal_scope)
  scope_set(proc_scope, "kind", "closure")
  scope_set(proc_scope, "host_scope", scope_root(parent_scope))
  scope_set(proc_scope, "forbid_superassign", forbid_superassign)

  arg_names <- character()
  optional_locals <- list()
  optional_inits <- character()
  if (length(optional_args)) {
    unknown_optional <- setdiff(optional_args, formal_names)
    if (length(unknown_optional)) {
      stop(
        "internal error: optional args not in closure formals: ",
        str_flatten_commas(unknown_optional)
      )
    }
  }
  for (nm in formal_names) {
    var <- formal_vars[[nm]]
    stopifnot(inherits(var, Variable))
    fortran_name <- var@name %||% fortranize_name(nm)

    arg_var <- Variable(
      mode = var@mode,
      dims = var@dims,
      name = fortran_name,
      r_name = nm
    )
    if (logical_as_int(var)) {
      arg_var@logical_as_int <- TRUE
    }
    formal_scope[[nm]] <- arg_var
    arg_names <- c(arg_names, fortran_name)

    if (nm %in% optional_args) {
      local_name <- make_shadow_fortran_name(proc_scope, fortran_name)
      local_var <- Variable(
        mode = var@mode,
        dims = var@dims,
        name = local_name,
        r_name = nm
      )
      if (logical_as_int(var)) {
        local_var@logical_as_int <- TRUE
      }
      local_var@optional_dummy <- fortran_name
      proc_scope[[nm]] <- local_var
      optional_locals[[nm]] <- local_var
      optional_inits <- c(
        optional_inits,
        glue(
          "if (present({fortran_name})) then\n  {local_name} = {fortran_name}\nend if"
        )
      )
    }
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

  if (length(optional_args)) {
    unsafe <- character()
    for (nm in optional_args) {
      used <- any(map_lgl(stmts, optional_arg_used, nm = nm))
      if (!used) {
        next
      }
      missing_init <- any(map_lgl(stmts, optional_arg_missing_init, nm = nm))
      assigned_before_use <- optional_arg_assigned_before_use(stmts, nm = nm)
      if (!missing_init && !assigned_before_use) {
        unsafe <- c(unsafe, nm)
      }
    }
    if (length(unsafe)) {
      stop(
        "optional argument(s) used without initializing when NULL: ",
        str_flatten_commas(unsafe),
        ". Add an is.null() branch that assigns a value before use.",
        call. = FALSE
      )
    }
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

  if (length(optional_locals)) {
    for (nm in names(optional_locals)) {
      dummy_var <- formal_scope[[nm]]
      local_var <- proc_scope[[nm]]
      if (is.null(local_var@mode) && !is.null(dummy_var@mode)) {
        local_var@mode <- dummy_var@mode
        local_var@dims <- dummy_var@dims
      }
      if (is.null(dummy_var@mode) && !is.null(local_var@mode)) {
        dummy_var@mode <- local_var@mode
        dummy_var@dims <- local_var@dims
      }
      if (is.null(dummy_var@mode) || is.null(local_var@mode)) {
        stop(
          "optional argument `",
          nm,
          "` type could not be inferred; provide a typed value or assign a typed default",
          call. = FALSE
        )
      }
      formal_scope[[nm]] <- dummy_var
      proc_scope[[nm]] <- local_var
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

  formals_intents <- rep(list("intent(in)"), length(vars_formals))
  names(formals_intents) <- names(vars_formals)
  if (length(optional_locals)) {
    for (nm in names(optional_locals)) {
      formals_intents[[nm]] <- "intent(in), optional"
    }
  }

  decls <- c(
    emit_decls(
      vars_formals,
      formal_scope,
      intents = formals_intents,
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

  body_code <- str_flatten_lines(optional_inits, body_prefix, assign_code)
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

optional_arg_used <- function(expr, nm) {
  if (is.symbol(expr)) {
    return(identical(as.character(expr), nm))
  }

  if (!is.call(expr)) {
    return(FALSE)
  }

  if (
    is_call(expr, quote(is.null)) &&
      length(expr) == 2L &&
      is.symbol(expr[[2L]]) &&
      identical(as.character(expr[[2L]]), nm)
  ) {
    return(FALSE)
  }

  if (
    is_call(expr, quote(`!`)) &&
      length(expr) == 2L &&
      is_call(expr[[2L]], quote(is.null)) &&
      length(expr[[2L]]) == 2L &&
      is.symbol(expr[[2L]][[2L]]) &&
      identical(as.character(expr[[2L]][[2L]]), nm)
  ) {
    return(FALSE)
  }

  if (is_call(expr, quote(`{`))) {
    return(any(map_lgl(as.list(expr)[-1L], optional_arg_used, nm = nm)))
  }

  if (is_call(expr, quote(`if`))) {
    if (optional_arg_used(expr[[2L]], nm = nm)) {
      return(TRUE)
    }
    if (optional_arg_used(expr[[3L]], nm = nm)) {
      return(TRUE)
    }
    if (length(expr) == 4L && optional_arg_used(expr[[4L]], nm = nm)) {
      return(TRUE)
    }
    return(FALSE)
  }

  if (
    is_call(expr, quote(`<-`)) ||
      is_call(expr, quote(`=`)) ||
      is_call(expr, quote(`<<-`))
  ) {
    return(optional_arg_used(expr[[3L]], nm = nm))
  }

  any(map_lgl(as.list(expr)[-1L], optional_arg_used, nm = nm))
}

optional_arg_assigned <- function(expr, nm) {
  if (!is.call(expr)) {
    return(FALSE)
  }

  if (
    is_call(expr, quote(`<-`)) ||
      is_call(expr, quote(`=`)) ||
      is_call(expr, quote(`<<-`))
  ) {
    lhs <- expr[[2L]]
    if (is.symbol(lhs) && identical(as.character(lhs), nm)) {
      return(TRUE)
    }
    return(optional_arg_assigned(expr[[3L]], nm = nm))
  }

  if (is_call(expr, quote(`{`))) {
    return(any(map_lgl(as.list(expr)[-1L], optional_arg_assigned, nm = nm)))
  }

  if (is_call(expr, quote(`if`))) {
    if (optional_arg_assigned(expr[[2L]], nm = nm)) {
      return(TRUE)
    }
    if (optional_arg_assigned(expr[[3L]], nm = nm)) {
      return(TRUE)
    }
    if (length(expr) == 4L && optional_arg_assigned(expr[[4L]], nm = nm)) {
      return(TRUE)
    }
    return(FALSE)
  }

  any(map_lgl(as.list(expr)[-1L], optional_arg_assigned, nm = nm))
}

optional_arg_missing_init <- function(expr, nm) {
  if (!is.call(expr)) {
    return(FALSE)
  }

  if (is_call(expr, quote(`{`))) {
    return(any(map_lgl(as.list(expr)[-1L], optional_arg_missing_init, nm = nm)))
  }

  if (is_call(expr, quote(`if`))) {
    cond <- expr[[2L]]
    then_branch <- expr[[3L]]
    else_branch <- if (length(expr) == 4L) expr[[4L]] else NULL

    if (
      is_call(cond, quote(is.null)) &&
        length(cond) == 2L &&
        is.symbol(cond[[2L]]) &&
        identical(as.character(cond[[2L]]), nm)
    ) {
      if (optional_arg_assigned(then_branch, nm = nm)) return(TRUE)
    }

    if (
      is_call(cond, quote(`!`)) &&
        length(cond) == 2L &&
        is_call(cond[[2L]], quote(is.null)) &&
        length(cond[[2L]]) == 2L &&
        is.symbol(cond[[2L]][[2L]]) &&
        identical(as.character(cond[[2L]][[2L]]), nm)
    ) {
      if (
        !is.null(else_branch) &&
          optional_arg_assigned(else_branch, nm = nm)
      ) {
        return(TRUE)
      }
    }

    if (optional_arg_missing_init(then_branch, nm = nm)) {
      return(TRUE)
    }
    if (
      !is.null(else_branch) &&
        optional_arg_missing_init(else_branch, nm = nm)
    ) {
      return(TRUE)
    }
    return(FALSE)
  }

  FALSE
}

optional_arg_assigned_before_use <- function(stmts, nm) {
  stopifnot(is.list(stmts), is_string(nm))
  assigned <- FALSE

  scan_expr <- function(expr, assigned) {
    if (is_call(expr, quote(`{`))) {
      for (stmt in as.list(expr)[-1L]) {
        res <- scan_expr(stmt, assigned)
        if (res$used_before) {
          return(res)
        }
        assigned <- res$assigned
      }
      return(list(assigned = assigned, used_before = FALSE))
    }

    if (
      is_call(expr, quote(`<-`)) ||
        is_call(expr, quote(`=`)) ||
        is_call(expr, quote(`<<-`))
    ) {
      lhs <- expr[[2L]]
      if (is.symbol(lhs) && identical(as.character(lhs), nm)) {
        return(list(assigned = TRUE, used_before = FALSE))
      }
    }

    if (optional_arg_used(expr, nm = nm)) {
      return(list(assigned = assigned, used_before = !assigned))
    }

    list(assigned = assigned, used_before = FALSE)
  }

  for (stmt in stmts) {
    res <- scan_expr(stmt, assigned)
    if (res$used_before) {
      return(FALSE)
    }
    assigned <- res$assigned
  }

  assigned
}

closure_formal_vars <- function(args_f, formal_names) {
  stopifnot(is.list(args_f), is.character(formal_names))
  if (!length(formal_names)) {
    return(setNames(list(), character()))
  }
  if (length(args_f) != length(formal_names)) {
    stop("internal error: argument values do not match closure formals")
  }
  fortran_names <- map_chr(formal_names, fortranize_name)
  dup <- duplicated(tolower(fortran_names)) |
    duplicated(tolower(fortran_names), fromLast = TRUE)
  if (any(dup)) {
    dup_map <- paste0(formal_names[dup], " -> ", fortran_names[dup])
    stop(
      "local closure arguments map to the same Fortran name: ",
      str_flatten_commas(dup_map),
      call. = FALSE
    )
  }
  fortran_by_r <- setNames(fortran_names, formal_names)
  names(args_f) <- formal_names
  formal_vars <- imap(args_f, function(f, nm) {
    fortran_name <- fortran_by_r[[nm]]
    if (is.null(f)) {
      v <- Variable(name = fortran_name, r_name = nm)
    } else {
      stopifnot(inherits(f, Fortran), inherits(f@value, Variable))
      v <- Variable(
        mode = f@value@mode,
        dims = f@value@dims,
        name = fortran_name,
        r_name = nm
      )
      if (logical_as_int_symbol(f@value)) {
        v@logical_as_int <- TRUE
      }
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

  formals_list <- as.list(formals(fun))
  formal_names <- names(formals_list) %||% character()
  closure_name <- closure_obj@name %||% "local closure"

  arg_names <- names(args_expr) %||% rep("", length(args_expr))
  if (any(!nzchar(arg_names))) {
    stop("internal error: local closure call has unnamed arguments")
  }

  extra <- setdiff(arg_names, formal_names)
  if (length(extra)) {
    stop(
      closure_name,
      " call has unknown argument(s): ",
      str_flatten_commas(extra),
      call. = FALSE
    )
  }

  args_aligned <- formals_list
  if (length(args_expr)) {
    args_aligned[names(args_expr)] <- args_expr
  }

  optional_args <- names(formals_list)[map_lgl(formals_list, function(x) {
    is.null(x) || identical(x, quote(NULL))
  })] %||%
    character()

  args_present <- setNames(rep(TRUE, length(formal_names)), formal_names)
  missing_args <- character()
  for (nm in formal_names) {
    default_expr <- formals_list[[nm]]
    arg_expr <- args_aligned[[nm]]
    optional_here <- nm %in% optional_args

    if (is_missing(arg_expr)) {
      if (is_missing(default_expr)) {
        missing_args <- c(missing_args, nm)
      } else if (optional_here) {
        args_present[[nm]] <- FALSE
        args_aligned[[nm]] <- quote(expr = )
      } else {
        args_aligned[[nm]] <- default_expr
      }
      next
    }

    if (
      optional_here && (is.null(arg_expr) || identical(arg_expr, quote(NULL)))
    ) {
      args_present[[nm]] <- FALSE
      args_aligned[[nm]] <- quote(expr = )
    }
  }

  if (length(missing_args)) {
    stop(
      closure_name,
      " call is missing required argument(s): ",
      str_flatten_commas(missing_args),
      call. = FALSE
    )
  }

  args_expr <- args_aligned

  args_f <- lapply(formal_names, function(nm) {
    if (!isTRUE(args_present[[nm]])) {
      return(NULL)
    }
    r2f(args_expr[[nm]], scope, ..., hoist = hoist)
  })
  names(args_f) <- formal_names

  arg_ok <- map_lgl(
    args_f,
    \(arg) {
      is.null(arg) ||
        (inherits(arg@value, Variable) && !is.null(arg@value@mode))
    }
  )
  if (any(!arg_ok)) {
    bad_args <- formal_names[!arg_ok]
    stop(
      closure_name,
      " call has argument(s) without inferred types: ",
      str_flatten_commas(bad_args),
      ". Provide explicit typed values.",
      call. = FALSE
    )
  }

  formal_vars <- closure_formal_vars(args_f, formal_names)

  list(
    call_expr = call_expr,
    args_expr = args_expr,
    args_f = args_f,
    formal_names = formal_names,
    formal_vars = formal_vars,
    optional_args = optional_args,
    args_present = args_present
  )
}

closure_call_inputs <- function(args_f, args_present, formal_vars) {
  stopifnot(is.list(args_f), is.logical(args_present), is.list(formal_vars))
  formal_names <- names(formal_vars) %||% character()
  if (!length(formal_names)) {
    return(list(args = character(), use_keywords = FALSE))
  }
  present_names <- formal_names[args_present[formal_names]]
  use_keywords <- any(!args_present)
  if (!length(present_names)) {
    return(list(args = character(), use_keywords = use_keywords))
  }
  if (use_keywords) {
    args <- map_chr(present_names, function(nm) {
      dummy_name <- formal_vars[[nm]]@name %||% nm
      glue("{dummy_name} = {args_f[[nm]]}")
    })
  } else {
    args <- unname(args_f[present_names])
  }
  list(args = args, use_keywords = use_keywords)
}

compile_local_closure_proc <- function(
  proc_name,
  closure_obj,
  scope,
  formal_vars,
  res_var,
  allow_void_return = FALSE,
  forbid_superassign = character(),
  optional_args = character()
) {
  proc <- compile_internal_subroutine(
    proc_name,
    closure_obj,
    scope,
    formal_vars = formal_vars,
    res_var = res_var,
    allow_void_return = allow_void_return,
    forbid_superassign = forbid_superassign,
    optional_args = optional_args
  )
  scope_add_internal_proc(scope_root(scope), proc)
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
  optional_args <- call_info$optional_args
  args_present <- call_info$args_present

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
      res_var = NULL,
      optional_args = optional_args
    )

    inputs <- closure_call_inputs(args_f, args_present, formal_vars)
    call_args <- inputs$args
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
    allow_void_return = !needs_value,
    optional_args = optional_args
  )

  if (!needs_value && is.null(proc$res)) {
    inputs <- closure_call_inputs(args_f, args_present, formal_vars)
    call_args <- inputs$args
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
  inputs <- closure_call_inputs(args_f, args_present, formal_vars)
  call_args <- inputs$args
  res_arg <- if (inputs$use_keywords) {
    glue("{proc$res} = {tmp@name}")
  } else {
    tmp@name
  }
  call_args <- c(call_args, res_arg)
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
  optional_args <- call_info$optional_args
  args_present <- call_info$args_present

  return_names <- scope_get(scope, "return_names", default = character()) %||%
    character()
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
    res_var = res_var,
    optional_args = optional_args
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
        res_var = res_var,
        optional_args = optional_args
      )
    }
    scope[[target_name]] <- target_var
  }
  scope_add_internal_proc(scope_root(scope), proc)

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

  inputs <- closure_call_inputs(args_f, args_present, formal_vars)
  res_arg <- if (inputs$use_keywords) {
    glue("{proc$res} = {res_target}")
  } else {
    res_target
  }
  call_args <- c(inputs$args, res_arg)

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

  env <- environment(scope_closure(scope))
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

    iterable_tmp <- scope_unique_var(
      scope,
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

    return_names <- scope_get(scope, "return_names", default = character()) %||%
      character()
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

  scope_add_internal_proc(scope_root(scope), proc)

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

  idx <- scope_unique_var(scope, "integer")
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
  drop = TRUE,
  allow_logical_vector_subscripts = FALSE
) {
  stopifnot(
    inherits(base_var, Variable),
    is_string(base_name),
    inherits(scope, "quickr_scope"),
    is_bool(drop),
    is_bool(allow_logical_vector_subscripts)
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
        if (!allow_logical_vector_subscripts) {
          stop("logical subscript vectors are not supported for assignment")
        }

        # Convert logical vectors to integer vector subscripts (R's `which()`).
        # Fortran array designators do not accept logical vectors directly.
        mask <- booleanize_logical_as_int(subscript)
        it <- scope_unique_var(scope, "integer")
        f <- glue("pack([({it}, {it}=1, size({mask}))], {mask})")
        Fortran(f, Variable("int", NA))
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
    base <- subset_call[[2L]]
    if (!is.symbol(base)) {
      stop("assignment targets must be symbols, found: ", deparse1(base))
    }
    base_name <- as.character(base)

    pre <- NULL

    if (scope_is_closure(scope)) {
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

    base_var <- get0(base_name, scope)
    if (!inherits(base_var, Variable)) {
      stop("could not resolve symbol: ", base_name)
    }

    idx_args <- as.list(subset_call)[-1L]
    idx_args <- idx_args[-1L]
    drop <- idx_args$drop %||% TRUE
    idx_args$drop <- NULL

    # When compiling subscripts for assignment, force iterable indices like `i:i`
    # to render as Fortran triplets (i:i) rather than value sequences.
    dots <- list(...)
    dots$calls <- c(dots$calls %||% character(), "[")

    designator <- do.call(
      compile_subset_designator,
      c(
        list(
          base_var = base_var,
          base_name = base_var@name,
          idx_args = idx_args,
          scope = scope,
          hoist = hoist,
          drop = drop,
          allow_logical_vector_subscripts = TRUE
        ),
        dots
      )
    )

    return(list(pre = pre, lhs = Fortran(designator)))
  }

  if (is.null(scope) || !identical(scope_kind(scope), "closure")) {
    stop("host-target subset compilation is only valid inside local closures")
  }

  base <- subset_call[[2L]]
  if (!is.symbol(base)) {
    stop("only superassignment to x[...] is supported")
  }
  name <- as.character(base)

  host_scope <- scope_host_scope(scope) %||%
    stop("internal error: missing host scope")
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
    drop = drop,
    allow_logical_vector_subscripts = TRUE
  )

  list(pre = NULL, lhs = Fortran(designator))
}
