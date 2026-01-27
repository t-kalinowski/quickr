make_c_bridge <- function(fsub, strict = TRUE, headers = TRUE) {
  stopifnot(inherits(fsub, FortranSubroutine))

  closure <- fsub@closure
  scope <- fsub@scope
  uses_rng <- isTRUE(attr(scope, "uses_rng", TRUE))
  uses_errors <- isTRUE(attr(scope, "uses_errors", TRUE))

  fsub_arg_names <- fsub@signature # arg names
  closure_arg_names <- names(formals(closure)) %||% character()

  c_body <- character()
  c_hoist <- c_bridge_hoist()

  closure_arg_vars <- mget(closure_arg_names, scope)
  closure_arg_fortran_names <- map_chr(closure_arg_vars, \(var) var@name)
  if (!all(closure_arg_fortran_names %in% fsub_arg_names)) {
    missing_idx <- which(!closure_arg_fortran_names %in% fsub_arg_names)
    stop(
      "Undeclared arguments: ",
      str_flatten_commas(closure_arg_names[missing_idx])
    )
  }

  # first unpack all the input vars into named C variables (including sizes and pointer)
  arg_defs <- lapply(
    closure_arg_vars,
    closure_arg_c_defs,
    strict = strict
  ) |>
    rbind("")

  ## TODO, might still need to define a length size for vars where rank>1, if in checks.

  # now do all size checks.
  size_checks <- unlist(
    lapply(
      closure_arg_vars,
      closure_arg_size_checks,
      scope = scope,
      c_hoist = c_hoist
    ),
    use.names = FALSE
  )

  # maybe define and allocate the output var(s)
  n_protected <- 0L
  return_var_names <- closure_return_var_names(closure)
  return_defs <- character()
  # Deduplicate by the underlying variable name to avoid duplicate C defs
  for (return_var in mget(unique(unname(return_var_names)), scope)) {
    return_var_r_name <- return_var@r_name %||% return_var@name
    if (!return_var_r_name %in% closure_arg_names) {
      return_var@modified <- TRUE
      assign(return_var_r_name, return_var, scope)
      append(return_defs) <- return_var_c_defs(
        return_var,
        fsub@scope,
        c_hoist = c_hoist
      )
      add(n_protected) <- 1L # allocated return var
      if (return_var@rank > 1) {
        add(n_protected) <- 1L # allocated _dim_sexp
      }
    }
  }

  c_body <- c(arg_defs)
  append(c_body) <- size_checks
  append(c_body) <- return_defs

  if (uses_errors) {
    append(c_body) <- c(
      "",
      glue("char {quickr_error_msg_name()}[{quickr_error_msg_len()}];"),
      glue("{quickr_error_msg_name()}[0] = '\\0';"),
      ""
    )
  }

  fsub_call_args <- fsub_arg_names |>
    lapply(\(nm) {
      if (is_quickr_error_msg(nm)) {
        return(nm)
      }
      paste0(nm, if (!is_size_name(nm)) "__")
    }) |>
    unlist()

  if (length(fsub_call_args) > 3) {
    fsub_call_args <- paste0("\n  ", fsub_call_args)
  }

  append(c_body) <- c(
    "",
    if (uses_rng) "GetRNGstate();",
    glue("{fsub@name}({str_flatten_commas(fsub_call_args)});"),
    if (uses_rng) "PutRNGstate();",
    if (uses_errors) glue("if ({quickr_error_msg_name()}[0] != '\\0') {{"),
    if (uses_errors) {
      indent(glue(
        "Rf_error(\"%s\", {quickr_error_msg_name()});"
      ))
    },
    if (uses_errors) "}",
    ""
  )
  # Determine if the closure returns a list call or a single symbol
  is_list_return <- is_call(last(body(closure)), quote(list))

  if (length(return_var_names) == 1L && !is_list_return) {
    return_var_r_name <- unname(return_var_names)[[1L]]
    return_var_c_name <- scope[[return_var_r_name]]@name
    if (n_protected > 0) {
      append(c_body) <- glue("UNPROTECT({n_protected});")
    }
    append(c_body) <- glue("return {return_var_c_name};")
  } else {
    return_var_values <- unname(return_var_names)
    return_var_c_names <- unname(map_chr(
      mget(return_var_values, scope),
      \(var) var@name
    ))
    provided_names <- names(return_var_names)
    if (is.null(provided_names)) {
      provided_names <- rep("", length(return_var_values))
    }
    has_any_names <- any(nzchar(provided_names))

    append(c_body) <- c(
      glue(
        "SEXP _ans = PROTECT(Rf_allocVector(VECSXP, {length(return_var_values)}));"
      ),
      imap(return_var_c_names, function(nm, i) {
        glue("SET_VECTOR_ELT(_ans, {i-1}, {nm});")
      })
    )

    if (has_any_names) {
      names_to_use <- provided_names
      append(c_body) <- c(
        glue(
          "SEXP _names = PROTECT(Rf_allocVector(STRSXP, {length(return_var_values)}));"
        ),
        imap(names_to_use, function(nm, i) {
          glue('SET_STRING_ELT(_names, {i-1}, Rf_mkChar("{nm}"));')
        }),
        "Rf_setAttrib(_ans, R_NamesSymbol, _names);"
      )
      append(c_body) <- glue("UNPROTECT({n_protected + 2});")
    } else {
      append(c_body) <- glue("UNPROTECT({n_protected + 1});")
    }

    append(c_body) <- "return _ans;"
  }

  c_args <- paste("SEXP", names(formals(closure)), collapse = ", ")
  c_body <- as_glue(str_flatten_lines(c_body))

  c_func_def <- glue("SEXP {fsub@name}_(SEXP _args) {c_block(c_body)}")

  fsub_extern_decl <- fsub_extern_decl(fsub)

  c_headers <- str_flatten_lines(
    "#define R_NO_REMAP",
    "#include <R.h>",
    "#include <Rinternals.h>",
    if (uses_rng) "#include <R_ext/Random.h>",
    "",
    ""
  )

  as_glue(str_flatten_lines(c(
    if (headers) c_headers,
    fsub_extern_decl,
    "",
    c_func_def
  )))
}


closure_arg_c_defs <- function(var, strict = TRUE) {
  name <- var@name
  mode <- var@mode

  c_code <- character()

  name <- var@name
  SEXPTYPE <- sexptype(var@mode)
  protect <- glue("SETCAR(_args, {var@name});")

  append(c_code) <- glue(
    "// {name}
    _args = CDR(_args);
    SEXP {var@name} = CAR(_args);"
  )

  # first maybe duplicate or coerce the SEXP if needed.
  append(c_code) <- glue("if (TYPEOF({name}) != {SEXPTYPE}) {{")
  append(c_code) <- indent(
    if (strict) {
      glue(
        r"(
      Rf_error("typeof({name}) must be '{mode}', not '%s'", Rf_type2char(TYPEOF({name})));
      )"
      )
    } else {
      glue(
        "{name} = Rf_coerceVector({name}, {SEXPTYPE});
         {protect}"
      )
    }
  )

  if (var@modified) {
    dup <- glue(
      '
      {name} = Rf_duplicate({name});
      {protect}
      '
    )

    if (strict) {
      append(c_code) <- c("}", dup)
    } else {
      append(c_code) <- sprintf("} else %s", dup)
    }
  } else {
    append(c_code) <- "}"
  }

  # define the variable that will be passed to the fsub
  append(c_code) <- glue(
    "{fsub_arg_var_c_type(var)} {name}__ = {sexpdata(var@mode)}({name});"
  )

  if (var@rank == 1) {
    size_name <- get_size_name(var)
    append(c_code) <- glue(
      "const R_xlen_t {size_name} = Rf_xlength({var@name});"
    )
  } else if (var@rank > 1) {
    append(c_code) <- glue(
      'const int* const {var@name}__dim_ = ({{
         SEXP dim_ = Rf_getAttrib({var@name}, R_DimSymbol);
         if (Rf_length(dim_) != {var@rank}) Rf_error(
           "{var@name} must be a {var@rank}D-array, but length(dim({var@name})) is %i",
           (int) Rf_length(dim_));
         INTEGER(dim_);}});'
    )
    append(c_code) <- map_chr(seq_len(var@rank), \(axis) {
      size_name <- get_size_name(var, axis)
      glue("const int {size_name} = {var@name}__dim_[{axis-1}];")
    })
  } else {
    stop("bad rank")
  }

  as_glue(str_flatten_lines(c_code))
}


closure_arg_size_checks <- function(var, scope, c_hoist = NULL) {
  imap(var@dims, function(d, axis) {
    # axis is either:
    #  - an integer
    #  - a symbol of a size_name
    #  - a call, consisting of only size_name symbols and basic arithmetic ops.
    size_name <- get_size_name(var, axis)

    if (is_scalar_integer(d)) {
      return(glue(
        '
          if ({size_name} != {d})
            Rf_error("{friendly_size(var, axis)} must be {d}, not %0.f",
                      (double){size_name});'
      ))
    }

    if (is.symbol(d)) {
      if (as.character(d) == size_name) {
        # self-named size_name is expected to be passed along to subroutine
        return()
      }

      d_name <- as.character(d)
      d_var <- get0(d_name, scope)
      if (inherits(d_var, Variable)) {
        d_expr <- as_c_name(d_var, c_hoist = c_hoist)
        d_label <- d_name
      } else if (is_size_name(d_name)) {
        d_expr <- d_name
        d_label <- as_friendly_size_name(d_name)
      } else {
        d_expr <- d_name
        d_label <- d_name
      }

      decls <- if (!is.null(c_hoist)) {
        c_bridge_hoist_take_pending(c_hoist)
      } else {
        character()
      }
      c_lines <- c(
        decls,
        glue(
          '
            if ({d_expr} != {size_name})
              Rf_error("{as_friendly_size_name(size_name)} must equal {d_label},"
                       " but are %0.f and %0.f",
                        (double){size_name}, (double){d_expr});'
        )
      )
      return(as_glue(str_flatten_lines(c_lines)))
    }

    if (is.call(d)) {
      size.c <- dims2c(list(d), scope, c_hoist = c_hoist)[[1L]]
      decls <- if (!is.null(c_hoist)) {
        c_bridge_hoist_take_pending(c_hoist)
      } else {
        character()
      }
      c_lines <- c(
        decls,
        glue(
          '{{
            const R_xlen_t expected = {size.c};
            if ({size_name} != expected)
              Rf_error("{as_friendly_size_name(size_name)} must equal {as_friendly_size_expression(d)},"
                       " but are %0.f and %0.f",
                        (double){size_name}, (double)expected);
          }}'
        )
      )
      return(as_glue(str_flatten_lines(c_lines)))
    }

    stop("bad dim")
  })
}


return_var_c_defs <- function(var, scope, c_hoist = NULL) {
  # allocate the return var.
  name <- var@name
  len_name <- get_size_name(var)
  c_dims <- dims2c(var@dims, scope, c_hoist = c_hoist)
  names(c_dims) <- NULL
  c_len <- c_dims2c_len(c_dims)
  decls <- if (!is.null(c_hoist)) {
    c_bridge_hoist_take_pending(c_hoist)
  } else {
    character()
  }

  c_code <- c(
    decls,
    glue("const R_xlen_t {len_name} = {c_len};"),
    glue(switch(
      var@mode,
      double = "
        SEXP {name} = PROTECT(Rf_allocVector(REALSXP, {len_name}));
        double* {name}__ = REAL({name});",
      integer = "
        SEXP {name} = PROTECT(Rf_allocVector(INTSXP, {len_name}));
        int* {name}__ = INTEGER({name});",
      complex = "
        SEXP {name} = PROTECT(Rf_allocVector(CPLXSXP, {len_name}));
        Rcomplex* {name}__ = COMPLEX({name});",
      logical = "
        SEXP {name} = PROTECT(Rf_allocVector(LGLSXP, {len_name}));
        int* {name}__ = LOGICAL({name});"
    ))
  )

  if (var@rank > 1) {
    append(c_code) <- c_block(
      glue(
        "
        const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, {var@rank}));
        int* const _dim = INTEGER(_dim_sexp);"
      ),
      imap(c_dims, function(d, i) {
        glue("_dim[{i-1}] = {d};")
      }),
      glue("Rf_dimgets({var@name}, _dim_sexp);")
    )
  }

  str_flatten_lines(c_code)
}


c_bridge_hoist <- function() {
  hoist <- new.env(parent = emptyenv())
  hoist$as_int <- new.env(parent = emptyenv())
  hoist$as_int_tmp <- new.env(parent = emptyenv())
  hoist$used_tmp <- new.env(parent = emptyenv())
  hoist$pending <- character()
  hoist
}

c_bridge_hoist_as_int <- function(hoist, sexp_name, expr) {
  stopifnot(is.environment(hoist), is_string(sexp_name), is_string(expr))

  existing <- get0(sexp_name, envir = hoist$as_int, inherits = FALSE)
  if (is_string(existing)) {
    return(existing)
  }

  tmp <- c_bridge_hoist_tmp_name(hoist, sexp_name)
  assign(sexp_name, tmp, envir = hoist$as_int)
  hoist$pending <- c(hoist$pending, glue("const int {tmp} = {expr};"))
  tmp
}

c_bridge_hoist_tmp_name <- function(hoist, sexp_name) {
  existing <- get0(sexp_name, envir = hoist$as_int_tmp, inherits = FALSE)
  if (is_string(existing)) {
    return(existing)
  }

  safe <- gsub("[^A-Za-z0-9_]", "_", sexp_name)
  if (!nzchar(safe) || !grepl("^[A-Za-z_]", safe)) {
    safe <- paste0("x_", safe)
  }

  tmp_base <- paste0("_as_int_", safe)
  tmp <- tmp_base
  i <- 1L
  while (isTRUE(get0(tmp, envir = hoist$used_tmp, inherits = FALSE))) {
    i <- i + 1L
    tmp <- paste0(tmp_base, "_", i)
  }
  assign(tmp, TRUE, envir = hoist$used_tmp)
  assign(sexp_name, tmp, envir = hoist$as_int_tmp)
  tmp
}

c_bridge_hoist_take_pending <- function(hoist) {
  stopifnot(is.environment(hoist))
  pending <- hoist$pending %||% character()
  hoist$pending <- character()
  pending
}


as_c_name <- function(var, c_hoist = NULL) {
  stopifnot(inherits(var, Variable))
  expr <- glue("Rf_asInteger({var@name})")
  if (is.null(c_hoist)) {
    return(expr)
  }
  c_bridge_hoist_as_int(c_hoist, var@name, expr)
}

dims2c_length_expr <- function(arg, scope) {
  if (!is.symbol(arg)) {
    stop("length() size expressions must refer to a symbol")
  }
  nm <- as.character(arg)
  var <- get0(nm, scope)
  if (!inherits(var, Variable)) {
    stop("could not resolve size: ", nm)
  }
  if (var@rank <= 0L) {
    return("1")
  }
  if (var@rank == 1L) {
    return(get_size_name(var))
  }
  dims <- map_chr(seq_len(var@rank), \(axis) get_size_name(var, axis))
  paste0("(", paste0(dims, collapse = " * "), ")")
}

dims2c_dim_index_expr <- function(cl, scope) {
  stopifnot(is.call(cl), identical(as.character(cl[[1L]]), "["))
  if (length(cl) != 3L || !is_call(cl[[2L]], quote(dim))) {
    stop("unsupported size expression: ", deparse1(cl))
  }
  dim_arg <- cl[[2L]][[2L]]
  if (!is.symbol(dim_arg)) {
    stop("dim() size expressions must refer to a symbol")
  }
  axis <- cl[[3L]]
  if (!is_wholenumber(axis)) {
    stop("dim(x)[axis] requires integer axis")
  }
  var <- get0(as.character(dim_arg), scope)
  if (!inherits(var, Variable)) {
    stop("could not resolve size: ", deparse1(cl))
  }
  if (axis > var@rank) {
    stop("insufficient rank of variable in ", deparse1(cl))
  }
  get_size_name(var, axis)
}

dims2c_expr <- function(e, scope, c_hoist = NULL) {
  if (is.null(e)) {
    return(NULL)
  }

  if (inherits(e, Variable)) {
    return(as_c_name(e, c_hoist = c_hoist))
  }

  if (is_scalar_integer(e)) {
    return(as.character(e))
  }

  if (is_wholenumber(e)) {
    return(as.character(as.integer(e)))
  }

  if (is.symbol(e)) {
    nm <- as.character(e)
    if (is_size_name(nm)) {
      return(nm)
    }
    var <- get0(nm, scope)
    if (!inherits(var, Variable) && inherits(scope, "quickr_scope")) {
      var <- scope_var_by_fortran_name(scope, nm)
    }
    if (!inherits(var, Variable)) {
      stop("could not resolve size: ", nm)
    }
    return(as_c_name(var, c_hoist = c_hoist))
  }

  if (!is.call(e)) {
    stop("unsupported size expression: ", deparse1(e))
  }

  op <- as.character(e[[1L]])
  args <- as.list(e)[-1L]

  if (identical(op, "(")) {
    if (length(args) != 1L) {
      stop("unsupported size expression: ", deparse1(e))
    }
    return(dims2c_expr(args[[1L]], scope, c_hoist = c_hoist))
  }

  if (identical(op, "length")) {
    if (length(args) != 1L) {
      stop("length() expects one argument")
    }
    return(dims2c_length_expr(args[[1L]], scope))
  }

  if (identical(op, "[")) {
    return(dims2c_dim_index_expr(e, scope))
  }

  if (identical(op, "nrow")) {
    if (length(args) != 1L) {
      stop("nrow() expects one argument")
    }
    return(dims2c_dim_index_expr(call("[", call("dim", args[[1L]]), 1L), scope))
  }

  if (identical(op, "ncol")) {
    if (length(args) != 1L) {
      stop("ncol() expects one argument")
    }
    return(dims2c_dim_index_expr(call("[", call("dim", args[[1L]]), 2L), scope))
  }

  if (identical(op, "abs")) {
    if (length(args) != 1L) {
      stop("abs() expects one argument")
    }
    e1 <- dims2c_expr(args[[1L]], scope, c_hoist = c_hoist)
    return(glue("(({e1}) < 0 ? -({e1}) : ({e1}))"))
  }

  if (op %in% c("+", "-", "*", "/", "%/%", "%%", "^")) {
    if (length(args) == 1L && op %in% c("+", "-")) {
      e1 <- dims2c_expr(args[[1L]], scope, c_hoist = c_hoist)
      return(glue("({op}({e1}))"))
    }
    if (length(args) != 2L) {
      stop("unsupported size expression: ", deparse1(e))
    }
    e1 <- dims2c_expr(args[[1L]], scope, c_hoist = c_hoist)
    e2 <- dims2c_expr(args[[2L]], scope, c_hoist = c_hoist)
    return(switch(
      op,
      `+` = glue("({e1} + {e2})"),
      `-` = glue("({e1} - {e2})"),
      `*` = glue("({e1} * {e2})"),
      `/` = glue("((double)({e1}) / (double)({e2}))"),
      `%/%` = glue("((R_xlen_t){e1} / (R_xlen_t){e2})"),
      `%%` = glue("((R_xlen_t){e1} % (R_xlen_t){e2})"),
      `^` = glue("({e1}**{e2})")
    ))
  }

  if (op %in% c("min", "max")) {
    if (!length(args)) {
      return("0")
    }
    rendered <- lapply(args, dims2c_expr, scope = scope, c_hoist = c_hoist)
    cmp <- if (identical(op, "min")) "<" else ">"
    reduce(rendered, \(a, b) glue("(({a}) {cmp} ({b}) ? ({a}) : ({b}))"))
  } else {
    stop("unsupported size expression: ", deparse1(e))
  }
}

dims2c <- function(dims, scope, c_hoist = NULL) {
  if (!length(dims) || identical(dims, list(1L))) {
    return(list(NULL, "1"))
  }
  lapply(dims, dims2c_expr, scope = scope, c_hoist = c_hoist)
}

c_dims2c_len <- function(c_dims) {
  if (length(c_dims) == 1) {
    c_dims[[1L]]
  } else {
    paste0("(", unlist(c_dims), ")", collapse = " * ")
  }
  # eval(Reduce(\(a, b) { call("*", as.symbol(a@name), as.symbol(b@name)) }, dims),
  #      eval_env)
}


# --- utils ----

c_block <- function(...) {
  as_glue(paste0(c("{", indent(c(...)), "}"), collapse = "\n"))
}

# is_var_size <- function(x) inherits(x, VariableSize)

passes_as_scalar <- function(var) {
  var@rank == 0 || var@rank == 1 && identical(var@dims, list(1L))
}

passes_as_value <- function(var) {
  passes_as_scalar(var) && isFALSE(var@modified)
}

sexptype <- function(mode) {
  switch(
    mode,
    integer = "INTSXP",
    double = "REALSXP",
    complex = "CPLXSXP",
    logical = "LGLSXP",
    stop("Unrecognized mode: ", mode)
  )
}

sexpdata <- function(mode) {
  switch(
    mode,
    integer = "INTEGER",
    double = "REAL",
    complex = "COMPLEX",
    logical = "LOGICAL",
    stop("Unrecognized mode: ", mode)
  )
}

is_size_name <- function(name) {
  if (is.symbol(name)) {
    name <- as.character(name)
  } else if (!is_string(name)) {
    return(FALSE)
  }

  grepl("(_len_|_dim_[0-9]+_)$", name)
}

friendly_size <- function(var, axis = NULL) {
  if (is.null(axis) || var@rank == 1 && axis == 1) {
    glue("length({var@name})")
  } else {
    glue("dim({var@name})[{axis}]")
  }
}

as_friendly_size_name <- function(size_name) {
  size_name <- as.character(size_name)
  if (endsWith(size_name, "__len_")) {
    sprintf("length(%s)", sub("__len_$", "", size_name))
  } else {
    sub("^(.*)__dim_([0-9]+)_$", "dim(\\1)[\\2]", size_name)
  }
}

as_friendly_size_expression <- function(d) {
  stopifnot(is.call(d))
  nms <- all.names(d, functions = FALSE, unique = TRUE)
  friendly_substitutions <- new.env(parent = emptyenv())
  for (name in nms) {
    if (is_size_name(name)) {
      assign(
        name,
        str2lang(as_friendly_size_name(name)),
        friendly_substitutions
      )
    }
  }
  d <- substitute_(d, friendly_substitutions)
  d <- call("(", d)
  deparse1(d)
}

closure_return_var_names <- function(closure) {
  return_var_expr <- last(body(closure))
  if (is.symbol(return_var_expr)) {
    val <- as.character(return_var_expr)
    # Return named to keep interface consistent
    return(setNames(val, val))
  }
  if (is_call(return_var_expr, quote(list))) {
    args <- as.list(return_var_expr)[-1L]
    if (length(args) == 0L) {
      stop("return list must contain at least one element")
    }
    vals <- map_chr(args, as.character)
    nms <- names(args)
    if (is.null(nms)) {
      nms <- rep("", length(vals))
    }
    # validate names are syntactic when provided
    if (any(nzchar(nms))) {
      bad <- nzchar(nms) & make.names(nms) != nms
      if (any(bad)) {
        stop(
          "only syntactic names are valid, encountered: ",
          paste0(nms[bad], sep = ", ")
        )
      }
    }
    # Use provided names when present; fallback to symbol names
    # nms <- ifelse(nzchar(nms), nms, vals)
    return(setNames(vals, nms))
  }
  ## is it redundent ? new_fortran_subroutine also errors ?
  stop("return value must be a symbol or list of symbols")
}


fsub_arg_var_c_type <- function(var) {
  type <- switch(
    var@mode,
    double = "double*",
    integer = "int*",
    complex = "Rcomplex*",
    logical = "int*",
  )

  # the first const declares that the pointed to values can't be modified
  #   (the array values are read only)
  # the second const declares that the pointer itself can't be modified
  #   (the fsub can never move/reallocate the array, so this const is always present)
  paste0(c(if (!var@modified) "const", type, "const"), collapse = " ")
}

fsub_extern_decl <- function(fsub) {
  fsub_arg_names <- fsub@signature # arg names
  scope <- fsub@scope

  fsub_c_sig <- map_chr(fsub_arg_names, function(name) {
    if (is_quickr_error_msg(name)) {
      return(glue("char* {name}"))
    }
    if (is_size_name(name)) {
      type <- if (name |> endsWith("__len_")) {
        "R_xlen_t"
      } else {
        "R_len_t"
      }
      glue("const {type} {name}")
    } else {
      var <- scope_var_by_fortran_name(scope, name)
      if (!inherits(var, Variable)) {
        stop("internal error: could not resolve variable: ", name)
      }
      glue("{fsub_arg_var_c_type(var)} {var@name}__")
    }
  })
  if (length(fsub_c_sig) >= 3L) {
    fsub_c_sig <- paste0("\n  ", fsub_c_sig)
  }

  glue("extern void {fsub@name}({str_flatten_commas(fsub_c_sig)});")
}
