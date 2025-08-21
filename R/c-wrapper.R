make_c_bridge <- function(fsub, strict = TRUE, headers = TRUE) {
  stopifnot(inherits(fsub, FortranSubroutine))

  closure <- fsub@closure
  scope <- fsub@scope

  fsub_arg_names <- fsub@signature # arg names
  closure_arg_names <- names(formals(closure))

  c_body <- character()

  if (!all(closure_arg_names %in% fsub_arg_names)) {
    stop(
      "Undeclared arguments: ",
      str_flatten_commas(setdiff(closure_arg_names, fsub_arg_names))
    )
  }

  closure_arg_vars <- mget(closure_arg_names, scope)

  # first unpack all the input vars into named C variables (including sizes and pointer)
  append(c_body) <- lapply(
    closure_arg_vars,
    closure_arg_c_defs,
    strict = strict
  ) |>
    rbind("")

  ## TODO, might still need to define a length size for vars where rank>1, if in checks.

  # now do all size checks.
  append(c_body) <- lapply(
    closure_arg_vars,
    closure_arg_size_checks,
    scope = scope
  )

  # maybe define and allocate the output var
  n_protected <- 0L
  return_var <- get(closure_return_var_name(closure), scope)
  if (!return_var@name %in% closure_arg_names) {
    return_var@modified <- TRUE
    assign(return_var@name, return_var, scope)
    append(c_body) <- return_var_c_defs(return_var, fsub@scope)
    add(n_protected) <- 1L # allocated return var
    if (return_var@rank > 1) {
      add(n_protected) <- 1L  # allocated _dim_sexp
    }
  }

  fsub_call_args <- fsub_arg_names |>
    lapply(\(nm) paste0(nm, if (!is_size_name(nm)) "__")) |>
    unlist()

  if (length(fsub_call_args) > 3) {
    fsub_call_args <- paste0("\n  ", fsub_call_args)
  }

  append(c_body) <- c(
    "",
    glue("{fsub@name}({str_flatten_commas(fsub_call_args)});"),
    ""
  )
  if (n_protected > 0) {
    append(c_body) <- glue("UNPROTECT({n_protected});")
  }
  append(c_body) <- glue("return {return_var@name};")

  c_args <- paste("SEXP", names(formals(closure)), collapse = ", ")
  c_body <- as_glue(str_flatten_lines(c_body))

  c_func_def <- glue("SEXP {fsub@name}_(SEXP _args) {c_block(c_body)}")

  fsub_extern_decl <- fsub_extern_decl(fsub)

  c_headers <- glue::trim(
    r"--(
     #define R_NO_REMAP
     #include <R.h>
     #include <Rinternals.h>


     )--"
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
      Rf_error("typeof({name}) must be '{mode}', not '%s'", R_typeToChar({name}));
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


closure_arg_size_checks <- function(var, scope) {
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
      } else {
        # it's a constraint for another size
        return(glue(
          '
            if ({d} != {size_name})
              Rf_error("{as_friendly_size_name(size_name)} must equal {as_friendly_size_name(d)},"
                       " but are %0.f and %0.f",
                        (double){size_name}, (double){d});'
        ))
      }
    }

    if (is.call(d)) {
      size.c <- dims2c(list(d), scope)
      return(glue(
        '{{
          const R_xlen_t expected = {size.c};
          if ({size_name} != expected)
            Rf_error("{as_friendly_size_name(size_name)} must equal {as_friendly_size_expression(d)},"
                     " but are %0.f and %0.f",
                      (double){size_name}, (double)expected);
        }}'
      ))
    }

    stop("bad dim")
  })
}


return_var_c_defs <- function(var, scope) {
  # allocate the return var.
  name <- var@name
  c_dims <- dims2c(var@dims, scope)
  c_len <- c_dims2c_len(c_dims)
  len_name <- get_size_name(var)

  c_code <- c(
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


dims2c_eval_base_env <- new.env()


dims2c_eval_base_env[["("]] <- baseenv()[["("]]
dims2c_eval_base_env[["+"]] <- function(e1, e2) glue("({e1} + {e2})")
dims2c_eval_base_env[["-"]] <- function(e1, e2) glue("({e1} - {e2})")
dims2c_eval_base_env[["*"]] <- function(e1, e2) glue("({e1} * {e2})")
dims2c_eval_base_env[["/"]] <- function(e1, e2) {
  glue("((double)({e1}) / (double)({e2}))")
}
# dividing integers truncates towards 0
dims2c_eval_base_env[["%/%"]] <- function(e1, e2) {
  glue("((R_xlen_t){e1} / (R_xlen_t){e2})")
}
dims2c_eval_base_env[["%%"]] <- function(e1, e2) {
  glue("((R_xlen_t){e1} % (R_xlen_t){e2})")
}
dims2c_eval_base_env[["^"]] <- function(e1, e2) glue("({e1}**{e2})")


dims2c <- function(dims, scope) {
  if (!length(dims) || identical(dims, list(1L))) {
    return(list(NULL, "1"))
  }

  syms <- as.character(unique(unlist(lapply(dims, all.vars))))

  syms <- mget(syms, scope, ifnotfound = syms) |>
    lapply(function(var) {
      if (is_size_name(var)) {
        return(as.character(var))
      }
      # resolve a variable from scope (i.e., some other arg var)
      if (!inherits(var, Variable)) {
        stop("could not resolve size: ", var)
      }
      glue("Rf_asInteger({var@name})")
      # Should this be as double?
      # TODO: force this into a named c var, to avoid repeated calls
    })

  eval_env <- list2env(syms, parent = dims2c_eval_base_env)
  c_dims <- lapply(dims, function(d) {
    if (inherits(d, Variable)) {
      return(glue("Rf_asInteger({d@name})"))
    }
    eval(d, eval_env)
  })

  c_dims
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

closure_return_var_name <- function(closure) {
  return_var_name <- last(body(closure))
  if (!is.symbol(return_var_name)) {
    stop("return value must be a symbol")
  }
  as.character(return_var_name)
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
    if (is_size_name(name)) {
      type <- if (name |> endsWith("__len_")) {
        "R_xlen_t"
      } else {
        "R_len_t"
      }
      glue("const {type} {name}")
    } else {
      var <- get(name, fsub@scope)
      glue("{fsub_arg_var_c_type(var)} {var@name}__")
    }
  })
  if (length(fsub_c_sig) >= 3L) {
    fsub_c_sig <- paste0("\n  ", fsub_c_sig)
  }

  glue("extern void {fsub@name}({str_flatten_commas(fsub_c_sig)});")
}
