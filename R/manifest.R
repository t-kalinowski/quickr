### local variables with unspecified size are 'allocatable'. If they are bound
### to a named symbol, the manifest must mark it as allocatable.
###
### Generally, if an expression produces an array of unspecified size, even if
### it's never bound, it's still 'allocatable'. For example, an inline fortran
### `pack()` call likely still produces a corresponding `malloc()` in the
### generated code, regardless of if the output of `pack()` is bound to
### a symbol (in the case of pack specifically, the malloc is behind a
### _gfortran_pack() call.
###
### We can potentially link/mask `_malloc` and `_free` with a custom one that
### uses R_alloc(), which will automatically free after the .External() call
### returns. We can also pass along -fstack-arrays to gfortran and flang-new
### (llvm), and that will mostly get rid most of the malloc calls, instead
### allocating arrays on the C stack (which will automatically free on
### return/lngjmp), but that will run into issues with larger arrays (especially
### on windows)
###
### local vars of undefined sizes are allocatable. These will typically be
### allocated on the c stack if they are not too large, but may include a
### malloc+free call if they are large. Those might leak if we lngjmp
### away (e.g., due to an interrupt). This potential leak is a non-issue for
### now, since interrupts aren't supported yet, so there is no risk of lngjmp.
###
### When we do add support for interruptable quick functions, this potential
### leak could be guarded against by:
###
### a) linking malloc -> R_alloc() for the fortran compilation unit which
### would make the memory automatically be released after .External()
### return. Note that unlinke malloc(), R_alloc() is not thread safe, so we would need
### additional work for a `do concurrent` context to be supported.
###
### b) forcing all arrays to be stack allocated with -fstack-arrays passed
### to the gfortran/flang-new. This is not a great, since c stack limits are
### typically "small" and enforced by the OS.

logical_as_int <- function(var) {
  stopifnot(inherits(var, Variable))
  identical(var@mode, "logical") && isTRUE(var@logical_as_int)
}

block_tmp_allocatable_threshold <- 16L

subroutine_local_allocatable_threshold_bytes <- 256L * 1024L

var_element_count <- function(var) {
  stopifnot(inherits(var, Variable))
  dims <- var@dims
  stopifnot(is.list(dims), length(dims) > 0L)
  sizes <- vapply(
    dims,
    function(axis) {
      if (is.integer(axis) && length(axis) == 1L && !is.na(axis)) {
        axis
      } else {
        NA_integer_
      }
    },
    integer(1)
  )
  if (anyNA(sizes)) {
    return(NA_integer_)
  }
  prod(as.numeric(sizes))
}

block_tmp_element_count <- function(var) {
  var_element_count(var)
}

var_storage_bytes <- function(var) {
  stopifnot(inherits(var, Variable))
  switch(
    var@mode,
    double = 8,
    integer = 4,
    complex = 16,
    logical = 4,
    raw = 1,
    stop("var_storage_bytes() does not support mode: ", var@mode)
  )
}

subroutine_local_allocatable <- function(
  var,
  scope,
  max_stack_bytes = subroutine_local_allocatable_threshold_bytes
) {
  stopifnot(inherits(var, Variable))
  if (passes_as_scalar(var) || is.null(var@dims)) {
    return(FALSE)
  }

  # For declarations like `type(a = double(NA, NA))`, substitute_declared_sizes()
  # rewrites NA axes to `a__dim_*` symbols. Those sizes are not available for
  # explicit allocation, so treat these as implicitly-sized locals.
  self_size_names <- vapply(
    seq_along(var@dims),
    function(i) get_size_name(var, axis = as.integer(i)),
    character(1)
  )
  if (
    any(vapply(
      seq_along(var@dims),
      function(i) {
        d <- var@dims[[i]]
        is.symbol(d) && identical(as.character(d), self_size_names[[i]])
      },
      logical(1)
    ))
  ) {
    return(FALSE)
  }

  # If any dimension is deferred-shape (":"), don't emit an explicit allocate().
  # Those locals are expected to be allocated implicitly (e.g., on assignment).
  dims <- dims2f(var@dims, scope)
  if (!nzchar(dims) || grepl(":", dims, fixed = TRUE)) {
    return(FALSE)
  }

  n_elements <- var_element_count(var)
  if (is.na(n_elements)) {
    return(TRUE)
  }

  as.numeric(n_elements) * var_storage_bytes(var) > max_stack_bytes
}

block_tmp_allocatable <- function(
  var,
  scope,
  max_stack_elements = block_tmp_allocatable_threshold
) {
  stopifnot(inherits(var, Variable))
  if (!inherits(scope, "quickr_scope") || !identical(scope_kind(scope), "block")) {
    return(FALSE)
  }
  if (passes_as_scalar(var) || is.null(var@dims)) {
    return(FALSE)
  }

  dims <- dims2f(var@dims, scope)
  if (!nzchar(dims) || grepl(":", dims, fixed = TRUE)) {
    return(FALSE)
  }

  n_elements <- block_tmp_element_count(var)
  is.na(n_elements) || n_elements > max_stack_elements
}

block_tmp_allocation_lines <- function(vars, scope) {
  stopifnot(is.list(vars))
  allocs <- lapply(vars, function(var) {
    if (!block_tmp_allocatable(var, scope)) {
      return(NULL)
    }
    dims <- dims2f(var@dims, scope)
    glue("allocate({var@name}({dims}))")
  })
  unlist(allocs, use.names = FALSE)
}

scope_vars <- function(scope) {
  vars <- as.list(scope)
  keep(vars, inherits, what = Variable)
}

scope_var_by_fortran_name <- function(scope, name) {
  stopifnot(inherits(scope, "quickr_scope"), is_string(name))
  vars <- scope_vars(scope)
  var_names <- map_chr(vars, \(var) var@name %||% "")
  idx <- match(tolower(name), tolower(var_names))
  if (is.na(idx)) {
    return(NULL)
  }
  vars[[idx]]
}

iso_c_binding_symbols <- function(
  vars,
  body_code = "",
  logical_is_c_int = logical_as_int,
  uses_rng = FALSE,
  include_errors = FALSE
) {
  stopifnot(is.list(vars), is_string(body_code), is.function(logical_is_c_int))

  used_iso_bindings <- unique(unlist(
    use.names = FALSE,
    lapply(vars, function(var) {
      stopifnot(inherits(var, Variable))
      list(
        switch(
          var@mode,
          double = "c_double",
          integer = "c_int",
          complex = "c_double_complex",
          logical = if (isTRUE(logical_is_c_int(var))) "c_int",
          raw = "c_int8_t",
          stop("unrecognized kind: ", format(var))
        ),
        lapply(var@dims, function(size) {
          syms <- all.vars(size)
          c(
            if (any(grepl("__len_$", syms))) "c_ptrdiff_t",
            if (any(grepl("__dim_[0-9]+_$", syms))) "c_int"
          )
        })
      )
    })
  ))

  # check for literal kinds used in the body
  if (grepl("\\b[0-9]+_c_int\\b", body_code)) {
    used_iso_bindings <- union(used_iso_bindings, "c_int")
  }
  if (grepl("\\bc_int\\b", body_code)) {
    used_iso_bindings <- union(used_iso_bindings, "c_int")
  }
  if (grepl("\\b[0-9]+\\.[0-9]+_c_double\\b", body_code)) {
    used_iso_bindings <- union(used_iso_bindings, "c_double")
  }
  if (grepl("\\bc_ptrdiff_t\\b", body_code)) {
    used_iso_bindings <- union(used_iso_bindings, "c_ptrdiff_t")
  }

  if (isTRUE(uses_rng)) {
    used_iso_bindings <- union(used_iso_bindings, "c_double")
  }

  if (isTRUE(include_errors)) {
    used_iso_bindings <- union(
      used_iso_bindings,
      c("c_char", "c_null_char")
    )
  }

  used_iso_bindings |>
    compact() |>
    unique() |>
    sort(method = "radix")
}

emit_decl_line <- function(
  var,
  scope,
  intent = NULL,
  assumed_shape = FALSE,
  allow_allocatable = TRUE
) {
  stopifnot(inherits(var, Variable))
  if (isTRUE(var@host_associated)) {
    return(NULL)
  }

  type <- switch(
    var@mode,
    double = "real(c_double)",
    integer = "integer(c_int)",
    complex = "complex(c_double_complex)",
    logical = if (logical_as_int(var)) "integer(c_int)" else "logical",
    raw = "integer(c_int8_t)",
    stop("unrecognized kind: ", format(var))
  )

  # Block-scoped temporaries are explicitly marked allocatable so we can
  # allocate them on the heap rather than relying on compiler defaults.
  # GFortran already heap-allocates large/unknown-size locals implicitly,
  # but flang lowers block locals to `alloca` and will stack-allocate even
  # large runtime shapes, which can segfault under typical stack limits.
  # We keep small, fixed-size temps (<= 16 elements) as automatic arrays
  # to avoid allocation overhead and leave those to the compiler.
  block_allocatable <- allow_allocatable && block_tmp_allocatable(var, scope)

  dims <- if (passes_as_scalar(var)) {
    NULL
  } else if (block_allocatable) {
    sprintf("(%s)", str_flatten_commas(rep(":", var@rank)))
  } else if (assumed_shape) {
    sprintf("(%s)", str_flatten_commas(rep(":", var@rank)))
  } else {
    dims2f(var@dims, scope) |> str_flatten_commas() |> sprintf(fmt = "(%s)")
  }

  allocatable <- if (block_allocatable) {
    "allocatable"
  } else if (
    allow_allocatable &&
      !assumed_shape &&
      !is.null(dims) &&
      grepl(":", dims, fixed = TRUE)
  ) {
    "allocatable"
  }

  name <- var@name
  comment <- if (var@mode == "logical") " ! logical"

  glue(
    '{str_flatten_commas(type, intent, allocatable)} :: {name}{dims}{comment}',
    .null = ""
  )
}

emit_decls <- function(
  vars,
  scope,
  intents = NULL,
  assumed_shape = FALSE,
  allow_allocatable = TRUE
) {
  stopifnot(is.list(vars))
  if (is.null(intents)) {
    intents <- rep(list(NULL), length(vars))
    names(intents) <- names(vars)
  }
  Map(
    f = emit_decl_line,
    var = vars,
    intent = intents,
    MoreArgs = list(
      scope = scope,
      assumed_shape = assumed_shape,
      allow_allocatable = allow_allocatable
    )
  ) |>
    unlist(use.names = FALSE)
}

emit_block <- function(decls, stmts) {
  decls <- unlist(decls, use.names = FALSE)
  stmts <- unlist(stmts, use.names = FALSE)
  glue::trim(glue(
    "
    block
    {indent(str_flatten_lines(decls, \"\", stmts))}
    end block
    "
  ))
}

r2f.scope <- function(scope, include_errors = FALSE) {
  return_var_names <- unname(scope_return_var_names(scope))
  vars <- scope_vars(scope)

  local_allocs <- character()
  vars <- lapply(vars, function(var) {
    r_name <- var@r_name %||% var@name
    intent_in <- r_name %in% names(formals(scope_closure(scope)))
    intent_out <-
      (r_name %in% return_var_names) ||
      (intent_in && var@modified)

    intent <-
      if (intent_in && intent_out) {
        "intent(in out)"
      } else if (intent_in) {
        "intent(in)"
      } else if (intent_out) {
        "intent(out)"
      } else {
        NULL
      }

    type <- switch(
      var@mode,
      double = "real(c_double)",
      integer = "integer(c_int)",
      complex = "complex(c_double_complex)",
      logical = if (logical_as_int(var)) "integer(c_int)" else "logical",
      raw = "integer(c_int8_t)",
      stop("unrecognized kind: ", format(var))
    )

    dims <- if (passes_as_scalar(var)) {
      NULL
    } else {
      dims2f(var@dims, scope) |> str_flatten_commas() |> sprintf(fmt = "(%s)")
    }

    # In subroutines, locals declared with unspecified dims (NA -> `a__dim_*`)
    # are emitted as deferred-shape allocatables and rely on implicit allocation.
    if (
      is.null(intent) &&
        !is.null(dims) &&
        any(vapply(
          seq_along(var@dims),
          function(i) {
            d <- var@dims[[i]]
            is.symbol(d) &&
              identical(as.character(d), get_size_name(var, axis = i))
          },
          logical(1)
        ))
    ) {
      dims <- sprintf("(%s)", str_flatten_commas(rep(":", var@rank)))
    }

    heap_local <- is.null(intent) && subroutine_local_allocatable(var, scope)
    if (isTRUE(heap_local)) {
      # Deferred-shape allocatable avoids large stack allocations (notably flang).
      dims <- sprintf("(%s)", str_flatten_commas(rep(":", var@rank)))
      local_allocs <<- c(
        local_allocs,
        glue("allocate({var@name}({dims2f(var@dims, scope)}))")
      )
    }

    allocatable <- if (isTRUE(heap_local)) {
      "allocatable"
    } else if (!is.null(dims) && grepl(":", dims, fixed = TRUE)) {
      "allocatable"
    }

    if (intent_in && intent_out && !is.null(allocatable)) {
      stop("all input and output vars must have a fully defined shape")
    }

    name <- var@name
    comment <- if (var@mode == "logical") " ! logical"

    glue(
      '{str_flatten_commas(type, intent, allocatable)} :: {name}{dims}{comment}',
      .null = ""
    )
  })

  # vars that will be visible in the C bridge, either as an input or output
  non_local_var_names <- unique(c(
    names(formals(scope_closure(scope))),
    return_var_names
  ))

  # collect all size_names; sort so non-locals are declared first.
  size_names <- unique(unlist(lapply(non_local_var_names, function(name) {
    var <- scope[[name]]
    lapply(var@dims, all.names, functions = FALSE, unique = TRUE)
  }))) |>
    setdiff(names(formals(scope_closure(scope))))
  if (length(names(formals(scope_closure(scope))))) {
    formal_vars <- mget(names(formals(scope_closure(scope))), scope)
    formal_fortran_names <- unique(map_chr(formal_vars, \(var) {
      var@name %||% ""
    }))
    formal_fortran_names <- formal_fortran_names[nzchar(formal_fortran_names)]
    size_names <- setdiff(size_names, formal_fortran_names)
  }
  if (is.null(size_names)) {
    size_names <- character()
  }

  sizes <- lapply(size_names, function(name) {
    kind <- if (endsWith(name, "_len_")) "c_ptrdiff_t" else "c_int"
    glue("integer({kind}), intent(in), value :: {name}")
  })

  manifest <- compact(list(
    sizes = sizes,
    error = if (isTRUE(include_errors)) quickr_error_manifest_lines(),
    args = vars[non_local_var_names],
    locals = vars[setdiff(names(vars), non_local_var_names)]
  ))

  manifest <- imap(manifest, \(declarations, category) {
    str_flatten_lines(paste("!", category), declarations)
  }) |>
    str_flatten("\n\n")

  manifest <- str_flatten_lines("! manifest start", manifest, "! manifest end")
  attr(manifest, "local_allocations") <- local_allocs

  # symbols that must come in as args to the subroutine
  # # method="radix" for locale-independent stable order.
  signature <- unique(c(
    map_chr(mget(non_local_var_names, scope), \(var) var@name),
    sort(size_names, method = "radix"),
    if (isTRUE(include_errors)) quickr_error_arg_names()
  ))
  attr(manifest, "signature") <- signature

  manifest
}


## fortran precedence order
##   ** (exp)
##   * /
##   + -
##
## R prededence order
##   ^
##   - +
##   %/% %%
##   * /

## generally, we just deparse() to convert an axis size.
## except for NA, which becomes ":"

dims2f_eval_base_env <- new.env(parent = emptyenv())
dims2f_eval_base_env[["("]] <- baseenv()[["("]]

# any call always evaluates to a string.
# every argument will be either:
# - NA  -> translates to ":"
# - a symbol -> translates to deparsed string
# - a call  ->

dims2f_eval_base_env[["+"]] <- function(e1, e2) glue("({e1} + {e2})")
dims2f_eval_base_env[["-"]] <- function(e1, e2) glue("({e1} - {e2})")
dims2f_eval_base_env[["*"]] <- function(e1, e2) glue("({e1} * {e2})")
dims2f_eval_base_env[["/"]] <- function(e1, e2) glue("real({e1}) / real({e2})")
# dividing integers truncates towards 0
dims2f_eval_base_env[["%/%"]] <- function(e1, e2) glue("int({e1}) / int({e2})")
dims2f_eval_base_env[["%%"]] <- function(e1, e2) {
  glue("mod(int({e1}), int({e2}))")
}
dims2f_eval_base_env[["^"]] <- function(e1, e2) glue("({e1})**({e2})")
dims2f_eval_base_env[["abs"]] <- function(x) glue("abs({x})")
dims2f_eval_base_env[["length"]] <- function(x) {
  if (is.symbol(x)) {
    glue("size({as.character(x)})")
  } else {
    glue("size({x})")
  }
}
dims2f_eval_base_env[["nrow"]] <- function(x) {
  if (is.symbol(x)) {
    glue("size({as.character(x)}, 1)")
  } else {
    glue("size({x}, 1)")
  }
}
dims2f_eval_base_env[["ncol"]] <- function(x) {
  if (is.symbol(x)) {
    glue("size({as.character(x)}, 2)")
  } else {
    glue("size({x}, 2)")
  }
}
dims2f_eval_base_env[["dim"]] <- function(x) x
dims2f_eval_base_env[["["]] <- function(x, i) {
  if (!is_wholenumber(i)) {
    stop("dim(x)[axis] requires integer axis")
  }
  if (is.symbol(x)) {
    glue("size({as.character(x)}, {as.integer(i)})")
  } else {
    glue("size({x}, {as.integer(i)})")
  }
}
dims2f_eval_base_env[["min"]] <- function(...) {
  args <- list(...)
  glue("min({str_flatten_commas(args)})")
}
dims2f_eval_base_env[["max"]] <- function(...) {
  args <- list(...)
  glue("max({str_flatten_commas(args)})")
}


dims2f <- function(dims, scope) {
  syms <- unique(unlist(lapply(dims, \(d) if (is.language(d)) all.vars(d))))
  vars <- lapply(syms, function(sym) {
    scope_fortran_symbol(as.symbol(sym), scope)
  })
  names(vars) <- syms
  eval_env <- list2env(vars, parent = dims2f_eval_base_env)
  dims <- map_chr(dims, function(d) {
    d <- eval(d, eval_env)
    if (is.symbol(d)) {
      as.character(d)
    } else if (is_wholenumber(d)) {
      as.character(d)
    } else if (is_scalar_na(d)) {
      ":"
    } else if (is_string(d)) {
      d
    } else if (inherits(d, Variable)) {
      # a locally allocated var that is a return var
      if (!d@modified && d@is_arg) {
        return(d@name)
      }
      stop("unexpected axis size value")
    }
  })
  if (!length(dims) || identical(dims, "1")) {
    ""
  } else {
    str_flatten_commas(dims)
  }
}
