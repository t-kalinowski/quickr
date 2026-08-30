# r2f-aab-core.R
# Core translation engine for R-to-Fortran conversion.
#
# Take parsed R code (anything returnable by base::str2lang()) and returns
# a Fortran object, which is a string of Fortran code and some attributes
# describing the value.

# --- Hoisting Infrastructure ---

stop_deferred_branch_error <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("quickr_deferred_branch_error", "error", "condition")
  ))
}

stop_static_mode_error <- function(message, hoist) {
  if (!is.null(hoist) && isTRUE(hoist$defer_static_mode_error)) {
    stop_deferred_branch_error(message)
  }
  stop(message, call. = FALSE)
}

new_hoist <- function(scope) {
  hoisted <- character()
  has_runtime_guard <- FALSE
  block_scope <- NULL
  point_allocated <- character()

  emit <- function(...) {
    hoisted <<- c(
      hoisted,
      as.character(unlist(c(character(), ...), use.names = FALSE))
    )
  }

  mark_runtime_guard <- function() {
    has_runtime_guard <<- TRUE
    invisible()
  }

  contains_runtime_guard <- function() has_runtime_guard

  has_block <- function() !is.null(block_scope)

  # TRUE when render(code) would return `code` unchanged: nothing emitted,
  # no block-scoped temporaries declared.
  is_empty <- function() !length(hoisted) && !has_block()

  ensure_block_scope <- function() {
    if (is.null(block_scope)) {
      block_scope <<- scope_new_child(scope, "block")
    }
    block_scope
  }

  declare_tmp <- function(mode, dims, logical_as_int = FALSE) {
    stopifnot(
      is_string(mode),
      is.null(dims) || is.list(dims),
      is_bool(logical_as_int)
    )
    ensure_block_scope()@get_unique_var(
      mode = mode,
      dims = dims,
      logical_as_int = logical_as_int
    )
  }

  tmp_allocation_line <- function(var) {
    local_var <- if (is.null(block_scope)) {
      NULL
    } else {
      scope_var_by_fortran_name(block_scope, var@name)
    }
    if (is.null(local_var)) {
      return(character())
    }
    if (!block_tmp_allocatable(var, block_scope)) {
      return(character())
    }
    if (var@name %in% point_allocated) {
      return(character())
    }
    point_allocated <<- c(point_allocated, var@name)
    glue(
      "allocate({var@name}({dims2f(var@dims, block_scope)}))"
    )
  }

  allocate_tmp_at_point <- function(var, emit_at_point) {
    line <- tmp_allocation_line(var)
    if (length(line)) {
      emit_at_point(line)
    }
    var
  }

  allocate_existing_tmp_at_point <- function(var) {
    line <- tmp_allocation_line(var)
    if (length(line)) {
      first_use <- which(grepl(var@name, hoisted, fixed = TRUE))[[1L]]
      hoisted <<- append(hoisted, line, after = first_use - 1L)
    }
    var
  }

  declare_tmp_at_point <- function(mode, dims, logical_as_int = FALSE) {
    var <- declare_tmp(mode, dims, logical_as_int)
    allocate_tmp_at_point(var, emit)
  }

  capture <- function() {
    captured <- character()
    captured_runtime_guard <- FALSE
    capture_emit <- function(...) {
      captured <<- c(
        captured,
        as.character(unlist(c(character(), ...), use.names = FALSE))
      )
    }
    capture_render <- function(code) {
      str_flatten_lines(str_split_lines(captured, code))
    }
    capture_has_code <- function() length(captured) > 0L
    capture_allocate_existing_tmp_at_point <- function(var) {
      line <- tmp_allocation_line(var)
      if (length(line)) {
        first_use <- which(grepl(var@name, captured, fixed = TRUE))[[1L]]
        captured <<- append(captured, line, after = first_use - 1L)
      }
      var
    }
    capture_declare_tmp_at_point <- function(
      mode,
      dims,
      logical_as_int = FALSE
    ) {
      var <- declare_tmp(mode, dims, logical_as_int)
      allocate_tmp_at_point(var, capture_emit)
    }
    capture_mark_runtime_guard <- function() {
      captured_runtime_guard <<- TRUE
      invisible()
    }
    capture_contains_runtime_guard <- function() captured_runtime_guard
    list2env(
      list(
        emit = capture_emit,
        declare_tmp = declare_tmp,
        declare_tmp_at_point = capture_declare_tmp_at_point,
        allocate_tmp_at_point = capture_allocate_existing_tmp_at_point,
        render = capture_render,
        has_code = capture_has_code,
        mark_runtime_guard = capture_mark_runtime_guard,
        contains_runtime_guard = capture_contains_runtime_guard,
        capture = capture
      ),
      parent = emptyenv()
    )
  }

  render <- function(code) {
    code <- str_split_lines(code)
    if (is_empty()) {
      return(str_flatten_lines(code))
    }

    stmts <- str_split_lines(hoisted, code)

    if (has_block()) {
      block_vars <- scope_vars(block_scope)
      decls <- emit_decls(block_vars, block_scope)
      prologue_vars <- keep(
        block_vars,
        \(var) !var@name %in% point_allocated
      )
      allocs <- block_tmp_allocation_lines(prologue_vars, block_scope)
      if (length(allocs)) {
        stmts <- c(allocs, stmts)
      }
      return(str_flatten_lines(emit_block(decls, stmts)))
    }

    str_flatten_lines(stmts)
  }

  list2env(
    list(
      emit = emit,
      declare_tmp = declare_tmp,
      is_empty = is_empty,
      declare_tmp_at_point = declare_tmp_at_point,
      allocate_tmp_at_point = allocate_existing_tmp_at_point,
      render = render,
      mark_runtime_guard = mark_runtime_guard,
      contains_runtime_guard = contains_runtime_guard,
      capture = capture
    ),
    parent = emptyenv()
  )
}

# Materialize `code` into a hoisted temporary and return the temporary.
# `hoist` is always available in a handler: r2f() opens one per statement
# before dispatching, and every caller forwards the one it received.
# Used by: hoist_unless_name(), r2f-constructors.R, r2f-subscript.R,
#          r2f-rev.R
materialize_via_hoist <- function(
  code,
  mode,
  dims,
  hoist,
  logical_storage = NULL,
  allocate_at_point = FALSE
) {
  stopifnot(is.environment(hoist), is_bool(allocate_at_point))
  if (is.null(logical_storage)) {
    logical_storage <- inherits(code, Fortran) &&
      inherits(code@value, Variable) &&
      logical_as_int(code@value) &&
      !isTRUE(code@logical_booleanized)
  }
  declare_tmp <- if (allocate_at_point) {
    hoist$declare_tmp_at_point
  } else {
    hoist$declare_tmp
  }
  tmp <- declare_tmp(
    mode = mode,
    dims = dims,
    logical_as_int = logical_storage
  )
  hoist$emit(glue("{tmp@name} = {code}"))
  Fortran(tmp@name, tmp)
}

capture_hoist <- function(hoist) {
  stopifnot(inherits(hoist, "environment"))
  captured_hoist <- hoist$capture()
  captured_hoist$defer_static_shape_error <- isTRUE(
    hoist$defer_static_shape_error
  )
  captured_hoist$defer_builtin_arity_error <- isTRUE(
    hoist$defer_builtin_arity_error
  )
  captured_hoist$defer_static_mode_error <- isTRUE(
    hoist$defer_static_mode_error
  )
  captured_hoist
}

# Hoist `x` into a temporary variable unless it already renders as a bare
# variable name or a literal constant. Use this whenever the same operand is
# spliced into generated code more than once: Fortran evaluates intrinsic
# actual arguments before the call, so repeating an expression duplicates
# its side effects (e.g. RNG state via runif()) -- which names and literals
# don't have.
hoist_unless_name <- function(
  x,
  hoist,
  allocate_at_point = FALSE,
  force = FALSE
) {
  stopifnot(
    inherits(x, Fortran),
    inherits(x@value, Variable),
    is_bool(allocate_at_point),
    is_bool(force)
  )
  code <- trimws(as.character(x))
  if (
    !force &&
      !is.null(x@value@name) &&
      identical(code, x@value@name)
  ) {
    if (allocate_at_point) {
      hoist$allocate_tmp_at_point(x@value)
    }
    return(x)
  }
  if (
    !force &&
      passes_as_scalar(x@value) &&
      grepl("^-?[0-9]+(\\.[0-9]+)?(_c_(int|double))?$", code)
  ) {
    return(x)
  }
  materialize_via_hoist(
    x,
    mode = x@value@mode,
    dims = x@value@dims,
    hoist = hoist,
    logical_storage = logical_as_int(x@value) &&
      !isTRUE(x@logical_booleanized),
    allocate_at_point = allocate_at_point
  )
}

# Name of the call one frame above the current handler ("" at top level).
# Materialization decisions branch on it: a fill constructor or
# matrix(scalar, ...) may stay a scalar only where the parent broadcasts,
# spreads, or pads it.
# Used by: r2f-constructors.R
parent_call_name <- function(calls) {
  if (length(calls) >= 2L) calls[[length(calls) - 1L]] else ""
}

# Replay statements captured while lowering one operand before the next
# operand is lowered. runif() is the only effectful expression that remains
# inline; materialize it so its RNG effect also happens at this point.
finish_captured_operand <- function(operand, captured_hoist, hoist) {
  stopifnot(
    inherits(operand, Fortran),
    inherits(captured_hoist, "environment"),
    inherits(hoist, "environment")
  )

  if (captured_hoist$contains_runtime_guard()) {
    hoist$mark_runtime_guard()
  }

  if (!inherits(operand@value, Variable)) {
    if (captured_hoist$has_code()) {
      hoist$emit(captured_hoist$render(character()))
    }
    return(operand)
  }

  if (grepl("unif_rand()", as.character(operand), fixed = TRUE)) {
    tmp <- hoist$declare_tmp(
      mode = operand@value@mode,
      dims = operand@value@dims,
      logical_as_int = logical_as_int(operand@value) &&
        !isTRUE(operand@logical_booleanized)
    )
    hoist$emit(captured_hoist$render(glue("{tmp@name} = {operand}")))
    return(Fortran(tmp@name, tmp))
  }
  if (captured_hoist$has_code()) {
    hoist$emit(captured_hoist$render(character()))
  }
  operand
}

snapshot_operand_before_later_effects <- function(
  operand,
  arg,
  later_args,
  scope,
  hoist
) {
  stopifnot(is.list(later_args), inherits(scope, "quickr_scope"))
  if (!inherits(operand@value, Variable)) {
    return(operand)
  }
  mutation_names <- unique(unlist(lapply(
    later_args,
    r2f_expression_host_mutations,
    scope = scope
  )))
  read_names <- if (is.null(arg)) {
    character()
  } else {
    all.names(arg, functions = FALSE, unique = TRUE)
  }
  operand_code <- trimws(as.character(operand))
  already_materialized <- !is.null(operand@value@name) &&
    identical(operand_code, operand@value@name) &&
    operand@value@name %in% scope_generated_fortran_names(scope)
  if (
    !length(later_args) ||
      is.null(arg) ||
      is_scalar_atomic(arg) ||
      already_materialized ||
      !any(read_names %in% mutation_names)
  ) {
    return(operand)
  }
  hoist_unless_name(
    operand,
    hoist,
    allocate_at_point = TRUE,
    force = TRUE
  )
}

r2f_expression_host_mutations <- function(
  e,
  scope,
  seen = character(),
  local_functions = list()
) {
  if (!is.call(e)) {
    return(character())
  }

  if (is_call(e, quote(`{`))) {
    for (child in as.list(e)[-1L]) {
      child <- unwrap_parens(child)
      if (
        (is_call(child, quote(`<-`)) || is_call(child, quote(`=`))) &&
          length(child) == 3L &&
          is.symbol(child[[2L]]) &&
          is_function_call(child[[3L]])
      ) {
        local_functions[[as.character(child[[2L]])]] <- child[[3L]]
      }
    }
  }

  callable <- e[[1L]]
  while (is_call(callable, quote(`(`)) && length(callable) == 2L) {
    callable <- callable[[2L]]
  }
  op <- if (is.symbol(callable)) as.character(callable) else NULL
  if (identical(op, "function")) {
    return(character())
  }

  # Direct `<-` / `=` calls are statement-only in the supported translator
  # contract, so host mutation here comes from local-closure `<<-` calls.
  mutations <- character()
  if (identical(op, "<<-")) {
    target <- e[[2L]]
    while (is_call(target, quote(`(`)) && length(target) == 2L) {
      target <- target[[2L]]
    }
    if (is.call(target) && identical(target[[1L]], quote(`[`))) {
      target <- target[[2L]]
    }
    if (is.symbol(target)) {
      mutations <- as.character(target)
    }
  }

  if (!is.null(op)) {
    closure_obj <- scope[[op]]
    if (inherits(closure_obj, LocalClosure) && !op %in% seen) {
      mutations <- c(
        mutations,
        r2f_expression_host_mutations(
          body(closure_obj@fun),
          scope,
          seen = c(seen, op),
          local_functions = local_functions
        )
      )
    }
    if (identical(op, "sapply") && length(e) >= 3L) {
      fun_expr <- unwrap_parens(e[[3L]])
      fun_body <- NULL
      if (is_function_call(fun_expr)) {
        fun_body <- fun_expr[[3L]]
      } else if (is.symbol(fun_expr)) {
        fun_name <- as.character(fun_expr)
        fun_definition <- local_functions[[fun_name]]
        fun_obj <- scope[[fun_name]]
        if (is_function_call(fun_definition)) {
          fun_body <- fun_definition[[3L]]
        } else if (inherits(fun_obj, LocalClosure)) {
          fun_body <- body(fun_obj@fun)
        }
      }
      if (!is.null(fun_body)) {
        mutations <- c(
          mutations,
          r2f_expression_host_mutations(
            fun_body,
            scope,
            seen,
            local_functions
          )
        )
      }
    }
  } else if (
    is.call(callable) &&
      identical(callable[[1L]], quote(`function`))
  ) {
    mutations <- c(
      mutations,
      r2f_expression_host_mutations(
        callable[[3L]],
        scope,
        seen,
        local_functions
      )
    )
  }

  children <- as.list(e)[-1L]
  if (length(children)) {
    mutations <- c(
      mutations,
      unlist(lapply(
        children,
        r2f_expression_host_mutations,
        scope = scope,
        seen = seen,
        local_functions = local_functions
      ))
    )
  }
  unique(mutations)
}

lower_r2f_operand_in_order <- function(
  arg,
  scope,
  ...,
  hoist,
  later_args = list(),
  reject_runtime_guard = FALSE,
  runtime_guard_message = NULL
) {
  stopifnot(
    is.list(later_args),
    is_bool(reject_runtime_guard),
    !reject_runtime_guard || is_string(runtime_guard_message)
  )
  if (is.symbol(arg) || is_scalar_atomic(arg)) {
    operand <- r2f(arg, scope, ..., hoist = hoist)
    return(snapshot_operand_before_later_effects(
      operand,
      arg,
      later_args,
      scope,
      hoist
    ))
  }

  captured_hoist <- capture_hoist(hoist)
  operand <- r2f(arg, scope, ..., hoist = captured_hoist)
  if (reject_runtime_guard && captured_hoist$contains_runtime_guard()) {
    stop(runtime_guard_message, call. = FALSE)
  }
  operand <- finish_captured_operand(operand, captured_hoist, hoist)
  snapshot_operand_before_later_effects(
    operand,
    arg,
    later_args,
    scope,
    hoist
  )
}


# --- Scope Helpers ---

logical_as_int_symbol <- function(var) {
  inherits(var, Variable) &&
    identical(var@mode, "logical") &&
    logical_as_int(var)
}

scope_is_closure <- function(scope) {
  inherits(scope, "quickr_scope") && identical(scope_kind(scope), "closure")
}

scope_fortran_names <- function(scope) {
  stopifnot(inherits(scope, "quickr_scope"))
  out <- character()
  while (inherits(scope, "quickr_scope")) {
    vars <- scope_vars(scope)
    out <- c(out, map_chr(vars, \(v) v@name %||% ""))
    scope <- parent.env(scope)
  }
  unique(out[nzchar(out)])
}

make_shadow_fortran_name <- function(
  scope,
  base,
  suffix = "__local_",
  used = scope_fortran_names(scope)
) {
  stopifnot(inherits(scope, "quickr_scope"), is_string(base), is_string(suffix))
  used <- tolower(used)
  candidate <- paste0(base, suffix)
  if (!tolower(candidate) %in% used) {
    return(candidate)
  }
  i <- 1L
  repeat {
    candidate <- paste0(base, suffix, i, "_")
    if (!tolower(candidate) %in% used) {
      return(candidate)
    }
    i <- i + 1L
  }
}


# --- Main Translation Engine ---

lang2fortran <- r2f <- function(
  e,
  scope = NULL,
  ...,
  calls = character(),
  hoist = NULL
) {
  ## 'hoist' is a per-statement context that handlers can use to pre-emit some
  ## Fortran code. E.g., to setup a temporary variable if the generated Fortran
  ## code doesn't neatly translate into a single expression.
  render_hoist <- is.null(hoist)
  if (render_hoist) {
    hoist <- new_hoist(scope)
  }

  fortran <- switch(
    typeof(e),
    language = {
      # a call
      callable <- e[[1L]]
      callable_unwrapped <- unwrap_parens(callable)

      if (isTRUE(hoist$defer_builtin_arity_error)) {
        arity_error <- lazy_builtin_arity_error(e, scope, recursive = FALSE)
        if (!is.null(arity_error)) {
          stop_deferred_branch_error(arity_error)
        }
      }

      if (!is.null(scope)) {
        maybe_lower_local_closure_call(
          e,
          scope,
          ...,
          hoist = hoist,
          needs_value = !render_hoist
        ) %||%
          {
            handler <- get_r2f_handler(callable_unwrapped)

            match.fun <- handler_field(handler, "match_fun", "match.fun")
            if (is.null(match.fun)) {
              match.fun <- get0(
                callable_unwrapped,
                parent.env(globalenv()),
                mode = "function"
              )
              # this is a best effort to, eg. resolve `seq.default` from `seq`.
              # This should likely be moved into attaching the `match.fun` attr
              # to handlers, for more involved resolution (e.g., with getS3Method())
              if ("UseMethod" %in% all.names(body(match.fun))) {
                match.fun <- get0(
                  paste0(callable_unwrapped, ".default"),
                  parent.env(globalenv()),
                  mode = "function",
                  ifnotfound = match.fun
                )
              }
            }
            if (typeof(match.fun) == "closure") {
              e <- match.call(match.fun, e)
            }

            if (isTRUE(getOption("quickr.r2f.debug"))) {
              try(handler(
                as.list(e)[-1L],
                scope,
                ...,
                calls = c(calls, as.character(callable_unwrapped)),
                hoist = hoist
              )) -> res
              if (inherits(res, "try-error")) {
                debugonce(handler)
                handler(
                  as.list(e)[-1L],
                  scope,
                  ...,
                  calls = c(calls, as.character(callable_unwrapped)),
                  hoist = hoist
                )
              }

              res
            } else {
              handler(
                as.list(e)[-1L],
                scope,
                ...,
                calls = c(calls, as.character(callable_unwrapped)),
                hoist = hoist
              )
            }
          }
      }
    },

    integer = ,
    double = ,
    complex = ,
    logical = atomic2Fortran(e),

    `NULL` = Fortran("", NULL),

    symbol = {
      r_name <- as.character(e)
      val <- if (is.null(scope)) NULL else get0(r_name, scope)
      if (inherits(val, SvdResult)) {
        stop(
          "svd() results must be accessed with $d, $u, or $v",
          call. = FALSE
        )
      }
      # `scope` inherits from the R closure environment. Ignore non-compiler
      # bindings (like captured R objects) so they can't mask undeclared args.
      if (!inherits(val, Variable) && !inherits(val, SvdResult)) {
        val <- NULL
      }
      if (is.null(val) && inherits(scope, "quickr_scope")) {
        closure <- scope_closure(scope)
        arg_names <- if (is.null(closure)) NULL else names(formals(closure))
        if (!is.null(arg_names) && r_name %in% arg_names) {
          stop(
            "arg not declared: ",
            r_name,
            ". Add declare(type(",
            r_name,
            " = ...))",
            call. = FALSE
          )
        }
      }
      if (is.null(val) && isTRUE(hoist$defer_static_mode_error)) {
        stop_deferred_branch_error(
          paste0("object '", r_name, "' not found")
        )
      }
      s <- if (inherits(val, Variable) && !is.null(val@name)) {
        val@name
      } else {
        r_name
      }
      if (logical_as_int_symbol(val)) {
        # logicals passed via the bind(c) interface are stored as integer(0/1)
        # and must be "booleanized" for Fortran logical operations.
        s <- paste0("(", s, "/=0)")
        out <- Fortran(s, value = if (inherits(val, Variable)) val else NULL)
        out@logical_booleanized <- TRUE
        out
      } else {
        Fortran(s, value = if (inherits(val, Variable)) val else NULL)
      }
    },

    # Top-level entry only: quick() hands the user's closure to r2f() to
    # start translation (new_fortran_subroutine()); expressions inside
    # compiled code never produce a closure here.
    closure = {
      if (is.null(name <- attr(e, "name", TRUE))) {
        name <- if (is.symbol(name <- substitute(e))) {
          as.character(name)
        } else {
          "anonymous_function"
        }
      }

      stopifnot(is.null(scope))
      new_fortran_subroutine(name, e)
    },

    ## all the other typeof() possible values
    # "character",
    # "raw" ,
    # "list",
    # "NULL",
    # "function",
    # "special",
    # "builtin",
    # "environment",
    # "S4",
    # "pairlist",
    # "promise",
    # "char",
    # "...",
    # "any",
    # "expression",
    # "externalptr",
    # "bytecode",
    # "weakref"
    # default
    stop_static_mode_error(
      paste0("Unsupported object type encountered: ", typeof(e)),
      hoist
    )
  )

  attr(fortran, "r") <- e
  if (render_hoist) {
    combined <- hoist$render(fortran)
    attributes(combined) <- attributes(fortran)
    attr(combined, "r") <- e
    combined
  } else {
    fortran
  }
}


# --- Atomic Conversion ---

atomic2Fortran <- function(x) {
  if (is_scalar_na(x)) {
    stop("NA literals are not supported", call. = FALSE)
  }
  stopifnot(is_scalar_atomic(x))
  s <- switch(
    typeof(x),
    double = ,
    integer = num2fortran(x),
    logical = if (x) ".true." else ".false.",
    complex = sprintf("(%s, %s)", num2fortran(Re(x)), num2fortran(Im(x)))
  )
  Fortran(s, Variable(typeof(x)))
}

num2fortran <- function(x) {
  stopifnot(typeof(x) %in% c("integer", "double"))
  digits <- 7L
  nsmall <- switch(typeof(x), integer = 0L, double = 1L)
  repeat {
    s <- format.default(x, digits = digits, nsmall = nsmall, scientific = 1L)
    if (x == eval(str2lang(s))) {
      # eval() needed for negative and complex numbers
      break
    }
    add(digits) <- 1L
    if (digits > 22L) {
      stop("number formatting error: ", x, " formatted as : ", s)
    }
  }
  paste0(s, switch(typeof(x), double = "_c_double", integer = "_c_int"))
}


# --- Handler Lookup ---

get_r2f_handler <- function(name) {
  stopifnot("All functions called must be named as symbols" = is.symbol(name))
  handler <- get0(name, r2f_handlers) %||%
    stop("Unsupported function: ", name, call. = FALSE)
  resolve_handler_fun(handler)
}


# Swap in the handler's current namespace binding, so an instrumented or
# otherwise rebound copy is dispatched instead of the one captured at
# registration. Only handlers registered as namespace-level named functions
# carry a `fun_name`; for every other handler this is a property read and a
# return. See register_r2f_handler() for why the name is recorded.
resolve_handler_fun <- function(handler) {
  # Only R2FHandler objects can carry a `fun_name`, so this doubles as the
  # check that `handler` is one -- bare-function handlers read NULL here.
  name <- handler_field(handler, "fun_name")
  if (!is_string(name)) {
    return(handler)
  }
  current <- get0(name, envir = environment(handler), mode = "function")
  if (is.null(current) || identical(current, S7_data(handler))) {
    return(handler)
  }
  S7_data(handler) <- current
  handler
}


# --- Destination Helpers ---

# Read a handler property, whether the handler is an R2FHandler object or
# a bare function carrying attributes. `attr_name` covers the one legacy
# spelling difference (the "match.fun" attr vs the match_fun property).
# NULL handlers read as NULL.
handler_field <- function(handler, name, attr_name = name) {
  if (inherits(handler, R2FHandler)) {
    prop(handler, name)
  } else {
    attr(handler, attr_name, exact = TRUE)
  }
}

# Resolve the registered handler for a (possibly parenthesized) call, or
# NULL when it is not a named-symbol call or has no handler.
handler_for_call <- function(call) {
  if (!is.call(call)) {
    return(NULL)
  }
  call <- unwrap_parens(call)
  if (!is.call(call) || !is.symbol(call[[1L]])) {
    return(NULL)
  }
  get0(as.character(call[[1L]]), r2f_handlers, inherits = FALSE)
}

dest_supported_for_call <- function(call) {
  isTRUE(handler_field(handler_for_call(call), "dest_supported"))
}

dest_infer_for_call <- function(call, scope) {
  handler <- handler_for_call(call)
  if (is.null(handler)) {
    return(NULL)
  }
  unwrapped <- unwrap_parens(call)
  infer <- handler_field(handler, "dest_infer")
  infer_name <- handler_field(handler, "dest_infer_name")

  infer_fun <- NULL
  if (is_string(infer_name)) {
    # Resolve dynamically from the handler's environment (typically the package
    # namespace) so instrumented/rebound functions are respected.
    infer_fun <- get0(
      infer_name,
      envir = environment(handler),
      mode = "function"
    )
  }
  if (!is.function(infer_fun)) {
    infer_fun <- infer
  }
  if (!is.function(infer_fun)) {
    return(NULL)
  }
  infer_fun(as.list(unwrapped)[-1L], scope)
}


# --- Default Handlers ---

.r2f_handler_not_implemented_yet <- function(e, scope, ...) {
  stop(
    gettextf("'%s' is not implemented yet", as.character(e[[1L]])),
    call. = FALSE
  )
}


# --- Utility ---

check_call <- function(e, nargs) {
  if (length(e) != (nargs + 1L)) {
    stop("Too many args to: ", as.character(e[[1L]]))
  }
}
