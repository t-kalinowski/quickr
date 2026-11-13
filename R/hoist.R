# Statement-local emission, temporary storage, and operand evaluation.

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

new_hoist <- function(scope, defer_errors = scope_defers_branch_errors(scope)) {
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

  allocate_tmp_at_point <- function(var, emit_at_point) {
    if (!block_tmp_allocatable(var, block_scope)) {
      return(var)
    }
    point_allocated <<- c(point_allocated, var@name)
    emit_at_point(glue(
      "allocate({var@name}({dims2f(var@dims, block_scope)}))"
    ))
    var
  }

  declare_tmp_at_point <- function(mode, dims, logical_as_int = FALSE) {
    var <- declare_tmp(mode, dims, logical_as_int)
    allocate_tmp_at_point(var, emit)
  }

  allocation_guard_at_point <- function(var) {
    stopifnot(inherits(var, Variable))
    if (!block_tmp_allocatable(var, block_scope)) {
      return(character())
    }
    point_allocated <<- unique(c(point_allocated, var@name))
    glue(
      "if (.not. allocated({var@name})) allocate({var@name}({dims2f(var@dims, block_scope)}))"
    )
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
        allocation_guard_at_point = allocation_guard_at_point,
        declare_tmp = declare_tmp,
        declare_tmp_at_point = capture_declare_tmp_at_point,
        render = capture_render,
        has_code = capture_has_code,
        mark_runtime_guard = capture_mark_runtime_guard,
        contains_runtime_guard = capture_contains_runtime_guard,
        capture = capture,
        capture_block = capture_block
      ),
      parent = emptyenv()
    )
  }

  capture_block <- function() {
    new_hoist(ensure_block_scope())
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
      defer_static_shape_error = defer_errors,
      defer_builtin_arity_error = defer_errors,
      defer_static_mode_error = defer_errors,
      emit = emit,
      allocation_guard_at_point = allocation_guard_at_point,
      declare_tmp = declare_tmp,
      declare_tmp_at_point = declare_tmp_at_point,
      is_empty = is_empty,
      render = render,
      mark_runtime_guard = mark_runtime_guard,
      contains_runtime_guard = contains_runtime_guard,
      capture = capture,
      capture_block = capture_block
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
  logical_as_int = FALSE
) {
  stopifnot(is.environment(hoist))
  tmp <- hoist$declare_tmp(
    mode = mode,
    dims = dims,
    logical_as_int = logical_as_int
  )
  hoist$emit(glue("{tmp@name} = {code}"))
  Fortran(tmp@name, tmp)
}

# Hoist `x` into a temporary variable unless it already renders as a bare
# variable name or scalar literal. Use this whenever the same operand is
# spliced into generated code more than once: Fortran evaluates actual
# arguments before the call, so repeating an expression duplicates its side
# effects (e.g. RNG state via runif()).
hoist_unless_name <- function(x, hoist, hoist_literals = FALSE) {
  stopifnot(inherits(x, Fortran), inherits(x@value, Variable))
  code <- trimws(as.character(x))
  if (!is.null(x@value@name) && identical(code, x@value@name)) {
    return(x)
  }
  # Array-shaped fills still need storage, e.g. matrix(numeric(6), 3, 2).
  # Some callers also need literals evaluated at runtime to avoid constant
  # division/domain errors, including in branches that will not execute.
  if (
    !hoist_literals &&
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
    logical_as_int = logical_as_int(x@value) &&
      !isTRUE(x@logical_booleanized)
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

# Scalar literal optimizations can make enclosing arithmetic constant too.
# Keep those operands at runtime. Preserve the array hoists in lazy branches
# without adding array allocations to ordinary arithmetic.
hoist_domain_operand <- function(x, hoist) {
  stopifnot(inherits(x, Fortran), inherits(x@value, Variable))
  if (passes_as_scalar(x@value) || isTRUE(hoist$defer_static_mode_error)) {
    return(hoist_unless_name(x, hoist, hoist_literals = TRUE))
  }
  x
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

preserve_captured_errors <- function(expr, captured_hoist, hoist) {
  tryCatch(
    expr,
    error = function(error) {
      # The lazy branch owns the diagnostic, but earlier operand effects
      # must reach that branch before the error is propagated.
      finish_captured_operand(Fortran(""), captured_hoist, hoist)
      stop(error)
    }
  )
}

capture_inheriting_deferred_errors <- function(hoist) {
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

# Known limitation: an earlier operand is not copied before a later local
# closure mutates its host bindings. General operand snapshots, including
# runif() bounds and reduction arguments, are deferred. Assignment-valued
# operands are also unsupported: assignment handlers emit statements.
lower_r2f_operand_in_order <- function(
  arg,
  scope,
  ...,
  hoist,
  reject_runtime_guard = FALSE,
  runtime_guard_message = NULL
) {
  stopifnot(
    is_bool(reject_runtime_guard),
    !reject_runtime_guard || is_string(runtime_guard_message)
  )
  if (is.symbol(arg) || is_scalar_atomic(arg)) {
    return(r2f(arg, scope, ..., hoist = hoist))
  }

  captured_hoist <- capture_inheriting_deferred_errors(hoist)
  operand <- preserve_captured_errors(
    r2f(arg, scope, ..., hoist = captured_hoist),
    captured_hoist,
    hoist
  )
  if (reject_runtime_guard && captured_hoist$contains_runtime_guard()) {
    stop(runtime_guard_message, call. = FALSE)
  }
  finish_captured_operand(operand, captured_hoist, hoist)
}
