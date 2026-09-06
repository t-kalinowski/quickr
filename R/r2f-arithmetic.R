# r2f-arithmetic.R
# Handlers for arithmetic operators: +, -, *, /, ^, %%, %/%

# --- Handlers ---

refuse_raw_arithmetic <- function(..., context) {
  operands <- list(...)
  if (any(map_lgl(operands, \(x) identical(x@value@mode, "raw")))) {
    stop(context, " does not support raw operands", call. = FALSE)
  }
  invisible(TRUE)
}

guard_nonzero_arithmetic_divisor <- function(right, hoist, scope, context) {
  stopifnot(inherits(right, Fortran), !is.null(hoist))
  message <- paste0(context, " does not support zero divisors")
  r_value <- attr(right, "r", exact = TRUE)
  if (is.atomic(r_value) && length(r_value) == 1L && !is.na(r_value)) {
    if (r_value == 0) {
      stop_static_value_error(message, hoist)
    }
    return(right)
  }

  right <- hoist_unless_name(right, hoist)
  condition <- if (passes_as_scalar(right@value)) {
    glue("{right} == 0")
  } else {
    glue("any({right} == 0)")
  }
  emit_quickr_error_if(condition, message, hoist, scope)
  right
}

r2f_handlers[["+"]] <- function(args, scope, ..., hoist = NULL) {
  # Support both binary and unary plus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ..., hoist = hoist)
    refuse_raw_arithmetic(x, context = "+")
    # R: +TRUE is 1L
    x <- cast_to_mode(x, arith_join_mode(x), "unary +")
    Fortran(glue("(+{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lower_elementwise_operands(
      args,
      scope,
      ...,
      hoist = hoist
    )
    refuse_raw_arithmetic(left, right, context = "+")
    .[left, right] <- promote_arith_pair(left, right, "+")
    .[left, right] <- maybe_reshape_vector_matrix(left, right, hoist, scope)
    Fortran(glue("({left} + {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["-"]] <- function(args, scope, ..., hoist = NULL) {
  # Support both binary and unary minus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ..., hoist = hoist)
    refuse_raw_arithmetic(x, context = "-")
    # R: -TRUE is -1L
    x <- cast_to_mode(x, arith_join_mode(x), "unary -")
    Fortran(glue("(-{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lower_elementwise_operands(
      args,
      scope,
      ...,
      hoist = hoist
    )
    refuse_raw_arithmetic(left, right, context = "-")
    .[left, right] <- promote_arith_pair(left, right, "-")
    .[left, right] <- maybe_reshape_vector_matrix(left, right, hoist, scope)
    Fortran(glue("({left} - {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["*"]] <- function(args, scope = NULL, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  refuse_raw_arithmetic(left, right, context = "*")
  .[left, right] <- promote_arith_pair(left, right, "*")
  .[left, right] <- maybe_reshape_vector_matrix(left, right, hoist, scope)
  Fortran(glue("({left} * {right})"), conform(left@value, right@value))
}

r2f_handlers[["/"]] <- function(args, scope = NULL, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  refuse_raw_arithmetic(left, right, context = "/")
  left <- maybe_cast_double(left)
  right <- maybe_cast_double(right)
  .[left, right] <- maybe_reshape_vector_matrix(left, right, hoist, scope)
  Fortran(glue("({left} / {right})"), conform(left@value, right@value))
}

r2f_handlers[["^"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  refuse_raw_arithmetic(left, right, context = "^")
  # R's ^ always returns double (R_pow), so cast the base. Keep an integer
  # exponent as integer: Fortran `real ** int` is exact and, unlike
  # `real ** real`, defined for negative bases -- matching R, which
  # special-cases whole-number exponents.
  left <- maybe_cast_double(left)
  if (identical(right@value@mode, "logical")) {
    right <- cast_to_mode(right, "integer", "^")
  }
  .[left, right] <- maybe_reshape_vector_matrix(left, right, hoist, scope)
  mode <- reduce_promoted_mode(left, right)
  if (!identical(mode, "complex")) {
    mode <- "double"
  }
  # Parenthesizing the exponent avoids non-standard `** -1_c_int`.
  Fortran(
    glue("({left} ** ({right}))"),
    conform(left@value, right@value, mode = mode)
  )
}


# ---- remainder (%%) and integer division (%/%) ----
#
# R semantics:
#   x %%  y  ==  r   where  r has the sign of y  (divisor)
#   x %/% y  ==  q   where  q = floor(x / y)
# and  x == r + y * q  (within rounding error)
#
# Fortran intrinsics:
#   - MODULO(a,p)   : remainder with sign(p)
#   - FLOOR(x)      : greatest integer <= x      (real)
#   - AINT(x)       : truncation toward 0       (real)

r2f_handlers[["%%"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  refuse_raw_arithmetic(left, right, context = "%%")
  # `modulo` requires same-typed arguments, so cast both operands to the
  # join (logical joins as integer: R's TRUE %% TRUE is 0L).
  mode <- arith_join_mode(left, right)
  if (identical(mode, "complex")) {
    # Fortran modulo() has no complex form; R refuses too.
    stop_static_mode_error("unimplemented complex operation", hoist)
  }
  left <- cast_to_mode(left, mode, "%%")
  right <- cast_to_mode(right, mode, "%%")
  .[left, right] <- maybe_reshape_vector_matrix(left, right, hoist, scope)
  right <- guard_nonzero_arithmetic_divisor(right, hoist, scope, "%%")
  out_val <- conform(left@value, right@value)
  # MODULO gives result with sign(right) - matches R %% behaviour
  Fortran(glue("modulo({left}, {right})"), out_val)
}

r2f_handlers[["%/%"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  refuse_raw_arithmetic(left, right, context = "%/%")
  .[left, right] <- promote_arith_pair(left, right, "%/%")
  .[left, right] <- maybe_reshape_vector_matrix(left, right, hoist, scope)
  right <- guard_nonzero_arithmetic_divisor(right, hoist, scope, "%/%")
  out_val <- conform(left@value, right@value)

  expr <- switch(
    out_val@mode,
    integer = glue(
      "int(floor(real({left}, kind=c_double) / real({right}, kind=c_double)), kind=c_int)"
    ),
    double = {
      # The quotient is spliced three times by real_floor_expr(), so
      # hoist it to evaluate once.
      q <- hoist_unless_name(
        Fortran(glue("({left} / {right})"), out_val),
        hoist
      )
      real_floor_expr(q)
    },
    stop_static_mode_error("%/% only implemented for numeric types", hoist)
  )

  Fortran(expr, out_val)
}
