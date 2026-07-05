# r2f-arithmetic.R
# Handlers for arithmetic operators: +, -, *, /, ^, %%, %/%

# --- Handlers ---

r2f_handlers[["+"]] <- function(args, scope, ...) {
  # Support both binary and unary plus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ...)
    # R: +TRUE is 1L
    x <- cast_to_mode(x, arith_join_mode(x), "unary +")
    Fortran(glue("(+{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lapply(args, r2f, scope, ...)
    pair <- promote_arith_pair(left, right, "+")
    left <- pair$left
    right <- pair$right
    reshaped <- maybe_reshape_vector_matrix(left, right)
    left <- reshaped$left
    right <- reshaped$right
    Fortran(glue("({left} + {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["-"]] <- function(args, scope, ...) {
  # Support both binary and unary minus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ...)
    # R: -TRUE is -1L
    x <- cast_to_mode(x, arith_join_mode(x), "unary -")
    Fortran(glue("(-{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lapply(args, r2f, scope, ...)
    pair <- promote_arith_pair(left, right, "-")
    left <- pair$left
    right <- pair$right
    reshaped <- maybe_reshape_vector_matrix(left, right)
    left <- reshaped$left
    right <- reshaped$right
    Fortran(glue("({left} - {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["*"]] <- function(args, scope = NULL, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  pair <- promote_arith_pair(left, right, "*")
  left <- pair$left
  right <- pair$right
  reshaped <- maybe_reshape_vector_matrix(left, right)
  left <- reshaped$left
  right <- reshaped$right
  Fortran(glue("({left} * {right})"), conform(left@value, right@value))
}

r2f_handlers[["/"]] <- function(args, scope = NULL, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  left <- maybe_cast_double(left)
  right <- maybe_cast_double(right)
  reshaped <- maybe_reshape_vector_matrix(left, right)
  left <- reshaped$left
  right <- reshaped$right
  Fortran(glue("({left} / {right})"), conform(left@value, right@value))
}

r2f_handlers[["^"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  # R's ^ always returns double (R_pow), so cast the base. Keep an integer
  # exponent as integer: Fortran `real ** int` is exact and, unlike
  # `real ** real`, defined for negative bases -- matching R, which
  # special-cases whole-number exponents.
  left <- maybe_cast_double(left)
  if (identical(right@value@mode, "logical")) {
    right <- cast_to_mode(right, "integer", "^")
  }
  reshaped <- maybe_reshape_vector_matrix(left, right)
  left <- reshaped$left
  right <- reshaped$right
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

r2f_handlers[["%%"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  # `modulo` requires same-typed arguments, so cast both operands to the
  # join (logical joins as integer: R's TRUE %% TRUE is 0L).
  mode <- arith_join_mode(left, right)
  left <- cast_to_mode(left, mode, "%%")
  right <- cast_to_mode(right, mode, "%%")
  out_val <- conform(left@value, right@value)
  # MODULO gives result with sign(right) - matches R %% behaviour
  Fortran(glue("modulo({left}, {right})"), out_val)
}

r2f_handlers[["%/%"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)
  pair <- promote_arith_pair(left, right, "%/%")
  left <- pair$left
  right <- pair$right
  out_val <- conform(left@value, right@value)

  expr <- switch(
    out_val@mode,
    integer = glue(
      "int(floor(real({left}, kind=c_double) / real({right}, kind=c_double)), kind=c_int)"
    ),
    double = {
      # Fortran FLOOR() returns an integer, so a large double quotient
      # (e.g. 1e20 %/% 3) would silently overflow. Stay in the real domain
      # as the floor() handler does; the quotient is spliced three times,
      # so hoist it to evaluate once.
      q <- hoist_unless_name(
        Fortran(glue("({left} / {right})"), out_val),
        hoist
      )
      aint <- glue("aint({q})")
      glue("({aint} - merge(1.0_c_double, 0.0_c_double, ({q} < {aint})))")
    },
    stop("%/% only implemented for numeric types")
  )

  Fortran(expr, out_val)
}
