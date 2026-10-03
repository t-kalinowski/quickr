# r2f-bitwise.R
# Handlers for bitwise operations: bitwAnd, bitwOr, bitwXor, bitwNot,
# bitwShiftL, bitwShiftR
#
# R semantics:
#   - the result is always an integer vector: array dims are dropped
#   - double operands are coerced as by as.integer() (truncation toward 0)
#   - logical operands are an error
#   - bitwShiftL/R are logical (unsigned) shifts; a shift count outside
#     0..31 yields NA
#
# Fortran intrinsics (F2008): IAND, IOR, IEOR, NOT, SHIFTL, SHIFTR.
# SHIFTR is a logical shift, matching R. R's NA_integer_ is the bit pattern
# INT_MIN, so results that land on it (e.g. bitwShiftL(1L, 31L)) already
# match R. NA operands are not propagated: quickr does not support NA.

# --- Local Helpers ---

# R's NA_integer_ (INT_MIN), spelled without overflowing a constant.
bitw_na_int <- "(-huge(0_c_int) - 1_c_int)"

# Coerce one operand of a bitw*() call to integer and drop its array dims.
# Used by: bitw_binary(), bitwNot
bitw_operand <- function(x, fn, scope, hoist) {
  mode <- x@value@mode
  if (identical(mode, "double")) {
    x <- Fortran(glue("int({x}, kind=c_int)"), Variable("integer", x@value@dims))
  } else if (!is.null(mode) && !identical(mode, "integer")) {
    stop(
      fn,
      "() requires integer or double arguments, not ",
      mode,
      call. = FALSE
    )
  }

  if (passes_as_scalar(x@value) || x@value@rank <= 1L) {
    return(x)
  }

  # R drops dimensions for bitw*(<array>): the operand is used as a vector.
  len_expr <- value_length_expr(x@value)
  if (is.call(len_expr) && !length(all.vars(len_expr))) {
    len_expr <- as.integer(eval(len_expr, baseenv()))
  }
  if (identical(len_expr, 1L)) {
    # A 1x1 array is a scalar operand (R: bitwAnd(matrix(5L), 1:3) has
    # length 3); reshape() would give a size-1 array instead.
    x <- hoist_unless_name(x, hoist)
    index <- paste(rep("1", x@value@rank), collapse = ", ")
    return(Fortran(glue("{x}({index})"), Variable("integer")))
  }
  len_str <- if (is_scalar_na(len_expr)) {
    glue("size({x})")
  } else {
    dims2f(list(len_expr), scope)
  }
  out_val <- Variable(
    "integer",
    list(if (is_scalar_na(len_expr)) NA else len_expr)
  )
  Fortran(glue("reshape({x}, [{len_str}])"), out_val)
}

# Lower the two operands of a binary bitw*() call to integer vectors that
# satisfy the elementwise conformability contract.
# Used by: binary bitw*() handlers
bitw_operands <- function(args, scope, ..., hoist, fn) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  left <- bitw_operand(left, fn, scope, hoist)
  right <- bitw_operand(right, fn, scope, hoist)
  # Both operands are now scalars or vectors, so this only applies the
  # equal-length rule (R would recycle, which quickr does not support).
  maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
}

# Used by: bitwAnd, bitwOr, bitwXor
bitw_binary <- function(args, scope, ..., hoist, fn, intrinsic) {
  .[left, right] <- bitw_operands(args, scope, ..., hoist = hoist, fn = fn)
  Fortran(
    glue("{intrinsic}({left}, {right})"),
    conform(left@value, right@value, mode = "integer")
  )
}

# Used by: bitwShiftL, bitwShiftR
bitw_shift <- function(args, scope, ..., hoist, fn, intrinsic) {
  .[a, n] <- bitw_operands(args, scope, ..., hoist = hoist, fn = fn)
  # `n` is spliced three times below, so evaluate it once.
  n <- hoist_unless_name(n, hoist)
  # SHIFTL/SHIFTR are undefined for a count outside 0..bit_size, and merge()
  # evaluates both branches, so clamp the count inside the shift too.
  in_range <- glue("({n} >= 0_c_int .and. {n} <= 31_c_int)")
  shifted <- glue("{intrinsic}({a}, min(max({n}, 0_c_int), 31_c_int))")
  Fortran(
    glue("merge({shifted}, {bitw_na_int}, {in_range})"),
    conform(a@value, n@value, mode = "integer")
  )
}


# --- Handlers ---

r2f_handlers[["bitwAnd"]] <- function(args, scope, ..., hoist = NULL) {
  bitw_binary(
    args,
    scope,
    ...,
    hoist = hoist,
    fn = "bitwAnd",
    intrinsic = "iand"
  )
}

r2f_handlers[["bitwOr"]] <- function(args, scope, ..., hoist = NULL) {
  bitw_binary(
    args,
    scope,
    ...,
    hoist = hoist,
    fn = "bitwOr",
    intrinsic = "ior"
  )
}

r2f_handlers[["bitwXor"]] <- function(args, scope, ..., hoist = NULL) {
  bitw_binary(
    args,
    scope,
    ...,
    hoist = hoist,
    fn = "bitwXor",
    intrinsic = "ieor"
  )
}

r2f_handlers[["bitwNot"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  a <- r2f(args[[1L]], scope, ..., hoist = hoist)
  a <- bitw_operand(a, "bitwNot", scope, hoist)
  Fortran(glue("not({a})"), Variable("integer", a@value@dims))
}

r2f_handlers[["bitwShiftL"]] <- function(args, scope, ..., hoist = NULL) {
  bitw_shift(
    args,
    scope,
    ...,
    hoist = hoist,
    fn = "bitwShiftL",
    intrinsic = "shiftl"
  )
}

r2f_handlers[["bitwShiftR"]] <- function(args, scope, ..., hoist = NULL) {
  bitw_shift(
    args,
    scope,
    ...,
    hoist = hoist,
    fn = "bitwShiftR",
    intrinsic = "shiftr"
  )
}
