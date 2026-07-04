# r2f-operators-helpers.R
# Generic helpers for binary operators and type conformance.

# Convert a logical value backed by bind(c) integer storage (0/1/NA) to a
# Fortran LOGICAL expression. Symbols are typically booleanized during r2f()
# (see r2f-aab-core.R), but expressions like rev(x) need handling at use sites.
# Used by: r2f-logical.R, r2f-conditionals.R, r2f-subscript.R, r2f-reductions.R
booleanize_logical_as_int <- function(x) {
  stopifnot(inherits(x, Fortran))

  if (
    is.null(x@value) ||
      !identical(x@value@mode, "logical") ||
      !logical_as_int(x@value)
  ) {
    return(x)
  }

  if (isTRUE(x@logical_booleanized)) {
    return(x)
  }

  out <- Fortran(glue("({x} /= 0)"), Variable("logical", x@value@dims))
  out@logical_booleanized <- TRUE
  out
}

# Cast a Fortran value to a target mode by wrapping its expression text.
# The only place cast spellings live; errors on casts it cannot spell
# (complex/character operands, or any narrowing) so an unsupported mode is
# a clean diagnostic instead of invalid generated Fortran.
# Used by: r2f-arithmetic.R, r2f-logical.R, r2f-constructors.R,
#          r2f-reductions.R, r2f-matrix.R
cast_to_mode <- function(x, mode, context = "operand") {
  stopifnot(inherits(x, Fortran))
  if (
    is.null(mode) ||
      is.null(x@value@mode) || # mode still being inferred; nothing to spell
      identical(x@value@mode, mode)
  ) {
    return(x)
  }
  from <- x@value@mode
  if (identical(mode, "double") && identical(from, "integer")) {
    return(Fortran(
      glue("real({x}, kind=c_double)"),
      Variable("double", x@value@dims)
    ))
  }
  if (identical(from, "logical") && mode %in% c("integer", "double")) {
    if (logical_as_int(x@value) && !isTRUE(x@logical_booleanized)) {
      # bind(c) logicals are already 0/1 integer storage; relabel, or cast
      # the integer text directly for a double target.
      relabeled <- Fortran(glue("{x}"), Variable("integer", x@value@dims))
      return(cast_to_mode(relabeled, mode, context))
    }
    x <- booleanize_logical_as_int(x)
    literals <- switch(
      mode,
      integer = "1_c_int, 0_c_int",
      double = "1.0_c_double, 0.0_c_double"
    )
    return(Fortran(
      glue("merge({literals}, {x})"),
      Variable(mode, x@value@dims)
    ))
  }
  stop(
    context,
    " does not support coercion from ",
    from,
    " to ",
    mode,
    call. = FALSE
  )
}

# Cast a value to double if it's logical or integer.
# Used by: r2f-arithmetic.R, r2f-math.R, r2f-matrix*.R, r2f-coercions.R
maybe_cast_double <- function(x) {
  if (x@value@mode %in% c("logical", "integer")) {
    cast_to_mode(x, "double")
  } else {
    x
  }
}

# Promote a list of operands to their common (lattice-join) mode, casting
# each one whose mode differs. For contexts where Fortran requires uniform
# argument types: array constructors (c()), min/max, merge, modulo.
# Returns list(args = <cast operands>, mode = <join>).
# Used by: r2f-arithmetic.R, r2f-constructors.R, r2f-reductions.R
promote_operands <- function(args, context = "operator") {
  mode <- reduce_promoted_mode(args)
  list(
    args = lapply(args, cast_to_mode, mode = mode, context = context),
    mode = mode
  )
}

# Lattice join for arithmetic contexts: logical joins as integer (R:
# TRUE + TRUE is 2L, sum(TRUE) is 1L; Fortran has no logical arithmetic).
# Used by: r2f-arithmetic.R, r2f-math.R, r2f-reductions.R
arith_join_mode <- function(...) {
  mode <- reduce_promoted_mode(...)
  if (identical(mode, "logical")) "integer" else mode
}

# Apply R's arithmetic rule for logical operands: they join as integer
# (TRUE + TRUE is 2L), and Fortran has no logical arithmetic, so cast them.
# int/double mixes are left alone -- Fortran's own promotion matches R.
# Used by: r2f-arithmetic.R, r2f-logical.R
promote_arith_pair <- function(left, right, context = "arithmetic") {
  if (
    identical(left@value@mode, "logical") ||
      identical(right@value@mode, "logical")
  ) {
    mode <- arith_join_mode(left, right)
    left <- cast_to_mode(left, mode, context)
    right <- cast_to_mode(right, mode, context)
  }
  list(left = left, right = right)
}

# Check if a dimension expression equals 1.
# Used by: r2f-arithmetic.R, r2f-logical.R
dim_is_one <- function(x) {
  is_wholenumber(x) && identical(as.integer(x), 1L)
}

# Check if a Fortran value is a 1x1 matrix.
# Used by: r2f-arithmetic.R, r2f-logical.R
is_one_by_one <- function(x) {
  stopifnot(inherits(x, Fortran))
  x@value@rank == 2L &&
    dim_is_one(x@value@dims[[1L]]) &&
    dim_is_one(x@value@dims[[2L]])
}

# Check if two dimension expressions match.
# Used by: r2f-arithmetic.R, r2f-logical.R
dims_match <- function(left, right) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    return(identical(as.integer(left), as.integer(right)))
  }
  identical(left, right)
}

# Three-valued conformability verdict for one axis of an elementwise op:
# ok+known (no guard needed), not-ok+known (compile error at the caller),
# or unknown (caller emits a runtime guard). Known lengths must be equal
# and nonzero -- R-style recycling is never implemented, and quickr cannot
# represent length-0 results. NA dims are always unknown: two unknown
# lengths are not the same quantity.
# Used by: maybe_reshape_vector_matrix()
check_elementwise_lengths <- function(left, right) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    left <- as.integer(left)
    right <- as.integer(right)
    return(list(ok = left == right && left > 0L, unknown = FALSE))
  }
  if (
    (is_wholenumber(left) && as.integer(left) == 0L) ||
      (is_wholenumber(right) && as.integer(right) == 0L)
  ) {
    return(list(ok = FALSE, unknown = FALSE))
  }
  if (!is_scalar_na(left) && !is_scalar_na(right)) {
    left_norm <- fortranize_expr_symbols(left)
    right_norm <- fortranize_expr_symbols(right)
    if (identical(left_norm, right_norm)) {
      return(list(ok = TRUE, unknown = FALSE))
    }
  }
  list(ok = TRUE, unknown = TRUE)
}

# Emit a statement-level runtime check that two elementwise operands have
# equal size along the given axes (whole size when an axis is NULL).
# size() is an inquiry, so applying it to operand expression text does not
# evaluate the operands.
# Used by: maybe_reshape_vector_matrix()
emit_elementwise_size_guard <- function(
  left,
  right,
  hoist,
  scope,
  message,
  left_axis = NULL,
  right_axis = left_axis
) {
  if (is.null(hoist)) {
    stop(
      "cannot emit a runtime length guard here; ",
      "operand lengths must match statically",
      call. = FALSE
    )
  }
  size_of <- function(x, axis) {
    if (is.null(axis)) glue("size({x})") else glue("size({x}, {axis})")
  }
  emit_quickr_error_if(
    glue("{size_of(left, left_axis)} /= {size_of(right, right_axis)}"),
    message,
    hoist,
    scope
  )
}

# Reshape a vector to match a matrix's dimensions.
# Used by: r2f-arithmetic.R, r2f-logical.R
reshape_vector_for_matrix <- function(vec, rows, cols) {
  stopifnot(inherits(vec, Fortran))
  out_val <- Variable(vec@value@mode, list(rows, cols))
  source <- if (passes_as_scalar(vec@value)) {
    glue("[{vec}]")
  } else {
    glue("{vec}")
  }
  out_expr <- glue(
    "reshape({source}, [{bind_dim_int(rows)}, {bind_dim_int(cols)}], pad = {source})"
  )
  Fortran(out_expr, out_val)
}

# Convert a 1x1 matrix to a scalar.
# Used by: r2f-arithmetic.R, r2f-logical.R
scalarize_matrix <- function(mat) {
  stopifnot(inherits(mat, Fortran))
  out_val <- Variable(mat@value@mode)
  Fortran(glue("{mat}(1, 1)"), out_val)
}

# Reshape vector/matrix operands to match ranks for binary operations, and
# enforce the elementwise conformability policy: known-mismatched lengths
# are compile errors (R-style recycling is not supported; scalar broadcast
# is native), lengths that cannot be compared statically get a runtime
# size guard through `hoist`.
# Used by: r2f-arithmetic.R, r2f-logical.R
maybe_reshape_vector_matrix <- function(left, right, hoist = NULL, scope = NULL) {
  if (
    !inherits(left, Fortran) ||
      !inherits(right, Fortran) ||
      is.null(left@value) ||
      is.null(right@value)
  ) {
    return(list(left = left, right = right))
  }

  left_scalar <- passes_as_scalar(left@value)
  right_scalar <- passes_as_scalar(right@value)
  left_rank <- if (left_scalar) 0L else left@value@rank
  right_rank <- if (right_scalar) 0L else right@value@rank

  if (left_rank == 2L && right_rank == 1L && is_one_by_one(left)) {
    right_len <- dim_or_one(right, 1L)
    if (!dim_is_one(right_len)) {
      left <- scalarize_matrix(left)
      left_rank <- 0L
    }
  } else if (left_rank == 1L && right_rank == 2L && is_one_by_one(right)) {
    left_len <- dim_or_one(left, 1L)
    if (!dim_is_one(left_len)) {
      right <- scalarize_matrix(right)
      right_rank <- 0L
    }
  }

  if (left_rank == 1L && right_rank == 1L) {
    vector_msg <- paste0(
      "elementwise vector operations require equal lengths or ",
      "a scalar operand; R-style recycling is not supported"
    )
    conform <- check_elementwise_lengths(
      dim_or_one(left, 1L),
      dim_or_one(right, 1L)
    )
    if (!conform$ok) {
      stop(vector_msg, call. = FALSE)
    }
    if (conform$unknown) {
      emit_elementwise_size_guard(left, right, hoist, scope, vector_msg)
    }
  }

  if (left_rank == 2L && right_rank == 2L) {
    matrix_msg <- "elementwise matrix operations require matching dimensions"
    left_dims <- matrix_dims(left)
    right_dims <- matrix_dims(right)
    row_conform <- check_elementwise_lengths(left_dims$rows, right_dims$rows)
    col_conform <- check_elementwise_lengths(left_dims$cols, right_dims$cols)
    if (!row_conform$ok || !col_conform$ok) {
      stop(matrix_msg, call. = FALSE)
    }
    if (row_conform$unknown) {
      emit_elementwise_size_guard(
        left,
        right,
        hoist,
        scope,
        matrix_msg,
        left_axis = 1L
      )
    }
    if (col_conform$unknown) {
      emit_elementwise_size_guard(
        left,
        right,
        hoist,
        scope,
        matrix_msg,
        left_axis = 2L
      )
    }
  }

  vec_mat_msg <- paste0(
    "elementwise vector-matrix operations require a scalar or ",
    "a vector length equal to the matrix first dimension (nrow)"
  )
  if (left_rank == 1L && right_rank == 2L) {
    right_dims <- matrix_dims(right)
    left_len <- dim_or_one(left, 1L)
    row_conform <- check_elementwise_lengths(left_len, right_dims$rows)
    if (!row_conform$ok) {
      stop(vec_mat_msg, call. = FALSE)
    }
    if (row_conform$unknown) {
      emit_elementwise_size_guard(
        left,
        right,
        hoist,
        scope,
        vec_mat_msg,
        right_axis = 1L
      )
    }
    left <- reshape_vector_for_matrix(left, right_dims$rows, right_dims$cols)
  } else if (left_rank == 2L && right_rank == 1L) {
    left_dims <- matrix_dims(left)
    right_len <- dim_or_one(right, 1L)
    row_conform <- check_elementwise_lengths(right_len, left_dims$rows)
    if (!row_conform$ok) {
      stop(vec_mat_msg, call. = FALSE)
    }
    if (row_conform$unknown) {
      emit_elementwise_size_guard(
        right,
        left,
        hoist,
        scope,
        vec_mat_msg,
        right_axis = 1L
      )
    }
    right <- reshape_vector_for_matrix(right, left_dims$rows, left_dims$cols)
  }

  list(left = left, right = right)
}

# R's promotion order for the supported atomic modes, lowest to highest.
# The one place the lattice is spelled: promotion joins take the highest
# rank present; the narrowing check refuses assignments that move down.
mode_lattice <- c("logical", "integer", "double", "complex")

# Rank of a mode on the lattice (NA for modes outside it, e.g. character).
# Used by: reduce_promoted_mode(), scope.R (check_reassignment_narrowing)
mode_rank <- function(mode) {
  match(mode, mode_lattice)
}

# Determine the promoted mode from a list of Fortran values.
# Used by: r2f-arithmetic.R, r2f-constructors.R
reduce_promoted_mode <- function(...) {
  getmode <- function(d) {
    if (inherits(d, Fortran)) {
      d <- d@value
    }
    if (inherits(d, Variable)) {
      return(d@mode)
    }
    if (is.list(d) && length(d)) {
      lapply(d, getmode)
    }
  }
  modes <- unique(unlist(getmode(list(...))))

  ranks <- mode_rank(modes)
  if (!length(ranks) || all(is.na(ranks))) {
    NULL
  } else {
    mode_lattice[[max(ranks, na.rm = TRUE)]]
  }
}

# Create a Variable with conforming dimensions from multiple inputs.
# Used by: r2f-arithmetic.R, r2f-logical.R, r2f-constructors.R, r2f-conditionals.R
conform <- function(..., mode = NULL) {
  vars <- drop_nulls(list(...))
  # Report the promoted (lattice-join) mode: the emitted expression already
  # promotes (Fortran's rules match R for numeric mixes), and `<-` copies
  # the reported mode verbatim into the target's declaration, so reporting
  # the first non-scalar's mode declared truncating targets.
  mode <- mode %||% reduce_promoted_mode(vars)
  var <- NULL
  for (var in vars) {
    if (passes_as_scalar(var)) {
      next
    } else {
      break
    }
  }
  if (is.null(var)) {
    NULL
  } else {
    Variable(mode %||% var@mode, var@dims)
  }
}
