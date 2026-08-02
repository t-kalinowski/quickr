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

# Check if a dimension expression is statically known and not 1. Symbolic
# dimensions are FALSE: "not provably 1" is not "provably not 1".
# Used by: maybe_reshape_vector_matrix()
dim_known_not_one <- function(x) {
  is_wholenumber(x) && !identical(as.integer(x), 1L)
}

# Check if a Fortran value is a 1x1 matrix.
# Used by: r2f-arithmetic.R, r2f-logical.R
is_one_by_one <- function(x) {
  stopifnot(inherits(x, Fortran))
  x@value@rank == 2L &&
    dim_is_one(x@value@dims[[1L]]) &&
    dim_is_one(x@value@dims[[2L]])
}

# Check if two dimension expressions provably match (both known and
# equal, or the identical symbolic expression). Weaker than
# check_elementwise_lengths(): no zero-length policy, no symbol
# normalization -- callers use it for routing/declaration decisions, not
# for the conformability contract.
# Used by: r2f-matrix.R (bind_common_dim), r2f-matrix-blas.R (solve routing)
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
# Used by: guard_conformable_dims()
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

# Render one side of a dim-comparison guard: a literal dim as the literal,
# anything else as the operand's actual extent (whole size when `axis` is
# NULL). size() is an inquiry, so applying it to operand expression text
# does not evaluate the operand.
# Used by: guard_conformable_dims()
guard_dim_f <- function(dim, operand, axis = NULL) {
  if (is_wholenumber(dim)) {
    return(as.character(as.integer(dim)))
  }
  if (is.null(axis)) {
    glue("size({operand})")
  } else {
    glue("size({operand}, {axis})")
  }
}

# The one conformability policy, shared by elementwise ops, ifelse(), and
# the BLAS/LAPACK lowerings: a statically known mismatch is a compile
# error; dims that cannot be compared statically get a statement-level
# runtime guard emitted before the consuming statement; provably equal
# dims need nothing. Never warn-and-proceed. `axis` NULL compares the
# operand's whole size (rank-1 operands).
#
# `hoist` is always live: r2f() opens one per statement before dispatching
# to a handler, and every caller forwards the one it received.
# emit_quickr_error_if() asserts it.
# Used by: maybe_reshape_vector_matrix(), r2f-conditionals.R, r2f-matrix*.R
guard_conformable_dims <- function(
  left_dim,
  right_dim,
  message,
  hoist,
  scope,
  left,
  right,
  left_axis = NULL,
  right_axis = NULL
) {
  stopifnot(is_string(message))
  conform <- check_elementwise_lengths(left_dim, right_dim)
  if (!conform$ok) {
    stop(message, call. = FALSE)
  }
  if (conform$unknown) {
    emit_quickr_error_if(
      glue(
        "{guard_dim_f(left_dim, left, left_axis)} /= {guard_dim_f(right_dim, right, right_axis)}"
      ),
      message,
      hoist,
      scope
    )
  }
  invisible(TRUE)
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

# Floor a double expression while staying in the real domain: Fortran
# FLOOR() returns an integer, so a large double (e.g. 1e20) would
# silently overflow. aint(x) truncates toward 0 (real result); adjust by
# -1 where truncation differs from floor (negative non-integers). `x` is
# spliced three times, so callers hoist non-trivial expressions first.
# Used by: r2f-math.R (floor), r2f-arithmetic.R (double %/%)
real_floor_expr <- function(x) {
  aint <- glue("aint({x})")
  glue("({aint} - merge(1.0_c_double, 0.0_c_double, ({x} < {aint})))")
}

# Convert a 1x1 matrix to a scalar.
# Used by: r2f-arithmetic.R, r2f-logical.R
scalarize_matrix <- function(mat) {
  stopifnot(inherits(mat, Fortran))
  out_val <- Variable(mat@value@mode)
  Fortran(glue("{mat}(1, 1)"), out_val)
}

# Reshape vector/matrix operands to match ranks for binary operations, and
# enforce the elementwise conformability policy via guard_conformable_dims()
# -- known-mismatched lengths are compile errors (R-style recycling is not
# supported; scalar broadcast is native), lengths that cannot be compared
# statically get a runtime size guard through `hoist`.
#
# `scalarize_one_by_one` mirrors R's split over length-1 arrays: arithmetic
# recycles a 1x1 matrix against a vector of statically known length != 1
# (deprecated in R but still the behavior: R drops the array dims). When
# the vector's length is only known at run time, the result's shape would
# depend on that value -- R keeps the 1x1 dims for a length-1 vector and
# drops them otherwise -- so the 1x1 falls through to the vector-matrix
# rule: a runtime guard requires length 1 and the result is a 1x1 matrix,
# an error where R would recycle. Comparisons and & | error in R itself,
# so strict callers pass FALSE and the 1x1 always takes the vector-matrix
# path.
# Used by: r2f-arithmetic.R, r2f-logical.R
maybe_reshape_vector_matrix <- function(
  left,
  right,
  hoist,
  scope,
  scalarize_one_by_one = TRUE
) {
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

  # Casts and booleanization wrap operands in expression text that Fortran
  # cannot index (`real(x, kind=c_double)(1, 1)` is invalid), so hoist
  # anything that is not a bare name before subscripting it.
  scalarize_via_hoist <- function(x) {
    if (!is.null(hoist)) {
      x <- hoist_unless_name(x, hoist)
    }
    scalarize_matrix(x)
  }

  if (
    scalarize_one_by_one &&
      left_rank == 2L &&
      right_rank == 1L &&
      is_one_by_one(left)
  ) {
    right_len <- dim_or_one(right, 1L)
    if (dim_known_not_one(right_len)) {
      left <- scalarize_via_hoist(left)
      left_rank <- 0L
    }
  } else if (
    scalarize_one_by_one &&
      left_rank == 1L &&
      right_rank == 2L &&
      is_one_by_one(right)
  ) {
    left_len <- dim_or_one(left, 1L)
    if (dim_known_not_one(left_len)) {
      right <- scalarize_via_hoist(right)
      right_rank <- 0L
    }
  }

  if (left_rank == 1L && right_rank == 1L) {
    vector_msg <- paste0(
      "elementwise vector operations require equal lengths or ",
      "a scalar operand; R-style recycling is not supported"
    )
    guard_conformable_dims(
      dim_or_one(left, 1L),
      dim_or_one(right, 1L),
      vector_msg,
      hoist,
      scope,
      left = left,
      right = right
    )
  }

  if (left_rank == 2L && right_rank == 2L) {
    matrix_msg <- "elementwise matrix operations require matching dimensions"
    left_dims <- matrix_dims(left)
    right_dims <- matrix_dims(right)
    for (axis in 1:2) {
      guard_conformable_dims(
        if (axis == 1L) left_dims$rows else left_dims$cols,
        if (axis == 1L) right_dims$rows else right_dims$cols,
        matrix_msg,
        hoist,
        scope,
        left = left,
        right = right,
        left_axis = axis,
        right_axis = axis
      )
    }
  }

  vec_mat_msg <- paste0(
    "elementwise vector-matrix operations require a scalar or ",
    "a vector length equal to the matrix first dimension (nrow)"
  )
  if (left_rank == 1L && right_rank == 2L) {
    right_dims <- matrix_dims(right)
    guard_conformable_dims(
      dim_or_one(left, 1L),
      right_dims$rows,
      vec_mat_msg,
      hoist,
      scope,
      left = left,
      right = right,
      right_axis = 1L
    )
    left <- reshape_vector_for_matrix(left, right_dims$rows, right_dims$cols)
  } else if (left_rank == 2L && right_rank == 1L) {
    left_dims <- matrix_dims(left)
    guard_conformable_dims(
      dim_or_one(right, 1L),
      left_dims$rows,
      vec_mat_msg,
      hoist,
      scope,
      left = right,
      right = left,
      right_axis = 1L
    )
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
