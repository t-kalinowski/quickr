# r2f-operators-helpers.R
# Generic helpers for binary operators and type conformance.

# Cast a value to double if it's logical or integer.
# Used by: r2f-arithmetic.R
maybe_cast_double <- function(x) {
  if (x@value@mode == "logical") {
    Fortran(
      glue("merge(1_c_double, 0_c_double, {x})"),
      Variable("double", x@value@dims)
    )
  } else if (x@value@mode == "integer") {
    Fortran(
      glue("real({x}, kind=c_double)"),
      Variable("double", x@value@dims)
    )
  } else {
    x
  }
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
    "reshape({source}, [{bind_dim_int(rows)}, {bind_dim_int(cols)}])"
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

# Reshape vector/matrix operands to match ranks for binary operations.
# Used by: r2f-arithmetic.R, r2f-logical.R
maybe_reshape_vector_matrix <- function(left, right) {
  if (
    !inherits(left, Fortran) ||
      !inherits(right, Fortran) ||
      is.null(left@value) ||
      is.null(right@value)
  ) {
    return(list(left = left, right = right))
  }

  left_rank <- left@value@rank
  right_rank <- right@value@rank

  if (left_rank == 1L && right_rank == 2L) {
    right_dims <- matrix_dims(right)
    left_len <- dim_or_one(left, 1L)
    if (dim_is_one(right_dims$cols) && dims_match(right_dims$rows, left_len)) {
      left <- reshape_vector_for_matrix(left, right_dims$rows, right_dims$cols)
    } else if (
      dim_is_one(right_dims$rows) &&
        dims_match(right_dims$cols, left_len)
    ) {
      left <- reshape_vector_for_matrix(left, right_dims$rows, right_dims$cols)
    }
  } else if (left_rank == 2L && right_rank == 1L) {
    left_dims <- matrix_dims(left)
    right_len <- dim_or_one(right, 1L)
    if (dim_is_one(left_dims$cols) && dims_match(left_dims$rows, right_len)) {
      right <- reshape_vector_for_matrix(right, left_dims$rows, left_dims$cols)
    } else if (
      dim_is_one(left_dims$rows) &&
        dims_match(left_dims$cols, right_len)
    ) {
      right <- reshape_vector_for_matrix(right, left_dims$rows, left_dims$cols)
    }
  }

  left_rank <- left@value@rank
  right_rank <- right@value@rank
  if (left_rank == 2L && right_rank == 1L && is_one_by_one(left)) {
    right_len <- dim_or_one(right, 1L)
    if (!dim_is_one(right_len)) {
      left <- scalarize_matrix(left)
    }
  } else if (left_rank == 1L && right_rank == 2L && is_one_by_one(right)) {
    left_len <- dim_or_one(left, 1L)
    if (!dim_is_one(left_len)) {
      right <- scalarize_matrix(right)
    }
  }

  list(left = left, right = right)
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

  if ("double" %in% modes) {
    "double"
  } else if ("integer" %in% modes) {
    "integer"
  } else if ("logical" %in% modes) {
    "logical"
  } else {
    NULL
  }
}

# Create a Variable with conforming dimensions from multiple inputs.
# Used by: r2f-arithmetic.R, r2f-logical.R, r2f-constructors.R, r2f-conditionals.R
conform <- function(..., mode = NULL) {
  var <- NULL
  # technically, types are implicit promoted, but we'll let <- handle that.
  for (var in drop_nulls(list(...))) {
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
