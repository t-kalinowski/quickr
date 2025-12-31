# Matrix-specific r2f handlers and helpers

# Return the R symbol name if operand is a bare symbol; otherwise NULL.
symbol_name_or_null <- function(x) {
  stopifnot(inherits(x, Fortran))
  r_expr <- x@r
  if (is.symbol(r_expr)) as.character(r_expr) else NULL
}

# Return the requested axis length, defaulting scalars (or missing axes) to 1L.
dim_or_one <- function(x, axis) {
  stopifnot(inherits(x, Fortran))
  stopifnot(is.numeric(axis), axis >= 1)
  axis <- as.integer(axis)
  dims <- x@value@dims
  if (axis <= length(dims) && !is.null(dims[[axis]])) {
    dims[[axis]]
  } else {
    1L
  }
}

# Interpret a Fortran value as a matrix for BLAS calls. Scalars become 1x1
# matrices, and vectors can be viewed as either row or column vectors.
matrix_dims <- function(x, orientation = c("matrix", "rowvec", "colvec")) {
  stopifnot(inherits(x, Fortran))
  orientation <- match.arg(orientation)
  rank <- x@value@rank
  rows <- dim_or_one(x, 1L)
  cols <- dim_or_one(x, 2L)

  if (rank == 0L) {
    rows <- 1L
    cols <- 1L
  } else if (rank == 1L) {
    if (orientation == "rowvec") {
      rows <- 1L
      cols <- dim_or_one(x, 1L)
    } else {
      rows <- dim_or_one(x, 1L)
      cols <- 1L
    }
  }

  list(rows = rows, cols = cols)
}

effective_dims <- function(dims, trans) {
  if (identical(trans, "T")) {
    list(rows = dims$cols, cols = dims$rows)
  } else {
    dims
  }
}

assert_conformable <- function(left, right, context) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    if (!identical(as.integer(left), as.integer(right))) {
      stop("non-conformable arguments in ", context, call. = FALSE)
    }
  }
}

unwrap_transpose_arg <- function(arg, scope, ..., hoist) {
  if (is_call(arg, quote(t)) && length(arg) == 2L) {
    inner <- r2f(arg[[2L]], scope, ..., hoist = hoist)
    inner <- maybe_cast_double(inner)
    if (inner@value@rank == 2L) {
      return(list(value = inner, trans = "T"))
    } else if (inner@value@rank == 1L) {
      len <- inner@value@dims[[1L]]
      val <- Variable("double", list(1L, len))
      return(list(
        value = Fortran(glue("reshape({inner}, [1, int({len})])"), val),
        trans = "N"
      ))
    } else if (inner@value@rank == 0L) {
      return(list(value = inner, trans = "N"))
    } else {
      stop("t() only supports rank 0-2 inputs")
    }
  }
  value <- r2f(arg, scope, ..., hoist = hoist)
  value <- maybe_cast_double(value)
  list(value = value, trans = "N")
}

# Whether it's safe and useful to write into dest (no aliasing with inputs)
can_use_output <- function(dest, left, right) {
  if (is.null(dest)) {
    return(FALSE)
  }
  output_name <- dest@name
  # check output name is not the same as left or right
  !identical(output_name, as.character(left)) &&
    !identical(output_name, as.character(right))
}

# Centralized GEMM emission with optional destination
# gemm: centralized BLAS GEMM emission.
# - 'hoist' is required and provided by r2f(); handlers thread it through so
#   helpers can pre-emit temporary assignments and BLAS calls.
gemm <- function(
  opA,
  opB,
  left,
  right,
  m,
  n,
  k,
  lda,
  ldb,
  ldc_expr,
  scope,
  hoist,
  dest = NULL
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  A_name <- symbol_name_or_null(left)
  if (is.null(A_name)) {
    tmp <- hoist$declare_tmp(
      mode = left@value@mode %||% "double",
      dims = left@value@dims
    )
    hoist$emit(glue("{tmp@name} = {left}"))
    A_name <- tmp@name
  }
  B_name <- symbol_name_or_null(right)
  if (is.null(B_name)) {
    tmp <- hoist$declare_tmp(
      mode = right@value@mode %||% "double",
      dims = right@value@dims
    )
    hoist$emit(glue("{tmp@name} = {right}"))
    B_name <- tmp@name
  }

  if (can_use_output(dest, left, right)) {
    hoist$emit(glue(
      "call dgemm('{opA}','{opB}', {m}, {n}, {k}, 1.0_c_double, {A_name}, {lda}, {B_name}, {ldb}, 0.0_c_double, {dest@name}, {ldc_expr})"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  output_var <- hoist$declare_tmp(mode = "double", dims = list(m, n))
  hoist$emit(glue(
    "call dgemm('{opA}','{opB}', {m}, {n}, {k}, 1.0_c_double, {A_name}, {lda}, {B_name}, {ldb}, 0.0_c_double, {output_var@name}, {ldc_expr})"
  ))
  Fortran(output_var@name, output_var)
}

# Centralized GEMV emission with optional destination
# gemv: centralized BLAS GEMV emission.
# - 'hoist' is required and provided by r2f(); handlers thread it through so
#   helpers can pre-emit temporary assignments and BLAS calls.
gemv <- function(
  transA,
  A,
  x,
  m,
  n,
  lda,
  out_dims,
  scope,
  hoist,
  dest = NULL
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  A_name <- symbol_name_or_null(A)
  if (is.null(A_name)) {
    tmp <- hoist$declare_tmp(
      mode = A@value@mode %||% "double",
      dims = A@value@dims
    )
    hoist$emit(glue("{tmp@name} = {A}"))
    A_name <- tmp@name
  }
  x_name <- symbol_name_or_null(x)
  if (is.null(x_name)) {
    tmp <- hoist$declare_tmp(
      mode = x@value@mode %||% "double",
      dims = x@value@dims
    )
    hoist$emit(glue("{tmp@name} = {x}"))
    x_name <- tmp@name
  }

  if (can_use_output(dest, A, x)) {
    # Assign output to output destination
    hoist$emit(glue(
      "call dgemv('{transA}', {m}, {n}, 1.0_c_double, {A_name}, {lda}, {x_name}, 1, 0.0_c_double, {dest@name}, 1)"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }
  # Else assign to a temporary variable
  output_var <- hoist$declare_tmp(mode = "double", dims = out_dims)
  hoist$emit(glue(
    "call dgemv('{transA}', {m}, {n}, 1.0_c_double, {A_name}, {lda}, {x_name}, 1, 0.0_c_double, {output_var@name}, 1)"
  ))
  Fortran(output_var@name, output_var)
}

# Centralized SYRK emission for symmetric rank-k update
# Computes: C := alpha * op(A) * op(A)^T + beta * C
# For crossprod(X):  C = t(X) %*% X  → trans = "T"
# For tcrossprod(X): C = X %*% t(X)  → trans = "N"
syrk <- function(
  trans,
  X,
  scope,
  hoist,
  dest = NULL
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }

  X_name <- symbol_name_or_null(X)
  if (is.null(X_name)) {
    tmp <- hoist$declare_tmp(
      mode = X@value@mode %||% "double",
      dims = X@value@dims
    )
    hoist$emit(glue("{tmp@name} = {X}"))
    X_name <- tmp@name
  }

  x_dims <- matrix_dims(X)

  # For trans = "T": C = t(X) %*% X, so C is k x k where k = ncol(X)
  # For trans = "N": C = X %*% t(X), so C is n x n where n = nrow(X)
  if (trans == "T") {
    n <- x_dims$cols
    k <- x_dims$rows
  } else {
    n <- x_dims$rows
    k <- x_dims$cols
  }
  lda <- x_dims$rows

  # Output is symmetric n x n matrix
  if (can_use_output(dest, X, X)) {
    hoist$emit(glue(
      "call dsyrk('U', '{trans}', {n}, {k}, 1.0_c_double, {X_name}, {lda}, 0.0_c_double, {dest@name}, {n})"
    ))
    # Fill lower triangle from upper
    idx_i <- hoist$declare_tmp(mode = "integer", dims = list(1L))
    idx_j <- hoist$declare_tmp(mode = "integer", dims = list(1L))
    hoist$emit(glue(
      "
do {idx_j@name} = 1_c_int, {n} - 1_c_int
  do {idx_i@name} = {idx_j@name} + 1_c_int, {n}
    {dest@name}({idx_i@name}, {idx_j@name}) = {dest@name}({idx_j@name}, {idx_i@name})
  end do
end do"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  output_var <- hoist$declare_tmp(mode = "double", dims = list(n, n))
  hoist$emit(glue(
    "call dsyrk('U', '{trans}', {n}, {k}, 1.0_c_double, {X_name}, {lda}, 0.0_c_double, {output_var@name}, {n})"
  ))
  # Fill lower triangle from upper
  idx_i <- hoist$declare_tmp(mode = "integer", dims = list(1L))
  idx_j <- hoist$declare_tmp(mode = "integer", dims = list(1L))
  hoist$emit(glue(
    "
do {idx_j@name} = 1_c_int, {n} - 1_c_int
  do {idx_i@name} = {idx_j@name} + 1_c_int, {n}
    {output_var@name}({idx_i@name}, {idx_j@name}) = {output_var@name}({idx_j@name}, {idx_i@name})
  end do
end do"
  ))
  Fortran(output_var@name, output_var)
}
# ---- matrix operation handlers ----

# %*% handler with optional destination hint
r2f_handlers[["%*%"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  stopifnot(length(args) == 2L)
  left_info <- unwrap_transpose_arg(args[[1L]], scope, ..., hoist = hoist)
  right_info <- unwrap_transpose_arg(args[[2L]], scope, ..., hoist = hoist)
  left <- left_info$value
  right <- right_info$value
  left_trans <- left_info$trans
  right_trans <- right_info$trans

  left_rank <- left@value@rank
  right_rank <- right@value@rank

  if (left_rank > 2 || right_rank > 2) {
    stop("%*% only supports vectors/matrices (rank <= 2)")
  }

  left_dims <- matrix_dims(
    left,
    orientation = if (left_rank == 1) "rowvec" else "matrix"
  )

  right_dims <- matrix_dims(
    right,
    orientation = if (right_rank == 1) "colvec" else "matrix"
  )

  left_eff <- if (left_rank == 2) {
    effective_dims(left_dims, left_trans)
  } else {
    left_dims
  }
  right_eff <- if (right_rank == 2) {
    effective_dims(right_dims, right_trans)
  } else {
    right_dims
  }

  # Compute effective shapes
  m <- left_eff$rows
  k <- left_eff$cols
  n <- right_eff$cols

  # Leading dimensions
  lda <- left_dims$rows
  ldb <- right_dims$rows
  ldc_expr <- m

  # Matrix-Vector: use GEMV
  if (left_rank == 2 && right_rank == 1) {
    expected_len <- if (left_trans == "N") left_dims$cols else left_dims$rows
    assert_conformable(expected_len, right_dims$rows, "%*%")
    out_len <- if (left_trans == "N") left_dims$rows else left_dims$cols
    return(gemv(
      transA = left_trans,
      A = left,
      x = right,
      m = left_dims$rows,
      n = left_dims$cols,
      lda = left_dims$rows,
      out_dims = list(out_len, 1L),
      scope = scope,
      hoist = hoist,
      dest = dest
    ))
  }
  # Vector-Matrix: use GEMV with transpose
  if (left_rank == 1 && right_rank == 2) {
    transA <- if (right_trans == "N") "T" else "N"
    expected_len <- if (transA == "N") right_dims$cols else right_dims$rows
    assert_conformable(left_dims$cols, expected_len, "%*%")
    out_len <- if (transA == "N") right_dims$rows else right_dims$cols
    return(gemv(
      transA = transA,
      A = right,
      x = left,
      m = right_dims$rows,
      n = right_dims$cols,
      lda = right_dims$rows,
      out_dims = list(1L, out_len),
      scope = scope,
      hoist = hoist,
      dest = dest
    ))
  }

  assert_conformable(k, right_eff$rows, "%*%")

  # Matrix-Matrix
  gemm(
    opA = left_trans,
    opB = right_trans,
    left = left,
    right = right,
    m = m,
    n = n,
    k = k,
    lda = lda,
    ldb = ldb,
    ldc_expr = ldc_expr,
    scope = scope,
    hoist = hoist,
    dest = dest
  )
}


# t(x) handler: transpose 2D; 1D becomes a 1 x n row matrix
r2f_handlers[["t"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  if (x@value@rank == 2) {
    val <- Variable("double", list(x@value@dims[[2]], x@value@dims[[1]]))
    return(Fortran(glue("transpose({x})"), val))
  } else if (x@value@rank == 1) {
    len <- x@value@dims[[1]]
    val <- Variable("double", list(1L, len))
    return(Fortran(glue("reshape({x}, [1, int({len})])"), val))
  } else if (x@value@rank == 0) {
    return(x)
  } else {
    stop("t() only supports rank 0-2 inputs")
  }
}


r2f_handlers[["crossprod"]] <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  dest = NULL
) {
  x_arg <- args[[1L]]
  y_arg <- if (length(args) > 1L) args[[2L]] else NULL

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)

  # Single-argument case: crossprod(X) = t(X) %*% X → use dsyrk
  if (is.null(y_arg)) {
    return(syrk(
      trans = "T",
      X = x,
      scope = scope,
      hoist = hoist,
      dest = dest
    ))
  }

  # Two-argument case: crossprod(X, Y) = t(X) %*% Y → use dgemm
  y <- maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  x_dims <- matrix_dims(x)
  y_dims <- matrix_dims(y)

  assert_conformable(x_dims$rows, y_dims$rows, "crossprod")

  m <- x_dims$cols
  n <- y_dims$cols
  k <- x_dims$rows

  lda <- x_dims$rows
  ldb <- y_dims$rows
  ldc_expr <- m

  gemm(
    opA = "T",
    opB = "N",
    left = x,
    right = y,
    m = m,
    n = n,
    k = k,
    lda = lda,
    ldb = ldb,
    ldc_expr = ldc_expr,
    scope = scope,
    hoist = hoist,
    dest = dest
  )
}


r2f_handlers[["tcrossprod"]] <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  dest = NULL
) {
  x_arg <- args[[1L]]
  y_arg <- if (length(args) > 1L) args[[2L]] else NULL

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)

  # Single-argument case: tcrossprod(X) = X %*% t(X) → use dsyrk
  if (is.null(y_arg)) {
    return(syrk(
      trans = "N",
      X = x,
      scope = scope,
      hoist = hoist,
      dest = dest
    ))
  }

  # Two-argument case: tcrossprod(X, Y) = X %*% t(Y) → use dgemm
  y <- maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  x_dims <- matrix_dims(x)
  y_dims <- matrix_dims(y)

  assert_conformable(x_dims$cols, y_dims$cols, "tcrossprod")

  m <- x_dims$rows
  n <- y_dims$rows
  k <- x_dims$cols

  lda <- x_dims$rows
  ldb <- y_dims$rows
  ldc_expr <- m

  gemm(
    opA = "N",
    opB = "T",
    left = x,
    right = y,
    m = m,
    n = n,
    k = k,
    lda = lda,
    ldb = ldb,
    ldc_expr = ldc_expr,
    scope = scope,
    hoist = hoist,
    dest = dest
  )
}
