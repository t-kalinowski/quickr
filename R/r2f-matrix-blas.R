# Matrix BLAS/LAPACK emission helpers

# ---- shared matrix helpers (loaded early for implicit collation) ----

# Return the R symbol name if operand is a bare symbol; otherwise NULL.
symbol_name_or_null <- function(x) {
  stopifnot(inherits(x, Fortran))
  r_expr <- unwrap_parens(x@r)
  if (is.symbol(r_expr)) {
    return(as.character(r_expr))
  }
  if (length(x) == 1L && grepl("^[A-Za-z][A-Za-z0-9_]*$", x)) {
    return(as.character(x))
  }
  NULL
}

# Return a dimension value for an axis, defaulting missing dims to 1L.
dim_or_one_from <- function(dims, axis) {
  stopifnot(is.numeric(axis), axis >= 1)
  axis <- as.integer(axis)
  if (is.null(dims)) {
    return(1L)
  }
  if (axis <= length(dims) && !is.null(dims[[axis]])) {
    dims[[axis]]
  } else {
    1L
  }
}

# Return the requested axis length, defaulting scalars (or missing axes) to 1L.
dim_or_one <- function(x, axis) {
  stopifnot(inherits(x, Fortran))
  dim_or_one_from(x@value@dims, axis)
}

# Return the requested axis length for a Variable, defaulting to 1L.
var_dim_or_one <- function(var, axis) {
  stopifnot(inherits(var, Variable))
  dim_or_one_from(var@dims, axis)
}

# Compute matrix-style row/column dimensions from rank, dims, and orientation.
matrix_dims_from <- function(
  rank,
  dims,
  orientation = c("matrix", "rowvec", "colvec")
) {
  orientation <- match.arg(orientation)
  rows <- dim_or_one_from(dims, 1L)
  cols <- dim_or_one_from(dims, 2L)

  if (rank == 0L) {
    rows <- 1L
    cols <- 1L
  } else if (rank == 1L) {
    if (orientation == "rowvec") {
      rows <- 1L
      cols <- dim_or_one_from(dims, 1L)
    } else {
      rows <- dim_or_one_from(dims, 1L)
      cols <- 1L
    }
  }

  list(rows = rows, cols = cols)
}

# Interpret a Fortran value as a matrix for BLAS calls. Scalars become 1x1
# matrices, and vectors can be viewed as either row or column vectors.
matrix_dims <- function(x, orientation = c("matrix", "rowvec", "colvec")) {
  stopifnot(inherits(x, Fortran))
  matrix_dims_from(x@value@rank, x@value@dims, orientation = orientation)
}

# Interpret a Variable value as a matrix for BLAS calls.
matrix_dims_var <- function(
  var,
  orientation = c("matrix", "rowvec", "colvec")
) {
  stopifnot(inherits(var, Variable))
  matrix_dims_from(var@rank, var@dims, orientation = orientation)
}

# Compute effective dimensions based on transpose flags.
effective_dims <- function(dims, trans) {
  if (identical(trans, "T")) {
    list(rows = dims$cols, cols = dims$rows)
  } else {
    dims
  }
}

# Return conformability status (ok/unknown) without side-effects.
check_conformable <- function(left, right) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    ok <- identical(as.integer(left), as.integer(right))
    return(list(ok = ok, unknown = FALSE))
  }
  if (identical(left, right)) {
    return(list(ok = TRUE, unknown = FALSE))
  }
  list(ok = TRUE, unknown = TRUE)
}

warn_conformability_unknown <- function(left, right, context) {
  left_txt <- if (is.null(left)) "NULL" else deparse(left)
  right_txt <- if (is.null(right)) "NULL" else deparse(right)
  warning(
    "cannot verify conformability in ",
    context,
    " at compile time: ",
    left_txt,
    " vs ",
    right_txt,
    call. = FALSE
  )
  invisible(FALSE)
}

# ---- BLAS emitters ----

# Check that destination dimensions match expected output dimensions.
assert_dest_dims_compatible <- function(dest, expected_dims, context) {
  if (is.null(dest) || is.null(expected_dims)) {
    return(invisible(TRUE))
  }
  expected_rank <- length(expected_dims)
  if (dest@rank != expected_rank) {
    stop("assignment target has incompatible rank for ", context, call. = FALSE)
  }
  for (i in seq_len(expected_rank)) {
    dest_dim <- dest@dims[[i]]
    expected_dim <- expected_dims[[i]]
    if (is_wholenumber(dest_dim) && is_wholenumber(expected_dim)) {
      if (!identical(as.integer(dest_dim), as.integer(expected_dim))) {
        stop(
          "assignment target has incompatible dimensions for ",
          context,
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}

# Determine if output can safely write into dest without aliasing.
can_use_output <- function(
  dest,
  input_names = character(),
  expected_dims = NULL,
  context,
  allow_alias = character()
) {
  if (is.null(dest)) {
    return(FALSE)
  }
  if (!identical(dest@mode, "double")) {
    return(FALSE)
  }
  assert_dest_dims_compatible(dest, expected_dims, context)
  output_name <- dest@name
  if (is.null(output_name) || !nzchar(output_name)) {
    return(FALSE)
  }

  input_names <- unique(as.character(input_names))
  input_names <- input_names[nzchar(input_names)]
  allow_alias <- unique(as.character(allow_alias))
  allow_alias <- allow_alias[nzchar(allow_alias)]
  disallowed <- setdiff(input_names, allow_alias)

  !output_name %in% disallowed
}

# Ensure a BLAS operand is named, hoisting into a temp if needed.
ensure_blas_operand_name <- function(x, hoist) {
  name <- symbol_name_or_null(x)
  if (!is.null(name)) {
    return(name)
  }
  tmp <- hoist$declare_tmp(
    mode = x@value@mode %||% "double",
    dims = x@value@dims
  )
  hoist$emit(glue("{tmp@name} = {x}"))
  tmp@name
}

# Wrap an expression as a BLAS int literal.
blas_int <- function(x) {
  glue("int({x}, kind=c_int)")
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
  dest = NULL,
  context = "gemm"
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  A_name <- ensure_blas_operand_name(left, hoist)
  B_name <- ensure_blas_operand_name(right, hoist)

  if (
    can_use_output(
      dest,
      input_names = c(A_name, B_name),
      expected_dims = list(m, n),
      context = context
    )
  ) {
    hoist$emit(glue(
      "call dgemm('{opA}','{opB}', {blas_int(m)}, {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {B_name}, {blas_int(ldb)}, 0.0_c_double, {dest@name}, {blas_int(ldc_expr)})"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  output_var <- hoist$declare_tmp(mode = "double", dims = list(m, n))
  hoist$emit(glue(
    "call dgemm('{opA}','{opB}', {blas_int(m)}, {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {B_name}, {blas_int(ldb)}, 0.0_c_double, {output_var@name}, {blas_int(ldc_expr)})"
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
  dest = NULL,
  context = "gemv"
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  A_name <- ensure_blas_operand_name(A, hoist)
  x_name <- ensure_blas_operand_name(x, hoist)

  if (
    can_use_output(
      dest,
      input_names = c(A_name, x_name),
      expected_dims = out_dims,
      context = context
    )
  ) {
    # Assign output to output destination
    hoist$emit(glue(
      "call dgemv('{transA}', {blas_int(m)}, {blas_int(n)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {x_name}, 1_c_int, 0.0_c_double, {dest@name}, 1_c_int)"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }
  # Else assign to a temporary variable
  output_var <- hoist$declare_tmp(mode = "double", dims = out_dims)
  hoist$emit(glue(
    "call dgemv('{transA}', {blas_int(m)}, {blas_int(n)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {x_name}, 1_c_int, 0.0_c_double, {output_var@name}, 1_c_int)"
  ))
  Fortran(output_var@name, output_var)
}

symmetrize_upper_to_lower <- function(target, n, hoist) {
  stopifnot(is_string(target), inherits(hoist, "environment"))

  idx_i <- hoist$declare_tmp(mode = "integer", dims = list(1L))
  idx_j <- hoist$declare_tmp(mode = "integer", dims = list(1L))
  n_int <- blas_int(n)
  hoist$emit(glue(
    "
do {idx_j@name} = 1_c_int, {n_int} - 1_c_int
  do {idx_i@name} = {idx_j@name} + 1_c_int, {n_int}
    {target}({idx_i@name}, {idx_j@name}) = {target}({idx_j@name}, {idx_i@name})
  end do
end do"
  ))
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
  dest = NULL,
  context = "syrk"
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  X_name <- ensure_blas_operand_name(X, hoist)

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
  writes_to_dest <- FALSE
  out_var <- NULL
  out_name <- NULL

  if (
    can_use_output(
      dest,
      input_names = X_name,
      expected_dims = list(n, n),
      context = context
    )
  ) {
    writes_to_dest <- TRUE
    out_var <- dest
    out_name <- dest@name
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = list(n, n))
    out_name <- out_var@name
  }

  hoist$emit(glue(
    "call dsyrk('U', '{trans}', {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {X_name}, {blas_int(lda)}, 0.0_c_double, {out_name}, {blas_int(n)})"
  ))
  symmetrize_upper_to_lower(out_name, n, hoist = hoist)

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    attr(out, "writes_to_dest") <- TRUE
  }
  out
}

# Emit BLAS outer product for vectors or scalars with optional destination.
outer_mul <- function(
  x,
  y,
  scope,
  hoist,
  dest = NULL,
  context = "outer"
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }

  x <- maybe_cast_double(x)
  y <- maybe_cast_double(y)

  if (x@value@rank > 1L || y@value@rank > 1L) {
    stop("outer() only supports vectors or scalars")
  }

  m <- dim_or_one(x, 1L)
  n <- dim_or_one(y, 1L)

  x_name <- ensure_blas_operand_name(x, hoist)
  y_name <- ensure_blas_operand_name(y, hoist)

  if (
    can_use_output(
      dest,
      input_names = c(x_name, y_name),
      expected_dims = list(m, n),
      context = context
    )
  ) {
    hoist$emit(glue("{dest@name} = 0.0_c_double"))
    hoist$emit(glue(
      "call dger({blas_int(m)}, {blas_int(n)}, 1.0_c_double, {x_name}, 1_c_int, {y_name}, 1_c_int, {dest@name}, {blas_int(m)})"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  output_var <- hoist$declare_tmp(mode = "double", dims = list(m, n))
  hoist$emit(glue("{output_var@name} = 0.0_c_double"))
  hoist$emit(glue(
    "call dger({blas_int(m)}, {blas_int(n)}, 1.0_c_double, {x_name}, 1_c_int, {y_name}, 1_c_int, {output_var@name}, {blas_int(m)})"
  ))
  Fortran(output_var@name, output_var)
}

# Emit triangular solve (vector or matrix RHS) with optional destination.
triangular_solve <- function(
  A,
  B,
  uplo,
  trans,
  diag,
  scope,
  hoist,
  dest = NULL,
  context = "triangular solve"
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }

  A <- maybe_cast_double(A)
  B <- maybe_cast_double(B)

  if (A@value@rank != 2L) {
    stop("triangular solve expects a matrix")
  }

  a_dims <- matrix_dims(A)
  conform <- check_conformable(a_dims$rows, a_dims$cols)
  if (!conform$ok) {
    stop("non-conformable arguments in triangular solve", call. = FALSE)
  }
  if (conform$unknown) {
    warn_conformability_unknown(a_dims$rows, a_dims$cols, "triangular solve")
  }
  n <- a_dims$rows

  b_rank <- B@value@rank
  if (b_rank > 2L) {
    stop("triangular solve only supports vector or matrix right-hand sides")
  }
  if (b_rank == 0L) {
    stop("triangular solve expects a vector or matrix right-hand side")
  } else if (b_rank == 1L) {
    b_len <- dim_or_one(B, 1L)
    conform <- check_conformable(n, b_len)
    if (!conform$ok) {
      stop("non-conformable arguments in triangular solve", call. = FALSE)
    }
    if (conform$unknown) {
      warn_conformability_unknown(n, b_len, "triangular solve")
    }
  } else {
    b_rows <- dim_or_one(B, 1L)
    conform <- check_conformable(n, b_rows)
    if (!conform$ok) {
      stop("non-conformable arguments in triangular solve", call. = FALSE)
    }
    if (conform$unknown) {
      warn_conformability_unknown(n, b_rows, "triangular solve")
    }
  }

  A_name <- ensure_blas_operand_name(A, hoist)
  B_input_name <- symbol_name_or_null(B)

  if (
    can_use_output(
      dest,
      input_names = c(A_name, B_input_name),
      expected_dims = B@value@dims,
      context = context,
      allow_alias = B_input_name
    )
  ) {
    hoist$emit(glue("{dest@name} = {B}"))
    B_name <- dest@name
    out_var <- dest
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(
      mode = B@value@mode %||% "double",
      dims = B@value@dims
    )
    hoist$emit(glue("{out_var@name} = {B}"))
    B_name <- out_var@name
    writes_to_dest <- FALSE
  }

  if (b_rank <= 1L) {
    hoist$emit(glue(
      "call dtrsv('{uplo}', '{trans}', '{diag}', {blas_int(n)}, {A_name}, {blas_int(n)}, {B_name}, 1_c_int)"
    ))
  } else {
    nrhs <- dim_or_one(B, 2L)
    hoist$emit(glue(
      "call dtrsm('L', '{uplo}', '{trans}', '{diag}', {blas_int(n)}, {blas_int(nrhs)}, 1.0_c_double, {A_name}, {blas_int(n)}, {B_name}, {blas_int(n)})"
    ))
  }

  out <- Fortran(B_name, out_var)
  if (writes_to_dest) {
    attr(out, "writes_to_dest") <- TRUE
  }
  out
}
