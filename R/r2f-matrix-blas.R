# Matrix BLAS/LAPACK emission helpers

# ---- shared matrix helpers (loaded early for implicit collation) ----

# Assert hoist is a valid environment for BLAS/LAPACK helpers.
assert_hoist_env <- function(hoist) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  invisible(TRUE)
}

# Assert a Fortran value is a rank-2 matrix.
assert_rank2_matrix <- function(x, message) {
  stopifnot(inherits(x, Fortran), is_string(message))
  if (x@value@rank != 2L) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

# Assert a Fortran value is a scalar or vector.
assert_rank_leq1 <- function(x, message) {
  stopifnot(inherits(x, Fortran), is_string(message))
  if (x@value@rank > 1L) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

# Assert a Fortran value is rank 0-2.
assert_rank_leq2 <- function(x, message) {
  stopifnot(inherits(x, Fortran), is_string(message))
  if (x@value@rank > 2L) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

# Assert right-hand side rank is vector or matrix.
assert_rhs_rank <- function(
  rank,
  err_scalar,
  err_high,
  call_scalar = FALSE,
  call_high = FALSE
) {
  stopifnot(
    is_wholenumber(rank),
    is_string(err_scalar),
    is_string(err_high),
    is_bool(call_scalar),
    is_bool(call_high)
  )
  if (rank > 2L) {
    stop(err_high, call. = call_high)
  }
  if (rank == 0L) {
    stop(err_scalar, call. = call_scalar)
  }
  invisible(TRUE)
}

# BLAS/LAPACK dimensions use equality semantics: equal zero contracted
# dimensions are conformable and can still produce a non-empty result.
check_blas_dims <- function(left, right) {
  check_equal_dims(left, right)
}

# Return the storage name if the operand is a bare named variable.
symbol_name_or_null <- function(x) {
  stopifnot(inherits(x, Fortran))
  code <- trimws(as.character(x))
  if (
    inherits(x@value, Variable) &&
      !is.null(x@value@name) &&
      identical(code, x@value@name)
  ) {
    return(x@value@name)
  }
  r_expr <- unwrap_parens(x@r)
  if (is.symbol(r_expr)) {
    return(as.character(r_expr))
  }
  if (length(x) == 1L && grepl("^[A-Za-z][A-Za-z0-9_]*$", code)) {
    return(code)
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

# Enforce that `dims` describe a square matrix: a known mismatch is a
# compile error; unverifiable dims get a runtime guard on the operand's
# actual extents.
assert_square_matrix <- function(dims, operand, context, hoist, scope) {
  guard_conformable_dims(
    dims$rows,
    dims$cols,
    paste0(context, " requires a square matrix"),
    hoist,
    scope,
    left = operand,
    right = operand,
    left_axis = 1L,
    right_axis = 2L,
    checker = check_blas_dims
  )
}

# ---- BLAS emitters ----

# Generated function results cannot currently represent zero-sized arrays.
# Reject a known zero output during translation and guard unknown output
# extents at runtime before emitting a BLAS call with an invalid leading
# dimension. A zero contracted dimension remains supported when every output
# extent is nonzero.
assert_nonempty_blas_output <- function(
  dim,
  operand,
  axis,
  context,
  hoist,
  scope
) {
  stopifnot(
    inherits(operand, Fortran),
    is.numeric(axis),
    length(axis) == 1L,
    is_string(context)
  )
  message <- paste0(context, " zero-sized outputs are not supported")
  if (is_wholenumber(dim)) {
    if (as.integer(dim) == 0L) {
      stop(message, call. = FALSE)
    }
    return(invisible(TRUE))
  }

  emit_quickr_error_if(
    glue("{guard_dim_f(dim, operand, axis)} == 0_c_ptrdiff_t"),
    message,
    hoist,
    scope
  )
  invisible(TRUE)
}

# Check that destination dimensions match expected output dimensions.
assert_dest_dims_compatible <- function(dest, expected_dims, context) {
  if (is.null(dest) || is.null(expected_dims)) {
    return(TRUE)
  }
  expected_rank <- length(expected_dims)
  if (dest@rank != expected_rank) {
    stop("assignment target has incompatible rank for ", context, call. = FALSE)
  }
  proven <- TRUE
  for (i in seq_len(expected_rank)) {
    dest_dim <- dest@dims[[i]]
    expected_dim <- expected_dims[[i]]
    verdict <- check_equal_dims(dest_dim, expected_dim)
    if (!verdict$ok) {
      stop(
        "assignment target has incompatible dimensions for ",
        context,
        call. = FALSE
      )
    }
    proven <- proven && !verdict$unknown
  }
  proven
}

# Determine if output can safely write into dest without aliasing.
can_use_output <- function(
  dest,
  input_names = character(),
  expected_dims = NULL,
  context,
  allow_alias = character(),
  mode = "double",
  logical_is_c_int = FALSE
) {
  stopifnot(
    is_bool(logical_is_c_int),
    !logical_is_c_int || identical(mode, "logical")
  )
  if (is.null(dest)) {
    return(FALSE)
  }
  if (!identical(dest@mode, mode)) {
    return(FALSE)
  }
  if (!identical(logical_as_int(dest), logical_is_c_int)) {
    return(FALSE)
  }
  dims_proven <- assert_dest_dims_compatible(dest, expected_dims, context)
  if (!dims_proven && isTRUE(dest@is_external)) {
    stop(
      "cannot change the shape of an external assignment target in ",
      context,
      call. = FALSE
    )
  }
  if (!dims_proven) {
    # Local allocatables fall back to intrinsic assignment, which reallocates
    # them to the temporary result's shape. External arrays have fixed ABI
    # extents and are rejected above.
    return(FALSE)
  }
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

allocate_reusable_local_output_at_point <- function(dest, scope, hoist) {
  stopifnot(inherits(dest, Variable), inherits(scope, "quickr_scope"))
  assert_hoist_env(hoist)

  if (
    !identical(scope_kind(scope), "subroutine") ||
      isTRUE(dest@is_external) ||
      !is.na(var_element_count(dest)) ||
      !subroutine_local_allocatable(dest, scope)
  ) {
    return(invisible(dest))
  }

  return_names <- scope_get(scope, "return_names", character()) %||%
    character()
  return_fortran_names <- vapply(
    return_names,
    fortranize_name,
    character(1L)
  )
  if (tolower(dest@name) %in% tolower(return_fortran_names)) {
    return(invisible(dest))
  }

  point_allocated <- scope_get(
    scope,
    "point_allocated_local_names",
    character()
  )
  scope_set(
    scope,
    "point_allocated_local_names",
    unique(c(point_allocated, dest@name))
  )
  hoist$emit(glue(
    "if (.not. allocated({dest@name})) allocate({dest@name}({dims2f(dest@dims, scope)}))"
  ))
  invisible(dest)
}

# Ensure a BLAS operand is named, hoisting into a temp if needed.
ensure_blas_operand_name <- function(x, hoist) {
  name <- symbol_name_or_null(x)
  if (!is.null(name)) {
    return(name)
  }
  tmp <- hoist$declare_tmp(
    mode = x@value@mode %||% "double",
    dims = x@value@dims,
    logical_as_int = logical_as_int(x@value)
  )
  hoist$emit(glue("{tmp@name} = {x}"))
  tmp@name
}

# Wrap an expression as a BLAS int literal.
blas_int <- function(x) {
  x_str <- if (is.language(x)) {
    gsub("([0-9]+)L\\b", "\\1", deparse1(x))
  } else if (is_wholenumber(x)) {
    as.character(as.integer(x))
  } else {
    as.character(x)
  }
  glue("int({x_str}, kind=c_int)")
}

# Emit a BLAS call for positive contractions and fill the result with zero
# without calling BLAS when the contracted dimension is zero.
emit_blas_contraction <- function(call, output, contracted_dim, hoist) {
  stopifnot(is_string(call), is_string(output))
  assert_hoist_env(hoist)

  if (is_wholenumber(contracted_dim)) {
    if (as.integer(contracted_dim) == 0L) {
      hoist$emit(glue("{output} = 0.0_c_double"))
    } else {
      hoist$emit(call)
    }
    return(invisible(TRUE))
  }

  hoist$emit(glue(
    "
if ({blas_int(contracted_dim)} == 0_c_int) then
  {output} = 0.0_c_double
else
  {call}
end if"
  ))
  invisible(TRUE)
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
  assert_hoist_env(hoist)
  left <- cast_linalg_double(left, context, hoist)
  right <- cast_linalg_double(right, context, hoist)
  assert_nonempty_blas_output(
    m,
    left,
    if (opA == "N") 1L else 2L,
    context,
    hoist,
    scope
  )
  assert_nonempty_blas_output(
    n,
    right,
    if (opB == "N") 2L else 1L,
    context,
    hoist,
    scope
  )
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
    allocate_reusable_local_output_at_point(dest, scope, hoist)
    blas_call <- glue(
      "call dgemm('{opA}','{opB}', {blas_int(m)}, {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {B_name}, {blas_int(ldb)}, 0.0_c_double, {dest@name}, {blas_int(ldc_expr)})"
    )
    emit_blas_contraction(blas_call, dest@name, k, hoist)
    out <- Fortran(dest@name, dest)
    out@writes_to_dest <- TRUE
    return(out)
  }

  output_var <- hoist$declare_tmp_at_point(
    mode = "double",
    dims = list(m, n)
  )
  blas_call <- glue(
    "call dgemm('{opA}','{opB}', {blas_int(m)}, {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {B_name}, {blas_int(ldb)}, 0.0_c_double, {output_var@name}, {blas_int(ldc_expr)})"
  )
  emit_blas_contraction(blas_call, output_var@name, k, hoist)
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
  assert_hoist_env(hoist)
  A <- cast_linalg_double(A, context, hoist)
  x <- cast_linalg_double(x, context, hoist)
  output_dim <- if (transA == "N") m else n
  assert_nonempty_blas_output(
    output_dim,
    A,
    if (transA == "N") 1L else 2L,
    context,
    hoist,
    scope
  )
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
    allocate_reusable_local_output_at_point(dest, scope, hoist)
    # Assign output to output destination
    blas_call <- glue(
      "call dgemv('{transA}', {blas_int(m)}, {blas_int(n)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {x_name}, 1_c_int, 0.0_c_double, {dest@name}, 1_c_int)"
    )
    contracted_dim <- if (transA == "N") n else m
    emit_blas_contraction(blas_call, dest@name, contracted_dim, hoist)
    out <- Fortran(dest@name, dest)
    out@writes_to_dest <- TRUE
    return(out)
  }
  # Else assign to a temporary variable
  output_var <- hoist$declare_tmp_at_point(mode = "double", dims = out_dims)
  blas_call <- glue(
    "call dgemv('{transA}', {blas_int(m)}, {blas_int(n)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {x_name}, 1_c_int, 0.0_c_double, {output_var@name}, 1_c_int)"
  )
  contracted_dim <- if (transA == "N") n else m
  emit_blas_contraction(blas_call, output_var@name, contracted_dim, hoist)
  Fortran(output_var@name, output_var)
}

symmetrize_upper_to_lower <- function(target, n, hoist) {
  stopifnot(is_string(target))
  assert_hoist_env(hoist)

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

diag_length_expr <- function(nrow, ncol, context) {
  if (is_scalar_na(nrow) || is_scalar_na(ncol)) {
    stop(context, " requires known dimensions", call. = FALSE)
  }
  if (is_wholenumber(nrow) && is_wholenumber(ncol)) {
    return(as.integer(min(nrow, ncol)))
  }
  if (identical(nrow, ncol)) {
    return(nrow)
  }
  call("min", nrow, ncol)
}

zero_lower_triangle <- function(target, n, hoist) {
  stopifnot(is_string(target))
  assert_hoist_env(hoist)

  idx_i <- hoist$declare_tmp(mode = "integer", dims = NULL)
  idx_j <- hoist$declare_tmp(mode = "integer", dims = NULL)
  n_int <- blas_int(n)
  hoist$emit(glue(
    "
do {idx_i@name} = 2_c_int, {n_int}
  do {idx_j@name} = 1_c_int, {idx_i@name} - 1_c_int
    {target}({idx_i@name}, {idx_j@name}) = 0.0_c_double
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
  assert_hoist_env(hoist)
  X <- cast_linalg_double(X, context, hoist)
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
  X_name <- ensure_blas_operand_name(X, hoist)
  X <- Fortran(X_name, X@value)
  assert_nonempty_blas_output(
    n,
    X,
    if (trans == "T") 2L else 1L,
    context,
    hoist,
    scope
  )

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
    allocate_reusable_local_output_at_point(dest, scope, hoist)
    out_var <- dest
    out_name <- dest@name
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = list(n, n))
    out_name <- out_var@name
  }

  blas_call <- glue(
    "call dsyrk('U', '{trans}', {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {X_name}, {blas_int(lda)}, 0.0_c_double, {out_name}, {blas_int(n)})"
  )
  emit_blas_contraction(blas_call, out_name, k, hoist)
  symmetrize_upper_to_lower(out_name, n, hoist = hoist)

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    out@writes_to_dest <- TRUE
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
  assert_hoist_env(hoist)

  x <- cast_linalg_double(x, context, hoist)
  y <- cast_linalg_double(y, context, hoist)

  if (x@value@rank > 1L || y@value@rank > 1L) {
    stop("outer() only supports vectors or scalars")
  }

  m <- dim_or_one(x, 1L)
  n <- dim_or_one(y, 1L)

  assert_nonempty_blas_output(m, x, 1L, context, hoist, scope)
  assert_nonempty_blas_output(n, y, 1L, context, hoist, scope)

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
    allocate_reusable_local_output_at_point(dest, scope, hoist)
    hoist$emit(glue("{dest@name} = 0.0_c_double"))
    hoist$emit(glue(
      "call dger({blas_int(m)}, {blas_int(n)}, 1.0_c_double, {x_name}, 1_c_int, {y_name}, 1_c_int, {dest@name}, {blas_int(m)})"
    ))
    out <- Fortran(dest@name, dest)
    out@writes_to_dest <- TRUE
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
  assert_hoist_env(hoist)

  A <- cast_linalg_double(A, context, hoist)
  B <- cast_linalg_double(B, context, hoist)

  assert_rank2_matrix(A, "triangular solve expects a matrix")

  # Runtime shape checks use SIZE(), which does not evaluate expressions.
  # Name operands in call order before any guard so error paths preserve R's
  # argument evaluation and each operand is evaluated once.
  A <- hoist_unless_name(A, hoist)
  B <- hoist_unless_name(B, hoist)

  a_dims <- matrix_dims(A)
  assert_square_matrix(a_dims, A, "triangular solve", hoist, scope)
  n <- a_dims$rows

  b_rank <- B@value@rank
  assert_rhs_rank(
    b_rank,
    err_scalar = "triangular solve expects a vector or matrix right-hand side",
    err_high = "triangular solve only supports vector or matrix right-hand sides"
  )
  guard_conformable_dims(
    n,
    dim_or_one(B, 1L),
    "non-conformable arguments in triangular solve",
    hoist,
    scope,
    left = A,
    right = B,
    left_axis = 1L,
    right_axis = if (b_rank == 1L) NULL else 1L,
    checker = check_blas_dims
  )
  assert_nonempty_blas_output(n, A, 1L, context, hoist, scope)
  if (b_rank == 2L) {
    assert_nonempty_blas_output(
      dim_or_one(B, 2L),
      B,
      2L,
      context,
      hoist,
      scope
    )
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
    allocate_reusable_local_output_at_point(dest, scope, hoist)
    hoist$emit(glue("{dest@name} = {B}"))
    B_name <- dest@name
    out_var <- dest
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp_at_point(
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
    out@writes_to_dest <- TRUE
  }
  out
}

lapack_solve <- function(
  A,
  B,
  scope,
  hoist,
  dest = NULL,
  context = "solve",
  tol = NULL
) {
  assert_hoist_env(hoist)

  A <- cast_linalg_double(A, context, hoist)
  B <- cast_linalg_double(B, context, hoist)

  assert_rank2_matrix(A, paste0(context, " expects a matrix for `a`"))

  A <- hoist_unless_name(A, hoist)
  B <- hoist_unless_name(B, hoist)

  a_dims <- matrix_dims(A)
  m <- a_dims$rows
  n <- a_dims$cols

  b_rank <- B@value@rank
  assert_rhs_rank(
    b_rank,
    err_scalar = paste0(context, " expects a vector or matrix right-hand side"),
    err_high = paste0(
      context,
      " only supports vector or matrix right-hand sides"
    ),
    call_scalar = FALSE,
    call_high = FALSE
  )

  # R's solve() rejects a rectangular coefficient matrix before checking
  # whether the right-hand side is conformable. Least squares remains
  # qr.solve()'s path.
  if (!identical(context, "qr.solve")) {
    assert_square_matrix(a_dims, A, context, hoist, scope)
  }

  guard_conformable_dims(
    m,
    dim_or_one(B, 1L),
    paste0("non-conformable arguments in ", context),
    hoist,
    scope,
    left = A,
    right = B,
    left_axis = 1L,
    right_axis = if (b_rank == 1L) NULL else 1L,
    checker = check_blas_dims
  )
  assert_nonempty_blas_output(n, A, 2L, context, hoist, scope)
  nrhs <- if (b_rank == 1L) 1L else dim_or_one(B, 2L)
  if (b_rank == 2L) {
    assert_nonempty_blas_output(nrhs, B, 2L, context, hoist, scope)
  }

  A_name <- ensure_blas_operand_name(A, hoist)
  B_input_name <- ensure_blas_operand_name(B, hoist)

  # Both lowerings write a solution shaped by R's contract: length follows
  # ncol(a), width follows the right-hand side. Each branch resolves the
  # output target at its own write point (declaration order matters for
  # the emitted block) with the one shared spelling below.
  expected_dims <- if (b_rank == 1L) list(n) else list(n, nrhs)
  dest_usable <- function() {
    can_use_output(
      dest,
      input_names = c(A_name, B_input_name),
      expected_dims = expected_dims,
      context = context,
      allow_alias = B_input_name
    )
  }

  if (!identical(context, "qr.solve")) {
    A_work <- hoist$declare_tmp_at_point(mode = "double", dims = list(m, n))
    hoist$emit(glue("{A_work@name} = {A_name}"))

    use_dest <- dest_usable()
    out_var <- if (use_dest) {
      allocate_reusable_local_output_at_point(dest, scope, hoist)
      dest
    } else {
      hoist$declare_tmp_at_point(mode = "double", dims = expected_dims)
    }
    out_name <- out_var@name
    # The output length follows ncol(a) (R's contract) while `b` follows
    # nrow(a); the two are only runtime-equal. When ncol is statically 1
    # the output declares as a scalar, so a symbolic-length `b` must be
    # copied elementwise, not by whole-array assignment.
    b_src <- if (passes_as_scalar(out_var) && !passes_as_scalar(B@value)) {
      subs <- str_flatten_commas(rep("1", b_rank))
      glue("{B_input_name}({subs})")
    } else {
      B_input_name
    }
    hoist$emit(glue("{out_name} = {b_src}"))

    ipiv <- hoist$declare_tmp_at_point(mode = "integer", dims = list(m))
    info <- hoist$declare_tmp(mode = "integer", dims = NULL)

    hoist$emit(glue(
      "call dgesv({blas_int(m)}, {blas_int(nrhs)}, {A_work@name}, {blas_int(m)}, {ipiv@name}, {out_name}, {blas_int(m)}, {info@name})"
    ))
    emit_quickr_error_if(
      condition = glue("{info@name} > 0_c_int"),
      message = "Lapack routine dgesv: system is exactly singular",
      hoist = hoist,
      scope = scope
    )
    emit_quickr_error_if(
      condition = glue("{info@name} < 0_c_int"),
      message = "Lapack routine dgesv: illegal argument",
      hoist = hoist,
      scope = scope
    )
  } else {
    A_work <- hoist$declare_tmp_at_point(mode = "double", dims = list(m, n))
    hoist$emit(glue("{A_work@name} = {A_name}"))

    B_work <- hoist$declare_tmp_at_point(
      mode = "double",
      dims = list(m, nrhs)
    )
    m_f <- dims2f(list(m), scope)
    if (!nzchar(m_f)) {
      m_f <- "1"
    }
    nrhs_f <- dims2f(list(nrhs), scope)
    if (!nzchar(nrhs_f)) {
      nrhs_f <- "1"
    }
    hoist$emit(glue("{B_work@name} = 0.0_c_double"))
    if (b_rank == 1L) {
      hoist$emit(glue("{B_work@name}(1:{m_f}, 1) = {B_input_name}"))
    } else {
      hoist$emit(glue("{B_work@name}(1:{m_f}, 1:{nrhs_f}) = {B_input_name}"))
    }

    qraux <- hoist$declare_tmp_at_point(mode = "double", dims = list(n))
    jpvt <- hoist$declare_tmp_at_point(mode = "integer", dims = list(n))
    work <- hoist$declare_tmp_at_point(mode = "double", dims = list(n, 2L))
    rank <- hoist$declare_tmp(mode = "integer", dims = NULL)
    idx <- hoist$declare_tmp(mode = "integer", dims = NULL)

    hoist$emit(glue(
      "
do {idx@name} = 1_c_int, {blas_int(n)}
  {jpvt@name}({idx@name}) = {idx@name}
end do"
    ))

    tol_value <- if (is.null(tol)) "1e-7_c_double" else as.character(tol)
    mn <- call("min", m, n)
    hoist$emit(glue(
      "call dqrdc2({A_work@name}, {blas_int(m)}, {blas_int(m)}, {blas_int(n)}, {tol_value}, {rank@name}, {qraux@name}, {jpvt@name}, {work@name})"
    ))

    emit_quickr_error_if(
      condition = glue("{rank@name} < {blas_int(mn)}"),
      message = "rank deficient matrix in qr.solve",
      hoist = hoist,
      scope = scope
    )

    coef_work <- hoist$declare_tmp_at_point(
      mode = "double",
      dims = list(mn, nrhs)
    )
    hoist$emit(glue("{coef_work@name} = 0.0_c_double"))
    info <- hoist$declare_tmp(mode = "integer", dims = NULL)

    hoist$emit(glue(
      "call dqrcf({A_work@name}, {blas_int(m)}, {rank@name}, {qraux@name}, {B_work@name}, {blas_int(nrhs)}, {coef_work@name}, {info@name})"
    ))
    emit_quickr_error_if(
      condition = glue("{info@name} /= 0_c_int"),
      message = "exact singularity in 'qr.coef'",
      hoist = hoist,
      scope = scope
    )

    use_dest <- dest_usable()
    out_var <- if (use_dest) {
      allocate_reusable_local_output_at_point(dest, scope, hoist)
      dest
    } else {
      hoist$declare_tmp_at_point(mode = "double", dims = expected_dims)
    }
    out_name <- out_var@name

    if (passes_as_scalar(out_var)) {
      hoist$emit(glue("{out_name} = {coef_work@name}(1, 1)"))
    } else {
      hoist$emit(glue("{out_name} = 0.0_c_double"))
      if (b_rank == 1L) {
        idx <- hoist$declare_tmp(mode = "integer", dims = NULL)
        hoist$emit(glue(
          "
do {idx@name} = 1_c_int, {rank@name}
  {out_name}({jpvt@name}({idx@name})) = {coef_work@name}({idx@name}, 1)
end do"
        ))
      } else {
        idx_i <- hoist$declare_tmp(mode = "integer", dims = NULL)
        idx_j <- hoist$declare_tmp(mode = "integer", dims = NULL)
        hoist$emit(glue(
          "
do {idx_j@name} = 1_c_int, {blas_int(nrhs)}
  do {idx_i@name} = 1_c_int, {rank@name}
    {out_name}({jpvt@name}({idx_i@name}), {idx_j@name}) = {coef_work@name}({idx_i@name}, {idx_j@name})
  end do
end do"
        ))
      }
    }
  }

  out <- Fortran(out_name, out_var)
  if (use_dest) {
    out@writes_to_dest <- TRUE
  }
  out
}

lapack_inverse <- function(A, scope, hoist, dest = NULL, context = "solve") {
  assert_hoist_env(hoist)

  A <- cast_linalg_double(A, context, hoist)
  assert_rank2_matrix(A, paste0(context, " expects a matrix for `a`"))
  A <- hoist_unless_name(A, hoist)

  a_dims <- matrix_dims(A)
  assert_square_matrix(a_dims, A, context, hoist, scope)
  n <- a_dims$rows

  A_name <- ensure_blas_operand_name(A, hoist)

  writes_to_dest <- FALSE
  if (
    can_use_output(
      dest,
      input_names = A_name,
      expected_dims = list(n, n),
      context = context,
      allow_alias = A_name
    )
  ) {
    out_var <- dest
    out_name <- dest@name
    allocate_reusable_local_output_at_point(out_var, scope, hoist)
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp_at_point(
      mode = "double",
      dims = list(n, n)
    )
    out_name <- out_var@name
  }

  hoist$emit(glue("{out_name} = {A_name}"))

  ipiv <- hoist$declare_tmp_at_point(mode = "integer", dims = list(n))
  info <- hoist$declare_tmp(mode = "integer", dims = NULL)
  work <- hoist$declare_tmp_at_point(mode = "double", dims = list(n))

  hoist$emit(glue(
    "call dgetrf({blas_int(n)}, {blas_int(n)}, {out_name}, {blas_int(n)}, {ipiv@name}, {info@name})"
  ))
  emit_quickr_error_if(
    condition = glue("{info@name} > 0_c_int"),
    message = "Lapack routine dgetrf: system is exactly singular",
    hoist = hoist,
    scope = scope
  )
  emit_quickr_error_if(
    condition = glue("{info@name} < 0_c_int"),
    message = "Lapack routine dgetrf: illegal argument",
    hoist = hoist,
    scope = scope
  )
  hoist$emit(glue(
    "call dgetri({blas_int(n)}, {out_name}, {blas_int(n)}, {ipiv@name}, {work@name}, {blas_int(n)}, {info@name})"
  ))
  emit_quickr_error_if(
    condition = glue("{info@name} > 0_c_int"),
    message = "Lapack routine dgetri: system is exactly singular",
    hoist = hoist,
    scope = scope
  )
  emit_quickr_error_if(
    condition = glue("{info@name} < 0_c_int"),
    message = "Lapack routine dgetri: illegal argument",
    hoist = hoist,
    scope = scope
  )

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    out@writes_to_dest <- TRUE
  }
  out
}

lapack_chol <- function(A, scope, hoist, dest = NULL, context = "chol") {
  assert_hoist_env(hoist)

  A <- cast_linalg_double(A, context, hoist)
  assert_rank2_matrix(A, paste0(context, " expects a matrix"))
  A <- hoist_unless_name(A, hoist)

  a_dims <- matrix_dims(A)
  assert_square_matrix(a_dims, A, context, hoist, scope)
  n <- a_dims$rows

  A_name <- ensure_blas_operand_name(A, hoist)

  writes_to_dest <- FALSE
  if (
    can_use_output(
      dest,
      input_names = A_name,
      expected_dims = list(n, n),
      context = context,
      allow_alias = A_name
    )
  ) {
    out_var <- dest
    out_name <- dest@name
    allocate_reusable_local_output_at_point(out_var, scope, hoist)
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp_at_point(
      mode = "double",
      dims = list(n, n)
    )
    out_name <- out_var@name
  }

  hoist$emit(glue("{out_name} = {A_name}"))

  info <- hoist$declare_tmp(mode = "integer", dims = NULL)
  hoist$emit(glue(
    "call dpotrf('U', {blas_int(n)}, {out_name}, {blas_int(n)}, {info@name})"
  ))
  emit_quickr_error_if(
    condition = glue("{info@name} > 0_c_int"),
    message = "Lapack routine dpotrf: leading minor is not positive definite",
    hoist = hoist,
    scope = scope
  )
  emit_quickr_error_if(
    condition = glue("{info@name} < 0_c_int"),
    message = "Lapack routine dpotrf: illegal argument",
    hoist = hoist,
    scope = scope
  )
  zero_lower_triangle(out_name, n, hoist = hoist)

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    out@writes_to_dest <- TRUE
  }
  out
}

lapack_chol2inv <- function(
  R,
  scope,
  hoist,
  dest = NULL,
  context = "chol2inv"
) {
  assert_hoist_env(hoist)

  R <- cast_linalg_double(R, context, hoist)
  assert_rank2_matrix(R, paste0(context, " expects a matrix"))
  R <- hoist_unless_name(R, hoist)

  r_dims <- matrix_dims(R)
  assert_square_matrix(r_dims, R, context, hoist, scope)
  n <- r_dims$rows

  R_name <- ensure_blas_operand_name(R, hoist)

  writes_to_dest <- FALSE
  if (
    can_use_output(
      dest,
      input_names = R_name,
      expected_dims = list(n, n),
      context = context,
      allow_alias = R_name
    )
  ) {
    out_var <- dest
    out_name <- dest@name
    allocate_reusable_local_output_at_point(out_var, scope, hoist)
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp_at_point(
      mode = "double",
      dims = list(n, n)
    )
    out_name <- out_var@name
  }

  hoist$emit(glue("{out_name} = {R_name}"))

  info <- hoist$declare_tmp(mode = "integer", dims = NULL)
  hoist$emit(glue(
    "call dpotri('U', {blas_int(n)}, {out_name}, {blas_int(n)}, {info@name})"
  ))
  emit_quickr_error_if(
    condition = glue("{info@name} > 0_c_int"),
    message = "Lapack routine dpotri: matrix is not positive definite",
    hoist = hoist,
    scope = scope
  )
  emit_quickr_error_if(
    condition = glue("{info@name} < 0_c_int"),
    message = "Lapack routine dpotri: illegal argument",
    hoist = hoist,
    scope = scope
  )
  symmetrize_upper_to_lower(out_name, n, hoist = hoist)

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    out@writes_to_dest <- TRUE
  }
  out
}

diag_extract <- function(x, scope, hoist, dest = NULL, context = "diag") {
  assert_hoist_env(hoist)

  # R's diag(<matrix>) preserves the input mode; the copy loop is
  # mode-agnostic.
  assert_rank2_matrix(x, paste0(context, " expects a matrix input"))

  x_dims <- matrix_dims(x)
  diag_len <- diag_length_expr(x_dims$rows, x_dims$cols, context)

  x_name <- ensure_blas_operand_name(x, hoist)
  logical_is_c_int <- logical_as_int(x@value)

  writes_to_dest <- FALSE
  if (
    can_use_output(
      dest,
      input_names = x_name,
      expected_dims = list(diag_len),
      context = context,
      mode = x@value@mode,
      logical_is_c_int = logical_is_c_int
    )
  ) {
    allocate_reusable_local_output_at_point(dest, scope, hoist)
    out_var <- dest
    out_name <- dest@name
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(
      mode = x@value@mode,
      dims = list(diag_len),
      logical_as_int = logical_is_c_int
    )
    out_name <- out_var@name
  }

  idx_i <- hoist$declare_tmp(mode = "integer", dims = NULL)
  hoist$emit(glue(
    "
do {idx_i@name} = 1_c_int, {blas_int(diag_len)}
  {out_name}({idx_i@name}) = {x_name}({idx_i@name}, {idx_i@name})
end do"
  ))

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    out@writes_to_dest <- TRUE
  }
  out
}

diag_matrix <- function(
  x,
  nrow,
  ncol,
  scope,
  hoist,
  dest = NULL,
  context = "diag"
) {
  assert_hoist_env(hoist)

  # R's diag(x, ...) preserves typeof(x). The identity-matrix callers pass
  # a synthesized 1.0_c_double, which keeps diag(n) double, as in R.
  assert_rank_leq1(x, paste0(context, " expects a vector or scalar input"))

  mode <- x@value@mode
  logical_is_c_int <- logical_as_int(x@value)

  diag_len <- diag_length_expr(nrow, ncol, context)
  x_scalar <- passes_as_scalar(x@value)
  x_len <- if (x_scalar) 1L else dim_or_one(x, 1L)

  x_name <- ensure_blas_operand_name(x, hoist)

  writes_to_dest <- FALSE
  if (
    can_use_output(
      dest,
      input_names = x_name,
      expected_dims = list(nrow, ncol),
      context = context,
      mode = mode,
      logical_is_c_int = logical_is_c_int
    )
  ) {
    allocate_reusable_local_output_at_point(dest, scope, hoist)
    out_var <- dest
    out_name <- dest@name
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(
      mode = mode,
      dims = list(nrow, ncol),
      logical_as_int = logical_is_c_int
    )
    out_name <- out_var@name
  }

  zero <- switch(
    mode,
    double = "0.0_c_double",
    integer = "0_c_int",
    logical = if (logical_as_int(out_var)) "0_c_int" else ".false.",
    complex = "(0.0_c_double, 0.0_c_double)",
    stop(context, " does not support mode ", mode, call. = FALSE)
  )
  hoist$emit(glue("{out_name} = {zero}"))

  idx_i <- hoist$declare_tmp(mode = "integer", dims = NULL)
  value_expr <- if (x_scalar) {
    x_name
  } else {
    idx_expr <- glue(
      "1_c_int + mod({idx_i@name} - 1_c_int, {blas_int(x_len)})"
    )
    glue("{x_name}({idx_expr})")
  }

  hoist$emit(glue(
    "
do {idx_i@name} = 1_c_int, {blas_int(diag_len)}
  {out_name}({idx_i@name}, {idx_i@name}) = {value_expr}
end do"
  ))

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    out@writes_to_dest <- TRUE
  }
  out
}

svd_dims <- function(A, context = "svd") {
  stopifnot(inherits(A, Fortran))
  assert_rank2_matrix(A, paste0(context, " expects a matrix"))
  a_dims <- matrix_dims(A)
  m <- a_dims$rows
  n <- a_dims$cols
  mn <- if (is_wholenumber(m) && is_wholenumber(n)) {
    as.integer(min(m, n))
  } else {
    call("min", m, n)
  }
  list(m = m, n = n, mn = mn)
}

lapack_svd <- function(
  A,
  d,
  u,
  v,
  scope,
  hoist,
  context = "svd"
) {
  assert_hoist_env(hoist)
  stopifnot(inherits(d, Variable), inherits(u, Variable), inherits(v, Variable))

  A <- cast_linalg_double(A, context, hoist)
  dims <- svd_dims(A, context = context)
  m <- dims$m
  n <- dims$n
  mn <- dims$mn

  A_name <- ensure_blas_operand_name(A, hoist)
  A_work <- hoist$declare_tmp(mode = "double", dims = list(m, n))
  hoist$emit(glue("{A_work@name} = {A_name}"))

  vt <- hoist$declare_tmp(mode = "double", dims = list(mn, n))

  info <- hoist$declare_tmp(mode = "integer", dims = NULL)
  lwork <- hoist$declare_tmp(mode = "integer", dims = NULL)
  work_query <- hoist$declare_tmp(
    mode = "double",
    dims = list(call("+", 1L, 0L))
  )
  iwork <- hoist$declare_tmp(
    mode = "integer",
    dims = list(call("*", 8L, mn))
  )

  hoist$emit(glue("{lwork@name} = -1_c_int"))
  hoist$emit(glue(
    "call dgesdd('S', {blas_int(m)}, {blas_int(n)}, {A_work@name}, {blas_int(m)}, {d@name}, {u@name}, {blas_int(m)}, {vt@name}, {blas_int(mn)}, {work_query@name}, {lwork@name}, {iwork@name}, {info@name})"
  ))
  hoist$emit(glue(
    "{lwork@name} = int({work_query@name}(1), kind=c_int)"
  ))
  work <- hoist$declare_tmp(mode = "double", dims = list(NA))
  hoist$emit(glue("allocate({work@name}({lwork@name}))"))

  hoist$emit(glue(
    "call dgesdd('S', {blas_int(m)}, {blas_int(n)}, {A_work@name}, {blas_int(m)}, {d@name}, {u@name}, {blas_int(m)}, {vt@name}, {blas_int(mn)}, {work@name}, {lwork@name}, {iwork@name}, {info@name})"
  ))
  emit_quickr_error_if(
    glue("{info@name} < 0_c_int"),
    "Lapack routine dgesdd: illegal argument",
    hoist,
    scope
  )
  emit_quickr_error_if(
    glue("{info@name} > 0_c_int"),
    "Lapack routine dgesdd failed to converge",
    hoist,
    scope
  )
  hoist$emit(glue("{v@name} = transpose({vt@name})"))

  invisible(list(d = d, u = u, v = v))
}
