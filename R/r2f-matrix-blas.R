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

# Assert conformability and warn on unknown.
assert_conformable_dims <- function(left, right, context, err_msg) {
  stopifnot(is_string(context), is_string(err_msg))
  conform <- check_conformable(left, right)
  if (!conform$ok) {
    stop(err_msg, call. = FALSE)
  }
  if (conform$unknown) {
    warn_conformability_unknown(left, right, context)
  }
  invisible(TRUE)
}

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

# Assert that dimensions represent a square matrix (rows == cols).
# Throws an error if dimensions are known to be non-conformable,
# and warns if conformability cannot be verified at compile time.
assert_square_matrix <- function(rows, cols, context) {
  conform <- check_conformable(rows, cols)
  if (!conform$ok) {
    stop(context, " requires a square matrix", call. = FALSE)
  }
  if (conform$unknown) {
    warn_conformability_unknown(rows, cols, context)
  }
  invisible(TRUE)
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
  x_str <- if (is.language(x)) {
    gsub("([0-9]+)L\\b", "\\1", deparse1(x))
  } else if (is_wholenumber(x)) {
    as.character(as.integer(x))
  } else {
    as.character(x)
  }
  glue("int({x_str}, kind=c_int)")
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
  assert_hoist_env(hoist)
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
  assert_hoist_env(hoist)

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
  assert_hoist_env(hoist)

  A <- maybe_cast_double(A)
  B <- maybe_cast_double(B)

  assert_rank2_matrix(A, "triangular solve expects a matrix")

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
  assert_rhs_rank(
    b_rank,
    err_scalar = "triangular solve expects a vector or matrix right-hand side",
    err_high = "triangular solve only supports vector or matrix right-hand sides"
  )
  if (b_rank == 1L) {
    b_len <- dim_or_one(B, 1L)
    assert_conformable_dims(
      n,
      b_len,
      context = "triangular solve",
      err_msg = "non-conformable arguments in triangular solve"
    )
  } else {
    b_rows <- dim_or_one(B, 1L)
    assert_conformable_dims(
      n,
      b_rows,
      context = "triangular solve",
      err_msg = "non-conformable arguments in triangular solve"
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

  A <- maybe_cast_double(A)
  B <- maybe_cast_double(B)

  assert_rank2_matrix(A, paste0(context, " expects a matrix for `a`"))

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

  if (b_rank == 1L) {
    b_len <- dim_or_one(B, 1L)
    assert_conformable_dims(
      m,
      b_len,
      context = context,
      err_msg = paste0("non-conformable arguments in ", context)
    )
  } else {
    b_rows <- dim_or_one(B, 1L)
    assert_conformable_dims(
      m,
      b_rows,
      context = context,
      err_msg = paste0("non-conformable arguments in ", context)
    )
  }

  A_name <- ensure_blas_operand_name(A, hoist)
  B_input_name <- ensure_blas_operand_name(B, hoist)

  nrhs <- if (b_rank == 1L) 1L else dim_or_one(B, 2L)

  square <- check_conformable(m, n)
  if (square$ok && !square$unknown && !identical(context, "qr.solve")) {
    A_work <- hoist$declare_tmp(mode = "double", dims = list(m, m))
    hoist$emit(glue("{A_work@name} = {A_name}"))

    expected_dims <- if (b_rank == 1L) list(n) else list(n, nrhs)
    writes_to_dest <- FALSE
    if (
      can_use_output(
        dest,
        input_names = c(A_name, B_input_name),
        expected_dims = expected_dims,
        context = context,
        allow_alias = B_input_name
      )
    ) {
      out_var <- dest
      out_name <- dest@name
      writes_to_dest <- TRUE
    } else {
      out_var <- hoist$declare_tmp(mode = "double", dims = expected_dims)
      out_name <- out_var@name
    }
    hoist$emit(glue("{out_name} = {B_input_name}"))

    ipiv <- hoist$declare_tmp(mode = "integer", dims = list(m))
    info <- hoist$declare_tmp(mode = "integer", dims = NULL)

    hoist$emit(glue(
      "call dgesv({blas_int(m)}, {blas_int(nrhs)}, {A_work@name}, {blas_int(m)}, {ipiv@name}, {out_name}, {blas_int(m)}, {info@name})"
    ))

    out <- Fortran(out_name, out_var)
    if (writes_to_dest) {
      attr(out, "writes_to_dest") <- TRUE
    }
    return(out)
  }

  A_work <- hoist$declare_tmp(mode = "double", dims = list(m, n))
  hoist$emit(glue("{A_work@name} = {A_name}"))

  max_mn <- call("max", m, n)

  B_work <- hoist$declare_tmp(mode = "double", dims = list(max_mn, nrhs))
  m_f <- dims2f(list(m), scope)
  if (!nzchar(m_f)) {
    m_f <- "1"
  }
  n_f <- dims2f(list(n), scope)
  if (!nzchar(n_f)) {
    n_f <- "1"
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

  info <- hoist$declare_tmp(mode = "integer", dims = NULL)

  mn <- call("min", m, n)
  if (identical(context, "qr.solve")) {
    jpvt <- hoist$declare_tmp(mode = "integer", dims = list(n))
    hoist$emit(glue("{jpvt@name} = 0_c_int"))

    rcond <- if (is.null(tol)) "1e-7_c_double" else as.character(tol)
    rank <- hoist$declare_tmp(mode = "integer", dims = NULL)

    lwork <- call(
      "max",
      1L,
      call("+", mn, call("max", mn, nrhs)),
      call("+", call("*", 2L, mn), call("*", 64L, call("+", n, 1L))),
      call("+", mn, call("*", 2L, n))
    )
    work <- hoist$declare_tmp(mode = "double", dims = list(lwork))

    hoist$emit(glue(
      "call dgelsy({blas_int(m)}, {blas_int(n)}, {blas_int(nrhs)}, {A_work@name}, {blas_int(m)}, {B_work@name}, {blas_int(max_mn)}, {jpvt@name}, {rcond}, {rank@name}, {work@name}, {blas_int(lwork)}, {info@name})"
    ))
  } else {
    lwork <- call("max", 1L, call("+", mn, call("max", mn, nrhs)))
    work <- hoist$declare_tmp(mode = "double", dims = list(lwork))

    hoist$emit(glue(
      "call dgels('N', {blas_int(m)}, {blas_int(n)}, {blas_int(nrhs)}, {A_work@name}, {blas_int(m)}, {B_work@name}, {blas_int(max_mn)}, {work@name}, {blas_int(lwork)}, {info@name})"
    ))
  }

  expected_dims <- if (b_rank == 1L) list(n) else list(n, nrhs)
  writes_to_dest <- FALSE
  if (
    can_use_output(
      dest,
      input_names = c(A_name, B_input_name),
      expected_dims = expected_dims,
      context = context,
      allow_alias = B_input_name
    )
  ) {
    out_var <- dest
    out_name <- dest@name
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = expected_dims)
    out_name <- out_var@name
  }

  if (b_rank == 1L) {
    if (passes_as_scalar(out_var)) {
      hoist$emit(glue("{out_name} = {B_work@name}(1, 1)"))
    } else {
      hoist$emit(glue("{out_name} = {B_work@name}(1:{n_f}, 1)"))
    }
  } else {
    hoist$emit(glue("{out_name} = {B_work@name}(1:{n_f}, 1:{nrhs_f})"))
  }

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    attr(out, "writes_to_dest") <- TRUE
  }
  out
}

lapack_inverse <- function(A, scope, hoist, dest = NULL, context = "solve") {
  assert_hoist_env(hoist)

  A <- maybe_cast_double(A)
  assert_rank2_matrix(A, paste0(context, " expects a matrix for `a`"))

  a_dims <- matrix_dims(A)
  assert_square_matrix(a_dims$rows, a_dims$cols, context)
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
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = list(n, n))
    out_name <- out_var@name
  }

  hoist$emit(glue("{out_name} = {A_name}"))

  ipiv <- hoist$declare_tmp(mode = "integer", dims = list(n))
  info <- hoist$declare_tmp(mode = "integer", dims = NULL)
  work <- hoist$declare_tmp(mode = "double", dims = list(n))

  hoist$emit(glue(
    "call dgetrf({blas_int(n)}, {blas_int(n)}, {out_name}, {blas_int(n)}, {ipiv@name}, {info@name})"
  ))
  hoist$emit(glue(
    "call dgetri({blas_int(n)}, {out_name}, {blas_int(n)}, {ipiv@name}, {work@name}, {blas_int(n)}, {info@name})"
  ))

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    attr(out, "writes_to_dest") <- TRUE
  }
  out
}

lapack_chol <- function(A, scope, hoist, dest = NULL, context = "chol") {
  assert_hoist_env(hoist)

  A <- maybe_cast_double(A)
  assert_rank2_matrix(A, paste0(context, " expects a matrix"))

  a_dims <- matrix_dims(A)
  assert_square_matrix(a_dims$rows, a_dims$cols, context)
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
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = list(n, n))
    out_name <- out_var@name
  }

  hoist$emit(glue("{out_name} = {A_name}"))

  info <- hoist$declare_tmp(mode = "integer", dims = NULL)
  hoist$emit(glue(
    "call dpotrf('U', {blas_int(n)}, {out_name}, {blas_int(n)}, {info@name})"
  ))
  zero_lower_triangle(out_name, n, hoist = hoist)

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    attr(out, "writes_to_dest") <- TRUE
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

  R <- maybe_cast_double(R)
  assert_rank2_matrix(R, paste0(context, " expects a matrix"))

  r_dims <- matrix_dims(R)
  assert_square_matrix(r_dims$rows, r_dims$cols, context)
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
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = list(n, n))
    out_name <- out_var@name
  }

  hoist$emit(glue("{out_name} = {R_name}"))

  info <- hoist$declare_tmp(mode = "integer", dims = NULL)
  hoist$emit(glue(
    "call dpotri('U', {blas_int(n)}, {out_name}, {blas_int(n)}, {info@name})"
  ))
  symmetrize_upper_to_lower(out_name, n, hoist = hoist)

  out <- Fortran(out_name, out_var)
  if (writes_to_dest) {
    attr(out, "writes_to_dest") <- TRUE
  }
  out
}

diag_extract <- function(x, scope, hoist, dest = NULL, context = "diag") {
  assert_hoist_env(hoist)

  x <- maybe_cast_double(x)
  assert_rank2_matrix(x, paste0(context, " expects a matrix input"))

  x_dims <- matrix_dims(x)
  diag_len <- diag_length_expr(x_dims$rows, x_dims$cols, context)

  x_name <- ensure_blas_operand_name(x, hoist)

  writes_to_dest <- FALSE
  if (
    can_use_output(
      dest,
      input_names = x_name,
      expected_dims = list(diag_len),
      context = context
    )
  ) {
    out_var <- dest
    out_name <- dest@name
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = list(diag_len))
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
    attr(out, "writes_to_dest") <- TRUE
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

  x <- maybe_cast_double(x)
  assert_rank_leq1(x, paste0(context, " expects a vector or scalar input"))

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
      context = context
    )
  ) {
    out_var <- dest
    out_name <- dest@name
    writes_to_dest <- TRUE
  } else {
    out_var <- hoist$declare_tmp(mode = "double", dims = list(nrow, ncol))
    out_name <- out_var@name
  }

  hoist$emit(glue("{out_name} = 0.0_c_double"))

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
    attr(out, "writes_to_dest") <- TRUE
  }
  out
}
