# Matrix-specific r2f handlers and helpers

# Return the R symbol name if operand is a bare symbol; otherwise NULL.
symbol_name_or_null <- function(x) {
  stopifnot(inherits(x, Fortran))
  r_expr <- x@r
  if (is.symbol(r_expr)) {
    return(as.character(r_expr))
  }
  if (length(x) == 1L && grepl("^[A-Za-z][A-Za-z0-9_]*$", x)) {
    return(as.character(x))
  }
  NULL
}

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

var_dim_or_one <- function(var, axis) {
  stopifnot(inherits(var, Variable))
  dim_or_one_from(var@dims, axis)
}

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

matrix_dims_var <- function(
  var,
  orientation = c("matrix", "rowvec", "colvec")
) {
  stopifnot(inherits(var, Variable))
  matrix_dims_from(var@rank, var@dims, orientation = orientation)
}

infer_symbol_var <- function(arg, scope) {
  if (!is.symbol(arg)) {
    return(NULL)
  }
  var <- get0(as.character(arg), scope, inherits = FALSE)
  if (inherits(var, Variable)) var else NULL
}

infer_matrix_arg <- function(arg, scope) {
  if (is_call(arg, quote(t)) && length(arg) == 2L) {
    inner <- infer_symbol_var(arg[[2L]], scope)
    if (is.null(inner)) {
      return(NULL)
    }
    if (inner@rank == 2L) {
      return(list(var = inner, trans = "T"))
    }
    if (inner@rank == 1L) {
      len <- inner@dims[[1L]]
      if (is.null(len)) {
        return(NULL)
      }
      val <- Variable("double", list(1L, len))
      return(list(var = val, trans = "N"))
    }
    if (inner@rank == 0L) {
      return(list(var = inner, trans = "N"))
    }
    return(NULL)
  }
  var <- infer_symbol_var(arg, scope)
  if (is.null(var)) {
    return(NULL)
  }
  list(var = var, trans = "N")
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
    return(invisible(TRUE))
  }
  if (identical(left, right)) {
    return(invisible(TRUE))
  }

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
  if (!identical(dest@mode, "double")) {
    return(FALSE)
  }
  output_name <- dest@name
  # check output name is not the same as left or right
  !identical(output_name, as.character(left)) &&
    !identical(output_name, as.character(right))
}

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

logical_arg_or_default <- function(args, name, default, context) {
  val <- args[[name]] %||% default
  if (is.null(val)) {
    return(default)
  }
  if (!is.logical(val) || length(val) != 1L || is.na(val)) {
    stop(context, " only supports literal ", name, " = TRUE/FALSE")
  }
  val
}

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
  dest = NULL
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  A_name <- ensure_blas_operand_name(left, hoist)
  B_name <- ensure_blas_operand_name(right, hoist)

  if (can_use_output(dest, left, right)) {
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
  dest = NULL
) {
  if (!inherits(hoist, "environment")) {
    stop("internal: hoist must be a hoist environment")
  }
  A_name <- ensure_blas_operand_name(A, hoist)
  x_name <- ensure_blas_operand_name(x, hoist)

  if (can_use_output(dest, A, x)) {
    # Assign output to output destination
    hoist$emit(glue(
      "call dgemv('{transA}', {blas_int(m)}, {blas_int(n)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {x_name}, 1, 0.0_c_double, {dest@name}, 1)"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }
  # Else assign to a temporary variable
  output_var <- hoist$declare_tmp(mode = "double", dims = out_dims)
  hoist$emit(glue(
    "call dgemv('{transA}', {blas_int(m)}, {blas_int(n)}, 1.0_c_double, {A_name}, {blas_int(lda)}, {x_name}, 1, 0.0_c_double, {output_var@name}, 1)"
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
  if (can_use_output(dest, X, X)) {
    hoist$emit(glue(
      "call dsyrk('U', '{trans}', {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {X_name}, {blas_int(lda)}, 0.0_c_double, {dest@name}, {blas_int(n)})"
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
    "call dsyrk('U', '{trans}', {blas_int(n)}, {blas_int(k)}, 1.0_c_double, {X_name}, {blas_int(lda)}, 0.0_c_double, {output_var@name}, {blas_int(n)})"
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

outer_mul <- function(x, y, scope, hoist, dest = NULL) {
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

  if (can_use_output(dest, x, y)) {
    hoist$emit(glue("{dest@name} = 0.0_c_double"))
    hoist$emit(glue(
      "call dger({blas_int(m)}, {blas_int(n)}, 1.0_c_double, {x_name}, 1, {y_name}, 1, {dest@name}, {blas_int(m)})"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  output_var <- hoist$declare_tmp(mode = "double", dims = list(m, n))
  hoist$emit(glue("{output_var@name} = 0.0_c_double"))
  hoist$emit(glue(
    "call dger({blas_int(m)}, {blas_int(n)}, 1.0_c_double, {x_name}, 1, {y_name}, 1, {output_var@name}, {blas_int(m)})"
  ))
  Fortran(output_var@name, output_var)
}

triangular_solve <- function(
  A,
  B,
  uplo,
  trans,
  diag,
  scope,
  hoist,
  dest = NULL
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
  assert_conformable(a_dims$rows, a_dims$cols, "triangular solve")
  n <- a_dims$rows

  b_rank <- B@value@rank
  if (b_rank > 2L) {
    stop("triangular solve only supports vector or matrix right-hand sides")
  }
  if (b_rank == 0L) {
    stop("triangular solve expects a vector or matrix right-hand side")
  } else if (b_rank == 1L) {
    b_len <- dim_or_one(B, 1L)
    assert_conformable(n, b_len, "triangular solve")
  } else {
    b_rows <- dim_or_one(B, 1L)
    assert_conformable(n, b_rows, "triangular solve")
  }

  A_name <- ensure_blas_operand_name(A, hoist)

  if (can_use_output(dest, A, B)) {
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
      "call dtrsv('{uplo}', '{trans}', '{diag}', {blas_int(n)}, {A_name}, {blas_int(n)}, {B_name}, 1)"
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

crossprod_like <- function(
  x_arg,
  y_arg,
  scope,
  ...,
  hoist,
  dest,
  trans_single,
  opA,
  opB,
  context
) {
  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)

  if (is.null(y_arg)) {
    return(syrk(
      trans = trans_single,
      X = x,
      scope = scope,
      hoist = hoist,
      dest = dest
    ))
  }

  y <- maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  x_dims <- matrix_dims(x)
  y_dims <- matrix_dims(y)
  x_eff <- effective_dims(x_dims, opA)
  y_eff <- effective_dims(y_dims, opB)

  assert_conformable(x_eff$cols, y_eff$rows, context)

  m <- x_eff$rows
  n <- y_eff$cols
  k <- x_eff$cols

  lda <- x_dims$rows
  ldb <- y_dims$rows
  ldc_expr <- m

  gemm(
    opA = opA,
    opB = opB,
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
  crossprod_like(
    x_arg = x_arg,
    y_arg = y_arg,
    scope = scope,
    ...,
    hoist = hoist,
    dest = dest,
    trans_single = "T",
    opA = "T",
    opB = "N",
    context = "crossprod"
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
  crossprod_like(
    x_arg = x_arg,
    y_arg = y_arg,
    scope = scope,
    ...,
    hoist = hoist,
    dest = dest,
    trans_single = "N",
    opA = "N",
    opB = "T",
    context = "tcrossprod"
  )
}

r2f_handlers[["outer"]] <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  dest = NULL
) {
  x_arg <- args$X %||% args[[1L]]
  y_arg <- args$Y %||% args[[2L]]
  if (is.null(x_arg) || is.null(y_arg)) {
    stop("outer() expects X and Y")
  }

  fun <- args$FUN %||% "*"
  if (!identical(fun, "*")) {
    stop("outer() only supports FUN = \"*\"")
  }
  x <- r2f(x_arg, scope, ..., hoist = hoist)
  y <- r2f(y_arg, scope, ..., hoist = hoist)
  outer_mul(x, y, scope = scope, hoist = hoist, dest = dest)
}

r2f_handlers[["%o%"]] <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  dest = NULL
) {
  stopifnot(length(args) == 2L)
  x <- r2f(args[[1L]], scope, ..., hoist = hoist)
  y <- r2f(args[[2L]], scope, ..., hoist = hoist)
  outer_mul(x, y, scope = scope, hoist = hoist, dest = dest)
}

r2f_handlers[["forwardsolve"]] <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  dest = NULL
) {
  stopifnot(length(args) >= 2L)
  if (!is.null(args$k)) {
    stop("forwardsolve() does not support k yet")
  }
  upper_tri <- logical_arg_or_default(
    args,
    "upper.tri",
    FALSE,
    "forwardsolve()"
  )
  transpose <- logical_arg_or_default(
    args,
    "transpose",
    FALSE,
    "forwardsolve()"
  )
  diag_unit <- logical_arg_or_default(args, "diag", FALSE, "forwardsolve()")

  A <- r2f(args[[1L]], scope, ..., hoist = hoist)
  B <- r2f(args[[2L]], scope, ..., hoist = hoist)

  triangular_solve(
    A = A,
    B = B,
    uplo = if (upper_tri) "U" else "L",
    trans = if (transpose) "T" else "N",
    diag = if (diag_unit) "U" else "N",
    scope = scope,
    hoist = hoist,
    dest = dest
  )
}

r2f_handlers[["backsolve"]] <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  dest = NULL
) {
  stopifnot(length(args) >= 2L)
  if (!is.null(args$k)) {
    stop("backsolve() does not support k yet")
  }
  upper_tri <- logical_arg_or_default(args, "upper.tri", TRUE, "backsolve()")
  transpose <- logical_arg_or_default(args, "transpose", FALSE, "backsolve()")
  diag_unit <- logical_arg_or_default(args, "diag", FALSE, "backsolve()")

  A <- r2f(args[[1L]], scope, ..., hoist = hoist)
  B <- r2f(args[[2L]], scope, ..., hoist = hoist)

  triangular_solve(
    A = A,
    B = B,
    uplo = if (upper_tri) "U" else "L",
    trans = if (transpose) "T" else "N",
    diag = if (diag_unit) "U" else "N",
    scope = scope,
    hoist = hoist,
    dest = dest
  )
}

infer_dest_matmul <- function(args, scope) {
  if (length(args) != 2L) {
    return(NULL)
  }
  left_info <- infer_matrix_arg(args[[1L]], scope)
  right_info <- infer_matrix_arg(args[[2L]], scope)
  if (is.null(left_info) || is.null(right_info)) {
    return(NULL)
  }

  left <- left_info$var
  right <- right_info$var
  left_trans <- left_info$trans
  right_trans <- right_info$trans

  left_rank <- left@rank
  right_rank <- right@rank
  if (left_rank > 2L || right_rank > 2L) {
    return(NULL)
  }

  left_dims <- matrix_dims_var(
    left,
    orientation = if (left_rank == 1L) "rowvec" else "matrix"
  )
  right_dims <- matrix_dims_var(
    right,
    orientation = if (right_rank == 1L) "colvec" else "matrix"
  )

  left_eff <- if (left_rank == 2L) {
    effective_dims(left_dims, left_trans)
  } else {
    left_dims
  }
  right_eff <- if (right_rank == 2L) {
    effective_dims(right_dims, right_trans)
  } else {
    right_dims
  }

  if (left_rank == 2L && right_rank == 1L) {
    out_len <- if (left_trans == "N") left_dims$rows else left_dims$cols
    return(Variable("double", list(out_len, 1L)))
  }
  if (left_rank == 1L && right_rank == 2L) {
    transA <- if (right_trans == "N") "T" else "N"
    out_len <- if (transA == "N") right_dims$rows else right_dims$cols
    return(Variable("double", list(1L, out_len)))
  }

  Variable("double", list(left_eff$rows, right_eff$cols))
}

infer_dest_crossprod_like <- function(args, scope, trans) {
  x <- infer_symbol_var(args[[1L]], scope)
  if (is.null(x)) {
    return(NULL)
  }
  y <- if (length(args) > 1L) infer_symbol_var(args[[2L]], scope) else NULL
  x_dims <- matrix_dims_var(x)
  if (is.null(y)) {
    n <- if (identical(trans, "T")) x_dims$cols else x_dims$rows
    return(Variable("double", list(n, n)))
  }
  y_dims <- matrix_dims_var(y)
  if (identical(trans, "T")) {
    Variable("double", list(x_dims$cols, y_dims$cols))
  } else {
    Variable("double", list(x_dims$rows, y_dims$rows))
  }
}

infer_dest_crossprod <- function(args, scope) {
  infer_dest_crossprod_like(args, scope, trans = "T")
}

infer_dest_tcrossprod <- function(args, scope) {
  infer_dest_crossprod_like(args, scope, trans = "N")
}

infer_dest_outer <- function(args, scope) {
  x_arg <- args$X %||% args[[1L]]
  y_arg <- args$Y %||% args[[2L]]
  x <- infer_symbol_var(x_arg, scope)
  y <- infer_symbol_var(y_arg, scope)
  if (is.null(x) || is.null(y)) {
    return(NULL)
  }
  if (x@rank > 1L || y@rank > 1L) {
    return(NULL)
  }
  m <- var_dim_or_one(x, 1L)
  n <- var_dim_or_one(y, 1L)
  Variable("double", list(m, n))
}

infer_dest_triangular <- function(args, scope) {
  if (length(args) < 2L) {
    return(NULL)
  }
  A <- infer_symbol_var(args[[1L]], scope)
  B <- infer_symbol_var(args[[2L]], scope)
  if (is.null(A) || is.null(B)) {
    return(NULL)
  }
  if (A@rank != 2L || B@rank == 0L || B@rank > 2L) {
    return(NULL)
  }
  if (is.null(B@dims)) {
    return(NULL)
  }
  Variable("double", B@dims)
}

attr(r2f_handlers[["%*%"]], "dest_supported") <- TRUE
attr(r2f_handlers[["crossprod"]], "dest_supported") <- TRUE
attr(r2f_handlers[["tcrossprod"]], "dest_supported") <- TRUE
attr(r2f_handlers[["outer"]], "dest_supported") <- TRUE
attr(r2f_handlers[["%o%"]], "dest_supported") <- TRUE
attr(r2f_handlers[["forwardsolve"]], "dest_supported") <- TRUE
attr(r2f_handlers[["backsolve"]], "dest_supported") <- TRUE

attr(r2f_handlers[["%*%"]], "dest_infer") <- infer_dest_matmul
attr(r2f_handlers[["crossprod"]], "dest_infer") <- infer_dest_crossprod
attr(r2f_handlers[["tcrossprod"]], "dest_infer") <- infer_dest_tcrossprod
attr(r2f_handlers[["outer"]], "dest_infer") <- infer_dest_outer
attr(r2f_handlers[["%o%"]], "dest_infer") <- infer_dest_outer
attr(r2f_handlers[["forwardsolve"]], "dest_infer") <- infer_dest_triangular
attr(r2f_handlers[["backsolve"]], "dest_infer") <- infer_dest_triangular
