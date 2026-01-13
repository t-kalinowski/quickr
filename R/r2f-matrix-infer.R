# Matrix destination inference helpers

# Infer a variable from a symbol in the current scope.
infer_symbol_var <- function(arg, scope) {
  arg <- unwrap_parens(arg)
  if (!is.symbol(arg)) {
    return(NULL)
  }
  var <- get0(as.character(arg), scope, inherits = FALSE)
  if (inherits(var, Variable)) var else NULL
}

# Infer a matrix argument, handling t() and scalar/vector promotion.
infer_matrix_arg <- function(arg, scope) {
  arg <- unwrap_parens(arg)
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

# Infer destination dimensions for %*% based on inputs.
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

# Shared inference for crossprod/tcrossprod destination sizes.
infer_dest_crossprod_like <- function(args, scope, trans) {
  x_arg <- args$x %||% args[[1L]]
  x <- infer_symbol_var(x_arg, scope)
  if (is.null(x)) {
    return(NULL)
  }
  y_arg <- args$y %||% if (length(args) > 1L) args[[2L]] else NULL
  y <- if (!is.null(y_arg)) infer_symbol_var(y_arg, scope) else NULL
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

# Infer destination dimensions for crossprod().
infer_dest_crossprod <- function(args, scope) {
  infer_dest_crossprod_like(args, scope, trans = "T")
}

# Infer destination dimensions for tcrossprod().
infer_dest_tcrossprod <- function(args, scope) {
  infer_dest_crossprod_like(args, scope, trans = "N")
}

# Infer destination dimensions for outer() and %o%().
infer_dest_outer <- function(args, scope) {
  x_arg <- args$X %||% args[[1L]]
  y_arg <- args$Y %||% if (length(args) >= 2L) args[[2L]] else NULL
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

# Infer destination dimensions for forwardsolve() and backsolve().
# forwardsolve(l, x, ...), backsolve(r, x, ...)
infer_dest_triangular <- function(args, scope) {
  if (length(args) < 2L) {
    return(NULL)
  }
  # First arg is l (forwardsolve) or r (backsolve)
  a_arg <- args$l %||% args$r %||% args[[1L]]
  # Second arg is x
  b_arg <- args$x %||% args[[2L]]
  A <- infer_symbol_var(a_arg, scope)
  B <- infer_symbol_var(b_arg, scope)
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

# Infer destination dimensions for solve().
infer_dest_solve <- function(args, scope) {
  a_arg <- args$a %||% args[[1L]]
  if (is.null(a_arg)) {
    return(NULL)
  }
  A <- infer_symbol_var(a_arg, scope)
  if (is.null(A) || A@rank != 2L) {
    return(NULL)
  }
  a_dims <- matrix_dims_var(A)
  n <- a_dims$rows

  b_arg <- args$b %||% if (length(args) >= 2L) args[[2L]] else NULL
  if (is.null(b_arg)) {
    return(Variable("double", list(n, n)))
  }
  B <- infer_symbol_var(b_arg, scope)
  if (is.null(B)) {
    return(NULL)
  }
  if (B@rank == 1L) {
    return(Variable("double", list(n)))
  }
  if (B@rank == 2L) {
    return(Variable("double", list(n, B@dims[[2L]])))
  }
  NULL
}

# Infer destination dimensions for chol().
infer_dest_chol <- function(args, scope) {
  x_arg <- args$x %||% args[[1L]]
  if (is.null(x_arg)) {
    return(NULL)
  }
  X <- infer_symbol_var(x_arg, scope)
  if (is.null(X) || X@rank != 2L) {
    return(NULL)
  }
  x_dims <- matrix_dims_var(X)
  Variable("double", list(x_dims$rows, x_dims$cols))
}

# Infer destination dimensions for chol2inv().
infer_dest_chol2inv <- function(args, scope) {
  x_arg <- args$x %||% args[[1L]]
  if (is.null(x_arg)) {
    return(NULL)
  }
  X <- infer_symbol_var(x_arg, scope)
  if (is.null(X) || X@rank != 2L) {
    return(NULL)
  }
  x_dims <- matrix_dims_var(X)
  Variable("double", list(x_dims$rows, x_dims$cols))
}

# Helper to infer a size from a literal or symbol.
infer_size <- function(arg, scope) {
  if (is.null(arg) || is_missing(arg)) {
    return(NULL)
  }
  if (is.numeric(arg) && length(arg) == 1L && is_wholenumber(arg)) {
    return(as.integer(arg))
  }
  if (is.symbol(arg)) {
    var <- get0(as.character(arg), scope, inherits = FALSE)
    if (!inherits(var, Variable)) {
      return(NULL)
    }
    # Scalar (rank 0) or length-1 vector (rank 1 with dim 1)
    if (var@rank == 0L) {
      return(arg)
    }
    if (var@rank == 1L && length(var@dims) == 1L) {
      d <- var@dims[[1L]]
      if (is_wholenumber(d) && identical(as.integer(d), 1L)) {
        return(arg)
      }
    }
  }
  NULL
}

# Infer destination dimensions for diag().
infer_dest_diag <- function(args, scope) {
  # R signature: diag(x = 1, nrow, ncol, names = TRUE)
  # Handle both named and positional arguments
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }
  unnamed_idx <- which(!nzchar(arg_names) | is.na(arg_names))

  # Extract x (named or position 1)
  x_arg <- NULL
  if (!is.null(args$x) && !is_missing(args$x)) {
    x_arg <- args$x
  } else if (length(unnamed_idx) >= 1L) {
    candidate <- args[[unnamed_idx[[1L]]]]
    if (!is_missing(candidate)) {
      x_arg <- candidate
    }
  }

  # Extract nrow (named or position 2)
  nrow_arg <- args$nrow
  if (is.null(nrow_arg) && length(unnamed_idx) >= 2L) {
    nrow_arg <- args[[unnamed_idx[[2L]]]]
  }

  # Extract ncol (named or position 3)
  ncol_arg <- args$ncol
  if (is.null(ncol_arg) && length(unnamed_idx) >= 3L) {
    ncol_arg <- args[[unnamed_idx[[3L]]]]
  }

  has_nrow <- !is.null(nrow_arg) && !is_missing(nrow_arg)
  has_ncol <- !is.null(ncol_arg) && !is_missing(ncol_arg)

  # Case: no x, just nrow (identity matrix)
  if (is.null(x_arg) || is_missing(x_arg)) {
    if (!has_nrow) {
      return(NULL)
    }
    nrow <- infer_size(nrow_arg, scope)
    if (is.null(nrow)) {
      return(NULL)
    }
    ncol <- if (has_ncol) infer_size(ncol_arg, scope) else nrow
    if (is.null(ncol)) {
      return(NULL)
    }
    return(Variable("double", list(nrow, ncol)))
  }

  x <- infer_symbol_var(x_arg, scope)

  # Case: x is a matrix -> extract diagonal (returns vector)
  if (!is.null(x) && x@rank == 2L) {
    x_dims <- matrix_dims_var(x)
    diag_len <- diag_length_expr(x_dims$rows, x_dims$cols, "diag")
    return(Variable("double", list(diag_len)))
  }

  # Case: x is a scalar literal (identity matrix of that size)
  if (is.null(x) && is.numeric(x_arg) && length(x_arg) == 1L) {
    if (!has_nrow && !has_ncol && is_wholenumber(x_arg)) {
      n <- as.integer(x_arg)
      return(Variable("double", list(n, n)))
    }
  }

  # Case: x is a scalar symbol without nrow/ncol (identity matrix)
  if (!is.null(x) && x@rank == 0L && !has_nrow && !has_ncol) {
    return(NULL)
  }

  # Case: x is a vector or scalar, construct diagonal matrix
  if (!is.null(x) && x@rank <= 1L) {
    if (has_nrow || has_ncol) {
      nrow <- if (has_nrow) infer_size(nrow_arg, scope) else NULL
      ncol <- if (has_ncol) infer_size(ncol_arg, scope) else NULL
      if (is.null(nrow) && is.null(ncol)) {
        return(NULL)
      }
      if (is.null(nrow)) {
        nrow <- ncol
      }
      if (is.null(ncol)) {
        ncol <- nrow
      }
      return(Variable("double", list(nrow, ncol)))
    }
    # No nrow/ncol: square matrix from vector length
    if (x@rank == 1L) {
      len <- var_dim_or_one(x, 1L)
      return(Variable("double", list(len, len)))
    }
  }

  NULL
}
