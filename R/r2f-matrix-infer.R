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
  a_arg <- args[[1L]]
  if (is.null(a_arg)) {
    return(NULL)
  }
  A <- infer_symbol_var(a_arg, scope)
  if (is.null(A) || A@rank != 2L) {
    return(NULL)
  }
  a_dims <- matrix_dims_var(A)
  Variable("double", list(a_dims$rows, a_dims$cols))
}

# Infer destination dimensions for chol2inv().
infer_dest_chol2inv <- function(args, scope) {
  r_arg <- args[[1L]]
  if (is.null(r_arg)) {
    return(NULL)
  }
  R <- infer_symbol_var(r_arg, scope)
  if (is.null(R) || R@rank != 2L) {
    return(NULL)
  }
  r_dims <- matrix_dims_var(R)
  Variable("double", list(r_dims$rows, r_dims$cols))
}
