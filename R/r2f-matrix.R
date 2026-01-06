# Matrix-specific r2f handlers and wiring

# %*% handler with optional destination hint
register_r2f_handler(
  "%*%",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
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
      conform <- check_conformable(expected_len, right_dims$rows)
      if (!conform$ok) {
        stop("non-conformable arguments in %*%", call. = FALSE)
      }
      if (conform$unknown) {
        warn_conformability_unknown(expected_len, right_dims$rows, "%*%")
      }
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
        dest = dest,
        context = "%*%"
      ))
    }
    # Vector-Matrix: use GEMV with transpose
    if (left_rank == 1 && right_rank == 2) {
      transA <- if (right_trans == "N") "T" else "N"
      expected_len <- if (transA == "N") right_dims$cols else right_dims$rows
      conform <- check_conformable(left_dims$cols, expected_len)
      if (!conform$ok) {
        stop("non-conformable arguments in %*%", call. = FALSE)
      }
      if (conform$unknown) {
        warn_conformability_unknown(left_dims$cols, expected_len, "%*%")
      }
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
        dest = dest,
        context = "%*%"
      ))
    }

    conform <- check_conformable(k, right_eff$rows)
    if (!conform$ok) {
      stop("non-conformable arguments in %*%", call. = FALSE)
    }
    if (conform$unknown) {
      warn_conformability_unknown(k, right_eff$rows, "%*%")
    }

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
      dest = dest,
      context = "%*%"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_matmul
)


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


# Handle crossprod(), using SYRK for single-arg and GEMM for two-arg forms.
register_r2f_handler(
  "crossprod",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
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
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_crossprod
)


# Handle tcrossprod(), using SYRK for single-arg and GEMM for two-arg forms.
register_r2f_handler(
  "tcrossprod",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
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
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_tcrossprod
)

# Handle outer() for FUN = "*" as BLAS outer product.
register_r2f_handler(
  "outer",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
    x_arg <- args$X %||% args[[1L]]
    y_arg <- args$Y %||% if (length(args) >= 2L) args[[2L]] else NULL
    if (is.null(x_arg) || is.null(y_arg)) {
      stop("outer() expects X and Y")
    }

    fun <- args$FUN %||% "*"
    if (!identical(fun, "*")) {
      stop("outer() only supports FUN = \"*\"")
    }
    x <- r2f(x_arg, scope, ..., hoist = hoist)
    y <- r2f(y_arg, scope, ..., hoist = hoist)
    outer_mul(
      x,
      y,
      scope = scope,
      hoist = hoist,
      dest = dest,
      context = "outer"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_outer
)

# Handle %o% for outer products via BLAS GER.
register_r2f_handler(
  "%o%",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
    stopifnot(length(args) == 2L)
    x <- r2f(args[[1L]], scope, ..., hoist = hoist)
    y <- r2f(args[[2L]], scope, ..., hoist = hoist)
    outer_mul(
      x,
      y,
      scope = scope,
      hoist = hoist,
      dest = dest,
      context = "%o%"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_outer
)

# Handle forwardsolve() via triangular BLAS routines.
register_r2f_handler(
  "forwardsolve",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
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
      dest = dest,
      context = "forwardsolve"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_triangular
)

# Handle backsolve() via triangular BLAS routines.
register_r2f_handler(
  "backsolve",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
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
      dest = dest,
      context = "backsolve"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_triangular
)

# Shared crossprod/tcrossprod logic for one- and two-argument forms.
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
      dest = dest,
      context = context
    ))
  }

  y <- maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  x_dims <- matrix_dims(x)
  y_dims <- matrix_dims(y)
  x_eff <- effective_dims(x_dims, opA)
  y_eff <- effective_dims(y_dims, opB)

  conform <- check_conformable(x_eff$cols, y_eff$rows)
  if (!conform$ok) {
    stop("non-conformable arguments in ", context, call. = FALSE)
  }
  if (conform$unknown) {
    warn_conformability_unknown(x_eff$cols, y_eff$rows, context)
  }

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
    dest = dest,
    context = context
  )
}
