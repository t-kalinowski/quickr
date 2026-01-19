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

bind_output_mode <- function(values, context) {
  modes <- unique(vapply(
    values,
    function(val) val@value@mode %||% NA_character_,
    character(1)
  ))
  if (anyNA(modes) || any(!nzchar(modes))) {
    stop(context, " inputs must have a known type", call. = FALSE)
  }
  if ("complex" %in% modes) {
    if (length(modes) > 1L) {
      stop(
        context,
        " does not support mixing complex with other types",
        call. = FALSE
      )
    }
    return("complex")
  }
  if ("double" %in% modes) {
    return("double")
  }
  if ("integer" %in% modes) {
    return("integer")
  }
  if ("logical" %in% modes) {
    return("logical")
  }
  if ("raw" %in% modes) {
    return("raw")
  }
  stop(context, " does not support input mode(s): ", str_flatten_commas(modes))
}

bind_cast_value <- function(value, mode, context) {
  if (identical(value@value@mode, mode)) {
    return(value)
  }
  if (identical(mode, "double")) {
    return(maybe_cast_double(value))
  }
  if (identical(mode, "integer") && identical(value@value@mode, "logical")) {
    return(Fortran(
      glue("merge(1_c_int, 0_c_int, {value})"),
      Variable("integer", value@value@dims)
    ))
  }
  stop(
    context,
    " does not support coercion from ",
    value@value@mode,
    " to ",
    mode,
    call. = FALSE
  )
}

bind_dim_sum <- function(values, context, label) {
  if (!length(values)) {
    return(0L)
  }
  if (any(map_lgl(values, is_scalar_na))) {
    stop(
      context,
      " requires inputs with known ",
      label,
      " sizes",
      call. = FALSE
    )
  }
  if (all(map_lgl(values, is_wholenumber))) {
    return(sum(as.integer(values)))
  }
  reduce(values, \(a, b) call("+", a, b))
}

bind_common_dim <- function(dim_list, scalar_flags, context, label) {
  non_scalar <- which(!scalar_flags)
  if (!length(non_scalar)) {
    return(1L)
  }
  target <- dim_list[[non_scalar[[1L]]]]
  if (is_scalar_na(target)) {
    stop(
      context,
      " requires inputs with known ",
      label,
      " counts",
      call. = FALSE
    )
  }
  if (length(non_scalar) > 1L) {
    for (idx in non_scalar[-1L]) {
      conform <- check_conformable(target, dim_list[[idx]])
      if (!conform$ok) {
        stop(
          context,
          " requires inputs with a common ",
          label,
          " count",
          call. = FALSE
        )
      }
      if (conform$unknown) {
        stop(
          context,
          " requires inputs with a common ",
          label,
          " count",
          call. = FALSE
        )
      }
    }
  }
  target
}

bind_dim_string <- function(dim) {
  if (is.character(dim)) {
    dim
  } else if (is_wholenumber(dim)) {
    as.character(as.integer(dim))
  } else if (is.numeric(dim)) {
    as.character(dim)
  } else {
    gsub("([0-9]+)L\\b", "\\1", deparse1(dim))
  }
}

bind_dim_int <- function(dim) {
  paste0("int(", bind_dim_string(dim), ")")
}

bind_col_matrix_expr <- function(value, rows, is_scalar, context) {
  rows_int <- bind_dim_int(rows)
  if (is_scalar) {
    vec <- glue("spread({value}, 1, {rows_int})")
    return(glue("reshape({vec}, [{rows_int}, 1])"))
  }
  if (value@value@rank == 1L) {
    return(glue("reshape({value}, [{rows_int}, 1])"))
  }
  if (value@value@rank == 2L) {
    return(as.character(value))
  }
  stop(context, " only supports rank 0-2 inputs", call. = FALSE)
}

bind_row_matrix_expr <- function(value, cols, is_scalar, context) {
  cols_int <- bind_dim_int(cols)
  if (is_scalar) {
    vec <- glue("spread({value}, 1, {cols_int})")
    return(glue("reshape({vec}, [1, {cols_int}])"))
  }
  if (value@value@rank == 1L) {
    return(glue("reshape({value}, [1, {cols_int}])"))
  }
  if (value@value@rank == 2L) {
    return(as.character(value))
  }
  stop(context, " only supports rank 0-2 inputs", call. = FALSE)
}

register_r2f_handler(
  "cbind",
  function(args, scope, ..., hoist = NULL) {
    context <- "cbind()"
    if (!is.null(args$deparse.level) && !is_missing(args$deparse.level)) {
      args$deparse.level <- NULL
    }
    args <- args[!vapply(args, is_missing, logical(1))]
    args <- args[
      !vapply(
        args,
        \(x) is.null(x) || identical(x, quote(NULL)),
        logical(1)
      )
    ]
    if (!length(args)) {
      stop("cbind() requires at least one argument", call. = FALSE)
    }

    values <- lapply(args, r2f, scope, ..., hoist = hoist)
    for (val in values) {
      if (is.null(val@value) || is.null(val@value@mode)) {
        stop(context, " inputs must have a value", call. = FALSE)
      }
      assert_rank_leq2(val, paste0(context, " only supports rank 0-2 inputs"))
    }

    mode <- bind_output_mode(values, context)
    values <- lapply(values, bind_cast_value, mode = mode, context = context)

    dims <- lapply(values, matrix_dims, orientation = "colvec")
    scalar_flags <- map_lgl(values, \(val) passes_as_scalar(val@value))
    row_sizes <- lapply(dims, `[[`, "rows")
    col_sizes <- lapply(dims, `[[`, "cols")

    rows <- bind_common_dim(row_sizes, scalar_flags, context, "row")
    cols <- bind_dim_sum(col_sizes, context, "column")

    col_exprs <- vector("list", length(values))
    for (i in seq_along(values)) {
      col_exprs[[i]] <- bind_col_matrix_expr(
        value = values[[i]],
        rows = rows,
        is_scalar = scalar_flags[[i]],
        context = context
      )
    }

    data_expr <- glue("[{str_flatten_commas(col_exprs)}]")
    out_expr <- glue(
      "reshape({data_expr}, [{bind_dim_int(rows)}, {bind_dim_int(cols)}])"
    )
    Fortran(out_expr, Variable(mode, list(rows, cols)))
  }
)

register_r2f_handler(
  "rbind",
  function(args, scope, ..., hoist = NULL) {
    context <- "rbind()"
    if (!is.null(args$deparse.level) && !is_missing(args$deparse.level)) {
      args$deparse.level <- NULL
    }
    args <- args[!vapply(args, is_missing, logical(1))]
    args <- args[
      !vapply(
        args,
        \(x) is.null(x) || identical(x, quote(NULL)),
        logical(1)
      )
    ]
    if (!length(args)) {
      stop("rbind() requires at least one argument", call. = FALSE)
    }

    values <- lapply(args, r2f, scope, ..., hoist = hoist)
    for (val in values) {
      if (is.null(val@value) || is.null(val@value@mode)) {
        stop(context, " inputs must have a value", call. = FALSE)
      }
      assert_rank_leq2(val, paste0(context, " only supports rank 0-2 inputs"))
    }

    mode <- bind_output_mode(values, context)
    values <- lapply(values, bind_cast_value, mode = mode, context = context)

    dims <- lapply(values, matrix_dims, orientation = "rowvec")
    scalar_flags <- map_lgl(values, \(val) passes_as_scalar(val@value))
    row_sizes <- lapply(dims, `[[`, "rows")
    col_sizes <- lapply(dims, `[[`, "cols")

    cols <- bind_common_dim(col_sizes, scalar_flags, context, "column")
    rows <- bind_dim_sum(row_sizes, context, "row")

    row_exprs <- vector("list", length(values))
    for (i in seq_along(values)) {
      row_exprs[[i]] <- bind_row_matrix_expr(
        value = values[[i]],
        cols = cols,
        is_scalar = scalar_flags[[i]],
        context = context
      )
    }
    transposed <- lapply(row_exprs, \(expr) glue("transpose({expr})"))

    data_expr <- glue("[{str_flatten_commas(transposed)}]")
    combined <- glue(
      "reshape({data_expr}, [{bind_dim_int(cols)}, {bind_dim_int(rows)}])"
    )
    out_expr <- glue("transpose({combined})")

    Fortran(out_expr, Variable(mode, list(rows, cols)))
  }
)


# Handle crossprod(), using SYRK for single-arg and GEMM for two-arg forms.
register_r2f_handler(
  "crossprod",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
    x_arg <- args$x %||% args[[1L]]
    y_arg <- args$y %||% if (length(args) > 1L) args[[2L]] else NULL
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
    x_arg <- args$x %||% args[[1L]]
    y_arg <- args$y %||% if (length(args) > 1L) args[[2L]] else NULL
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

register_r2f_handler(
  "solve",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
    a_arg <- args$a %||% args[[1L]]
    if (is.null(a_arg) || is_missing(a_arg)) {
      stop("solve() expects `a`", call. = FALSE)
    }
    if (!is.null(args$tol) && !is_missing(args$tol)) {
      stop("solve() does not support tol yet", call. = FALSE)
    }
    if (!is.null(args$LINPACK) && !is_missing(args$LINPACK)) {
      stop("solve() does not support LINPACK yet", call. = FALSE)
    }

    b_arg <- args$b %||% if (length(args) >= 2L) args[[2L]] else NULL
    if (!is.null(b_arg) && is_missing(b_arg)) {
      b_arg <- NULL
    }

    A <- r2f(a_arg, scope, ..., hoist = hoist)
    if (is.null(b_arg)) {
      return(lapack_inverse(
        A,
        scope = scope,
        hoist = hoist,
        dest = dest,
        context = "solve"
      ))
    }

    B <- r2f(b_arg, scope, ..., hoist = hoist)
    lapack_solve(
      A = A,
      B = B,
      scope = scope,
      hoist = hoist,
      dest = dest,
      context = "solve"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_solve
)

register_r2f_handler(
  "qr.solve",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
    a_arg <- args$a %||% args[[1L]]
    if (is.null(a_arg) || is_missing(a_arg)) {
      stop("qr.solve() expects `a`", call. = FALSE)
    }
    if (!is.null(args$tol) && !is_missing(args$tol)) {
      stop("qr.solve() does not support tol yet", call. = FALSE)
    }

    b_arg <- args$b %||% if (length(args) >= 2L) args[[2L]] else NULL
    if (is.null(b_arg) || is_missing(b_arg)) {
      stop("qr.solve() expects `b`", call. = FALSE)
    }

    A <- r2f(a_arg, scope, ..., hoist = hoist)
    B <- r2f(b_arg, scope, ..., hoist = hoist)
    lapack_solve(
      A = A,
      B = B,
      scope = scope,
      hoist = hoist,
      dest = dest,
      context = "qr.solve"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_solve
)

register_r2f_handler(
  "chol",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
    x_arg <- args$x %||% args[[1L]]
    if (is.null(x_arg) || is_missing(x_arg)) {
      stop("chol() expects `x`", call. = FALSE)
    }
    pivot <- logical_arg_or_default(args, "pivot", FALSE, "chol()")
    if (isTRUE(pivot)) {
      stop("chol() does not support pivot = TRUE yet", call. = FALSE)
    }
    if (!is.null(args$LINPACK) && !is_missing(args$LINPACK)) {
      stop("chol() does not support LINPACK yet", call. = FALSE)
    }

    x <- r2f(x_arg, scope, ..., hoist = hoist)
    lapack_chol(
      A = x,
      scope = scope,
      hoist = hoist,
      dest = dest,
      context = "chol"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_chol
)

register_r2f_handler(
  "chol2inv",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
    x_arg <- args$x %||% args[[1L]]
    if (is.null(x_arg) || is_missing(x_arg)) {
      stop("chol2inv() expects `x`", call. = FALSE)
    }
    if (!is.null(args$size) && !is_missing(args$size)) {
      stop("chol2inv() does not support size yet", call. = FALSE)
    }

    x <- r2f(x_arg, scope, ..., hoist = hoist)
    lapack_chol2inv(
      R = x,
      scope = scope,
      hoist = hoist,
      dest = dest,
      context = "chol2inv"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_chol2inv
)

register_r2f_handler(
  "diag",
  function(args, scope, ..., hoist = NULL, dest = NULL) {
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

    if (!is.null(args$names) && !is_missing(args$names)) {
      logical_arg_or_default(args, "names", TRUE, "diag()")
    }

    has_nrow <- !is.null(nrow_arg) && !is_missing(nrow_arg)
    has_ncol <- !is.null(ncol_arg) && !is_missing(ncol_arg)

    if (is.null(x_arg) || is_missing(x_arg)) {
      if (!has_nrow && !has_ncol) {
        stop("argument \"nrow\" is missing, with no default", call. = FALSE)
      }
      if (!has_nrow && has_ncol) {
        stop("argument \"nrow\" is missing, with no default", call. = FALSE)
      }
      x_val <- Fortran("1.0_c_double", Variable("double"))
      nrow <- if (has_nrow) r2size(nrow_arg, scope) else r2size(ncol_arg, scope)
      ncol <- if (has_ncol) r2size(ncol_arg, scope) else nrow
      return(diag_matrix(
        x = x_val,
        nrow = nrow,
        ncol = ncol,
        scope = scope,
        hoist = hoist,
        dest = dest,
        context = "diag"
      ))
    }

    x <- r2f(x_arg, scope, ..., hoist = hoist)
    x_rank <- x@value@rank

    if (x_rank == 2L) {
      if (has_nrow || has_ncol) {
        stop(
          "'nrow' or 'ncol' cannot be specified when 'x' is a matrix",
          call. = FALSE
        )
      }
      return(diag_extract(
        x = x,
        scope = scope,
        hoist = hoist,
        dest = dest,
        context = "diag"
      ))
    }

    assert_rank_leq2(
      x,
      "diag() only supports scalar, vector, or matrix inputs"
    )

    if (!has_nrow && !has_ncol && x_rank == 0L) {
      nrow <- r2size(x_arg, scope)
      ncol <- nrow
      x_val <- Fortran("1.0_c_double", Variable("double"))
      return(diag_matrix(
        x = x_val,
        nrow = nrow,
        ncol = ncol,
        scope = scope,
        hoist = hoist,
        dest = dest,
        context = "diag"
      ))
    }

    nrow <- if (has_nrow) r2size(nrow_arg, scope) else NULL
    ncol <- if (has_ncol) r2size(ncol_arg, scope) else NULL

    if (is.null(nrow) && is.null(ncol)) {
      nrow <- dim_or_one(x, 1L)
      ncol <- nrow
    } else if (is.null(nrow)) {
      nrow <- ncol
    } else if (is.null(ncol)) {
      ncol <- nrow
    }

    diag_matrix(
      x = x,
      nrow = nrow,
      ncol = ncol,
      scope = scope,
      hoist = hoist,
      dest = dest,
      context = "diag"
    )
  },
  dest_supported = TRUE,
  dest_infer = infer_dest_diag
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

    l_arg <- args$l %||% args[[1L]]
    x_arg <- args$x %||% args[[2L]]
    A <- r2f(l_arg, scope, ..., hoist = hoist)
    B <- r2f(x_arg, scope, ..., hoist = hoist)

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

    r_arg <- args$r %||% args[[1L]]
    x_arg <- args$x %||% args[[2L]]
    A <- r2f(r_arg, scope, ..., hoist = hoist)
    B <- r2f(x_arg, scope, ..., hoist = hoist)

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
    stop(
      "cannot verify conformability in ",
      context,
      " at compile time",
      call. = FALSE
    )
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
