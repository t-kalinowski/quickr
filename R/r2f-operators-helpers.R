# r2f-operators-helpers.R
# Generic helpers for binary operators and type conformance.

# Convert a logical value backed by bind(c) integer storage (0/1/NA) to a
# Fortran LOGICAL expression. Symbols are typically booleanized during r2f()
# (see r2f-aab-core.R), but expressions like rev(x) need handling at use sites.
# Used by: r2f-logical.R, r2f-conditionals.R, r2f-subscript.R, r2f-reductions.R
booleanize_logical_as_int <- function(x) {
  stopifnot(inherits(x, Fortran))

  if (
    is.null(x@value) ||
      !identical(x@value@mode, "logical") ||
      !logical_as_int(x@value)
  ) {
    return(x)
  }

  if (isTRUE(x@logical_booleanized)) {
    return(x)
  }

  out <- Fortran(glue("({x} /= 0)"), Variable("logical", x@value@dims))
  out@logical_booleanized <- TRUE
  out
}

# Cast a Fortran value to a target mode by wrapping its expression text.
# The only place cast spellings live; errors on casts it cannot spell
# (complex/character operands, or any narrowing) so an unsupported mode is
# a clean diagnostic instead of invalid generated Fortran.
# Used by: r2f-arithmetic.R, r2f-math.R, r2f-reductions.R, r2f-subscript.R
cast_to_mode <- function(x, mode, context = "operand") {
  stopifnot(inherits(x, Fortran))
  if (
    is.null(mode) ||
      is.null(x@value@mode) || # mode still being inferred; nothing to spell
      identical(x@value@mode, mode)
  ) {
    return(x)
  }
  from <- x@value@mode
  if (identical(mode, "double") && identical(from, "integer")) {
    return(Fortran(
      glue("real({x}, kind=c_double)"),
      Variable("double", x@value@dims)
    ))
  }
  if (identical(from, "logical") && mode %in% c("integer", "double")) {
    if (logical_as_int(x@value) && !isTRUE(x@logical_booleanized)) {
      # bind(c) logicals are already 0/1 integer storage; relabel, or cast
      # the integer text directly for a double target.
      relabeled <- Fortran(glue("{x}"), Variable("integer", x@value@dims))
      return(cast_to_mode(relabeled, mode, context))
    }
    x <- booleanize_logical_as_int(x)
    literals <- switch(
      mode,
      integer = "1_c_int, 0_c_int",
      double = "1.0_c_double, 0.0_c_double"
    )
    return(Fortran(
      glue("merge({literals}, {x})"),
      Variable(mode, x@value@dims)
    ))
  }
  stop(
    context,
    " does not support coercion from ",
    from,
    " to ",
    mode,
    call. = FALSE
  )
}

# Cast a value to double if it's logical or integer.
# Used by: r2f-arithmetic.R, r2f-math.R, r2f-matrix*.R, r2f-coercions.R
maybe_cast_double <- function(x) {
  if (x@value@mode %in% c("logical", "integer")) {
    cast_to_mode(x, "double")
  } else {
    x
  }
}

# Cast a linear-algebra operand to double for the real BLAS/LAPACK
# lowerings (dgemm, dgesv, ...). Complex operands are refused: the d*
# routines would read complex storage as reals and return a plausible
# wrong answer, and quickr has no z* lowerings. R supports complex
# linear algebra, so the message names the divergence.
# Used by: r2f-matrix.R, r2f-matrix-parse.R, r2f-matrix-blas.R
cast_linalg_double <- function(x, context, hoist = NULL) {
  if (identical(x@value@mode, "complex")) {
    stop_static_mode_error(
      paste0(
        context,
        " does not support complex operands; ",
        "linear algebra in quickr is double-only"
      ),
      hoist
    )
  }
  x <- maybe_cast_double(x)
  if (!identical(x@value@mode, "double")) {
    stop_static_mode_error(
      paste0(
        context,
        " does not support ",
        x@value@mode,
        " operands; linear algebra in quickr is double-only"
      ),
      hoist
    )
  }
  x
}

# Promote a list of operands to their common (lattice-join) mode, casting
# each one whose mode differs. For contexts where Fortran requires uniform
# argument types: array constructors (c()), min/max, merge, modulo.
# Returns list(args = <cast operands>, mode = <join>).
# Used by: r2f-conditionals.R, r2f-constructors.R
promote_operands <- function(args, context = "operator") {
  mode <- reduce_promoted_mode(args)
  list(
    args = lapply(args, cast_to_mode, mode = mode, context = context),
    mode = mode
  )
}

# Lattice join for arithmetic contexts: logical joins as integer (R:
# TRUE + TRUE is 2L, sum(TRUE) is 1L; Fortran has no logical arithmetic).
# Used by: r2f-arithmetic.R, r2f-math.R, r2f-reductions.R
arith_join_mode <- function(...) {
  mode <- reduce_promoted_mode(...)
  if (identical(mode, "logical")) "integer" else mode
}

# Apply R's arithmetic rule for logical operands: they join as integer
# (TRUE + TRUE is 2L), and Fortran has no logical arithmetic, so cast them.
# int/double mixes are left alone -- Fortran's own promotion matches R.
# Used by: r2f-arithmetic.R, r2f-logical.R
promote_arith_pair <- function(left, right, context = "arithmetic") {
  if (
    identical(left@value@mode, "logical") ||
      identical(right@value@mode, "logical")
  ) {
    mode <- arith_join_mode(left, right)
    left <- cast_to_mode(left, mode, context)
    right <- cast_to_mode(right, mode, context)
  }
  list(left = left, right = right)
}

# Lower arbitrary argument lists in R's left-to-right order. Used by
# conditionals and constructors; elementwise lowering has additional matrix
# fill handling below.
lower_operands_in_order <- function(args, scope, ..., hoist) {
  stopifnot(!is.null(hoist))
  out <- vector("list", length(args))
  names(out) <- names(args)
  for (i in seq_along(args)) {
    later_args <- if (i < length(args)) {
      args[seq.int(i + 1L, length(args))]
    } else {
      list()
    }
    out[[i]] <- lower_r2f_operand_in_order(
      args[[i]],
      scope,
      ...,
      hoist = hoist,
      later_args = later_args
    )
  }
  out
}

# Match `matrix(<scalar>, nrow, ncol)`: a matrix() call
# matrix_call_args() accepts (data/nrow/ncol present, no
# byrow/dimnames) whose data is a length-1 literal or a declared
# scalar. Returns the matched arguments or NULL. Used by
# lower_elementwise_operands() to lower the fill to a native scalar
# broadcast instead of the O(nrow * ncol) temporary the matrix()
# handler would otherwise materialize; anything it declines falls back
# to the matrix() handler, which raises the real diagnostics.
match_scalar_matrix_fill <- function(e, scope) {
  if (!is.call(e) || !identical(e[[1L]], quote(matrix))) {
    return(NULL)
  }
  if (!is.null(scope) && inherits(scope[["matrix"]], LocalClosure)) {
    return(NULL)
  }
  mc <- tryCatch(match.call(matrix, e), error = function(...) NULL)
  if (is.null(mc)) {
    return(NULL)
  }
  margs <- tryCatch(
    matrix_call_args(as.list(mc)[-1L]),
    error = function(...) NULL
  )
  if (is.null(margs)) {
    return(NULL)
  }
  data <- margs$data
  data_is_scalar <- (is.atomic(data) && length(data) == 1L && !is.na(data)) ||
    (is.symbol(data) &&
      {
        var <- get0(as.character(data), scope)
        inherits(var, Variable) && passes_as_scalar(var)
      })
  if (!data_is_scalar) {
    return(NULL)
  }
  margs
}

# Compile the two operands of an elementwise binary op. The one special
# case: `matrix(scalar, m, n)` against a genuine rank-2 array broadcasts
# natively -- compile just the scalar and enforce the claimed dims against
# the other operand (compile error when statically wrong, runtime guard
# when symbolic, spelled from the dim expressions since the fill has no
# array to size()). Everything else compiles as written.
# Used by: r2f-arithmetic.R, r2f-logical.R
lower_elementwise_operands <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 2L, !is.null(hoist))
  lower_one <- function(arg) {
    lower_r2f_operand_in_order(arg, scope, ..., hoist = hoist)
  }

  fills <- lapply(args, match_scalar_matrix_fill, scope = scope)
  fill_idx <- which(!map_lgl(fills, is.null))

  if (length(fill_idx) == 1L && !is.null(hoist)) {
    j <- fill_idx
    fill_args <- fills[[j]]
    fill_dims <- lapply(list(fill_args$nrow, fill_args$ncol), function(dim) {
      if (is.numeric(dim) && length(dim) == 1L && is.finite(dim)) {
        as.integer(dim)
      } else {
        dim
      }
    })
    validate_constructor_dims(fill_dims, "matrix()", scope, hoist)
    fill_dims_f <- map_chr(fill_dims, bind_dim_int)
    lower_fill <- function(materialize = FALSE) {
      lower_arg <- function(arg) {
        value <- lower_one(arg)
        if (!materialize) {
          return(value)
        }
        materialize_via_hoist(
          value,
          value@value@mode,
          value@value@dims,
          hoist,
          allocate_at_point = TRUE
        )
      }
      value <- lower_arg(fill_args$data)
      dims <- lapply(list(fill_args$nrow, fill_args$ncol), lower_arg)
      list(
        value = value,
        dims_f = map_chr(dims, \(dim) glue("int({dim})"))
      )
    }

    if (j == 1L) {
      fill <- lower_fill(materialize = TRUE)
      fill_dims_f <- fill$dims_f
      other <- lower_one(args[[2L]])
    } else {
      other <- lower_one(args[[1L]])
    }
    broadcastable <- inherits(other, Fortran) &&
      !is.null(other@value) &&
      other@value@rank == 2L &&
      !passes_as_scalar(other@value) &&
      !any(map_lgl(fill_dims, is_scalar_na)) &&
      all(nzchar(fill_dims_f)) &&
      !any(grepl(":", fill_dims_f, fixed = TRUE))
    if (broadcastable) {
      if (j == 2L) {
        fill <- lower_fill()
        fill_dims_f <- fill$dims_f
      }
      other_dims <- matrix_dims(other)
      for (axis in 1:2) {
        # The fill has no array to size(), so its side of a runtime guard
        # is spelled from the claimed dim expression via `left_f`.
        guard_conformable_dims(
          fill_dims[[axis]],
          if (axis == 1L) other_dims$rows else other_dims$cols,
          elementwise_matrix_msg,
          hoist,
          scope,
          left = NULL,
          right = other,
          right_axis = axis,
          left_f = glue("({fill_dims_f[[axis]]})")
        )
      }
      out <- list(fill$value, other)
      return(if (j == 1L) out else rev(out))
    }
    fallback <- if (j == 1L) {
      materialize_via_hoist(
        fill$value,
        fill$value@value@mode,
        fill_dims,
        hoist,
        allocate_at_point = TRUE
      )
    } else {
      lower_one(args[[j]])
    }
    out <- list(fallback, other)
    return(if (j == 1L) out else rev(out))
  }

  lower_operands_in_order(args, scope, ..., hoist = hoist)
}

# Check if a dimension expression equals 1.
# Used by: r2f-matrix.R, is_one_by_one()
dim_is_one <- function(x) {
  is_wholenumber(x) && identical(as.integer(x), 1L)
}

# Check if a dimension expression is statically known and greater than 1.
# Symbolic dimensions and zero are FALSE: neither is a supported scalarized
# vector length.
# Used by: conform_elementwise_operands()
dim_known_greater_than_one <- function(x) {
  is_wholenumber(x) && as.integer(x) > 1L
}

# Check if a Fortran value is a 1x1 matrix.
# Used by: conform_elementwise_operands()
is_one_by_one <- function(x) {
  stopifnot(inherits(x, Fortran))
  x@value@rank == 2L &&
    dim_is_one(x@value@dims[[1L]]) &&
    dim_is_one(x@value@dims[[2L]])
}

# Check if two dimension expressions provably match (both known and
# equal, or the identical symbolic expression). Weaker than
# check_elementwise_lengths(): no zero-length policy, no symbol
# normalization -- callers use it for routing/declaration decisions, not
# for the conformability contract.
# Used by: r2f-matrix.R (bind_common_dim)
dims_match <- function(left, right) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    return(identical(as.integer(left), as.integer(right)))
  }
  identical(left, right)
}

# Check if two dimension expressions are *proven* equal: both known and
# equal, or structurally identical after symbol normalization. NA dims
# are never proven (two unknown lengths are not the same quantity).
# Stronger than dims_match() (normalizes symbols), stricter than
# check_elementwise_lengths() (an incomparable pair is FALSE, not a
# deferred runtime guard).
# Used by: r2f-matrix-blas.R (dest_dims_proven_equal)
dims_proven_equal <- function(left, right) {
  if (is_scalar_na(left) || is_scalar_na(right)) {
    return(FALSE)
  }
  if (is_wholenumber(left) && is_wholenumber(right)) {
    return(identical(as.integer(left), as.integer(right)))
  }
  identical(fortranize_expr_symbols(left), fortranize_expr_symbols(right))
}

# Equality-only dimension checks permit matching zero extents. This is the
# conformability contract for BLAS operands and shape-preserving operations
# such as ifelse(); arithmetic uses the stricter nonempty checker below.
check_equal_dims <- function(left, right) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    return(list(
      ok = identical(as.integer(left), as.integer(right)),
      unknown = FALSE,
      reject_zero = FALSE
    ))
  }
  if (!is_scalar_na(left) && !is_scalar_na(right)) {
    left_norm <- fortranize_expr_symbols(left)
    right_norm <- fortranize_expr_symbols(right)
    if (identical(left_norm, right_norm)) {
      return(list(ok = TRUE, unknown = FALSE, reject_zero = FALSE))
    }
  }
  list(ok = TRUE, unknown = TRUE, reject_zero = FALSE)
}

# Three-valued conformability verdict for one axis of an elementwise op:
# ok+known (no guard needed), not-ok+known (compile error at the caller),
# or unknown (caller emits a runtime guard). Known lengths must be equal
# and nonzero -- R-style recycling is never implemented, and quickr cannot
# represent length-0 results. Symbolic dims are always unknown because their
# positivity must be checked at runtime, even when both operands use the same
# expression.
# Used by: guard_conformable_dims()
check_elementwise_lengths <- function(left, right) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    left <- as.integer(left)
    right <- as.integer(right)
    return(list(
      ok = left == right && left > 0L,
      unknown = FALSE,
      reject_zero = TRUE
    ))
  }
  if (
    (is_wholenumber(left) && as.integer(left) == 0L) ||
      (is_wholenumber(right) && as.integer(right) == 0L)
  ) {
    return(list(ok = FALSE, unknown = FALSE, reject_zero = TRUE))
  }
  list(ok = TRUE, unknown = TRUE, reject_zero = TRUE)
}

# Equality-only shape checks allow zero extents. Used where operands must have
# the same physical shape but no elementwise recycling policy applies.
check_equal_dims <- function(left, right) {
  if (is_wholenumber(left) && is_wholenumber(right)) {
    return(list(
      ok = identical(as.integer(left), as.integer(right)),
      unknown = FALSE,
      reject_zero = FALSE
    ))
  }
  if (!is_scalar_na(left) && !is_scalar_na(right)) {
    left_norm <- fortranize_expr_symbols(left)
    right_norm <- fortranize_expr_symbols(right)
    if (identical(left_norm, right_norm)) {
      return(list(ok = TRUE, unknown = FALSE, reject_zero = FALSE))
    }
  }
  list(ok = TRUE, unknown = TRUE, reject_zero = FALSE)
}

# The message shared by every enforcement point of the elementwise matrix
# shape contract: the runtime-guard text must match the compile-error text.
elementwise_matrix_msg <-
  "elementwise matrix operations require matching dimensions"

# Render one side of a dim-comparison guard: a caller-provided spelling
# (`f`, for operands with no array to size(), e.g. a scalar fill's claimed
# dims) wins; then a literal dim as the literal; anything else as the
# operand's actual extent (whole size when `axis` is NULL). size() is an
# inquiry, so applying it to operand expression text does not evaluate the
# operand.
# Used by: guard_conformable_dims()
dimension_guard_expr <- function(dim, operand, axis = NULL, f = NULL) {
  if (!is.null(f)) {
    return(f)
  }
  if (is_wholenumber(dim)) {
    return(as.character(as.integer(dim)))
  }
  if (is.null(axis)) {
    glue("size({operand}, kind=c_ptrdiff_t)")
  } else {
    glue("size({operand}, {axis}, kind=c_ptrdiff_t)")
  }
}

# Shared conformability guard emitter. `checker` supplies the caller's
# static and runtime policy: elementwise operations require equal nonzero
# dimensions, while BLAS/LAPACK callers use equality semantics that permit zero.
# A statically known mismatch is a compile error; unknown dimensions get a
# statement-level runtime guard; provably equal dimensions need nothing.
# `axis` NULL compares the operand's whole size (rank-1 operands).
#
# `hoist` is always live: r2f() opens one per statement before dispatching
# to a handler, and every caller forwards the one it received.
# emit_quickr_error_if() asserts it.
# Used by: conform_elementwise_operands(), lower_elementwise_operands(),
# r2f-conditionals.R, r2f-matrix*.R. `left_f`/`right_f` override that
# side's guard spelling (see dimension_guard_expr()); its `left`/`right` operand is
# then unused and may be NULL.
guard_conformable_dims <- function(
  left_dim,
  right_dim,
  message,
  hoist,
  scope,
  left,
  right,
  left_axis = NULL,
  right_axis = NULL,
  left_f = NULL,
  right_f = NULL,
  checker = check_elementwise_lengths,
  defer_static_error = isTRUE(hoist$defer_static_shape_error)
) {
  stopifnot(
    is_string(message),
    is.function(checker),
    is_bool(defer_static_error)
  )
  conform <- checker(left_dim, right_dim)
  if (!conform$ok) {
    if (defer_static_error) {
      stop_deferred_branch_error(message)
    }
    stop(message, call. = FALSE)
  }
  if (conform$unknown) {
    left_guard <- dimension_guard_expr(left_dim, left, left_axis, left_f)
    right_guard <- dimension_guard_expr(right_dim, right, right_axis, right_f)
    condition <- glue("{left_guard} /= {right_guard}")
    if (isTRUE(conform$reject_zero)) {
      condition <- glue(
        "{left_guard} == 0 .or. {condition}"
      )
    }
    emit_quickr_error_if(
      condition,
      message,
      hoist,
      scope
    )
  }
  invisible(TRUE)
}

# Reshape a vector to match a matrix's dimensions.
# Used by: r2f-constructors.R, conform_elementwise_operands()
reshape_vector_for_matrix <- function(vec, rows, cols) {
  stopifnot(inherits(vec, Fortran))
  out_val <- Variable(vec@value@mode, list(rows, cols))
  source <- if (passes_as_scalar(vec@value)) {
    glue("[{vec}]")
  } else {
    glue("{vec}")
  }
  out_expr <- glue(
    "reshape({source}, [{bind_dim_int(rows)}, {bind_dim_int(cols)}], pad = {source})"
  )
  Fortran(out_expr, out_val)
}

# Size expressions may carry an as.integer() coercion (diag()'s identity
# form sizes the result with as.integer(x), as R does). The two renderers
# that spell a dim by deparsing -- bind_dim_string() and blas_int() -- would
# emit the R name verbatim, so map it to Fortran's INT(), which truncates
# toward zero the same way. dims2f()/dims2c() translate the call properly
# and do not need this.
# Used by: bind_dim_string() (r2f-matrix.R), blas_int() (r2f-matrix-blas.R)
fortranize_size_calls <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  if (identical(e[[1L]], quote(as.integer))) {
    e[[1L]] <- quote(int)
  }
  for (i in seq_along(e)[-1L]) {
    if (!is_missing(e[[i]])) {
      e[[i]] <- fortranize_size_calls(e[[i]])
    }
  }
  e
}

# Floor a double expression while staying in the real domain: Fortran
# FLOOR() returns an integer, so a large double (e.g. 1e20) would
# silently overflow. aint(x) truncates toward 0 (real result); adjust by
# -1 where truncation differs from floor (negative non-integers). `x` is
# spliced three times, so callers hoist non-trivial expressions first.
# Used by: r2f-math.R (floor), r2f-arithmetic.R (double %/%)
real_floor_expr <- function(x) {
  aint <- glue("aint({x})")
  glue("({aint} - merge(1.0_c_double, 0.0_c_double, ({x} < {aint})))")
}

# Convert a 1x1 matrix to a scalar.
# Used by: r2f-matrix.R, conform_elementwise_operands()
scalarize_matrix <- function(mat) {
  stopifnot(inherits(mat, Fortran))
  out_val <- Variable(mat@value@mode)
  Fortran(glue("{mat}(1, 1)"), out_val)
}

# Reshape vector/matrix operands to match ranks for binary operations, and
# enforce the elementwise conformability policy via guard_conformable_dims():
# known-mismatched lengths are compile errors (R-style recycling is not
# supported), while unknown lengths get a runtime guard. Scalar broadcast
# requires a value represented as scalar at translation time, such as
# `double(1)`; an assumed-shape `double(NA)` remains a vector even when its
# runtime length is one.
#
# `scalarize_one_by_one` mirrors R's split over length-1 arrays: arithmetic
# recycles a 1x1 matrix against a vector of statically known length != 1
# (deprecated in R but still the behavior: R drops the array dims). When
# the vector's length is only known at run time, the result's shape would
# depend on that value -- R keeps the 1x1 dims for a length-1 vector and
# drops them otherwise -- so the 1x1 falls through to the vector-matrix
# rule: a runtime guard requires length 1 and the result is a 1x1 matrix,
# an error where R would recycle. Comparisons and & | error in R itself,
# so strict callers pass FALSE and the 1x1 always takes the vector-matrix
# path.
# Used by: r2f-arithmetic.R, r2f-logical.R
conform_elementwise_operands <- function(
  left,
  right,
  hoist,
  scope,
  scalarize_one_by_one = TRUE
) {
  defer_static_error <- isTRUE(hoist$defer_static_shape_error)
  stop_static_error <- function(message) {
    if (defer_static_error) {
      stop_deferred_branch_error(message)
    }
    stop(message, call. = FALSE)
  }
  if (
    !inherits(left, Fortran) ||
      !inherits(right, Fortran) ||
      is.null(left@value) ||
      is.null(right@value)
  ) {
    return(list(left = left, right = right))
  }

  left_scalar <- passes_as_scalar(left@value)
  right_scalar <- passes_as_scalar(right@value)
  left_rank <- if (left_scalar) 0L else left@value@rank
  right_rank <- if (right_scalar) 0L else right@value@rank

  # Casts and booleanization wrap operands in expression text that Fortran
  # cannot index (`real(x, kind=c_double)(1, 1)` is invalid), so hoist
  # anything that is not a bare name before subscripting it.
  scalarize_via_hoist <- function(x) {
    if (!is.null(hoist)) {
      x <- hoist_unless_name(x, hoist)
    }
    scalarize_matrix(x)
  }

  if (
    scalarize_one_by_one &&
      left_rank == 2L &&
      right_rank == 1L &&
      is_one_by_one(left)
  ) {
    right_len <- r2size(dim_or_one(right, 1L), scope)
    if (dim_known_greater_than_one(right_len)) {
      left <- scalarize_via_hoist(left)
      left_rank <- 0L
    }
  } else if (
    scalarize_one_by_one &&
      left_rank == 1L &&
      right_rank == 2L &&
      is_one_by_one(right)
  ) {
    left_len <- r2size(dim_or_one(left, 1L), scope)
    if (dim_known_greater_than_one(left_len)) {
      right <- scalarize_via_hoist(right)
      right_rank <- 0L
    }
  }

  check_nonempty <- function(x, message) {
    for (axis in seq_len(x@value@rank)) {
      dim <- r2size(dim_or_one(x, axis), scope)
      if (is_wholenumber(dim)) {
        if (as.integer(dim) == 0L) {
          stop_static_error(message)
        }
      } else {
        emit_quickr_error_if(
          glue("size({x}, {axis}, kind=c_ptrdiff_t) == 0_c_ptrdiff_t"),
          message,
          hoist,
          scope
        )
      }
    }
  }

  if (xor(left_scalar, right_scalar)) {
    array_operand <- if (left_scalar) right else left
    message <- if (array_operand@value@rank == 1L) {
      paste0(
        "elementwise vector operations require equal lengths or ",
        "a scalar operand; R-style recycling is not supported"
      )
    } else {
      "elementwise matrix operations require matching dimensions"
    }
    check_nonempty(array_operand, message)
  }

  if (left_rank == 1L && right_rank == 1L) {
    vector_msg <- paste0(
      "elementwise vector operations require equal lengths or ",
      "a scalar operand; R-style recycling is not supported"
    )
    guard_conformable_dims(
      dim_or_one(left, 1L),
      dim_or_one(right, 1L),
      vector_msg,
      hoist,
      scope,
      left = left,
      right = right,
      defer_static_error = defer_static_error
    )
  }

  if (left_rank >= 2L && left_rank == right_rank) {
    array_msg <- if (left_rank == 2L) {
      elementwise_matrix_msg
    } else {
      "elementwise array operations require matching dimensions"
    }
    for (axis in seq_len(left_rank)) {
      guard_conformable_dims(
        dim_or_one(left, axis),
        dim_or_one(right, axis),
        array_msg,
        hoist,
        scope,
        left = left,
        right = right,
        left_axis = axis,
        right_axis = axis,
        defer_static_error = defer_static_error
      )
    }
  } else if (
    left_rank > 0L &&
      right_rank > 0L &&
      left_rank != right_rank &&
      !setequal(c(left_rank, right_rank), c(1L, 2L))
  ) {
    stop_static_error(
      "elementwise array operations require matching dimensions"
    )
  }

  vec_mat_msg <- paste0(
    "elementwise vector-matrix operations require a scalar or ",
    "a vector length equal to the matrix first dimension (nrow)"
  )
  if (left_rank == 1L && right_rank == 2L) {
    right_dims <- matrix_dims(right)
    check_nonempty(right, vec_mat_msg)
    guard_conformable_dims(
      dim_or_one(left, 1L),
      right_dims$rows,
      vec_mat_msg,
      hoist,
      scope,
      left = left,
      right = right,
      right_axis = 1L,
      defer_static_error = defer_static_error
    )
    left <- hoist_unless_name(left, hoist)
    left <- reshape_vector_for_matrix(left, right_dims$rows, right_dims$cols)
  } else if (left_rank == 2L && right_rank == 1L) {
    left_dims <- matrix_dims(left)
    check_nonempty(left, vec_mat_msg)
    guard_conformable_dims(
      dim_or_one(right, 1L),
      left_dims$rows,
      vec_mat_msg,
      hoist,
      scope,
      left = right,
      right = left,
      right_axis = 1L,
      defer_static_error = defer_static_error
    )
    right <- hoist_unless_name(right, hoist)
    right <- reshape_vector_for_matrix(right, left_dims$rows, left_dims$cols)
  }

  list(left = left, right = right)
}

# R's promotion order for the supported atomic modes, lowest to highest.
# The one place the lattice is spelled: promotion joins take the highest
# rank present; the narrowing check refuses assignments that move down.
mode_lattice <- c("logical", "integer", "double", "complex")

# Rank of a mode on the lattice (NA for modes outside it, e.g. character).
# Used by: reduce_promoted_mode(), check_reassignment_narrowing()
mode_rank <- function(mode) {
  match(mode, mode_lattice)
}

# A dim expression can be spelled in a runtime guard only if every
# self-size symbol it references (`foo__len_`, `foo__dim_1_`) belongs
# to an external variable -- those arrive as size dummies; a local's
# self-sizes are phantoms backing implicit allocation and do not exist
# in the generated Fortran.
# Used by: check_assignment_compatible()
dim_guard_spellable <- function(dim, scope) {
  if (!is.language(dim)) {
    return(TRUE)
  }
  if (is.null(scope)) {
    return(FALSE)
  }
  matches <- regmatches(
    syms <- all.vars(dim),
    regexec("^(.*)__(dim_[0-9]+|len)_$", syms)
  )
  all(vapply(
    matches,
    function(match) {
      if (!length(match)) {
        return(TRUE) # not a self-size symbol
      }
      var <- get0(match[[2L]], scope)
      inherits(var, Variable) && var@is_external
    },
    logical(1)
  ))
}

# Reassignment cannot re-declare a Fortran variable to a new shape the
# way R rebinds a symbol, so rank and every extent must stay
# compatible. Scalars broadcast natively into an array target (an
# existing divergence: R rebinds the symbol to the scalar), and a
# deferred-shape local (declared with NA dims) reallocates extents on
# same-rank whole-array assignment, matching R, so it is exempt. Per
# axis, the conformability policy applies: a statically known mismatch
# is a compile error, dims that cannot be compared statically get a
# statement-level runtime guard through `hoist` (spelled from the dim
# expressions, when spellable), and provably equal dims need nothing.
# Callers with no statement context (no hoist) get the static checks
# only.
# Used by: r2f-assign.R, scope.R
check_assignment_compatible <- function(
  name,
  target,
  value,
  hoist = NULL,
  scope = NULL
) {
  if (
    is.null(value) ||
      !inherits(target, Variable) ||
      !inherits(value, Variable)
  ) {
    return(invisible())
  }
  target_scalar <- passes_as_scalar(target)
  value_scalar <- passes_as_scalar(value)
  # Two length-1 values conform whatever their ranks (a declared `double(1)`
  # is rank 1; a literal is rank 0).
  if (target_scalar && value_scalar) {
    return(invisible())
  }
  deferred_local <- !target@is_external && has_self_size_dims(target)
  if (target_scalar || value_scalar) {
    # A deferred-shape local can genuinely take a new array shape.
    if (deferred_local && !value_scalar) {
      return(invisible())
    }
    # Otherwise one side is length 1 and the other is a real array. R
    # rebinds the symbol to the new shape; Fortran cannot, and would
    # silently broadcast a scalar across the array (or drop all but the
    # first element of an array into a scalar).
    stop(
      "cannot reassign `",
      name,
      "`: replacement is ",
      if (value_scalar) "a scalar" else "an array",
      " but `",
      name,
      "` is ",
      if (target_scalar) "a scalar" else "an array",
      "; R would rebind `",
      name,
      "` to the new shape",
      call. = FALSE
    )
  }
  if (target@rank != value@rank) {
    stop(
      "cannot reassign `",
      name,
      "`: replacement rank (",
      value@rank,
      ") differs from the declared rank (",
      target@rank,
      "); R would rebind `",
      name,
      "` to the new shape",
      call. = FALSE
    )
  }
  if (deferred_local) {
    # deferred-shape local: implicit (re)allocation matches R's rebind
    return(invisible())
  }
  emitted <- character()
  for (axis in seq_len(target@rank)) {
    t_dim <- target@dims[[axis]]
    v_dim <- value@dims[[axis]]
    unknown_extent <- is_scalar_na(t_dim) || is_scalar_na(v_dim)
    if (!unknown_extent) {
      if (is_wholenumber(t_dim) && is_wholenumber(v_dim)) {
        if (!identical(as.integer(t_dim), as.integer(v_dim))) {
          stop(
            "cannot reassign `",
            name,
            "`: dimension ",
            axis,
            " would change from ",
            as.integer(t_dim),
            " to ",
            as.integer(v_dim),
            "; R would rebind `",
            name,
            "` to the new shape",
            call. = FALSE
          )
        }
        next
      }
      if (
        identical(
          fortranize_expr_symbols(t_dim),
          fortranize_expr_symbols(v_dim)
        )
      ) {
        next
      }
    }
    if (is.null(hoist)) {
      next
    }
    if (!is.null(target@name) && !is.null(value@name)) {
      condition <- glue(
        "size({target@name}, {axis}, kind=c_ptrdiff_t) /= size({value@name}, {axis}, kind=c_ptrdiff_t)"
      )
    } else if (
      !unknown_extent &&
        dim_guard_spellable(t_dim, scope) &&
        dim_guard_spellable(v_dim, scope)
    ) {
      condition <- glue(
        "({dims2f(list(t_dim), scope)}) /= ({dims2f(list(v_dim), scope)})"
      )
    } else {
      stop(
        "cannot reassign `",
        name,
        "`: runtime shape cannot be validated",
        call. = FALSE
      )
    }
    # Different axes can spell the same guard (e.g. a square dest vs a
    # square result, m/=k on both axes); emit each condition once.
    if (condition %in% emitted) {
      next
    }
    emitted <- c(emitted, condition)
    emit_quickr_error_if(
      condition,
      sprintf("reassignment must preserve the shape of `%s`", name),
      hoist,
      scope
    )
  }
  invisible()
}

# Whole-binding reassignment cannot re-type a Fortran variable the way R
# rebinds a symbol, so require the same mode. Subassignment writes into the
# existing storage and may coerce a lower-mode replacement, but still refuses
# a replacement that would promote the R vector and truncate in Fortran.
# Used by: r2f-assign.R
check_reassignment_narrowing <- function(
  name,
  target,
  value,
  whole_binding = TRUE
) {
  stopifnot(is_bool(whole_binding))
  if (
    !inherits(target, Variable) ||
      !inherits(value, Variable) ||
      is.null(target@mode) ||
      is.null(value@mode)
  ) {
    return()
  }
  if (identical(target@mode, value@mode)) {
    return()
  }
  target_rank <- mode_rank(target@mode)
  value_rank <- mode_rank(value@mode)
  if (
    isTRUE(whole_binding) &&
      (is.na(target_rank) ||
        is.na(value_rank) ||
        value_rank <= target_rank)
  ) {
    stop(
      "cannot reassign `",
      name,
      "`: replacement mode ",
      value@mode,
      " differs from the declared mode ",
      target@mode,
      "; R would rebind `",
      name,
      "` to ",
      value@mode,
      call. = FALSE
    )
  }
  if (is.na(target_rank) || is.na(value_rank) || value_rank <= target_rank) {
    return()
  }
  stop(
    "cannot reassign `",
    name,
    "`: assignment would narrow ",
    value@mode,
    " to ",
    target@mode,
    "; R would promote `",
    name,
    "` to ",
    value@mode,
    call. = FALSE
  )
}

# Determine the promoted mode from a list of Fortran values.
# Used by: r2f-arithmetic.R, r2f-matrix.R
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

  ranks <- mode_rank(modes)
  if (!length(ranks) || all(is.na(ranks))) {
    NULL
  } else {
    mode_lattice[[max(ranks, na.rm = TRUE)]]
  }
}

# Create a Variable with conforming dimensions from multiple inputs.
# Used by: r2f-arithmetic.R, r2f-logical.R
infer_result_variable <- function(..., mode = NULL) {
  vars <- drop_nulls(list(...))
  # Report the promoted (lattice-join) mode: the emitted expression already
  # promotes (Fortran's rules match R for numeric mixes), and `<-` copies
  # the reported mode verbatim into the target's declaration, so reporting
  # the first non-scalar's mode declared truncating targets.
  mode <- mode %||% reduce_promoted_mode(vars)
  var <- NULL
  for (var in vars) {
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
