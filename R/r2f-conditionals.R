# r2f-conditionals.R
# Handlers for vectorized conditionals: ifelse

# --- Local Helpers ---

ifelse_branch_shape_msg <- paste0(
  "ifelse() `yes` and `no` must be scalars or match the shape of `test`; ",
  "R-style recycling is not supported"
)

# Three-valued conformability verdict for one axis of an ifelse() branch
# against `test`: ok+known (no guard), not-ok+known (compile error), or
# unknown (runtime guard). NA dims are always unknown: two unknown lengths
# are not the same quantity.
ifelse_axis_verdict <- function(test_dim, branch_dim) {
  if (is_wholenumber(test_dim) && is_wholenumber(branch_dim)) {
    return(list(
      ok = identical(as.integer(test_dim), as.integer(branch_dim)),
      unknown = FALSE
    ))
  }
  if (!is_scalar_na(test_dim) && !is_scalar_na(branch_dim)) {
    test_norm <- fortranize_expr_symbols(test_dim)
    branch_norm <- fortranize_expr_symbols(branch_dim)
    if (identical(test_norm, branch_norm)) {
      return(list(ok = TRUE, unknown = FALSE))
    }
  }
  list(ok = TRUE, unknown = TRUE)
}

# Enforce the shape contract for one ifelse() branch: scalars broadcast
# natively; a non-scalar branch must match `test`'s shape, because
# merge() requires conformable arguments and a runtime mismatch would
# read past the shorter branch. Statically unequal dims are a compile
# error; symbolic dims get a statement-level runtime size guard.
check_ifelse_branch_shape <- function(branch, mask, hoist, scope) {
  if (passes_as_scalar(branch@value)) {
    return(invisible())
  }
  if (branch@value@rank != mask@value@rank) {
    stop(ifelse_branch_shape_msg, call. = FALSE)
  }
  unknown_axes <- integer()
  for (axis in seq_len(mask@value@rank)) {
    verdict <- ifelse_axis_verdict(
      dim_or_one(mask, axis),
      dim_or_one(branch, axis)
    )
    if (!verdict$ok) {
      stop(ifelse_branch_shape_msg, call. = FALSE)
    }
    if (verdict$unknown) {
      unknown_axes <- c(unknown_axes, axis)
    }
  }
  if (!length(unknown_axes)) {
    return(invisible())
  }
  if (is.null(hoist)) {
    stop(
      "cannot emit a runtime length guard here; ",
      "ifelse() branch lengths must match `test` statically",
      call. = FALSE
    )
  }
  # size() is an inquiry, so applying it to operand expression text does
  # not evaluate the operands.
  condition <- str_flatten(
    map_chr(
      unknown_axes,
      function(axis) glue("size({branch}, {axis}) /= size({mask}, {axis})")
    ),
    " .or. "
  )
  emit_quickr_error_if(condition, ifelse_branch_shape_msg, hoist, scope)
  invisible()
}

# --- Handlers ---

r2f_handlers[["ifelse"]] <- function(args, scope, ..., hoist = NULL) {
  .[mask, tsource, fsource] <- lapply(args, r2f, scope, ..., hoist = hoist)

  # R: the result is shaped like `test` (branches only contribute values).
  # A scalar test with array branches is not representable with merge().
  if (
    passes_as_scalar(mask@value) &&
      !(passes_as_scalar(tsource@value) && passes_as_scalar(fsource@value))
  ) {
    stop(
      "ifelse() result takes the shape of `test`; ",
      "array-valued yes/no with scalar test is not supported",
      call. = FALSE
    )
  }

  # Checked before casts so guards splice the bare operand text.
  check_ifelse_branch_shape(tsource, mask, hoist, scope)
  check_ifelse_branch_shape(fsource, mask, hoist, scope)

  mask <- booleanize_logical_as_int(mask)

  # merge() requires same-typed branches; promote both to their common mode.
  promoted <- promote_operands(list(tsource, fsource), context = "ifelse()")
  .[tsource, fsource] <- promoted$args
  mode <- promoted$mode

  Fortran(
    glue("merge({tsource}, {fsource}, {mask})"),
    Variable(mode, mask@value@dims)
  )
}
