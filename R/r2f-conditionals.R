# r2f-conditionals.R
# Handlers for vectorized conditionals: ifelse

# --- Local Helpers ---

ifelse_branch_shape_msg <- paste0(
  "ifelse() `yes` and `no` must be scalars or match the shape of `test`; ",
  "R-style recycling is not supported"
)

# Enforce the shape contract for one ifelse() branch: scalars broadcast
# natively; a non-scalar branch must match `test`'s shape, because
# merge() requires conformable arguments and a runtime mismatch would
# read past the shorter branch. Per axis, guard_conformable_dims()
# applies the framework policy: statically unequal dims are a compile
# error; symbolic dims get a statement-level runtime size guard, emitted
# into `hoist` -- always a live hoist context, since r2f() substitutes a
# fresh one before dispatching to any handler.
check_ifelse_branch_shape <- function(branch, mask, hoist, scope) {
  if (passes_as_scalar(branch@value)) {
    return(invisible())
  }
  if (branch@value@rank != mask@value@rank) {
    stop(ifelse_branch_shape_msg, call. = FALSE)
  }
  for (axis in seq_len(mask@value@rank)) {
    guard_conformable_dims(
      dim_or_one(branch, axis),
      dim_or_one(mask, axis),
      ifelse_branch_shape_msg,
      hoist,
      scope,
      left = branch,
      right = mask,
      left_axis = axis,
      right_axis = axis
    )
  }
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
