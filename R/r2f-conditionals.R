# r2f-conditionals.R
# Handlers for vectorized conditionals: ifelse

# --- Local Helpers ---

ifelse_branch_shape_msg <- paste0(
  "ifelse() `yes` and `no` must be scalars or match the shape of `test`; ",
  "R-style recycling is not supported"
)

# Conservative classifier for expressions whose evaluation timing is not
# observable. It lets ifelse() keep safe branches inline and local closures
# accept actual values without pretending to implement R promise semantics.
r2f_expression_is_pure <- function(e, scope) {
  if (is.null(e) || identical(e, quote(NULL))) {
    return(TRUE)
  }
  if (is.symbol(e)) {
    var <- get0(as.character(e), scope)
    return(!inherits(var, Variable) || is.null(var@optional_dummy))
  }
  if (is.atomic(e)) {
    return(TRUE)
  }
  if (!is.call(e) || !is.symbol(e[[1L]])) {
    return(FALSE)
  }
  op <- as.character(e[[1L]])
  if (
    !op %in%
      c(
        "(",
        "!",
        "&",
        "|",
        "<",
        "<=",
        ">",
        ">=",
        "==",
        "!=",
        "+",
        "-",
        "*",
        "abs"
      )
  ) {
    return(FALSE)
  }
  if (inherits(get0(op, scope), LocalClosure)) {
    return(FALSE)
  }
  all(vapply(as.list(e)[-1L], r2f_expression_is_pure, logical(1L), scope))
}

ifelse_branch_shape_is_known <- function(branch, mask) {
  if (
    passes_as_scalar(branch@value) ||
      branch@value@rank != mask@value@rank
  ) {
    return(TRUE)
  }
  all(vapply(
    seq_len(mask@value@rank),
    function(axis) {
      !check_equal_dims(
        dim_or_one(mask, axis),
        dim_or_one(branch, axis)
      )$unknown
    },
    logical(1L)
  ))
}

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
      right_axis = axis,
      checker = check_equal_dims
    )
  }
  invisible()
}

# --- Handlers ---

r2f_handlers[["ifelse"]] <- function(args, scope, ..., hoist = NULL) {
  mask <- lower_operands_in_order(
    args[1L],
    scope,
    ...,
    hoist = hoist
  )[[1L]]
  mask_code <- trimws(as.character(mask))
  if (is.null(mask@value@name) || !identical(mask_code, mask@value@name)) {
    # Keep the selector in the parent statement block. Branch captures share
    # this declaration scope, so their temporary names cannot shadow it.
    mask_tmp <- hoist$declare_tmp(
      mode = mask@value@mode,
      dims = mask@value@dims,
      logical_as_int = logical_as_int(mask@value) &&
        !isTRUE(mask@logical_booleanized)
    )
    hoist$emit(glue("{mask_tmp@name} = {mask}"))
    mask <- Fortran(mask_tmp@name, mask_tmp)
  }

  # Known limitation: static diagnostics in both branches are checked during
  # lowering, even when R would skip a branch. Deferring these errors is separate
  # work from scalar &&/|| lowering.
  lower_branch <- function(arg) {
    sub <- hoist$capture_block()
    branch <- r2f(arg, scope, ..., hoist = sub)
    if (
      !passes_as_scalar(mask@value) &&
        (!r2f_expression_is_pure(arg, scope) ||
          !ifelse_branch_shape_is_known(branch, mask))
    ) {
      # WHERE may evaluate only selected RHS elements. Materialize a branch
      # when full evaluation is observable or a runtime shape guard needs its
      # actual extent.
      branch <- hoist_unless_name(branch, sub)
    }
    list(value = branch, hoist = sub)
  }

  yes <- lower_branch(args[[2L]])
  no <- lower_branch(args[[3L]])
  tsource <- yes$value
  fsource <- no$value

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

  # Checked before casts so guards splice the bare operand text. Keep each
  # guard with its branch because an unselected branch is not evaluated by R.
  check_ifelse_branch_shape(tsource, mask, yes$hoist, scope)
  check_ifelse_branch_shape(fsource, mask, no$hoist, scope)

  mask <- booleanize_logical_as_int(mask)

  # Both branches share one promoted result mode and the selector's shape.
  # Known limitation: constant selectors do not retain the selected branch's
  # mode, and branch recycling or scalar tests with array branches are deferred.
  promoted <- promote_operands(list(tsource, fsource), context = "ifelse()")
  .[tsource, fsource] <- promoted$args
  mode <- promoted$mode
  result <- hoist$declare_tmp(mode = mode, dims = mask@value@dims)

  if (passes_as_scalar(mask@value)) {
    hoist$emit(glue("if ({mask}) then"))
    hoist$emit(indent(yes$hoist$render(glue("{result@name} = {tsource}"))))
    hoist$emit("else")
    hoist$emit(indent(no$hoist$render(glue("{result@name} = {fsource}"))))
    hoist$emit("end if")
  } else {
    selectors <- list(mask, glue(".not. {mask}"))
    branches <- list(tsource, fsource)
    branch_hoists <- list(yes$hoist, no$hoist)
    for (i in seq_along(branches)) {
      selector <- selectors[[i]]
      hoist$emit(glue("if (any({selector})) then"))
      assignment <- c(
        hoist$allocation_guard_at_point(result),
        glue("where ({selector}) {result@name} = {branches[[i]]}")
      )
      hoist$emit(indent(branch_hoists[[i]]$render(assignment)))
      hoist$emit("end if")
    }
  }

  Fortran(result@name, result)
}
