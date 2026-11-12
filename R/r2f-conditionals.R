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

# Booleanizing a bind(c) logical preserves its storage shape. Use that storage
# when proving dimensions, before the selector becomes a temporary array.
ifelse_shape_operand <- function(x) {
  if (isTRUE(x@logical_booleanized) && !is.null(x@value@name)) {
    Fortran(x@value@name, x@value)
  } else {
    x
  }
}

ifelse_branch_shape_is_known <- function(branch, mask, scope) {
  if (
    passes_as_scalar(branch@value) ||
      branch@value@rank != mask@value@rank
  ) {
    return(TRUE)
  }
  branch <- ifelse_shape_operand(branch)
  mask <- ifelse_shape_operand(mask)
  all(vapply(
    seq_len(mask@value@rank),
    function(axis) {
      mask_dim <- dim_or_one(mask, axis)
      branch_dim <- dim_or_one(branch, axis)
      if (!check_equal_dims(mask_dim, branch_dim)$unknown) {
        return(TRUE)
      }
      if (
        identical(
          dimension_guard_expr(mask_dim, mask, axis),
          dimension_guard_expr(branch_dim, branch, axis)
        )
      ) {
        return(TRUE)
      }
      mask_input <- input_dimension(mask_dim, mask, axis, scope)
      branch_input <- input_dimension(branch_dim, branch, axis, scope)
      !is.null(mask_input) && identical(mask_input, branch_input)
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
  branch <- ifelse_shape_operand(branch)
  mask <- ifelse_shape_operand(mask)
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
  # Known issue: branch bindings can leak into the shared compiler scope.
  # Keep assignments and declarations separate; branch scope isolation is deferred.
  mask <- lower_operands_in_order(
    args[1L],
    scope,
    ...,
    hoist = hoist
  )[[1L]]

  # Known limitation: static diagnostics in both branches are checked during
  # lowering, even when R would skip a branch. Deferring these errors is separate
  # work from scalar &&/|| lowering.
  lower_branch <- function(arg) {
    sub <- hoist$capture_block()
    branch <- r2f(arg, scope, ..., hoist = sub)
    list(
      value = branch,
      hoist = sub,
      pure = r2f_expression_is_pure(arg, scope)
    )
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

  # Pure branches with proven shapes need no conditional work. Keeping the
  # MERGE expression inline lets the enclosing assignment write its destination
  # directly, without mask/result allocations or ANY/WHERE passes. An empty
  # hoist also excludes arithmetic that can signal a runtime shape error.
  mergeable <- identical(mask@value@mode, "logical") &&
    all(vapply(
      list(yes, no),
      function(branch) {
        branch$pure &&
          branch$hoist$is_empty() &&
          ifelse_branch_shape_is_known(branch$value, mask, scope)
      },
      logical(1L)
    ))
  if (mergeable) {
    check_ifelse_branch_shape(tsource, mask, yes$hoist, scope)
    check_ifelse_branch_shape(fsource, mask, no$hoist, scope)
    promoted <- promote_operands(list(tsource, fsource), context = "ifelse()")
    .[tsource, fsource] <- promoted$args
    mask <- booleanize_logical_as_int(mask)
    return(Fortran(
      glue("merge({tsource}, {fsource}, {mask})"),
      Variable(promoted$mode, mask@value@dims)
    ))
  }

  mask_code <- trimws(as.character(mask))
  # Known limitation: a branch can mutate a named mask or its symbolic
  # extents through a local closure. Freezing both before branch evaluation
  # requires broader ifelse() allocation changes and remains deferred.
  if (is.null(mask@value@name) || !identical(mask_code, mask@value@name)) {
    # Keep the selector in the parent statement block. Nested block hoists
    # share its temporary-name counter, including branches already lowered.
    mask_tmp <- hoist$declare_tmp(
      mode = mask@value@mode,
      dims = mask@value@dims,
      logical_as_int = logical_as_int(mask@value) &&
        !isTRUE(mask@logical_booleanized)
    )
    hoist$emit(glue("{mask_tmp@name} = {mask}"))
    mask <- Fortran(mask_tmp@name, mask_tmp)
  }

  materialize_branch <- function(branch) {
    if (
      !passes_as_scalar(mask@value) &&
        (!branch$pure ||
          !ifelse_branch_shape_is_known(branch$value, mask, scope))
    ) {
      # WHERE may evaluate only selected RHS elements. Materialize a branch
      # when full evaluation is observable or a runtime shape guard needs its
      # actual extent.
      branch$value <- hoist_unless_name(branch$value, branch$hoist)
    }
    branch
  }
  yes <- materialize_branch(yes)
  no <- materialize_branch(no)
  tsource <- yes$value
  fsource <- no$value

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
    # Neither branch runs for an empty selector. The result still needs valid
    # zero-length storage before an enclosing expression or assignment uses it.
    hoist$emit(hoist$allocation_guard_at_point(result))
  }

  Fortran(result@name, result)
}
