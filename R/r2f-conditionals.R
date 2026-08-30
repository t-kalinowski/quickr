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
    if (isTRUE(hoist$defer_static_shape_error)) {
      stop_deferred_branch_error(ifelse_branch_shape_msg)
    }
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
  mask <- lower_r2f_operand_in_order(args[[1L]], scope, ..., hoist = hoist)
  mask_code <- trimws(as.character(mask))
  if (
    !passes_as_scalar(mask@value) ||
      is.null(mask@value@name) ||
      !identical(mask_code, mask@value@name)
  ) {
    # Branch hoists introduce nested block scopes whose temporary names can
    # repeat. Keep the selector in procedure scope so it cannot be shadowed.
    mask_tmp <- scope_unique_var(
      scope,
      mode = mask@value@mode,
      dims = mask@value@dims,
      logical_as_int = logical_as_int(mask@value) &&
        !isTRUE(mask@logical_booleanized)
    )
    register_openmp_private(scope, mask_tmp@name)
    hoist$emit(glue("{mask_tmp@name} = {mask}"))
    mask <- Fortran(mask_tmp@name, mask_tmp)
  }
  result_allocation_dims <- lapply(
    seq_len(mask@value@rank),
    function(axis) call("[", call("dim", as.name(mask@value@name)), axis)
  )

  lower_branch <- function(arg) {
    sub <- new_hoist(scope)
    sub$defer_static_shape_error <- TRUE
    sub$defer_builtin_arity_error <- TRUE
    sub$defer_static_mode_error <- TRUE
    deferred_error <- NULL
    branch <- tryCatch(
      {
        branch <- r2f(arg, scope, ..., hoist = sub)
        if (!inherits(branch@value, Variable)) {
          stop_deferred_branch_error(
            "ifelse() branches must produce a value"
          )
        }
        if (
          !passes_as_scalar(mask@value) &&
            (!r2f_expression_is_pure(arg, scope) ||
              !ifelse_branch_shape_is_known(branch, mask))
        ) {
          # WHERE may evaluate only selected RHS elements. Materialize a branch
          # when full evaluation is observable or a runtime shape guard needs its
          # actual extent.
          branch <- hoist_unless_name(
            branch,
            sub,
            allocate_at_point = TRUE
          )
        }
        if (!passes_as_scalar(mask@value)) {
          check_ifelse_branch_shape(branch, mask, sub, scope)
        }
        branch
      },
      quickr_deferred_branch_error = function(error) {
        deferred_error <<- conditionMessage(error)
        NULL
      },
      error = function(error) {
        deferred_error <<- conditionMessage(error)
        NULL
      }
    )
    if (!is.null(deferred_error)) {
      emit_quickr_error_if(".true.", deferred_error, sub, scope)
      return(list(value = NULL, hoist = sub))
    }
    list(value = branch, hoist = sub)
  }

  yes <- lower_branch(args[[2L]])
  no <- lower_branch(args[[3L]])
  value <- yes$value %||% no$value
  mode <- if (inherits(value, Fortran)) value@value@mode else "logical"
  error_placeholder <- atomic2Fortran(switch(
    mode,
    logical = FALSE,
    integer = 0L,
    double = 0,
    complex = 0 + 0i
  ))
  if (is.null(yes$value)) {
    yes$value <- error_placeholder
  }
  if (is.null(no$value)) {
    no$value <- error_placeholder
  }
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

  mask <- booleanize_logical_as_int(mask)

  # Assign both branches into one result, promoting them to a common mode.
  promoted <- promote_operands(list(tsource, fsource), context = "ifelse()")
  .[tsource, fsource] <- promoted$args
  mode <- promoted$mode
  result <- scope_unique_var(scope, mode = mode, dims = mask@value@dims)
  register_openmp_private(scope, result@name)

  if (passes_as_scalar(mask@value)) {
    hoist$emit(glue("if ({mask}) then"))
    hoist$emit(indent(yes$hoist$render(glue("{result@name} = {tsource}"))))
    hoist$emit("else")
    hoist$emit(indent(no$hoist$render(glue("{result@name} = {fsource}"))))
    hoist$emit("end if")
  } else {
    allocate_reusable_local_output_at_point(
      result,
      scope,
      hoist,
      dims = result_allocation_dims
    )
    selectors <- list(mask, glue(".not. {mask}"))
    branches <- list(tsource, fsource)
    branch_hoists <- list(yes$hoist, no$hoist)
    for (i in seq_along(branches)) {
      selector <- selectors[[i]]
      hoist$emit(glue("if (any({selector})) then"))
      assignment <- glue(
        "where ({selector}) {result@name} = {branches[[i]]}"
      )
      hoist$emit(indent(branch_hoists[[i]]$render(assignment)))
      hoist$emit("end if")
    }
  }

  Fortran(result@name, result)
}
