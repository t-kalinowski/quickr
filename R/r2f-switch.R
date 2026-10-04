# r2f-switch.R
# Handler for numeric switch(): lowered to a Fortran SELECT CASE.
#
# R semantics (numeric EXPR):
#   - EXPR is coerced to integer (doubles truncate, TRUE is 1) and selects the
#     alternative at that position; names are ignored
#   - an out-of-range EXPR (or NA) returns NULL invisibly
#   - an empty alternative is an error
#   - break/next inside an alternative apply to the enclosing loop
#
# Used as a statement, each alternative compiles like an if() branch and an
# out-of-range EXPR does nothing. Used as a value, every alternative must
# give the same type and shape, and an out-of-range EXPR is a runtime error
# (quickr cannot return NULL). Each alternative is compiled once, so large
# switches (e.g. opcode dispatch) translate in linear time.

# --- Local Helpers ---

switch_shape_msg <- "all switch() alternatives must have the same shape"

switch_out_of_range_msg <- paste0(
  "switch() index is out of range; ",
  "R would return NULL, which quickr cannot return"
)

# Positions of the alternatives among switch() arguments: everything except
# EXPR, which is matched by name or else taken as the first argument.
# Used by: switch_split_args(), check_return_positions()
switch_alternative_indices <- function(args) {
  arg_names <- names(args) %||% rep("", length(args))
  expr_index <- match("EXPR", arg_names)
  if (is.na(expr_index)) {
    expr_index <- 1L
  }
  setdiff(seq_along(args), expr_index)
}

# Split switch() arguments into the EXPR expression and the alternatives.
switch_split_args <- function(args) {
  if (!length(args)) {
    stop("switch() requires an EXPR argument", call. = FALSE)
  }
  alternative_indices <- switch_alternative_indices(args)
  expr_index <- setdiff(seq_along(args), alternative_indices)
  alternatives <- args[alternative_indices]
  if (!length(alternatives)) {
    stop("switch() needs at least one alternative", call. = FALSE)
  }
  empty <- map_lgl(alternatives, function(alt) identical(alt, quote(expr = )))
  if (any(empty)) {
    stop("empty alternative in numeric switch", call. = FALSE)
  }
  list(expr = args[[expr_index]], alternatives = unname(alternatives))
}

# Lower EXPR to a scalar integer selector, coercing like R.
switch_selector <- function(expr, scope, ..., hoist) {
  if (is.character(expr)) {
    stop(
      "only numeric switch() is supported; quickr has no character values",
      call. = FALSE
    )
  }
  sel <- r2f(expr, scope, ..., hoist = hoist)
  mode <- if (inherits(sel@value, Variable)) sel@value@mode
  if (is.null(mode)) {
    stop(
      "switch() EXPR must be a numeric value: ",
      deparse1(expr),
      call. = FALSE
    )
  }
  if (!passes_as_scalar(sel@value)) {
    stop("switch() EXPR must be a length 1 vector", call. = FALSE)
  }
  switch(
    mode,
    integer = sel,
    double = Fortran(glue("int({sel}, kind=c_int)"), Variable("integer")),
    logical = cast_to_mode(sel, "integer", "switch()"),
    stop(
      "only numeric switch() is supported; EXPR is ",
      mode,
      call. = FALSE
    )
  )
}

# Check an alternative's value against the result variable defined by the
# first alternative: same type, same shape (a runtime guard, emitted inside
# the alternative's case block, where lengths are only known at run time).
switch_check_alternative <- function(k, result, value, sub, scope) {
  val <- value@value
  if (!identical(result@mode, val@mode)) {
    stop(
      "all switch() alternatives must give the same type; alternative ",
      k,
      " is ",
      val@mode,
      " where alternative 1 is ",
      result@mode,
      call. = FALSE
    )
  }
  result_scalar <- passes_as_scalar(result)
  if (
    result_scalar != passes_as_scalar(val) ||
      (!result_scalar && result@rank != val@rank)
  ) {
    stop(switch_shape_msg, call. = FALSE)
  }
  if (result_scalar) {
    return(invisible())
  }
  for (axis in seq_len(result@rank)) {
    guard_conformable_dims(
      var_dim_or_one(result, axis),
      dim_or_one(value, axis),
      switch_shape_msg,
      sub,
      scope,
      left = Fortran(result@name, result),
      right = value,
      left_axis = axis,
      right_axis = axis,
      checker = check_equal_dims
    )
  }
  invisible()
}

switch_select_block <- function(selector, cases) {
  str_flatten_lines(
    glue("select case ({selector})"),
    cases,
    "end select"
  )
}

# switch() as a statement: each alternative compiles like an if() branch.
switch_statement <- function(selector, alternatives, scope, ...) {
  cases <- vapply(
    seq_along(alternatives),
    function(k) {
      body <- r2f(alternatives[[k]], scope, ..., hoist = NULL)
      check_pending_parallel_consumed(scope)
      str_flatten_lines(glue("case ({k})"), indent(body))
    },
    ""
  )
  Fortran(switch_select_block(selector, cases))
}

# switch() as a value: compile each alternative into its own block and
# assign a result temporary; an out-of-range index is a runtime error.
switch_value <- function(selector, alternatives, scope, ..., hoist) {
  lowered <- lapply(alternatives, function(alt) {
    sub <- hoist$capture_block()
    value <- r2f(alt, scope, ..., hoist = sub)
    if (!inherits(value@value, Variable) || is.null(value@value@mode)) {
      stop(
        "switch() alternative does not produce a value: ",
        deparse1(alt),
        call. = FALSE
      )
    }
    if (identical(value@value@mode, "logical")) {
      value <- booleanize_logical_as_int(value)
    }
    list(value = value, hoist = sub)
  })

  first <- lowered[[1L]]$value@value
  result <- hoist$declare_tmp(mode = first@mode, dims = first@dims)

  cases <- vapply(
    seq_along(lowered),
    function(k) {
      value <- lowered[[k]]$value
      sub <- lowered[[k]]$hoist
      if (k > 1L) {
        switch_check_alternative(k, result, value, sub, scope)
      }
      code <- sub$render(glue("{result@name} = {value}"))
      str_flatten_lines(glue("case ({k})"), indent(code))
    },
    ""
  )

  mark_scope_uses_errors(scope)
  hoist$mark_runtime_guard()
  default_case <- str_flatten_lines(
    "case default",
    indent(str_flatten_lines(
      quickr_error_fortran_lines(switch_out_of_range_msg, scope = scope)
    ))
  )
  hoist$emit(switch_select_block(selector, c(cases, default_case)))
  Fortran(result@name, result)
}


# --- Handlers ---

r2f_switch <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  needs_value = TRUE
) {
  .[expr, alternatives] <- switch_split_args(args)
  selector <- switch_selector(expr, scope, ..., hoist = hoist)
  if (isTRUE(needs_value)) {
    switch_value(selector, alternatives, scope, ..., hoist = hoist)
  } else {
    switch_statement(selector, alternatives, scope, ...)
  }
}

register_r2f_handler(
  "switch",
  r2f_switch,
  match_fun = FALSE,
  needs_value = TRUE
)
