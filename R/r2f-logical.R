# r2f-logical.R
# Handlers for comparison and logical operators, plus is.null().

# --- Handlers ---

# ---- unary logical not ----

r2f_handlers[["!"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ...)
  if (x@value@mode != "logical") {
    stop("'!' expects a logical value; numeric coercions not yet supported")
  }
  x <- booleanize_logical_as_int(x)
  Fortran(glue("(.not. {x})"), Variable("logical", x@value@dims))
}

register_r2f_handler(
  "is.null",
  function(args, scope, ...) {
    stopifnot(length(args) == 1L)
    arg <- args[[1L]]
    if (!is.symbol(arg)) {
      stop("is.null() is only supported on symbols", call. = FALSE)
    }
    var <- get0(as.character(arg), scope)
    if (!inherits(var, Variable) || is.null(var@optional_dummy)) {
      stop(
        "is.null() is only supported for optional arguments with NULL defaults",
        call. = FALSE
      )
    }
    Fortran(glue("(.not. present({var@optional_dummy}))"), Variable("logical"))
  }
)

lower_comparison_operands <- function(args, scope, op, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  if (
    op %in%
      c("<", "<=", ">", ">=") &&
      "complex" %in% c(left@value@mode, right@value@mode)
  ) {
    stop("invalid comparison with complex values", call. = FALSE)
  }
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  conform_elementwise_operands(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
}

r2f_handlers[["<"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "<",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} < {right})"), value)
}

r2f_handlers[["<="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "<=",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} <= {right})"), value)
}

r2f_handlers[[">"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    ">",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} > {right})"), value)
}

r2f_handlers[[">="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    ">=",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} >= {right})"), value)
}

r2f_handlers[["=="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "==",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} == {right})"), value)
}

r2f_handlers[["!="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "!=",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} /= {right})"), value)
}

lower_logical_operands <- function(args, scope, op, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  for (operand in list(left, right)) {
    if (operand@value@mode != "logical") {
      stop("`", op, "` requires logical operands", call. = FALSE)
    }
  }
  left <- booleanize_logical_as_int(left)
  right <- booleanize_logical_as_int(right)
  .[left, right] <- conform_elementwise_operands(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  list(left, right)
}

r2f_handlers[["&"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_logical_operands(
    args,
    scope,
    "&",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("{left} .and. {right}"), value)
}

r2f_handlers[["|"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_logical_operands(
    args,
    scope,
    "|",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("{left} .or. {right}"), value)
}

# && and || are scalar control operators. The right operand is conditionally
# lowered when eager evaluation could be observable.
check_short_circuit_operand <- function(x, op) {
  if (is.null(x@value) || !identical(x@value@mode, "logical")) {
    stop("`", op, "` requires logical operands", call. = FALSE)
  }
  if (!passes_as_scalar(x@value)) {
    stop(
      "`",
      op,
      "` requires length-1 operands; use `",
      if (op == "&&") "&" else "|",
      "` for elementwise operations",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

is_eager_safe_condition <- function(e) {
  if (is.symbol(e) || (is.atomic(e) && length(e) == 1L)) {
    return(TRUE)
  }
  if (!is.call(e) || !is.symbol(e[[1L]])) {
    return(FALSE)
  }
  op <- as.character(e[[1L]])
  pure_ops <- c(
    "(",
    "!",
    "&&",
    "||",
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
    "/",
    "abs"
  )
  op %in%
    pure_ops &&
    all(vapply(as.list(e)[-1L], is_eager_safe_condition, logical(1L)))
}

lower_short_circuit_operator <- function(args, scope, op, ..., hoist = NULL) {
  stopifnot(length(args) == 2L, op %in% c("&&", "||"))

  left <- r2f(args[[1L]], scope, ..., hoist = hoist)
  check_short_circuit_operand(left, op)
  left <- booleanize_logical_as_int(left)
  fortran_op <- if (op == "&&") ".and." else ".or."

  if (is_eager_safe_condition(args[[2L]])) {
    right <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_short_circuit_operand(right, op)
    right <- booleanize_logical_as_int(right)
    return(Fortran(glue("{left} {fortran_op} {right}"), Variable("logical")))
  }

  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }
  sub <- new_hoist(scope)
  right <- r2f(args[[2L]], scope, ..., hoist = sub)
  check_short_circuit_operand(right, op)
  right <- booleanize_logical_as_int(right)

  tmp <- hoist$declare_tmp(mode = "logical", dims = NULL)
  hoist$emit(glue("{tmp@name} = {left}"))
  condition <- if (op == "&&") tmp@name else glue(".not. {tmp@name}")
  hoist$emit(glue("if ({condition}) then"))
  hoist$emit(indent(sub$render(glue("{tmp@name} = {right}"))))
  hoist$emit("end if")
  Fortran(tmp@name, tmp)
}

r2f_handlers[["&&"]] <- function(args, scope, ..., hoist = NULL) {
  lower_short_circuit_operator(args, scope, "&&", ..., hoist = hoist)
}

r2f_handlers[["||"]] <- function(args, scope, ..., hoist = NULL) {
  lower_short_circuit_operator(args, scope, "||", ..., hoist = hoist)
}
