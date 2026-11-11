# r2f-logical.R
# Handlers for comparison and logical operators, plus is.null().

# ---- unary logical not ----

r2f_handlers[["!"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ..., hoist = hoist)
  if (x@value@mode != "logical") {
    stop_static_mode_error(
      "'!' expects a logical value; numeric coercions not yet supported",
      hoist
    )
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
    stop_static_mode_error("invalid comparison with complex values", hoist)
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
      stop_static_mode_error(
        paste0("`", op, "` requires logical operands"),
        hoist
      )
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

# ---- scalar short-circuit operators: && and || ----

# && and || are R's *scalar* control operators: operands must be length 1
# (R errors otherwise), and the right operand is evaluated only when the
# left side does not already decide the answer.
check_short_circuit_operand <- function(
  x,
  op,
  hoist,
  scope,
  defer_error = FALSE
) {
  if (is.null(x@value) && nzchar(trimws(as.character(x)))) {
    # Statement-only handlers such as print() cannot supply an operand.
    # Reject their use here before their emitted statements can be discarded.
    stop(
      "`",
      op,
      "` does not support statement-only operands; use a separate statement",
      call. = FALSE
    )
  }
  if (is.null(x@value) || !identical(x@value@mode, "logical")) {
    message <- paste0("`", op, "` requires logical operands")
    if (!defer_error) {
      stop(message, call. = FALSE)
    }
    emit_quickr_error_if(".true.", message, hoist, scope)
    return(Fortran(".false.", Variable("logical")))
  }

  message <- paste0(
    "`",
    op,
    "` requires length-1 operands; use `",
    if (op == "&&") "&" else "|",
    "` for elementwise operations"
  )
  if (passes_as_scalar(x@value)) {
    return(booleanize_logical_as_int(x))
  }

  dims <- lapply(x@value@dims, r2size, scope = scope)
  length_known_bad <- any(vapply(
    dims,
    \(dim) is_wholenumber(dim) && !dim_is_one(dim),
    logical(1L)
  ))
  if (length_known_bad) {
    if (!defer_error) {
      stop(message, call. = FALSE)
    }
    emit_quickr_error_if(".true.", message, hoist, scope)
  }
  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }

  if (isTRUE(x@logical_booleanized)) {
    tmp <- hoist$declare_tmp(mode = "logical", dims = x@value@dims)
    hoist$emit(glue("{tmp@name} = {x}"))
    x <- Fortran(tmp@name, tmp)
  } else {
    x <- hoist_unless_name(x, hoist)
  }

  if (
    !length_known_bad &&
      !all(vapply(dims, dim_is_one, logical(1L)))
  ) {
    emit_quickr_error_if(
      glue("size({x}, kind=c_ptrdiff_t) /= 1_c_ptrdiff_t"),
      message,
      hoist,
      scope
    )
  }
  idxs <- rep("1", x@value@rank)
  Fortran(
    glue("{x}({str_flatten_commas(idxs)})"),
    Variable("logical")
  )
}

# TRUE when evaluating `e` eagerly is indistinguishable from R's lazy
# right-operand evaluation: no side effects, no errors, no traps. A
# conservative whitelist -- names, literals, and compositions of pure
# non-trapping operations. Anything else (subscripts, %%/%/%, function
# calls, ...) gets the conditional lowering.
is_eager_safe_condition <- function(e, scope) {
  # Only these modes have supported promotions throughout the whitelist.
  # Other modes may fail during lowering even when the expression is pure.
  eager_modes <- c("logical", "integer", "double")
  if (is.symbol(e)) {
    var <- get0(as.character(e), scope)
    return(
      inherits(var, Variable) &&
        var@mode %in% eager_modes &&
        (passes_as_scalar(var) ||
          var@rank > 0L && all(vapply(var@dims, dim_is_one, logical(1L))))
    )
  }
  if (is.atomic(e) && length(e) == 1L) {
    return(
      typeof(e) %in% eager_modes && is.finite(e)
    )
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
    "abs"
  )
  if (!op %in% pure_ops) {
    return(FALSE)
  }
  if (inherits(scope[[op]], LocalClosure)) {
    return(FALSE)
  }
  allowed <- lazy_builtin_arities[[op]]
  if (!((length(e) - 1L) %in% allowed)) {
    return(FALSE)
  }
  if (
    !all(vapply(
      as.list(e)[-1L],
      is_eager_safe_condition,
      logical(1L),
      scope = scope
    ))
  ) {
    return(FALSE)
  }
  if (op %in% c("!", "&&", "||", "&", "|", "<", "<=", ">", ">=")) {
    return(is_statically_logical_condition(e, scope))
  }
  TRUE
}

is_statically_logical_condition <- function(e, scope) {
  if (is.symbol(e)) {
    var <- get0(as.character(e), scope)
    return(inherits(var, Variable) && identical(var@mode, "logical"))
  }
  if (is.atomic(e)) {
    return(is.logical(e) && length(e) == 1L)
  }
  if (!is.call(e) || !is.symbol(e[[1L]])) {
    return(FALSE)
  }
  op <- as.character(e[[1L]])
  args <- as.list(e)[-1L]
  if (op == "(" && length(args) == 1L) {
    return(is_statically_logical_condition(args[[1L]], scope))
  }
  if (op %in% c("==", "!=", "<", "<=", ">", ">=")) {
    return(TRUE)
  }
  if (op == "!" && length(args) == 1L) {
    return(is_statically_logical_condition(args[[1L]], scope))
  }
  if (op %in% c("&&", "||", "&", "|") && length(args) == 2L) {
    return(all(vapply(
      args,
      is_statically_logical_condition,
      logical(1L),
      scope = scope
    )))
  }
  FALSE
}

lazy_builtin_arities <- list(
  `(` = 1L,
  `$` = 2L,
  `!` = 1L,
  `&&` = 2L,
  `||` = 2L,
  `&` = 2L,
  `|` = 2L,
  `<` = 2L,
  `<=` = 2L,
  `>` = 2L,
  `>=` = 2L,
  `==` = 2L,
  `!=` = 2L,
  `+` = 1:2,
  `-` = 1:2,
  `*` = 2L,
  `/` = 2L,
  `^` = 2L,
  `%%` = 2L,
  `%/%` = 2L,
  `%*%` = 2L,
  `%o%` = 2L,
  `:` = 2L,
  sin = 1L,
  cos = 1L,
  tan = 1L,
  tanh = 1L,
  asin = 1L,
  acos = 1L,
  atan = 1L,
  sqrt = 1L,
  exp = 1L,
  log = 1L,
  floor = 1L,
  ceiling = 1L,
  trunc = 1L,
  log10 = 1L,
  abs = 1L,
  Re = 1L,
  Im = 1L,
  Mod = 1L,
  Arg = 1L,
  Conj = 1L,
  as.double = 1L,
  as.integer = 1L,
  array = 2:3,
  cat = 1L,
  chol2inv = 1L,
  dim = 1L,
  drop = 1L,
  is.null = 1L,
  ifelse = 3L,
  length = 1L,
  ncol = 1L,
  nrow = 1L,
  print = 1L,
  rep.int = 2L,
  rev = 1L,
  seq_along = 1L,
  seq_len = 1L,
  t = 1L,
  which.max = 1L,
  which.min = 1L
)

lazy_builtin_min_arities <- list(
  cbind = 1L,
  rbind = 1L,
  crossprod = 1L,
  tcrossprod = 1L,
  outer = 2L,
  solve = 1L,
  qr.solve = 2L,
  chol = 1L,
  forwardsolve = 2L,
  backsolve = 2L
)

lazy_builtin_arity_error <- function(e, scope, recursive = TRUE) {
  if (!is.call(e) || !is.symbol(e[[1L]])) {
    return(NULL)
  }

  op <- as.character(e[[1L]])
  allowed <- lazy_builtin_arities[[op]]
  minimum <- lazy_builtin_min_arities[[op]]
  is_builtin <- !inherits(scope[[op]], LocalClosure)
  if (
    is_builtin &&
      !is.null(minimum) &&
      length(e) - 1L < minimum
  ) {
    arity_words <- c("zero", "one", "two", "three")
    minimum_word <- arity_words[[minimum + 1L]]
    return(paste0(
      "`",
      op,
      "` requires at least ",
      minimum_word,
      " argument",
      if (minimum == 1L) "" else "s"
    ))
  }
  if (
    is_builtin &&
      !is.null(allowed) &&
      !((length(e) - 1L) %in% allowed)
  ) {
    arity_words <- c("zero", "one", "two", "three")
    allowed_words <- arity_words[allowed + 1L]
    expected <- if (length(allowed_words) == 1L) {
      paste0(
        "exactly ",
        allowed_words,
        " argument",
        if (allowed == 1L) "" else "s"
      )
    } else {
      paste0(str_flatten(allowed_words, " or "), " arguments")
    }
    return(paste0("`", op, "` requires ", expected))
  }
  if (!recursive) {
    return(NULL)
  }

  args <- as.list(e)[-1L]
  if (is_builtin && op %in% c("&&", "||") && length(args) == 2L) {
    # A nested scalar short-circuit owns its right operand. Only its left
    # operand is unconditionally evaluated when the nested call is reached.
    args <- args[1L]
  }
  for (arg in args) {
    error <- lazy_builtin_arity_error(arg, scope, recursive = TRUE)
    if (!is.null(error)) {
      return(error)
    }
  }
  NULL
}

# Bindings and compiler annotations must be separate statements. Function
# literals own their scopes, so their bodies do not affect this check.
has_current_scope_assignment <- function(e) {
  if (!is.call(e) || is_function_call(e)) {
    return(FALSE)
  }
  if (
    is.symbol(e[[1L]]) &&
      as.character(e[[1L]]) %in% c("<-", "=", "for", "declare")
  ) {
    return(TRUE)
  }
  any(vapply(as.list(e), has_current_scope_assignment, logical(1L)))
}

lower_short_circuit_operator <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  defer_andor_error = FALSE
) {
  op <- last(list(...)$calls)
  stopifnot(length(args) == 2L, op %in% c("&&", "||"))
  has_assignment <- vapply(args, has_current_scope_assignment, logical(1L))
  if (any(has_assignment)) {
    stop(
      "`",
      op,
      "` does not support assignment expressions or declarations; use separate statements",
      call. = FALSE
    )
  }

  # R always evaluates the left operand: its hoists stay unconditional.
  left <- r2f(
    args[[1L]],
    scope,
    ...,
    hoist = hoist,
    defer_andor_error = defer_andor_error
  )
  left <- check_short_circuit_operand(
    left,
    op,
    hoist,
    scope,
    defer_error = defer_andor_error
  )

  f <- if (op == "&&") ".and." else ".or."
  rhs_arity_error <- lazy_builtin_arity_error(args[[2L]], scope)

  if (
    is.null(rhs_arity_error) &&
      is_eager_safe_condition(args[[2L]], scope) &&
      is_statically_logical_condition(args[[2L]], scope)
  ) {
    # Fortran may evaluate both operands of .and./.or.; for a pure right
    # operand that is indistinguishable from short-circuiting, so keep
    # the compact infix form.
    right <- r2f(
      args[[2L]],
      scope,
      ...,
      hoist = hoist,
      defer_andor_error = defer_andor_error
    )
    right <- check_short_circuit_operand(
      right,
      op,
      hoist,
      scope,
      defer_error = defer_andor_error
    )
    return(Fortran(glue("{left} {f} {right}"), Variable("logical")))
  }

  # The right operand can error or have side effects; R reaches it only
  # when the left side does not decide. Compile it into its own hoist and
  # emit everything inside the conditional.
  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }
  # The result must remain visible outside the nested right-operand block.
  # Declare it in the procedure scope so block-local temporaries cannot
  # shadow it.
  tmp <- scope_unique_var(scope, mode = "logical", dims = NULL)
  register_openmp_private(scope, tmp@name)
  sub <- new_hoist(scope)
  sub$defer_static_shape_error <- TRUE
  sub$defer_builtin_arity_error <- TRUE
  sub$defer_static_mode_error <- TRUE
  deferred_error <- NULL
  right <- tryCatch(
    r2f(
      args[[2L]],
      scope,
      ...,
      hoist = sub,
      defer_andor_error = TRUE
    ),
    quickr_deferred_branch_error = function(error) {
      deferred_error <<- conditionMessage(error)
      NULL
    },
    error = function(error) {
      deferred_error <<- conditionMessage(error)
      NULL
    }
  )
  if (is.null(deferred_error)) {
    right <- check_short_circuit_operand(
      right,
      op,
      sub,
      scope,
      defer_error = TRUE
    )
  } else {
    emit_quickr_error_if(".true.", deferred_error, sub, scope)
    right <- Fortran(".false.", Variable("logical"))
  }

  hoist$emit(glue("{tmp@name} = {left}"))
  cond <- if (op == "&&") tmp@name else glue(".not. {tmp@name}")
  hoist$emit(glue("if ({cond}) then"))
  hoist$emit(indent(sub$render(glue("{tmp@name} = {right}"))))
  hoist$emit("end if")
  Fortran(tmp@name, tmp)
}

register_r2f_handler(c("&&", "||"), lower_short_circuit_operator)
