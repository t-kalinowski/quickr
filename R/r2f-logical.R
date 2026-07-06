# r2f-logical.R
# Handlers for logical and comparison operators: !, &, |, >=, >, <, <=, ==, !=
# plus the scalar short-circuit forms && and || (compile_andor below).

# --- Handlers ---

# ---- comparison operators ----

r2f_handlers[[">="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} >= {right})"), var)
}

r2f_handlers[[">"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} > {right})"), var)
}

r2f_handlers[["<"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} < {right})"), var)
}

r2f_handlers[["<="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} <= {right})"), var)
}

r2f_handlers[["=="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} == {right})"), var)
}

r2f_handlers[["!="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} /= {right})"), var)
}

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


# ---- binary logical operators ----

# TODO: gfortran supports implicit casting that of logical to integer when
# assigning a logical to a variable declared integer, converting `.true.` to `1`,
# but this is not a standard language feature, and Intel's `ifort` uses `-1` for `.true`.
# We should explicitly use
#   `merge(1_c_int, 0_c_int, <lgl>)` to cast logical to int.
register_r2f_handler(
  c("&", "|"),
  function(args, scope, ..., hoist = NULL) {
    args <- lapply(args, r2f, scope, ..., hoist = hoist)
    args <- lapply(args, function(a) {
      if (a@value@mode != "logical") {
        stop("must be logical")
      }
      a
    })
    .[left, right] <- args
    left <- booleanize_logical_as_int(left)
    right <- booleanize_logical_as_int(right)
    .[left, right] <- maybe_reshape_vector_matrix(
      left,
      right,
      hoist,
      scope,
      scalarize_one_by_one = FALSE
    )

    operator <- switch(last(list(...)$calls), `&` = ".and.", `|` = ".or.")

    s <- glue("{left} {operator} {right}")
    val <- conform(left@value, right@value)
    val@mode <- "logical"
    Fortran(s, val)
  }
)

# ---- scalar short-circuit operators: && and || ----

# && and || are R's *scalar* control operators: operands must be length 1
# (R errors otherwise), and the right operand is evaluated only when the
# left side does not already decide the answer.
check_andor_operand <- function(x, op) {
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

# TRUE when evaluating `e` eagerly is indistinguishable from R's lazy
# right-operand evaluation: no side effects, no errors, no traps. A
# conservative whitelist -- names, literals, and compositions of pure
# non-trapping operations. Anything else (subscripts, %%/%/%, function
# calls, ...) gets the conditional lowering.
is_pure_scalar_condition <- function(e) {
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
  if (!op %in% pure_ops) {
    return(FALSE)
  }
  all(vapply(as.list(e)[-1L], is_pure_scalar_condition, logical(1L)))
}

compile_andor <- function(args, scope, ..., hoist = NULL) {
  op <- last(list(...)$calls)
  stopifnot(length(args) == 2L, op %in% c("&&", "||"))

  # R always evaluates the left operand: its hoists stay unconditional.
  left <- r2f(args[[1L]], scope, ..., hoist = hoist)
  check_andor_operand(left, op)
  left <- booleanize_logical_as_int(left)

  f <- if (op == "&&") ".and." else ".or."

  if (is_pure_scalar_condition(args[[2L]])) {
    # Fortran may evaluate both operands of .and./.or.; for a pure right
    # operand that is indistinguishable from short-circuiting, so keep
    # the compact infix form.
    right <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_andor_operand(right, op)
    right <- booleanize_logical_as_int(right)
    return(Fortran(glue("{left} {f} {right}"), Variable("logical")))
  }

  # The right operand can error or have side effects; R reaches it only
  # when the left side does not decide. Compile it into its own hoist and
  # emit everything inside the conditional.
  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }
  sub <- new_hoist(scope)
  right <- r2f(args[[2L]], scope, ..., hoist = sub)
  check_andor_operand(right, op)
  right <- booleanize_logical_as_int(right)

  tmp <- hoist$declare_tmp(mode = "logical", dims = NULL)
  hoist$emit(glue("{tmp@name} = {left}"))
  cond <- if (op == "&&") tmp@name else glue(".not. {tmp@name}")
  hoist$emit(glue("if ({cond}) then"))
  hoist$emit(indent(sub$render(glue("{tmp@name} = {right}"))))
  hoist$emit("end if")
  Fortran(tmp@name, tmp)
}

register_r2f_handler(c("&&", "||"), compile_andor)
