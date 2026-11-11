# r2f-math.R
# Handlers for math intrinsics: sin, cos, tan, asin, acos, atan, sqrt, exp,
# log, tanh, floor, ceiling, trunc, log10, abs, Re, Im, Mod, Arg, Conj

# --- Local Helpers ---

# Keep the handler in a namespace binding so coverage can instrument it.
lower_unary_intrinsic <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1L]], scope, ..., hoist = hoist)
  intrinsic <- last(list(...)$calls)
  if (intrinsic %in% c("asin", "acos", "sqrt", "exp", "log")) {
    # Derived constants can fold to domain errors or overflow. Evaluate
    # at runtime both here and inside a branch that may not run.
    arg <- hoist_domain_operand(arg, hoist)
  }
  mode <- switch(
    intrinsic,
    Re = ,
    Im = ,
    Mod = ,
    Arg = "double",
    Conj = "complex",
    arg@value@mode
  )
  expr <- switch(
    intrinsic,
    # kind-less real() of an integer or double argument is single precision;
    # kind=c_double preserves precision for real and complex arguments.
    Re = glue("real({arg}, kind=c_double)"),
    Im = glue("aimag({arg})"),
    Mod = glue("abs({arg})"),
    Arg = glue("atan2(aimag({arg}), real({arg}))"),
    Conj = glue("conjg({arg})"),
    glue("{intrinsic}({arg})")
  )
  Fortran(expr, Variable(mode = mode, dims = arg@value@dims))
}


# --- Handlers ---

## real and complex intrinsics
register_r2f_handler(
  c(
    "sin",
    "cos",
    "tan",
    "tanh",
    "asin",
    "acos",
    "atan",
    "sqrt",
    "exp",
    "log",
    "Re",
    "Im",
    "Mod",
    "Arg",
    "Conj"
  ),
  lower_unary_intrinsic
)

r2f_handlers[["floor"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1L]], scope, ..., hoist = hoist)

  arg <- maybe_cast_double(arg)
  if (!identical(arg@value@mode, "double")) {
    stop_static_mode_error(
      "floor() only implemented for logical, integer, and double",
      hoist
    )
  }
  # `arg` is spliced into the emitted expression three times below; hoist
  # non-trivial expressions so side effects (e.g. RNG state) happen once.
  arg <- hoist_unless_name(arg, hoist)
  out_val <- Variable("double", arg@value@dims)

  # Avoid Fortran FLOOR() overflow (it returns an integer) by staying in
  # the real domain; real_floor_expr() shares the spelling with `%/%`.
  Fortran(real_floor_expr(arg), out_val)
}

r2f_handlers[["ceiling"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1L]], scope, ..., hoist = hoist)

  arg <- maybe_cast_double(arg)
  if (!identical(arg@value@mode, "double")) {
    stop_static_mode_error(
      "ceiling() only implemented for logical, integer, and double",
      hoist
    )
  }
  # `arg` is spliced into the emitted expression three times below; hoist
  # non-trivial expressions so side effects (e.g. RNG state) happen once.
  arg <- hoist_unless_name(arg, hoist)
  out_val <- Variable("double", arg@value@dims)

  # As with floor(): avoid integer overflow by implementing in real arithmetic.
  aint <- glue("aint({arg})")
  Fortran(
    glue("({aint} + merge(1.0_c_double, 0.0_c_double, ({arg} > {aint})))"),
    out_val
  )
}

r2f_handlers[["trunc"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1L]], scope, ..., hoist = hoist)

  # R's trunc() always returns a double and truncates toward 0.
  # - For double input we can use Fortran AINT(), which returns a real.
  # - For integer/logical inputs, a cast-to-double is sufficient.
  if (arg@value@mode == "double") {
    return(Fortran(glue("aint({arg})"), Variable("double", arg@value@dims)))
  }
  if (arg@value@mode %in% c("integer", "logical")) {
    return(maybe_cast_double(arg))
  }
  stop_static_mode_error(
    "trunc() only implemented for logical, integer, and double",
    hoist
  )
}

r2f_handlers[["log10"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ..., hoist = hoist)
  arg <- hoist_domain_operand(arg, hoist)
  f <- if (arg@value@mode == "complex") {
    glue("(log({arg}) / log(10.0_c_double))")
  } else {
    glue("log10({arg})")
  }
  Fortran(
    f,
    Variable(mode = arg@value@mode, dims = arg@value@dims)
  )
}

## accepts real, integer, or complex
r2f_handlers[["abs"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  # R: abs(logical) is integer (abs(TRUE) is 1L); Fortran's abs() requires
  # a numeric argument, so a logical operand participates as integer.
  arg <- cast_to_mode(arg, arith_join_mode(arg), "abs()")
  out_mode <- if (arg@value@mode == "complex") "double" else arg@value@mode
  Fortran(glue("abs({arg})"), Variable(mode = out_mode, dims = arg@value@dims))
}
