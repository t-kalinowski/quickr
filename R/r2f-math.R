# r2f-math.R
# Handlers for math intrinsics: sin, cos, tan, asin, acos, atan, sqrt, exp,
# log, floor, ceiling, log10, abs, Re, Im, Mod, Arg, Conj

# --- Local Helpers ---

# Register a unary intrinsic handler for one or more function names.
# Used by: math intrinsic registrations in this file
register_unary_intrinsic <- function(
  name,
  mode_fun,
  expr_fun
) {
  handler <- function(args, scope, ...) {
    stopifnot(length(args) == 1L)
    arg <- r2f(args[[1L]], scope, ...)
    val <- Variable(mode = mode_fun(arg), dims = arg@value@dims)
    Fortran(expr_fun(arg, last(list(...)$calls)), val)
  }
  register_r2f_handler(name, handler)
  invisible(handler)
}


# --- Handlers ---

## real and complex intrinsics
register_unary_intrinsic(
  c(
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sqrt",
    "exp",
    "log",
    "floor",
    "ceiling"
  ),
  mode_fun = function(arg) arg@value@mode,
  expr_fun = function(arg, intrinsic) glue("{intrinsic}({arg})")
)

r2f_handlers[["log10"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
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
  out_mode <- if (arg@value@mode == "complex") "double" else arg@value@mode
  Fortran(glue("abs({arg})"), Variable(mode = out_mode, dims = arg@value@dims))
}


# ---- complex elemental unary intrinsics ----

register_unary_intrinsic(
  "Re",
  mode_fun = function(arg) "double",
  expr_fun = function(arg, intrinsic) glue("real({arg})")
)

register_unary_intrinsic(
  "Im",
  mode_fun = function(arg) "double",
  expr_fun = function(arg, intrinsic) glue("aimag({arg})")
)

register_unary_intrinsic(
  "Mod",
  mode_fun = function(arg) "double",
  expr_fun = function(arg, intrinsic) glue("abs({arg})")
)

register_unary_intrinsic(
  "Arg",
  mode_fun = function(arg) "double",
  expr_fun = function(arg, intrinsic) glue("atan2(aimag({arg}), real({arg}))")
)

register_unary_intrinsic(
  "Conj",
  mode_fun = function(arg) "complex",
  expr_fun = function(arg, intrinsic) glue("conjg({arg})")
)
