# r2f-coercions.R
# Handlers for type coercions: as.double, as.integer

# --- Handlers ---

r2f_handlers[["as.double"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ...)
  x <- maybe_cast_double(x)

  # R drops dimensions for as.double(<array>): the result is a vector.
  if (!passes_as_scalar(x@value) && x@value@rank > 1L) {
    len_expr <- value_length_expr(x@value)
    len_str <- if (is_scalar_na(len_expr)) {
      glue("size({x})")
    } else {
      # dims2f() returns "" for a scalar "1", but we need a literal length.
      out <- dims2f(list(len_expr), scope)
      if (!nzchar(out)) "1" else out
    }

    out_val <- Variable(
      "double",
      list(if (is_scalar_na(len_expr)) NA else len_expr)
    )
    return(Fortran(glue("reshape({x}, [{len_str}])"), out_val))
  }

  x
}

r2f_handlers[["as.integer"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1L]], scope, ...)

  # R semantics:
  # - numeric -> integer truncates toward 0
  # - logical -> integer is 0/1
  # - result is an integer vector
  out_val <- Variable("integer", arg@value@dims)

  out <- switch(
    arg@value@mode,
    integer = arg,
    double = Fortran(glue("int({arg}, kind=c_int)"), out_val),
    logical = {
      # External logicals are integer-backed (0/1/NA) under bind(c); if the
      # expression preserves that storage (e.g. rev(m)), return it directly.
      if (logical_as_int(arg@value)) {
        src <- arg@value@name %||% as.character(arg)
        return(Fortran(src, out_val))
      }
      arg <- booleanize_logical_as_int(arg)
      Fortran(glue("merge(1_c_int, 0_c_int, {arg})"), out_val)
    },
    stop("as.integer() only implemented for logical, integer, and double")
  )

  # R drops dimensions for as.integer(<array>): the result is a vector.
  if (!passes_as_scalar(out@value) && out@value@rank > 1L) {
    len_expr <- value_length_expr(out@value)
    len_str <- if (is_scalar_na(len_expr)) {
      glue("size({out})")
    } else {
      out_len <- dims2f(list(len_expr), scope)
      if (!nzchar(out_len)) "1" else out_len
    }
    out_val <- Variable(
      "integer",
      list(if (is_scalar_na(len_expr)) NA else len_expr)
    )
    return(Fortran(glue("reshape({out}, [{len_str}])"), out_val))
  }

  out
}
