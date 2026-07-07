# r2f-coercions.R
# Handlers for type coercions: as.double, as.integer, as.vector

# --- Handlers ---

r2f_handlers[["as.double"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ...)
  x <- maybe_cast_double(x)

  # R drops dimensions for as.double(<array>): the result is a vector.
  flatten_to_vector(x, scope)
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
      # expression preserves that storage (e.g. rev(m)), reuse it directly.
      if (logical_as_int(arg@value)) {
        Fortran(arg@value@name %||% as.character(arg), out_val)
      } else {
        arg <- booleanize_logical_as_int(arg)
        Fortran(glue("merge(1_c_int, 0_c_int, {arg})"), out_val)
      }
    },
    stop("as.integer() only implemented for logical, integer, and double")
  )

  # R drops dimensions for as.integer(<array>): the result is a vector.
  flatten_to_vector(out, scope)
}

r2f_handlers[["as.vector"]] <- function(args, scope = NULL, ...) {
  x_arg <- args$x %||% args[[1L]]
  if (is.null(x_arg) || is_missing(x_arg)) {
    stop("as.vector() expects `x`", call. = FALSE)
  }

  # `mode` selects the coercion; "any" (the default) preserves the type
  # and only drops dimensions. Numeric modes delegate to the dedicated
  # coercion handlers so the cast spellings stay in one place.
  mode_arg <- args$mode %||% if (length(args) >= 2L) args[[2L]] else NULL
  mode <- if (is.null(mode_arg) || is_missing(mode_arg)) {
    "any"
  } else if (is.character(mode_arg) && length(mode_arg) == 1L) {
    mode_arg
  } else {
    stop("as.vector() `mode` must be a string constant", call. = FALSE)
  }

  if (mode %in% c("double", "numeric")) {
    return(r2f(as.call(list(quote(as.double), x_arg)), scope, ...))
  }
  if (mode == "integer") {
    return(r2f(as.call(list(quote(as.integer), x_arg)), scope, ...))
  }
  if (!mode %in% c("any", "logical", "complex")) {
    stop(
      "as.vector() does not support mode = ",
      encodeString(mode, quote = "\""),
      call. = FALSE
    )
  }

  x <- r2f(x_arg, scope, ...)
  if (mode != "any" && !identical(x@value@mode, mode)) {
    stop(
      "as.vector(x, mode = ",
      encodeString(mode, quote = "\""),
      ") requires an operand already of that mode; casting is only ",
      "supported for numeric modes",
      call. = FALSE
    )
  }
  flatten_to_vector(x, scope)
}
