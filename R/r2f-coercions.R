# r2f-coercions.R
# Handlers for type coercions: as.double, as.integer

# --- Handlers ---

r2f_handlers[["as.double"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  maybe_cast_double(r2f(args[[1]], scope, ...))
}

r2f_handlers[["as.integer"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1L]], scope, ...)

  # R semantics:
  # - numeric -> integer truncates toward 0
  # - logical -> integer is 0/1
  # - result is an integer vector
  out_val <- Variable("integer", arg@value@dims)

  switch(
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
}
