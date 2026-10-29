# r2f-dimensions.R
# Handlers for dimension queries: length, nrow, ncol, dim

# --- Handlers ---

r2f_handlers[["length"]] <- function(args, scope, hoist = NULL, ...) {
  x <- r2f(args[[1]], scope, hoist = hoist, ...)
  if (passes_as_scalar(x@value) && !renders_as_array_constructor(x)) {
    # The result is known, but evaluating x can still have side effects.
    hoist_unless_name(x, hoist)
    return(Fortran("1_c_int", Variable("integer")))
  }
  Fortran(glue("size({x})"), Variable("integer"))
}

r2f_handlers[["nrow"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x}, 1)"), Variable("integer"))
}

r2f_handlers[["ncol"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x}, 2)"), Variable("integer"))
}

r2f_handlers[["dim"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("shape({x})"), Variable("integer", x@value@rank))
}
