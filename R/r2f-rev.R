# r2f-rev.R
# Handler for rev()

r2f_handlers[["rev"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)

  x <- r2f(args[[1L]], scope, ..., hoist = hoist)
  if (is.null(x@value)) {
    stop("rev() expects a typed value", call. = FALSE)
  }

  # Scalars (incl. length-1 vectors that are lowered as scalars) reverse to self.
  if (passes_as_scalar(x@value)) {
    return(x)
  }

  if (x@value@rank != 1L) {
    stop("rev() only supports rank 0-1 inputs", call. = FALSE)
  }

  # Fortran array sections require an array designator; hoist array expressions.
  if (is.null(x@value@name)) {
    tmp <- hoist$declare_tmp(
      mode = x@value@mode,
      dims = x@value@dims,
      logical_as_int = logical_as_int(x@value)
    )
    hoist$emit(glue("{tmp@name} = {x}"))
    x <- Fortran(tmp@name, tmp)
  }

  base_name <- x@value@name %||%
    stop("missing array name for rev()", call. = FALSE)

  # External logical args are stored as integer(0/1) and symbol-lowered as `(x/=0)`.
  # Reverse the underlying storage (including NA_LOGICAL sentinel values).
  #
  # Note: this returns integer storage (0/1/NA) for bind(c) logicals, which
  # preserves NA when the reversed value is returned back to R.
  if (identical(x@value@mode, "logical") && logical_as_int(x@value)) {
    out_val <- Variable("logical", x@value@dims)
    out_val@logical_as_int <- TRUE
    return(Fortran(
      glue("{base_name}(size({base_name}):1:-1)"),
      out_val
    ))
  }

  out_val <- Variable(x@value@mode, x@value@dims)
  Fortran(glue("{base_name}(size({base_name}):1:-1)"), out_val)
}
