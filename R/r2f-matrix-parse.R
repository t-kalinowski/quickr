# Matrix parsing helpers

# Unwrap t() calls to infer transpose flags and normalize scalars/vectors.
unwrap_transpose_arg <- function(arg, scope, ..., hoist) {
  arg_unwrapped <- unwrap_parens(arg)
  if (is_call(arg_unwrapped, quote(t)) && length(arg_unwrapped) == 2L) {
    inner_arg <- unwrap_parens(arg_unwrapped[[2L]])
    inner <- r2f(inner_arg, scope, ..., hoist = hoist)
    inner <- maybe_cast_double(inner)
    if (inner@value@rank == 2L) {
      return(list(value = inner, trans = "T"))
    } else if (inner@value@rank == 1L) {
      len <- inner@value@dims[[1L]]
      val <- Variable("double", list(1L, len))
      return(list(
        value = Fortran(glue("reshape({inner}, [1, int({len})])"), val),
        trans = "N"
      ))
    } else if (inner@value@rank == 0L) {
      return(list(value = inner, trans = "N"))
    } else {
      stop("t() only supports rank 0-2 inputs")
    }
  }
  value <- r2f(arg, scope, ..., hoist = hoist)
  value <- maybe_cast_double(value)
  list(value = value, trans = "N")
}

# Extract a logical argument or use the provided default.
logical_arg_or_default <- function(args, name, default, context) {
  val <- args[[name]] %||% default
  if (is.null(val)) {
    return(default)
  }
  if (!is.logical(val) || length(val) != 1L || is.na(val)) {
    stop(context, " only supports literal ", name, " = TRUE/FALSE")
  }
  val
}
