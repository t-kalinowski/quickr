# r2f-conditionals.R
# Handlers for vectorized conditionals: ifelse

# --- Handlers ---

r2f_handlers[["ifelse"]] <- function(args, scope, ...) {
  .[mask, tsource, fsource] <- lapply(args, r2f, scope, ...)
  mask <- booleanize_logical_as_int(mask)

  # merge() requires same-typed branches; promote both to their common mode.
  promoted <- promote_operands(list(tsource, fsource), context = "ifelse()")
  .[tsource, fsource] <- promoted$args
  mode <- promoted$mode

  # R: the result is shaped like `test` (branches only contribute values).
  # A scalar test with array branches is not representable with merge().
  if (
    passes_as_scalar(mask@value) &&
      !(passes_as_scalar(tsource@value) && passes_as_scalar(fsource@value))
  ) {
    stop(
      "ifelse() result takes the shape of `test`; ",
      "array-valued yes/no with scalar test is not supported",
      call. = FALSE
    )
  }

  Fortran(
    glue("merge({tsource}, {fsource}, {mask})"),
    Variable(mode, mask@value@dims)
  )
}
