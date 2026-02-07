# r2f-conditionals.R
# Handlers for vectorized conditionals: ifelse

# --- Handlers ---

r2f_handlers[["ifelse"]] <- function(args, scope, ...) {
  .[mask, tsource, fsource] <- lapply(args, r2f, scope, ...)
  mask <- booleanize_logical_as_int(mask)
  # (tsource, fsource, mask)
  mode <- tsource@value@mode
  dims <- conform(mask@value, tsource@value, fsource@value)@dims
  Fortran(glue("merge({tsource}, {fsource}, {mask})"), Variable(mode, dims))
}
