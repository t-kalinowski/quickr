# r2f-coercions.R
# Handlers for type coercions: as.double

# --- Handlers ---

r2f_handlers[["as.double"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  maybe_cast_double(r2f(args[[1]], scope, ...))
}
