# r2f-random.R
# Handlers for random number generation: runif

# --- Handlers ---

r2f_handlers[["runif"]] <- function(args, scope, ..., hoist = NULL) {
  scope_mark_uses_rng(scope)

  dims <- r2dims(args$n, scope)
  var <- Variable("double", dims)

  min <- args$min %||% 0
  max <- args$max %||% 1
  default_min <- identical(min, 0) || identical(min, 0L)
  default_max <- identical(max, 1) || identical(max, 1L)

  # R evaluates runif() bounds exactly once, but `min` is spliced twice below
  # and the implied-do re-evaluates the whole expression per element; hoist
  # non-trivial bounds (e.g. an impure runif(1)) so they are evaluated once.
  bound <- function(r_arg) {
    b <- r2f(r_arg, scope, ..., hoist = hoist)
    if (is.atomic(r_arg)) b else hoist_unless_name(b, hoist)
  }

  if (default_min && default_max) {
    get1rand <- "unif_rand()"
  } else if (default_min) {
    max <- bound(max)
    get1rand <- glue("unif_rand() * {max}")
  } else {
    min <- bound(min)
    max <- bound(max)
    get1rand <- glue("({min} + (unif_rand() * ({max} - {min})))")
  }

  if (passes_as_scalar(var)) {
    fortran <- get1rand
  } else {
    tmp_i <- scope_unique_var(scope, "integer") ## would be better as uint64...
    fortran <- glue("[({get1rand}, {tmp_i}=1, {dims[[1L]]})]")
  }

  Fortran(fortran, var)
}
