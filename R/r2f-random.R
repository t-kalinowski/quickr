# r2f-random.R
# Handlers for random number generation: runif

# --- Handlers ---

r2f_handlers[["runif"]] <- function(args, scope, ..., hoist = NULL) {
  attr(scope, "uses_rng") <- TRUE

  dims <- r2dims(args$n, scope)
  var <- Variable("double", dims)

  min <- args$min %||% 0
  max <- args$max %||% 1
  default_min <- identical(min, 0) || identical(min, 0L)
  default_max <- identical(max, 1) || identical(max, 1L)

  if (default_min && default_max) {
    get1rand <- "unif_rand()"
  } else if (default_min) {
    max <- r2f(max, scope, ..., hoist = hoist)
    get1rand <- glue("unif_rand() * {max}")
  } else {
    max <- r2f(max, scope, ..., hoist = hoist)
    min <- r2f(min, scope, ..., hoist = hoist)
    get1rand <- glue("({min} + (unif_rand() * ({max} - {min})))")
  }

  if (passes_as_scalar(var)) {
    fortran <- get1rand
  } else {
    tmp_i <- scope@get_unique_var("integer") ## would be better as uint64...
    fortran <- glue("[({get1rand}, {tmp_i}=1, {dims[[1L]]})]")
  }

  Fortran(fortran, var)
}
