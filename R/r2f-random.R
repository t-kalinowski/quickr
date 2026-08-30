# r2f-random.R
# Handlers for random number generation: runif

# --- Handlers ---

r2f_handlers[["runif"]] <- function(args, scope, ..., hoist = NULL) {
  scope_mark_uses_rng(scope)
  mark_openmp_scope_uses_rng(scope)

  dims <- r2dims(args$n, scope)
  n <- dims[[1L]]
  if (is_scalar_integerish(n) && as.integer(n) < 0L) {
    stop("runif() sample count must be non-negative", call. = FALSE)
  }
  var <- Variable("double", dims)

  min <- args$min %||% 0
  max <- args$max %||% 1
  default_min <- identical(min, 0) || identical(min, 0L)
  default_max <- identical(max, 1) || identical(max, 1L)

  # R evaluates runif() bounds exactly once, but `min` is spliced twice below
  # and the implied-do re-evaluates the whole expression per element; hoist
  # non-trivial bounds (e.g. an impure runif(1)) so they are evaluated once.
  # (hoist_unless_name() leaves names and literals alone.)
  bound <- function(r_arg, later_args = list()) {
    operand <- lower_r2f_operand_in_order(
      r_arg,
      scope,
      ...,
      hoist = hoist,
      later_args = later_args
    )
    hoist_unless_name(operand, hoist)
  }

  if (default_min && default_max) {
    get1rand <- "unif_rand()"
  } else if (default_min) {
    max <- bound(max)
    get1rand <- glue("unif_rand() * {max}")
  } else {
    min <- bound(min, later_args = list(max))
    max <- bound(max)
    get1rand <- glue("({min} + (unif_rand() * ({max} - {min})))")
  }

  # R forces the bounds before reporting a negative dynamic sample count.
  # Emit this guard after bound hoists so effectful arguments consume their
  # RNG draws in the same order.
  if (!is_wholenumber(n) && !is_scalar_na(n) && !is_size_name(n)) {
    n_f <- dims2f(list(n), scope)
    if (nzchar(n_f) && !grepl(":", n_f, fixed = TRUE)) {
      emit_quickr_error_if(
        glue("{n_f} < 0"),
        "runif() sample count must be non-negative",
        hoist,
        scope
      )
    }
  }

  if (passes_as_scalar(var)) {
    fortran <- get1rand
  } else {
    tmp_i <- scope_unique_implied_do_var(scope) ## would be better as uint64...
    fortran <- glue("[({get1rand}, {tmp_i}=1, {dims[[1L]]})]")
  }

  Fortran(fortran, var)
}
