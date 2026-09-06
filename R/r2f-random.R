# r2f-random.R
# Handlers for random number generation: runif

# --- Handlers ---

r2f_handlers[["runif"]] <- function(args, scope, ..., hoist = NULL) {
  scope_mark_uses_rng(scope)
  mark_openmp_scope_uses_rng(scope)

  min <- args$min %||% 0
  max <- args$max %||% 1
  count_value <- lower_r2f_operand_in_order(
    args$n,
    scope,
    ...,
    hoist = hoist,
    later_args = list(min, max)
  )
  if (!passes_as_scalar(count_value@value)) {
    stop("runif() requires a scalar sample count", call. = FALSE)
  }
  dims <- r2dims(args$n, scope)
  n <- dims[[1L]]
  count <- n
  count_is_double <- FALSE
  if (is_scalar_integerish(n) && as.integer(n) < 0L) {
    stop("runif() sample count must be non-negative", call. = FALSE)
  }
  if (!is_scalar_integerish(n)) {
    count <- count_value
    count_is_double <- identical(count@value@mode, "double")
    count <- trimws(as.character(count))
  }

  default_min <- identical(min, 0) || identical(min, 0L)
  default_max <- identical(max, 1) || identical(max, 1L)

  # R evaluates runif() bounds exactly once, but `min` is spliced twice below
  # and the implied-do re-evaluates the whole expression per element; hoist
  # non-trivial bounds (e.g. an impure runif(1)) so they are evaluated once.
  # (hoist_unless_name() leaves names and literals alone.)
  #
  # R recycles vector bounds elementwise across exactly `n` draws, while the
  # implied-do below splices each bound into every element and would produce
  # `n * length(bound)` values. Require scalar bounds instead of diverging.
  scalar_bound <- function(value, arg) {
    if (!passes_as_scalar(value@value)) {
      stop(glue("runif() requires a scalar `{arg}` bound"), call. = FALSE)
    }
    value
  }
  bound <- function(r_arg, arg) {
    value <- scalar_bound(r2f(r_arg, scope, ..., hoist = hoist), arg)
    hoist_unless_name(value, hoist)
  }

  if (default_min && default_max) {
    get1rand <- "unif_rand()"
  } else if (default_min) {
    max <- bound(max, "max")
    get1rand <- glue("unif_rand() * {max}")
  } else {
    min <- lower_r2f_operand_in_order(
      min,
      scope,
      ...,
      hoist = hoist,
      later_args = list(max)
    )
    min <- scalar_bound(min, "min")
    min <- hoist_unless_name(min, hoist)
    max <- bound(max, "max")
    get1rand <- glue("({min} + (unif_rand() * ({max} - {min})))")
  }

  # R forces the bounds before reporting a negative dynamic sample count.
  # Emit this guard after bound hoists so effectful arguments consume their
  # RNG draws in the same order.
  if (!is_size_name(n)) {
    message <- "runif() sample count must be non-negative"
    checked <- guard_constructor_dims(
      list(count),
      "runif",
      hoist,
      scope,
      count_is_double,
      message
    )
    if (!identical(checked[[1L]], count)) {
      count <- dims2f(checked, scope)
      dims[[1L]] <- call("quickr_extent_int", n, message)
    }
  }
  var <- Variable("double", dims)

  if (passes_as_scalar(var)) {
    fortran <- get1rand
  } else {
    tmp_i <- scope_unique_var(scope, "integer") ## would be better as uint64...
    fortran <- glue("[({get1rand}, {tmp_i}=1, {count})]")
  }

  Fortran(fortran, var)
}
