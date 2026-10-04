# r2f-return.R
# Support for return() in quick() functions and local closures.
#
# A trailing return(value) is rewritten to its plain value, so it compiles
# exactly like a function that ends in `value`. Any other return() is an
# early return: it assigns the result variable and emits a Fortran RETURN.
# Every return site must agree on the result's type and shape, because the
# result is a single variable allocated before the call.

# --- Local Helpers ---

is_return_call <- function(e) {
  is.call(e) && identical(e[[1L]], quote(return))
}

# Does evaluating `e` always end in return() (or an error)?
# Used by: normalize_return_tail()
always_returns <- function(e) {
  if (is_return_call(e) || is_call(e, "stop")) {
    return(TRUE)
  }
  if (is_call(e, "{")) {
    return(length(e) > 1L && always_returns(e[[length(e)]]))
  }
  if (is_call(e, "if") && length(e) == 4L) {
    return(always_returns(e[[3L]]) && always_returns(e[[4L]]))
  }
  FALSE
}

# All return() calls in `e`, not descending into nested function
# definitions (local closures handle their own returns).
# Used by: normalize_function_returns(), normalize_closure_returns(),
#          compile_internal_subroutine()
find_return_calls <- function(e) {
  if (!is.call(e) || is_function_call(e)) {
    return(list())
  }
  found <- if (is_return_call(e)) list(e) else list()
  elements <- as.list(e)
  for (i in seq_along(elements)) {
    if (!identical(elements[[i]], quote(expr = ))) {
      found <- c(found, find_return_calls(elements[[i]]))
    }
  }
  found
}

# return() must be a statement: a direct statement of a body, a branch of
# an if() used as a statement, or a loop body.
# Used by: normalize_function_returns(), normalize_closure_returns()
check_return_positions <- function(e, statement = TRUE) {
  if (!is.call(e) || is_function_call(e)) {
    return(invisible())
  }
  if (is_return_call(e)) {
    if (!statement) {
      stop(
        "return() must be used as a statement, not as part of an expression: ",
        deparse1(e),
        call. = FALSE
      )
    }
    if (length(e) > 1L) {
      check_return_positions(e[[2L]], statement = FALSE)
    }
    return(invisible())
  }
  args <- as.list(e)[-1L]
  statement_args <- if (statement && is.symbol(e[[1L]])) {
    switch(
      as.character(e[[1L]]),
      `{` = seq_along(args),
      `if` = 2:3,
      `for` = 3L,
      `while` = 2L,
      `repeat` = 1L,
      integer()
    )
  } else {
    integer()
  }
  for (i in seq_along(args)) {
    if (!identical(args[[i]], quote(expr = ))) {
      check_return_positions(args[[i]], statement = i %in% statement_args)
    }
  }
  invisible()
}

# Rewrite the end of a statement list so a trailing return(value) becomes
# `value`. A final `if (c) A else B` whose `A` always returns is split into
# `if (c) A` followed by the statements of `B`, so `else if` chains ending in
# return() leave a plain value last (quickr cannot use if() as a value).
# A trailing return() without a value becomes NULL.
# Used by: normalize_function_returns(), normalize_closure_returns()
normalize_return_tail <- function(stmts) {
  repeat {
    n <- length(stmts)
    if (!n) {
      break
    }
    last_stmt <- stmts[[n]]
    if (is_call(last_stmt, "{")) {
      stmts <- c(stmts[-n], as.list(last_stmt)[-1L])
      next
    }
    if (is_return_call(last_stmt)) {
      if (length(last_stmt) == 1L) {
        stmts[n] <- list(NULL)
        break
      }
      stmts[[n]] <- last_stmt[[2L]]
      next
    }
    if (
      is_call(last_stmt, "if") &&
        length(last_stmt) == 4L &&
        always_returns(last_stmt[[3L]])
    ) {
      stmts <- c(stmts[-n], list(last_stmt[1:3], last_stmt[[4L]]))
      next
    }
    break
  }
  stmts
}

is_list_result <- function(stmts) {
  n <- length(stmts)
  final <- stmts[[n]]
  if (is_call(final, "list")) {
    return(TRUE)
  }
  is.symbol(final) &&
    n >= 2L &&
    is_call(stmts[[n - 1L]], "<-") &&
    identical(stmts[[n - 1L]][[2L]], final) &&
    is_call(stmts[[n - 1L]][[3L]], "list")
}

check_early_return_values <- function(returns) {
  for (ret in returns) {
    if (length(ret) == 1L || is.null(ret[[2L]])) {
      stop(
        "return() must return a value in a quick() function",
        call. = FALSE
      )
    }
    if (is_call(ret[[2L]], "list")) {
      stop(
        "return(list(...)) is only supported as the last statement: ",
        deparse1(ret),
        call. = FALSE
      )
    }
  }
}

# Pick a result variable name not used anywhere in `body`.
fresh_return_name <- function(body, base = "out_") {
  used <- all.vars(body)
  name <- base
  while (name %in% used) {
    name <- paste0(name, "_")
  }
  name
}

# Rewrite the body of a quick() function for return(). Returns
# list(body, target, prebind): `target` is the result variable early
# returns assign (NULL without early returns), and `prebind` names a
# result variable the caller must bind before compiling (NULL when every
# return() already returns the final symbol).
# Used by: new_fortran_subroutine()
normalize_function_returns <- function(body) {
  unchanged <- list(body = body, target = NULL, prebind = NULL)
  if (!length(find_return_calls(body))) {
    return(unchanged)
  }
  check_return_positions(body)

  stmts <- as.list(body)[-1L]
  stmts <- normalize_return_tail(stmts)
  if (!length(stmts) || is.null(last(stmts))) {
    stop("return() must return a value in a quick() function", call. = FALSE)
  }
  body <- as.call(c(quote(`{`), stmts))

  returns <- find_return_calls(body)
  if (!length(returns)) {
    return(list(body = body, target = NULL, prebind = NULL))
  }
  check_early_return_values(returns)

  final <- last(stmts)
  if (
    is.call(final) &&
      as.character(final[[1L]])[[1L]] %in%
        c("if", "for", "while", "repeat", "break", "next")
  ) {
    stop(
      "a quick() function that uses return() must end with a value or ",
      "return() on every path; the last statement is: ",
      deparse1(final),
      call. = FALSE
    )
  }
  if (is_list_result(stmts)) {
    stop(
      "an early return() cannot be combined with a list result yet",
      call. = FALSE
    )
  }

  # When every return() and the final value are the same variable, that
  # variable is the result and an early return() needs no assignment.
  values <- lapply(returns, `[[`, 2L)
  if (
    is.symbol(final) &&
      all(map_lgl(values, function(v) identical(v, final)))
  ) {
    return(list(
      body = body,
      target = as.character(final),
      prebind = NULL
    ))
  }

  target <- fresh_return_name(body)
  n <- length(stmts)
  final_stmts <- if (
    (is_call(final, "<-") || is_call(final, "=")) && is.symbol(final[[2L]])
  ) {
    list(final, call("return", final[[2L]]))
  } else {
    list(call("return", final))
  }
  stmts <- c(stmts[-n], final_stmts, list(as.symbol(target)))
  list(
    body = as.call(c(quote(`{`), stmts)),
    target = target,
    prebind = target
  )
}

# Rewrite a local closure body for return(): only the trailing return is
# changed; early returns are compiled by the return() handler against the
# closure's result argument.
# Used by: new_local_closure()
normalize_closure_returns <- function(body) {
  if (!length(find_return_calls(body))) {
    return(body)
  }
  check_return_positions(body, statement = TRUE)
  braced <- is_call(body, "{")
  stmts <- if (braced) as.list(body)[-1L] else list(body)
  stmts <- normalize_return_tail(stmts)
  for (ret in find_return_calls(as.call(c(quote(`{`), stmts)))) {
    if (length(ret) > 1L && is_call(ret[[2L]], "list")) {
      stop(
        "return(list(...)) is not supported in local closures: ",
        deparse1(ret),
        call. = FALSE
      )
    }
  }
  if (!braced && length(stmts) == 1L) {
    return(stmts[[1L]])
  }
  as.call(c(quote(`{`), stmts))
}

# The scope that owns the return target: the quick() function or closure
# being compiled, skipping block scopes created for temporaries.
return_owner_scope <- function(scope) {
  while (
    inherits(parent.env(scope), "quickr_scope") &&
      identical(scope_kind(scope), "block")
  ) {
    scope <- parent.env(scope)
  }
  scope
}

return_shape_msg <- "all return() values must have the same shape"

# Check a return value against the result variable defined by an earlier
# return site: the type must be identical and the shape the same, with a
# runtime guard where lengths are only known at run time.
check_return_value_matches <- function(var, value, hoist, scope) {
  val <- value@value
  if (!identical(var@mode, val@mode)) {
    stop(
      "all return() values must have the same type; found ",
      val@mode,
      " where another return value is ",
      var@mode,
      call. = FALSE
    )
  }
  var_scalar <- passes_as_scalar(var)
  val_scalar <- passes_as_scalar(val)
  if (var_scalar != val_scalar || (!var_scalar && var@rank != val@rank)) {
    stop(return_shape_msg, call. = FALSE)
  }
  if (var_scalar) {
    return(invisible())
  }
  target <- Fortran(var@name, var)
  for (axis in seq_len(var@rank)) {
    guard_conformable_dims(
      var_dim_or_one(var, axis),
      dim_or_one(value, axis),
      return_shape_msg,
      hoist,
      scope,
      left = target,
      right = value,
      left_axis = axis,
      right_axis = axis,
      checker = check_equal_dims
    )
  }
  invisible()
}


# --- Handlers ---

r2f_handlers[["return"]] <- function(args, scope, ..., hoist = NULL) {
  owner <- return_owner_scope(scope)
  target <- scope_get(owner, "return_target")
  if (is.null(target)) {
    stop(
      "return() is only supported as a statement in a quick() function ",
      "or local closure body",
      call. = FALSE
    )
  }
  if (scope_in_openmp(scope)) {
    stop("return() is not supported inside a parallel() loop", call. = FALSE)
  }

  has_value <- length(args) >= 1L &&
    !is_missing(args[[1L]]) &&
    !is.null(args[[1L]])
  if (isTRUE(target$void)) {
    if (has_value) {
      stop(
        "return(<value>) is not supported in a local closure whose ",
        "result is not used",
        call. = FALSE
      )
    }
    return(Fortran("return"))
  }
  if (!has_value) {
    stop("return() must return a value here", call. = FALSE)
  }

  value_expr <- args[[1L]]
  name <- target$name
  if (is.symbol(value_expr) && identical(as.character(value_expr), name)) {
    # The result variable already holds the value.
    if (!inherits(get0(name, scope), Variable)) {
      stop("could not resolve return value: ", name, call. = FALSE)
    }
    return(Fortran("return"))
  }

  value <- r2f(value_expr, scope, ..., hoist = hoist)
  if (!inherits(value@value, Variable) || is.null(value@value@mode)) {
    stop(
      "return() value must be an atomic vector, matrix, or array: ",
      deparse1(value_expr),
      call. = FALSE
    )
  }

  var <- get0(name, owner, inherits = FALSE)
  if (!inherits(var, Variable)) {
    stop("internal error: missing return() result variable: ", name)
  }
  if (is.null(var@mode)) {
    var@mode <- value@value@mode
    var@dims <- value@value@dims
    if (logical_as_int(value@value) && !isTRUE(value@logical_booleanized)) {
      var@logical_as_int <- TRUE
    }
  } else {
    check_return_value_matches(var, value, hoist, scope)
  }
  var@modified <- TRUE
  owner[[name]] <- var

  Fortran(str_flatten_lines(glue("{var@name} = {value}"), "return"))
}
