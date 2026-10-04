# r2f-closure-arg-promises.R
# Decide when a local closure argument can be evaluated before the call.
#
# R passes closure arguments as promises: an actual argument is evaluated
# when the closure first uses it, and never if it is not used. Local
# closures lower to Fortran procedures, whose actual arguments are
# evaluated before the call. The two orders are indistinguishable when the
# argument has no side effects of its own and the closure is guaranteed to
# use it before doing anything observable (a side effect, or a `<<-` that
# could change what the argument reads). These helpers check exactly that,
# so an argument such as `x[5L]` or `compute(x)` can be materialized into a
# temporary before the call, while anything else stays a compile error.

# --- Local Helpers ---

# Calls with observable side effects. `<<-` covers `x[i] <<- v` too.
closure_arg_effect_ops <- c("runif", "cat", "print", "<<-")

# Look up a local closure by name, as seen from `scope`.
closure_arg_local_closure <- function(name, scope) {
  obj <- get0(name, scope)
  if (inherits(obj, LocalClosure)) obj else NULL
}

# Apply `f` to every element of a call (or pairlist) that is not an empty
# argument, such as the missing index in `x[, 1]`.
closure_arg_walk_elements <- function(e, f) {
  elements <- as.list(e)
  for (i in seq_along(elements)) {
    if (!identical(elements[[i]], quote(expr = ))) {
      f(elements[[i]])
    }
  }
}

# Does evaluating `e` have a side effect? Calls to local closures count when
# their bodies do, following further closure calls (`seen` stops cycles).
# Used by: check_closure_arg_promise()
expr_has_effects <- function(e, scope, seen = character()) {
  if (!is.call(e) && !is.pairlist(e)) {
    return(FALSE)
  }
  if (is.call(e) && is.symbol(e[[1L]])) {
    op <- as.character(e[[1L]])
    if (op %in% closure_arg_effect_ops) {
      return(TRUE)
    }
    closure_obj <- closure_arg_local_closure(op, scope)
    if (!is.null(closure_obj) && !op %in% seen) {
      if (expr_has_effects(body(closure_obj@fun), scope, c(seen, op))) {
        return(TRUE)
      }
    }
  }
  found <- FALSE
  closure_arg_walk_elements(e, function(el) {
    if (!found && expr_has_effects(el, scope, seen)) {
      found <<- TRUE
    }
  })
  found
}

# Names of the variables `e` can modify with `<<-`, directly or through the
# local closures it calls.
# Used by: check_closure_arg_promise()
closure_superassign_targets <- function(e, scope, seen = character()) {
  if (!is.call(e) && !is.pairlist(e)) {
    return(character())
  }
  targets <- character()
  if (is.call(e) && is.symbol(e[[1L]])) {
    op <- as.character(e[[1L]])
    if (identical(op, "<<-")) {
      root <- e[[2L]]
      while (is.call(root) && length(root) >= 2L) {
        root <- root[[2L]]
      }
      if (is.symbol(root)) {
        targets <- as.character(root)
      }
    }
    closure_obj <- closure_arg_local_closure(op, scope)
    if (!is.null(closure_obj) && !op %in% seen) {
      targets <- c(
        targets,
        closure_superassign_targets(body(closure_obj@fun), scope, c(seen, op))
      )
    }
  }
  closure_arg_walk_elements(e, function(el) {
    targets <<- c(targets, closure_superassign_targets(el, scope, seen))
  })
  unique(targets)
}

# Formals of `fun` that every call is guaranteed to force before the body
# does anything observable. The body is walked in R's evaluation order and
# the walk stops at the first construct whose later effects or forcing are
# not unconditional: control flow (only its always-evaluated part is
# walked), a side effect, an error, or a call to a local closure that has
# side effects. Arguments passed on to other local closures are promises
# there too, so they do not count as forced.
# Used by: check_closure_arg_promise()
closure_forced_formals <- function(fun, scope) {
  formal_names <- names(formals(fun)) %||% character()
  forced <- character()
  # Formals reassigned before use: R never evaluates their promise.
  dropped <- character()
  stopped <- FALSE

  walk_args <- function(args) {
    for (i in seq_along(args)) {
      if (!identical(args[[i]], quote(expr = ))) {
        walk(args[[i]])
      }
    }
  }

  walk <- function(e) {
    if (stopped) {
      return(invisible())
    }
    if (is.symbol(e)) {
      name <- as.character(e)
      if (name %in% formal_names && !name %in% c(forced, dropped)) {
        forced <<- c(forced, name)
      }
      return(invisible())
    }
    if (!is.call(e)) {
      return(invisible())
    }
    if (!is.symbol(e[[1L]])) {
      # e.g. an inline (function(v) ...)(x) call
      stopped <<- TRUE
      return(invisible())
    }
    op <- as.character(e[[1L]])
    args <- as.list(e)[-1L]

    closure_obj <- closure_arg_local_closure(op, scope)
    if (!is.null(closure_obj)) {
      if (expr_has_effects(e, scope)) {
        stopped <<- TRUE
      }
      return(invisible())
    }

    switch(
      op,
      `function` = invisible(),
      `<-` = ,
      `=` = {
        # R evaluates the value before the target.
        walk(args[[2L]])
        target <- args[[1L]]
        if (is.symbol(target)) {
          name <- as.character(target)
          if (name %in% formal_names && !name %in% forced) {
            dropped <<- c(dropped, name)
          }
        } else {
          walk(target)
        }
      },
      `<<-` = {
        walk(args[[2L]])
        stopped <<- TRUE
      },
      `if` = ,
      `while` = ,
      `&&` = ,
      `||` = ,
      ifelse = ,
      sapply = {
        walk(args[[1L]])
        stopped <<- TRUE
      },
      `for` = {
        walk(args[[2L]])
        stopped <<- TRUE
      },
      `switch` = {
        # Only EXPR is always evaluated; one alternative runs after it.
        expr_index <- setdiff(seq_along(args), switch_alternative_indices(args))
        if (length(expr_index)) {
          walk(args[[expr_index]])
        }
        stopped <<- TRUE
      },
      `repeat` = ,
      `break` = ,
      `next` = {
        stopped <<- TRUE
      },
      `return` = ,
      stop = ,
      runif = ,
      cat = ,
      print = {
        walk_args(args)
        stopped <<- TRUE
      },
      walk_args(args)
    )
    invisible()
  }

  walk(body(fun))
  forced
}

# Shared prefix for every rejection: existing callers and tests match it.
closure_arg_promise_error <- function(closure_name) {
  paste0(
    closure_name,
    " call: local closure calls only support pure argument expressions"
  )
}

closure_arg_reject_message <- function(closure_name, nm, expr, reason) {
  paste0(
    closure_arg_promise_error(closure_name),
    "; argument `",
    nm,
    " = ",
    deparse1(expr),
    "` ",
    reason,
    ". Assign it to a variable before the call."
  )
}

# Classify one actual argument the caller supplied (not a formal default).
# Returns list(materialize = <bool>, guard_message = <string or NULL>):
# - materialize: lower the argument eagerly into a temporary before the call
# - guard_message: when non-NULL, a runtime guard emitted while lowering the
#   argument is rejected with this message (the argument may not be forced)
# Rejected arguments are a compile error.
# Used by: match_closure_call_args()
check_closure_arg_promise <- function(
  expr,
  nm,
  closure_obj,
  closure_name,
  scope,
  forced
) {
  if (is.symbol(expr) || is.atomic(expr)) {
    if (!r2f_expression_is_pure(expr, scope)) {
      stop(closure_arg_promise_error(closure_name), call. = FALSE)
    }
    return(list(materialize = FALSE, guard_message = NULL))
  }

  is_forced <- nm %in% forced
  reject <- function(reason) {
    stop(
      closure_arg_reject_message(closure_name, nm, expr, reason),
      call. = FALSE
    )
  }
  not_used_reason <- paste0(
    "may raise an error, but `",
    closure_name,
    "` does not always use `",
    nm,
    "` before other effects"
  )

  if (r2f_expression_is_pure(expr, scope)) {
    # Pure arguments keep their direct lowering. They are still evaluated
    # before the call, which R would only match if the closure cannot change
    # what they read before forcing them.
    reads <- all.vars(expr)
    modified <- intersect(
      reads,
      closure_superassign_targets(body(closure_obj@fun), scope)
    )
    if (length(modified) && !is_forced) {
      reject(paste0(
        "reads `",
        str_flatten_commas(modified),
        "`, which `",
        closure_name,
        "` may modify with `<<-` before using `",
        nm,
        "`"
      ))
    }
    guard_message <- if (is_forced) {
      NULL
    } else {
      closure_arg_reject_message(closure_name, nm, expr, not_used_reason)
    }
    return(list(materialize = FALSE, guard_message = guard_message))
  }

  # As in r2f_expression_is_pure(): an optional dummy of an enclosing
  # closure may be absent, so it cannot be read before the call.
  optional_dummy_refs <- keep(all.vars(expr), function(name) {
    var <- get0(name, scope)
    inherits(var, Variable) && !is.null(var@optional_dummy)
  })
  if (length(optional_dummy_refs)) {
    stop(closure_arg_promise_error(closure_name), call. = FALSE)
  }
  if (expr_has_effects(expr, scope)) {
    reject("has side effects")
  }
  if (!is_forced) {
    reject(not_used_reason)
  }
  list(materialize = TRUE, guard_message = NULL)
}
