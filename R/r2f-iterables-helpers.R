# r2f-iterables-helpers.R
# Generic helpers for iterable sequences and for-loop iteration.

# Determine the context in which an iterable is used.
# Used by: r2f-sequences.R
r2f_iterable_context <- function(calls) {
  if (is.null(calls) || !length(calls)) {
    return(NULL)
  }
  calls <- drop_last(calls)
  if (!length(calls)) {
    return(NULL)
  }
  parent_call <- last(calls)
  if (parent_call %in% c("[", "for")) {
    parent_call
  } else {
    NULL
  }
}

# Compute the length expression for a Variable value.
# Used by: r2f-sequences.R, r2f-control-flow.R
value_length_expr <- function(value) {
  stopifnot(inherits(value, Variable))
  if (passes_as_scalar(value)) {
    return(1L)
  }
  dims <- value@dims
  if (is.null(dims) || any(map_lgl(dims, is_scalar_na))) {
    return(NA_integer_)
  }
  if (value@rank == 1L) {
    return(dims[[1L]])
  }
  reduce(dims, \(d1, d2) call("*", d1, d2))
}

# Compute the length expression for a seq-like construct.
# Used by: r2f-sequences.R
seq_like_length_expr <- function(from, to, by = NULL) {
  if (is.null(from) || is.null(to) || is_missing(from) || is_missing(to)) {
    return(NA_integer_)
  }
  if (identical(from, to)) {
    return(1L)
  }

  if (is_scalar_integerish(from) && is_scalar_integerish(to)) {
    from_val <- as.integer(from)
    to_val <- as.integer(to)
    delta <- to_val - from_val

    if (delta == 0L) {
      return(1L)
    }

    if (!is.null(by) && is_scalar_integerish(by)) {
      by_val <- as.integer(by)
      if (by_val == 0L) {
        stop("invalid '(to - from)/by'", call. = FALSE)
      }
      if (sign(delta) != sign(by_val)) {
        stop("wrong sign in 'by' argument", call. = FALSE)
      }
      return(abs(delta %/% by_val) + 1L)
    }

    return(abs(delta) + 1L)
  }

  if (is.null(by)) {
    return(call("+", call("abs", call("-", to, from)), 1L))
  }
  call("+", call("abs", call("%/%", call("-", to, from), by)), 1L)
}

# Parse a seq-like call into its components.
# Used by: r2f-sequences.R, r2f-control-flow.R
seq_like_parse <- function(name, args, scope) {
  stopifnot(is_string(name))
  switch(
    name,
    `:` = {
      args <- whole_doubles_to_ints(args)
      from <- args[[1L]]
      to <- args[[2L]]
      len_expr <- seq_like_length_expr(from, to)
      list(
        kind = ":",
        from = from,
        to = to,
        by = NULL,
        len_expr = len_expr
      )
    },
    seq = {
      call_expr <- as.call(c(as.symbol("seq"), args))
      seq_call <- match.call(seq.default, call_expr)
      seq_call <- whole_doubles_to_ints(seq_call)

      if (!is.null(seq_call$length.out) && !is_missing(seq_call$length.out)) {
        stop("seq(length.out=, along.with=) not implemented yet")
      }
      if (!is.null(seq_call$along.with) && !is_missing(seq_call$along.with)) {
        stop("seq(length.out=, along.with=) not implemented yet")
      }

      from <- seq_call$from
      to <- seq_call$to
      by <- seq_call$by

      if (is.null(from) || is.null(to)) {
        stop("seq() requires both `from` and `to`", call. = FALSE)
      }

      if (is_scalar_integer(by) && identical(by, 0L)) {
        if (is.null(from) || is.null(to) || !identical(from, to)) {
          stop("invalid '(to - from)/by'", call. = FALSE)
        }
        by <- NULL
      }

      len_expr <- seq_like_length_expr(from, to, by)
      list(
        kind = "seq",
        from = from,
        to = to,
        by = by,
        len_expr = len_expr
      )
    },
    seq_len = {
      stopifnot(length(args) == 1L)
      n_expr <- whole_doubles_to_ints(args[[1L]])
      list(kind = "seq_len", n = n_expr)
    },
    seq_along = {
      stopifnot(length(args) == 1L)
      list(kind = "seq_along", arg = args[[1L]])
    },
    stop("unsupported iterable: ", name, call. = FALSE)
  )
}

# Translate a seq-like call to Fortran.
# Used by: r2f-sequences.R, r2f-control-flow.R
seq_like_r2f <- function(
  name,
  args,
  scope,
  ...,
  context = NULL,
  reversed = FALSE
) {
  info <- seq_like_parse(name, args, scope)
  kind <- info$kind
  len_expr <- info$len_expr %||% NA_integer_

  state <- switch(
    kind,
    `:` = {
      from <- r2f(info$from, scope, ...)
      to <- r2f(info$to, scope, ...)
      list(
        from = from,
        to = to,
        by = Fortran(glue("sign(1, {to}-{from})"), Variable("integer")),
        len_expr = len_expr,
        omit_step = FALSE
      )
    },
    seq = {
      from <- r2f(info$from, scope, ...)
      to <- r2f(info$to, scope, ...)
      by <- if (is.null(info$by)) {
        Fortran(glue("sign(1, {to}-{from})"), Variable("integer"))
      } else {
        r2f(info$by, scope, ...)
      }

      if (
        from@value@mode != "integer" ||
          to@value@mode != "integer" ||
          by@value@mode != "integer"
      ) {
        stop("non-integer seq()'s not implemented yet.")
      }

      if (isTRUE(reversed)) {
        if (!passes_as_scalar(from@value) || !passes_as_scalar(to@value)) {
          stop("seq() iterable bounds must be scalars")
        }
        if (!passes_as_scalar(by@value)) {
          stop("seq() iterable step must be a scalar")
        }
      }

      list(
        from = from,
        to = to,
        by = by,
        len_expr = len_expr,
        omit_step = FALSE
      )
    },
    seq_len = {
      n <- r2f(info$n, scope, ...)
      if (n@value@mode != "integer" || !passes_as_scalar(n@value)) {
        stop("seq_len() expects an integer scalar")
      }
      len_expr <- r2size(info$n, scope)
      if (is_scalar_na(len_expr)) {
        len_expr <- NA_integer_
      }
      list(
        from = Fortran("1", Variable("integer")),
        to = n,
        by = Fortran("1", Variable("integer")),
        len_expr = len_expr,
        omit_step = !isTRUE(reversed)
      )
    },
    seq_along = {
      x <- r2f(info$arg, scope, ...)
      if (is.null(x@value)) {
        stop("seq_along() argument must have a value")
      }
      len_expr <- value_length_expr(x@value)
      end <- if (passes_as_scalar(x@value)) "1" else glue("size({x})")
      list(
        from = Fortran("1", Variable("integer")),
        to = Fortran(end, Variable("integer")),
        by = Fortran("1", Variable("integer")),
        len_expr = len_expr,
        omit_step = !isTRUE(reversed)
      )
    },
    stop("unsupported iterable: ", kind, call. = FALSE)
  )
  from <- state$from
  to <- state$to
  by <- state$by
  len_expr <- state$len_expr
  omit_step <- state$omit_step

  context <- context %||% r2f_iterable_context(list(...)$calls)
  if (is.null(context)) {
    context <- "value"
  }

  if (is.null(len_expr) || is_scalar_na(len_expr)) {
    len_expr <- NA_integer_
  }
  if (
    context == "value" &&
      is_wholenumber(len_expr) &&
      identical(as.integer(len_expr), 1L)
  ) {
    len_expr <- call("+", 0L, 1L)
  }
  val <- Variable("integer", list(len_expr))

  if (isTRUE(reversed)) {
    last <- glue("{from} + (({to} - {from}) / {by}) * {by}")
    start <- last
    end <- from
    step <- glue("(-{by})")
    omit_step <- FALSE
  } else {
    start <- from
    end <- to
    step <- by
  }

  if (context == "for") {
    fr <- if (omit_step) {
      glue("{start}, {end}")
    } else {
      glue("{start}, {end}, {step}")
    }
  } else if (context == "[") {
    fr <- if (omit_step) {
      glue("{start}:{end}")
    } else {
      glue("{start}:{end}:{step}")
    }
  } else {
    i <- scope@get_unique_var("integer")
    fr <- if (omit_step) {
      glue("[ ({i}, {i} = {start}, {end}) ]")
    } else {
      glue("[ ({i}, {i} = {start}, {end}, {step}) ]")
    }
  }

  Fortran(fr, val)
}

# Unwrap a for-loop iterable, handling rev() calls.
# Used by: r2f-control-flow.R
r2f_unwrap_for_iterable <- function(iterable) {
  reversed <- FALSE
  repeat {
    while (
      is_call(iterable, quote(`(`)) &&
        length(iterable) == 2L
    ) {
      iterable <- iterable[[2L]]
    }
    if (is_call(iterable, quote(rev)) && length(iterable) == 2L) {
      reversed <- !reversed
      iterable <- iterable[[2L]]
      next
    }
    break
  }

  list(iterable = iterable, reversed = reversed)
}

# Check if an iterable is a singleton sequence of 1.
# Used by: r2f-control-flow.R
iterable_is_singleton_one <- function(iterable, scope) {
  is_one <- function(x) {
    is_wholenumber(x) && identical(as.integer(x), 1L)
  }

  if (!is.call(iterable) || !is.symbol(iterable[[1L]])) {
    return(FALSE)
  }

  name <- as.character(iterable[[1L]])
  if (!name %in% c(":", "seq", "seq_len", "seq_along")) {
    return(FALSE)
  }

  args <- as.list(iterable)[-1L]
  info <- tryCatch(seq_like_parse(name, args, scope), error = function(e) NULL)
  if (is.null(info)) {
    return(FALSE)
  }

  switch(
    info$kind,
    seq_len = is_one(info$n),
    seq_along = {
      arg <- info$arg
      if (is.symbol(arg)) {
        var <- get0(as.character(arg), scope)
        if (inherits(var, Variable) && passes_as_scalar(var)) {
          return(TRUE)
        }
      }
      FALSE
    },
    `:` = is_one(info$from) && is_one(info$to),
    seq = is_one(info$from) && is_one(info$to),
    FALSE
  )
}

# Translate a for-loop iterable to Fortran.
# Used by: r2f-control-flow.R
r2f_for_iterable <- function(iterable, scope, ...) {
  original <- iterable
  unwrapped <- r2f_unwrap_for_iterable(iterable)
  iterable <- unwrapped$iterable
  reversed <- unwrapped$reversed

  stop_unsupported <- function(x) {
    stop(
      "unsupported iterable in for(): ",
      deparse1(x),
      "\nSupported: `<symbol>`, `a:b`, `seq(from, to, by)`, `seq_len(n)`, `seq_along(x)`.",
      call. = FALSE
    )
  }

  if (is.symbol(iterable)) {
    stop_unsupported(original)
  }
  if (!is.call(iterable) || !is.symbol(iterable[[1L]])) {
    stop_unsupported(original)
  }

  name <- as.character(iterable[[1L]])
  supported <- c(":", "seq", "seq_len", "seq_along")
  if (!name %in% supported) {
    stop_unsupported(original)
  }

  if (!reversed) {
    return(r2f(iterable, scope, ...))
  }

  args <- as.list(iterable)[-1L]
  switch(
    name,
    `:` = seq_like_r2f(":", args, scope, ..., context = "for", reversed = TRUE),
    seq_len = seq_like_r2f(
      "seq_len",
      args,
      scope,
      ...,
      context = "for",
      reversed = TRUE
    ),
    seq_along = seq_like_r2f(
      "seq_along",
      args,
      scope,
      ...,
      context = "for",
      reversed = TRUE
    ),
    seq = seq_like_r2f(
      "seq",
      args,
      scope,
      ...,
      context = "for",
      reversed = TRUE
    )
  )
}
