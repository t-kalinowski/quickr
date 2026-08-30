check_type_call <- function(cl) {
  if (length(cl) > 2) {
    stop("only one variable can be declared per type() call")
  }
  args <- as.list(cl)[-1]
  if (length(names(args)) != 1) {
    stop("name must be provided as: type(<name> = <mode>(<<dims>>)")
  }
  mode_expr <- args[[1]]
  mode_sym <- if (is.call(mode_expr)) mode_expr[[1L]] else mode_expr
  if (
    !is.symbol(mode_sym) ||
      !as.character(mode_sym) %in% .atomic_type_names
  ) {
    stop("only atomic modes are supported, not: ", deparse1(mode_sym))
  }
  if (!is.call(mode_expr)) {
    stop(
      "the mode must be a call with dimensions, as in: type(",
      names(args),
      " = ",
      as.character(mode_sym),
      "(<dims>))"
    )
  }
}


type_call_to_var <- function(cl) {
  check_type_call(cl)
  r_name <- names(cl)[-1]
  mode <- as.character(cl[[2L]][[1L]])
  if (identical(mode, "character")) {
    # No Fortran translation exists; refuse at the declaration instead of
    # surfacing an internal error from the code generator.
    stop(
      "in declare(type(",
      r_name,
      " = character(...))): character values are not supported by quickr",
      call. = FALSE
    )
  }
  Variable(
    name = fortranize_name(r_name),
    r_name = r_name,
    mode = mode,
    dims = unname(as.list(cl[[2]])[-1])
  )
}

var_to_type_call <- function(var) {
  arg <- as.call(c(as.symbol(var@mode), var@dims))
  arg <- setNames(list(arg), var@r_name %||% var@name)
  as.call(c(quote(type), arg))
}


get_flattened_args <- function(cl) {
  # flatten exprs from `{` in usage like declare({ ... })`
  args <- as.list(cl)[-1]
  args <- lapply(args, function(e) {
    if (is_missing(e)) {
      NULL
    } else if (is_call(e, quote(`{`))) {
      get_flattened_args(e)
    } else {
      list(e)
    }
  })
  unlist(args, recursive = FALSE)
}

self_evaluate <- function(...) sys.call()

substitute_declared_sizes <- function(e) {
  stopifnot(is_call(e, quote(`{`)))

  declared_names <- local({
    names_out <- character()
    walk <- function(node) {
      if (is_call(node, quote(declare))) {
        args <- get_flattened_args(node)
        for (a in args) {
          if (is_type_call(a)) {
            nm <- names(as.list(a)[-1])
            names_out <<- c(names_out, nm)
          }
        }
      }
      if (is.call(node)) {
        lapply(as.list(node), walk)
      }
    }
    walk(e)
    unique(names_out[nzchar(names_out)])
  })

  aliases <- new.env(parent = emptyenv())
  eval_env <- new.env(parent = emptyenv())
  for (name in all.names(e, functions = TRUE, unique = TRUE)) {
    assign(name, self_evaluate, eval_env)
  }
  eval_env <- new.env(parent = eval_env)
  for (name in all.names(e, functions = FALSE, unique = TRUE)) {
    assign(name, as.symbol(name), eval_env)
  }

  eval_env$`{` <- function(...) {
    as.call(c(list(quote(`{`)), list(...)))
  }

  eval_env$declare <- function(...) {
    args <- get_flattened_args(sys.call())
    args <- lapply(args, function(e) {
      if (is_type_call(e)) {
        var <- type_call_to_var(e)
        var@dims <- imap(var@dims, function(size, axis) {
          size_name <- as.symbol(get_size_name(var, axis))
          if (
            is.symbol(size) &&
              !exists(size, aliases) &&
              !(as.character(size) %in% declared_names)
          ) {
            # user defined implicit size_name alias
            assign(as.character(size), size_name, aliases)
            size <- size_name
          } else if (is_scalar_na(size)) {
            size <- size_name
          } else if (is_wholenumber(size)) {
            size <- as.integer(size)
          }
          size
        })
        e <- var_to_type_call(var)
      }
      e
    })

    as.call(c(quote(declare), args))
  }

  e <- eval(e, eval_env)

  # Now the 'aliases' env is populated; go through and substitute
  # size aliases with the actual size name.
  eval_env$declare <- function(...) {
    as.call(lapply(sys.call(), function(e) {
      if (is_type_call(e)) {
        e <- substitute_(e, aliases)
      }
      e
    }))
  }

  eval(e, eval_env)
}


reject_local_closure_size_call <- function(r, scope) {
  if (!is.call(r) || !is.symbol(r[[1L]]) || is.null(scope)) {
    return(invisible(NULL))
  }
  name <- as.character(r[[1L]])
  if (inherits(get0(name, scope), LocalClosure)) {
    stop(
      "local closure `",
      name,
      "()` cannot determine a result size before the generated function runs",
      call. = FALSE
    )
  }
  invisible(NULL)
}

unwrap_scalar_size_expr <- function(r, scope) {
  repeat {
    r <- unwrap_parens(r)
    reject_local_closure_size_call(r, scope)
    if (!is_call(r, quote(c)) || length(r) != 2L) {
      return(r)
    }
    r <- r[[2L]]
  }
}

size_power_exponent_is_integer <- function(x, scope) {
  x <- unwrap_parens(x)
  if (
    is.atomic(x) &&
      typeof(x) %in% c("integer", "double") &&
      length(x) == 1L &&
      !is.na(x) &&
      (is.integer(x) || is.finite(x) && trunc(x) == x)
  ) {
    return(TRUE)
  }
  if (is.symbol(x)) {
    if (is_size_name(x)) {
      return(TRUE)
    }
    var <- get0(as.character(x), scope)
    return(
      inherits(var, Variable) &&
        identical(var@mode, "integer") &&
        passes_as_scalar(var)
    )
  }
  if (!is.call(x) || !is.symbol(x[[1L]])) {
    return(FALSE)
  }
  op <- as.character(x[[1L]])
  args <- as.list(x)[-1L]
  if (identical(op, "as.integer")) {
    return(length(args) == 1L)
  }
  if (op %in% c("length", "nrow", "ncol")) {
    return(length(args) == 1L)
  }
  if (
    identical(op, "[") &&
      length(args) == 2L &&
      is_call(args[[1L]], quote(dim))
  ) {
    return(TRUE)
  }
  if (op %in% c("+", "-", "*", "%/%", "%%", "abs", "min", "max")) {
    return(all(vapply(
      args,
      size_power_exponent_is_integer,
      logical(1L),
      scope = scope
    )))
  }
  FALSE
}

validate_static_size_integer_range <- function(x) {
  stopifnot(is.numeric(x), length(x) == 1L)
  if (is.nan(x) || is.infinite(x)) {
    stop("size must be finite, found: ", x, call. = FALSE)
  }
  if (is.na(x)) {
    return(invisible(TRUE))
  }
  if (x <= -2147483648 || x >= 2147483648) {
    stop(
      "size must be representable as an R integer, found: ",
      x,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

r2size <- function(r, scope, preserve_numeric = FALSE) {
  r <- unwrap_scalar_size_expr(r, scope)

  sanitize_dim <- function(dim) {
    if (is.symbol(dim) || is.call(dim)) {
      return(r2size(dim, scope, preserve_numeric = preserve_numeric))
    }
    dim
  }

  resolve_var_dim <- function(var_expr, axis, expr = var_expr) {
    var <- get0(as.character(var_expr), scope)
    if (!inherits(var, Variable)) {
      stop("could not resolve size: ", deparse1(expr))
    }
    if (axis > var@rank) {
      stop("insufficient rank of variable in ", deparse1(expr))
    }
    sanitize_dim(var@dims[[axis]])
  }

  typeof(r) |>
    switch(
      integer = r,
      double = {
        validate_static_size_integer_range(r)
        if (is_wholenumber(r)) {
          as.integer(r)
        } else if (isTRUE(preserve_numeric)) {
          r
        } else {
          stop("size must be an integer, found: ", r)
        }
      },
      symbol = {
        if (is_size_name(r)) {
          return(r)
        }
        var <- get0(as.character(r), scope)
        if (!inherits(var, Variable)) {
          stop("could not resolve size: ", as.character(r))
        }
        # !identical(): @mode can be NULL (deferred-mode binding)
        if (!identical(var@mode, "integer") || !passes_as_scalar(var)) {
          warning("size is not an integer:", as.character(r))
        }
        if (var@is_arg && !var@modified) {
          return(scope_fortran_symbol(r, scope))
        }
        # TODO: add specific unit tests here
        if (identical(var@r, r)) {
          return(scope_fortran_symbol(r, scope))
        }
        # make a best effort to use the r expression last assigned to the
        # symbol, or fail gracefully and return NA.
        # closure-locals with unspecified shape are declared allocatable
        # input and/or output args with unspecified shape signal an error.
        r2size(var@r, scope, preserve_numeric = preserve_numeric)
      },
      language = {
        op <- as.character(r[[1]])

        if (
          identical(op, "^") &&
            length(r) == 3L &&
            !size_power_exponent_is_integer(r[[3L]], scope)
        ) {
          stop(
            "size expression powers require an integer exponent",
            call. = FALSE
          )
        }

        if (
          op %in%
            c(
              "+",
              "-",
              "/",
              "*",
              "^",
              "%/%",
              "%%",
              "abs",
              "min",
              "max"
            )
        ) {
          args <- as.list(r)[-1]
          args <- lapply(
            args,
            r2size,
            scope,
            preserve_numeric = preserve_numeric
          )
          if (anyNA(rapply(args, as.list))) {
            return(NA_integer_)
          }
          cl <- as.call(c(r[[1]], args))
          if (all(map_lgl(args, is.atomic))) {
            cl <- eval(cl, baseenv())
          }
          return(cl)
        }

        switch(
          op,
          as.integer = {
            if (length(r) != 2L) {
              stop("as.integer() in a size expression expects one argument")
            }
            inner_expr <- unwrap_scalar_size_expr(r[[2L]], scope)
            # A scalar literal is coerced here rather than recursed into:
            # r2size() rejects a non-whole double (and a bare logical), which
            # are exactly the cases as.integer() exists to handle.
            if (
              is.atomic(inner_expr) &&
                typeof(inner_expr) %in% c("logical", "integer", "double") &&
                length(inner_expr) == 1L
            ) {
              if (is.double(inner_expr)) {
                validate_static_size_integer_range(inner_expr)
              }
              return(as.integer(inner_expr))
            }
            # An explicit coercion is exactly what the "not an integer"
            # warning asks for, so don't also warn about the operand.
            inner <- withCallingHandlers(
              r2size(inner_expr, scope, preserve_numeric = TRUE),
              warning = function(w) {
                if (
                  grepl(
                    "size is not an integer",
                    conditionMessage(w),
                    fixed = TRUE
                  )
                ) {
                  invokeRestart("muffleWarning")
                }
              }
            )
            if (is.atomic(inner) && length(inner) == 1L) {
              if (is.na(inner)) {
                return(NA_integer_)
              }
              # truncates toward zero, as as.integer() does in R
              return(as.integer(inner))
            }
            call("as.integer", inner)
          },
          length = {
            var <- get0(as.character(r[[2L]]), scope)
            if (!inherits(var, Variable)) {
              stop("could not resolve size: ", deparse1(r))
            }
            if (var@rank == 1) {
              return(sanitize_dim(var@dims[[1L]]))
            }
            len <- reduce(var@dims, \(d1, d2) call("*", d1, d2))
            r2size(len, scope)
          },
          `[` = {
            # [ only works when paired with dim()
            if (!is_call(r[[2L]], quote(dim))) {
              return(NA_integer_)
            }
            axis <- r[[3]]
            if (!is_wholenumber(axis)) {
              return(NA_integer_)
            }
            resolve_var_dim(r[[2L]][[2L]], axis, r)
          },
          # dim = {
          #
          # },
          nrow = resolve_var_dim(r[[2L]], 1L, r),
          ncol = resolve_var_dim(r[[2L]], 2L, r),
          NA_integer_
        )
      },
      NA_integer_
    )
}

r2dims <- function(r, scope) {
  if (is.call(r)) {
    as.character(r[[1]]) |>
      switch(
        dim = {
          var <- get0(as.character(r[[2L]]), scope)
          if (!inherits(var, Variable)) {
            stop("could not resolve dims: ", deparse1(r))
          }
          return(var@dims)
        },
        c = {
          args <- lapply(r[-1], r2dims, scope)
          dims <- unlist(args, recursive = FALSE)
          return(as.list(dims))
        },
        r <- list(r)
      )
  }
  lapply(r, r2size, scope)
}

size_expr_is_known_nonnegative <- function(x) {
  x <- unwrap_parens(x)
  if (is_scalar_integerish(x)) {
    return(as.integer(x) >= 0L)
  }
  if (is_size_name(x)) {
    return(TRUE)
  }
  if (
    is.call(x) &&
      is.symbol(x[[1L]]) &&
      as.character(x[[1L]]) %in%
        c("length", "nrow", "ncol", "quickr_seq_length")
  ) {
    return(TRUE)
  }
  if (
    is_call(x, quote(`[`)) &&
      length(x) == 3L &&
      is_call(x[[2L]], quote(dim))
  ) {
    return(TRUE)
  }
  if (
    !is.call(x) ||
      !is.symbol(x[[1L]]) ||
      !as.character(x[[1L]]) %in% c("+", "*", "min", "max")
  ) {
    return(FALSE)
  }
  all(vapply(
    as.list(x)[-1L],
    size_expr_is_known_nonnegative,
    logical(1L)
  ))
}

get_size_name <- function(var, axis = NULL, name = var@name, rank = var@rank) {
  stopifnot(is.null(axis) || is_wholenumber(axis) && axis > 0)
  if (is.null(axis) || rank == 1 && axis == 1) {
    sprintf("%s__len_", name)
  } else {
    if (axis > rank) {
      stop("axis must not be > rank")
    }
    sprintf("%s__dim_%i_", name, axis)
  }
}

# TRUE when any of `var`'s dims is its own self-size symbol
# (`a__dim_1_`, `a__len_`), i.e. the variable was declared with unknown
# (NA) sizes that substitute_declared_sizes() rewrote. External
# variables receive those sizes as dummies; for locals they are
# phantoms, so the manifest declares such locals deferred-shape and
# relies on implicit allocation.
# Used by: manifest.R, check_assignment_compatible()
has_self_size_dims <- function(var) {
  stopifnot(inherits(var, Variable))
  any(vapply(
    seq_along(var@dims),
    function(i) {
      d <- var@dims[[i]]
      is.symbol(d) &&
        identical(as.character(d), get_size_name(var, axis = i))
    },
    logical(1)
  ))
}

# TODO: allow syntax like:
#   declare(type(a, b, c = integer(1)))
# or:
#   declare(type(a = , b = , c = integer(1)))
