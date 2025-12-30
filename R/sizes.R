check_type_call <- function(cl) {
  if (length(cl) > 2) {
    stop("only one variable can be declared per type() call")
  }
  args <- as.list(cl)[-1]
  if (length(names(args)) != 1) {
    stop("name must be provided as: type(<name> = <mode>(<<dims>>)")
  }
  if (!is.call(args[[1]]) && as.character(args[[1]]) %in% .atomic_type_names) {
    stop("only atomic modes are supported")
  }
}


type_call_to_var <- function(cl) {
  check_type_call(cl)
  Variable(
    name = names(cl)[-1],
    mode = as.character(cl[[2L]][[1L]]),
    dims = unname(as.list(cl[[2]])[-1])
  )
}

var_to_type_call <- function(var) {
  arg <- as.call(c(as.symbol(var@mode), var@dims))
  arg <- setNames(list(arg), var@name)
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


r2size <- function(r, scope) {
  typeof(r) |>
    switch(
      integer = r,
      double = {
        if (is_wholenumber(r)) {
          as.integer(r)
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
        if (var@mode != "integer" || !passes_as_scalar(var)) {
          warning("size is not an integer:", as.character(r))
        }
        if (var@is_arg && !var@modified) {
          return(r)
        }
        # TODO: add specific unit tests here
        if (identical(var@r, r)) {
          return(r)
        }
        # make a best effort to use the r expression last assigned to the
        # symbol, or fail gracefully and return NA.
        # closure-locals with unspecified shape are declared allocatable
        # input and/or output args with unspecified shape signal an error.
        r2size(var@r, scope)
      },
      language = {
        op <- as.character(r[[1]])

        if (op %in% c("+", "-", "/", "*", "^", "%/%", "%%")) {
          args <- as.list(r)[-1]
          args <- lapply(args, r2size, scope)
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
          length = {
            var <- get0(as.character(r[[2L]]), scope)
            if (!inherits(var, Variable)) {
              stop("could not resolve size: ", deparse1(r))
            }
            if (var@rank == 1) {
              return(var@dims[[1L]])
            }
            len <- reduce(var@dims, \(d1, d2) call("*", d1, d2))
            r2size(len, scope)
          },
          `[` = {
            # [ only works when paired with dim()
            if (!is_call(r[[2L]], quote(dim))) {
              return(NA_integer_)
            }
            var <- get0(as.character(r[[2L]][[2L]]), scope)
            if (!inherits(var, Variable)) {
              stop("could not resolve size: ", deparse1(r))
            }
            axis <- r[[3]]
            if (!is_wholenumber(axis)) {
              return(NA_integer_)
            }
            if (axis > var@rank) {
              stop("insufficient rank of variable in ", deparse1(r))
            }
            var@dims[[axis]]
          },
          # dim = {
          #
          # },
          nrow = {
            var <- get0(as.character(r[[2L]]), scope)
            if (!inherits(var, Variable)) {
              stop("could not resolve size: ", deparse1(r))
            }
            var@dims[[1]]
          },
          ncol = {
            var <- get0(as.character(r[[2L]]), scope)
            if (!inherits(var, Variable)) {
              stop("could not resolve size: ", deparse1(r))
            }
            var@dims[[2]]
          },
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

# TODO: allow syntax like:
#   declare(type(a, b, c = integer(1)))
# or:
#   declare(type(a = , b = , c = integer(1)))
