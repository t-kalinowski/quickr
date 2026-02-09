# r2f-constructors.R
# Handlers for value constructors: c, logical, integer, double, numeric,
# character, raw, matrix, array

# --- Handlers ---

r2f_handlers[["c"]] <- function(args, scope = NULL, ...) {
  ff <- lapply(args, r2f, scope, ...)
  s <- glue("[ {str_flatten_commas(ff)} ]")
  lens <- lapply(ff[order(map_int(ff, \(f) f@value@rank))], function(e) {
    rank <- e@value@rank
    if (rank == 0) {
      1L
    } else if (rank == 1) {
      e@value@dims[[1]]
    } else {
      stop("all args passed to c() must be scalars or 1-d arrays")
    }
  })
  mode <- reduce_promoted_mode(ff)
  len <- Reduce(
    \(l1, l2) {
      if (is_scalar_na(l1) || is_scalar_na(l2)) {
        NA
      } else if (is_wholenumber(l1) && is_wholenumber(l2)) {
        l1 + l2
      } else {
        call("+", l1, l2)
      }
    },
    lens
  )
  Fortran(s, Variable(mode, list(len)))
}


r2f_handlers[["rep.int"]] <- function(args, scope, ..., hoist = NULL) {
  # This handler exists to support `x[rep.int(i, n)]` style subscripting. A
  # general rep.int() translation would need to preserve x's type/shape; until
  # implemented, fail fast outside `[` context to avoid silent semantic changes.
  context <- r2f_iterable_context(list(...)$calls)
  if (!identical(context, "[")) {
    stop(
      "rep.int() is only supported when used as an index inside `x[...]`",
      call. = FALSE
    )
  }

  # Only support the common scalar form used in indexing: rep.int(scalar, times).
  x_arg <- args$x %||% args[[1L]]
  times_arg <- args$times %||% args[[2L]]

  if (is_missing(x_arg) || is_missing(times_arg) || length(args) != 2L) {
    stop(
      "rep.int() only supports `rep.int(x, times)` with 2 arguments",
      call. = FALSE
    )
  }

  x_arg <- whole_doubles_to_ints(x_arg)
  times_arg <- whole_doubles_to_ints(times_arg)

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  times <- r2f(times_arg, scope, ..., hoist = hoist)

  if (is.null(x@value) || is.null(times@value)) {
    stop(
      "rep.int() only supports scalar integer arguments in indexing (x and times must not be NULL)",
      call. = FALSE
    )
  }

  if (x@value@mode == "double") {
    x <- Fortran(
      glue("int({x}, kind=c_int)"),
      Variable("integer", x@value@dims)
    )
  }
  if (times@value@mode == "double") {
    times <- Fortran(
      glue("int({times}, kind=c_int)"),
      Variable("integer", times@value@dims)
    )
  }

  if (x@value@mode != "integer" || !passes_as_scalar(x@value)) {
    stop("rep.int() expects an integer scalar `x`", call. = FALSE)
  }
  if (times@value@mode != "integer" || !passes_as_scalar(times@value)) {
    stop("rep.int() expects an integer scalar `times`", call. = FALSE)
  }

  len_expr <- r2size(times_arg, scope)
  if (is.null(len_expr) || is_scalar_na(len_expr)) {
    len_expr <- NA_integer_
  }

  i <- scope_unique_var(scope, "integer")
  out_val <- Variable("integer", list(len_expr))
  Fortran(glue("[({x}, {i}=1, int({times}, kind=c_int))]"), out_val)
}


register_r2f_handler(
  "logical",
  function(args, scope, ...) {
    Fortran(".false.", Variable(mode = "logical", dims = r2dims(args, scope)))
  },
  match_fun = FALSE
)

register_r2f_handler(
  "integer",
  function(args, scope, ...) {
    Fortran("0", Variable(mode = "integer", dims = r2dims(args, scope)))
  },
  match_fun = FALSE
)

register_r2f_handler(
  c("double", "numeric"),
  function(args, scope, ...) {
    Fortran("0", Variable(mode = "double", dims = r2dims(args, scope)))
  },
  match_fun = FALSE
)


r2f_handlers[["character"]] <- r2f_handlers[["raw"]] <-
  .r2f_handler_not_implemented_yet


r2f_handlers[["matrix"]] <- function(args, scope = NULL, ..., hoist = NULL) {
  args$data %||% stop("matrix(data=) must be provided, cannot be NA")
  if (!is.null(args$byrow) && !is_missing(args$byrow) && !isFALSE(args$byrow)) {
    stop("matrix(byrow=TRUE) is not supported", call. = FALSE)
  }

  # Require explicit dims for now. (R can infer one dimension, but quickr's
  # lowering keeps this strict to avoid surprising recycling rules.)
  if (is.null(args$nrow) || is_missing(args$nrow)) {
    stop("matrix(nrow=) must be provided", call. = FALSE)
  }
  if (is.null(args$ncol) || is_missing(args$ncol)) {
    stop("matrix(ncol=) must be provided", call. = FALSE)
  }

  src <- r2f(args$data, scope, ..., hoist = hoist)
  dims <- r2dims(list(args$nrow, args$ncol), scope)
  out_val <- Variable(mode = src@value@mode, dims = dims)

  # Scalars can be broadcast into an array on assignment, so keep them as-is.
  if (passes_as_scalar(src@value)) {
    src@value <- out_val
    return(src)
  }

  rows <- dims[[1L]]
  cols <- dims[[2L]]
  source <- glue("{src}")

  # Avoid double-evaluating non-trivial expressions when used in both the
  # `source` and `pad` args. In Fortran, intrinsic actual args are evaluated
  # before the call, so repeating the expression can duplicate side effects
  # (e.g. RNG state via runif()).
  if (
    is.null(src@value@name) ||
      !identical(trimws(source), src@value@name)
  ) {
    tmp <- hoist$declare_tmp(
      mode = src@value@mode,
      dims = src@value@dims,
      logical_as_int = logical_as_int(src@value)
    )
    hoist$emit(glue("{tmp@name} = {src}"))
    source <- tmp@name
  }
  Fortran(
    glue(
      "reshape({source}, [{bind_dim_int(rows)}, {bind_dim_int(cols)}], pad = {source})"
    ),
    out_val
  )
}

r2f_handlers[["array"]] <- function(args, scope = NULL, ..., hoist = NULL) {
  args$data %||% stop("array(data=) must be provided, cannot be NA")
  if (is.null(args$dim)) {
    stop("array(dim=) must be provided, cannot be NA")
  }
  if (!is.null(args$dimnames)) {
    stop("array(dimnames=) not supported")
  }

  dim_to_dims <- function(dim_arg) {
    if (
      is.atomic(dim_arg) &&
        typeof(dim_arg) %in% c("integer", "double")
    ) {
      if (!length(dim_arg) || anyNA(dim_arg)) {
        stop(
          "array(dim=) must be non-empty and must not contain NA",
          call. = FALSE
        )
      }
      dim_arg <- vapply(
        dim_arg,
        function(x) {
          if (!is_wholenumber(x)) {
            stop(
              "array(dim=) must be whole numbers, found: ",
              x,
              call. = FALSE
            )
          }
          as.integer(x)
        },
        integer(1L)
      )
      return(as.list(dim_arg))
    }

    if (is.call(dim_arg) && is.symbol(dim_arg[[1L]])) {
      op <- as.character(dim_arg[[1L]])
      if (op == ":") {
        if (length(dim_arg) != 3L) {
          stop("bad dim sequence", call. = FALSE)
        }
        from <- dim_arg[[2L]]
        to <- dim_arg[[3L]]
        if (
          !(is.atomic(from) && length(from) == 1L && is_wholenumber(from)) ||
            !(is.atomic(to) && length(to) == 1L && is_wholenumber(to))
        ) {
          stop(
            "array(dim=) only supports literal sequences like 2:4",
            call. = FALSE
          )
        }
        return(as.list(seq.int(as.integer(from), as.integer(to))))
      }
    }

    if (is.symbol(dim_arg)) {
      var <- get0(as.character(dim_arg), scope)
      if (
        inherits(var, Variable) &&
          var@mode %in% c("integer", "double") &&
          var@rank == 1L &&
          (is.language(var@r) || is.atomic(var@r)) &&
          !identical(var@r, dim_arg)
      ) {
        return(dim_to_dims(var@r))
      }
    }

    r2dims(dim_arg, scope)
  }

  out <- r2f(args$data, scope, ..., hoist = hoist)
  target_dims <- dim_to_dims(args$dim)
  if (!length(target_dims)) {
    stop("array(dim=) must not be empty", call. = FALSE)
  }
  if (!passes_as_scalar(out@value)) {
    # R semantics: `array()` flattens its input (dropping dim) then reshapes.
    # We implement this as Fortran `reshape()`. Recycling (i.e. expanding a
    # shorter SOURCE to a larger target shape) is not supported.
    dims_f <- dims2f(target_dims, scope)
    scalar_target <- !nzchar(dims_f) && length(target_dims) == 1L
    if (scalar_target) {
      # `dim = 1` is scalar-like in quickr (rank-1 length-1 is declared scalar).
      # Avoid `reshape(..., [1])` (rank-1) and instead return the first element.
      if (is.null(hoist)) {
        stop("internal error: array() requires hoist context", call. = FALSE)
      }
      target_dims <- list(1L)
      tmp <- hoist$declare_tmp(mode = out@value@mode, dims = out@value@dims)
      hoist$emit(glue("{tmp@name} = {out}"))
      idxs <- rep("1", out@value@rank)
      out <- Fortran(
        glue("{tmp@name}({str_flatten_commas(idxs)})"),
        Variable(mode = out@value@mode, dims = list(1L))
      )
    } else {
      if (!nzchar(dims_f)) {
        dims_f <- "1"
      }
      if (grepl(":", dims_f, fixed = TRUE)) {
        stop("array(dim=) must be known", call. = FALSE)
      }
      shape <- glue("int([{dims_f}])")

      data_r <- args$data
      is_fill_constructor <-
        is.call(data_r) &&
        is.symbol(data_r[[1L]]) &&
        as.character(data_r[[1L]]) %in%
          c(
            "logical",
            "integer",
            "double",
            "numeric"
          )

      axis_terms <- vapply(
        target_dims,
        function(d) {
          axis <- dims2f(list(d), scope)
          if (!nzchar(axis)) {
            "1"
          } else {
            axis
          }
        },
        character(1L)
      )
      n_expr <- if (length(axis_terms) == 1L) {
        axis_terms[[1L]]
      } else {
        paste0("(", paste0("(", axis_terms, ")", collapse = " * "), ")")
      }

      known_prod <- function(dims) {
        if (is.null(dims) || !length(dims)) {
          return(1)
        }
        vals <- vapply(
          dims,
          function(d) {
            if (
              is.atomic(d) &&
                length(d) == 1L &&
                !is.na(d) &&
                is_wholenumber(d)
            ) {
              as.double(d)
            } else {
              NA_real_
            }
          },
          double(1L)
        )
        if (anyNA(vals)) {
          return(NA_real_)
        }
        prod(vals)
      }

      source <- if (is_fill_constructor) {
        i <- scope_unique_var(scope, "integer")
        glue("[({out}, {i}=1, int({n_expr}))]")
      } else {
        n_target <- known_prod(target_dims)
        n_source <- known_prod(out@value@dims)
        if (!is.na(n_target) && !is.na(n_source) && n_target > n_source) {
          stop(
            "array() reshape does not support recycling: prod(dim)=",
            n_target,
            " > length(data)=",
            n_source,
            call. = FALSE
          )
        }
        if (!is.null(hoist)) {
          mark_scope_uses_errors(scope)
          err <- quickr_error_fortran_lines(
            "array() reshape does not support recycling (data shorter than prod(dim))",
            scope = scope
          )
          hoist$emit(glue("if (int({n_expr}) > size({out})) then"))
          hoist$emit(paste0("  ", err))
          hoist$emit("end if")
        }

        # RESHAPE() requires `SOURCE` to be an array expression; array constructors
        # flatten array-valued expressions (which matches R's array() semantics).
        glue("[{out}]")
      }

      out <- Fortran(glue("reshape({source}, {shape})"), out@value)
    }
  }

  out@value <- Variable(
    mode = out@value@mode,
    dims = target_dims
  )
  out
}
