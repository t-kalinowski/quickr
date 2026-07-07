# r2f-constructors.R
# Handlers for value constructors: c, logical, integer, double, numeric,
# character, raw, matrix, array

# --- Helpers ---

# TRUE for calls to the zero-fill constructors: logical(k), integer(k),
# double(k), numeric(k). These lower to a single scalar literal carrying
# array dims, so splicing contexts must spread them explicitly.
# Used by: c(), array()
is_fill_constructor_call <- function(e) {
  is.call(e) &&
    is.symbol(e[[1L]]) &&
    as.character(e[[1L]]) %in% c("logical", "integer", "double", "numeric")
}

# (parent_call_name() and materialize_via_hoist(), which the handlers
# below build on, live with the hoisting infrastructure in
# r2f-aab-core.R.)

# Parse array(dim=)'s argument to a dims list: literal vectors, literal
# `a:b` sequences, and symbols bound to a known literal vector; anything
# else falls through to r2dims().
# Used by: array()
array_dim_to_dims <- function(dim_arg, scope) {
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
      return(array_dim_to_dims(var@r, scope))
    }
  }

  r2dims(dim_arg, scope)
}

# Product of a dims list when every dim is a known whole number, NA_real_
# otherwise (in double to dodge integer overflow on large dims).
# Used by: array()
known_dims_prod <- function(dims) {
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

# --- Handlers ---

r2f_handlers[["c"]] <- function(args, scope = NULL, ...) {
  ff <- lapply(args, r2f, scope, ...)
  # R's c() flattens matrix/array arguments column-major; drop their dims
  # to a rank-1 view before joining (fill constructors are already
  # rank-1, so they pass through untouched for the spread below).
  ff <- lapply(ff, flatten_to_vector, scope = scope)
  # Fortran array constructors require uniform element types; cast every
  # element whose mode differs from the promoted mode (R: c(1L, 2.5) is
  # double, c(TRUE, 2L) is integer).
  promoted <- promote_operands(ff, context = "c()")
  ff <- promoted$args
  mode <- promoted$mode
  # Fill constructors are one scalar literal claiming length k; spread them
  # as implied-dos so the emitted element count matches the claimed length.
  fill_idx <- which(map_lgl(args, is_fill_constructor_call))
  if (length(fill_idx)) {
    spread_var <- NULL
    for (j in fill_idx) {
      len_f <- dims2f(ff[[j]]@value@dims, scope)
      if (!nzchar(len_f)) {
        next # statically length 1: a single spliced scalar is already right
      }
      if (grepl(":", len_f, fixed = TRUE)) {
        stop(
          "the length of ",
          deparse1(args[[j]]),
          " inside c() must be known",
          call. = FALSE
        )
      }
      spread_var <- spread_var %||% scope_unique_var(scope, "integer")
      ff[[j]] <- Fortran(
        glue("({ff[[j]]}, {spread_var}=1, int({len_f}))"),
        ff[[j]]@value
      )
    }
  }
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


# Compile a zero-fill constructor call: a single scalar literal carrying
# array dims. Whole-array assignment broadcasts that correctly, and
# c()/array() spread it as an implied-do, so those contexts keep the
# scalar form. Any other consumer (elementwise ops, reductions,
# matrix() -- whose reshape() lowering needs an array SOURCE, not a
# scalar literal) needs a real array expression -- an expression like
# `numeric(2) + 1` would otherwise contribute one element where its dims
# claim two -- so materialize the fill into a hoisted temporary there.
fill_constructor_value <- function(literal, mode, args, scope, ..., hoist) {
  var <- Variable(mode = mode, dims = r2dims(args, scope))
  out <- Fortran(literal, var)
  if (passes_as_scalar(var)) {
    return(out)
  }
  parent_call <- parent_call_name(list(...)$calls)
  if (parent_call %in% c("<-", "=", "<<-", "c", "array")) {
    return(out)
  }
  materialize_via_hoist(literal, mode, var@dims, hoist)
}

register_r2f_handler(
  "logical",
  function(args, scope, ..., hoist = NULL) {
    fill_constructor_value(
      ".false.",
      "logical",
      args,
      scope,
      ...,
      hoist = hoist
    )
  },
  match_fun = FALSE
)

register_r2f_handler(
  "integer",
  function(args, scope, ..., hoist = NULL) {
    fill_constructor_value(
      "0_c_int",
      "integer",
      args,
      scope,
      ...,
      hoist = hoist
    )
  },
  match_fun = FALSE
)

register_r2f_handler(
  c("double", "numeric"),
  function(args, scope, ..., hoist = NULL) {
    fill_constructor_value(
      "0.0_c_double",
      "double",
      args,
      scope,
      ...,
      hoist = hoist
    )
  },
  match_fun = FALSE
)


r2f_handlers[["character"]] <- r2f_handlers[["raw"]] <-
  .r2f_handler_not_implemented_yet


# Validate matrix()'s matched arguments once for every consumer: the
# matrix() handler and the elementwise scalar-fill fast path
# (match_scalar_matrix_fill() in r2f-operators-helpers.R), so the two cannot
# drift as more vectorization contexts are added. The shared policy:
# data is required, byrow=TRUE and dimnames are unsupported, and
# nrow/ncol are both required. (R can infer one dimension, but quickr's
# lowering keeps this strict to avoid surprising recycling rules.)
# Returns list(data, nrow, ncol) or stops.
matrix_call_args <- function(args) {
  data <- args$data
  if (is.null(data) || is_missing(data)) {
    stop("matrix(data=) must be provided, cannot be NA", call. = FALSE)
  }
  if (!is.null(args$byrow) && !is_missing(args$byrow) && !isFALSE(args$byrow)) {
    stop("matrix(byrow=TRUE) is not supported", call. = FALSE)
  }
  if (
    !is.null(args$dimnames) &&
      !is_missing(args$dimnames) &&
      !identical(args$dimnames, quote(NULL))
  ) {
    stop("matrix(dimnames=) not supported", call. = FALSE)
  }
  if (is.null(args$nrow) || is_missing(args$nrow)) {
    stop("matrix(nrow=) must be provided", call. = FALSE)
  }
  if (is.null(args$ncol) || is_missing(args$ncol)) {
    stop("matrix(ncol=) must be provided", call. = FALSE)
  }
  list(data = data, nrow = args$nrow, ncol = args$ncol)
}

r2f_handlers[["matrix"]] <- function(args, scope = NULL, ..., hoist = NULL) {
  margs <- matrix_call_args(args)

  src <- r2f(margs$data, scope, ..., hoist = hoist)
  dims <- r2dims(list(margs$nrow, margs$ncol), scope)
  out_val <- Variable(mode = src@value@mode, dims = dims)

  # A scalar broadcasts natively on direct whole-array assignment, so keep
  # it as-is there; in any other context (sum(...), %*%, ...) the expression
  # must be a real rank-2 array, so materialize it into a hoisted temporary.
  if (passes_as_scalar(src@value)) {
    if (parent_call_name(list(...)$calls) %in% c("<-", "=", "<<-")) {
      src@value <- out_val
      return(src)
    }
    return(materialize_via_hoist(src, src@value@mode, dims, hoist))
  }

  # reshape_vector_for_matrix() splices its source into both the `source`
  # and `pad` args; hoist non-trivial expressions so they evaluate once.
  reshape_vector_for_matrix(
    hoist_unless_name(src, hoist),
    dims[[1L]],
    dims[[2L]]
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

  out <- r2f(args$data, scope, ..., hoist = hoist)
  target_dims <- array_dim_to_dims(args$dim, scope)
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
      target_dims <- list(1L)
      tmp <- materialize_via_hoist(
        out,
        mode = out@value@mode,
        dims = out@value@dims,
        hoist = hoist
      )
      idxs <- rep("1", out@value@rank)
      out <- Fortran(
        glue("{tmp}({str_flatten_commas(idxs)})"),
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

      is_fill_constructor <- is_fill_constructor_call(args$data)

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

      source <- if (is_fill_constructor) {
        i <- scope_unique_var(scope, "integer")
        glue("[({out}, {i}=1, int({n_expr}))]")
      } else {
        n_target <- known_dims_prod(target_dims)
        n_source <- known_dims_prod(out@value@dims)
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
          emit_quickr_error_if(
            condition = glue("int({n_expr}) > size({out})"),
            message = paste0(
              "array() reshape does not support recycling ",
              "(data shorter than prod(dim))"
            ),
            hoist = hoist,
            scope = scope
          )
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
