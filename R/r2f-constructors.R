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


r2f_handlers[["matrix"]] <- function(args, scope = NULL, ...) {
  args$data %||% stop("matrix(data=) must be provided, cannot be NA")
  out <- r2f(args$data, scope, ...)
  out@value <- Variable(
    mode = out@value@mode,
    dims = r2dims(list(args$nrow, args$ncol), scope)
  )
  out

  # TODO: reshape() if !passes_as_scalar(out)
}

r2f_handlers[["array"]] <- function(args, scope = NULL, ...) {
  args$data %||% stop("array(data=) must be provided, cannot be NA")
  if (is.null(args$dim)) {
    stop("array(dim=) must be provided, cannot be NA")
  }
  if (!is.null(args$dimnames)) {
    stop("array(dimnames=) not supported")
  }

  out <- r2f(args$data, scope, ...)
  target_dims <- r2dims(args$dim, scope)
  if (!passes_as_scalar(out@value)) {
    # R semantics: `array()` flattens its input (dropping dim) then reshapes.
    # We implement this as `reshape()`; recycling is not supported here.
    dims_f <- dims2f(target_dims, scope)
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

    source <- if (is_fill_constructor) {
      dims_terms <- strsplit(dims_f, ",\\s*")[[1L]]
      n_expr <- if (length(dims_terms) == 1L) {
        dims_terms[[1L]]
      } else {
        glue("({paste(dims_terms, collapse = ' * ')})")
      }
      i <- scope@get_unique_var("integer")
      glue("[({out}, {i}=1, int({n_expr}))]")
    } else {
      # RESHAPE() requires `SOURCE` to be an array expression; array constructors
      # flatten array-valued expressions (which matches R's array() semantics).
      glue("[{out}]")
    }

    out <- Fortran(glue("reshape({source}, {shape})"), out@value)
  }

  out@value <- Variable(
    mode = out@value@mode,
    dims = target_dims
  )
  out
}
