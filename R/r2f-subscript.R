# r2f-subscript.R
# Handlers for subscripting operations: [

# --- Handlers ---

r2f_handlers[["["]] <- function(
  args,
  scope,
  ...,
  hoist_mask = function(mask) FALSE,
  hoist = NULL
) {
  # only a subset of R's x[...] features can be translated here. `...` can only be:
  # - a single logical mask, of the same rank as `x`. returns a rank 1 vector.
  # - a number of arguments matching the rank of `x`, with each being
  #   an integer of rank 0 or 1. In this case, a rank 1 logical becomes
  #   converted to an integer with

  var <- args[[1]]
  var <- r2f(var, scope, ..., hoist = hoist)

  idx_args <- args[-1]
  drop <- idx_args$drop %||% TRUE
  idx_args$drop <- NULL

  idxs <- whole_doubles_to_ints(idx_args)
  idxs <- imap(idxs, function(idx, i) {
    if (is_missing(idx)) {
      Fortran(":", Variable("integer", var@value@dims[[i]]))
    } else {
      sub <- r2f(idx, scope, ...)
      if (sub@value@mode == "double") {
        # Fortran subscripts must be integers; coerce numeric expressions
        Fortran(
          glue("int({sub}, kind=c_ptrdiff_t)"),
          Variable("integer", sub@value@dims)
        )
      } else {
        sub
      }
    }
  })

  if (
    length(idxs) == 1 &&
      idxs[[1]]@value@mode == "logical" &&
      idxs[[1]]@value@rank == var@value@rank
  ) {
    mask <- idxs[[1]]
    mask <- booleanize_logical_as_int(mask)
    if (hoist_mask(mask)) {
      return(var)
    }
    return(Fortran(
      glue("pack({var}, {mask})"),
      Variable(var@value@mode, dims = NA)
    ))
  }

  # Indexing a scalar (rank-1 length-1) with `[1]` is valid in R, but Fortran
  # scalars cannot be subscripted. Treat it as a no-op.
  if (
    passes_as_scalar(var@value) &&
      length(idxs) == 1 &&
      idxs[[1]]@value@mode == "integer" &&
      passes_as_scalar(idxs[[1]]@value)
  ) {
    idx_r <- attr(idxs[[1]], "r", exact = TRUE)
    if (identical(idx_r, 1L) || identical(idx_r, 1)) {
      return(var)
    }
    if (isTRUE(idxs[[1]]@value@loop_is_singleton)) {
      return(var)
    }
  }

  # R-style linear indexing for rank>1 arrays: x[i]
  if (
    length(idxs) == 1 &&
      idxs[[1]]@value@mode == "integer" &&
      passes_as_scalar(idxs[[1]]@value) &&
      var@value@rank > 1
  ) {
    # Hoist array expressions before subscripting (no invalid (expr)(i)).
    if (!passes_as_scalar(var@value) && is.null(var@value@name)) {
      tmp <- hoist$declare_tmp(mode = var@value@mode, dims = var@value@dims)
      hoist$emit(glue("{tmp@name} = {var}"))
      var <- Fortran(tmp@name, tmp)
    }

    base_name <- var@value@name %||% stop("missing array name for subscripting")
    subs <- linear_subscripts_from_1d(base_name, var@value@rank, idxs[[1]])
    outval <- Variable(var@value@mode)

    if (var@value@mode == "logical" && logical_as_int(var@value)) {
      designator <- glue("{base_name}({str_flatten_commas(subs)})")
      return(Fortran(glue("({designator} /= 0)"), outval))
    }
    return(Fortran(glue("{base_name}({str_flatten_commas(subs)})"), outval))
  }

  if (length(idxs) != var@value@rank) {
    stop(
      "number of args to x[...] must match the rank of x, received:",
      deparse1(as.call(c(list(as.name("[")), args)))
    )
  }

  idxs <- imap(idxs, function(subscript, i) {
    # if (!idx@value@rank %in% 0:1)
    #   stop("all args to x[...] must have rank 0 or 1",
    #        deparse1(as.call(c(quote(`[`,args )))))
    switch(
      paste0(subscript@value@mode, subscript@value@rank),
      logical0 = {
        Fortran(":", Variable("integer", var@value@dims[[i]]))
      },
      logical1 = {
        # we convert to a temp integer vector, doing the equivalent of R's which()
        i <- scope@get_unique_var("integer")
        f <- glue("pack([({i}, {i}=1, size({subscript}))], {subscript})")
        return(Fortran(f, Variable("int", NA)))
      },
      integer0 = {
        if (drop) {
          subscript
        } else {
          Fortran(glue("{subscript}:{subscript}"), Variable("int", 1))
        }
      },
      integer1 = {
        subscript
      },
      # double0 = { },
      # double1 = { },
      stop(
        "all args to x[...] must be logical or integer of rank 0 or 1",
        deparse1(as.call(c(list(as.name("[")), args)))
      )
    )
  })

  dims <- drop_nulls(lapply(idxs, function(idx) {
    if (drop && passes_as_scalar(idx@value)) {
      return(NULL)
    }

    idx@value@dims[[1]]
  }))
  outval <- Variable(var@value@mode, dims)

  # Fortran does not allow subscripting arbitrary parenthesized expressions,
  # so if the base is an array expression (not a named array designator),
  # hoist it into a temporary array first.
  if (
    !passes_as_scalar(var@value) &&
      is.null(var@value@name)
  ) {
    tmp <- hoist$declare_tmp(mode = var@value@mode, dims = var@value@dims)
    hoist$emit(glue("{tmp@name} = {var}"))
    var <- Fortran(tmp@name, tmp)
  }

  # External logicals are passed as integer storage (0/1) and are "booleanized"
  # during symbol lowering as `(x/=0)`. When indexing, we must subscript the
  # underlying storage first, then convert the indexed value/section to logical.
  if (var@value@mode == "logical" && logical_as_int(var@value)) {
    base_name <- var@value@name %||% stop("missing array name for subscripting")
    designator <- glue("{base_name}({str_flatten_commas(idxs)})")
    Fortran(glue("({designator} /= 0)"), outval)
  } else {
    base_name <- var@value@name %||% stop("missing array name for subscripting")
    Fortran(glue("{base_name}({str_flatten_commas(idxs)})"), outval)
  }
}
