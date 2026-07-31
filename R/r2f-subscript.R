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

  check_subscript_exprs(var@value, idx_args)

  idxs <- whole_doubles_to_ints(idx_args)
  idxs <- imap(idxs, function(idx, i) {
    if (is_missing(idx)) {
      Fortran(":", Variable("integer", var@value@dims[[i]]))
    } else {
      # Important: pass along the per-statement hoist context, otherwise
      # subscript expressions that need temporaries (e.g. rev(seq_len(n)))
      # will self-render as an inline `block ... end block` *expression*,
      # which is invalid Fortran inside an array designator.
      sub <- r2f(idx, scope, ..., hoist = hoist)
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
        i <- scope_unique_var(scope, "integer")
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
        if (drop && passes_as_scalar(subscript@value)) {
          # R drop=TRUE drops any dimensions of length 1 in the result, even when
          # the index is a length-1 range like 1:1. Fortran, however, treats a
          # triplet subscript (1:1) as an array section, so the rank would not
          # be reduced. Scalarize the index so the generated Fortran rank
          # matches the dropped R result.
          r <- attr(subscript, "r", exact = TRUE)
          while (is_call(r, quote(`(`)) && length(r) == 2L) {
            r <- r[[2L]]
          }
          while (is_call(r, quote(rev)) && length(r) == 2L) {
            r <- r[[2L]]
          }

          if (is_call(r, quote(`:`)) && length(r) == 3L) {
            scalar <- r2f(r[[2L]], scope, ..., hoist = hoist)
            if (scalar@value@mode == "double") {
              scalar <- Fortran(
                glue("int({scalar}, kind=c_ptrdiff_t)"),
                Variable("integer", scalar@value@dims)
              )
            }
            return(scalar)
          }

          if (is_call(r, quote(seq_len)) && length(r) == 2L) {
            n <- r[[2L]]
            if (is_wholenumber(n) && identical(as.integer(n), 1L)) {
              return(Fortran("1_c_int", Variable("integer")))
            }
          }

          if (is_call(r, quote(seq_along)) && length(r) == 2L) {
            return(Fortran("1_c_int", Variable("integer")))
          }

          if (is_call(r, quote(seq))) {
            info <- seq_like_parse("seq", as.list(r)[-1L], scope)
            scalar <- r2f(info$from, scope, ..., hoist = hoist)
            if (scalar@value@mode == "double") {
              scalar <- Fortran(
                glue("int({scalar}, kind=c_ptrdiff_t)"),
                Variable("integer", scalar@value@dims)
              )
            }
            return(scalar)
          }
        }

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

# Reject non-positive subscripts at compile time. R's negative subscript
# means exclusion, so the result's shape depends on the subscript's value --
# not representable in quickr's static-shape model -- while the generated
# Fortran would silently read out of bounds. Unary minus on a subscript is
# unambiguously exclusion syntax in R, so the form is rejected, not just
# statically-known values. Binary minus (x[n - 1]) is untouched.
#
# When the base's extent along the subscript's axis is statically known,
# literal values beyond it are also compile errors: R pads out-of-range
# reads with NA and grows the vector on out-of-range writes -- neither
# representable in quickr's static-shape model -- while the generated
# Fortran would silently read or write out of bounds. Literal `:` range
# endpoints are checked against the extent here too; their >= 1 lower-bound
# validation (including the runtime guard for symbolic endpoints) stays in
# check_subscript_range_bounds().
check_subscript_expr <- function(e, extent = NULL) {
  e <- unwrap_parens(e)
  if (!is_wholenumber(extent)) {
    extent <- NULL
  }
  if (is.numeric(e) && length(e) >= 1L && !anyNA(e)) {
    # R truncates numeric subscripts toward zero; match the int() conversion
    # used by the generated Fortran before validating the resulting indices.
    indices <- trunc(e)
    if (any(indices <= 0)) {
      stop(
        "subscripts must be positive; R's negative (exclusion) and zero ",
        "subscripts are not supported: ",
        deparse1(e),
        call. = FALSE
      )
    }
    if (!is.null(extent) && any(indices > extent)) {
      stop(
        "subscript exceeds its dimension's extent (",
        as.integer(extent),
        "): ",
        deparse1(e),
        "; R's out-of-range subscripts (NA padding, vector growing) ",
        "are not supported",
        call. = FALSE
      )
    }
  }
  if (is_call(e, quote(`-`)) && length(e) == 2L) {
    stop(
      "negative subscripts (exclusion) are not supported: ",
      deparse1(e),
      call. = FALSE
    )
  }
  if (is_call(e, quote(`:`)) && length(e) == 3L && !is.null(extent)) {
    for (endpoint in as.list(e)[-1L]) {
      endpoint <- unwrap_parens(endpoint)
      if (is_wholenumber(endpoint) && endpoint > extent) {
        stop(
          "index range in x[a:b] exceeds its dimension's extent (",
          as.integer(extent),
          "): ",
          deparse1(e),
          "; R's out-of-range subscripts (NA padding, vector growing) ",
          "are not supported",
          call. = FALSE
        )
      }
    }
  }
  if (is_call(e, quote(c))) {
    for (arg in as.list(e)[-1L]) {
      check_subscript_expr(arg, extent = extent)
    }
  }
  invisible(NULL)
}

# Validate every non-missing subscript in `idx_args` against `base_var`'s
# statically known extents. The single entry point for both the read side
# (the `[` handler) and the write side (compile_subset_designator() in
# r2f-closures.R), so read and write subscripts validate identically.
check_subscript_exprs <- function(base_var, idx_args) {
  extents <- subscript_axis_extents(base_var, length(idx_args))
  for (i in seq_along(idx_args)) {
    if (!is_missing(idx_args[[i]])) {
      check_subscript_expr(idx_args[[i]], extent = extents[[i]])
    }
  }
  invisible(NULL)
}

# Statically-known extent per subscript axis; NULL where symbolic/unknown.
# A single subscript on a rank>1 base is R's linear indexing -- its extent
# is the product of the dims when all of them are known.
# Used by: check_subscript_exprs()
subscript_axis_extents <- function(var, n_idx) {
  dims <- var@dims
  if (n_idx == length(dims)) {
    return(lapply(dims, function(d) {
      if (is_wholenumber(d)) as.integer(d) else NULL
    }))
  }
  if (
    n_idx == 1L &&
      length(dims) > 1L &&
      all(vapply(dims, is_wholenumber, logical(1)))
  ) {
    return(list(prod(unlist(dims))))
  }
  rep(list(NULL), n_idx)
}
