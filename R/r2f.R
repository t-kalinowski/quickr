# Take parsed R code (anything returnable by base::str2lang()) and returns
# a Fortran object, which is a string of Fortran code and some attributes
# describing the value.
new_hoist <- function(scope) {
  hoisted <- character()
  block_scope <- NULL

  emit <- function(...) {
    hoisted <<- c(
      hoisted,
      as.character(unlist(c(character(), ...), use.names = FALSE))
    )
  }

  has_block <- function() !is.null(block_scope)

  ensure_block_scope <- function() {
    if (is.null(block_scope)) {
      block_scope <<- scope@new_child("block")
    }
    block_scope
  }

  declare_tmp <- function(mode, dims) {
    stopifnot(is_string(mode), is.null(dims) || is.list(dims))
    ensure_block_scope()@get_unique_var(mode = mode, dims = dims)
  }

  render <- function(code) {
    code <- str_split_lines(code)
    if (!length(hoisted) && !has_block()) {
      return(str_flatten_lines(code))
    }

    stmts <- str_split_lines(hoisted, code)

    if (has_block()) {
      decls <- emit_decls(scope_vars(block_scope), block_scope)
      return(str_flatten_lines(emit_block(decls, stmts)))
    }

    str_flatten_lines(stmts)
  }

  list2env(
    list(
      emit = emit,
      declare_tmp = declare_tmp,
      render = render
    ),
    parent = emptyenv()
  )
}

logical_as_int_symbol <- function(var) {
  inherits(var, Variable) &&
    identical(var@mode, "logical") &&
    logical_as_int(var)
}

scope_is_closure <- function(scope) {
  inherits(scope, "quickr_scope") && identical(scope@kind, "closure")
}

scope_fortran_names <- function(scope) {
  stopifnot(inherits(scope, "quickr_scope"))
  out <- character()
  while (inherits(scope, "quickr_scope")) {
    vars <- scope_vars(scope)
    out <- c(out, map_chr(vars, \(v) v@name %||% ""))
    scope <- parent.env(scope)
  }
  unique(out[nzchar(out)])
}

make_shadow_fortran_name <- function(scope, base, suffix = "__local_") {
  stopifnot(inherits(scope, "quickr_scope"), is_string(base), is_string(suffix))
  used <- scope_fortran_names(scope)
  candidate <- paste0(base, suffix)
  if (!candidate %in% used) {
    return(candidate)
  }
  i <- 1L
  repeat {
    candidate <- paste0(base, suffix, i, "_")
    if (!candidate %in% used) {
      return(candidate)
    }
    i <- i + 1L
  }
}

lang2fortran <- r2f <- function(
  e,
  scope = NULL,
  ...,
  calls = character(),
  hoist = NULL
) {
  ## 'hoist' is a per-statement context that handlers can use to pre-emit some
  ## Fortran code. E.g., to setup a temporary variable if the generated Fortran
  ## code doesn't neatly translate into a single expression.
  render_hoist <- is.null(hoist)
  if (render_hoist) {
    hoist <- new_hoist(scope)
  }

  fortran <- switch(
    typeof(e),
    language = {
      # a call
      callable <- e[[1L]]
      callable_unwrapped <- callable
      while (
        is_call(callable_unwrapped, quote(`(`)) &&
          length(callable_unwrapped) == 2L
      ) {
        callable_unwrapped <- callable_unwrapped[[2L]]
      }

      if (!is.null(scope)) {
        {
          # Local closure calls: lower to internal procedures under `contains`.
          if (is.symbol(callable_unwrapped)) {
            callable_name <- as.character(callable_unwrapped)
            closure_obj <- scope[[callable_name]]
            if (inherits(closure_obj, LocalClosure)) {
              call_expr <- as.call(c(list(callable_unwrapped), as.list(e)[-1L]))
              call_fortran <- compile_closure_call(
                call_expr = call_expr,
                closure_obj = closure_obj,
                proc_name = callable_name,
                scope = scope,
                ...,
                hoist = hoist,
                needs_value = !render_hoist
              )
              call_fortran
            } else {
              NULL
            }
          } else if (is_function_call(callable_unwrapped)) {
            proc_name <- scope_root(scope)@get_unique_proc(prefix = "closure")
            host_closure <- scope_root(scope)@closure
            closure_obj <- as_local_closure(
              callable_unwrapped,
              env = environment(host_closure),
              name = proc_name
            )
            call_expr <- as.call(c(list(callable_unwrapped), as.list(e)[-1L]))
            compile_closure_call(
              call_expr = call_expr,
              closure_obj = closure_obj,
              proc_name = proc_name,
              scope = scope,
              ...,
              hoist = hoist,
              needs_value = !render_hoist
            )
          }
        } %||%
          {
            handler <- get_r2f_handler(callable_unwrapped)

            match.fun <- attr(handler, "match.fun", TRUE)
            if (is.null(match.fun)) {
              match.fun <- get0(
                callable_unwrapped,
                parent.env(globalenv()),
                mode = "function"
              )
              # this is a best effort to, eg. resolve `seq.default` from `seq`.
              # This should likely be moved into attaching the `match.fun` attr
              # to handlers, for more involved resolution (e.g., with getS3Method())
              if ("UseMethod" %in% all.names(body(match.fun))) {
                match.fun <- get0(
                  paste0(callable_unwrapped, ".default"),
                  parent.env(globalenv()),
                  mode = "function",
                  ifnotfound = match.fun
                )
              }
            }
            if (typeof(match.fun) == "closure") {
              e <- match.call(match.fun, e)
            }

            if (isTRUE(getOption("quickr.r2f.debug"))) {
              try(handler(
                as.list(e)[-1L],
                scope,
                ...,
                calls = c(calls, as.character(callable_unwrapped)),
                hoist = hoist
              )) -> res
              if (inherits(res, "try-error")) {
                debugonce(handler)
                handler(
                  as.list(e)[-1L],
                  scope,
                  ...,
                  calls = c(calls, as.character(callable_unwrapped)),
                  hoist = hoist
                )
              }

              res
            } else {
              handler(
                as.list(e)[-1L],
                scope,
                ...,
                calls = c(calls, as.character(callable_unwrapped)),
                hoist = hoist
              )
            }
          }
      }
    },

    integer = ,
    double = ,
    complex = ,
    logical = atomic2Fortran(e),

    `NULL` = Fortran("", NULL),

    symbol = {
      r_name <- as.character(e)
      val <- if (is.null(scope)) NULL else get0(r_name, scope)
      s <- if (inherits(val, Variable) && !is.null(val@name)) {
        val@name
      } else {
        r_name
      }
      if (logical_as_int_symbol(val)) {
        # logicals passed via the bind(c) interface are stored as integer(0/1)
        # and must be "booleanized" for Fortran logical operations.
        s <- paste0("(", s, "/=0)")
      }
      Fortran(s, value = if (inherits(val, Variable)) val else NULL)
    },

    ## handling 'object' and 'closure' here are both bad ideas,
    ## TODO: delete both
    # "object" = {
    #   if (inherits(e, Variable))
    #     e <- Fortran(character(), e)
    #   stopifnot(inherits(e, Fortran))
    #   e
    # },

    closure = {
      if (is.null(name <- attr(e, "name", TRUE))) {
        name <- if (is.symbol(name <- substitute(e))) {
          as.character(name)
        } else {
          "anonymous_function"
        }
      }

      stopifnot(is.null(scope))
      new_fortran_subroutine(name, e)
    },

    ## all the other typeof() possible values
    # "character",
    # "raw" ,
    # "list",
    # "NULL",
    # "function",
    # "special",
    # "builtin",
    # "environment",
    # "S4",
    # "pairlist",
    # "promise",
    # "char",
    # "...",
    # "any",
    # "expression",
    # "externalptr",
    # "bytecode",
    # "weakref"
    # default
    stop("Unsupported object type encountered: ", typeof(e))
  )

  attr(fortran, "r") <- e
  if (render_hoist) {
    combined <- hoist$render(fortran)
    attributes(combined) <- attributes(fortran)
    attr(combined, "r") <- e
    combined
  } else {
    fortran
  }
}


atomic2Fortran <- function(x) {
  stopifnot(is_scalar_atomic(x))
  s <- switch(
    typeof(x),
    double = ,
    integer = num2fortran(x),
    logical = if (x) ".true." else ".false.",
    complex = sprintf("(%s, %s)", num2fortran(Re(x)), num2fortran(Im(x)))
  )
  Fortran(s, Variable(typeof(x)))
}

num2fortran <- function(x) {
  stopifnot(typeof(x) %in% c("integer", "double"))
  digits <- 7L
  nsmall <- switch(typeof(x), integer = 0L, double = 1L)
  repeat {
    s <- format.default(x, digits = digits, nsmall = nsmall, scientific = 1L)
    if (x == eval(str2lang(s))) {
      # eval() needed for negative and complex numbers
      break
    }
    add(digits) <- 1L
    if (digits > 22L) {
      stop("number formatting error: ", x, " formatted as : ", s)
    }
  }
  paste0(s, switch(typeof(x), double = "_c_double", integer = "_c_int"))
}


r2f_handlers := new.env(parent = emptyenv())


get_r2f_handler <- function(name) {
  stopifnot("All functions called must be named as symbols" = is.symbol(name))
  get0(name, r2f_handlers) %||%
    stop("Unsupported function: ", name, call. = FALSE)
}

r2f_default_handler <- function(args, scope = NULL, ..., calls) {
  # stopifnot(is.call(e), is.symbol(e[[1L]]))

  x <- lapply(args, r2f, scope = scope, calls = calls, ...)
  s <- sprintf("%s(%s)", last(calls), str_flatten_commas(x[-1]))
  Fortran(s)
}

## ??? export as S7::convert() methods?
register_r2f_handler <- function(name, fun) {
  for (nm in name) {
    r2f_handlers[[nm]] <- fun
  }
  invisible(fun)
}

.r2f_handler_not_implemented_yet <- function(e, scope, ...) {
  stop(
    gettextf("'%s' is not implemented yet", as.character(e[[1L]])),
    call. = FALSE
  )
}

r2f_handlers[["declare"]] <- function(args, scope, ...) {
  for (a in args) {
    if (is_missing(a)) {
      next
    }
    if (is_type_call(a)) {
      var <- type_call_to_var(a)
      var@is_arg <- var@name %in% names(formals(scope@closure))
      if (identical(var@mode, "logical") && isTRUE(var@is_arg)) {
        attr(var, "logical_as_int") <- TRUE
      }
      scope[[var@name]] <- var
    } else if (is_call(a, quote(`{`))) {
      Recall(as.list(a)[-1], scope)
    }
  }

  Fortran("")
}


r2f_handlers[["Fortran"]] <- function(args, scope = NULL, ...) {
  if (!is_string(args[[1]])) {
    stop("Fortran() must be called with a string")
  }
  Fortran(args[[1]])
  # enable passing through literal fortran code
  # used like:
  #   Fortran("nearest(x, 1)", double(length(x)))
  #   Fortran("nearest(x, 1)", x)
  #   Fortran("x = nearest(x, 1)")
}

r2f_handlers[["("]] <- function(args, scope, ...) {
  x <- r2f(args[[1L]], scope, ...)
  Fortran(glue("({x})"), x@value)
}

r2f_handlers[["{"]] <- function(args, scope, ..., hoist = NULL) {
  # every top level R-expr / fortran statement gets its own hoist target.
  x <- lapply(args, r2f, scope, ...)
  code <- str_flatten_lines(x)

  # browser()
  value <- (if (length(args)) last(x)@value) %||% Variable()
  Fortran(code, value)
}


# ---- reduction intrinsics ----

create_mask_hoist <- function() {
  .hoisted_mask <- NULL

  try_set <- function(mask) {
    stopifnot(inherits(mask, Fortran), mask@value@mode == "logical")
    # each hoist can only accept one mask.
    if (is.null(.hoisted_mask)) {
      .hoisted_mask <<- mask
      return(TRUE)
    }
    # if the mask is identical, we accept it.
    if (identical(.hoisted_mask, mask)) {
      return(TRUE)
    }
    # can't hoist this mask.
    FALSE
  }

  get_hoisted <- function() .hoisted_mask

  environment()
}

register_r2f_handler(
  c("max", "min", "sum", "prod"),
  function(
    args,
    scope,
    ...
  ) {
    intrinsic <- switch(
      last(list(...)$calls),
      max = "maxval",
      min = "minval",
      sum = "sum",
      prod = "product"
    )

    reduce_arg <- function(arg) {
      mask_hoist <- create_mask_hoist()
      x <- r2f(arg, scope, ..., hoist_mask = mask_hoist$try_set)
      if (x@value@is_scalar) {
        return(x)
      }
      hoisted_mask <- mask_hoist$get_hoisted()
      s <- glue(
        if (is.null(hoisted_mask)) {
          "{intrinsic}({x})"
        } else {
          "{intrinsic}({x}, mask = {hoisted_mask})"
        }
      )
      Fortran(s, Variable(x@value@mode))
    }

    if (length(args) == 1) {
      reduce_arg(args[[1]])
    } else {
      args <- lapply(args, reduce_arg)
      mode <- reduce_promoted_mode(args)
      s <- switch(
        last(list(...)$calls),
        max = glue("max({str_flatten_commas(args)})"),
        min = glue("min({str_flatten_commas(args)})"),
        sum = glue("({str_flatten(args, ' + ')})"),
        prod = glue("({str_flatten(args, ' * ')})")
      )
      Fortran(s, Variable(mode))
    }
  }
)


r2f_handlers[["which.max"]] <- r2f_handlers[["which.min"]] <-
  function(args, scope = NULL, ...) {
    stopifnot(length(args) == 1)
    x <- r2f(args[[1L]], scope, ...)
    stopifnot(
      "Values passed to which.max()/which.min() must be 1d arrays" = x@value@rank ==
        1
    )
    valout <- Variable(mode = "integer") # integer scalar

    if (x@value@mode == "logical") {
      val <- switch(
        last(list(...)$calls),
        which.max = ".true.",
        which.min = ".false."
      )
      f <- glue("findloc({x}, {val}, 1)")
    } else {
      intrinsic <- switch(
        last(list(...)$calls),
        which.max = "maxloc",
        which.min = "minloc"
      )
      f <- glue("{intrinsic}({x}, 1)")
    }

    Fortran(f, valout)
  }

linear_subscripts_from_1d <- function(base_name, rank, idx) {
  stopifnot(is_string(base_name), is_wholenumber(rank), inherits(idx, Fortran))
  k <- glue("int({idx}, kind=c_int)")
  tmp <- glue("({k} - 1_c_int)")
  subs <- character(rank)
  if (rank == 1L) {
    subs[[1L]] <- k
    return(subs)
  }

  for (axis in seq_len(rank - 1L)) {
    dim_size <- glue("size({base_name}, {axis})")
    subs[[axis]] <- glue("(mod({tmp}, {dim_size}) + 1_c_int)")
    tmp <- glue("({tmp} / {dim_size})")
  }
  subs[[rank]] <- glue("({tmp} + 1_c_int)")
  subs
}


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
  var <- r2f(var, scope, ...)

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
      deparse1(as.call(c(quote(`[`, args))))
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
        deparse1(as.call(c(quote(`[`, args))))
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
    designator <- glue("{var@value@name}({str_flatten_commas(idxs)})")
    Fortran(glue("({designator} /= 0)"), outval)
  } else {
    Fortran(glue("{var}({str_flatten_commas(idxs)})"), outval)
  }
}


r2f_handlers[[":"]] <- function(args, scope, ...) {
  # depending on context, this translation can vary.

  # x[a:b]    becomes   x(a:b)
  # for(i in a:b){}  becomes  do i = a,b ...
  # c(a:b)  becomes  ({tmp}, {tmp}=a,b)
  args <- whole_doubles_to_ints(args)
  .[start, end] <- lapply(args, r2f, scope, ...)
  step <- glue("sign(1, {end}-{start})")
  val <- Variable("integer", NA)
  fr <- switch(
    list(...)$calls |> drop_last() |> last(),
    "[" = glue("{start}:{end}:{step}"),
    "for" = glue("{start}, {end}, {step}"),
    {
      i <- scope@get_unique_var("integer")
      glue("[ ({i}, {i} = {start}, {end}, {step}) ]")
    }
    # default
  )
  Fortran(fr, val)
}


r2f_handlers[["seq"]] <- function(args, scope, ...) {
  args <- whole_doubles_to_ints(args) # only casts if trunc(dbl) == dbl
  if (!is.null(args$length.out) || !is.null(args$along.with)) {
    stop("seq(length.out=, along.with=) not implemented yet")
  }

  .[from, to, by] <- lapply(args, r2f, scope, ...)[c("from", "to", "by")]
  by <- by %||% Fortran(glue("sign(1, {to}-{from})"), Variable("integer"))

  # Fortran only supports integer sequences in do and implicit do contexts.
  # to make a double sequence, needs to be in via an implied map() call, like
  # seq(1, 10, .1) ->    [(x * 0.1, x = 10, 50)]
  #
  # e.g., i <- scope@get_unique_var("integer")
  # glue("[({i} * by, {i} = int(from/by), int(to/by))]")
  if (
    from@value@mode != "integer" ||
      to@value@mode != "integer" ||
      by@value@mode != "integer"
  ) {
    stop("non-integer seq()'s not implemented yet.")
  }

  # depending on context, this translation can vary.
  #
  # x[a:b]    becomes   x(a:b)
  # for(i in a:b){}  becomes  do i = a,b ...
  # c(a:b)  becomes  ({tmp}, {tmp}=a,b)
  val <- Variable("integer", NA)
  fr <- switch(
    list(...)$calls |> drop_last() |> last(),
    "[" = glue("{from}:{to}:{by}"),
    "for" = glue("{from}, {to}, {by}"),
    {
      i <- scope@get_unique_var("integer")
      glue("[ ({i}, {i} = {from}, {to}, {by}) ]")
    }
    # default
  )
  Fortran(fr, val)
}


r2f_handlers[["ifelse"]] <- function(args, scope, ...) {
  .[mask, tsource, fsource] <- lapply(args, r2f, scope, ...)
  # (tsource, fsource, mask)
  mode <- tsource@value@mode
  dims <- conform(mask@value, tsource@value, fsource@value)@dims
  Fortran(glue("merge({tsource}, {fsource}, {mask})"), Variable(mode, dims))
}

# ---- pure elemental unary math intrinsics ----

## real and complex intrinsics
register_r2f_handler(
  c(
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sqrt",
    "exp",
    "log",
    "floor",
    "ceiling"
  ),
  function(args, scope, ...) {
    stopifnot(length(args) == 1L)
    arg <- r2f(args[[1]], scope, ...)
    intrinsic <- last(list(...)$calls)
    Fortran(
      glue("{intrinsic}({arg})"),
      Variable(mode = arg@value@mode, dims = arg@value@dims)
    )
  }
)

r2f_handlers[["log10"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  f <- if (arg@value@mode == "complex") {
    glue("(log({arg}) / log(10.0_c_double))")
  } else {
    glue("log10({arg})")
  }
  Fortran(
    f,
    Variable(mode = arg@value@mode, dims = arg@value@dims)
  )
}

## accepts real, integer, or complex
r2f_handlers[["abs"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- if (arg@value@mode == "complex") {
    Variable(mode = "double", dims = arg@value@dims)
  } else {
    Variable(mode = arg@value@mode, dims = arg@value@dims)
  }
  Fortran(glue("abs({arg})"), val)
}


# ---- complex elemental unary intrinsics ----

r2f_handlers[["Re"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("real({arg})"), val)
}

r2f_handlers[["Im"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("aimag({arg})"), val)
}

# Modulus (magnitude)
r2f_handlers[["Mod"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("abs({arg})"), val)
}

# Argument (phase angle, radians)
r2f_handlers[["Arg"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "double", dims = arg@value@dims)
  Fortran(glue("atan2(aimag({arg}), real({arg}))"), val)
}

# conjg() returns a complex value; R uses Conj()
r2f_handlers[["Conj"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  arg <- r2f(args[[1]], scope, ...)
  val <- Variable(mode = "complex", dims = arg@value@dims)
  Fortran(glue("conjg({arg})"), val)
}


# ---- elemental binary infix operators ----

maybe_cast_double <- function(x) {
  if (x@value@mode == "logical") {
    Fortran(
      glue("merge(1_c_double, 0_c_double, {x})"),
      Variable("double", x@value@dims)
    )
  } else if (x@value@mode == "integer") {
    Fortran(
      glue("real({x}, kind=c_double)"),
      Variable("double", x@value@dims)
    )
  } else {
    x
  }
}

r2f_handlers[["+"]] <- function(args, scope, ...) {
  # Support both binary and unary plus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ...)
    Fortran(glue("(+{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lapply(args, r2f, scope, ...)
    Fortran(glue("({left} + {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["-"]] <- function(args, scope, ...) {
  # Support both binary and unary minus
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ...)
    Fortran(glue("(-{x})"), Variable(x@value@mode, x@value@dims))
  } else {
    .[left, right] <- lapply(args, r2f, scope, ...)
    Fortran(glue("({left} - {right})"), conform(left@value, right@value))
  }
}

r2f_handlers[["*"]] <- function(args, scope = NULL, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  Fortran(glue("({left} * {right})"), conform(left@value, right@value))
}

r2f_handlers[["/"]] <- function(args, scope = NULL, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  left <- maybe_cast_double(left)
  right <- maybe_cast_double(right)
  Fortran(glue("({left} / {right})"), conform(left@value, right@value))
}

r2f_handlers[["as.double"]] <- function(args, scope = NULL, ...) {
  stopifnot(length(args) == 1L)
  maybe_cast_double(r2f(args[[1]], scope, ...))
}

r2f_handlers[["^"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  Fortran(glue("({left} ** {right})"), conform(left@value, right@value))
}


r2f_handlers[[">="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} >= {right})"), var)
}
r2f_handlers[[">"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} > {right})"), var)
}
r2f_handlers[["<"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} < {right})"), var)
}
r2f_handlers[["<="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} <= {right})"), var)
}
r2f_handlers[["=="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} == {right})"), var)
}
r2f_handlers[["!="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} /= {right})"), var)
}

# ---- unary logical not ----
r2f_handlers[["!"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ...)
  if (x@value@mode != "logical") {
    stop("'!' expects a logical value; numeric coercions not yet supported")
  }
  Fortran(glue("(.not. {x})"), Variable("logical", x@value@dims))
}


# ---- remainder (%%) and integer division (%/%) ----
#
# R semantics:
#   x %%  y  ==  r   where  r has the sign of y  (divisor)
#   x %/% y  ==  q   where  q = floor(x / y)
# and  x == r + y * q  (within rounding error)
#
# Fortran intrinsics:
#   - MODULO(a,p)   : remainder with sign(p)
#   - FLOOR(x)      : greatest integer ≤ x      (real)
#   - AINT(x)       : truncation toward 0       (real)

r2f_handlers[["%%"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  out_val <- conform(left@value, right@value)
  # MODULO gives result with sign(right) – matches R %% behaviour
  Fortran(glue("modulo({left}, {right})"), out_val)
}

r2f_handlers[["%/%"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  out_val <- conform(left@value, right@value)

  expr <- switch(
    out_val@mode,
    integer = glue("int(floor(real({left}) / real({right})))"),
    double = glue("floor({left} / {right})"),
    stop("%/% only implemented for numeric types")
  )

  Fortran(expr, out_val)
}


# TODO: the scalar || probably need some more type checking.
# TODO: gfortran supports implicit casting that of logical to integer when
# assigning a logical to a variable declared integer, converting `.true.` to `1`,
# but this is not a standard language feature, and Intel's `ifort` uses `-1` for `.true`.
# We should explicitly use
#   `merge(1_c_int, 0_c_int, <lgl>)` to cast logical to int.
register_r2f_handler(
  c("&", "&&", "|", "||"),
  function(args, scope, ...) {
    args <- lapply(args, r2f, scope, ...)
    args <- lapply(args, function(a) {
      if (a@value@mode != "logical") {
        stop("must be logical")
      }
      a
    })
    .[left, right] <- args

    operator <- switch(
      last(list(...)$calls),
      `&` = ,
      `&&` = ".and.",
      `|` = ,
      `||` = ".or."
    )

    s <- glue("{left} {operator} {right}")
    val <- conform(left@value, right@value)
    val@mode <- "logical"
    Fortran(s, val)
  }
)

# --- constructors  ----

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


r2f_handlers[["cbind"]] <- function(e, scope) {
  .NotYetImplemented()
  ee <- lapply(e[-1], r2f, scope)
  ncols <- lapply(ee, function(f) {
    if (f@value@rank %in% c(0, 1)) {
      1
    } else if (f@value@rank == 2) {
      f@value@dims[[2]]
    }
  })
  ncols <- Reduce(\(a, b) call("+", a, b), ncols)
  ncols <- eval(ncols, scope@sizes)
}

is_function_call <- function(x) {
  is.call(x) && identical(x[[1L]], as.symbol("function"))
}
is_sapply_call <- function(x) is.call(x) && identical(x[[1L]], quote(sapply))
is_seq_along_call <- function(x) {
  is.call(x) && identical(x[[1L]], quote(seq_along))
}

new_local_closure <- function(fun, name = NULL) {
  stopifnot(is.function(fun), is.null(name) || is_string(name))
  LocalClosure(fun = fun, name = name)
}

as_local_closure <- function(fun_expr, env, name = NULL) {
  stopifnot(is_function_call(fun_expr))
  new_local_closure(eval(fun_expr, envir = env), name = name)
}

scope_root <- function(scope) {
  stopifnot(inherits(scope, "quickr_scope"))
  while (
    !identical(scope@kind, "subroutine") &&
      inherits(parent.env(scope), "quickr_scope")
  ) {
    scope <- parent.env(scope)
  }
  scope
}

compile_internal_subroutine <- function(
  proc_name,
  closure_obj,
  parent_scope,
  formal_vars,
  res_var,
  forbid_superassign = character()
) {
  stopifnot(is_string(proc_name), inherits(closure_obj, LocalClosure))
  fun <- closure_obj@fun
  stopifnot(is.function(fun))
  stopifnot(is.null(res_var) || inherits(res_var, Variable))
  stopifnot(is.character(forbid_superassign))
  forbid_superassign <- unique(forbid_superassign)
  if (length(forbid_superassign) && any(!nzchar(forbid_superassign))) {
    stop("forbid_superassign must contain only non-empty names")
  }

  formal_names <- names(formals(fun)) %||% character()
  if (length(formal_names) && any(!nzchar(formal_names))) {
    stop("local closures must have only named arguments")
  }
  stopifnot(is.list(formal_vars))
  if (!identical((names(formal_vars) %||% character()), formal_names)) {
    stop("internal error: formal vars do not match closure formals")
  }

  used_names <- unique(all.vars(body(fun), functions = FALSE))

  formal_scope <- new_scope(closure = NULL, parent = parent_scope)
  attr(formal_scope, "kind") <- "closure_formals"

  proc_scope <- new_scope(fun, parent = formal_scope)
  attr(proc_scope, "kind") <- "closure"
  attr(proc_scope, "host_scope") <- scope_root(parent_scope)
  attr(proc_scope, "forbid_superassign") <- forbid_superassign

  arg_names <- character()
  for (nm in formal_names) {
    var <- formal_vars[[nm]]
    stopifnot(inherits(var, Variable))

    arg_var <- Variable(mode = var@mode, dims = var@dims, name = nm)
    if (logical_as_int(var)) {
      attr(arg_var, "logical_as_int") <- TRUE
    }
    formal_scope[[nm]] <- arg_var
    arg_names <- c(arg_names, nm)
  }

  res_name <- NULL
  if (!is.null(res_var)) {
    res_name <- "res"
    while (res_name %in% c(formal_names, used_names)) {
      res_name <- paste0(res_name, "_")
    }
    res_var@name <- res_name
    proc_scope[[res_name]] <- res_var
    arg_names <- c(arg_names, res_name)
  }

  body_expr <- body(fun)
  stmts <- if (is_call(body_expr, quote(`{`))) {
    as.list(body_expr)[-1L]
  } else {
    list(body_expr)
  }
  if (!length(stmts)) {
    stop("closure body must not be empty")
  }

  body_prefix <- character()
  assign_code <- character()
  if (is.null(res_var)) {
    if (is.null(last(stmts))) {
      stmts <- drop_last(stmts)
    }
    if (length(stmts)) {
      body_prefix <- lapply(stmts, function(stmt) r2f(stmt, proc_scope))
      body_prefix <- str_flatten_lines(body_prefix)
    }
  } else {
    last_expr <- last(stmts)
    prefix <- drop_last(stmts)

    body_prefix <- lapply(prefix, function(stmt) r2f(stmt, proc_scope))
    body_prefix <- str_flatten_lines(body_prefix)

    h <- new_hoist(proc_scope)
    expr <- r2f(last_expr, proc_scope, hoist = h)
    if (is.null(expr@value)) {
      stop(structure(
        list(message = "local closure does not return a value"),
        class = c("quickr_void_closure_return", "error", "condition")
      ))
    }
    if (is.null(expr@value@mode)) {
      stop("could not infer closure return type")
    }
    if (is.null(res_var@mode)) {
      res_var@mode <- expr@value@mode
      res_var@dims <- expr@value@dims
      proc_scope[[res_name]] <- res_var
    }
    if (!identical(expr@value@mode, res_var@mode)) {
      stop(
        "closure result mode (",
        expr@value@mode,
        ") does not match output mode (",
        res_var@mode,
        ")"
      )
    }
    if (passes_as_scalar(res_var)) {
      if (!passes_as_scalar(expr@value)) {
        stop("closure must return a scalar for scalar outputs")
      }
    } else {
      if (passes_as_scalar(expr@value)) {
        stop("closure must return an array for array outputs")
      }
      if (expr@value@rank != res_var@rank) {
        stop("closure result rank does not match output rank")
      }
      if (
        !identical(
          dims2f(expr@value@dims, proc_scope),
          dims2f(res_var@dims, proc_scope)
        )
      ) {
        stop("closure result shape does not match output shape")
      }
    }
    assign_code <- h$render(glue("{res_name} = {expr}"))
  }

  vars_formals <- scope_vars(formal_scope)
  vars_locals <- scope_vars(proc_scope)
  locals <- vars_locals
  if (!is.null(res_name)) {
    locals <- locals[setdiff(names(locals), res_name)]
  }

  if (length(locals)) {
    bad <- map_lgl(locals, function(v) {
      !passes_as_scalar(v) &&
        grepl(":", dims2f(v@dims, proc_scope), fixed = TRUE)
    })
    if (any(bad)) {
      stop("closures cannot declare allocatable locals yet")
    }
  }

  out_arg <- if (is.null(res_name)) {
    list()
  } else {
    setNames(list(proc_scope[[res_name]]), res_name)
  }
  vars_declared <- c(vars_formals, out_arg, locals)

  decls <- c(
    emit_decls(
      vars_formals,
      formal_scope,
      intents = rep(list("intent(in)"), length(vars_formals)),
      assumed_shape = TRUE,
      allow_allocatable = FALSE
    ),
    if (!is.null(res_name)) {
      emit_decls(
        out_arg,
        formal_scope,
        intents = list("intent(out)"),
        assumed_shape = TRUE,
        allow_allocatable = FALSE
      )
    },
    if (length(locals)) emit_decls(locals, proc_scope) else character()
  )

  body_code <- str_flatten_lines(body_prefix, assign_code)
  used_iso_bindings <- iso_c_binding_symbols(
    vars = vars_declared,
    body_code = body_code,
    logical_is_c_int = logical_as_int,
    uses_rng = FALSE
  )
  use_iso <- if (length(used_iso_bindings)) {
    glue("use iso_c_binding, only: {str_flatten_commas(used_iso_bindings)}")
  } else {
    NULL
  }

  proc_sig <- str_flatten_commas(arg_names)
  proc_code <- glue(
    "
    subroutine {proc_name}({proc_sig})
    {indent(str_flatten_lines(use_iso, \"implicit none\"), 2)}

    {indent(str_flatten_lines(decls), 2)}

    {indent(body_code, 2)}
    end subroutine
    ",
    .null = ""
  )
  proc_code <- glue::trim(proc_code)

  proc_code <- insert_fortran_line_continuations(
    proc_code,
    preserve_attributes = FALSE
  )

  list(
    name = proc_name,
    code = proc_code,
    captures = character(),
    res = res_name,
    res_var = res_var
  )
}

closure_last_expr <- function(fun) {
  stopifnot(is.function(fun))
  body_expr <- body(fun)
  if (is_call(body_expr, quote(`{`))) {
    stmts <- as.list(body_expr)[-1L]
    if (!length(stmts)) {
      stop("closure body must not be empty")
    }
    last(stmts)
  } else {
    body_expr
  }
}

compile_closure_call <- function(
  call_expr,
  closure_obj,
  proc_name,
  scope,
  ...,
  hoist,
  needs_value
) {
  stopifnot(
    is.call(call_expr),
    inherits(closure_obj, LocalClosure),
    is_string(proc_name),
    inherits(scope, "quickr_scope"),
    inherits(hoist, "environment"),
    is_bool(needs_value)
  )

  fun <- closure_obj@fun
  call_expr <- match.call(fun, call_expr)
  args_expr <- as.list(call_expr)[-1L]

  formal_names <- names(formals(fun)) %||% character()
  if (!identical((names(args_expr) %||% character()), formal_names)) {
    stop("internal error: match.call did not align closure args")
  }

  args_f <- lapply(args_expr, r2f, scope, ..., hoist = hoist)
  formal_vars <- imap(args_f, function(f, nm) {
    stopifnot(inherits(f, Fortran), inherits(f@value, Variable))
    v <- Variable(mode = f@value@mode, dims = f@value@dims, name = nm)
    if (logical_as_int_symbol(f@value)) {
      attr(v, "logical_as_int") <- TRUE
    }
    v
  })
  names(formal_vars) <- formal_names

  last_expr <- closure_last_expr(fun)

  if (is.null(last_expr)) {
    if (needs_value) {
      stop("local closure calls that return `NULL` cannot be used as values")
    }
    proc <- compile_internal_subroutine(
      proc_name,
      closure_obj,
      scope,
      formal_vars = formal_vars,
      res_var = NULL
    )
    scope_root(scope)@add_internal_proc(proc)

    call_args <- unname(args_f)
    if (length(call_args)) {
      return(Fortran(glue("call {proc$name}({str_flatten_commas(call_args)})")))
    }
    return(Fortran(glue("call {proc$name}()")))
  }

  proc <- if (!needs_value) {
    tryCatch(
      compile_internal_subroutine(
        proc_name,
        closure_obj,
        scope,
        formal_vars = formal_vars,
        res_var = Variable()
      ),
      quickr_void_closure_return = function(e) {
        compile_internal_subroutine(
          proc_name,
          closure_obj,
          scope,
          formal_vars = formal_vars,
          res_var = NULL
        )
      }
    )
  } else {
    compile_internal_subroutine(
      proc_name,
      closure_obj,
      scope,
      formal_vars = formal_vars,
      res_var = Variable()
    )
  }
  scope_root(scope)@add_internal_proc(proc)

  if (!needs_value && is.null(proc$res)) {
    call_args <- unname(args_f)
    if (length(call_args)) {
      return(Fortran(glue("call {proc$name}({str_flatten_commas(call_args)})")))
    }
    return(Fortran(glue("call {proc$name}()")))
  }

  res_var <- proc$res_var
  if (is.null(res_var@mode)) {
    stop("internal error: could not infer closure return type")
  }

  tmp <- hoist$declare_tmp(mode = res_var@mode, dims = res_var@dims)
  call_args <- c(unname(args_f), tmp@name)
  call_stmt <- glue("call {proc$name}({str_flatten_commas(call_args)})")

  if (needs_value) {
    hoist$emit(call_stmt)
    Fortran(tmp@name, tmp)
  } else {
    Fortran(call_stmt)
  }
}

compile_closure_call_assignment <- function(
  target_name,
  call_expr,
  scope,
  ...
) {
  stopifnot(
    is_string(target_name),
    is.call(call_expr),
    is.symbol(call_expr[[1L]])
  )
  hoist <- list(...)$hoist
  if (is.null(hoist)) {
    hoist <- new_hoist(scope)
  }

  closure_name <- as.character(call_expr[[1L]])
  closure_obj <- scope[[closure_name]]
  if (!inherits(closure_obj, LocalClosure)) {
    stop("internal error: expected a LocalClosure")
  }

  target_var <- get0(target_name, scope)
  target_exists <- inherits(target_var, Variable)

  fun <- closure_obj@fun
  if (is.null(closure_last_expr(fun))) {
    stop("local closure calls that return `NULL` cannot be assigned")
  }
  call_expr <- match.call(fun, call_expr)
  args_expr <- as.list(call_expr)[-1L]
  formal_names <- names(formals(fun)) %||% character()
  if (!identical((names(args_expr) %||% character()), formal_names)) {
    stop("internal error: match.call did not align closure args")
  }

  args_f <- lapply(args_expr, r2f, scope, ...)
  formal_vars <- imap(args_f, function(f, nm) {
    stopifnot(inherits(f, Fortran), inherits(f@value, Variable))
    v <- Variable(mode = f@value@mode, dims = f@value@dims, name = nm)
    if (logical_as_int_symbol(f@value)) {
      attr(v, "logical_as_int") <- TRUE
    }
    v
  })
  names(formal_vars) <- formal_names

  return_names <- attr(scope, "return_names", exact = TRUE) %||% character()
  res_var <- if (target_exists) {
    out <- Variable(
      mode = target_var@mode,
      dims = target_var@dims,
      name = target_name
    )
    if (logical_as_int(target_var)) {
      attr(out, "logical_as_int") <- TRUE
    }
    out
  } else {
    Variable()
  }

  proc <- compile_internal_subroutine(
    closure_name,
    closure_obj,
    scope,
    formal_vars = formal_vars,
    res_var = res_var
  )
  if (!target_exists) {
    inferred_res_var <- proc$res_var
    if (is.null(inferred_res_var@mode)) {
      stop("internal error: could not infer closure return type")
    }
    target_var <- Variable(
      mode = inferred_res_var@mode,
      dims = inferred_res_var@dims
    )
    target_var@name <- target_name
    if (
      identical(inferred_res_var@mode, "logical") &&
        target_name %in% return_names
    ) {
      attr(target_var, "logical_as_int") <- TRUE
      # Recompile with the correct storage for the output dummy argument.
      res_var <- Variable(mode = target_var@mode, dims = target_var@dims)
      res_var@name <- target_name
      attr(res_var, "logical_as_int") <- TRUE
      proc <- compile_internal_subroutine(
        closure_name,
        closure_obj,
        scope,
        formal_vars = formal_vars,
        res_var = res_var
      )
    }
    scope[[target_name]] <- target_var
  }
  scope_root(scope)@add_internal_proc(proc)

  arg_reads_target <- any(map_lgl(args_expr, function(e) {
    any(all.vars(e, functions = FALSE) == target_name)
  }))

  res_target <- target_name
  post <- character()
  if (arg_reads_target) {
    tmp <- hoist$declare_tmp(mode = target_var@mode, dims = target_var@dims)
    if (logical_as_int(target_var)) {
      attr(tmp, "logical_as_int") <- TRUE
    }
    res_target <- tmp@name
    post <- glue("{target_name} = {res_target}")
  }

  call_args <- c(
    unname(args_f),
    res_target
  )

  if (target_exists) {
    target_var@modified <- TRUE
    scope[[target_name]] <- target_var
  }

  Fortran(glue(
    "
    call {proc$name}({str_flatten_commas(call_args)})
    {str_flatten_lines(post)}
    "
  ))
}

compile_sapply_assignment <- function(out_name, call_expr, scope, ...) {
  stopifnot(is_string(out_name), is_sapply_call(call_expr))
  hoist <- list(...)$hoist
  if (is.null(hoist)) {
    hoist <- new_hoist(scope)
  }
  args <- as.list(call_expr)[-1L]
  if (length(args) < 2L) {
    stop("sapply() requires at least 2 arguments")
  }

  seq_call <- args[[1L]]
  fun_expr <- args[[2L]]
  simplify <- args$simplify
  if (!is.null(simplify) && !is_missing(simplify)) {
    if (is_string(simplify) && identical(simplify, "array")) {
      # ok
    } else if (is_bool(simplify) && isTRUE(simplify)) {
      simplify <- NULL
    } else {
      stop('Only `sapply(..., simplify = "array")` is supported.')
    }
  }

  out_var <- get0(out_name, scope)
  if (is.null(out_var) || !inherits(out_var, Variable)) {
    stop("sapply() result must be assigned to an existing variable: ", out_name)
  }
  if (out_var@rank < 1L) {
    stop("sapply() output must be an array: ", out_name)
  }
  if (
    out_var@rank > 2L && (is.null(simplify) || !identical(simplify, "array"))
  ) {
    stop('sapply() that returns arrays requires `simplify = "array"`.')
  }

  env <- environment(scope@closure)
  if (is.symbol(fun_expr)) {
    fun_name <- as.character(fun_expr)
    closure_obj <- scope[[fun_name]]
    if (!inherits(closure_obj, LocalClosure)) {
      stop("unsupported FUN in sapply(): ", fun_name)
    }
    proc_name <- fun_name
  } else if (is_function_call(fun_expr)) {
    proc_name <- scope_root(scope)@get_unique_proc(prefix = "closure")
    closure_obj <- as_local_closure(fun_expr, env, name = proc_name)
  } else {
    stop("unsupported FUN in sapply(); use a local closure or function(i) ...")
  }

  formal_names <- names(formals(closure_obj@fun)) %||% character()
  if (length(formal_names) != 1L || any(!nzchar(formal_names))) {
    stop("sapply() FUN must have exactly one named argument")
  }

  res_dims <- if (out_var@rank == 1L) {
    NULL
  } else {
    out_var@dims[seq_len(out_var@rank - 1L)]
  }
  res_var <- Variable(mode = out_var@mode, dims = res_dims)
  if (logical_as_int(out_var)) {
    attr(res_var, "logical_as_int") <- TRUE
  }

  formal_vars <- list(Variable(mode = "integer", name = formal_names[[1L]]))
  names(formal_vars) <- formal_names
  proc <- compile_internal_subroutine(
    proc_name,
    closure_obj,
    scope,
    formal_vars,
    res_var,
    forbid_superassign = out_name
  )
  scope_root(scope)@add_internal_proc(proc)

  out_target <- out_name
  post_stmts <- character()
  if (out_name %in% all.vars(body(closure_obj@fun), functions = FALSE)) {
    tmp_out <- hoist$declare_tmp(mode = out_var@mode, dims = out_var@dims)
    if (logical_as_int(out_var)) {
      attr(tmp_out, "logical_as_int") <- TRUE
    }
    out_target <- tmp_out@name
    post_stmts <- glue("{out_name} = {out_target}")
  }

  idx <- scope@get_unique_var("integer")
  if (is_seq_along_call(seq_call) && length(seq_call) == 2L) {
    seq_target_f <- r2f(seq_call[[2L]], scope, ...)
    last_i <- glue("size({seq_target_f})")
  } else if (
    is.call(seq_call) &&
      identical(seq_call[[1L]], quote(seq_len)) &&
      length(seq_call) == 2L
  ) {
    last_i <- r2f(seq_call[[2L]], scope, ...)
  } else {
    stop(
      "only sapply(seq_along(x), FUN) and sapply(seq_len(n), FUN) are supported"
    )
  }

  res_target <- if (out_var@rank == 1L) {
    glue("{out_target}({idx@name})")
  } else {
    subs <- c(rep(":", out_var@rank - 1L), idx@name)
    glue("{out_target}({str_flatten_commas(subs)})")
  }

  call_args <- str_flatten_commas(c(
    idx@name,
    res_target
  ))

  out_var@modified <- TRUE
  scope[[out_name]] <- out_var

  Fortran(glue(
    "
    do {idx@name} = 1_c_int, {last_i}
      call {proc_name}({call_args})
    end do
    {str_flatten_lines(post_stmts)}
    "
  ))
}


r2f_handlers[["<-"]] <- function(args, scope, ...) {
  target <- args[[1]]
  if (is.call(target)) {
    # given a call like `foo(x) <- y`, dispatch to `foo<-`
    target_callable <- target[[1]]
    stopifnot(is.symbol(target_callable))
    name <- as.symbol(paste0(as.character(target_callable), "<-"))
    handler <- get_r2f_handler(name)
    return(handler(args, scope, ...)) # new hoist target
  }

  # It sure seems like it's be nice if the Fortran() constructor
  # took mode and dims as args directly,
  # without needing to go through Variable...
  stopifnot(is.symbol(target))
  name <- as.character(target)

  rhs <- args[[2]]

  # Local closure definition: `f <- function(i) ...`
  if (is_function_call(rhs)) {
    scope[[name]] <- as_local_closure(
      rhs,
      environment(scope@closure),
      name = name
    )
    return(Fortran(""))
  }

  # Local closure call: `x <- f(...)` where `f <- function(...) ...` in scope.
  if (
    is.call(rhs) &&
      is.symbol(rhs[[1L]]) &&
      inherits(scope[[as.character(rhs[[1L]])]], LocalClosure)
  ) {
    return(compile_closure_call_assignment(name, rhs, scope, ...))
  }

  # Targeted higher-order lowering: `out <- sapply(seq_along(x), f)`
  if (is_sapply_call(rhs)) {
    return(compile_sapply_assignment(name, rhs, scope, ...))
  }

  value <- r2f(rhs, scope, ...)

  # immutable / copy-on-modify usage of Variable()
  var <- get0(name, scope, inherits = FALSE)
  if (is.null(var) || !inherits(var, Variable)) {
    # The var does not exist -> this is a binding to a new symbol
    # Create a fresh Variable carrying only mode/dims and a new name.
    src <- value@value
    var <- Variable(mode = src@mode, dims = src@dims)
    fortran_name <- if (
      scope_is_closure(scope) && inherits(get0(name, scope), Variable)
    ) {
      make_shadow_fortran_name(scope, name)
    } else {
      name
    }
    var@name <- fortran_name
    # keep a reference to the R expression assigned, if available
    tryCatch(
      var@r <- attr(value, "r", TRUE),
      error = function(e) NULL
    )
    scope[[name]] <- var
  } else {
    # The var already exists, this assignment is a modification / reassignment
    check_assignment_compatible(var, value@value)
    var@modified <- TRUE
    # could probably drop this @modified property, and instead track
    # if the var populated by declare is identical at the end (e.g., perhaps by
    # address, or by attaching a unique id to each var, or ???)
    assign(name, var, scope)
  }

  Fortran(glue("{var@name} = {value}"))
}


r2f_handlers[["[<-"]] <- function(args, scope = NULL, ...) {
  # TODO: handle logical subsetting here, which must become a where a construct like:
  #   x[lgl] <- val
  # becomes
  # where (lgl)
  #   x = val
  # end where
  # ! but if {va} references {x}, it will only see the subset x, not the full {x}
  # e.g.,
  # sum(x) is not the same as `where lgl \n sum(x) \n end where`
  # ditto for ifelse() ?
  # e <- as.list(e)

  stopifnot(is_call(target_call <- args[[1L]], "["))

  pre <- NULL
  if (scope_is_closure(scope) && is.symbol(base <- target_call[[2L]])) {
    base_name <- as.character(base)
    local_base <- get0(base_name, scope, inherits = FALSE)
    if (is.null(local_base) || !inherits(local_base, Variable)) {
      parent_base <- get0(base_name, scope)
      if (!inherits(parent_base, Variable)) {
        stop("could not resolve symbol: ", base_name)
      }

      shadow <- Variable(mode = parent_base@mode, dims = parent_base@dims)
      shadow@name <- make_shadow_fortran_name(scope, base_name)
      if (logical_as_int(parent_base)) {
        attr(shadow, "logical_as_int") <- TRUE
      }
      scope[[base_name]] <- shadow

      pre <- glue("{shadow@name} = {parent_base@name}")
    }
  }

  target <- r2f(target_call, scope, ...)
  value <- r2f(args[[2L]], scope, ...)

  Fortran(str_flatten_lines(pre, glue("{target} = {value}")))
}

r2f_handlers[["<<-"]] <- function(args, scope, ..., hoist = NULL) {
  if (is.null(scope) || !identical(scope@kind, "closure")) {
    stop("<<- is only supported inside local closures")
  }

  target <- args[[1L]]
  if (is.call(target)) {
    target_callable <- target[[1L]]
    stopifnot(is.symbol(target_callable))
    name <- as.symbol(paste0(as.character(target_callable), "<<-"))
    handler <- get_r2f_handler(name)
    return(handler(args, scope, ..., hoist = hoist))
  }

  stopifnot(is.symbol(target))
  name <- as.character(target)

  formal_names <- names(formals(scope@closure)) %||% character()
  if (name %in% formal_names) {
    stop("<<- targets must not shadow closure formals: ", name)
  }

  forbidden <- attr(scope, "forbid_superassign", exact = TRUE) %||% character()
  if (name %in% forbidden) {
    stop("closure must not superassign to its output variable: ", name)
  }

  host_scope <- scope@host_scope %||% stop("internal error: missing host scope")
  host_var <- get0(name, host_scope)
  if (!inherits(host_var, Variable)) {
    stop(
      "<<- targets must resolve to an existing variable in the enclosing quick() scope: ",
      name
    )
  }

  host_var@modified <- TRUE
  host_scope[[name]] <- host_var

  value <- r2f(args[[2L]], scope, ..., hoist = hoist)
  check_assignment_compatible(host_var, value@value)

  Fortran(glue("{host_var@name} = {value}"))
}

r2f_handlers[["[<<-"]] <- function(args, scope, ..., hoist = NULL) {
  if (is.null(scope) || !identical(scope@kind, "closure")) {
    stop("<<- is only supported inside local closures")
  }

  stopifnot(is_call(target <- args[[1L]], "["))
  subset_call <- target

  idx_args <- as.list(subset_call)[-1L]
  idx_args <- idx_args[-1L]
  drop <- idx_args$drop %||% TRUE
  if (!isTRUE(drop)) {
    stop("drop = FALSE not supported for superassignment")
  }

  base <- subset_call[[2L]]
  if (!is.symbol(base)) {
    stop("only superassignment to x[...] is supported")
  }
  name <- as.character(base)

  formal_names <- names(formals(scope@closure)) %||% character()
  if (name %in% formal_names) {
    stop("<<- targets must not shadow closure formals: ", name)
  }

  forbidden <- attr(scope, "forbid_superassign", exact = TRUE) %||% character()
  if (name %in% forbidden) {
    stop("closure must not superassign to its output variable: ", name)
  }

  host_scope <- scope@host_scope %||% stop("internal error: missing host scope")
  host_var <- get0(name, host_scope)
  if (!inherits(host_var, Variable)) {
    stop(
      "<<- targets must resolve to an existing variable in the enclosing quick() scope: ",
      name
    )
  }

  host_var@modified <- TRUE
  host_scope[[name]] <- host_var

  target_scope <- scope@new_child("block")
  alias <- paste0(name, "__hostref_")
  while (exists(alias, target_scope, inherits = TRUE)) {
    alias <- paste0(alias, "_")
  }
  proxy <- Variable(
    mode = host_var@mode,
    dims = host_var@dims,
    name = host_var@name
  )
  if (logical_as_int(host_var)) {
    attr(proxy, "logical_as_int") <- TRUE
  }
  target_scope[[alias]] <- proxy
  subset_call[[2L]] <- as.symbol(alias)
  target <- r2f(subset_call, target_scope, ..., hoist = hoist)
  value <- r2f(args[[2L]], scope, ..., hoist = hoist)
  Fortran(glue("{target} = {value}"))
}

reduce_promoted_mode <- function(...) {
  getmode <- function(d) {
    if (inherits(d, Fortran)) {
      d <- d@value
    }
    if (inherits(d, Variable)) {
      return(d@mode)
    }
    if (is.list(d) && length(d)) {
      lapply(d, getmode)
    }
  }
  modes <- unique(unlist(getmode(list(...))))

  if ("double" %in% modes) {
    "double"
  } else if ("integer" %in% modes) {
    "integer"
  } else if ("logical" %in% modes) {
    "logical"
  } else {
    NULL
  }
}


r2f_handlers[["="]] <- r2f_handlers[["<-"]]

r2f_handlers[["logical"]] <- function(args, scope, ...) {
  Fortran(".false.", Variable(mode = "logical", dims = r2dims(args, scope)))
}
attr(r2f_handlers[["logical"]], "match.fun") <- FALSE

r2f_handlers[["integer"]] <- function(args, scope, ...) {
  Fortran("0", Variable(mode = "integer", dims = r2dims(args, scope)))
}
attr(r2f_handlers[["integer"]], "match.fun") <- FALSE

r2f_handlers[["double"]] <- function(args, scope, ...) {
  Fortran("0", Variable(mode = "double", dims = r2dims(args, scope)))
}
attr(r2f_handlers[["double"]], "match.fun") <- FALSE

r2f_handlers[["numeric"]] <- r2f_handlers[["double"]]
attr(r2f_handlers[["numeric"]], "match.fun") <- FALSE

r2f_handlers[["runif"]] <- function(args, scope, ..., hoist = NULL) {
  attr(scope, "uses_rng") <- TRUE

  dims <- r2dims(args$n, scope)
  var <- Variable("double", dims)

  min <- args$min %||% 0
  max <- args$max %||% 1
  default_min <- identical(min, 0) || identical(min, 0L)
  default_max <- identical(max, 1) || identical(max, 1L)

  if (default_min && default_max) {
    get1rand <- "unif_rand()"
  } else if (default_min) {
    max <- r2f(max, scope, ..., hoist = hoist)
    get1rand <- glue("unif_rand() * {max}")
  } else {
    max <- r2f(max, scope, ..., hoist = hoist)
    min <- r2f(min, scope, ..., hoist = hoist)
    get1rand <- glue("({min} + (unif_rand() * ({max} - {min})))")
  }

  if (passes_as_scalar(var)) {
    fortran <- get1rand
  } else {
    tmp_i <- scope@get_unique_var("integer") ## would be better as uint64...
    fortran <- glue("[({get1rand}, {tmp_i}=1, {dims[[1L]]})]")
  }

  Fortran(fortran, var)
}


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
  if (!passes_as_scalar(out@value)) {
    stop("array(data=) must be a scalar for now")
  }

  out@value <- Variable(
    mode = out@value@mode,
    dims = r2dims(args$dim, scope)
  )
  out
}


conform <- function(..., mode = NULL) {
  var <- NULL
  # technically, types are implicit promoted, but we'll let <- handle that.
  for (var in drop_nulls(list(...))) {
    if (passes_as_scalar(var)) {
      next
    } else {
      break
    }
  }
  if (is.null(var)) {
    NULL
  } else {
    Variable(mode %||% var@mode, var@dims)
  }
}


# ---- printers ----

r2f_handlers[["cat"]] <- function(args, scope, ...) {
  args <- lapply(args, r2f, scope, ...)
  # can do a lot more here still, just a POC for now
  stopifnot(length(args) == 1, typeof(args[[1]]) == "character")
  label <- args[[1]]
  if (!endsWith(label, "\n")) {
    stop("cat(<strings>) must end with '\n'")
  }
  label <- substring(label, 1, nchar(label) - 1)

  Fortran(glue('call labelpr("{label}", {nchar(label)})'))
}

r2f_handlers[["print"]] <- function(args, scope = NULL, ...) {
  # args <- lapply(as.list(e)[-1], r2f, scope)
  # args <- as.list(e)[-1]
  # can do alot more here still, just a POC for now
  stopifnot(length(args) == 1, typeof(args[[1]]) == "symbol")
  name <- args[[1]]
  var <- get0(as.character(name), scope)
  if (!inherits(var, Variable)) {
    stop("could not resolve symbol: ", as.character(name))
  }
  name <- as.character(name)
  if (var@mode == "logical") {
    name <- sprintf("(%s/=0)", name)
  }
  label <- ""
  # browser()
  if (passes_as_scalar(var)) {
    # } "scalar"
    # paste0(c(var@mode, scalar) collapse = "_"),
    printer <- switch(
      var@mode,
      logical = ,
      integer = "intpr1",
      double = "dblepr1",
      {
        print(var)
        stop("Unsupported type in print()")
      }
    )

    Fortran(glue('call {printer}("{label}", {nchar(label)}, {name})'))
  } else {
    printer <- switch(
      var@mode,
      logical = ,
      integer = "intpr",
      double = "dblepr",
      {
        print(var)
        stop("Unsupported type in print()")
      }
    )

    Fortran(glue(
      'call {printer}("{label}", {nchar(label)}, {name}, size({name}))'
    ))
  }
}

# r2f_handlers[["ifelse"]] <- function(e, scope) {
#   # TODO:
#   #   <- and [<- need to be aware of this construct for it to make sense.
#   .[test, yes, no] <- lapply(e[-1], r2f, scope)
#   Fortran(glue("where ({test}}
#                 {indent(yes)}
#                 elsewhere
#                 {indent({no})
#                 end where"))
# }

r2f_handlers[["length"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x})"), Variable("integer"))
}
r2f_handlers[["nrow"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x}, 1)"), Variable("integer"))
}
r2f_handlers[["ncol"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("size({x}, 2)"), Variable("integer"))
}
r2f_handlers[["dim"]] <- function(args, scope, ...) {
  x <- r2f(args[[1]], scope, ...)
  Fortran(glue("shape({x})"), Variable("integer", x@value@rank))
}


# this is just `[` handler
r2f_slice <- function(args, scope, ...) {}


# ---- control flow ----

r2f_handlers[["if"]] <- function(args, scope, ..., hoist = NULL) {
  # cond uses the current hoist context.
  cond <- r2f(args[[1]], scope, ..., hoist = hoist)

  # true and false branchs gets their own hoist target.
  true <- r2f(args[[2]], scope, ..., hoist = NULL)

  if (length(args) == 2) {
    Fortran(glue(
      "
      if ({cond}) then
      {indent(true)}
      end if
      "
    ))
  } else {
    false <- r2f(args[[3]], scope, ..., hoist = NULL)
    Fortran(glue(
      "
      if ({cond}) then
      {indent(true)}
      else
      {indent(false)}
      end if
      "
    ))
  }
}


# TODO: return

# ---- repeat ----
r2f_handlers[["repeat"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  body <- r2f(args[[1]], scope, ...)
  Fortran(glue(
    "do
    {indent(body)}
    end do
    "
  ))
}

# ---- break ----
r2f_handlers[["break"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 0L)
  Fortran("exit")
}

# ---- break ----
r2f_handlers[["next"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 0L)
  Fortran("cycle")
}

# ---- while ----
r2f_handlers[["while"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 2L)
  cond <- r2f(args[[1]], scope, ...)
  body <- r2f(args[[2]], scope, ...) ## should we set a new hoist target here?
  Fortran(glue(
    "do while ({cond})
    {indent(body)}
    end do
    "
  ))
}

## ---- for ----
r2f_iterable <- function(e, scope, ...) {
  .NotYetImplemented()

  if (is.symbol(e)) {
    var <- get(e, scope)
    iterable <- r2f(...)
  }

  # list(var, iterable, body_prefix)
}


r2f_handlers[["for"]] <- function(args, scope, ...) {
  .[var, iterable, body] <- args
  stopifnot(is.symbol(var))
  var <- as.character(var)
  existing <- get0(var, scope, inherits = FALSE)
  var_name <- if (inherits(existing, Variable) && !is.null(existing@name)) {
    existing@name
  } else if (scope_is_closure(scope) && inherits(get0(var, scope), Variable)) {
    make_shadow_fortran_name(scope, var)
  } else {
    var
  }
  scope[[var]] <- Variable(mode = "integer", name = var_name)

  iterable <- r2f_iterable_handlers[[as.character(iterable[[1]])]](
    iterable,
    scope,
    ...
  )
  body <- r2f(body, scope, ...)

  Fortran(glue(
    "do {var_name} = {iterable}
    {indent(body)}
    end do
    "
  ))
}

r2f_iterable_handlers := new.env()

r2f_iterable_handlers[["seq_len"]] <- function(e, scope, ...) {
  x <- as.list(e)[-1]
  if (length(x) != 1) {
    stop("too many args to seq_len()")
  }
  x <- x[[1]]
  start <- 1L
  end <- r2f(x, scope, ...)
  glue("{start}, {end}")
}

r2f_iterable_handlers[["seq"]] <- function(e, scope, ...) {
  ee <- match.call(seq.default, e)
  ee <- whole_doubles_to_ints(ee)

  start <- r2f(ee$from, scope, ...)
  end <- r2f(ee$to, scope, ...)
  step <- if (is.null(ee$by)) {
    glue("sign(1, {end}-{start})")
  } else {
    r2f(ee$by, scope, ...)
  }

  str_flatten_commas(
    start,
    end,
    step
  )
}

r2f_iterable_handlers[[":"]] <- function(e, scope, ...) {
  ee <- whole_doubles_to_ints(e)
  .[start, end] <- as.list(ee)[-1] |> lapply(r2f, scope, ...)

  glue("{start}, {end}, sign(1, {end}-{start})")
}


r2f_iterable_handlers[["seq_along"]] <- function(e, scope, ...) {
  x <- as.list(e)[-1]
  if (length(x) != 1) {
    stop("too many args to seq_along()")
  }
  x <- x[[1]]
  start <- 1
  end <- sprintf("size(%s)", r2f(x, scope, ...))
  glue("{start}, {end}")
}


# ---- helpers ----

check_call <- function(e, nargs) {
  if (length(e) != (nargs + 1L)) {
    stop("Too many args to: ", as.character(e[[1L]]))
  }
}
