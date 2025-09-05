# Take parsed R code (anything returnable by base::str2lang()) and returns
# a Fortran object, which is a string of Fortran code and some attributes
# describing the value.
lang2fortran <- r2f <- function(
  e,
  scope = NULL,
  ...,
  calls = character(),
  hoist = NULL
) {
  ## 'hoist()' is a function that individual handlers can call to pre-emit some
  ## Fortran code. E.g., to setup a temporary variable if the generated Fortran
  ## code doesn't neatly translate into a single expression.
  hoisted <- character()
  if (is.null(hoist)) {
    delayedAssign("hoist_connection", textConnection("hoisted", "w", TRUE))
    hoist <- function(...) {
      writeLines(as.character(unlist(c(character(), ...))), hoist_connection)
    }
    # if performance with textConnection() becomes an issue, maybe switch to an
    # anonymous file(), though, each hoisting context is typically shortlived and
    # usually 0 lines are hoisted per context, and if they are hoisted, a small number.
  }

  fortran <- switch(
    typeof(e),
    language = {
      # a call
      handler <- get_r2f_handler(callable <- e[[1L]])

      match.fun <- attr(handler, "match.fun", TRUE)
      if (is.null(match.fun)) {
        match.fun <- get0(callable, parent.env(globalenv()), mode = "function")
        # this is a best effort to, eg. resolve `seq.default` from `seq`.
        # This should likely be moved into attaching the `match.fun` attr
        # to handlers, for more involved resolution (e.g., with getS3Method())
        if ("UseMethod" %in% all.names(body(match.fun))) {
          match.fun <- get0(
            paste0(callable, ".default"),
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
          calls = c(calls, as.character(callable)),
          hoist = hoist
        )) -> res
        if (inherits(res, "try-error")) {
          debugonce(handler)
          handler(
            as.list(e)[-1L],
            scope,
            ...,
            calls = c(calls, as.character(callable)),
            hoist = hoist
          )
        }

        res
      } else {
        handler(
          as.list(e)[-1L],
          scope,
          ...,
          calls = c(calls, as.character(callable)),
          hoist = hoist
        )
      }
    },

    integer = ,
    double = ,
    complex = ,
    logical = atomic2Fortran(e),

    symbol = {
      s <- as.character(e)
      # logicals that come in from R are passed as integer types,
      # so for all fortran ops we cast to logical with /=0
      if (
        !is.null(scope[[e]] -> val) &&
          val@mode == "logical" &&
          val@is_external
      ) {
        s <- paste0("(", s, "/=0)")
      }
      Fortran(s, value = scope[[e]])
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

  if (length(hoisted)) {
    combined <- str_flatten_lines(c(hoisted, fortran))
    attributes(combined) <- attributes(fortran)
    fortran <- combined
  }

  attr(fortran, "r") <- e
  fortran
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
  r2f(args[[1L]], scope, ...)
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
      if (x@value@rank == 0) {
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


r2f_handlers[["["]] <- function(
  args,
  scope,
  ...,
  hoist_mask = function(mask) FALSE
) {
  # only a subset of R's x[...] features can be translated here. `...` can only be:
  # - a single logical mask, of the same rank as `x`. returns a rank 1 vector.
  # - a number of arguments matching the rank of `x`, with each being
  #   an integer of rank 0 or 1. In this case, a rank 1 logical becomes
  #   converted to an integer with

  var <- args[[1]]
  var <- r2f(var, scope, ...)

  idxs <- whole_doubles_to_ints(args[-1])
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

  if (length(idxs) != var@value@rank) {
    stop(
      "number of args to x[...] must match the rank of x, received:",
      deparse1(as.call(c(quote(`[`, args))))
    )
  }

  drop <- args$drop %||% TRUE

  idxs <- lapply(idxs, function(subscript) {
    # if (!idx@value@rank %in% 0:1)
    #   stop("all args to x[...] must have rank 0 or 1",
    #        deparse1(as.call(c(quote(`[`,args )))))
    switch(
      paste0(subscript@value@mode, subscript@value@rank),
      logical0 = {
        Fortran(":", Variable("integer", NA))
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

  dims <- drop_nulls(lapply(idxs, \(idx) idx@value@dims[[1]]))
  outval <- Variable(var@value@mode, dims)
  Fortran(glue("{var}({str_flatten_commas(idxs)})"), outval)
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
  .[left, right] <- lapply(args, r2f, scope, ...)
  Fortran(glue("({left} + {right})"), conform(left@value, right@value))
}

r2f_handlers[["-"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  Fortran(glue("({left} - {right})"), conform(left@value, right@value))
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

  value <- args[[2]]
  value <- r2f(value, scope, ...)

  # immutable / copy-on-modify usage of Variable()
  if (is.null(var <- get0(name, scope))) {
    # this is a binding to a new symbol
    var <- value@value
    var@name <- name
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

  Fortran(glue("{name} = {value}"))
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

  stopifnot(is_call(target <- args[[1L]], "["))
  target <- r2f(target, scope)

  value <- r2f(args[[2L]], scope)
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

r2f_handlers[["integer"]] <- function(args, scope, ...) {
  Fortran("0", Variable(mode = "integer", dims = r2dims(args, scope)))
}

r2f_handlers[["double"]] <- function(args, scope, ...) {
  Fortran("0", Variable(mode = "double", dims = r2dims(args, scope)))
}

r2f_handlers[["numeric"]] <- r2f_handlers[["double"]]

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
  var <- get(name, envir = scope)
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
  scope[[var]] <- Variable(mode = "integer", name = var)

  iterable <- r2f_iterable_handlers[[as.character(iterable[[1]])]](
    iterable,
    scope
  )
  body <- r2f(body, scope, ...)

  Fortran(glue(
    "do {var} = {iterable}
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
  end <- r2f(x)
  glue("{start}, {end}")
}

r2f_iterable_handlers[["seq"]] <- function(e, scope) {
  ee <- match.call(seq.default, e)
  ee <- whole_doubles_to_ints(ee)

  start <- r2f(ee$from, scope)
  end <- r2f(ee$to, scope)
  step <- if (is.null(ee$by)) {
    glue("sign(1, {end}-{start})")
  } else {
    r2f(ee$by, scope)
  }

  str_flatten_commas(
    start,
    end,
    step
  )
}

r2f_iterable_handlers[[":"]] <- function(e, scope) {
  ee <- whole_doubles_to_ints(e)
  .[start, end] <- as.list(ee)[-1] |> lapply(r2f, scope)

  glue("{start}, {end}, sign(1, {end}-{start})")
}


r2f_iterable_handlers[["seq_along"]] <- function(e, scope) {
  x <- as.list(e)[-1]
  if (length(x) != 1) {
    stop("too many args to seq_along()")
  }
  x <- x[[1]]
  start <- 1
  end <- sprintf("size(%s)", r2f(x, scope))
  glue("{start}, {end}")
}


# ---- helpers ----

check_call <- function(e, nargs) {
  if (length(e) != (nargs + 1L)) {
    stop("Too many args to: ", as.character(e[[1L]]))
  }
}
