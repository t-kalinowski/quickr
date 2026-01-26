# r2f-aab-core.R
# Core translation engine for R-to-Fortran conversion.
#
# Take parsed R code (anything returnable by base::str2lang()) and returns
# a Fortran object, which is a string of Fortran code and some attributes
# describing the value.

# --- Hoisting Infrastructure ---

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

  declare_tmp <- function(mode, dims, logical_as_int = FALSE) {
    stopifnot(
      is_string(mode),
      is.null(dims) || is.list(dims),
      is_bool(logical_as_int)
    )
    ensure_block_scope()@get_unique_var(
      mode = mode,
      dims = dims,
      logical_as_int = logical_as_int
    )
  }

  render <- function(code) {
    code <- str_split_lines(code)
    if (!length(hoisted) && !has_block()) {
      return(str_flatten_lines(code))
    }

    stmts <- str_split_lines(hoisted, code)

    if (has_block()) {
      block_vars <- scope_vars(block_scope)
      decls <- emit_decls(block_vars, block_scope)
      allocs <- block_tmp_allocation_lines(block_vars, block_scope)
      if (length(allocs)) {
        stmts <- c(allocs, stmts)
      }
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


# --- Scope Helpers ---

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


# --- Main Translation Engine ---

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
        maybe_lower_local_closure_call(
          e,
          scope,
          ...,
          hoist = hoist,
          needs_value = !render_hoist
        ) %||%
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
      if (is.null(val) && inherits(scope, "quickr_scope")) {
        closure <- scope@closure
        arg_names <- if (is.null(closure)) NULL else names(formals(closure))
        if (!is.null(arg_names) && r_name %in% arg_names) {
          stop(
            "arg not declared: ",
            r_name,
            ". Add declare(type(",
            r_name,
            " = ...))",
            call. = FALSE
          )
        }
      }
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


# --- Atomic Conversion ---

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


# --- Handler Lookup ---

get_r2f_handler <- function(name) {
  stopifnot("All functions called must be named as symbols" = is.symbol(name))
  get0(name, r2f_handlers) %||%
    stop("Unsupported function: ", name, call. = FALSE)
}


# --- Destination Helpers ---

dest_supported_for_call <- function(call) {
  if (!is.call(call)) {
    return(FALSE)
  }
  unwrapped <- call
  while (is_call(unwrapped, "(") && length(unwrapped) == 2L) {
    unwrapped <- unwrapped[[2L]]
  }
  if (!is.call(unwrapped) || !is.symbol(unwrapped[[1L]])) {
    return(FALSE)
  }
  handler <- get0(as.character(unwrapped[[1L]]), r2f_handlers, inherits = FALSE)
  isTRUE(attr(handler, "dest_supported", exact = TRUE))
}

dest_infer_for_call <- function(call, scope) {
  if (!is.call(call)) {
    return(NULL)
  }
  unwrapped <- call
  while (is_call(unwrapped, "(") && length(unwrapped) == 2L) {
    unwrapped <- unwrapped[[2L]]
  }
  if (!is.call(unwrapped) || !is.symbol(unwrapped[[1L]])) {
    return(NULL)
  }
  handler <- get0(as.character(unwrapped[[1L]]), r2f_handlers, inherits = FALSE)
  infer <- attr(handler, "dest_infer", exact = TRUE)
  infer_name <- attr(handler, "dest_infer_name", exact = TRUE)

  infer_fun <- NULL
  if (is_string(infer_name)) {
    # Resolve dynamically from the handler's environment (typically the package
    # namespace) so instrumented/rebound functions are respected.
    infer_fun <- get0(
      infer_name,
      envir = environment(handler),
      mode = "function"
    )
  }
  if (!is.function(infer_fun)) {
    infer_fun <- infer
  }
  if (!is.function(infer_fun)) {
    return(NULL)
  }
  infer_fun(as.list(unwrapped)[-1L], scope)
}


# --- Default Handlers ---

r2f_default_handler <- function(args, scope = NULL, ..., calls) {
  # stopifnot(is.call(e), is.symbol(e[[1L]]))

  x <- lapply(args, r2f, scope = scope, calls = calls, ...)
  s <- sprintf("%s(%s)", last(calls), str_flatten_commas(x[-1]))
  Fortran(s)
}

.r2f_handler_not_implemented_yet <- function(e, scope, ...) {
  stop(
    gettextf("'%s' is not implemented yet", as.character(e[[1L]])),
    call. = FALSE
  )
}


# --- Utility ---

check_call <- function(e, nargs) {
  if (length(e) != (nargs + 1L)) {
    stop("Too many args to: ", as.character(e[[1L]]))
  }
}
