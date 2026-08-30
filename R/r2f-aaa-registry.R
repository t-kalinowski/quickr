# Handler registry and registration helpers.

r2f_handlers <- new.env(parent = emptyenv())

## ??? export as S7::convert() methods?
register_r2f_handler <- function(
  name,
  fun,
  dest_supported = NULL,
  dest_infer = NULL,
  match_fun = TRUE
) {
  stopifnot(is.function(fun))

  handler <- if (inherits(fun, R2FHandler)) fun else R2FHandler(fun)

  # Same hazard as `dest_infer` below, for the handler itself: the function
  # object is captured here, at build time, while covr rebinds its instrumented
  # copies into the namespace after the package has loaded. A handler registered
  # as a top-level named function would keep dispatching the copy taken here and
  # read as 0% covered however well it is tested. Record the name so
  # get_r2f_handler() can re-resolve it; the object stays authoritative, since
  # most handlers are anonymous literals with no name to resolve.
  handler@fun_name <- registered_fun_name(substitute(fun), fun)

  if (!is.null(dest_supported)) {
    handler@dest_supported <- isTRUE(dest_supported)
  }

  if (!is.null(dest_infer)) {
    handler@dest_infer <- dest_infer
    # covr rewrites function bindings in the namespace; resolving by name at call
    # time ensures instrumented/rebound functions are respected. We keep the
    # function object for robustness (e.g., anonymous functions) and additionally
    # store the name when `dest_infer` is passed as a symbol.
    dest_infer_expr <- substitute(dest_infer)
    if (is.symbol(dest_infer_expr)) {
      handler@dest_infer_name <- as.character(dest_infer_expr)
    } else {
      handler@dest_infer_name <- NULL
    }
  }

  if (isTRUE(match_fun)) {
    handler@match_fun <- NULL
  } else if (
    is.null(match_fun) || isFALSE(match_fun) || is.function(match_fun)
  ) {
    handler@match_fun <- match_fun
  } else {
    stop("match_fun must be TRUE, FALSE, NULL, or a function")
  }

  for (nm in name) {
    r2f_handlers[[nm]] <- handler
  }
  invisible(handler)
}


# The name to re-resolve a registered handler by, or NULL if there isn't one.
#
# `expr` is the unevaluated `fun` argument and `fun` its value. A name is only
# usable if `expr` is a symbol *and* it names this same function in a namespace
# -- the only environment covr rebinds into. That rules out the two non-literal
# registrations that are not namespace-level functions: the local `handler`
# closure built by register_unary_intrinsic() (a symbol, but bound in a call
# frame, where the name would mean something else on a later call) and
# `r2f_handlers[["<-"]]` (not a symbol at all).
registered_fun_name <- function(expr, fun) {
  if (!is.symbol(expr)) {
    return(NULL)
  }
  name <- as.character(expr)
  env <- environment(fun)
  if (!is.environment(env) || !isNamespace(env)) {
    return(NULL)
  }
  if (!identical(get0(name, envir = env, mode = "function"), fun)) {
    return(NULL)
  }
  name
}
