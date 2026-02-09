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
  } else if (is.null(match_fun) || isFALSE(match_fun) || is.function(match_fun)) {
    handler@match_fun <- match_fun
  } else {
    stop("match_fun must be TRUE, FALSE, NULL, or a function")
  }

  for (nm in name) {
    r2f_handlers[[nm]] <- handler
  }
  invisible(handler)
}
