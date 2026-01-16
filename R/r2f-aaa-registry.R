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
  if (!is.null(dest_supported)) {
    attr(fun, "dest_supported") <- dest_supported
  }
  if (!is.null(dest_infer)) {
    attr(fun, "dest_infer") <- dest_infer
    # covr rewrites function bindings in the namespace; resolving by name at call
    # time ensures instrumented/rebound functions are respected. We keep the
    # function object for robustness (e.g., anonymous functions) and additionally
    # store the name when `dest_infer` is passed as a symbol.
    dest_infer_expr <- substitute(dest_infer)
    if (is.symbol(dest_infer_expr)) {
      attr(fun, "dest_infer_name") <- as.character(dest_infer_expr)
    }
  }
  if (!is.null(match_fun) && !isTRUE(match_fun)) {
    attr(fun, "match.fun") <- match_fun
  }
  for (nm in name) {
    r2f_handlers[[nm]] <- fun
  }
  invisible(fun)
}
