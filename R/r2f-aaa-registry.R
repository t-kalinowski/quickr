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
  in_covr <- identical(Sys.getenv("R_COVR"), "true")
  if (!is.null(dest_supported)) {
    attr(fun, "dest_supported") <- dest_supported
  }
  if (!is.null(dest_infer)) {
    if (in_covr) {
      # covr rewrites function bindings in the namespace; storing a function
      # object here can bypass instrumentation. Under covr, store the name (when
      # available) and resolve it at call time.
      dest_infer_expr <- substitute(dest_infer)
      if (is.symbol(dest_infer_expr)) {
        attr(fun, "dest_infer") <- as.character(dest_infer_expr)
      } else {
        attr(fun, "dest_infer") <- dest_infer
      }
    } else {
      attr(fun, "dest_infer") <- dest_infer
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
