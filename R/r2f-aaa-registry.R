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
  }
  if (!is.null(match_fun) && !isTRUE(match_fun)) {
    attr(fun, "match.fun") <- match_fun
  }
  for (nm in name) {
    r2f_handlers[[nm]] <- fun
  }
  invisible(fun)
}
