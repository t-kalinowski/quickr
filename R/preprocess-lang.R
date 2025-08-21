defuse_numeric_literals <- function(e) {
  if (is.call(e)) {
    e <- as.call(lapply(e, defuse_numeric_literals))
    if (
      is.symbol(e1 <- e[[1L]]) &&
        as.character(e1) %in% c("+", "-", "*", "/", "%%", "%/%", "^") &&
        all(map_lgl(e[-1L], is.atomic))
    ) {
      e <- eval(e, baseenv())
    }
  }
  e
}


ensure_last_expr_sym <- function(bdy) {
  if (!is_call(bdy, quote(`{`))) {
    stop("bad body, needs {")
  }
  if (!is.symbol(last_expr <- last(bdy))) {
    bdy[[length(bdy)]] <- call("<-", quote(out_), last_expr)
    bdy[[length(bdy) + 1L]] <- quote(out_)
  }
  bdy
}


whole_doubles_to_ints <- function(x) {
  walker <- function(x) {
    switch(
      typeof(x),
      double = if (trunc(x) == x) as.integer(x),
      language = as.call(lapply(x, walker)),
      list = lapply(x, walker),
      x
    )
  }
  walker(x)
}


substitute_unique_case_insensitive_symbols <- function(x) {
  # TODO: would be nice to fix case-insenstive name clashes
  # with automatic substitutions. Would be a little involved since
  # substitute will not replace tag names in a call, e.g.,
  # declare(type(<NAME> = ...)), NAME would need to be manually replaced.
  stopifnot(is.function(x))
  nms <- unique(c(all.names(body(x), names(formals(x)))))
  stop("not yet implemented")
}
