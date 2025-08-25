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
  last_expr <- last(bdy)
  if (is.symbol(last_expr)) {
    n <- length(bdy)
    if (n >= 3L) {
      prev_expr <- bdy[[n - 1L]]
      if (
        is_call(prev_expr, quote(`<-`)) &&
          identical(prev_expr[[2L]], last_expr) &&
          is_call(prev_expr[[3L]], quote(list))
      ) {
        args <- as.list(prev_expr[[3L]])[-1L]
        if (!all(map_lgl(args, is.symbol))) {
          stop("all elements of return list must be symbols")
        }
        bdy_list <- as.list(bdy)
        bdy_list <- bdy_list[-(n - 1L)]
        bdy_list[[length(bdy_list)]] <- prev_expr[[3L]]
        return(as.call(bdy_list))
      }
    }
    return(bdy)
  }
  if (is_call(last_expr, quote(list))) {
    args <- as.list(last_expr)[-1L]
    if (!all(map_lgl(args, is.symbol))) {
      stop("all elements of return list must be symbols")
    }
    return(bdy)
  }
  bdy[[length(bdy)]] <- call("<-", quote(out_), last_expr)
  bdy[[length(bdy) + 1L]] <- quote(out_)
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
