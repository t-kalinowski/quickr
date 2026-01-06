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

# Helper function to validate that all list elements are symbols
validate_list_symbols <- function(list_call) {
  args <- as.list(list_call)[-1L]
  if (!all(map_lgl(args, is.symbol))) {
    stop("all elements of the list must be symbols")
  }
  invisible(TRUE)
}


ensure_last_expr_sym <- function(bdy) {
  if (!is_call(bdy, quote(`{`))) {
    stop("bad body, needs {")
  }

  last_expr <- last(bdy)

  # Case 1: Last expression is a symbol
  if (is.symbol(last_expr)) {
    # Check for pattern: out <- list(...); out
    n <- length(bdy)
    second_last_expr <- bdy[[n - 1L]]

    list_pattern <-
      is_call(second_last_expr, quote(`<-`)) &&
      identical(second_last_expr[[2L]], last_expr) &&
      is_call(second_last_expr[[3L]], quote(list))

    if (list_pattern) {
      # Modify body such that last espression is list(...)
      list_call <- second_last_expr[[3L]]
      validate_list_symbols(list_call)
      bdy[[n - 1]] <- NULL # delete list assignment
      bdy[[n - 1]] <- list_call # replace last line with list(...)
    }

    return(bdy)
  }

  # Case 2: Last expression is a direct list call
  if (is_call(last_expr, quote(list))) {
    validate_list_symbols(last_expr)
    return(bdy)
  }

  # Case 3: Other expressions - create assignment to out_
  bdy[[length(bdy)]] <- call("<-", quote(out_), last_expr)
  bdy[[length(bdy) + 1L]] <- quote(out_)
  bdy
}

whole_doubles_to_ints <- function(x) {
  walker <- function(x) {
    switch(
      typeof(x),
      double = if (trunc(x) == x) as.integer(x) else x,
      language = as.call(lapply(x, walker)),
      list = lapply(x, walker),
      x
    )
  }
  walker(x)
}
