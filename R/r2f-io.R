# r2f-io.R
# Handlers for I/O operations: cat, print

# --- Handlers ---

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
  var <- get0(as.character(name), scope)
  if (!inherits(var, Variable)) {
    stop("could not resolve symbol: ", as.character(name))
  }
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
