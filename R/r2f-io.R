# r2f-io.R
# Handlers for I/O operations: cat, print

# --- Handlers ---

r2f_handlers[["cat"]] <- function(args, scope, ...) {
  # can do a lot more here still, just a POC for now
  stopifnot(length(args) == 1, typeof(args[[1]]) == "character")
  label <- args[[1]]
  if (!endsWith(label, "\n")) {
    stop("cat(<strings>) must end with '\n'")
  }
  label <- substring(label, 1, nchar(label) - 1)
  Fortran(glue(
    'call labelpr({fortran_string_literal(label)}, {nchar(label, type = "bytes")})'
  ))
}

r2f_handlers[["print"]] <- function(args, scope = NULL, ...) {
  # Only statement use is supported; R's invisible return value is not lowered.
  # args <- lapply(as.list(e)[-1], r2f, scope)
  # args <- as.list(e)[-1]
  # can do alot more here still, just a POC for now
  stopifnot(length(args) == 1, typeof(args[[1]]) == "symbol")
  name <- args[[1]]
  var <- get0(as.character(name), scope)
  if (!inherits(var, Variable)) {
    stop("could not resolve symbol: ", as.character(name))
  }
  name <- Fortran(var@name, value = var)
  if (var@mode == "logical") {
    name <- cast_to_mode(name, "integer", "print()")
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
