# r2f-logical.R
# Handlers for logical and comparison operators: !, &, &&, |, ||, >=, >, <, <=, ==, !=

# --- Handlers ---

# ---- comparison operators ----

r2f_handlers[[">="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} >= {right})"), var)
}

r2f_handlers[[">"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} > {right})"), var)
}

r2f_handlers[["<"]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} < {right})"), var)
}

r2f_handlers[["<="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} <= {right})"), var)
}

r2f_handlers[["=="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} == {right})"), var)
}

r2f_handlers[["!="]] <- function(args, scope, ...) {
  .[left, right] <- lapply(args, r2f, scope, ...)
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} /= {right})"), var)
}

# ---- unary logical not ----

r2f_handlers[["!"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ...)
  if (x@value@mode != "logical") {
    stop("'!' expects a logical value; numeric coercions not yet supported")
  }
  x <- booleanize_logical_as_int(x)
  Fortran(glue("(.not. {x})"), Variable("logical", x@value@dims))
}

register_r2f_handler(
  "is.null",
  function(args, scope, ...) {
    stopifnot(length(args) == 1L)
    arg <- args[[1L]]
    if (!is.symbol(arg)) {
      stop("is.null() is only supported on symbols", call. = FALSE)
    }
    var <- get0(as.character(arg), scope)
    if (!inherits(var, Variable) || is.null(var@optional_dummy)) {
      stop(
        "is.null() is only supported for optional arguments with NULL defaults",
        call. = FALSE
      )
    }
    Fortran(glue("(.not. present({var@optional_dummy}))"), Variable("logical"))
  }
)


# ---- binary logical operators ----

# TODO: the scalar || probably need some more type checking.
# TODO: gfortran supports implicit casting that of logical to integer when
# assigning a logical to a variable declared integer, converting `.true.` to `1`,
# but this is not a standard language feature, and Intel's `ifort` uses `-1` for `.true`.
# We should explicitly use
#   `merge(1_c_int, 0_c_int, <lgl>)` to cast logical to int.
register_r2f_handler(
  c("&", "&&", "|", "||"),
  function(args, scope, ...) {
    args <- lapply(args, r2f, scope, ...)
    args <- lapply(args, function(a) {
      if (a@value@mode != "logical") {
        stop("must be logical")
      }
      a
    })
    .[left, right] <- args
    left <- booleanize_logical_as_int(left)
    right <- booleanize_logical_as_int(right)

    operator <- switch(
      last(list(...)$calls),
      `&` = ,
      `&&` = ".and.",
      `|` = ,
      `||` = ".or."
    )

    s <- glue("{left} {operator} {right}")
    val <- conform(left@value, right@value)
    val@mode <- "logical"
    Fortran(s, val)
  }
)
