

new_fortran_subroutine <- function(name, closure, parent = emptyenv()) {


  check_all_var_names_valid(closure)

  scope <- new_scope(closure, parent)

  # translate body, and populate scope with variables
  body <- body(closure)
  # body <- rlang::zap_srcref(body)

  # defuse calls like `-1` and `1+1i`. Not really necessary, but simplifies downstream a little.
  body <- defuse_numeric_literals(body)

  # TODO: try harder here to use one of the input vars as the output var
  body <- ensure_last_expr_sym(body)

  # inject symbols for var sizes in declare calls, so like:
  #   declare(type(foo = integer(nr, NA)),
  #           type(bar = integer(nr, 3)))
  # become:
  #   declare(type(foo = integer(foo_dim_1_, foo_dim_2_)),
  #           type(bar = integer(foo_dim_1_, 3L)))
  body <- substitute_declared_sizes(body)
  body <- r2f(drop_last(body), scope)

  # check all input vars were declared
  # TODO: this check might be too late, because r2f() might throw cryptic errors
  # when handling undeclared variables. Either throw better errors from r2f(), or
  # handle all declares first
  for(arg_name in names(formals(closure))) {
    if (is.null(var <- get0(arg_name, scope)))
      stop("arg not declared: ", arg_name)
  }

  # figure out the return variable.
  if (is.symbol(last_expr <- last(body(closure)))) {
    return_var <- get(last_expr, scope)
    return_var@is_return <- TRUE
    scope[[as.character(last_expr)]] <- return_var
  } else {
    # lots we can still do here, just not implemented yet.
    stop("last expression in the function must be a bare symbol")
  }

  manifest <- r2f.scope(scope)
  fsub_arg_names <- attr(manifest, "signature", TRUE)

  used_iso_bindings <- unique(unlist(use.names = FALSE, list(
    lapply(scope, function(var) {
      list(
        switch(
          var@mode,
          double = "c_double",
          integer = "c_int",
          logical = if (var@name %in% fsub_arg_names)
            "c_int",
          complex = "c_double_complex",
          raw = "c_int8_t"
        ),
        lapply(var@dims, function(size) {
          syms <- all.vars(size)
          c(if (any(grepl("__len_$", syms))) "c_ptrdiff_t",
            if (any(grepl("__dim_[0-9]+_$", syms))) "c_int")
        })
      )
    }))))

  # check for literal kinds
  if (!"c_int" %in% used_iso_bindings) {
    if (grepl("\\b[0-9]+_c_int\\b", body))
      append(used_iso_bindings) <- "c_int"
  }
  if (!"c_double" %in% used_iso_bindings) {
    if (grepl("\\b[0-9]+\\.[0-9]+_c_double\\b", body))
      append(used_iso_bindings) <- "c_double"
  }
  used_iso_bindings <- sort(used_iso_bindings, method = "radix")

  subroutine <- glue("
    subroutine {name}({str_flatten_commas(fsub_arg_names)}) bind(c)
      use iso_c_binding, only: {str_flatten_commas(used_iso_bindings)}
      implicit none

    {indent(manifest)}

    {indent(body)}
    end subroutine
    ")

  subroutine <- insert_fortran_line_continuations(subroutine)

  FortranSubroutine(
    subroutine,
    name = name,
    signature = fsub_arg_names,
    scope = scope,
    closure = closure
  )
}

insert_fortran_line_continuations <- function(code, preserve_attributes = TRUE) {
  attrs_in <- attributes(code)

  code <- as.character(code)
  lines <- str_split_lines(code)
  lines <- trimws(lines, "right")

  if (any(too_long <- nchar(lines) > 132)) {
    # remove leading indentation
    lines[too_long] <- trimws(lines[too_long], "left")

    # move trailing comment at the end
    lines[too_long] <- sub("^(.*)!(.*)$", "!\\2\n\\1", lines[too_long])
    lines <- str_split_lines(lines)

    # maximum 255 continuations are allowed
    for (i in 1:256) {
      if (!any(too_long <- nchar(lines) > 132))
        break
      lines[too_long] <- sub("^(.{1,130})\\s", "\\1 &\n", lines[too_long])
      lines <- str_split_lines(lines)
    }
    if (i > 255L)
      stop("Too long line encountered. Please split long expressions into a sequence of smaller expressions.")
  }

  code <- str_flatten_lines(lines)
  if (preserve_attributes)
    attributes(code) <- attrs_in
  code
}

