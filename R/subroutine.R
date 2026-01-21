new_fortran_subroutine <- function(
  name,
  closure,
  parent = environment(closure)
) {
  check_fortran_subroutine_name_valid(name)
  check_all_var_names_valid(closure)

  # translate body, and populate scope with variables
  body <- body(closure)

  # defuse calls like `-1` and `1+1i`. Not really necessary, but simplifies downstream a little.
  body <- defuse_numeric_literals(body)

  # TODO: try harder here to use one of the input vars as the output var
  body <- ensure_last_expr_sym(body)

  # update closure with sym return value
  base::body(closure) <- body
  # body <- rlang::zap_srcref(body)

  scope <- new_scope(closure, parent)
  attr(scope, "return_names") <- unique(unname(closure_return_var_names(
    closure
  )))

  # inject symbols for var sizes in declare calls, so like:
  #   declare(type(foo = integer(nr, NA)),
  #           type(bar = integer(nr, 3)))
  # become:
  #   declare(type(foo = integer(foo_dim_1_, foo_dim_2_)),
  #           type(bar = integer(foo_dim_1_, 3L)))
  body <- substitute_declared_sizes(body)
  stmts <- as.list(body)[-1L]
  body <- compile_nonreturn_statements(drop_last(stmts), scope)

  # check all input vars were declared
  # TODO: this check might be too late, because r2f() might throw cryptic errors
  # when handling undeclared variables. Either throw better errors from r2f(), or
  # handle all declares first
  for (arg_name in names(formals(closure))) {
    var <- get0(arg_name, scope)
    if (is.null(var) || !inherits(var, Variable)) {
      stop("arg not declared: ", arg_name)
    }
  }

  # Ensure return vars are marked (primarily to mark logical outputs as integer
  # storage for bind(c) and to support early "external-ness" checks).
  for (return_name in attr(scope, "return_names", exact = TRUE)) {
    if (!is.null(var <- get0(return_name, scope))) {
      stopifnot(inherits(var, Variable))
      var@is_return <- TRUE
      if (identical(var@mode, "logical")) {
        var@logical_as_int <- TRUE
      }
      scope[[return_name]] <- var
    }
  }

  manifest <- r2f.scope(scope)
  fsub_arg_names <- attr(manifest, "signature", TRUE)

  internal_procs <- attr(scope, "internal_procs", exact = TRUE) %||% list()
  contains_block <- if (length(internal_procs)) {
    procs_code <- lapply(internal_procs, `[[`, "code") |>
      unlist(use.names = FALSE) |>
      str_flatten_lines()
    str_flatten_lines("contains", indent(procs_code))
  } else {
    NULL
  }
  contains_block_indented <- if (is.null(contains_block)) {
    NULL
  } else {
    indent(contains_block)
  }
  body_section <- indent(body)
  if (!is.null(contains_block_indented)) {
    body_section <- str_flatten_lines(body_section, "", contains_block_indented)
  }

  uses_rng <- isTRUE(attr(scope, 'uses_rng', TRUE))
  used_iso_bindings <- iso_c_binding_symbols(
    vars = scope_vars(scope),
    body_code = body,
    logical_is_c_int = function(var) var@name %in% fsub_arg_names,
    uses_rng = uses_rng
  )
  if (uses_rng) {
    rng_interface <- glue::trim(
      '
      interface
        function unif_rand() bind(c, name = "unif_rand") result(u)
          use iso_c_binding, only: c_double
          real(c_double) :: u
        end function unif_rand
      end interface
      '
    )

    manifest <- str_flatten_lines(manifest, "", rng_interface)
  }
  subroutine <- glue(
    "
subroutine {name}({str_flatten_commas(fsub_arg_names)}) {fortran_bind_clause(name)}
  use iso_c_binding, only: {str_flatten_commas(used_iso_bindings)}
  implicit none

{indent(manifest)}

{body_section}
end subroutine
    "
  )
  subroutine <- glue::trim(subroutine)

  subroutine <- insert_fortran_line_continuations(subroutine)

  FortranSubroutine(
    subroutine,
    name = name,
    signature = fsub_arg_names,
    scope = scope,
    closure = closure
  )
}

check_fortran_subroutine_name_valid <- function(name) {
  if (!is_string(name) || !nzchar(name)) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }
  # Must be valid in:
  # - Fortran: used as the procedure name.
  # - C: used as the binding label and referenced from the C bridge.
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
    suggestion <- gsub("[^A-Za-z0-9_]", "_", name)
    if (!grepl("^[A-Za-z]", suggestion)) {
      suggestion <- paste0("quick_", suggestion)
    }
    stop(
      "Invalid `quick()` name: '",
      name,
      "'. The name must match the pattern '^[A-Za-z][A-Za-z0-9_]*$' ",
      "(letters, digits, underscore; starting with a letter). ",
      "Suggested name: '",
      suggestion,
      "'.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

fortran_bind_clause <- function(name) {
  # Fortran is case-insensitive, but C symbol names are case-sensitive.
  # When `bind(c)` is used without an explicit `name=`, some toolchains
  # canonicalize the external symbol name (e.g., lowercase), which can cause
  # the C bridge to look up the wrong symbol for mixed-case procedure names.
  if (identical(name, tolower(name))) {
    return("bind(c)")
  }
  glue('bind(c, name = "{name}")')
}

insert_fortran_line_continuations <- function(
  code,
  preserve_attributes = TRUE
) {
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
      if (!any(too_long <- nchar(lines) > 132)) {
        break
      }
      lines[too_long] <- sub("^(.{1,130})\\s", "\\1 &\n", lines[too_long])
      lines <- str_split_lines(lines)
    }
    if (i > 255L) {
      stop(
        "Too long line encountered. Please split long expressions into a sequence of smaller expressions."
      )
    }
  }

  code <- str_flatten_lines(lines)
  if (preserve_attributes) {
    attributes(code) <- attrs_in
  }
  code
}
