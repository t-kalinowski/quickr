


### local variables with unspecified size are 'allocatable'. If they are bound
### to a named symbol, the manifest must mark it as allocatable.
###
### Generally, if an expression produces an array of unspecified size, even if
### it's never bound, it's still 'allocatable'. For example, an inline fortran
### `pack()` call likely still produces a corresponding `malloc()` in the
### generated code, regardless of if the output of `pack()` is bound to
### a symbol (in the case of pack specifically, the malloc is behind a
### _gfortran_pack() call.
###
### We can potentially link/mask `_malloc` and `_free` with a custom one that
### uses R_alloc(), which will automatically free after the .External() call
### returns. We can also pass along -fstack-arrays to gfortran and flang-new
### (llvm), and that will mostly get rid most of the malloc calls, instead
### allocating arrays on the C stack (which will automatically free on
### return/lngjmp), but that will run into issues with larger arrays (especially
### on windows)
###
### local vars of undefined sizes are allocatable. These will typically be
### allocated on the c stack if they are not too large, but may include a
### malloc+free call if they are large. Those might leak if we lngjmp
### away (e.g., due to an interrupt). This potential leak is a non-issue for
### now, since interrupts aren't supported yet, so there is no risk of lngjmp.
###
### When we do add support for interruptable quick functions, this potential
### leak could be guarded against by:
###
### a) linking malloc -> R_alloc() for the fortran compilation unit which
### would make the memory automatically be released after .External()
### return. Note that unlinke malloc(), R_alloc() is not thread safe, so we would need
### additional work for a `do concurrent` context to be supported.
###
### b) forcing all arrays to be stack allocated with -fstack-arrays passed
### to the gfortran/flang-new. This is not a great, since c stack limits are
### typically "small" and enforced by the OS.

r2f.scope <- function(scope) {

  vars <- as.list.environment(scope, all.names = TRUE)
  vars <- lapply(vars, function(var) {

    intent_in <- var@name %in% names(formals(scope@closure))
    intent_out <- var@name == closure_return_var_name(scope@closure) || intent_in && var@modified

    intent <-
      if (intent_in && intent_out) "intent(in out)"
      else if (intent_in) "intent(in)"
      else if (intent_out) "intent(out)"
      else NULL

    type <- switch(var@mode,
      double = "real(c_double)",
      integer = "integer(c_int)",
      complex = "complex(c_double_complex)",
      logical = if (intent_in || intent_out) "integer(c_int)" else "logical",
      raw = "integer(c_int8_t)",
      stop("unrecognized kind: ", format(var))
    )

    dims <- if (passes_as_scalar(var)) {
      NULL
    } else {
      dims2f(var@dims, scope) |> str_flatten_commas() |> sprintf(fmt = "(%s)")
    }

    allocatable <- if (!is.null(dims) && grepl(":", dims, fixed = TRUE))
      "allocatable"

    if (intent_in && intent_out && !is.null(allocatable))
      stop("all input and output vars must have a fully defined shape")

    name <- var@name
    comment <- if (var@mode == "logical") " ! logical"

    glue('{str_flatten_commas(type, intent, allocatable)} :: {name}{dims}{comment}',
         .null = "")
  })

  # vars that will be visible in the C bridge, either as an input or output
  non_local_var_names <- unique(c(names(formals(scope@closure)),
                                  closure_return_var_name(scope@closure)))

  # collect all size_names; sort so non-locals are declared first.
  size_names <- unique(unlist(lapply(non_local_var_names, function(name) {
    var <- scope[[name]]
    lapply(var@dims, all.names, functions = FALSE, unique = TRUE)
  }))) |> setdiff(names(formals(scope@closure)))

  sizes <- lapply(size_names, function(name) {
    kind <- if (endsWith(name, "_len_")) "c_ptrdiff_t" else "c_int"
    glue("integer({kind}), intent(in), value :: {name}")
  })

  manifest <- compact(list(
    sizes = sizes,
    args = vars[non_local_var_names],
    locals = vars[setdiff(names(vars), non_local_var_names)]
  ))

  manifest <- imap(manifest, \(declarations, category)
                   str_flatten_lines(paste("!", category), declarations)) |>
    str_flatten("\n\n")

  manifest <- str_flatten_lines("! manifest start", manifest, "! manifest end")

  # symbols that must come in as args to the subroutine
  # # method="radix" for locale-independent stable order.
  signature <- unique(c(non_local_var_names, sort(size_names, method = "radix")))
  attr(manifest, "signature") <- signature

  manifest
}



## fortran precedence order
##   ** (exp)
##   * /
##   + -
##
## R prededence order
##   ^
##   - +
##   %/% %%
##   * /

## generally, we just deparse() to convert an axis size.
## except for NA, which becomes ":"

dims2f_eval_base_env <- new.env(parent = emptyenv())
dims2f_eval_base_env[["("]] <- baseenv()[["("]]

# any call always evaluates to a string.
# every argument will be either:
# - NA  -> translates to ":"
# - a symbol -> translates to deparsed string
# - a call  ->

dims2f_eval_base_env[["+"]] <- function(e1, e2) glue("({e1} + {e2})")
dims2f_eval_base_env[["-"]] <- function(e1, e2) glue("({e1} - {e2})")
dims2f_eval_base_env[["*"]] <- function(e1, e2) glue("({e1} * {e2})")
dims2f_eval_base_env[["/"]] <- function(e1, e2) glue("real({e1}) / real({e2})")
# dividing integers truncates towards 0
dims2f_eval_base_env[["%/%"]] <- function(e1, e2) glue("int({e1}) / int({e2})")
dims2f_eval_base_env[["%%"]]  <- function(e1, e2) glue("mod(int({e1}), int({e2}))")
dims2f_eval_base_env[["^"]]  <- function(e1, e2) glue("({e1})**({e2})")


dims2f <- function(dims, scope) {
  syms <- unique(unlist(lapply(dims, \(d) if (is.language(d)) all.vars(d))))
  vars <- as.list(syms)
  names(vars) <- syms
  eval_env <- list2env(vars, parent = dims2f_eval_base_env)
  dims <- map_chr(dims, function(d) {
    d <- eval(d, eval_env)
    if (is.symbol(d)) as.character(d)
    else if (is_wholenumber(d)) as.character(d)
    else if (is_scalar_na(d)) ":"
    else if (is_string(d)) d
    else if (inherits(d, Variable)) {
      # a locally allocated var that is a return var
      if (!d@modified && d@is_arg)
        return(d@name)
      stop("unexpected axis size value")
    }
  })
  if (!length(dims) || identical(dims, "1")) ""
  else str_flatten_commas(dims)
}

