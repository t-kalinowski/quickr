#' @import S7
NULL

new_setter <- function(
  coerce = NULL,
  coerce_null = FALSE,
  set_once = FALSE,
  env = parent.frame(2L)
) {
  if (is.null(coerce) || isFALSE(coerce) && isFALSE(set_once)) {
    return()
  }

  bind_name <- quote(
    name <- as.character(last(attr(self, ".setting_prop", TRUE)))
  )

  check_set_once <- if (set_once) {
    quote(
      if (!is.null(prop(self, name))) {
        stop(name, " can only be set once")
      }
    )
  }

  rebind_coerced_value <-
    if (is.null(coerce) || isFALSE(coerce)) {
      NULL
    } else if (isTRUE(coerce)) {
      quote(
        value <- convert(
          from = value,
          to = S7_class(self)@properties[[as.character(name)]]$class
        )
      )
    } else if (is.function(coerce) || is.symbol(coerce)) {
      bquote(value <- .(coerce)(value))
    } else if (is.language(coerce)) {
      bquote(value <- .(coerce))
    } else {
      stop("coerce must be TRUE, FALSE, NULL, a function, a symbol, or a call")
    }

  if (!coerce_null && !is.null(rebind_coerced_value)) {
    rebind_coerced_value <- bquote(if (!is.null(value)) .(rebind_coerced_value))
  }

  set <- quote(`prop<-`(
    object = self,
    name = name,
    check = FALSE,
    value = value
  ))

  new_function(
    args = alist(self = , value = ),
    body = as.call(c(
      quote(`{`),
      bind_name,
      check_set_once,
      rebind_coerced_value,
      set
    )),
    env = env
  )
}


new_scalar_validator <- function(
  allow_null = FALSE,
  allow_na = FALSE,
  additional_checks = NULL,
  env = parent.frame(2L)
) {
  checks <- c(
    if (allow_null) quote(if (is.null(value)) return()),
    quote(if (length(value) != 1L) return("must be a scalar")),
    if (!allow_na) quote(if (anyNA(value)) return("must not be NA")),
    additional_checks
  )

  new_function(
    args = alist(value = ),
    body = as.call(c(quote(`{`), checks)),
    env = parent.frame(2L)
  )
}


prop_bool <- function(
  default,
  allow_null = FALSE,
  allow_na = FALSE,
  set_once = FALSE
) {
  stopifnot(is_bool(set_once), is_bool(allow_null), is_bool(allow_na))

  new_property(
    class = if (allow_null) NULL | class_logical else class_logical,
    setter = new_setter(set_once = set_once),
    validator = new_scalar_validator(
      allow_null = allow_null,
      allow_na = allow_na
    ),
    default = default
  )
}


prop_string <- function(
  default = NULL,
  allow_null = FALSE,
  allow_na = FALSE,
  coerce = FALSE,
  set_once = FALSE
) {
  stopifnot(is_bool(set_once), is_bool(allow_null), is_bool(allow_na))

  if (isTRUE(coerce)) {
    coerce <- quote(as.character)
  }

  new_property(
    class = if (allow_null) NULL | class_character else class_character,
    default = default,
    validator = new_scalar_validator(allow_null = allow_null),
    setter = new_setter(
      coerce = coerce,
      coerce_null = !allow_null,
      set_once = set_once
    )
  )
}


prop_wholenumber <- function(
  default = NULL,
  allow_null = FALSE,
  allow_na = FALSE,
  coerce = TRUE,
  set_once = FALSE
) {
  stopifnot(is_bool(set_once), is_bool(allow_null), is_bool(allow_na))

  if (isTRUE(coerce)) {
    coerce <- quote(
      if (is_wholenumber(value)) {
        as.integer(value)
      } else {
        stop("@", name, " must be a whole number, but received: ", value)
      }
    )
  }

  new_property(
    class = if (allow_null) NULL | class_integer else class_integer,
    default = as.integer(default),
    setter = new_setter(
      coerce = coerce,
      coerce_null = !allow_null,
      set_once = set_once
    ),
    validator = new_scalar_validator(allow_null = allow_null)
  )
}


prop_enum <- function(
  values,
  nullable = FALSE,
  default = if (nullable) NULL else values[1],
  exact = FALSE,
  set_once = FALSE
) {
  stopifnot(
    "values must be a character vector of length >= 2 without any NA" = is.character(
      values
    ) &&
      length(values) >= 2 &&
      !anyNA(values)
  )

  coerce <- if (exact) {
    NULL
  } else {
    bquote(
      if (length(value) == 1L && !anyNA(i <- charmatch(value, .(values)))) {
        .(values)[i]
      } else {
        value
      }
    )
  }

  display_values <- glue_collapse(
    single_quote(values),
    sep = ", ",
    last = ", or "
  )
  msg <- sprintf("must be either %s, not '", display_values)
  validator <- new_scalar_validator(
    allow_null = nullable,
    additional_checks = bquote(
      if (!match(value, .(values), nomatch = 0L)) {
        return(paste0(.(msg), value, "'."))
      }
    )
  )

  new_property(
    class = if (nullable) NULL | class_character else class_character,
    setter = new_setter(
      coerce = coerce,
      coerce_null = !nullable,
      set_once = set_once
    ),
    validator = validator,
    default = default
  )
}


.atomic_type_names <- c(
  "integer",
  "logical",
  "double",
  "character",
  "raw",
  "complex"
)


# the print method for this should only print non-null values
Variable := new_class(
  properties = list(
    mode = prop_enum(.atomic_type_names, nullable = TRUE, set_once = FALSE),

    dims = new_property(
      # NULL means scalar
      NULL | class_list,
      setter = function(self, value) {
        if (!length(value)) {
          return(self)
        }

        value <- switch(
          typeof(value),
          logical = ,
          integer = ,
          double = as.list(value),
          language = ,
          symbol = list(value), # implicit rank-1
          list = value,
          stop("@dims must be a list")
        )

        value <- lapply(value, \(axis) {
          if (is.language(axis)) {
            axis
          } else if (is_wholenumber(axis) || is_scalar_na(axis)) {
            as.integer(axis)
          } else {
            stop(sprintf(
              "%s@dims must be a list of language or scalar integers, not %s",
              self@name %||% '',
              axis
            ))
          }
        })

        self@dims <- value
        self
      } # dims$setter
    ), # dims = new_property()

    name = prop_string(
      allow_null = TRUE,
      coerce = quote(switch(
        typeof(value),
        symbol = as.character(value),
        value
      )),
      set_once = FALSE #TRUE
    ),

    r_name = prop_string(
      allow_null = TRUE,
      coerce = quote(switch(
        typeof(value),
        symbol = as.character(value),
        value
      )),
      set_once = FALSE
    ),

    rank = new_property(
      class_integer,
      getter = function(self) {
        length(self@dims)
      }
    ),

    modified = prop_bool(default = FALSE),

    loop_is_singleton = prop_bool(default = FALSE),

    r = new_property(
      NULL | class_language | class_atomic,
      setter = function(self, value) {
        # custom setter to workaround https://github.com/RConsortium/S7/issues/511
        attr(self, "r") <- value
        self
      }
    ),

    is_arg = prop_bool(default = FALSE),

    is_return = prop_bool(default = FALSE),

    # TRUE for closure args and return values, FALSE for all other vars.
    is_external = new_property(
      class_logical,
      getter = function(self) {
        self@is_arg || self@is_return
      }
    ),

    is_scalar = new_property(
      class_logical,
      getter = function(self) {
        self@rank == 0 || identical(self@dims, list(1L))
      }
    ),

    # When TRUE, Fortran/C interfaces should treat logicals as integer(c_int)
    # storage (0/1) rather than Fortran LOGICAL.
    logical_as_int = prop_bool(default = FALSE),

    # TRUE when the variable is available via host association and should not
    # be redeclared in the local scope.
    host_associated = prop_bool(default = FALSE),

    # When set, references to this variable should treat the named dummy
    # argument as an optional input (e.g., is.null() -> .not. present()).
    optional_dummy = prop_string(default = NULL, allow_null = TRUE)
  ),

  validator = function(self) {
    if (isTRUE(self@logical_as_int) && !identical(self@mode, "logical")) {
      "`logical_as_int` can only be TRUE when `mode` is 'logical'"
    }
  }
)

# method(print, Variable) <- function(x, ...) {
#
# }

Fortran := new_class(
  class_character,

  properties = list(
    value = NULL | Variable,

    r = new_property(
      # custom setter only to workaround https://github.com/RConsortium/S7/issues/511
      NULL | class_language | class_atomic,
      setter = function(self, value) {
        attr(self, "r") <- value
        self
      }
    )
  ),

  validator = function(self) {
    if (length(self) != 1L) {
      "must be a length 1 string"
    }
  }
)

SvdResult := new_class(
  properties = list(
    d = Variable,
    u = NULL | Variable,
    v = NULL | Variable
  )
)

LocalClosure := new_class(
  properties = list(
    name = prop_string(default = NULL, allow_null = TRUE),
    fun = class_function
  )
)


FortranSubroutine := new_class(
  Fortran,
  properties = list(
    name = prop_string(),
    signature = class_character,
    closure = class_function,
    scope = NULL | class_environment,
    c_bridge = S7::new_property(
      NULL | class_character,
      getter = function(self) {
        make_c_bridge(self) %error% NULL
      }
    )
  )
)

try_prop <- function(object, name) S7::prop(object, name) %error% NULL

emit <- function(..., sep = "", end = "\n") cat(..., end, sep = sep)

method(format, Variable) <- function(x, ...) {
  capture.output(str(x))
}

method(as.character, Variable) <- function(x, ...) {
  x@name %||% stop("Variable does not have a name")
}

method(print, Fortran) <- function(x, ...) {
  emit(trimws(x), end = "\n\n")
  for (prop_name in c("value", "r", "c_bridge")) {
    if (!is.null(prop_val <- try_prop(x, prop_name))) {
      emit("@", prop_name, ": ", trimws(indent(format(prop_val))))
    }
  }
}
