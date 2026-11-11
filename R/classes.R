#' @import S7
NULL

validate_scalar <- function(value) {
  if (is.null(value)) {
    return()
  }
  if (length(value) != 1L) {
    return("must be a scalar")
  }
  if (anyNA(value)) {
    "must not be NA"
  }
}

prop_bool <- function(default) {
  new_property(
    class_logical,
    validator = validate_scalar,
    default = default
  )
}

prop_string <- function(allow_null = FALSE) {
  new_property(
    if (allow_null) NULL | class_character else class_character,
    validator = validate_scalar,
    setter = function(self, name, value) {
      if (is.symbol(value)) {
        value <- as.character(value)
      }
      prop(self, name) <- value
      self
    }
  )
}

prop_enum <- function(
  values,
  nullable = FALSE,
  default = if (nullable) NULL else values[1],
  exact = FALSE
) {
  new_property(
    if (nullable) NULL | class_character else class_character,
    setter = if (!exact) {
      function(self, name, value) {
        if (length(value) == 1L && !anyNA(i <- charmatch(value, values))) {
          value <- values[i]
        }
        prop(self, name) <- value
        self
      }
    },
    validator = function(value) {
      if (is.null(value)) {
        return()
      }
      error <- validate_scalar(value)
      if (!is.null(error)) {
        return(error)
      }
      if (!value %in% values) {
        sprintf(
          "must be either %s, not '%s'.",
          glue_collapse(single_quote(values), sep = ", ", last = ", or "),
          value
        )
      }
    },
    default = default
  )
}

# S7's property assignment still evaluates calls and symbols when checking a
# typed property (src/prop.c: prop_validate()). Keep ASTs unevaluated here until
# that part of https://github.com/RConsortium/S7/issues/511 is fixed.
set_r <- function(self, value) {
  prop(self, "r", check = FALSE) <- value
  self
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
    mode = prop_enum(.atomic_type_names, nullable = TRUE),

    dims = new_property(
      # NULL means scalar
      NULL | class_list,
      setter = function(self, value) {
        if (!length(value)) {
          self@dims <- NULL
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

    name = prop_string(allow_null = TRUE),

    r_name = prop_string(allow_null = TRUE),

    rank = new_property(
      class_integer,
      getter = function(self) {
        length(self@dims)
      }
    ),

    modified = prop_bool(default = FALSE),

    loop_is_singleton = prop_bool(default = FALSE),

    r = new_property(NULL | class_language | class_atomic, setter = set_r),

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

    # Fortran kind for integer variables. User-facing R integers remain c_int;
    # pointer-sized compiler locals opt into c_ptrdiff_t explicitly.
    integer_kind = prop_enum(
      c("c_int", "c_ptrdiff_t"),
      default = "c_int",
      exact = TRUE
    ),

    # TRUE when the variable is available via host association and should not
    # be redeclared in the local scope.
    host_associated = prop_bool(default = FALSE),

    # When set, references to this variable should treat the named dummy
    # argument as an optional input (e.g., is.null() -> .not. present()).
    optional_dummy = prop_string(allow_null = TRUE)
  ),

  validator = function(self) {
    if (isTRUE(self@logical_as_int) && !identical(self@mode, "logical")) {
      return("`logical_as_int` can only be TRUE when `mode` is 'logical'")
    }
    if (
      !identical(self@integer_kind, "c_int") &&
        !identical(self@mode, "integer")
    ) {
      "`integer_kind` can only differ from 'c_int' when `mode` is 'integer'"
    }
  }
)

Fortran := new_class(
  class_character,

  properties = list(
    value = NULL | Variable,

    # Metadata flags used during compilation/lowering. Keep these as explicit
    # properties rather than ad-hoc attributes so they are discoverable and
    # consistently propagated with the Fortran object.
    logical_booleanized = prop_bool(default = FALSE),
    writes_to_dest = prop_bool(default = FALSE),
    # A scalar expression broadcast to the dimensions in @value.
    scalar_fill = prop_bool(default = FALSE),

    r = new_property(
      NULL | class_language | class_atomic | class_function,
      setter = set_r
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
    name = prop_string(allow_null = TRUE),
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
    c_bridge = new_property(
      NULL | class_character,
      getter = function(self) {
        make_c_bridge(self) %error% NULL
      }
    )
  )
)

R2FHandler := new_class(
  class_function,
  properties = list(
    dest_supported = prop_bool(default = FALSE),
    # Set when the handler was registered as a namespace-level named function,
    # so dispatch can re-resolve it by name. See register_r2f_handler().
    fun_name = prop_string(allow_null = TRUE),
    # When NULL, r2f will resolve the callable by name and use match.call().
    # When FALSE, r2f will not attempt match.call().
    match_fun = new_property(
      NULL | class_function | class_logical,
      default = NULL
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
