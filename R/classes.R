#' @import S7
NULL

validate_scalar <- function(value) {
  if (!is.null(value) && (length(value) != 1L || anyNA(value))) {
    "must be a non-missing scalar"
  }
}

prop_bool <- new_property(
  class_logical,
  validator = validate_scalar,
  default = FALSE
)

prop_string <- new_property(class_character, validator = validate_scalar)
prop_optional_string <- new_property(
  NULL | class_character,
  validator = validate_scalar
)

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
    mode = new_property(
      NULL | class_character,
      validator = function(value) {
        if (
          !is.null(value) &&
            (length(value) != 1L || !value %in% .atomic_type_names)
        ) {
          "must be a single atomic mode"
        }
      }
    ),

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

    name = prop_optional_string,

    r_name = prop_optional_string,

    rank = new_property(
      class_integer,
      getter = function(self) {
        length(self@dims)
      }
    ),

    modified = prop_bool,

    loop_is_singleton = prop_bool,

    r = new_property(NULL | class_language | class_atomic, setter = set_r),

    is_arg = prop_bool,

    is_return = prop_bool,

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
    logical_as_int = prop_bool,

    # Fortran kind for integer variables. User-facing R integers remain c_int;
    # pointer-sized compiler locals opt into c_ptrdiff_t explicitly.
    integer_kind = new_property(
      class_character,
      default = "c_int",
      validator = function(value) {
        if (length(value) != 1L || !value %in% c("c_int", "c_ptrdiff_t")) {
          "must be 'c_int' or 'c_ptrdiff_t'"
        }
      }
    ),

    # TRUE when the variable is available via host association and should not
    # be redeclared in the local scope.
    host_associated = prop_bool,

    # When set, references to this variable should treat the named dummy
    # argument as an optional input (e.g., is.null() -> .not. present()).
    optional_dummy = prop_optional_string,

    # Per-axis provenance to wrapper-validated input storage. Entries are NULL
    # or list(variable = <root R binding>, axis = <input axis>); shape-preserving
    # expressions and fixed-shape copies may retain them.
    input_shape = new_property(NULL | class_list)
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
    logical_booleanized = prop_bool,
    writes_to_dest = prop_bool,
    # A scalar expression broadcast to the dimensions in @value.
    scalar_fill = prop_bool,

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
    name = prop_optional_string,
    fun = class_function
  )
)


FortranSubroutine := new_class(
  Fortran,
  properties = list(
    name = prop_string,
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
    dest_supported = prop_bool,
    # Set when the handler was registered as a namespace-level named function,
    # so dispatch can re-resolve it by name. See register_r2f_handler().
    fun_name = prop_optional_string,
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
