# r2f-reductions.R
# Handlers for reduction operations:
# - numeric: max, min, sum, prod
# - logical: any, all
# - index: which.max, which.min

# --- Helpers ---

# A c(TRUE)-style literal lowers to a rank-1 Fortran array constructor
# ("[ ... ]") even when its value passes as a scalar; any()/all() must
# wrap such values to reduce them back to a scalar expression.
# Used by: any/all handler
renders_as_array_ctor <- function(f) {
  startsWith(trimws(as.character(f)), "[")
}

# TRUE for values declared length-1 (rank-1, dims list(1L)): scalar in
# the ABI, but not a Fortran scalar expression.
# Used by: any/all handler
is_declared_len1 <- function(f) {
  !is.null(f@value) && identical(f@value@dims, list(1L))
}

# --- Handlers ---

register_r2f_handler(
  c("max", "min", "sum", "prod"),
  function(
    args,
    scope,
    ...
  ) {
    # Named arguments like `na.rm` would otherwise be treated as data
    # arguments (e.g. `sum(x, na.rm = TRUE)` -> `(sum(x) + .true.)`).
    arg_names <- names(args) %||% character()
    if (length(arg_names) && any(nzchar(arg_names))) {
      stop(
        "max()/min()/sum()/prod() do not support named arguments (e.g. `na.rm`)",
        call. = FALSE
      )
    }

    call_name <- last(list(...)$calls)
    intrinsic <- switch(
      call_name,
      max = "maxval",
      min = "minval",
      sum = "sum",
      prod = "product"
    )

    reduce_arg <- function(arg) {
      mask_hoist <- create_mask_hoist()
      x <- reduce_arg_with_mask(arg, scope, mask_hoist, list(...))
      # R's numeric reductions treat logicals as integers (sum(TRUE) is 1L),
      # and Fortran's sum/product/minval/maxval reject logical arrays.
      x <- cast_to_mode(x, arith_join_mode(x), sprintf("%s()", call_name))
      if (x@value@is_scalar) {
        return(x)
      }
      hoisted_mask <- mask_hoist$get_hoisted()
      s <- glue(
        if (is.null(hoisted_mask)) {
          "{intrinsic}({x})"
        } else {
          "{intrinsic}({x}, mask = {hoisted_mask})"
        }
      )
      Fortran(s, Variable(x@value@mode))
    }

    if (length(args) == 1) {
      reduce_arg(args[[1]])
    } else {
      args <- lapply(args, reduce_arg)
      # Fortran's max/min require uniform argument types; cast every operand
      # whose mode differs from the join. The + / * spellings for sum/prod
      # don't strictly need it, but one code path beats two. Logical
      # operands join as integer (R: max(TRUE, FALSE) is 1L).
      mode <- arith_join_mode(args)
      args <- lapply(
        args,
        cast_to_mode,
        mode = mode,
        context = sprintf("%s()", call_name)
      )
      s <- switch(
        call_name,
        max = glue("max({str_flatten_commas(args)})"),
        min = glue("min({str_flatten_commas(args)})"),
        sum = glue("({str_flatten(args, ' + ')})"),
        prod = glue("({str_flatten(args, ' * ')})")
      )
      Fortran(s, Variable(mode))
    }
  }
)

register_r2f_handler(
  c("any", "all"),
  function(
    args,
    scope,
    ...
  ) {
    # For now, we only support the most common `any(x)` / `all(x)` shape.
    # We intentionally do not support named arguments like `na.rm`.
    arg_names <- names(args) %||% character()
    if (length(arg_names) && any(nzchar(arg_names))) {
      stop(
        "any()/all() do not support named arguments (e.g. `na.rm`)",
        call. = FALSE
      )
    }

    call_name <- last(list(...)$calls)
    intrinsic <- switch(
      call_name,
      any = "any",
      all = "all",
      stop("internal error: unexpected call: ", call_name, call. = FALSE)
    )

    # Match R's base semantics: any() == FALSE, all() == TRUE.
    if (length(args) == 0L) {
      lit <- if (identical(call_name, "any")) ".false." else ".true."
      return(Fortran(lit, Variable("logical")))
    }

    reduce_arg <- function(arg) {
      mask_hoist <- create_mask_hoist()
      x <- reduce_arg_with_mask(arg, scope, mask_hoist, list(...))

      if (!identical(x@value@mode, "logical")) {
        stop("any()/all() only implemented for logical", call. = FALSE)
      }

      hoisted_mask <- mask_hoist$get_hoisted()

      # Scalar logical: any(x) == x, all(x) == x
      if (x@value@is_scalar) {
        if (is.null(hoisted_mask)) {
          # `c(FALSE)` lowers to a 1-element Fortran array constructor
          # (`[.false.]`) but any()/all() must still return scalars.
          if (renders_as_array_ctor(x)) {
            return(Fortran(glue("{intrinsic}({x})"), Variable("logical")))
          }
          return(x)
        }

        # For scalar `x`, `x[mask]` is empty iff `!any(mask)`.
        #
        # Note: `logical(1)` masks are represented as rank-1 (dims = list(1L))
        # but pass as scalars in the ABI and must *not* be wrapped in `any()` /
        # `all()` (compilers reject `any()` / `all()` on scalar arguments).
        #
        # Conversely, literal masks like `c(FALSE)` compile to array constructors
        # (e.g. `[ .false. ]`) and must be reduced to a scalar condition.
        mask_is_scalar <-
          !is.null(hoisted_mask@value) &&
          passes_as_scalar(hoisted_mask@value) &&
          !renders_as_array_ctor(hoisted_mask)

        mask_len1 <- is_declared_len1(hoisted_mask)

        if (!mask_is_scalar && !mask_len1) {
          stop(
            "any()/all(): scalar masked subsets only support scalar or length-1 masks",
            call. = FALSE
          )
        }

        mask_scalar <- if (mask_is_scalar) {
          glue("{hoisted_mask}")
        } else {
          glue("any({hoisted_mask})")
        }

        # When `[` hoists a scalar mask (x[mask] -> x with a hoisted mask),
        # we must preserve empty-selection semantics:
        # - any(logical(0)) == FALSE
        # - all(logical(0)) == TRUE
        identity <- if (identical(call_name, "any")) ".false." else ".true."
        x_scalar <- if (renders_as_array_ctor(x)) {
          glue("{intrinsic}({x})")
        } else {
          glue("{x}")
        }
        return(Fortran(
          glue("merge({x_scalar}, {identity}, {mask_scalar})"),
          Variable("logical", x@value@dims)
        ))
      }

      x_expr <- if (is.null(hoisted_mask)) {
        glue("{x}")
      } else {
        # Avoid `pack()` temporaries. For a mask-selected subset:
        # - any(x[mask]) is equivalent to any(x .and. mask)
        # - all(x[mask]) is equivalent to all((.not. mask) .or. x)
        # Both preserve empty-selection semantics.
        #
        # Note: A length-1 mask constructor like `c(TRUE)` compiles to a rank-1
        # array constructor (`[ .true. ]`). In R, this is recycled as a scalar
        # mask, so we must scalarize it to keep elementwise ops conformable.
        mask_ctor_len1 <-
          renders_as_array_ctor(hoisted_mask) && is_declared_len1(hoisted_mask)
        mask_expr <- if (mask_ctor_len1) {
          glue("any({hoisted_mask})")
        } else {
          glue("{hoisted_mask}")
        }
        if (identical(call_name, "any")) {
          glue("(({x}) .and. ({mask_expr}))")
        } else {
          glue("((.not. ({mask_expr})) .or. ({x}))")
        }
      }

      Fortran(glue("{intrinsic}({x_expr})"), Variable("logical"))
    }

    if (length(args) == 1L) {
      return(reduce_arg(args[[1L]]))
    }

    args <- lapply(args, reduce_arg)
    op <- if (identical(call_name, "any")) ".or." else ".and."
    Fortran(glue("({str_flatten(args, glue(' {op} '))})"), Variable("logical"))
  }
)


r2f_handlers[["which.max"]] <- r2f_handlers[["which.min"]] <-
  function(args, scope = NULL, ...) {
    stopifnot(length(args) == 1)
    call_name <- last(list(...)$calls)
    x <- r2f(args[[1L]], scope, ...)
    stopifnot(
      "Values passed to which.max()/which.min() must be 1d arrays" = x@value@rank ==
        1
    )
    valout <- Variable(mode = "integer") # integer scalar

    if (x@value@mode == "logical") {
      # R semantics:
      # - which.max(all FALSE) == 1
      # - which.min(all TRUE)  == 1
      # findloc() returns 0 when the value is not found, so we wrap it with
      # max(1, ...) to preserve R's tie/default.
      #
      # Performance notes (quickr-compiled, n = 20,000,000 logicals ~= 76 MiB):
      # - maxloc(merge(1_c_int, 0_c_int, (a/=0)), 1) is ~10ms regardless of
      #   where the first .true. occurs (full traversal).
      # - max(1_c_int, findloc((a/=0), .true., 1, kind=c_int)) can early-exit
      #   (~1.3ms when the first element is .true.) but is much slower on full
      #   scans (~55-62ms when the last element is .true. or no .true. exists).
      # - max(1_c_int, findloc(a, 1_c_int, 1, kind=c_int)) on the underlying
      #   integer storage keeps full-scan performance close to maxloc (~14ms)
      #   while retaining early-exit.
      # Results are compiler/runtime dependent; the relative pattern was stable.
      #
      has_var_name <- inherits(x@value, Variable) && !is.null(x@value@name)
      use_lgl_storage <- has_var_name && !logical_as_int(x@value)
      int_backed_expr <-
        logical_as_int(x@value) &&
        !isTRUE(x@logical_booleanized)

      # Prefer searching the underlying integer storage directly when available
      # (external logical arrays are passed as integer(0/1)). If the input is an
      # actual Fortran logical array, search it directly to avoid unnecessary
      # casting.
      haystack <- if (has_var_name) {
        x@value@name
      } else if (int_backed_expr) {
        as.character(x)
      } else {
        glue("merge(1_c_int, 0_c_int, {x})")
      }
      needle <- switch(
        call_name,
        which.max = if (use_lgl_storage) ".true." else "1_c_int",
        which.min = if (use_lgl_storage) ".false." else "0_c_int"
      )

      loc <- glue("findloc({haystack}, {needle}, 1, kind=c_int)")
      f <- glue("max(1_c_int, {loc})")
    } else {
      intrinsic <- switch(
        call_name,
        which.max = "maxloc",
        which.min = "minloc"
      )
      f <- glue("{intrinsic}({x}, 1)")
    }

    Fortran(f, valout)
  }
