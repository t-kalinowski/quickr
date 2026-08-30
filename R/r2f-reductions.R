# r2f-reductions.R
# Handlers for reduction operations:
# - numeric: max, min, sum, prod
# - logical: any, all
# - index: which.max, which.min

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

    empty_extrema_message <- "min()/max() of empty inputs are not supported"

    reduce_arg <- function(
      arg,
      ordered = FALSE,
      allow_empty = FALSE,
      later_args = list()
    ) {
      stopifnot(is.list(later_args))
      mask_hoist <- create_mask_hoist()
      # Nested reductions (e.g., min(max(...), ...)) can thread an existing
      # hoist_mask through `...`. We always want a single mask hoister per
      # reduction context, so we ignore any inherited one and install ours.
      dots <- list(...)
      arg_hoist <- if (ordered) capture_hoist(dots$hoist) else dots$hoist
      x <- r2f(
        arg,
        scope,
        calls = dots$calls,
        hoist = arg_hoist,
        hoist_mask = mask_hoist$try_set
      )
      if (mask_hoist$has_conflict()) {
        stop(
          "reduction expressions only support a single logical mask",
          call. = FALSE
        )
      }
      hoisted_mask <- mask_hoist$get_hoisted()
      nonempty <- TRUE
      empty_condition <- NULL
      if (
        call_name %in%
          c("min", "max") &&
          (!x@value@is_scalar || !is.null(hoisted_mask))
      ) {
        element_count <- var_element_count(x@value)
        if (!is.null(hoisted_mask)) {
          mask_code <- trimws(as.character(hoisted_mask))
          mask_is_scalar <-
            passes_as_scalar(hoisted_mask@value) &&
            !startsWith(mask_code, "[")
          nonempty <- if (mask_is_scalar) {
            glue("{hoisted_mask}")
          } else {
            glue("any({hoisted_mask})")
          }
          empty_condition <- glue(".not. ({nonempty})")
        } else if (!is.na(element_count)) {
          nonempty <- element_count > 0
        } else {
          x <- hoist_unless_name(x, arg_hoist)
          nonempty <- glue(
            "size({x}, kind=c_ptrdiff_t) > 0_c_ptrdiff_t"
          )
          empty_condition <- glue(
            "size({x}, kind=c_ptrdiff_t) == 0_c_ptrdiff_t"
          )
        }

        if (!allow_empty) {
          if (identical(nonempty, FALSE)) {
            stop(empty_extrema_message, call. = FALSE)
          }
          if (is_string(nonempty)) {
            emit_quickr_error_if(
              empty_condition,
              empty_extrema_message,
              arg_hoist,
              scope
            )
          }
        }
      }
      # R's numeric reductions treat logicals as integers (sum(TRUE) is 1L),
      # and Fortran's sum/product/minval/maxval reject logical arrays.
      x <- cast_to_mode(
        x,
        arith_join_mode(x),
        sprintf("%s()", call_name)
      )
      out <- if (x@value@is_scalar) {
        x
      } else {
        s <- glue(
          if (is.null(hoisted_mask)) {
            "{intrinsic}({x})"
          } else {
            "{intrinsic}({x}, mask = {hoisted_mask})"
          }
        )
        Fortran(s, Variable(x@value@mode))
      }

      if (ordered) {
        if (!allow_empty || identical(nonempty, TRUE)) {
          out <- snapshot_operand_before_later_effects(
            out,
            arg,
            later_args,
            scope,
            arg_hoist
          )
        }
        if (allow_empty) {
          return(list(value = out, nonempty = nonempty, hoist = arg_hoist))
        }
        out <- hoist_unless_name(out, arg_hoist)
        dots$hoist$emit(arg_hoist$render(character()))
      }
      out
    }

    if (length(args) == 1) {
      reduce_arg(args[[1]])
    } else if (call_name %in% c("min", "max")) {
      reduced <- lapply(seq_along(args), function(i) {
        reduce_arg(
          args[[i]],
          ordered = TRUE,
          allow_empty = TRUE,
          later_args = tail(args, -i)
        )
      })
      if (
        all(vapply(
          reduced,
          function(x) identical(x$nonempty, FALSE),
          logical(1L)
        ))
      ) {
        stop(empty_extrema_message, call. = FALSE)
      }

      values <- lapply(reduced, `[[`, "value")
      mode <- arith_join_mode(values)
      context <- sprintf("%s()", call_name)
      dots <- list(...)

      if (
        !any(vapply(
          reduced,
          function(x) is_string(x$nonempty),
          logical(1L)
        ))
      ) {
        active <- list()
        for (i in seq_along(reduced)) {
          if (identical(reduced[[i]]$nonempty, FALSE)) {
            dots$hoist$emit(reduced[[i]]$hoist$render(character()))
            next
          }
          value <- hoist_unless_name(values[[i]], reduced[[i]]$hoist)
          dots$hoist$emit(reduced[[i]]$hoist$render(character()))
          active[[length(active) + 1L]] <- value
        }
        active <- lapply(
          active,
          cast_to_mode,
          mode = mode,
          context = context
        )
        s <- if (length(active) == 1L) {
          active[[1L]]
        } else {
          glue("{call_name}({str_flatten_commas(active)})")
        }
        return(Fortran(s, Variable(mode)))
      }

      values <- lapply(values, cast_to_mode, mode = mode, context = context)
      result <- dots$hoist$declare_tmp(mode = mode, dims = NULL)
      seen <- dots$hoist$declare_tmp(mode = "integer", dims = NULL)
      dots$hoist$emit(glue("{seen@name} = 0_c_int"))

      for (i in seq_along(reduced)) {
        dots$hoist$emit(reduced[[i]]$hoist$render(character()))
        nonempty <- reduced[[i]]$nonempty
        if (identical(nonempty, FALSE)) {
          next
        }

        update <- glue(
          "
          if ({seen@name} == 0_c_int) then
            {result@name} = {values[[i]]}
            {seen@name} = 1_c_int
          else
            {result@name} = {call_name}({result@name}, {values[[i]]})
          end if
          "
        )
        if (is_string(nonempty)) {
          update <- glue(
            "
            if ({nonempty}) then
            {indent(update)}
            end if
            "
          )
        }
        dots$hoist$emit(update)
      }

      emit_quickr_error_if(
        glue("{seen@name} == 0_c_int"),
        empty_extrema_message,
        dots$hoist,
        scope
      )
      Fortran(result@name, result)
    } else {
      args <- lapply(seq_along(args), function(i) {
        reduce_arg(
          args[[i]],
          ordered = TRUE,
          later_args = tail(args, -i)
        )
      })
      # Fortran's max/min require uniform argument types; cast every operand
      # whose mode differs from the join. The + / * spellings for sum/prod
      # don't strictly need it, but one code path beats two. Logical
      # operands join as integer (R: max(TRUE, FALSE) is 1L).
      mode <- arith_join_mode(args)
      context <- sprintf("%s()", call_name)
      args <- lapply(args, cast_to_mode, mode = mode, context = context)
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
      x <- r2f(arg, scope, ..., hoist_mask = mask_hoist$try_set)
      if (mask_hoist$has_conflict()) {
        stop(
          "reduction expressions only support a single logical mask",
          call. = FALSE
        )
      }

      if (!identical(x@value@mode, "logical")) {
        stop("any()/all() only implemented for logical", call. = FALSE)
      }

      hoisted_mask <- mask_hoist$get_hoisted()

      # Scalar logical: any(x) == x, all(x) == x
      if (x@value@is_scalar) {
        if (is.null(hoisted_mask)) {
          # `c(FALSE)` lowers to a 1-element Fortran array constructor
          # (`[.false.]`) but any()/all() must still return scalars.
          x_code <- trimws(as.character(x))
          if (startsWith(x_code, "[")) {
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
        mask_code <- trimws(as.character(hoisted_mask))
        is_array_ctor <- startsWith(mask_code, "[")
        mask_is_scalar <-
          !is.null(hoisted_mask@value) &&
          passes_as_scalar(hoisted_mask@value) &&
          !is_array_ctor

        mask_len1 <-
          !is.null(hoisted_mask@value) &&
          identical(hoisted_mask@value@dims, list(1L))

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
        x_code <- trimws(as.character(x))
        x_scalar <- if (startsWith(x_code, "[")) {
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
        mask_code <- trimws(as.character(hoisted_mask))
        mask_is_array_ctor <- startsWith(mask_code, "[")
        mask_ctor_len1 <-
          mask_is_array_ctor &&
          !is.null(hoisted_mask@value) &&
          identical(hoisted_mask@value@dims, list(1L))
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
      call_name <- last(list(...)$calls)

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
        last(list(...)$calls),
        which.max = "maxloc",
        which.min = "minloc"
      )
      f <- glue("{intrinsic}({x}, 1)")
    }

    Fortran(f, valout)
  }
