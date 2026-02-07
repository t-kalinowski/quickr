# r2f-reductions.R
# Handlers for reduction operations: max, min, sum, prod, which.max, which.min

# --- Handlers ---

register_r2f_handler(
  c("max", "min", "sum", "prod"),
  function(
    args,
    scope,
    ...
  ) {
    intrinsic <- switch(
      last(list(...)$calls),
      max = "maxval",
      min = "minval",
      sum = "sum",
      prod = "product"
    )

    reduce_arg <- function(arg) {
      mask_hoist <- create_mask_hoist()
      x <- r2f(arg, scope, ..., hoist_mask = mask_hoist$try_set)
      if (mask_hoist$has_conflict()) {
        stop(
          "reduction expressions only support a single logical mask",
          call. = FALSE
        )
      }
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
      mode <- reduce_promoted_mode(args)
      s <- switch(
        last(list(...)$calls),
        max = glue("max({str_flatten_commas(args)})"),
        min = glue("min({str_flatten_commas(args)})"),
        sum = glue("({str_flatten(args, ' + ')})"),
        prod = glue("({str_flatten(args, ' * ')})")
      )
      Fortran(s, Variable(mode))
    }
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
        !isTRUE(attr(x, "logical_booleanized", exact = TRUE))

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
