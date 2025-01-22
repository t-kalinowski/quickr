


#' Quick Function
#'
#' @param fun An R function
#' @param name Optional string, name to use for the function.
#'
#' @returns A quicker R function.
#' @export
quick <- function(fun, name = NULL) {
  if (is.null(name)) {
    name <- if (is.symbol(substitute(fun)))
      deparse(substitute(fun))
    else
      make_unique_name(prefix = "anonymous_quick_function_")
  }

  if (nzchar(pkgname <- Sys.getenv("DEVTOOLS_LOAD"))) {
    if (requireNamespace("pkgload", quietly = TRUE)) {
      if (!collector$is_active()) {
        for (i in seq_along(sys.calls())) {
          if (identical(sys.function(i), pkgload::load_code)) {
            collector$activate(paste0(pkgname, ":quick_funcs"))
            defer(dump_collected(), sys.frame(i))
            break
          }
        }
      }
    }
  }

  if (collector$is_active()) {
    # we are in a quickr::compile_package() or a devtools::load_all() call,
    # merely collecting functions at this point.
    quick_closure <- create_quick_closure(name, fun)
    collector$add(name = name, closure = fun, quick_closure = quick_closure)
    return(quick_closure)
  }

  pkgname <- parent.pkg()
  if (!is.null(pkgname) && pkgname != "quickr") {
    # we are in a package - but outside a quickr::compile_package() call.
    return(create_quick_closure(name, fun))
  }

  # not in a package. Compile and load eagerly.
  attr(fun, "name") <- name
  fun <- compile(r2f(fun))
  attr(fun, "name") <- NULL

  fun
}

compile <- function(fsub, build_dir = tempfile(paste0(fsub@name, "-build-"))) {
  stopifnot(inherits(fsub, FortranSubroutine))

  name <- fsub@name
  c_wrapper <- make_c_bridge(fsub)

  if (dir.exists(build_dir)) unlink(build_dir, recursive = T)
  if (!dir.exists(build_dir))
    dir.create(build_dir)
  owd <- setwd(build_dir)
  on.exit(setwd(owd))

  fsub_path <- paste0(name, "_fsub.f90")
  c_wrapper_path <- paste0(name, "_c_wrapper.c")
  dll_path <- paste0(name, .Platform$dynlib.ext)
  writeLines(fsub, fsub_path)
  writeLines(c_wrapper, c_wrapper_path)

  suppressWarnings({
    result <- system2(
      R.home("bin/R"),
      c("CMD SHLIB --use-LTO", "-o", dll_path, fsub_path, c_wrapper_path),
      stdout = TRUE, stderr = TRUE
    )
  })
  if (!is.null(attr(result, "status"))) {
    writeLines(result, stderr())
    str(attributes(result))
    stop("Compilation Error")
  }

  # tryCatch(dyn.unload(dll_path), error = identity)
  dll <- dyn.load(dll_path)
  c_wrapper_name <- paste0(fsub@name, "_")
  ptr <- getNativeSymbolInfo(c_wrapper_name, dll)$address

  create_quick_closure(fsub@name, fsub@closure, native_symbol = ptr)
}



create_quick_closure <- function(name, closure,
                                 native_symbol = as.name(paste0(name, "_"))) {
  body(closure) <- as.call(c(quote(.External), native_symbol,
                             lapply(names(formals(closure)), as.name)))
  closure
}



check_all_var_names_valid <- function(fun) {
  nms <- unique(c(names(formals(fun)), all.vars(body(fun), functions = FALSE)))
  invalid <- endsWith(nms, "_") | startsWith(nms, "_") | nms %in% c(

    # clashes with Fortran subroutine symbols
    "c_int", "c_double", "c_ptrdiff_t",

    # clashes with C bridge symbols
    "int" #, "double",

    # ??? (clashes with R symbols?)
    # "double", "integer"
  )
  if (any(invalid)) {
    stop("symbols cannot start or end with '_', but found: ",
         glue_collapse(invalid, ", ", last = ", and "))
  }
}



# ---- utils ----

make_unique_name <- local({
  i <- 0L
  function(prefix = "tmp") {
    paste0(prefix, i <<- i + 1L)
  }
})
