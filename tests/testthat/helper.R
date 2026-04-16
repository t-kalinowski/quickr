scrub_environment <- function(x) {
  gsub("environment: 0x[0-9a-f]+", "environment: 0x0", x)
}

expect_translation_snapshots <- function(
  fn,
  name = deparse(substitute(fn)),
  note = NULL
) {
  fn
  fsub <- r2f(fn)
  cwrapper <- make_c_bridge(fsub)

  if (is.null(note)) {
    expect_snapshot(
      {
        fn
        cat(fsub)
        cat(cwrapper)
      },
      transform = scrub_environment
    )
  } else {
    expect_snapshot(
      {
        cat("# Snapshot note: ", note, "\n", sep = "")
        fn
        cat(fsub)
        cat(cwrapper)
      },
      transform = scrub_environment
    )
  }
}

expect_quick_identical <- function(fn, ...) {
  dll_paths_before <- loaded_dll_paths()
  on.exit(
    cleanup_new_quick_dlls(dll_paths_before),
    add = TRUE
  )
  qfn := quick(fn)
  args_list <- rlang::list2(...)
  args_list <- lapply(args_list, function(x) if (!is.list(x)) list(x) else x)

  for (args in args_list) {
    fn_res <- do.call(fn, args)
    qfn_res <- do.call(qfn, args)
    expect_identical(fn_res, qfn_res)
  }
}


expect_quick_equal <- function(fn, ...) {
  dll_paths_before <- loaded_dll_paths()
  on.exit(
    cleanup_new_quick_dlls(dll_paths_before),
    add = TRUE
  )
  qfn := quick(fn)
  args_list <- rlang::list2(...)
  args_list <- lapply(args_list, function(x) if (!is.list(x)) list(x) else x)

  for (args in args_list) {
    fn_res <- do.call(fn, args)
    qfn_res <- do.call(qfn, args)
    expect_equal(fn_res, qfn_res)
    expect_identical(typeof(fn_res), typeof(qfn_res))
  }
}

assign_in_global <- function(...) {
  x <- rlang::dots_list(..., .named = TRUE)
  list2env(x, envir = globalenv())
}

set_seed_and_call <- function(fun, ...) {
  set.seed(1234)
  fun(...)
}

loaded_dll_paths <- function() {
  unname(vapply(getLoadedDLLs(), function(dll) dll[["path"]], character(1)))
}

cleanup_new_quick_dlls <- function(before) {
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  after <- loaded_dll_paths()
  new_paths <- setdiff(after, before)
  new_paths <- new_paths[
    nzchar(new_paths) &
      startsWith(new_paths, temp_root) &
      grepl("-build-", new_paths, fixed = TRUE)
  ]

  for (path in rev(new_paths)) {
    tryCatch(dyn.unload(path), error = function(...) NULL)
    tryCatch(
      unlink(dirname(path), recursive = TRUE, force = TRUE),
      error = function(...) NULL
    )
  }

  invisible(NULL)
}

skip_if_no_openmp <- local({
  supported <- NULL
  function() {
    skip_on_cran()
    skip_if_not_installed("pkgload")
    if (is.null(supported)) {
      supported <<- tryCatch(
        {
          quick(function(x) {
            declare(type(x = double(1)))
            declare(parallel())
            for (i in seq_len(1L)) {
              x[i] <- x[i] + 1
            }
            x
          })
          TRUE
        },
        quickr_openmp_unavailable = function(e) FALSE,
        quickr_openmp_ignored = function(e) FALSE,
        quickr_openmp_load_failed = function(e) FALSE
      )
    }
    if (!isTRUE(supported)) {
      skip("OpenMP toolchain not available")
    }
  }
})
